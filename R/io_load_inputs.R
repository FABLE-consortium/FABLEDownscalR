#' Load all required spatial + mapping inputs for one country
#'
#' @param data_root Root folder containing per-country subfolders (e.g. DATA_ROOT/IND/)
#' @param country ISO3 string (e.g. "IND")
#' @param start_map_source "HILDA" or "COPERNICUS"
#' @param pathway Name of the FABLE sheet containing transitions (e.g. "CurrentTrends")
#' @param spatial_registry tibble with columns: rdataset, filename
#' @param mapping_registry tibble with columns: rdataset, sheetname
#' @param grid_filename Relative path under data_root to the grid csv
#'
#' @return A named list with:
#'   - spatial: named list of data.frames (id_c as character; no 'id' col)
#'   - mapping: named list of mapping tables (+ map_LC, map_LUC aliases)
#'   - grid: grid table for the country (from grid50_equal_area.csv)
#'   - grid_sp: geometry to rasterize (sf polygons; typically Travel layer geometry)
#'   - LC_targets: baseline national totals (lu, value)
#'   - FABLE_targets: pathway sheet table
#'   - meta: file paths
#' @export
fdr_load_inputs <- function(
    data_root,
    country,
    start_map_source = c("HILDA", "COPERNICUS"),
    pathway,
    spatial_registry = NULL,
    mapping_registry = NULL,
    grid_filename = file.path("global", "grid50_equal_area.csv")
) {

  start_map_source <- toupper(trimws(start_map_source))
  start_map_source <- match.arg(start_map_source, choices = c("HILDA", "COPERNICUS"))

  # ---- default registries ----
  if (is.null(spatial_registry)) {
    spatial_registry <- tibble::tribble(
      ~rdataset,             ~filename,
      "population",          "Population2020.geojson",
      "protectedareas",      "ProtectedAreas.geojson",
      "travel",              "TravelTime.geojson",
      "landcoverHILDA",      "LandCoverHILDA2015.geojson",
      "landcoverCopernicus", "LandCoverCopernicus2019.geojson",
      "landcoverchange",     "LandCoverChangeHILDA2015_2019.geojson",
      "forestmanagement",    "ForestManagement.geojson",
      "altitude",            "Altitude.geojson",
      "slope",               "Slope.geojson",
      "gaez",                "GAEZCropDistribution2015.geojson",
      "livestock",           "GLW4WorldGriddedLivestock2020.geojson"
    )
  }

  if (is.null(mapping_registry)) {
    mapping_registry <- tibble::tribble(
      ~rdataset,             ~sheetname,
      "map_HILDA",           "HILDA",
      "map_HILDA_LUC",       "HILDA_change",
      "map_Copernicus",      "Copernicus",
      "map_ForestMgmt",      "ForestManagement",
      "map_protectedareas",  "ProtectedAreas"
    )
  }

  country_dir <- file.path(data_root, country)

  # ---- helpers ----
  read_geojson_sp <- function(fp) geojsonio::geojson_read(fp, what = "sp")

  read_geojson_df <- function(fp) {
    sp <- read_geojson_sp(fp)
    df <- sp@data

    if (!"id_c" %in% names(df)) stop("GeoJSON missing 'id_c': ", fp)
    df$id_c <- as.character(df$id_c)

    # drop common EE artefacts that break pivot_longer / joins
    df <- dplyr::select(df, -dplyr::any_of(c("id", "id.x", "id.y")))

    df
  }

  # ---- spatial (@data) ----
  spatial <- list()
  spatial_sp <- list()  # keep SP objects only when useful (e.g., travel geometry)

  for (i in seq_len(nrow(spatial_registry))) {
    nm <- spatial_registry$rdataset[[i]]
    fn <- spatial_registry$filename[[i]]
    fp <- file.path(country_dir, fn)

    if (!file.exists(fp)) {
      spatial[[nm]] <- NULL
      spatial_sp[[nm]] <- NULL
    } else {
      spatial[[nm]] <- read_geojson_df(fp)
      # keep geometry for travel, because we use it to build grid_sp for rasterization
      if (nm == "travel") spatial_sp[[nm]] <- read_geojson_sp(fp)
    }
  }

  spatial$landcover <- if (start_map_source == "HILDA") spatial$landcoverHILDA else spatial$landcoverCopernicus

  # ---- mapping ----
  map_fp <- file.path(data_root, "mapping_code.xlsx")
  if (!file.exists(map_fp)) {
    map_fp2 <- file.path(country_dir, "mapping_code.xlsx")
    if (!file.exists(map_fp2)) stop("mapping_code.xlsx not found in data_root or country folder.")
    map_fp <- map_fp2
  }

  mapping <- list()
  for (i in seq_len(nrow(mapping_registry))) {
    nm <- mapping_registry$rdataset[[i]]
    sh <- mapping_registry$sheetname[[i]]
    mapping[[nm]] <- readxl::read_excel(map_fp, sheet = sh)
  }

  mapping$map_LUC <- mapping$map_HILDA_LUC
  mapping$map_LC  <- if (start_map_source == "HILDA") mapping$map_HILDA else mapping$map_Copernicus

  # ---- FABLE inputs ----
  fable_fp <- file.path(country_dir, "FABLE.xlsx")
  if (!file.exists(fable_fp)) stop("FABLE.xlsx not found in: ", country_dir)

  LC_targets <- readxl::read_excel(fable_fp, sheet = "Baseline") %>%
    dplyr::rename(lu = LandCover) %>%
    dplyr::transmute(
      lu    = tolower(as.character(lu)),
      value = as.numeric(value)
    )

  FABLE_targets <- readxl::read_excel(fable_fp, sheet = pathway)

  # ---- GRID TABLE (global csv filtered to country) ----
  grid_fp <- file.path(data_root, grid_filename)
  if (!file.exists(grid_fp)) stop("Grid file not found: ", grid_fp)

  grid <- readr::read_csv(grid_fp, show_col_types = FALSE) %>%
    dplyr::filter(iso3 == country) %>%
    dplyr::mutate(id_c = as.character(id_c))

  # ---- grid_sp geometry used for rasterization ----
  # Best: use Travel layer geometry (same grid polygons) and keep only id_c
  grid_sp <- spatial_sp$travel
  if (is.null(grid_sp)) {
    stop("Cannot build grid_sp because travel layer is missing. Provide TravelTime.geojson or pass a grid geometry.")
  }

  list(
    spatial       = spatial,
    mapping       = mapping,
    grid          = grid,
    grid_sp       = grid_sp,
    LC_targets    = LC_targets,
    FABLE_targets = FABLE_targets,
    meta = list(
      country_dir   = country_dir,
      mapping_file  = map_fp,
      fable_file    = fable_fp,
      grid_file     = grid_fp
    )
  )
}
