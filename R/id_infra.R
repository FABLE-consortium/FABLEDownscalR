# -----------------------------------------------------------------------------
# ID infrastructure helpers (ns_map, ID raster, grid sf)
#
# Why:
# - Downscaling tables often use a character cell id (id_c / ns)
# - Raster-based plotting needs an integer id stored in pixels (ns_int)
# - We standardize these conversions so every workflow uses the same rules
# -----------------------------------------------------------------------------

#' Build ns_map (character id -> integer ns_int)
#'
#' Why:
#' - Your tables use ns/id_c as a character identifier.
#' - Your raster canvas stores integer ids (ns_int) because raster values are numeric.
#' - This function creates a stable mapping so you can always convert between both.
#'
#' @param ns Character vector of cell IDs (or a data.frame column).
#' @param id_col Name to store the character id column in output (default "id_c").
#' @return A tibble with columns: id_c (character) and ns_int (integer)
#' @export
fdr_build_ns_map <- function(ns, id_col = "id_c") {
  ns <- as.character(ns)

  out <- tibble::tibble(!!id_col := ns) %>%
    dplyr::distinct(.data[[id_col]]) %>%
    dplyr::arrange(.data[[id_col]]) %>%
    dplyr::mutate(ns_int = dplyr::row_number())

  out
}

#' Build an ID raster (each raster pixel = ns_int)
#'
#' Why:
#' - ggplot needs a dense pixel canvas (x,y grid) to plot values.
#' - We rasterize the grid polygons so each pixel stores the cell ID (ns_int).
#' - Later, we can join any model outputs by ns_int and paint maps quickly.
#'
#' Inputs:
#' - grid geometry: sf polygons with id_c
#' - ns_map: id_c -> ns_int
#'
#' @param grid_sf sf polygon object with an id column (default "id_c")
#' @param ns_map tibble/data.frame containing id_c and ns_int
#' @param id_col name of the id column in grid_sf and ns_map (default "id_c")
#' @param crs_equal_area EPSG code for equal area calculations (default 6933)
#' @param pixel_res_m pixel resolution in meters (e.g. 50000, 10000, 1000)
#' @param touches terra rasterize touches argument (default TRUE; helps avoid missing thin polygons)
#' @return A SpatRaster with values = ns_int
#' @export
fdr_make_id_raster <- function(
    grid_sf,
    ns_map,
    id_col = "id_c",
    crs_equal_area = 6933,
    pixel_res_m = .fdr_defaults$pixel_res_m,
    touches = TRUE
) {
  # --- checks (fail early) ---
  chk_required_cols(ns_map, c(id_col, "ns_int"))
  if (!inherits(grid_sf, "sf")) stop("grid_sf must be an sf object.")
  if (!id_col %in% names(grid_sf)) stop("grid_sf must contain column: ", id_col)

  # --- ensure consistent types ---
  ns_map <- ns_map %>%
    dplyr::mutate(
      !!id_col := as.character(.data[[id_col]]),
      ns_int = as.integer(ns_int)
    ) %>%
    dplyr::distinct(.data[[id_col]], ns_int)

  grid_sf <- grid_sf %>%
    dplyr::mutate(!!id_col := as.character(.data[[id_col]]))

  # --- project grid to equal-area CRS (important for pixel size in meters) ---
  grid_eq <- sf::st_transform(grid_sf, crs_equal_area)

  # --- attach ns_int to grid polygons ---
  grid_eq$ns_int <- ns_map$ns_int[match(grid_eq[[id_col]], ns_map[[id_col]])]

  # If some ids didn't match, that's a data problem worth surfacing:
  if (any(is.na(grid_eq$ns_int))) {
    bad <- unique(grid_eq[[id_col]][is.na(grid_eq$ns_int)])
    stop("Some grid polygons have no ns_int match in ns_map. Example id(s): ",
         paste(head(bad, 10), collapse = ", "))
  }

  # --- build raster template ---
  r_template <- terra::rast(
    terra::ext(terra::vect(grid_eq)),
    resolution = pixel_res_m,
    crs = paste0("EPSG:", crs_equal_area)
  )

  # --- rasterize ns_int into pixels ---
  terra::rasterize(
    terra::vect(grid_eq),
    r_template,
    field = "ns_int",
    touches = touches
  )
}

#' Build a clean grid sf for plotting overlays
#'
#' Why:
#' - We often only need borders for ggplot overlays.
#' - st_make_valid() prevents plotting failures from invalid geometries.
#'
#' @param grid_sf sf polygon object
#' @param keep_cols columns to keep (default "id_c")
#' @return sf object with valid geometries
#' @export
fdr_make_grid_overlay <- function(grid_sf, keep_cols = "id_c") {
  if (!inherits(grid_sf, "sf")) stop("grid_sf must be an sf object.")
  grid_sf %>%
    dplyr::select(dplyr::any_of(keep_cols)) %>%
    sf::st_make_valid()
}


#' Build ID infrastructure for plotting (ns_int raster + grid_sf)
#'
#' Why:
#' - Downscaling results are keyed by `ns` (character), but rasters store integers.
#' - We rasterize the 50km grid (or finer) so every pixel has a stable integer ID (ns_int).
#' - We also return an sf grid for plotting borders.
#'
#' @param grid_sp A spatial object representing the grid polygons.
#'   Can be sp::SpatialPolygonsDataFrame, sf object, or terra vector.
#'   Must contain an `id_c` column.
#' @param ns_map A data.frame/tibble with columns `id_c` and `ns_int`.
#' @param pixel_res_m Raster resolution in meters (e.g. 50000, 10000, 1000).
#' @param crs_equal_area EPSG code or CRS string used for rasterization (default 6933).
#' @param keep_only_ids Optional vector of ns_int to keep (otherwise keep all).
#' @param touches Passed to terra::rasterize(). TRUE makes rasterization more inclusive on borders.
#'
#' @return list(rasterized_layer = SpatRaster, grid_sf = sf, ns_map = ns_map)
#' @export
fdr_build_id_maps <- function(
    grid_sp,
    ns_map,
    pixel_res_m = .fdr_defaults$pixel_res_m,
    crs_equal_area = .fdr_defaults$crs_equal_area,
    keep_only_ids = NULL,
    touches = TRUE,
    id_col = "id_c"
) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("sf is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr is required.")

  # 1) Convert grid_sp -> sf
  grid_sf <- if (inherits(grid_sp, "sf")) {
    grid_sp
  } else if (inherits(grid_sp, "Spatial")) {
    sf::st_as_sf(grid_sp)
  } else if (inherits(grid_sp, "SpatVector")) {
    sf::st_as_sf(grid_sp)
  } else {
    stop("grid_sp must be sf, sp::Spatial*, or terra::SpatVector.")
  }

  if (!id_col %in% names(grid_sf)) stop("grid_sf must contain column: ", id_col)

  # 2) Attach ns_int + optional filtering
  grid_sf <- grid_sf %>%
    dplyr::mutate(!!id_col := as.character(.data[[id_col]]))

  ns_map <- ns_map %>%
    dplyr::mutate(
      !!id_col := as.character(.data[[id_col]]),
      ns_int = as.integer(ns_int)
    ) %>%
    dplyr::distinct(.data[[id_col]], ns_int)

  grid_sf <- dplyr::left_join(grid_sf, ns_map, by = id_col) %>%
    dplyr::filter(!is.na(ns_int))

  if (!is.null(keep_only_ids)) {
    grid_sf <- dplyr::filter(grid_sf, ns_int %in% as.integer(keep_only_ids))
  }

  # 3) Build raster + overlay using existing helpers
  rasterized_layer <- fdr_make_id_raster(
    grid_sf = grid_sf,
    ns_map = ns_map,
    id_col = id_col,
    crs_equal_area = crs_equal_area,
    pixel_res_m = pixel_res_m,
    touches = touches
  )

  grid_overlay <- fdr_make_grid_overlay(grid_sf)

  list(
    rasterized_layer = rasterized_layer,
    grid_sf = grid_overlay,
    ns_map = ns_map
  )
}
