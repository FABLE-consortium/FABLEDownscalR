#' Build a consistent filename tag for outputs
#'
#' Why:
#' - We want stable, readable output filenames across teams and runs.
#' - The tag is used as: <stamp>_<country>_<pathway>_<start_map_source>
#'
#' @param cfg Named list (typically from YAML) with fields:
#'   stamp, country, pathway, start_map_source
#' @return character(1)
#' @export
fdr_make_tag <- function(cfg) {
  required <- c("stamp", "country", "pathway", "start_map_source")
  missing <- setdiff(required, names(cfg))
  if (length(missing) > 0) {
    stop("cfg is missing field(s): ", paste(missing, collapse = ", "))
  }

  paste(cfg$stamp, cfg$country, cfg$pathway, cfg$start_map_source, sep = "_")
}

#' Save standard outputs for a downscaling run
#'
#' Why:
#' - Keeps all outputs in a predictable structure for sharing and reruns
#' - Centralizes IO (so workflow scripts stay short)
#'
#' @param country ISO3 code (e.g. "IND")
#' @param tag Filename tag (usually from fdr_make_tag(cfg))
#' @param outputs Named list of objects to save. Expected names commonly include:
#'   start_map_reproj, ns_map, rasterized_layer, grid_sf, X_long, betas,
#'   country_start_areas, downscaled_LUC
#' @param output_root Root output folder (default ".")
#' @param overwrite Logical; overwrite files if they exist (default TRUE)
#'
#' @return Invisibly returns a named character vector of paths written.
#' @export
fdr_save_outputs <- function(
    country,
    tag,
    outputs,
    output_root = ".",
    overwrite = TRUE
) {
  stopifnot(is.character(country), length(country) == 1L, nzchar(country))
  stopifnot(is.character(tag), length(tag) == 1L, nzchar(tag))
  stopifnot(is.list(outputs), length(outputs) > 0)

  out_dir <- file.path(output_root, country)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  written <- character(0)

  # helper: save RDS with standard naming
  save_rds <- function(obj, suffix) {
    fp <- file.path(out_dir, paste0(tag, "_", suffix, ".rds"))
    if (!overwrite && file.exists(fp)) stop("File exists and overwrite=FALSE: ", fp)
    saveRDS(obj, fp)
    written[[suffix]] <<- fp
  }

  # helper: save raster if terra SpatRaster
  save_raster <- function(r, suffix) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("terra is required to write rasters.")
    }
    fp <- file.path(out_dir, paste0(tag, "_", suffix, ".tif"))
    if (!overwrite && file.exists(fp)) stop("File exists and overwrite=FALSE: ", fp)
    terra::writeRaster(r, fp, overwrite = overwrite)
    written[[suffix]] <<- fp
  }

  # ---- standard outputs (only save if present) ----
  if (!is.null(outputs$start_map_reproj))    save_rds(outputs$start_map_reproj, "start_map_reproj")
  if (!is.null(outputs$ns_map))              save_rds(outputs$ns_map,           "ns_map")
  if (!is.null(outputs$grid_sf))             save_rds(outputs$grid_sf,          "grid_sf")
  if (!is.null(outputs$X_long))              save_rds(outputs$X_long,           "X_long")
  if (!is.null(outputs$betas))               save_rds(outputs$betas,            "pred_coeff_long")
  if (!is.null(outputs$country_start_areas)) save_rds(outputs$country_start_areas, "country_start_areas")
  if (!is.null(outputs$downscaled_LUC))      save_rds(outputs$downscaled_LUC,   "downscaled_LUC")

  # rasterized layer -> GeoTIFF
  if (!is.null(outputs$rasterized_layer)) {
    if (inherits(outputs$rasterized_layer, "SpatRaster")) {
      save_raster(outputs$rasterized_layer, "rasterized_layer")
    } else {
      # If it's not a SpatRaster, still save as RDS (so you don't lose it)
      save_rds(outputs$rasterized_layer, "rasterized_layer")
    }
  }

  invisible(written)
}
