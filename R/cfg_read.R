#' Read YAML config for a downscaling run
#' @export
fdr_read_config <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Run: install.packages('yaml')")
  }
  cfg <- yaml::read_yaml(path)

  required <- c("country", "pathway", "start_map_source")
  missing <- setdiff(required, names(cfg))
  if (length(missing) > 0) {
    stop("Missing required config fields: ", paste(missing, collapse = ", "))
  }

  # defaults
  cfg$stamp       <- cfg$stamp       %||% format(Sys.Date(), "%y%m%d")
  cfg$data_root   <- cfg$data_root   %||% "Data"
  cfg$output_root <- cfg$output_root %||% "Output"
  cfg$pixel_res_m <- cfg$pixel_res_m %||% .fdr_defaults$pixel_res_m

  cfg
}
