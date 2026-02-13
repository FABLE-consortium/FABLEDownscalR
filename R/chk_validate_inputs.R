#' Validate required inputs are present and minimally well-formed
#'
#' @param inputs Output of fdr_load_inputs()
#' @return TRUE invisibly
#' @export
fdr_validate_inputs <- function(inputs) {
  stopifnot(is.list(inputs), !is.null(inputs$spatial), !is.null(inputs$mapping))

  must_have_spatial <- c("travel", "landcoverchange", "landcover")
  missing_sp <- must_have_spatial[vapply(must_have_spatial, function(x) is.null(inputs$spatial[[x]]), logical(1))]
  if (length(missing_sp) > 0) {
    stop("Missing required spatial datasets: ", paste(missing_sp, collapse = ", "),
         "\nCheck Data/<country>/ filenames.")
  }

  must_have_maps <- c("map_HILDA_LUC")
  missing_mp <- must_have_maps[vapply(must_have_maps, function(x) is.null(inputs$mapping[[x]]), logical(1))]
  if (length(missing_mp) > 0) stop("Missing required mapping sheets: ", paste(missing_mp, collapse = ", "))

  # Column checks
  if (!"id_c" %in% names(inputs$spatial$travel)) stop("travel dataset must contain id_c.")
  if (!"id_c" %in% names(inputs$spatial$landcoverchange)) stop("landcoverchange dataset must contain id_c.")
  if (!"code" %in% names(inputs$mapping$map_HILDA_LUC)) stop("map_HILDA_LUC must contain 'code' column.")

  invisible(TRUE)
}
