`%||%` <- function(x, y) if (is.null(x)) y else x

#' Assert that required columns exist
#'
#' @keywords internal
chk_required_cols <- function(df, cols, df_name = deparse(substitute(df))) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(df_name, " is missing required column(s): ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}
