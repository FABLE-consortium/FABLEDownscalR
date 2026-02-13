#' Harmonize baseline land cover map to match FABLE baseline totals
#'
#' Why:
#' - The spatial baseline (LandCover_df) and the national baseline (LC_targets)
#'   often differ (source/year/classification/rounding).
#' - If we downscale from an inconsistent baseline, national totals can drift away
#'   from FABLE projections.
#' - This function computes the reconciliation needed and redistributes it back
#'   into the spatial map as shares, producing a start map consistent with FABLE.
#'
#' @param LandCover_df data.frame with id_c + X<code> columns (areas per class per cell)
#' @param map_LC mapping table with columns code and LandCover (or equivalent name)
#' @param LC_targets data.frame with columns lu and value (national totals, same unit as start_map_harm)
#' @param keep_areas passed to LU_to_LUC (typically "from")
#' @param id_col id column name in LandCover_df (default "id_c")
#' @param area_unit_factor conversion factor applied to LandCover_df areas to match LC_targets units
#'   (e.g. your comment: km2 -> 1000ha uses /10)
#'
#' @return list(start_map, start_map_harm, start_map_reproj, LC_targets, LUC_targets, CLC_shares, ns_map)
#' @export
fdr_harmonize_start_map <- function(
    LandCover_df,
    map_LC,
    LC_targets,
    keep_areas = "from",
    id_col = "id_c",
    area_unit_factor = 1/10
) {
  chk_required_cols(LandCover_df, id_col)
  chk_required_cols(map_LC, "code")
  chk_required_cols(LC_targets, c("lu","value"))

  LandCover_df <- dplyr::mutate(LandCover_df, !!id_col := as.character(.data[[id_col]]))

  # ---- 1) Build spatial baseline start_map: ns x lu.from x value ----
  # Assumes LandCover_df has columns like X12, X45... that map_LC$code matches
  start_map <- LandCover_df %>%
    tidyr::pivot_longer(-dplyr::all_of(id_col), values_to = "AreaPerCover") %>%
    dplyr::mutate(
      AreaPerCover = as.numeric(AreaPerCover),
      code_chr = stringr::str_remove(name, "X")
    ) %>%
    dplyr::left_join(
      map_LC %>% dplyr::mutate(code = as.character(code)),
      by = c("code_chr" = "code")
    ) %>%
    # IMPORTANT: adjust this column name if your mapping uses a different one
    # In your Rmd it was "LandCover"
    dplyr::rename(lu.from = LandCover) %>%
    dplyr::filter(!lu.from %in% c("ocean","water","not relevant")) %>%
    dplyr::group_by(.data[[id_col]], lu.from) %>%
    dplyr::summarise(value = sum(AreaPerCover, na.rm = TRUE) * area_unit_factor, .groups = "drop") %>%
    dplyr::rename(ns = !!id_col) %>%
    dplyr::mutate(ns = as.character(ns)) %>%
    dplyr::bind_rows(
      tibble::tibble(ns = unique(as.character(LandCover_df[[id_col]])),
                     lu.from = "newforest",
                     value = 0)
    )

  # ---- 2) Aggregate spatial baseline to national totals ----
  start_map_harm <- start_map %>%
    dplyr::group_by(lu.from) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(lu = lu.from)

  # ---- 3) Compute reconciliation targets (needs LU_to_LUC somewhere in package) ----
  # This is your existing step:
  LUC_targets <- downscalr::LU_to_LUC(start_map_harm, LC_targets, keep_areas = keep_areas)

  # ---- 4) Convert those targets into shares by origin lu.from ----
  CLC_shares <- LUC_targets %>%
    dplyr::group_by(lu.from) %>%
    dplyr::mutate(share = value / sum(value, na.rm = TRUE)) %>%
    tidyr::pivot_wider(
      id_cols     = "lu.from",
      names_from  = "lu.to",
      values_from = "share"
    ) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))
    ) %>%
    tidyr::pivot_longer(
      cols = where(is.numeric),
      names_to = "lu.to",
      values_to = "proj_share"
    )

  # ---- 5) Apply shares back to the spatial map ----
  start_map_reproj <- dplyr::left_join(start_map, CLC_shares, by = "lu.from", relationship = "many-to-many") %>%
    dplyr::mutate(
      value = dplyr::if_else(!is.na(lu.to), value * proj_share, value),
      lu.to = dplyr::if_else(is.na(lu.to), lu.from, lu.to)
    ) %>%
    dplyr::group_by(ns, lu.to) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(lu.from = lu.to) %>%
    tidyr::drop_na()

  # ---- 6) Provide ns_map for later ID infra ----
  ns_map <- fdr_build_ns_map(start_map$ns, id_col = "id_c") %>%
    dplyr::mutate(id_c = as.character(id_c))

  list(
    start_map = start_map,
    start_map_harm = start_map_harm,
    start_map_reproj = start_map_reproj,
    LC_targets = LC_targets,
    LUC_targets = LUC_targets,
    CLC_shares = CLC_shares,
    ns_map = ns_map
  )
}
