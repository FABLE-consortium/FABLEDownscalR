# -----------------------------------------------------------------------------
# lc_build_country_luc()
# Why:
# - DownscalR expects transitions in a standard schema:
#   ns, lu.from, lu.to, Ts, value
# - Your LandCoverChange table is wide and uses numeric codes.
# - We convert wide -> long, map codes to class names, normalise to shares,
#   and ensure required transitions exist (as zeros) so matrices are consistent.
# -----------------------------------------------------------------------------
#' Build country land-use transition table (DownscalR/FABLE format)
#'
#' @param LandCoverChange_df Wide table: id_c + X<code> columns
#' @param map_HILDA_LUC Mapping table with columns code, from, to
#' @param expected_lu Expected land-use names
#' @param Ts Baseline year of transition shares
#'
#' @return list(country_luc = df, missing = list(missing_from, missing_to))
#' @export
lc_build_country_luc <- function(LandCoverChange_df,
                                 map_HILDA_LUC,
                                 expected_lu = c("cropland","forest","newforest","otherland","pasture","urban"),
                                 Ts = 2015) {

  LandCoverChange_df <- dplyr::mutate(LandCoverChange_df, id_c = as.character(id_c))

  chk_required_cols(LandCoverChange_df, "id_c")
  chk_required_cols(map_HILDA_LUC, c("code","from","to"))

  # ---- wide -> long ----
  luc_long <- LandCoverChange_df %>%
    tidyr::pivot_longer(-id_c, values_to = "AreaPerCode") %>%
    dplyr::mutate(
      name = stringr::str_remove(name, "X"),
      AreaPerCode = as.numeric(AreaPerCode)
    ) %>%
    dplyr::left_join(map_HILDA_LUC %>% dplyr::mutate(code = as.character(code)),
                     by = c("name" = "code")) %>%
    dplyr::filter(!(from %in% c("ocean", "water", "not relevant"))) %>%
    dplyr::filter(!(to   %in% c("ocean", "water", "not relevant")))

  luc_long <- luc_long %>%
    dplyr::filter(!is.na(from), !is.na(to))

  # ---- shares within each cell ----
  luc_shares <- luc_long %>%
    dplyr::group_by(id_c) %>%
    dplyr::mutate(
      TotalArea = sum(AreaPerCode, na.rm = TRUE),
      value = dplyr::if_else(TotalArea > 0, AreaPerCode / TotalArea, 0)
    ) %>%
    dplyr::ungroup()

  # ---- collapse duplicates (same from/to after mapping) ----
  luc_std <- luc_shares %>%
    dplyr::group_by(id_c, from, to) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::transmute(
      ns      = as.character(id_c),
      lu.from = from,
      lu.to   = to,
      Ts      = Ts,
      value   = dplyr::coalesce(value, 0)
    ) %>%
    # FABLE convention: non-forest -> forest is “newforest”
    dplyr::mutate(lu.to = dplyr::if_else(lu.from != "forest" & lu.to == "forest",
                                         "newforest", lu.to))

  # ---- complete missing transition pairs with zeros ----
  ns_all <- sort(unique(luc_std$ns))

  add_to_forest <- tidyr::crossing(
    ns = ns_all,
    lu.from = c("cropland", "newforest", "otherland", "pasture", "urban")
  ) %>% dplyr::mutate(lu.to = "forest", Ts = Ts, value = 0)

  add_from_newforest <- tidyr::crossing(
    ns = ns_all,
    lu.to = c("cropland", "newforest", "otherland", "pasture", "urban")
  ) %>% dplyr::mutate(lu.from = "newforest", Ts = Ts, value = 0)

  add_forest_to_newforest <- tidyr::crossing(ns = ns_all) %>%
    dplyr::mutate(lu.from = "forest", lu.to = "newforest", Ts = Ts, value = 0)

  luc_std <- dplyr::bind_rows(luc_std, add_to_forest, add_from_newforest, add_forest_to_newforest) %>%
    dplyr::distinct(ns, lu.from, lu.to, Ts, .keep_all = TRUE)

  present_from <- sort(unique(luc_std$lu.from))
  present_to   <- sort(unique(luc_std$lu.to))

  list(
    country_luc = luc_std,
    missing = list(
      missing_from = setdiff(expected_lu, present_from),
      missing_to   = setdiff(expected_lu, present_to)
    )
  )
}

#' Wrangle FABLE land transition matrix into DownscalR targets format
#'
#' @param fable_sheet A data.frame/tibble like inputs$FABLE_targets (raw Excel sheet)
#' @param min_year Keep only targets with times >= min_year (default 2020)
#' @return tibble with columns: times, lu.from, lu.to, value
#' @export
fdr_wrangle_fable_targets <- function(fable_sheet, min_year = 2020) {

  chk_required_cols(
    fable_sheet,
    c("LandCoverInit", "YearEnd",
      "ToForest", "ToOtherLand", "ToCropland", "ToPasture", "ToUrban", "ToNewForest")
  )

  out <- fable_sheet %>%
    dplyr::transmute(
      times   = as.integer(YearEnd),
      lu.from = tolower(as.character(LandCoverInit)),
      ToForest, ToOtherLand, ToCropland, ToPasture, ToUrban, ToNewForest
    ) %>%
    tidyr::pivot_longer(
      cols      = dplyr::starts_with("To"),
      names_to  = "lu.to",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      lu.to = tolower(stringr::str_remove(lu.to, "^To")),
      value = as.numeric(value)
    ) %>%
    # FABLE convention: non-forest -> forest becomes "newforest"
    dplyr::mutate(
      lu.to = dplyr::if_else(lu.from != "forest" & lu.to == "forest", "newforest", lu.to)
    ) %>%
    # Remove flows that are not modeled / not meaningful in your setup
    dplyr::filter(!(lu.from == "forest"   & lu.to == "newforest")) %>%
    dplyr::filter(!(lu.from == "newforest")) %>%
    dplyr::filter(!(lu.from == "urban" & lu.to!= "urban")) %>%
    # Collapse duplicates created by the remap
    dplyr::group_by(times, lu.from, lu.to) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(times >= min_year)

  out
}

