# -----------------------------------------------------------------------------
# fdr_build_priors()
#
# Why:
# - The MNL step needs an explanatory matrix X (covariates) per grid cell.
# - DownscalR expects X in "long" form: ns, ks, value
# - Any cell with NA in any covariate must be dropped (or imputed).
# - This helper:
#   (1) builds LU baseline covariates from the harmonised start map
#   (2) builds extra covariates (travel, pop, forest mgmt, altitude, slope, zones, yields, livestock)
#   (3) stacks everything into X_long
#   (4) optionally filters to "complete cases" only
# -----------------------------------------------------------------------------

#' Build priors (X matrix) for downscaling + drop cells with missing covariates
#'
#' @param inputs Output of fdr_load_inputs() (must contain inputs$spatial and inputs$mapping)
#' @param start_map Harmonised baseline map (typically harm$start_map_reproj).
#'   Must contain columns: ns (or id_c), lu.from, value
#' @param good_ns_only If TRUE, drop any cell (ns) that has NA in any covariate
#'
#' @return list with:
#'   - xmat: long table (ns, ks, value)
#'   - lu_levels: start_map (ns, lu.from, value)
#'   - good_ns: vector of ns kept
#'   - na_by_var / na_by_ns: diagnostics
#' @export
fdr_build_priors <- function(inputs, start_map, good_ns_only = TRUE) {

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr required")

  # ---- helpers ----
  to_char_id <- function(df, id = "id_c") {
    if (!id %in% names(df)) stop("Missing id column: ", id)
    df[[id]] <- as.character(df[[id]])
    df
  }

  # ---- pull spatial tables ----
  sp <- inputs$spatial

  # start_map should already be harmonised and in 1000 ha (your convention)
  # Accept either ns or id_c as key, normalize to ns
  if ("id_c" %in% names(start_map) && !"ns" %in% names(start_map)) {
    start_map <- dplyr::rename(start_map, ns = id_c)
  }
  chk_required_cols(start_map, c("ns", "lu.from", "value"))
  start_map <- dplyr::mutate(start_map, ns = as.character(ns))

  # ---- 1) LU baseline covariates (wide: one column per lu.from) ----
  Xmat_LU <- start_map %>%
    tidyr::pivot_wider(names_from = "lu.from", values_from = "value", values_fill = 0) %>%
    dplyr::rename(id_c = ns)

  # Xmat_LU: id_c + one column per LU (baseline levels)
  # These LU columns are compositional (sum ≈ constant). Drop one to avoid collinearity.
  lu_ref <- "otherland"  # choose one that always exists
  if (lu_ref %in% names(Xmat_LU)) {
    Xmat_LU <- dplyr::select(Xmat_LU, -dplyr::all_of(lu_ref))
  }


  # ---- 2) Forest management (wide) ----
  Xmat_ForestManagement <- NULL
  if (!is.null(sp$forestmanagement) && !is.null(inputs$mapping$map_ForestMgmt)) {
    ForestManagement.df <- to_char_id(sp$forestmanagement, "id_c")
    map_ForestMgmt <- inputs$mapping$map_ForestMgmt

    Xmat_ForestManagement <- ForestManagement.df %>%
      tidyr::pivot_longer(-id_c, values_to = "AreaPerCover") %>%
      dplyr::mutate(
        AreaPerCover = as.numeric(AreaPerCover),
        name = stringr::str_remove(name, "X")
      ) %>%
      dplyr::left_join(map_ForestMgmt %>% dplyr::mutate(code = as.character(code)),
                       by = c("name" = "code")) %>%
      dplyr::group_by(id_c, type) %>%
      dplyr::summarise(value = sum(AreaPerCover, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = type, values_from = value, values_fill = 0)
  }

  # ---- 3) Travel time (log1p) ----
  Xmat_Travel <- NULL
  if (!is.null(sp$travel) && "MeanTravelTime" %in% names(sp$travel)) {
    Travel.df <- to_char_id(sp$travel, "id_c")
    Xmat_Travel <- Travel.df %>%
      dplyr::transmute(id_c, log_MeanTravelTime = log1p(as.numeric(MeanTravelTime)))
  }

  # ---- 4) Population (log1p) ----
  Xmat_pop <- NULL
  if (!is.null(sp$population) && "TotalPop" %in% names(sp$population)) {
    population.df <- to_char_id(sp$population, "id_c")
    Xmat_pop <- population.df %>%
      dplyr::transmute(id_c, log_pop = log1p(as.numeric(TotalPop)))
  }

  # ---- 5) Altitude (log1p) ----
  Xmat_alt <- NULL
  if (!is.null(sp$altitude) && "MeanAltitude" %in% names(sp$altitude)) {
    altitude.df <- to_char_id(sp$altitude, "id_c")
    Xmat_alt <- altitude.df %>%
      dplyr::transmute(id_c,
                       log_altitude = log1p(as.numeric(MeanAltitude)-min(as.numeric(MeanAltitude),0)))
  }

  # ---- 6) Slope (log1p) ----
  Xmat_slope <- NULL
  if (!is.null(sp$slope) && "MeanSlope" %in% names(sp$slope)) {
    slope.df <- to_char_id(sp$slope, "id_c")
    Xmat_slope <- slope.df %>%
      dplyr::transmute(id_c, log_slope = log1p(as.numeric(MeanSlope)))
  }

  # ---- 7) Zones / thermal dummies (optional) ----
  # Only if inputs include grid table with these columns.
  Xmat_zones <- NULL
  if (!is.null(inputs$grid) &&
      all(c("id_c", "thermal", "zone") %in% names(inputs$grid))) {

    zones <- to_char_id(inputs$grid, "id_c") %>%
      dplyr::transmute(
        id_c,
        thermal = forcats::fct_na_value_to_level(as.factor(thermal), "UNK"),
        zone    = forcats::fct_na_value_to_level(as.factor(zone), "UNK")
      )

    MM <- stats::model.matrix(~ thermal + zone, data = zones)
    MM <- MM[, setdiff(colnames(MM), "(Intercept)"), drop = FALSE]

    Xmat_zones <- dplyr::bind_cols(
      dplyr::select(zones, id_c),
      as.data.frame(MM)
    ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        dplyr::across(dplyr::starts_with("thermal"), ~ tidyr::replace_na(.x, 0)),
        dplyr::across(dplyr::starts_with("zone"),    ~ tidyr::replace_na(.x, 0))
      )
  }

  # ---- 8) GAEZ yields + relative advantage (optional) ----
  # Keep simple here: add only a mean yield index if present.
  Xmat_yield <- NULL
  if (!is.null(sp$gaez)) {
    gaez.df <- to_char_id(sp$gaez, "id_c")
    yield_cols <- setdiff(names(gaez.df), "id_c")
    if (length(yield_cols) > 0) {
      Xmat_yield <- gaez.df %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(yield_cols), ~ log1p(pmax(as.numeric(.x), 0)))) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(yield_level_mean = mean(c_across(dplyr::all_of(yield_cols)), na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(id_c, yield_level_mean)
    }
  }

  # ---- 9) Livestock (optional; drop constant numeric columns) ----
  Xmat_livestock <- NULL
  if (!is.null(sp$livestock)) {
    livestock.df <- to_char_id(sp$livestock, "id_c")
    Xmat_livestock <- livestock.df %>%
      dplyr::select(
        dplyr::any_of("id_c"),
        dplyr::where(~ !is.numeric(.x) || any(abs(.x) > 1e-9, na.rm = TRUE))
      )
  }

  # ---- combine everything (wide) ----
  X_wide <- Xmat_LU %>%
    #dplyr::mutate(Intercept = 1) %>%
    dplyr::left_join(Xmat_Travel,           by = "id_c") %>%
    dplyr::left_join(Xmat_pop,              by = "id_c") %>%
    dplyr::left_join(Xmat_ForestManagement, by = "id_c") %>%
    dplyr::left_join(Xmat_alt,              by = "id_c") %>%
    dplyr::left_join(Xmat_slope,            by = "id_c") %>%
    dplyr::left_join(Xmat_zones,            by = "id_c") %>%
    dplyr::left_join(Xmat_yield,            by = "id_c") %>%
    dplyr::left_join(Xmat_livestock,        by = "id_c")

  # ---- wide -> long (DownscalR expects long) ----
  X_long <- X_wide %>%
    tidyr::pivot_longer(-id_c, names_to = "ks", values_to = "value") %>%
    dplyr::rename(ns = id_c) %>%
    dplyr::mutate(ns = as.character(ns))

  # ---- diagnostics ----
  na_by_var <- X_long %>%
    dplyr::group_by(ks) %>%
    dplyr::summarise(n_na = sum(is.na(value)), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n_na))

  na_by_ns <- X_long %>%
    dplyr::group_by(ns) %>%
    dplyr::summarise(n_na = sum(is.na(value)), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(n_na))

  good_ns <- X_long %>%
    dplyr::group_by(ns) %>%
    dplyr::summarise(ok = all(!is.na(value)), .groups = "drop") %>%
    dplyr::filter(ok) %>%
    dplyr::pull(ns)

  if (isTRUE(good_ns_only)) {
    X_long <- dplyr::filter(X_long, ns %in% good_ns)
    start_map <- dplyr::filter(start_map, ns %in% good_ns)
  }

  list(
    xmat = X_long,
    lu_levels = start_map,
    good_ns = good_ns,
    na_by_var = na_by_var,
    na_by_ns = na_by_ns
  )
}
