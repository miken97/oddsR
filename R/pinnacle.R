
#' Get Request to Pinnacle API
#'
#' @param url A character string.
#' @param flatten A logical passed to `httr2::resp_body_json()`.
#'
#' @return A data frame containing the extracted body from response
#' @export
#'
#' @examples
#' \dontrun{
#' # nhl endpoint
#' url <- https://guest.api.arcadia.pinnacle.com/0.1/leagues/1456
#'
#' # make get request
#' pinnacle_request(url)
#' }
#'
pinnacle_request <- function(url, flatten = FALSE) {

  httr2::request(url) |>
    httr2::req_headers(
      "Referer" = "https://www.pinnacle.com/",
      "X-Session" = "Vqlf7eLQOFpEeUYxrXlGfd9YMXj7xYGH",
      "X-API-Key" = "CmX2KcMrXuFmNg6YFbmTxE0y9CIrOi0R"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = flatten)

}



## make request to pinnacle api ----
pinnacle_api <- function(type = "markets") {

  pinn_type <- switch(
    type,
    "markets" = "/markets/straight",
    # "marketsx" = "/markets/related/straight",
    "matchups" = "/matchups"
  )

  flatten_resp <- dplyr::if_else(type == "markets", TRUE, FALSE)
  pinn_league_id <- "leagues/1456"
  pinn_url <- glue::glue("{oddsR_base_urls$pinnacle}{pinn_league_id}{pinn_type}")

  pinnacle_request(url = pinn_url, flatten = flatten_resp)

}


### extract game odds
pinnacle_markets <- function() {

  pinn_resp <- pinnacle_api(type = "markets")

  pinn_json_raw <- tibble::as_tibble(pinn_resp) |>
    dplyr::transmute(
      date_time = lubridate::ymd_hms(cutoffAt, quiet = TRUE, tz = "EST5EDT"),
      matchup_id = matchupId,
      version,
      prices,
      type,
      is_alternate = isAlternate,
      side,
      period,
      key
    )

  # period_look_up <- tribble(
  #   ~period, ~details,
  #   0L,      "OT INCLUDED",
  #   1L,      "1ST PERIOD",
  #   6L,      "REGULATION TIME",
  # )

  # don't include type == "team_total"
  pinn_tidy <- pinn_json_raw |>
    dplyr::filter(
      type %in% c("moneyline", "total", "spread"),
      is_alternate == FALSE,
      period == 0L
    ) |>
    tidyr::unnest_wider(prices) |>
    tidyr::unnest(cols = c(designation, price, points), keep_empty = TRUE) |>
    dplyr::transmute(
      date_time,
      matchup_id,
      version,
      type,
      venue = designation,
      points,
      price = dplyr::case_when(
        price > 0L ~ 1 + (price / 100),
        price < 0L ~ 1 - (100 / price),
        TRUE ~ NA_real_
      ),
      raw_prob = 1 / price
    ) |>
    dplyr::arrange(matchup_id, version)

  # calculate implied probabilities
  imp_probs <- pinn_tidy |>
    dplyr::group_by(type) |>
    dplyr::group_modify(~implied_probs(.x)) |>
    dplyr::ungroup()

  pinn_tidy |>
    dplyr::inner_join(imp_probs, by = c("matchup_id", "type", "venue")) |>
    dplyr::relocate(imp_prob, .before = price)

}


#' Calculate Implied Win Probabilities
#'
#' @param df A data frame.
#' @param id_col Column (quoted)  in `df` containing IDs.
#' @param x Column (quoted)  in `df` containing `id_col` groupings.
#' @param odds_col Column (quoted) in `df` containing odds (must be in decimal format).
#'
implied_probs <- function(df,
                          id_col = "matchup_id",
                          x = "venue",
                          odds_col = "price") {

  # calculate implied probabilities
  to_imp_probs <- tidyr::pivot_wider(
    data = df[, c(id_col, x, odds_col)],
    names_from = .data[[x]],
    values_from = .data[[odds_col]]
  )

  imp_probs_mat <- implied::implied_probabilities(
    odds = to_imp_probs[,c(2, 3)],
    method = "power"
  )

  names_vec <- colnames(imp_probs_mat$probabilities)

  imp_probs_df <- tibble::tibble(
    "{id_col}" := to_imp_probs[,id_col, drop = T],
    "{names_vec[1]}" := imp_probs_mat$probabilities[,1],
    "{names_vec[2]}" := imp_probs_mat$probabilities[,2],
    vig = imp_probs_mat$margin
  )

  imp_probs_df |>
    tidyr::pivot_longer(
      cols = -c(.data[[id_col]], .data[["vig"]]),
      names_to = x,
      values_to = "imp_prob"
    ) |>
    dplyr::relocate(vig, .after = imp_prob)

}


### extract team names, venue; return with match up ids ----
pinnacle_matchups <- function() {

  pinn_resp <- pinnacle_api(type = "matchups")

  matchup_tidy <- tibble::tibble(id = 1:length(pinn_resp)) |>
    dplyr::mutate(
      matchup_id = purrr::map(id, ~purrr::pluck(pinn_resp, .x, "parent", "id")),
      matchup_null = purrr::map_lgl(matchup_id, ~is.null(.x)),
      teams = purrr::map(id, ~dplyr::bind_rows(purrr::pluck(pinn_resp, .x, "parent", "participants"))),
      teams_null = purrr::map_lgl(teams, ~is.null(.x))
    ) |>
    dplyr::filter(!matchup_null, !teams_null) |>
    tidyr::unnest(matchup_id) |>
    tidyr::unnest_wider(teams) |>
    tidyr::unnest(cols = c(alignment, name)) |>
    dplyr::mutate(
      name = dplyr::case_when(
        name == "St Louis Blues" ~ "St. Louis Blues",
        TRUE ~ name
      )
    )

  matchup_tidy |>
    dplyr::distinct(matchup_id,  venue = alignment, name) |>
    dplyr::transmute(
      matchup_id,
      venue,
      team = nhldata::nhl_lookup(x = name, inp = team_full_name, out = team_abbr)
    )

}

tidy_pinnacle <- function() {

  matchups <- pinnacle_matchups()
  markets <- pinnacle_markets()

  markets_split <- markets |>
    dplyr::select(-version) %>%
    split(.$type)

  ml_wide <- markets_split$moneyline |>
    tidyr::pivot_wider(
      id_cols = c(matchup_id, type),
      names_from = venue,
      names_glue = "{venue}_{.value}_ml",
      values_from = c("imp_prob", "price", "raw_prob", "vig")
    ) |>
    dplyr::select(-c(dplyr::contains("vig"), dplyr::contains("price"),
                     dplyr::contains("raw"), type))

  pl_wide <- markets_split$spread |>
    tidyr::pivot_wider(
      id_cols = c(matchup_id, type),
      names_from = venue,
      names_glue = "{venue}_{.value}_pl",
      values_from = c("points", "imp_prob", "price", "raw_prob", "vig")
    ) |>
    dplyr::select(-c(dplyr::contains("vig"), dplyr::contains("price"),
                     dplyr::contains("raw"), type))

  totals_wide <- markets_split$total |>
    tidyr::pivot_wider(
      id_cols = c(matchup_id, type),
      names_from = venue,
      names_glue = "{venue}_{.value}",
      values_from = c("points", "imp_prob", "price", "raw_prob", "vig")
    ) |>
    dplyr::select(-c(dplyr::contains("vig"), dplyr::contains("price"),
                     dplyr::contains("raw"), type))

  markets |>
    dplyr::distinct(date_time, matchup_id) |>
    dplyr::inner_join(
      tidyr::pivot_wider(
        data = matchups,
        names_from = venue,
        names_glue = "{venue}_team",
        values_from = team),
      by = "matchup_id") |>
    dplyr::inner_join(ml_wide, by = "matchup_id") |>
    dplyr::inner_join(pl_wide,  by = "matchup_id") |>
    dplyr::inner_join(totals_wide, by = "matchup_id") |>
    dplyr::arrange(date_time)

}



# x <- pinn_resp[[1]][["league"]][["nhl"]]
# names(x)
#
# bet_categories <- tibble::tibble(id = 1:length(pinn_resp)) |>
#   mutate(
#     bet_details = map(pinn_resp, ~as_tibble(pluck(.x, "special"))),
#     type = map_chr(pinn_resp, ~pluck(.x, "type")),
#     units = map(pinn_resp, ~pluck(.x, "units"))
#   ) |>
#   unnest_wider(bet_details) |>
#   mutate(category = snakecase::to_snake_case(category))
#
# bet_categories |>
#   count(category, type)
#
# matchup_types <- bet_categories |>
#   filter(type == "matchup") |>
#   pull(id)
#
# pinn_resp[[105]] |> names()
#
#
#
# x <- tibble::tibble(id = 1:length(pinn_resp)) |>
#   mutate(bet_details = map(pinn_resp, ~pluck(.x, "parent", "participants"))) |>
#   filter(map_lgl(bet_details, ~!is.null(.x))) |>
#   mutate(bet_details = map(bet_details, ~bind_rows(.x))) |>
#   unnest_wider(bet_details) |>
#   rename(venue = alignment, team_full_name = name)
#
# x |>
#   mutate(team_full_name = map2(venue, team_full_name, ~tibble(.y, .name_repair = ~vctrs::vec_as_names(.x)))) |>
#   unnest_wider(team_full_name) |>
#   unnest_wider(value)



# Munge -------------------------------------------------------------------

# tibble(
#   id = 1:length(pinn_resp)
# ) |>
#   mutate(
#     matchup_id = map(id, ~pluck(pinn_resp, .x, "parent", "id")),
#     version = map(id, ~pluck(pinn_resp, .x, "version")),
#     type = map(id, ~pluck(pinn_resp, .x, "type")),
#     units = map(id, ~pluck(pinn_resp, .x, "units")),
#     category = map(id, ~pluck(pinn_resp, .x, "special", "category")),
#     description = map(id, ~pluck(pinn_resp, .x, "special", "description"))
#   ) |>
#   unnest(c(matchup_id, category, description, type, units, version), keep_empty = TRUE) |>
#   filter(type == "matchup")
#
# as_tibble(pluck(pinn_resp, 693, "parent")) |>
#   unnest(participants) |>
#   unnest(participants)

