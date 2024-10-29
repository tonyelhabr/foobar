## TODO: Cache this.
#' @importFrom utils read.csv
.foobar_load_csv <- function(suffix) {
  utils::read.csv(
    sprintf("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/%s", suffix)
  )
}

.foobar_get_league_urls <- function(
    leagues,
    league_id = NULL,
    country = NULL,
    league_name = NULL
) {
  has_country <- !is.null(country)
  has_league_name <- !is.null(league_name)
  has_league_id <- !is.null(league_id)
  if(!has_league_id & !(has_country & has_league_name)) {
    stop(
      "Must provide `league_id` or both of `country` and `league_name`."
    )
  }

  has_country_and_league_name <- has_country & has_league_name
  urls <- if(has_country_and_league_name) {
    n_country <- length(country)
    n_league_name <- length(league_name)
    if(n_country != n_league_name) {
      stop(
        sprintf(
          "If providing `country` and `league_name`, length of each must be the same (%s != %s).",
          n_country,
          n_league_name
        )
      )
    }

    pairs <- list(
      country = country,
      league_name = league_name
    ) |>
      purrr::transpose()

    purrr::map_dfr(
      pairs,
      ~dplyr::filter(
        leagues,
        .data[["ccode"]] == .x$country, .data[["name"]] == .x$league_name
      )
    )
  } else {
    leagues |>
      dplyr::filter(.data[["id"]] %in% league_id)
  }

  n_urls <- nrow(urls)
  if(n_urls == 0) {
    stop(
      "Could not find any leagues matching specified parameters."
    )
  }

  n_params <- ifelse(
    has_country_and_league_name,
    n_country,
    length(league_id)
  )

  if(n_urls < n_params) {
    warning(
      sprintf(
        "Found less leagues than specified (%s < %s).",
        n_urls,
        n_params
      )
    )
  } else if (n_urls > n_params) {
    warning(
      sprintf(
        "Found more leagues than specified (%s > %s).",
        n_urls,
        n_params
      )
    )
  }
  urls
}


#' Get foobar league ids
#'
#' Returns a dataframe of the league ids available on foobar
#'
#' @param cached Whether to load the dataframe from the \href{https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/foobar-leagues/all_leagues.csv}{data CSV}. This is faster and most likely what you want to do, unless you identify a league that's being tracked by foobar that's not in this pre-saved CSV.
#'
#' @export
foobar_get_league_ids <- function(cached = TRUE) {
  if (isTRUE(cached)) {
    return(.foobar_load_csv("foobar-leagues/all_leagues.csv"))
  }

  resp <- httr::POST("https://www.fotmob.com/api/allLeagues")
  cont <- httr::content(resp)

  .extract_leagues <- function(x) {
    cont[[x]] |>
      tibble::enframe() |>
      dplyr::select(tidyselect::vars_select_helpers$all_of("value")) |>
      tidyr::unnest_wider(tidyselect::vars_select_helpers$all_of("value")) |>
      tidyr::unnest_longer(tidyselect::vars_select_helpers$all_of("leagues")) |>
      dplyr::select(tidyselect::vars_select_helpers$all_of(c("ccode", "name", "leagues"))) |>
      dplyr::rename(
        country = .data[["name"]]
      ) |>
      tidyr::unnest_wider(tidyselect::vars_select_helpers$all_of("leagues")) |>
      janitor::clean_names() |>
      dplyr::select(tidyselect::vars_select_helpers$all_of(c("ccode", "country", "id", "name", "page_url")))
  }

  purrr::map_dfr(
    c(
      "international",
      "countries"
    ),
    .extract_leagues
  )
}


.foobar_get_league_ids <- function(cached = TRUE, ...) {
  leagues <- foobar_get_league_ids(cached = cached)
  .foobar_get_league_urls(
    leagues = leagues,
    ...
  )
}

#' @noRd
.foobar_get_league_resp <- function(league_id, page_url, season = NULL, fallback = TRUE) {
  url <- httr::parse_url("https://www.fotmob.com/api/leagues")
  url$query <- list(
    "id" = league_id,
    "season" = season
  )
  url <- httr::build_url(url)
  resp <- safely_get_content(url)
  if(!is.null(resp$result)) {
    return(resp$result)
  }

  stop(
    sprintf("Could not identify the league endpoint at %s. Error:\n", url, resp$error)
  )
}

#' Get foobar match results by league
#'
#' Returns match status given a league and season
#'
#' @param country Three character country code. Can be one or multiple. If provided, `league_name` must also be provided (of the same length)
#' @param league_name League names. If provided, `country` must also be provided (of the same length).
#' @param league_id foobar ID for the league. Only used if `country` and `league_name` are not specified.
#' @param season Season, e.g. `"2021/2022"`. Can be one or multiple. If left as `NULL` (default), data for the latest season available will be pulled.
#' @inheritParams foobar_get_league_ids
#'
#' @return returns a dataframe of league matches
#'
#' @export
#'
#' @examples
#' \donttest{
#' try({
#' library(dplyr)
#' library(tidyr)
#'
#' # one league
#' foobar_get_league_matches(
#'   country = "ENG",
#'   league_name = "Premier League"
#' )
#'
#' # one league, by id
#' foobar_get_league_matches(
#'   league_id = 47
#' )
#'
#' # can specify past seasons
#' foobar_get_league_matches(
#'   country = "GER",
#'   league_name = "1. Bundesliga",
#'   season = "2020/2021"
#' )
#'
#' # multiple leagues (could also use ids)
#' league_matches <- foobar_get_league_matches(
#'   country =     c("ENG",            "ESP"   ),
#'   league_name = c("Premier League", "LaLiga")
#' )
#'
#' # probably the data that you care about
#' league_matches |>
#'   dplyr::select(match_id = id, home, away) |>
#'   tidyr::unnest_wider(c(home, away), names_sep = "_")
#' })
#' }
foobar_get_league_matches <- function(country, league_name, league_id, season = NULL, cached = TRUE) {

  urls <- .foobar_get_league_ids(
    cached = cached,
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL)
  )

  # Need to coerce to `NA_character` since crossing doesn't like `NULL`
  season <- ifelse(is.null(season), NA_character_, season)
  urls <- tidyr::crossing(
    urls,
    "season" = season
  )

  purrr::pmap_dfr(
    list(
      urls$id,
      urls$page_url,
      urls$season
    ),
    .foobar_get_league_matches
  )
}

#' @noRd
.foobar_message_for_season <- function(resp, season = NULL) {

  if (is.null(season)) {
    rlang::inform(
      glue::glue('Defaulting `season` to latest ("{resp$allAvailableSeasons[1]}").'),
      .frequency = "once",
      .frequency_id = ".foobar_get_league_(matches|tables)"
    )
  } else {
    if (isFALSE(season %in% resp$allAvailableSeasons)) {
      stop(
        glue::glue(
          "`season` should be one of the following:\n{glue::glue_collapse(resp$allAvailableSeasons, '\n')}"
        )
      )
    }
  }

}

#' @noRd
.foobar_get_league_matches <- function(league_id, page_url, season = NULL) {
  # And now coerce NA back to NULL
  season <- switch(!is.na(season), season, NULL)
  resp <- .foobar_get_league_resp(
    league_id = league_id,
    page_url = page_url,
    season = season
  )
  .foobar_message_for_season(resp, season)
  matches <- resp$matches$allMatches
  matches |>
    janitor::clean_names() |>
    tibble::as_tibble()
}

#' Get standings from foobar
#'
#' Returns league standings from foobar.com. 4 types are returned: all, home, away, form
#'
#' @inheritParams foobar_get_league_matches
#'
#' @return returns a dataframe of league standings
#'
#' @export
#'
#' @examples
#' \donttest{
#' try({
#' library(dplyr)
#' library(tidyr)
#'
#' # one league
#' foobar_get_league_tables(
#'   country = "ENG",
#'   league_name = "Premier League"
#' )
#'
#' # one league, by id
#' foobar_get_league_tables(
#'   league_id = 47
#' )
#'
#' # one league, past season
#' foobar_get_league_tables(
#'   country = "GER",
#'   league_name = "1. Bundesliga",
#'   season = "2020/2021"
#' )
#'
#' # multiple leagues (could also use ids)
#' league_tables <- foobar_get_league_tables(
#'   country =     c("ENG",            "ESP"   ),
#'   league_name = c("Premier League", "LaLiga")
#' )
#'
#' # look at tables if only away matches are considered
#' league_tables |>
#'   dplyr::filter(table_type == "away")
#' })
#' }
foobar_get_league_tables <- function(country, league_name, league_id, season = NULL, cached = TRUE) {

  urls <- .foobar_get_league_ids(
    cached = cached,
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL)
  )

  season <- ifelse(is.null(season), NA_character_, season)
  urls <- tidyr::crossing(
    urls,
    "season" = season
  )

  purrr::pmap_dfr(
    list(
      urls$id,
      urls$page_url,
      urls$season
    ),
    .foobar_get_league_tables
  )
}
#' @noRd
.foobar_get_league_tables <- function(league_id, page_url, season = NULL) {

  season <- switch(!is.na(season), season, NULL)
  resp <- .foobar_get_league_resp(
    league_id = league_id,
    page_url = page_url,
    season = season
  )
  .foobar_message_for_season(resp, season)

  table_init <- resp$table$data

  table <- if("table" %in% names(table_init)) {
    cols <- purrr::flatten_chr(table_init$tableFilterTypes)
    table_init$table |> dplyr::select(tidyselect::vars_select_helpers$all_of(cols))
  } else if("tables" %in% names(table_init)) {
    ## leagues like the MLS don't have the "form" table type quite yet, although we could
    ##   theoretically get form from the `teamForm` element (which would require different post-processing).
    cols <- c("all", "home", "away")
    tables <- dplyr::bind_rows(table_init$tables)
    tables$all <- tables$table$all
    tables$home <- tables$table$home
    tables$away <- tables$table$away
    tables |>
      dplyr::rename(
        group_id = .data[["leagueId"]],
        group_page_url = .data[["pageUrl"]],
        group_name = .data[["leagueName"]]
      ) |>
      dplyr::select(
        -c(.data[["table"]], .data[["legend"]])
      )
  } else {
    stop(
      "Expected to find `table` or `tables` element but did not."
    )
  }
  table <- table |>
    janitor::clean_names() |>
    tibble::as_tibble()

  res <- table |>
    tidyr::pivot_longer(
      dplyr::all_of(cols),
      names_to = "table_type",
      values_to = "table"
    ) |>
    tidyr::unnest_longer(
      .data[["table"]]
    ) |>
    tidyr::unnest(
      .data[["table"]],
      names_sep = "_"
    ) |>
    janitor::clean_names() |>
    tibble::as_tibble()

  res |>
    dplyr::mutate(
      league_id = !!league_id,
      page_url = !!page_url,
      .before = 1
    )
}
