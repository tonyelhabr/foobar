#' @noRd
.foobar_extract_meta <- function() {
  page <- rvest::read_html(BASE_URL)

  page |>
    rvest::html_elements("script") |>
    rvest::html_text(trim = TRUE) |>
    purrr::keep(stringr::str_detect, "props")
}

#' @noRd
.foobar_get_build_id <- function() {
  meta <- .foobar_extract_meta()
  meta |>
    stringr::str_extract('(?<=\\"buildId\\"[:]).*(?=\\,\\"isFallback\\")') |>
    safely_get_content() |>
    purrr::pluck("result")
}

#' @noRd
.wrap_foobar_match_f <- function(match_ids, f) {
  purrr::map_dfr(
    rlang::set_names(match_ids),
    f,
    .id = "match_id"
  ) |>
    dplyr::mutate(
      dplyr::across(.data[["match_id"]], as.integer)
    )
}
