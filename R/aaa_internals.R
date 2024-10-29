#' @noRd
.file_reader <- function(file_url) {
  suppressWarnings(
    tryCatch(readRDS(url(file_url)), error = function(e) data.frame())
  )
}

#' @noRd
.load_page <- function(page_url) {
  agent <- getOption("foobar.agent", default = "R")
  ua <- httr::user_agent(agent)
  session <- rvest::session(url = page_url, ua)
  xml2::read_html(session)
}

#' @noRd
get_content <- function(url) {
  resp <- httr::GET(url)
  ## suppress encoding messages
  suppressMessages(cont <- httr::content(resp, as = "text"))
  jsonlite::fromJSON(cont)
}

#' @noRd
safely_get_content <- purrr::safely(get_content, otherwise = NULL, quiet = TRUE)
