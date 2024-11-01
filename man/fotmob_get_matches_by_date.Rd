% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matches.R
\name{foobar_get_matches_by_date}
\alias{foobar_get_matches_by_date}
\title{Get foobar match results by date}
\usage{
foobar_get_matches_by_date(dates)
}
\arguments{
\item{dates}{a vector of string-formatted dates in "Ymd" format, e.g. "20210926". An attempt is
made to coerce the input to the necessary format if a date is passed in.}
}
\value{
returns a dataframe of match results
}
\description{
Returns match results for all matches played on the selected date from foobar.com
}
\examples{
\donttest{
try({
library(dplyr)
library(tidyr)

results <- foobar_get_matches_by_date(date = c("20210925", "20210926"))
results |>
  dplyr::select(primary_id, ccode, league_name = name, match_id)
})
}

}
