% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leagues.R
\name{foobar_get_league_ids}
\alias{foobar_get_league_ids}
\title{Get foobar league ids}
\usage{
foobar_get_league_ids(cached = TRUE)
}
\arguments{
\item{cached}{Whether to load the dataframe from the \href{https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/foobar-leagues/all_leagues.csv}{data CSV}. This is faster and most likely what you want to do, unless you identify a league that's being tracked by foobar that's not in this pre-saved CSV.}
}
\description{
Returns a dataframe of the league ids available on foobar
}
