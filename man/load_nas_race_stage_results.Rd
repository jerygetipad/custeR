% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_nas_race_stage_results.R
\name{load_nas_race_stage_results}
\alias{load_nas_race_stage_results}
\title{Load Nas Stage Race Results}
\usage{
load_nas_race_stage_results(year, time_pause = 1)
}
\arguments{
\item{year}{2022}

\item{time_pause}{Wait time (in seconds) between page loads}
}
\value{
A data frame containing race stage results data
}
\description{
Gets Stage Race results for any season after 2019
}
\examples{
load_nas_race_stage_results(year=2022)

}
