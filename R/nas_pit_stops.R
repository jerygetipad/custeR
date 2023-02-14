#' Nas Pit Stops
#'
#' Gets pit stop data for any season after 2020
#'
#' @param year 2022
#' @returns A data frame containing pit stop data
#'
#' @examples
#' nas_pit_stops(year=2022)
#'
#' @export
nas_pit_stops <- function(year) {
  if (year < 2020 | year > 2023) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/race_pit_stops/{year}_race_pit_stops.csv")

  pit_stops <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(pit_stops,"try-error")) {
    message(paste0("No data for season: ", year))
    return()
  }
  return(pit_stops)
}
