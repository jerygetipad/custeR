#' Nas Race Infractions
#'
#' Gets Race Infractions for any season after 2018
#'
#' @param year 2022
#' @returns A data frame containing race infractions data
#'
#' @examples
#' nas_race_infractions(year=2022)
#'

#! @export
nas_race_infractions <- function(year) {
  if (year < 2018 | year > 2023) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/race_infractions/{year}_race_infractions.csv")

  race_infractions <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(race_infractions,"try-error")) {
    message(paste0("No data for season: ", year))
    return()
  }
  return(race_infractions)
}
