#' Nas Live Points
#'
#' Gets Live Points for any season after 2019
#'
#' @param year 2022
#' @returns A data frame containing lap time data
#'
#' @examples
#' nas_live_points(year=2022)
#'

#! @export
nas_live_points <- function(year) {
  if (year < 2019 | year > 2023) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/live_points/{year}_live_points.csv")

  live_points <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(live_points,"try-error")) {
    message(paste0("No data for season: ", year))
    return()
  }
  return(live_points)
}



