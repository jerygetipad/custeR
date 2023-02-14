#' Nas Schedule
#'
#' Gets Schedule results for any season after 2019
#'
#' @param year 2022
#' @returns A data frame containing schedule data
#'
#' @examples
#' nas_schedule(year=2022)
#'

#! @export
nas_schedule <- function(year) {
  if (year < 2015 | year > 2023) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/schedule/{year}_schedule.csv
")

  schedule <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(schedule,"try-error")) {
    message(paste0("No data for season: ", year))
    return()
  }
  return(schedule)
}
