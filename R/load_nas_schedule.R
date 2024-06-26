#' Load Nas Schedule
#'
#' Gets Schedule results for any season after 2019
#'
#' @param year 2022
#' @param time_pause Wait time (in seconds) between page loads
#' @returns A data frame containing schedule data
#'
#' @examples
#' load_nas_schedule(year=2022)
#'

#' @export
load_nas_schedule <- function(year, time_pause=1) {

  # Check For Valid Time Value
  if(time_pause < 0) {
    stop("Invalid time_pause value")
  }
  #

  if (year < 2015 | year > format(Sys.Date(), "%Y")) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/schedule/{year}_schedule.csv
")

  Sys.sleep(time_pause)

  schedule <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(schedule,"try-error")) {
    stop(paste0("No data for season: ", year))
  }
  return(schedule)
}
