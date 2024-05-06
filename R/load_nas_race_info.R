#' Load Nas Race Info
#'
#' Gets Race Info for any season after 2018
#'
#' @param year 2022
#' @param time_pause Wait time (in seconds) between page loads
#' @returns A data frame containing race info data
#'
#' @examples
#' load_nas_race_info(year=2022)
#'

#' @export
load_nas_race_info <- function(year, time_pause=1) {

  # Check For Valid Time Value
  if(time_pause < 0) {
    stop("Invalid time_pause value")
  }

  if (year < 2018 | year > format(Sys.Date(), "%Y")) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/race_info/{year}_race_info.csv")

  Sys.sleep(time_pause)

  race_info <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(race_info,"try-error")) {
    message(paste0("No data for season: ", year))
    return()
  }
  return(race_info)
}
