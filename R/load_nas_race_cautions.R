#' Load Nas Race Cautions
#'
#' Gets Load Race Cautions for any season after 2018
#'
#' @param year 2022
#' @param time_pause Wait time (in seconds) between page loads
#' @returns A data frame containing race cautions data
#'
#' @examples
#' load_nas_race_cautions(year=2022)
#'

#' @export
load_nas_race_cautions <- function(year, time_pause=1) {

  # Check For Valid Time Value
  if(time_pause < 0) {
    stop("Invalid time_pause value")
  }

  if (year < 2018 | year > 2023) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/race_cautions/{year}_race_cautions.csv")

  Sys.sleep(time_pause)

  race_cautions <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(race_cautions,"try-error")) {
    message(paste0("No data for season: ", year))
    return()
  }
  return(race_cautions)
}
