#' Load Nas Weekend Results
#'
#' Gets weekend results for any season after 2015
#'
#' @param year 2022
#' @param time_pause Wait time (in seconds) between page loads
#' @returns A data frame containing weekend data
#'
#' @examples
#' load_nas_weekend(year=2022)
#'
#' @export

load_nas_weekend <- function(year, time_pause=1) {

  # Check For Valid Time Value
  if(time_pause < 0) {
    stop("Invalid time_pause value")
  }
  #

  if (year < 2015 | year > format(Sys.Date(), "%Y")) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/weekend_runs/{year}_weekend_runs.csv")

  weekend <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(weekend,"try-error")) {
    stop(paste0("No data for season: ", year))
  }
  return(weekend)
}
