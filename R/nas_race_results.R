#' Nas Race Results
#'
#' Gets race results data for any season after 2018
#'
#' @param year 2022
#' @returns A data frame containing race results data
#'
#' @examples
#' nas_race_results(year=2022)
#'

#! @export
nas_race_results <- function(year) {
  if (year < 2018 | year > 2023) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/race_results/{year}_race_results.csv")

  race_results <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(race_results,"try-error")) {
    message(paste0("No data for season: ", year))
    return()
  }
  return(race_results)
}
