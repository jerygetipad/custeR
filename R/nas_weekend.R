#' Nas Weekend Results
#'
#' Gets weekend results for any season after 2015
#'
#' @param year 2022
#' @returns A data frame containing weekend data
#'
#' @examples
#' nas_weekend(year=2022)
#'
#' @export

nas_weekend <- function(year) {
  if (year < 2015 | year > 2023) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/weekend_runs/{year}_weekend_runs.csv")

  weekend <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(weekend,"try-error")) {
    message(paste0("No data for season: ", year))
    return()
  }
  return(weekend)
}
