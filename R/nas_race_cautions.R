#' Nas Race Cautions
#'
#' Gets Race Cautions for any season after 2018
#'
#' @param year 2022
#' @returns A data frame containing race cautions data
#'
#' @examples
#' nas_race_cautions(year=2022)
#'

#! @export
nas_race_cautions <- function(year) {
  if (year < 2018 | year > 2023) {
    stop("No data found for the specified year")
  }
  url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/race_cautions/{year}_race_cautions.csv")

  race_cautions <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(race_cautions,"try-error")) {
    message(paste0("No data for season: ", year))
    return()
  }
  return(race_cautions)
}
