#' Nas Lap Time
#'
#' Gets Individual Lap Time Data for any race after 2020
#'
#' @param rid Race ID
#' @returns A data frame containing lap time data
#'
#' @examples
#' nas_lap_times(rid=5009)
#'
#' @importFrom rlang .data
#' @export

nas_lap_times <- function(rid) {
  lap_time_list <- lapply(rid, function(r) {
    # Read in specified races
    race_info <-
      readr::read_csv("./data/xwalk_race_ids.csv",show_col_types = FALSE) |>
      dplyr::filter(.data$race_id %in% r) |>
      dplyr::select(.data$race_id, .data$series_id, .data$race_season) |>
      unlist()

    if (length(race_info)==0) {
      stop("No data found for the specified race_id")
    }
    url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/lap_times/{race_info[3]}_{rid}_lap_times.csv")

    lap_times <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
    if(inherits(lap_times,"try-error")) {
      message(paste0("No data for race: ",r))
      return()
    }
    return(lap_times)
  })
  return(lap_time_list)
}



