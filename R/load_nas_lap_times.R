#' Load NAS Lap Time
#'
#' Loads Individual Lap Time Data for any race after 2020
#'
#' @param rid Race ID
#' @param time_pause Wait time (in seconds) between page loads
#' @returns A data frame containing lap time data
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' load_nas_lap_times(rid=5009)
#'
load_nas_lap_times <- function(rid, time_pause=1) {

  # Check For Valid Time Value
  if(time_pause < 0) {
    stop("Invalid time_pause value")
  }
  #
  time_wait = time_pause

  lap_time_list <- lapply(rid, function(r, time_pause=time_wait) {
    # Read in specified races
    race_info <-
      xwalk_race_ids |>
      dplyr::filter(.data$race_id %in% r) |>
      dplyr::select(.data$race_id, .data$series_id, .data$race_season) |>
      unlist()

    if (length(race_info)==0) {
      stop("No data found for the specified race_id")
    }
    url = glue::glue("https://raw.githubusercontent.com/armstjc/nascar-data-repository/main/nascar_api/lap_times/{race_info[3]}_{rid}_lap_times.csv")

    #
    Sys.sleep(time_pause)

    lap_times <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
    if(inherits(lap_times,"try-error")) {
      message(paste0("No data for race: ",r))
      return()
    }
    return(lap_times)
  })
  return(lap_time_list)
}



