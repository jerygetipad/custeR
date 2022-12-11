#' Nas Speeds
#'
#' Gets Individual Lap Speed Data for the 2022 NASCAR Season
#'
#' @param year The year 2022.
#' @param series 1=Cup, 2=Xfinity, 3=Trucks.
#' @returns A list of data frames containing lap speeds.
#'
#' @examples
#' nas_speeds(year=2022,series=2)
#'

#! @export
nas_speeds <- function(year=2022,series=1) {
  # Read in specified races
  race_ids <-
    readr::read_csv("./data/misc/xwalk_race_ids.csv",show_col_types = FALSE) |>
    dplyr::filter(race_season == year & series_id == series)

  races <- race_ids$race_id

  if (length(races)==0 | !(year %in% c(2020:2022)) | !(series %in% c(1,2,3))) {
    stop("No lap time data found for the specified year/series")
  }
  lap_times <- lapply(1:length(races), function(x) {
    URL <- paste0("https://cf.nascar.com/cacher/",year,"/",series,"/",races[x],"/lap-times.json")

    data <- try(jsonlite::read_json(path = URL))
    if(inherits(data,"try-error")) {
      message(paste0("No data for race: ",races[x]))
      return(races[x])
    }
    # Separate Flags and Laps Data
    lap_times <- data$laps
    flags <- data$flags |>
      data.table::rbindlist()

    drivers <- length(lap_times)

    #
    suppressWarnings(
      lap_times <- lapply(1:drivers,
                          function(x) {
                            data.table::rbindlist(lap_times[[x]]$Laps) |>
                              dplyr::mutate(
                                Number = lap_times[[x]]$Number,
                                FullName = lap_times[[x]]$FullName,
                                Manufacturer = lap_times[[x]]$Manufacturer,
                                NASCARDriverID = lap_times[[x]]$NASCARDriverID,
                                Finish = lap_times[[x]]$RunningPos
                              )
                          }) |>
        data.table::rbindlist(fill=TRUE) |>
        dplyr::mutate(
          race_id = races[x],
          # Clean name strings
          FullName = stringr::str_remove_all(FullName," \\#"),
          FullName = stringr::str_remove_all(FullName,"\\* "),
          FullName = stringr::str_remove_all(FullName,"\\(i\\)"),
          FullName = stringr::str_remove_all(FullName," \\(P\\)"),
          FullName = stringr::str_remove_all(FullName,"\\.")
        ) |>
        dplyr::left_join(flags,by=c("Lap"="LapsCompleted"))
    )
    return(lap_times)
  })
  return(lap_times)
}



