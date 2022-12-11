#' Get Pit Data
#'
#' Gets Pit Stop Data for NASCAR Seasons 2017-2022 from NASCAR.com
#'
#' @param year Any year 2017-2022.
#' @param series 1=Cup, 2=Xfinity, 3=Trucks.
#' @returns A data frame of pit stops.
#'
#' @examples
#' nas_pits(year=2022,series=2)

#' @export
nas_pits <- function(year=2022,series=1) {
  # Check for valid arguments
  if ( !(year %in% c(2018:2022)) | !(series %in% c(1,2,3))) {
    stop("No lap time data found for the specified year/series")
  }
  # Get Race Id's for given year + series
  load("data/xwalk_race_ids.Rda")

  races <- xwalk_race_ids |>
    dplyr::filter(race_season==year & series_id==series) |>
    dplyr::select(race_id) |>
    unlist()

  if (length(races)==0) {
    stop("No lap time data found for the specified year")
  }
  #
  if(year %in% c(2018,2019)) {
    # Initialize Loop Variables
    masterpits <- data.frame()
    counter <- 1
    no_data <- c()
    l <- 1
    #
    pit_stops <- lapply(1:length(races), function(x) {
      message(races[x])
      url = paste0("https://cf.nascar.com/cacher/live/series_",series,"/",r,"/live-pit-data.json")
      suppressWarnings({
        pits <- try(jsonlite::read_json(path = url),silent=TRUE)
      })
      if(inherits(weekend_feed,"try-error")) {
        return(list())
      }
      pit_stops <- pits |>
        data.table::rbindlist() |>
        dplyr::mutate(race_id = races[x])
    }) |> data.table::rbindlist()
  } else {
    pit_stops <- lapply(1:length(races), function(x) {
      message(races[x])
      url=paste0("https://cf.nascar.com/cacher/",year,"/",series,"/",races[x],"/weekend-feed.json")
      suppressWarnings({
        weekend_feed <- try(jsonlite::read_json(path = url),silent=TRUE)
      })
      if(inherits(weekend_feed,"try-error")) {
        return(list())
      }
      pit_stops <- weekend_feed$weekend_race[[1]]$pit_reports |>
        data.table::rbindlist() |>
        dplyr::mutate(race_id = races[x])
    }) |> data.table::rbindlist()
  }
  return(pit_stops)
}

