#' NAS Schedule
#'
#' Pulls Weekend Schedule for seasons mentioned.
#'
#' @param years The year 2022.
#' @returns A data frame of schedule information.
#'
#' @examples
#' nas_schedule(years=2022)
#' nas_schedule(years=c(2021,2022))

nas_schedule <- function(years=2022) {
  schedule_all <- data.frame()
  if (!(all(years %in% c(2017:2022)))) {
    stop("No lap time data found for the specified year/series")
  }
  for(i in years) {

    URL <- paste0("https://cf.nascar.com/cacher/",i,"/1/schedule-feed.json")

    schedule_year <- try(jsonlite::read_json(path = URL),silent = TRUE)

    if(inherits(schedule_year,"try-error")) {
      stop(paste0("No schedule data for year: ",i))
    }
    schedule_year <- schedule_year  |>
      data.table::rbindlist(fill=TRUE) |>
      dplyr::mutate(
        year=i
      )
    schedule_all <-
      rbind(
        schedule_all,
        schedule_year,
        fill=TRUE
     )
  }
  return(schedule_all)
}








