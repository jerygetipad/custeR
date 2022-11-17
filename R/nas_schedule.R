#' NAS Schedule
#'
#' Pulls Weekend Schedule for seasons mentioned.
#'
#' @param years The year 2022.
#' @returns A data frame of schedule information.
#'
#' @examples
#' nas_schedule(year=2022)
#' nas_schedule(year=c(2021,2022))

nas_schedule <- function(years=2022) {
  masterschedule <- data.frame()
  for(i in years) {
    url <- paste0("https://cf.nascar.com/cacher/",i,"/1/schedule-feed.json")
    schedule_feed <- jsonlite::read_json(url)
    schedule <- data.table::rbindlist(schedule_feed)
    schedule$year <- i
    masterschedule <- rbind(masterschedule,schedule)
    ##readr::write_csv(x=schedule,file=paste0("schedules/schedule_",i,".csv"))
  }
  return(masterschedule)
}







