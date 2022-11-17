#' Nascar Race List
#'
#' Pulls Race List for seasons and series mentioned from NASCAR.com. This dataset contains the race ids.
#'
#' @param years The year 2022.
#' @param series 1=Cup, 2=Xfinity, 3=Trucks.
#' @returns A data frame of race information.
#'
#' @examples
#' nas_race_list(year=2022,series=2)
#' nas_race_list(year=c(2021,2022),series=c(1,2,3))

#' @export
nas_race_list <- function(year=2022, series=1) {
  for(yr in year) {
    URL = paste0("https://cf.nascar.com/cacher/",yr,"/race_list_basic.json")
    schedule_feed=jsonlite::read_json(URL)
    srs = paste0("series_",series)

    for(i in srs) {
      if(is.null(schedule_feed[[i]])) {next}
      data=schedule_feed[[i]]
      len = length(data)

      for(j in 1:len) {
        tmpdata = data[[j]]
        tmpdata[c("schedule","infractions")] <- NULL
        data[[j]] <- tmpdata
      }
      data=data.table::rbindlist(data)
      data= data %>% distinct()
    }
  }
}

