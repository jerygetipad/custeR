#' Nascar Race List
#'
#' Pulls Race List for seasons and series mentioned from NASCAR.com. This dataset contains the race ids.
#'
#' @param year The year 2022.
#' @param series 1=Cup, 2=Xfinity, 3=Trucks.
#' @returns A data frame of race information.
#'
#' @examples
#' nas_race_list(year=2022,series=2)
#' nas_race_list(year=c(2021,2022),series=c(1,2,3))

#' @export
nas_race_list <- function(year=2022, series=1) {
  for(yr in year) {
    url <- paste0("https://cf.nascar.com/cacher/",yr,"/race_list_basic.json")
    schedule_feed <- try(jsonlite::read_json(url),silent=TRUE)
    if(class(schedule_feed)[1]=="try-error") {
      stop(paste0("Invalid Arguments or No data available for the given year: ", yr))
    }
    srs <- paste0("series_",series)

    for(s in srs) {
      if(is.null(schedule_feed[[s]])) {next}
      data <- schedule_feed[[s]]
      races <- length(data)

      for(r in 1:races) {
        tmpdata <- data[[r]]
        tmpdata[c("schedule","infractions")] <- NULL
        data[[r]] <- tmpdata
      }
      data <- data.table::rbindlist(data)
      data <- data |> dplyr::distinct()
    }
  }
  return(data)
}

