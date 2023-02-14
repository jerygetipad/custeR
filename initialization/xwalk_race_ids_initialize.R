#' Creates a Crosswalk for race_ids from racing-reference.info and NASCAR.com
#'
#' Creates a Crosswalk for race_ids from racing-reference.info and NASCAR.com. Contains all races 2015-2023
#'
#' @returns data.frame
#'
#' @examples
#' xwalk_race_ids()
#' rr_loopdata(races=1, year=2022,series=2)

xwalk_race_ids <- function() {
  tryCatch(
    expr = {
      xwalk <- data.frame()
      for(yr in 2015:2023) {
        if(yr != 2017) {
          url = paste0("https://cf.nascar.com/cacher/",yr,"/race_list_basic.json")
          schedule_feed=jsonlite::read_json(url)
          series = paste0("series_",c(1,2,3))

          for(i in series) {
            if(is.null(schedule_feed[[i]])) {next}
            data <- schedule_feed[[i]]
            len <- length(data)
            # Points races only
            races = unlist(lapply(1:len,function(x) data[[x]]$race_type_id==1))
            data <- data[races]

            len <- length(data)
            for(j in 1:len) {
              tmpdata = data[[j]]

              tmpdata[c("schedule","infractions")] <- NULL
              tmpdata <- tmpdata[c("race_id","series_id","race_season")]
              data[[j]] <- tmpdata
            }
            data = data.table::rbindlist(data)
            data$rr_id <- sprintf("%02d", 1:len)
            xwalk <- rbind(xwalk,data) |>
              dplyr::distinct()
          }
        }
      }
    }
  )
  return(xwalk)
}
#xwalk_race_ids <- xwalk_race_ids()
#write(xwalk_race_ids,"./data/xwalk_race_ids.Rda")
