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

#! @export
nas_speeds <- function(year=2022,series=1) {
  race_ids <-
    readr::read_csv("./data/misc/xwalk_race_ids.csv") %>%
    dplyr::filter(race_season == year & series_id == series)

  races <- race_ids$race_id
  all_races <- list()
  counter=1
  for(r in races) {

    message(r)
    tmp = paste0("https://cf.nascar.com/cacher/",year,"/",series,"/",r,"/lap-times.json")
    lap_times <- try(jsonlite::read_json(path = tmp))
    if(class(lap_times)=="try-error") {
      message(paste0(r," has no lap time data"))
      next;
    }
    lap_times_laps <- lap_times$laps
    masterdf <- data.frame()
    for(drv in 1:length(lap_times_laps)) {
      len <- length(lap_times_laps[[drv]]$Laps)
      df <- data.frame(matrix(NA,nrow = len, ncol = 4))
      names(df) <- c("Lap", "LapTime", "LapSpeed", "RunningPos")
      for(i in 1:len) {
        df$Lap[i] = lap_times_laps[[drv]]$Laps[[i]]$Lap
        if(df$Lap[i]==0) {
          df$LapTime <- NA
          df$LapSpeed <- NA
        }
        else if(
          is.null(lap_times_laps[[drv]]$Laps[[i]]$LapTime) |
          is.null(lap_times_laps[[drv]]$Laps[[i]]$LapSpeed)) {
          df$LapTime <- NA
          df$LapSpeed <- NA
        }
        else {
          df$LapTime[i] <- lap_times_laps[[drv]]$Laps[[i]]$LapTime
          df$LapSpeed[i] <- lap_times_laps[[drv]]$Laps[[i]]$LapSpeed
        }
        df$RunningPos[i] <- lap_times_laps[[drv]]$Laps[[i]]$RunningPos
      }
      df$Number <- lap_times_laps[[drv]]$Number
      df$FullName <- lap_times_laps[[drv]]$FullName
      df$Manufacturer <- lap_times_laps[[drv]]$Manufacturer
      df$NASCARDriverID <- lap_times_laps[[drv]]$NASCARDriverID
      df$Finish <- lap_times_laps[[drv]]$RunningPos
      masterdf <- rbind(masterdf,df)
    }
    masterdf$race_id = r

    masterdf$FullName <- stringr::str_remove_all(masterdf$FullName,"\\* ")
    masterdf$FullName <- stringr::str_remove_all(masterdf$FullName," \\#")
    masterdf$FullName <- stringr::str_remove_all(masterdf$FullName,"\\(i\\)")
    masterdf$FullName <- stringr::str_remove_all(masterdf$FullName," \\(P\\)")
    # Fill laps not completed with NA's

    # Get Caution Laps
    # Lap Notes
    flags = lap_times$flags
    flags = data.table::rbindlist(flags)

    masterdf <- dplyr::left_join(masterdf, flags, by=c("Lap"="LapsCompleted"))
    all_races[[counter]] <- masterdf
    counter = counter+1
    #write_csv(x = masterdf,file = paste0("./files/speeds/series_",series,"/",year,"/",r,".csv"))
  }
  return(all_races)
}



