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
#' @importFrom magrittr "%>%"

#! @export
nas_speeds <- function(year=2022,series=1) {
  tryCatch(
    expr={
      race_ids <-
        readr::read_csv("./data/misc/xwalk_race_ids.csv") %>%
        dplyr::filter(race_season == year & series_id == series)
      races <- race_ids$race_id
      if (length(races)==0) {
        stop("No lap time data found for the specified year")
      }
      all_races <- list()
      counter=1
      no_data = c(); l=1;
      for(r in races) {
        message(r)
        tmp = paste0("https://cf.nascar.com/cacher/",year,"/",series,"/",r,"/lap-times.json")
        lap_times <- try(jsonlite::read_json(path = tmp))

        if(class(lap_times)=="try-error") {
          no_data[l] <- r; l = l+1;
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
    },
    error = function(e) {
      message(paste0("Invalid Arguments"))
    },
    warning = function(w) {
    },
    finally = {
      if(exists("no_data")) {
        message(paste0("No lap time data found for races: ", paste0(no_data,collapse=", ")))
      }
    }

  )
  return(all_races)
}


