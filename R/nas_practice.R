#' Nascar Practice Data
#'
#' Pulls Practice Lap Averages for seasons and series mentioned from NASCAR.com. This dataset contains the race ids.
#'
#' @param years The year 2022.
#' @param series 1=Cup, 2=Xfinity, 3=Trucks.
#' @returns A data frame of race information.
#'
#' @examples
#' nas_practice(year=2022,series=2)
#' nas_practice(year=c(2021,2022),series=c(1,2,3))

#' Get Pit Data
#'
#' Gets Pit Stop Data for the 2022 NASCAR Season from NASCAR.com
#'
#' @param year The year 2022.
#' @param series 1=Cup, 2=Xfinity, 3=Trucks.
#' @returns A data frame of pit stops.
#'
#' @examples
#' nas_pits(year=2022,series=2)

#' @export
nas_practice <- function(year=2022,series=1) {
  tmp = paste0("./data/race_list/series_",series,"/race_list_",year,".csv")
  race_list <- readr::read_csv(tmp)
  races = race_list$race_id[race_list$race_type_id==1]

  all_races <- list()
  counter <- 1
  for(r in races) {
    message(r)
    url <- paste0("https://cf.nascar.com/cacher/",year,"/",series,"/",r,"/lap-averages.json")
    raw_practice <- try(jsonlite::read_json(path = url))
    if(class(raw_practice)=="try-error") {
      message(paste0("no practice data for ",r))
      next
    }
    practice_type <- unlist(lapply(1:length(raw_practice),function(x) raw_practice[[x]]$Title))
    practice <- data.frame()
    for(i in 1:length(practice_type)) {
      tmp <- raw_practice[[i]]$Items
      tmp <- data.table::rbindlist(tmp)
      tmp[tmp==999] <- NA
      tmp$PracticeName <- practice_type[i]
      practice <- rbind(practice,tmp)
    }
    practice$race_id <- r

    all_races[[counter]] <- practice
    counter <- counter + 1
    #write_csv(x = pits,file = paste0("./files/pits/series_",series,"/",year,"/",r,".csv"))
  }
  return(all_races)
}

