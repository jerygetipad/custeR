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
nas_pits <- function(year=2022,series=1) {
  tmp = paste0("./data/race_list/series_",series,"/race_list_",year,".csv")
  race_list <- readr::read_csv(tmp)
  races = race_list$race_id[race_list$race_type_id==1]
  #if(update==TRUE) {
  #  files = list.files(path=paste0("./files/pits/series_",series,"/",year,"/"),pattern = ".csv")
  #  files = as.numeric(stringr::str_remove(files,".csv"))
  #  races = races[!(races %in% files)]
  #}
  masterpits <- data.frame()
  for(r in races) {
    message(r)
    tmp= paste0("https://cf.nascar.com/cacher/live/series_",series,"/",r,"/live-pit-data.json")
    pits <- try(jsonlite::read_json(path = tmp))
    if(class(pits)=="try-error") {
      message(paste0("no pit data for ",r))
      next
    }
    pits <- do.call(rbind, lapply(pits, data.frame))

    pits$id <- NULL
    pits$race_id <- r

    masterpits <- rbind(masterpits,pits)
    #write_csv(x = pits,file = paste0("./files/pits/series_",series,"/",year,"/",r,".csv"))
  }
  return(masterpits)
}

