#' Get Pit Data
#'
#' Gets Pit Stop Data for the 2022 NASCAR Season
#'
#' @param year The year 2022.
#' @param series 1=Cup, 2=Xfinity, 3=Trucks.
#' @returns A data frame of pit stops.
#'
#' @examples
#' get_pits(year=2022,series=2)

#' @export
get_pits <- function(year=2022,series=1) {
  tmp = paste0("./data/race_list/series_",series,"/race_list_",year,".csv")
  race_list <- readr::read_csv(tmp)
  races = race_list$race_id[race_list$race_type_id==1 & race_list$inspection_complete==1]
  #if(update==TRUE) {
  #  files = list.files(path=paste0("./files/pits/series_",series,"/",year,"/"),pattern = ".csv")
  #  files = as.numeric(stringr::str_remove(files,".csv"))
  #  races = races[!(races %in% files)]
  #}
  masterpits <- data.frame()
  for(r in races) {
    message(r)
    tmp= paste0("https://cf.nascar.com/cacher/live/series_",series,"/",r,"/live-pit-data.json")
    pits <- jsonlite::read_json(path = tmp)
    pits <- do.call(rbind, lapply(pits, data.frame))
    if(year == 2022) {
      pits$id <- row.names(pits)
      pitsYellow <-
      pits %>%
      filter(pit_in_flag_status == 2 & pit_out_flag_status == 2 &
               pit_stop_type == "FOUR_WHEEL_CHANGE") %>%
      group_by(leader_lap,lap_count) %>%
      mutate(cars_pitting = n()) %>%
      filter(cars_pitting > 1) %>%
      group_by(leader_lap,lap_count) %>%
      mutate(
        in_rank_pitcycle = rank(pit_in_rank),
        out_rank_pitcycle = rank(pit_out_rank),
        spots_pitcycle = in_rank_pitcycle - out_rank_pitcycle
      ) %>%
      ungroup(leader_lap, lap_count) %>%
      select(id, spots_pitcycle, cars_pitting)

    pits <- left_join(pits,pitsYellow,by="id")
    pits$id <- NULL
    pits$raceid <- r
    }
    masterpits <- rbind(masterpits,pits)
    #write_csv(x = pits,file = paste0("./files/pits/series_",series,"/",year,"/",r,".csv"))
  }
  return(masterpits)
}
