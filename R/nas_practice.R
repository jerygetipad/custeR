#' Nascar Practice Data
#'
#' Pulls Practice Lap Averages for seasons and series mentioned from NASCAR.com. This dataset contains the race ids.
#'
#' @param year The year 2022.
#' @param series 1=Cup, 2=Xfinity, 3=Trucks.
#' @returns A data frame of race information.
#'
#' @examples
#' nas_practice(year=2022,series=2)
#' nas_practice(year=c(2021,2022),series=c(1,2,3))
#'
#' @importFrom magrittr "%>%"


#' @export
nas_practice <- function(year=2022,series=1) {
  # Get Race Id's for given year + series
  tmp <- paste0("./data/race_list/series_",series,"/race_list_",year,".csv")
  race_list <- try(readr::read_csv(tmp,show_col_types = F), silent = T)
  if(class(race_list)[1]=="try-error") {
    stop("Invalid Arguments or No data available for the given series/year")
  }
  races <- race_list$race_id[race_list$race_type_id==1]
  if (length(races)==0) {
    stop("No lap time data found for the specified year")
  }
  # Initialize Loop Variables
  all_races <- list()
  counter <- 1
  no_data <- c()
  l <- 1
  #
  for(r in races) {
    # Get practive lap data from NASCAR website
    url <- paste0("https://cf.nascar.com/cacher/",year,"/",series,"/",r,"/lap-averages.json")
    suppressWarnings({
      raw_practice <- try(jsonlite::read_json(path = url),silent = T)
    })
    if(class(raw_practice)=="try-error") {
      no_data[l] <- r; l <- l+1;
      next;
    }
    practice_type <- unlist(lapply(1:length(raw_practice),function(x) raw_practice[[x]]$Title))
    practice <- data.frame()
    for(i in 1:length(practice_type)) {
      tmp <- raw_practice[[i]]$Items
      suppressWarnings({
        tmp <- data.table::rbindlist(tmp)
      })
      tmp[tmp==999] <- NA
      tmp$PracticeName <- practice_type[i]
      practice <- rbind(practice,tmp)
    }
    practice$race_id <- r

    all_races[[counter]] <- practice
    counter <- counter + 1
  }

  if(length(no_data)>1) {
    message(paste0("No practice lap time data found for races: ", paste0(no_data,collapse=", ")))
  }
  return(all_races)
}
