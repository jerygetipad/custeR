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
  # Get Race Id's for given year + series
  url <- paste0("./data/race_list/series_",series,"/race_list_",year,".csv")
  race_list <- try(readr::read_csv(url,show_col_types = FALSE),silent = TRUE)
  if(class(race_list)[1]=="try-error") {
    stop("Invalid Arguments or No data available for the given series/year")
  }
  races <- race_list$race_id[race_list$race_type_id==1]
  if (length(races)==0) {
    stop("No lap time data found for the specified year")
  }
  # Initialize Loop Variables
  masterpits <- data.frame()
  counter <- 1
  no_data <- c()
  l <- 1
  #
  for(r in races) {
    url = paste0("https://cf.nascar.com/cacher/live/series_",series,"/",r,"/live-pit-data.json")
    suppressWarnings({
      pits <- try(jsonlite::read_json(path = url),silent=TRUE)
    })
    if(class(pits)=="try-error") {
      no_data[l] <- r; l = l+1
      next
    }
    pits <- do.call(rbind, lapply(pits, data.frame))

    pits$id <- NULL
    pits$race_id <- r

    masterpits <- rbind(masterpits,pits)
    #write_csv(x = pits,file = paste0("./files/pits/series_",series,"/",year,"/",r,".csv"))
  }
  if(length(no_data)>1) {
    message(paste0("No pit stop data found for races: ", paste0(no_data,collapse=", ")))
  }
  return(masterpits)
}

