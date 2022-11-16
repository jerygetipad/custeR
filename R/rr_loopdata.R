#' Pull Loop Data from racing-reference.info
#'
#' Pulls Loop Data for the 2022 NASCAR Season
#'
#' @param years A single year 2015-2022.
#' @param series 1=Cup, 2=Xfinity, 3=Trucks.
#' @param races Either in race_id format (5146) or rr_id format ("01"), defaults to "all" races
#' @returns Loop data for the specified races.
#'
#' @examples
#' rr_loopdata(races=5146, year=2022,series=2)
#' rr_loopdata(races="01", year=2022,series=2)
#' rr_loopdata(races="all", year=2022,series=2)

#
rr_loopdata <- function(races="all",year=2022,series=1) {
  # Convert to NASCAR race_id to rr_id
  if(nchar(races[1])==4) {
    xwalk = readr::read_csv("./data/misc/xwalk_race_ids.csv") %>%
      dplyr::filter(race_season == year, series_id == series)
    races = xwalk$rr_id[races %in% xwalk$race_id]
  } else if (races[1]=="all") {
    xwalk = readr::read_csv("./data/misc/xwalk_race_ids.csv") %>%
      dplyr::filter(race_season == year, series_id == series)
    races = xwalk$rr_id
  }
  xpath <- "/html/body/div[1]/table[3]/tbody/tr/td"
  base_url <-
    paste0("https://www.racing-reference.info/loopdata/",year,"-RACE/",switch(series,"W","B","C"))

  loopdata = data.frame()
  for (rc in races) {
    url = stringr::str_replace(base_url, "RACE", rc)
    html <- xml2::read_html(url)
    # Racing Reference Driver Data
    table <- html %>%
      rvest::html_nodes('table')
    table <- table[[4]] %>% rvest::html_nodes('table') %>% rvest::html_table(fill=TRUE)
    table <- table[[1]]
    table <- table[-c(1:2), c(1:19)]
    names(table) <- as.character(table[1, ])
    table <- table[-c(1), ]
    table$rr_id <- rc
    table$race_season <- year
    loopdata <- rbind(loopdata, table)
  }

  loopdata <- loopdata %>% tidyr::drop_na()
  return(loopdata)
}
