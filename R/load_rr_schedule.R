#' Load RR Schedule
#'
#' Loads the Racing Reference Schedule for the following series:
#' a1_grand_prix      | A1 Grand Prix
#' act_late_model     | ACT Late Model
#' asa_national_tour  | ASA National Tour
#' cars_tour          | CARS Tour
#' cart               | CART
#' indy_lights        | Indy NXT
#' indycar            | Indycar
#' iroc               | IROC
#' nascar_arca        | NASCAR ARCA main series
#' nascar_arca_east   | NASCAR ARCA east
#' nascar_arca_west   | NASCAR ARCA west
#' nascar_busch       | NASCAR Xfinity series
#' nascar_convertible | NASCAR Convertible Division
#' nascar_cup         | NASCAR Cup series
#' nascar_fedex_challenge     | NASCAR FedEx Challenge
#' nascar_grand_national_east | NASCAR Grand National East
#' nascar_midwest_series      | NASCAR Midwest Series
#' nascar_modified    | NASCAR Whelen Modified Tour
#' nascar_north_tour  | NASCAR North Tour
#' nascar_northwest_series    | NASCAR Northwest Series
#' nascar_peak_mexico | NASCAR Peak Mexico Series
#' nascar_pintys      | NASCAR Pinty's Series
#' nascar_southeast_series    | NASCAR Southeast Series
#' nascar_southwest_series    | NASCAR Southwest Series
#' nascar_trucks      | NASCAR Truck Series
#' nascar_whelen_southern_modified | NASCAR Whelen Southern Modified Tour
#' natcc              | North American Touring Car Championship
#' @param year numeric year with format YYYY
#' @param series cup, xfinity, trucks, arca, indycar, ...
#' @param time_pause Wait time (in seconds) between page loads
#' @returns data.frame of the racing-reference schedule
#'
#' @export
#' @examples
#' load_rr_schedule(year=2022, series="cup")
#'

load_rr_schedule <- function(year, series, time_pause=1) {
  # Check for Valid Series
  series <- standardize_series_name(series)
  if (!(series %in% c("a1_grand_prix",
                      "act_late_model",
                      "asa_national_tour",
                      "cars_tour",
                      "cart",
                      "indy_lights",
                      "indycar",
                      "iroc",
                      "nascar_arca",
                      "nascar_arca_east",
                      "nascar_arca_west",
                      "nascar_busch",
                      "nascar_convertible",
                      "nascar_cup",
                      "nascar_fedex_challenge",
                      "nascar_grand_national_east",
                      "nascar_midwest_series",
                      "nascar_modified",
                      "nascar_north",
                      "nascar_northwest_series",
                      "nascar_peak_mexico",
                      "nascar_pintys",
                      "nascar_southeast_series",
                      "nascar_southwest_series",
                      "nascar_trucks",
                      "nascar_whelen_southern_modified",
                      "natcc"
  ))) {
    stop("Invalid series name")
  }



  # Check For Valid Time Value
  if(time_pause < 0) {
    stop("Invalid time_pause value")
  }

  # Check For Valid Year
  year_valid <-
    switch(
      series,
      a1_grand_prix = year %in% c(2005:2008),
      act_late_model = year %in% c(2006:2022),
      asa_national_tour = year %in% c(1998:2004),
      cars_tour = year %in% c(1997:2014),
      cart = year %in% c(1979:2007),
      indy_lights = year %in% c(1986:2022),
      indycar = year %in% c(1996:format(Sys.Date(), "%Y")),
      iroc = year %in% c(1979:2007),
      nascar_arca = year %in% c(1979:2023),
      nascar_arca_east = year %in% c(1954:1957,1964:2019),
      nascar_arca_west = year %in% c(1987:2019),
      nascar_busch = year %in% c(1984:format(Sys.Date(), "%Y")),
      nascar_convertible = year %in% c(1956:1959),
      nascar_cup = year %in% c(1949:format(Sys.Date(), "%Y")),
      nascar_fedex_challenge = year %in% c(2017:2022),
      nascar_grand_national_east = year %in% c(1972:1973),
      nascar_midwest_series = year %in% c(1998:2006),
      nascar_modified = year %in% c(1979:format(Sys.Date(), "%Y")),
      nascar_north_tour = year %in% c(1979:1985),
      nascar_northwest_series = year %in% c(1985:2006),
      nascar_peak_mexico = year %in% c(2008:2022),
      nascar_pintys = year %in% c(2007:2022),
      nascar_southeast_series = year %in% c(1991:2006),
      nascar_southwest_series = year %in% c(1986:2006),
      nascar_trucks = year %in% c(1995:format(Sys.Date(), "%Y")),
      nascar_whelen_southern_modified = year %in% c(2005:2016),
      natcc = year %in% c(1996:1997)
    )

  if(!year_valid) {
    stop(glue::glue("Schedule data not found for the {year} {series} series"))
  }

  # Get Race Results for series
  url = glue::glue("https://raw.githubusercontent.com/armstjc/racing-data-repository/main/racing_reference/{series}/schedule/{year}_schedule.csv")
  Sys.sleep(time_pause)

  schedule <- try(readr::read_csv(url, show_col_types = FALSE),silent = T)
  if(inherits(schedule,"try-error")) {
    message(paste0("No data for season: ", year))
    return()
  }

  return(schedule)
}
