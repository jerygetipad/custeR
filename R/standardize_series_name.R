#' Standardize Series Name
#'
#' Inputs series identifier and returns standardized series ID
#'
#' @param series cup, xfinity, trucks, arca, indycar, ...
#' @returns series_id
#'
#' @export
#'
#' @examples
#' standardize_series_name(series="MONSTER cup sEriES")
#'

standardize_series_name <- function(series) {

  series <- series |>
    as.character() |>
    tolower()
  ## A1 Grand Prix
  a1g_val <- c("a1gp", "a1", "a1 grand prix", "a1_grand_prix")

  ## ACT Late Model
  act_val <- c("act", "act late model", "act_late_model")

  ## ASA National Tour
  asa_val <- c("asa", "asa national tour", "asa_national_tour")

  ## Cars Tour
  car_val <- c("cars", "cars tour", "cars_tour")

  ## CART
  crt_val <- c("cart", "championship auto racing teams")

  ## Indy Lights
  inl_val <- c("indy lights","indy_lights","firestone indy nxt series", "indy nxt")

  ## IndyCar
  ind_val <- c("indycar", "indy", "ntt indycar series", "indy racing league", "indycar series")

  ## IROC
  iro_val <- c("iroc", "international race of champions")

  ## NASCAR ARCA
  arc_val <- c("nascar arca", "nascar_arca","arca menards series", "arca")

  ## NASCAR ARCA EAST
  are_val <- c("nascar arca east", "nascar_arca_east", "arca east")

  ## NASCAR ARCA WEST
  arw_val <- c("nascar arca west", "nascar_arca_west", "arca west")

  ## NASCAR CONVERTIBLE
  con_val <- c("nascar convertible","convertible","nascar convertible division", "nascar_convertible")

  ## NASCAR FEDEX CHALLENGE
  fed_val <- c("fedex", "fedex challenge", "nascar_fedex_challenge","nascar fedex challenge")

  ## NASCAR GRAND NATIONAL EAST
  gne_val <- c("nascar_grand_national_east", "nascar gne", "gne", "nascar grand national east")

  ## NASCAR MIDWEST SERIES
  mdw_val <- c("nascar_midwest_series", "nascar midwest series")

  ## NASCAR MODIFIED
  mod_val <- c("nascar_modified", "nascar whelen modified tour", "nascar modified")

  ## NASCAR NORTH TOUR
  nor_val <- c("nascar_north_tour", "nascar north","north", "nascar north tour")

  ## NASCAR NORTHWEST SERIES
  nws_val <- c("nascar_northwest_series", "nascar northwest","nascar northwest series", "northwest")

  ## NASCAR PEAK MEXICO
  mex_val <- c("nascar_peak_mexico", "nascar peak mexico", "mexico", "peak mexico")

  ## NASCAR PINTY'S
  pin_val <- c("nascar_pintys", "nascar pintys", "nascar pinty's", "pintys", "pinty", "pinty's")

  ## NASCAR SOUTHEAST SERIES
  soe_val <- c("nascar_southeast_series", "nascar southeast","nascar southeast series", "southeast")

  ## NASCAR SOUTHWEST SERIES
  sow_val <- c("nascar_southwest_series", "nascar southwest","nascar southwest series", "southwest")

  ## NASCAR CUP
  cup_val <- c(
    "series_1",
    "series 1",
    "1",
    "cup",
    "nascar_cup",
    "nascar cup",
    "strictly stock",
    "strictly stock division",
    "grand national",
    "grand national division",
    "winston",
    "winston cup",
    "winston cup series",
    "nextel",
    "nextel cup",
    "nextel cup series",
    "monster",
    "monster cup",
    "sprint",
    "sprint cup",
    "sprint cup series",
    "monster cup series",
    "monster energy cup",
    "monster energy cup series"
  )
  ## XFINITY
  xfi_val = c(
    "series 2",
    "series_2",
    "2",
    "xfinity series",
    "budweiser",
    "budweiser series",
    "late",
    "late model",
    "late model series",
    "sportsman",
    "sportsman series",
    "budweiser late series",
    "budweiser late model series",
    "budweiser sportsman",
    "budweiser sportsman series",
    "budweiser late sportsman series",
    "budweiser late model sportsman series",
    "busch",
    "busch series",
    "grand national",
    "busch grand national",
    "busch grand national series",
    "buschwhack",
    "buschwhacker",
    "buschwhacker series",
    "nationwide",
    "nationwide series"
  )
  ## TRUCK -- NASCAR TRUCK SERIES
  tru_val <- c(
    "series_3",
    "series 3",
    "3",
    "truck",
    "truck Series",
    "craftsman",
    "craftsman truck",
    "craftsman truck series",
    "supertruck",
    "supertruck series",
    "camping world",
    "camping world truck",
    "camping world truck series",
    "camping world series",
    "camping",
    "camping truck",
    "camping truck series",
    "camping series",
    "gander",
    "gander rv",
    "gander rv truck",
    "gander rv series",
    "gander rv truck series",
    "gander rv and outdoors",
    "gander rv and outdoors series",
    "gander rv and outdoors truck",
    "gander rv and outdoors truck series",
    "gander rv & outdoors",
    "gander rv & outdoors series",
    "gander rv & outdoors truck",
    "gander rv & outdoors truck series",
    "gander outdoors",
    "gander outdoors series",
    "gander outdoors truck",
    "gander outdoors truck series"
  )

  ## NASCAR Whelen Southern Modified Tour
  wsm_val <- c("nascar_whelen_southern_modified", "nascar whelen southern modified")

  ## NATCC
  nat_val <- c("natcc", "north american touring car championship")

   ## Reassign Series ID
  series_id = dplyr::case_when(
    series %in% a1g_val ~ "a1_grand_prix",
    series %in% act_val ~ "act_late_model",
    series %in% asa_val ~ "asa_national_tour",
    series %in% car_val ~ "cars_tour",
    series %in% crt_val ~ "cart",
    series %in% inl_val ~ "indy_lights",
    series %in% ind_val ~ "indycar",
    series %in% iro_val ~ "iroc",
    series %in% arc_val ~ "nascar_arca",
    series %in% are_val ~ "nascar_arca_east",
    series %in% arw_val ~ "nascar_arca_west",
    series %in% xfi_val ~ "nascar_busch",
    series %in% con_val ~ "nascar_convertible",
    series %in% cup_val ~ "nascar_cup",
    series %in% fed_val ~ "nascar_fedex_challenge",
    series %in% gne_val ~ "nascar_grand_national_east",
    series %in% mdw_val ~ "nascar_midwest_series",
    series %in% mod_val ~ "nascar_modified",
    series %in% nor_val ~ "nascar_north",
    series %in% nws_val ~ "nascar_northwest_series",
    series %in% mex_val ~ "nascar_peak_mexico",
    series %in% pin_val ~ "nascar_pintys",
    series %in% soe_val ~ "nascar_southeast_series",
    series %in% sow_val ~ "nascar_southwest_series",
    series %in% tru_val ~ "nascar_trucks",
    series %in% wsm_val ~ "nascar_whelen_southern_modified",
    series %in% nat_val ~ "natcc",
    TRUE ~ "invalid"
  )
  return(series_id)
}



