nas_images_initialize <- function() {
  url = "https://cf.nascar.com/cacher/drivers.json"
  driver_info <- try(jsonlite::read_json(path = url))[[3]]

  # Initialize Directories
  badge_dir = paste0("./data/images/badge")
  if(!dir.exists(badge_dir)) {
    dir.create(badge_dir,recursive = TRUE)
  }
  firesuit_dir = "./data/images/firesuit_small"
  if(!dir.exists(firesuit_dir)) {
    dir.create(firesuit_dir,recursive = TRUE)
  }
  image_dir = paste0("./data/images/image_small")
  if(!dir.exists(image_dir)) {
    dir.create(image_dir,recursive = TRUE)
  }
  # Populate Directories
  for(d in 1:length(driver_info)) {
    driver_id <- driver_info[[d]]$Nascar_Driver_ID
    badge = driver_info[[d]]$Badge_Image
    image_small = driver_info[[d]]$Image_Small
    firesuit_small = driver_info[[d]]$Firesuit_Image_Small
    if(badge!="") {
      download.file(badge,paste0(badge_dir,"/",driver_id,".png"), mode = 'wb')
    }
    if(image_small!="") {
      download.file(image_small,paste0(image_dir,"/",driver_id,".png"), mode = 'wb')
    }
    if(firesuit_small!="") {
      download.file(firesuit_small,paste0(firesuit_dir,"/",driver_id,".png"), mode = 'wb')
    }
  }
  df_driver_info <-
    data.table::rbindlist(driver_info) |>
    dplyr::select(
      Nascar_Driver_ID,
      Badge_Image,
      Image_Small,
      Firesuit_Image_Small
    )
  readr::write_csv(x=df_driver_info,"./data/misc/nas_driver_images.csv")
}

