nas_images_update <- function() {
  # Check if an update is needed
  url = "https://cf.nascar.com/cacher/drivers.json"
  new <- try(jsonlite::read_json(path = url))[[3]] |>
    data.table::rbindlist() |>
    dplyr::select(
      Nascar_Driver_ID,
      Badge_Image,
      Image_Small,
      Firesuit_Image_Small
    ) |>
    dplyr::distinct()
  old <- readr::read_csv("./data/misc/nas_driver_images.csv") |>
    dplyr::distinct()
  old[is.na(old)]<-""

  rows_to_update <- lapply(new$Nascar_Driver_ID,function(x) {
    oldrow = unlist(old[old$Nascar_Driver_ID==x,])
    newrow = unlist(new[new$Nascar_Driver_ID==x,])
    if(!identical(oldrow,newrow)) {return(x)}
  }) |>
    unlist()

  if(length(rows_to_update)==0) {
    stop("All images are up to date")
  }
  # Initialize Directory Locations
  badge_dir = paste0("./data/images/badge")
  firesuit_dir = "./data/images/firesuit_small"
  image_dir = paste0("./data/images/image_small")
  #
  for(i in rows_to_update) {
    driver_id <- new$Nascar_Driver_ID[new$Nascar_Driver_ID==i]
    badge = new$Badge_Image[new$Nascar_Driver_ID==i]
    image_small = new$Image_Small[new$Nascar_Driver_ID==i]
    firesuit_small = new$Firesuit_Image_Small[new$Nascar_Driver_ID==i]
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
  readr::write_csv(x=new,"./data/misc/nas_driver_images.csv")
  message("Driver Images Updated")
}
