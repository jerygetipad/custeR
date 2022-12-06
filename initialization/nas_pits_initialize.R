for(year in 2017:2022) {
  for(series in c(1,2,3)) {
    pit_data <- nas_pits(series=series,year=year)
    if(length(pit_data)==0) {next}
    races <- unlist(lapply(1:length(pit_data), function(x) unique(pit_data[x]$race_id)))
    base = paste0("./data/pits/series_",series,"/")
    if(!dir.exists(base)) {
      dir.create(base,recursive = TRUE)
    }
    readr::write_csv(x = pit_data,file = paste0(base,"/",year,".csv"))
  }
}
