for(year in 2015:2022) {
  for(series in c(1,2,3)) {
    tmp <- nas_pits(series=series,year=year)
    if(length(tmp)==0) {next}
    races <- unlist(lapply(1:length(tmp), function(x) unique(tmp[x]$race_id)))
    base = paste0("./data/pits/series_",series,"/")
    if(!dir.exists(base)) {
      dir.create(base,recursive = TRUE)
    }
    readr::write_csv(x = tmp[r],file = paste0(base,"/",year,".csv"))
  }
}
