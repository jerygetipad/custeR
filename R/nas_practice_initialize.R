for(year in 2020:2022) {
  for(series in c(1,2,3)) {
    tmp <- nas_practice(series=series,year=year)
    if(length(tmp)==0) {next}
    races <- unlist(lapply(1:length(tmp), function(x) unique(tmp[[x]]$race_id)))

    for(r in 1:length(races)) {
      base = paste0("./data/practice/series_",series,"/",year)
      if(!dir.exists(base)) {
        dir.create(base,recursive = TRUE)
      }
      readr::write_csv(x = tmp[[r]],file = paste0(base,"/",races[r],".csv"))
    }
  }
}

