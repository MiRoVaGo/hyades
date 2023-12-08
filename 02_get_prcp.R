library(VFS)
library(doParallel)
library(data.table)
library(magrittr, include.only = "%>%")

N_CORES <- detectCores()

registerDoParallel(cores = N_CORES - 1)

dly_filenames <- list.files(path = "~/ghcnd_all/", pattern = ".dly",
                            full.names = TRUE)

stations <- readRDS("data/stations_meta.rds")
stations <- unique(stations$id)

filenames <- foreach(idx = 1:length(stations), .combine = c) %dopar% {
  dummie <- grep(stations[idx], dly_filenames, value = TRUE)
  return(dummie)
}

foreach(idx = 1:length(filenames)) %dopar% {
  dummie <- read.dly(filenames[idx]) %>% as.data.table()
  dummie_name <- filenames[idx]
  dummie_name <- sub(".*//(.+).dly.*", "\\1", dummie_name)
  prcp_names <- colnames(dummie)
  prcp_names <- c("YEAR", "MONTH", "DAY",
                  grep("PRCP", prcp_names, value = TRUE))
  dummie <- dummie[, ..prcp_names]
  fwrite(dummie, file = paste0("data/raw/", dummie_name,".csv"))
}

# Total of 36495 stations
