library(ggplot2)
library(data.table)
library(doParallel)
library(magrittr, include.only = "%>%")
library(lubridate)

GOLDEN_RATIO <- (1 + sqrt(5))/2

N_CORES <- detectCores()

registerDoParallel(cores = N_CORES - 1)

stations <- readRDS("data/stations_meta.rds")

all_filenames <- list.files("data/raw/", pattern = ".csv", full.names = TRUE)

complete_record <- readRDS("data/complete_records_meta.rds")

stations <- stations[!(id %in% complete_record$id)]

missing_profiles <- stations$id

filenames <- foreach(idx = 1:length(missing_profiles), .combine = c) %dopar% {
  dummie <- grep(missing_profiles[idx], all_filenames, value = TRUE)
  return(dummie)
}

foreach(idx = 1:length(filenames)) %dopar% {
  dummie_name <- filenames[idx]
  dummie_name <- sub(".*//(.+).csv.*", "\\1", dummie_name)
  dummie <- fread(filenames[idx])
  dummie[!(is.na(PRCP.QFLAG) | PRCP.QFLAG == ""), PRCP.VALUE := NA]
  dummie_record <- max(dummie$YEAR) -  min(dummie$YEAR) + 1
  dummie[!is.na(PRCP.VALUE), profile := 1]
  dummie_out <- dummie$profile %>% data.table()
  fwrite(dummie_out, paste0("data/missing_profiles/", dummie_name, "_",
                            dummie_record, ".csv"))
}

## 26912 missing value profiles from 20 to 155 years
