library(ggplot2)
library(data.table)
library(doParallel)
library(magrittr, include.only = "%>%")
library(lubridate)

GOLDEN_RATIO <- (1 + sqrt(5))/2

N_CORES <- detectCores()

registerDoParallel(cores = N_CORES - 1)

filenames <- list.files("data/raw/", pattern = ".csv", full.names = TRUE)

prec <- foreach(idx = 1:length(filenames), .combine = rbind) %dopar% {
  dummie_name <- filenames[idx]
  dummie_name <- sub(".*//(.+).csv.*", "\\1", dummie_name)
  dummie <- fread(filenames[idx])
  dummie[!(is.na(PRCP.QFLAG) | PRCP.QFLAG == ""), PRCP.VALUE := NA]
  dummie[, `:=`(doy = .N, na_days = is.na(PRCP.VALUE)), YEAR
         ][, na_yrs := sum(na_days), YEAR]
  missing_years <- unique(dummie[na_yrs == doy, YEAR])
  dummie <- dummie[!(YEAR %in% missing_years) & na_yrs == 0]
  if (nrow(dummie) > 0) {
    consecutive_years <- unique(dummie[, YEAR]) %>% as.data.table() %>%
      setnames("YEAR")
    consecutive_years[, yrs_idx := cumsum(c(1, diff(YEAR) != 1))]
    longest_run <- data.table("lengths" = rle(consecutive_years$yrs_idx)$lengths,
                              "values" = rle(consecutive_years$yrs_idx)$values)
    longest_run <- longest_run[lengths == max(lengths), values]
    if (length(longest_run) > 1) {
      longest_run <- max(longest_run)
    }
    consecutive_years <- consecutive_years[yrs_idx == longest_run, YEAR]
    dummie <- dummie[YEAR %in% consecutive_years]
    record_length <- max(dummie$YEAR) - min(dummie$YEAR) + 1
    if (record_length >= 20) {
      dummie <- dummie[, c(1:7)]
      fwrite(dummie, file = paste0("data/complete_records/", dummie_name,
                                   ".csv"))
      dummie_out <- data.table("id" = dummie_name,
                               "start_complete" = min(dummie$YEAR),
                               "end_complete" = max(dummie$YEAR),
                               "complete_record" = record_length)
      return(dummie_out)
    }
  }
}

saveRDS(prec, "data/complete_records_meta.rds")


## 8968 records with full consecutive years from 20 to 194 years
