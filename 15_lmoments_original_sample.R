library(ggplot2)
library(data.table)
library(doParallel)
library(magrittr, include.only = "%>%")
library(lubridate)
library(Lmoments)

GOLDEN_RATIO <- (1 + sqrt(5))/2

N_CORES <- detectCores()

registerDoParallel(cores = N_CORES - 1)

filenames <- list.files("data/complete_records/", pattern = ".csv", full.names = TRUE)

prec <- foreach(idx = 1:length(filenames), .combine = rbind) %dopar% {
  dummie <- fread(filenames[idx])
  dummie_name <- filenames[idx]
  dummie_name <- sub(".*//(.+).csv.*", "\\1", dummie_name)
  dummie <- dummie[, .(max_prcp = max(PRCP.VALUE)), YEAR]
  dummie_moments <- Lmoments(dummie$max_prcp)
  dummie_record <- max(dummie$YEAR) - min(dummie$YEAR) + 1
  dummie_trend <- lm(max_prcp ~ YEAR, dummie)$coefficients[[2]]
  dummie[, `:=`(id = dummie_name,
                trend = dummie_trend,
                L1 = dummie_moments[1],
                L2 = dummie_moments[2],
                L3 = dummie_moments[3],
                L4 = dummie_moments[4],
                record = dummie_record)]
  return(dummie)
}

saveRDS(prec, file = "data/original_lmoments.rds")
