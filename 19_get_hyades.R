library(data.table)
library(doParallel)

GOLDEN_RATIO <- (1 + sqrt(5))/2

RANK_THRESHOLD <- 0.40

NAS_THRESHOLD <- 0.33

N_CORES <- detectCores()

registerDoParallel(cores = N_CORES - 1)

filenames <- list.files("data/raw/", pattern = ".csv",
                        full.names = TRUE)

prec <- foreach(idx = 1:length(filenames), .combine = rbind) %dopar% {
  dummie_name <- filenames[idx]
  dummie_name <- sub(".*//(.+).csv.*", "\\1", dummie_name)
  dummie <- fread(filenames[idx])
  dummie <- dummie[YEAR <= 2022]
  dummie[!(is.na(PRCP.QFLAG) | PRCP.QFLAG == ""), PRCP.VALUE := NA]
  dummie[, `:=`(na_yrs = sum(is.na(PRCP.VALUE))/.N,
                prec = max(PRCP.VALUE, na.rm = TRUE)), YEAR]
  dummie[prec == -Inf, prec := NA]
  dummie <- unique(dummie[, .(YEAR, na_yrs, prec)])
  dummie[, rank_prec := rank(prec, na.last = "keep", ties.method = "min")]
  dummie[(rank_prec <= RANK_THRESHOLD*max(rank_prec, na.rm = TRUE)) &
           (na_yrs >= NAS_THRESHOLD), prec := NA]
  dummie <- dummie[, .(ID = dummie_name, YEAR, MAX.PRCP = prec)]
  dummie[MAX.PRCP == 0, MAX.PRCP := NA]
  return(dummie)
}

prec[, `:=`(period = max(YEAR) - min(YEAR) + 1,
            na_yrs = sum(is.na(MAX.PRCP))/.N,
            yrs = (1 - sum(is.na(MAX.PRCP))/.N)*(max(YEAR) - min(YEAR) + 1)),
     ID]

prec_complete <- prec[complete.cases(prec)]

prec_complete[, diff_yrs := shift(YEAR, type = "lead") - YEAR, ID
            ][is.na(diff_yrs), diff_yrs := 0]

station_ids <- unique(prec_complete$ID)

prec_2 <- foreach(idx = 1:length(station_ids), .combine = rbind) %dopar% {
  dummie <- prec_complete[ID == station_ids[idx]]
  dummie[, row_number := .I][, diff_flag := shift(diff_yrs, type = "lead")]
  missing_end <- tail(dummie[diff_yrs >= 3, diff_flag], 1)
  if (length(missing_end) == 0) {missing_end <- 1}
  if (missing_end == 0) {
    while (missing_end == 0) {
      limit_row <- dummie[diff_flag == 0, row_number]
      dummie <- dummie[row_number <= limit_row]
      dummie[diff_flag == 0, diff_yrs := 0
             ][, diff_flag := shift(diff_yrs, type = "lead")]
      missing_end <- tail(dummie[diff_yrs >= 3, diff_flag], 1)
      if (length(missing_end) == 0) {missing_end <- 1}
    }
  }
  dummie[, diff_flag := NULL]
  while (max(dummie$diff_yrs) >= 3) {
    limit_row <- max(dummie[diff_yrs == max(diff_yrs), row_number])
    if (limit_row < nrow(dummie)%/%2) {
      dummie <- dummie[row_number > limit_row][, row_number := .I]
    } else {
      dummie <- dummie[row_number <= limit_row]
      dummie <- dummie[row_number == limit_row, diff_yrs := 0
                       ][, row_number := .I]
    }
  }
  dummie[, row_number := NULL]
  return(dummie)
}

prec_2[, `:=`(start_year = min(YEAR), end_year = max(YEAR)), ID]

prec <- merge(prec_2, prec, all = TRUE,
                     by = c("ID", "YEAR", "MAX.PRCP", "period", "na_yrs",
                            "yrs"))

prec <- split(prec, by = "ID")

hyades <- foreach(idx = 1:length(prec), .combine = rbind) %dopar% {
  dummie <- prec[[idx]]
  MIN_YEAR <- min(dummie$start_year, na.rm = TRUE)
  MAX_YEAR <- max(dummie$end_year, na.rm = TRUE)
  dummie <- dummie[YEAR >= MIN_YEAR & YEAR <= MAX_YEAR,
                   .(ID, YEAR, MAX.PRCP, period, na_yrs, yrs)]
  return(dummie)
}


hyades[, `:=`(period = max(YEAR) - min(YEAR) + 1,
              na_yrs = sum(is.na(MAX.PRCP))/.N,
              yrs = (1 - sum(is.na(MAX.PRCP))/.N)*(max(YEAR) - min(YEAR) + 1)),
       ID]

hyades <- hyades[period >= 16, .(ID, YEAR, MAX.PRCP, NA.PCT = na_yrs)]

saveRDS(hyades, file = "data/hyades_prelim.rds")

#32668 records 16 to 201 years
