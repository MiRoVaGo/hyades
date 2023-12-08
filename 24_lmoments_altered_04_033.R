library(ggplot2)
library(data.table)
library(doParallel)
library(lubridate)
library(Lmoments)
library(dplyr)
library(rlist)

GOLDEN_RATIO <- (1 + sqrt(5))/2

N_CORES <- detectCores()

registerDoParallel(cores = N_CORES - 1)

filenames <- list.files("data/complete_records/", pattern = ".csv",
                        full.names = TRUE)
profiles <- list.files("data/missing_profiles/", pattern = ".csv",
                       full.names = TRUE)
profiles <- data.table("file" = profiles,
                       "record" = sub(".*_(.+).csv.*", "\\1", profiles))

prec_MC <- foreach(idx = 1:length(filenames), .combine = rbind) %dopar% {
  dummie_name <- filenames[idx]
  dummie_name <- sub(".*//(.+).csv.*", "\\1", dummie_name)
  dummie <- fread(filenames[idx])
  record_length <- max(dummie$YEAR) - min(dummie$YEAR) + 1
  if(record_length <= 139) {
    dummie_MC <- sample(profiles[record >= record_length, file], 15) %>%
      lapply(., fread) %>% lapply(., head, n = nrow(dummie)) %>%
      list.cbind() %>% setnames(as.character(1:15))
    dummie_MC$idx <- seq.int(nrow(dummie_MC))
    dummie_MC <- melt(dummie_MC, id.vars = "idx") %>%
      setnames(c("idx", "profile", "flag"))
    dummie$idx <- seq.int(nrow(dummie))
    dummie_MC <- merge(dummie[, .(idx, YEAR, PRCP.VALUE)], dummie_MC,
                       by = "idx")
    dummie_MC[, prec := PRCP.VALUE*flag]
    dummie_MC <- dummie_MC[, .(idx, YEAR, profile, prec)]
    dummie_MC[, na_yrs := sum(is.na(prec))/.N, .(YEAR, profile)
              ][, max_prcp := max(prec, na.rm = TRUE), .(YEAR, profile)]
    dummie_MC[max_prcp == -Inf, max_prcp := NA]
    dummie_MC <- unique(dummie_MC[, .(YEAR, na_yrs, max_prcp, profile)])
    dummie_MC[, rank_prcp := rank(max_prcp, na.last = "keep",
                                  ties.method = "min"), profile]
    dummie_MC[, exceed_p := 1 - (rank_prcp/max(rank_prcp, na.rm = TRUE))]
    dummie_MC[(rank_prcp <= 0.4*max(rank_prcp, na.rm = TRUE)) &
                (na_yrs >= 0.33), max_prcp := NA, profile]
    dummie_MC <- dummie_MC[complete.cases(dummie_MC)]
    dummie_MC <- split(dummie_MC, by = "profile", drop = TRUE)
    dummie_MC <- lapply(dummie_MC, function(x) {
      dummie_trend <- lm(max_prcp ~ YEAR, x)$coefficients[[2]]
      dummie_moments <- Lmoments(x$max_prcp)
      dummie_record <- max(x$YEAR) - min(x$YEAR) + 1
      dummie_nas_percent <- 1 - length(unique(x$YEAR))/record_length
      x[, `:=`(id = dummie_name,
               trend = dummie_trend,
               L1 = dummie_moments[1],
               L2 = dummie_moments[2],
               L3 = dummie_moments[3],
               L4 = dummie_moments[4],
               total_nas = dummie_nas_percent)]
      return(x)
    })
    dummie_MC <-  rbindlist(dummie_MC, fill = TRUE)
    dummie_MC <- dummie_MC[complete.cases(dummie_MC)]
    return(dummie_MC)
  }
}

#prec_MC <- prec_MC[na_yrs <= 0.4]

saveRDS(prec_MC, file = "data/altered_lmoments_04_037.rds")
