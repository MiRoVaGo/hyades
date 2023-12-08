library(ggplot2)
library(data.table)
library(doParallel)
library(magrittr, include.only = "%>%")
library(lubridate)

##
GOLDEN_RATIO <- (1 + sqrt(5))/2

N_CORES <- detectCores()

registerDoParallel(cores = N_CORES - 1)

filenames <- list.files("data/raw/", pattern = ".csv", full.names = TRUE)

prec <- foreach(idx = 1:length(filenames), .combine = rbind) %dopar% {
  dummie_name <- filenames[idx]
  dummie_name <- sub(".*//(.+).csv.*", "\\1", dummie_name)
  dummie <- fread(filenames[idx])
  dummie[, record := max(YEAR) - min(YEAR) + 1
  ][, na_yrs := ifelse(sum(is.na(PRCP.VALUE)) >= 1, 1, 0), .(YEAR)]
  dummie <- unique(dummie[, .(YEAR, na_yrs, record)])
  dummie <- unique(dummie[, .(id = dummie_name, na_yrs = sum(na_yrs)/record,
                              start = min(YEAR), end = max(YEAR))])
  return(dummie)
}

p00 <- ggplot(prec, aes(x = na_yrs)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, center = 0.05,
                 fill = "#FFC20A", color = "gray23") +
  #geom_line(aes(y = dbeta(na_yrs, 1.169668, 1.225698)), linewidth = 2) +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = c("0", "20", "40", "60",
                                                         "80", "100")) +
  labs(x = "Percentage of years with missing values [%]",
       y = "Empirical density", title = NULL) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 0.5),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        axis.ticks.length = unit(-0.25, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave(plot = p00, "figures/yrs_with_nas.png", width = 4.5*GOLDEN_RATIO,
       height = 4.5)
