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
         ][, na_yrs := ifelse(sum(is.na(PRCP.VALUE)) >= 1, 1, 0), .(YEAR, MONTH)]
  dummie <- unique(dummie[, .(YEAR, MONTH, na_yrs, record)])
  dummie <- unique(dummie[, .(id = dummie_name, na_months = sum(na_yrs)),
                          .(YEAR)])
  return(dummie)
}

prec_plot <- prec[na_months > 0]

lab_01 <- round(100*mean(prec_plot$na_months <= 1), 1)
lab_02 <- round(100*mean(prec_plot$na_months <= 2), 1)
lab_03 <- round(100*mean(prec_plot$na_months <= 3), 1)
lab_04 <- round(100*mean(prec_plot$na_months <= 4), 1)
lab_05 <- round(100*mean(prec_plot$na_months <= 5), 1)
lab_06 <- round(100*mean(prec_plot$na_months <= 6), 1)
lab_07 <- round(100*mean(prec_plot$na_months <= 7), 1)
lab_08 <- round(100*mean(prec_plot$na_months <= 8), 1)
lab_09 <- round(100*mean(prec_plot$na_months <= 9), 1)
lab_10 <- round(100*mean(prec_plot$na_months <= 10), 1)
lab_11 <- round(100*mean(prec_plot$na_months <= 11), 1)
lab_12 <- round(100*mean(prec_plot$na_months <= 12), 1)

p00 <- ggplot(prec_plot, aes(x = na_months)) +
  geom_vline(xintercept = seq(1, 12, 1), linetype = "dashed") +
  geom_histogram(aes(y = 100*after_stat(count / sum(count))),
                 binwidth = 1, center = 1,
                 fill = "#FFC20A", color = "black") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 30, 5)) +
  annotate(geom = "text", x = 1.2, y = 32, label = paste0(lab_01, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 2.2, y = 32, label = paste0(lab_02, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 3.2, y = 32, label = paste0(lab_03, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 4.2, y = 32, label = paste0(lab_04, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 5.2, y = 32, label = paste0(lab_05, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 6.2, y = 32, label = paste0(lab_06, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 7.2, y = 32, label = paste0(lab_07, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 8.2, y = 32, label = paste0(lab_08, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 9.2, y = 32, label = paste0(lab_09, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 10.2, y = 32, label = paste0(lab_10, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 11.2, y = 32, label = paste0(lab_11, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 12.2, y = 32, label = paste0(lab_12, "[%]"),
           angle = 90) +
  labs(x = "Number of incomplete months",
       y = "Incomplete years [%]", title = NULL) +
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

ggsave(plot = p00, "figures/months_missing.png", width = 4.5*GOLDEN_RATIO,
       height = 4.5)
