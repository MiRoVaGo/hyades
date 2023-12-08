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
         ][, na_dys := sum(is.na(PRCP.VALUE)), .(YEAR, MONTH)]
  dummie <- unique(dummie[, .(id = dummie_name, YEAR, MONTH, na_dys)])
  return(dummie)
}

prec_plot <- prec[na_dys > 0]

lab_01 <- round(100*mean(prec_plot$na_dys <= 1), 1)
lab_04 <- round(100*mean(prec_plot$na_dys <= 4), 1)
lab_07 <- round(100*mean(prec_plot$na_dys <= 7), 1)
lab_10 <- round(100*mean(prec_plot$na_dys <= 10), 1)
lab_13 <- round(100*mean(prec_plot$na_dys <= 13), 1)
lab_16 <- round(100*mean(prec_plot$na_dys <= 16), 1)
lab_19 <- round(100*mean(prec_plot$na_dys <= 19), 1)
lab_22 <- round(100*mean(prec_plot$na_dys <= 22), 1)
lab_25 <- round(100*mean(prec_plot$na_dys <= 25), 1)
lab_28 <- round(100*mean(prec_plot$na_dys <= 28), 1)
lab_31 <- round(100*mean(prec_plot$na_dys <= 31), 1)

p00 <- ggplot(prec_plot, aes(x = na_dys)) +
  geom_vline(xintercept = seq(1, 31, 3), linetype = "dashed") +
  geom_histogram(aes(y = 100*after_stat(count / sum(count))),
                 binwidth = 1, center = 1,
                 fill = "#FFC20A", color = "gray23") +
  scale_y_continuous(limits = c(0,45), breaks = seq(0, 45, 5)) +
  scale_x_continuous(breaks = seq(1, 31, 3)) +
  annotate(geom = "text", x = 1.5, y = 40, label = paste0(lab_01, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 4.5, y = 40, label = paste0(lab_04, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 7.5, y = 40, label = paste0(lab_07, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 10.5, y = 40, label = paste0(lab_10, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 13.5, y = 40, label = paste0(lab_13, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 16.5, y = 40, label = paste0(lab_16, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 19.5, y = 40, label = paste0(lab_19, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 22.5, y = 40, label = paste0(lab_22, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 25.5, y = 40, label = paste0(lab_25, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 28.5, y = 40, label = paste0(lab_28, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 32, y = 40, label = paste0(lab_31, "[%]"),
           angle = 90) +
  labs(x = "Number of missing days",
       y = "Incomplete months [%]", title = NULL) +
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

ggsave(plot = p00, "figures/days_per_month_missing.png", width = 4.5*GOLDEN_RATIO,
       height = 4.5)
