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
         ][, na_dys := sum(is.na(PRCP.VALUE)), .(YEAR)]
  dummie <- unique(dummie[, .(id = dummie_name, YEAR, na_dys)])
  return(dummie)
}

prec_plot <- prec[na_dys > 0]

saveRDS(prec_plot, file = "data/nas_per_year.rds")

lab_30 <- round(100*mean(prec_plot$na_dys <= 30), 1)
lab_60 <- round(100*mean(prec_plot$na_dys <= 60), 1)
lab_90 <- round(100*mean(prec_plot$na_dys <= 90), 1)
lab_120 <- round(100*mean(prec_plot$na_dys <= 120), 1)
lab_150 <- round(100*mean(prec_plot$na_dys <= 150), 1)
lab_180 <- round(100*mean(prec_plot$na_dys <= 180), 1)
lab_210 <- round(100*mean(prec_plot$na_dys <= 210), 1)
lab_240 <- round(100*mean(prec_plot$na_dys <= 240), 1)
lab_270 <- round(100*mean(prec_plot$na_dys <= 270), 1)
lab_300 <- round(100*mean(prec_plot$na_dys <= 300), 1)
lab_330 <- round(100*mean(prec_plot$na_dys <= 330), 1)
lab_360 <- round(100*mean(prec_plot$na_dys <= 360), 1)

p00 <- ggplot(prec_plot, aes(x = na_dys)) +
  geom_histogram(aes(y = 100*after_stat(count / sum(count))),
                 binwidth = 30, center = 15,
                 fill = "#FFC20A", color = "gray23") +
  scale_y_continuous(limits = c(0,45), breaks = seq(0, 60, 10)) +
  scale_x_continuous(limits = c(0, 390), breaks = seq(0, 360, 30)) +
  geom_vline(xintercept = seq(30, 360, 30), linetype = "dashed") +
  annotate(geom = "text", x = 36, y = 42, label = paste0(lab_30, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 66, y = 42, label = paste0(lab_60, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 96, y = 42, label = paste0(lab_90, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 126, y = 42, label = paste0(lab_120, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 156, y = 42, label = paste0(lab_150, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 186, y = 42, label = paste0(lab_180, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 216, y = 42, label = paste0(lab_210, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 246, y = 42, label = paste0(lab_240, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 276, y = 42, label = paste0(lab_270, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 306, y = 42, label = paste0(lab_300, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 336, y = 42, label = paste0(lab_330, "[%]"),
           angle = 90) +
  annotate(geom = "text", x = 366, y = 42, label = paste0(lab_360, "[%]"),
           angle = 90) +
  labs(x = "Number days with missing values",
       y = "Incomplete years [%]", title = NULL) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 0.5),
        plot.title = element_text(size = 20), 
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        axis.ticks.length = unit(-0.25, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave(plot = p00, "figures/days_missing.png", width = 4.5*GOLDEN_RATIO,
       height = 4.5)
