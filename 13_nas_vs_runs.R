library(ggplot2)
library(data.table)
library(doParallel)
library(magrittr, include.only = "%>%")
library(lubridate)

##
GOLDEN_RATIO <- (1 + sqrt(5))/2

nas_year <- readRDS("data/nas_per_year.rds")
run_year <- readRDS("data/runs_per_year.rds")

prec <- merge(nas_year, run_year, by = c("id", "YEAR"))

prec[, counts := .N, .(na_dys, runs)]

prec_plot <- unique(prec[, .(na_dys, runs, counts = as.numeric(counts))]) %>%
setorder(cols = "counts") 

p00 <- ggplot(prec_plot, aes(x = runs, na_dys)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.5, linetype = "dashed",
              colour = "gray23") +
  geom_abline(slope = -1, intercept = 366, linewidth = 0.5, linetype = "dashed",
              colour = "gray23") +
  geom_point(aes(color = log10(counts))) +
  scale_color_viridis_c(option = "H", labels = scales::label_math()) +
  labs(y = "Number of missing values",
       x = "Number of missing value runs",
       color = "Counts",
       title = NULL) +
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

ggsave(plot = p00, "figures/nas_vs_runs.png", width = 4.5*GOLDEN_RATIO,
       height = 4.5)

