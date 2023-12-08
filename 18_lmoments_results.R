library(data.table)
library(ggplot2)
library(ggpubr)

GOLDEN_RATIO <- (1 + sqrt(5))/2

original_data <- readRDS("data/original_lmoments.rds")

original_data <- original_data[, .(id, YEAR, original = max_prcp)]

altered_data <- readRDS("data/altered_lmoments.rds")

altered_data <- altered_data[, .(id, YEAR, profile, nas_percent, rank_percent,
                                 simulated = max_prcp, total_nas)]

prec_data <- merge(original_data, altered_data, by = c("id", "YEAR"))

rm(altered_data, original_data)

prec_data[, relerr := 100*abs((simulated - original)/original)]

prec_data <- prec_data[complete.cases(prec_data)]

prec_data <- prec_data[, .(mean_diff = mean(relerr)),
                       .(nas_percent, rank_percent)]

p00 <- ggplot(prec_data) +
  geom_raster(aes(x = 100*nas_percent, y = 100*(1 - rank_percent),
                  fill = mean_diff)) +
  scale_x_continuous(breaks = seq(33, 50, 4)) +
  scale_y_continuous(breaks = seq(60, 76, 4)) +
  coord_fixed(ratio = 1) +
  scale_fill_distiller(palette = "Spectral") +
  labs(x = "Missing value percentage [%]", y = "Exceedance probability [%]",
       fill = "Relative\nerror [%]") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23"),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "white", color = "gray23",
                                        linewidth = 1),
        axis.ticks.length = unit(0.25, "cm")) 

ggsave(plot = p00, filename = "figures/summary_thresholds.png",
       width = 4.5*GOLDEN_RATIO, height = 4.5)


