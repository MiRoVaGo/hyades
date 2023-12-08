library(data.table)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(scattermore)

GOLDEN_RATIO <- (1 + sqrt(5))/2

original_data <- readRDS("data/original_lmoments.rds")

original_data[, `:=`(T2 = L2/L1, T3 = L3/L2, T4 = L4/L2)]

original_data <- melt(original_data, id = c("id", "record", "YEAR")) %>%
  setnames("value", "original")

altered_data <- readRDS("data/altered_lmoments_04_033.rds")

to_remove <- altered_data[total_nas > 0.4, .(id, profile)]

altered_data <- altered_data[!to_remove, on = .(id, profile)]

unique_ids <- unique(altered_data$id)
original_data <- original_data[id %in% unique_ids]

altered_data <- altered_data[, .(id, profile, YEAR, max_prcp, L1, L2, L3, L4,
                                 T2 = L2/L1, T3 = L3/L2, T4 = L4/L2, trend)]

altered_data <- melt(altered_data, id = c("id", "profile", "YEAR")) %>%
  setnames("value", "simulated")

prec_data <- merge(original_data, altered_data, by = c("id", "YEAR","variable"),
                   all.x = TRUE)

rm(altered_data, original_data, to_remove, unique_ids)

prec_max <- prec_data[variable == "max_prcp", .(original = original,
                                                simulated = simulated)]
#
p00 <- ggplot(prec_max, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)),
                  alpha = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = "Original Annual Maxima", y = "Artificial Annual Maxima") +
  coord_fixed(ratio = 1) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"))

p00_zoom <- ggplot(prec_max, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)),
                  alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 1, xlim = c(0, 150), ylim = c(0, 150)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"),
        plot.background = element_rect(colour = "gray23", fill = NA,
                                       linewidth = 1, linetype = "dashed"))

p00_merged <- ggarrange(p00, p00_zoom, nrow = 1, widths = c(GOLDEN_RATIO, 1),
                        labels = c("", "ZOOM IN"))

###
prec_L1 <- unique(prec_data[variable == "L1",
                            .(id, profile, original, simulated)])

p01 <- ggplot(prec_L1, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)), alpha = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = "Original Mean", y = "Artificial Mean") +
  theme_bw() +
  coord_fixed(ratio = 1) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"))

p01_zoom <- ggplot(prec_L1, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)),
                  alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  coord_fixed(ratio = 1, xlim = c(0, 150), ylim = c(0, 150)) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"),
        plot.background = element_rect(colour = "gray23", fill = NA,
                                       linewidth = 1, linetype = "dashed"))

p01_merged <- ggarrange(p01, p01_zoom, nrow = 1, widths = c(GOLDEN_RATIO, 1),
                        labels = c("", "ZOOM IN"))

###
prec_L2 <- unique(prec_data[variable == "T2",
                            .(id, profile, original, simulated)])

p02 <- ggplot(prec_L2, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)), alpha = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = "Original L-Variation", y = "Artificial L-Variation") +
  coord_fixed(ratio = 1, xlim = c(0, 0.6), ylim = c(0, 0.6)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"))

p02_zoom <- ggplot(prec_L2, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)),
                  alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 1, xlim = c(0.1, 0.3), ylim = c(0.1, 0.3)) +
  scale_x_continuous(breaks = c(0.1, 0.2, 0.3)) +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"),
        plot.background = element_rect(colour = "gray23", fill = NA,
                                       linewidth = 1, linetype = "dashed"))

p02_merged <- ggarrange(p02, p02_zoom, nrow = 1, widths = c(GOLDEN_RATIO, 1),
                        labels = c("", "ZOOM IN"))

###
prec_T3 <- unique(prec_data[variable == "T3",
                            .(id, profile, original, simulated)])

p03 <- ggplot(prec_T3, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)),
                  alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = "Original L-Skewness", y = "Artificial L-Skewness") +
  coord_fixed(ratio = 1, xlim = c(-.45, .75), ylim = c(-.45, .75)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"))

p03_zoom <- ggplot(prec_T3, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)),
                  alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 1, xlim = c(-.1, .5), ylim = c(-.1, .5)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"),
        plot.background = element_rect(colour = "gray23", fill = NA,
                                       linewidth = 1, linetype = "dashed"))

p03_merged <- ggarrange(p03, p03_zoom, nrow = 1, widths = c(GOLDEN_RATIO, 1),
                        labels = c("", "ZOOM IN"))

###
prec_T4 <- unique(prec_data[variable == "T4",
                            .(id, profile, original, simulated)])

p04 <- ggplot(prec_T4, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)),
                  alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = "Original L-Kurtosis", y = "Artificial L-Kurtosis") +
  coord_fixed(ratio = 1, xlim = c(-.25, .75), ylim = c(-.25, .75)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"))

p04_zoom <- ggplot(prec_T4, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)),
                  alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 1, xlim = c(-.05, .4), ylim = c(-.05, .4)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"),
        plot.background = element_rect(colour = "gray23", fill = NA,
                                       linewidth = 1, linetype = "dashed"))

p04_merged <- ggarrange(p04, p04_zoom, nrow = 1, widths = c(GOLDEN_RATIO, 1),
                        labels = c("", "ZOOM IN"))

###
prec_trend <- unique(prec_data[variable == "trend",
                               .(id, profile, original, simulated)])

p05 <- ggplot(prec_trend, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)),
                  alpha = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = "Original Trend", y = "Artificial Trend") +
  coord_fixed(ratio = 1, ylim = c(-10, 10), xlim = c(-10, 10)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"))

p05_zoom <- ggplot(prec_trend, aes(x = original, y = simulated)) +
  geom_scattermore(color = "gray69", alpha = 0.2, pointsize = 5.2) +
  stat_density_2d(geom = "polygon", aes(fill = after_stat(level)),
                  alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey23",
              linewidth = 0.5) +
  scale_fill_distiller(palette = "RdGy") +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 1, ylim = c(-.75, .75), xlim = c(-.75, .75)) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(0.25, "cm"),
        plot.background = element_rect(colour = "gray23", fill = NA,
                                       linewidth = 1, linetype = "dashed"))

p05_merged <- ggarrange(p05, p05_zoom, nrow = 1, widths = c(GOLDEN_RATIO, 1),
                        labels = c("", "ZOOM IN"))
###

p06 <- ggarrange(p00_merged, p01_merged, p02_merged, p03_merged, p04_merged,
                 p05_merged, nrow = 3, ncol = 2,
                 labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"))

ggsave(plot = p06, filename = "figures/summary_mc_results.png",
       width = 4.5*GOLDEN_RATIO*2, height = 4.5*3)
