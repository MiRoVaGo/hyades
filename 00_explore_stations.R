library(data.table)
library(magrittr, include.only = "%>%")
library(ggplot2)

GOLDEN_RATIO <- (1 + sqrt(5))/2

ghcn <- fread("~/shared/data_downloads/GHCNd/ghcnd-inventory.txt")

setnames(ghcn, c("id", "lat", "lon", "var", "start", "end"))

ghcn <- ghcn[var == "PRCP", ][, record := end - start + 1]

START_YEAR <- min(ghcn$start)
END_YEAR <- max(ghcn$end)

stations <- data.table("year" = seq(START_YEAR,END_YEAR), "count" = 0)

stations[, count := sum((year >= ghcn$start) & (year <= ghcn$end)), year]

p00 <- ggplot(stations) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = year, y = count), linewidth = 1.5, color = "#d95f02") +
  scale_x_continuous(limits = c(1780, 2024), expand = c(0, 0), 
                     breaks = seq(1780, 2040, 30)) +
  scale_y_continuous(limits = c(0, 42270),
                     breaks = seq(0, 40000, 10000)) +
  geom_vline(xintercept = 2001, linetype = "dashed") +
  labs(x = NULL, y = "Number of Active Stations", title = NULL) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length.y = unit(-.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave(plot = p00, "figures/active_stations.png", width = 4.5*GOLDEN_RATIO,
       height = 4.5)

p01 <- ggplot(ghcn, aes(x = record)) +
  geom_histogram(binwidth = 10, center = 5, fill = "#FFC20A", color = "black") +
  scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 50)) +
  scale_y_continuous(limits = c(0, 50000), expand = c(0, 0),
                     breaks = seq(0, 50000, 10000)) +
  labs(x = "Record Length in [years]", y = "Number of Stations", title = NULL) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        legend.position = "none",
        axis.ticks.length = unit(.25, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave(plot = p01, "figures/record_length.png", width = 4.5*GOLDEN_RATIO,
       height = 4.5)
