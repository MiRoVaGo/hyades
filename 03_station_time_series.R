library(data.table)
library(magrittr, include.only = "%>%")
library(ggplot2)

GOLDEN_RATIO <- (1 + sqrt(5))/2

ghcn <- fread("~/shared/data_downloads/GHCNd/ghcnd-inventory.txt")
setnames(ghcn, c("id", "lat", "lon", "var", "start", "end"))

ids <- list.files("data/raw/", pattern = ".csv") %>% sub(".csv", "", .)

ghcn <- ghcn[var == "PRCP" & id %in% ids, ][, record := end - start + 1]

START_YEAR <- min(ghcn$start)
END_YEAR <- max(ghcn$end)

stations <- data.table("year" = seq(START_YEAR,END_YEAR), "count" = 0)

stations[, count := sum((year >= ghcn$start) & (year <= ghcn$end)), year]

p00 <- ggplot(stations) +
  geom_line(aes(x = year, y = count), linewidth = 1.5, color = "#d95f02") +
  geom_vline(xintercept = 1970, linetype = "dashed") +
  scale_x_continuous(limits = c(1779, 2025), expand = c(0, 0), 
                     breaks = seq(1780, 2040, 30)) +
  scale_y_continuous(limits = c(0, 25000), 
                     breaks = seq(0, 40000, 5000)) +
  labs(x = NULL, y = "Number of Active Stations", title = NULL) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.text.x = element_text(hjust = 1),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        axis.ticks = element_line(colour = "gray23"),
        axis.ticks.length.y = unit(-.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave(plot = p00, "figures/active_stations_hyades.png",
       width = 4.5*GOLDEN_RATIO, height = 4.5)

p01 <- ggplot(ghcn, aes(x = record)) +
  geom_histogram(binwidth = 10, center = 10, fill = "#FFC20A", color = "gray23") +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(0, 260, 20)) +
  scale_y_continuous(limits = c(0, 7000), expand = c(0, 0),
                     breaks = seq(1000, 11000, 1000)) +
  labs(x = "Record Length in [years]", y = "Number of Stations", title = NULL) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 20),
        axis.ticks = element_line(colour = "gray23"),
        axis.ticks.length = unit(0.25, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave(plot = p01, "figures/record_length_distribution_hyades.png",
       width = 4.5*GOLDEN_RATIO,
       height = 4.5)
