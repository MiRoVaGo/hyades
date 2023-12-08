install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)
library(data.table)
library(raster)
library(ggplot2)
library(ggpubr)

GOLDEN_RATIO <- (1 + sqrt(5))/2

stations <- fread("data/map_inventory_grid.csv")

## Figure
### Borders and labels
earth_box <- readRDS("spatial/earth_box.rds") %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

labs_y <- data.frame(lon = -165, lat = c(55, 25, -5, -35, -65))
labs_y_labels <- seq(60, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°",
                       ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -80)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")
### Map data
to_plot_sf_avg <- stations[, .(lon, lat, avg_record)] %>%
  rasterFromXYZ(res = c(2.5, 2.5),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

### Map
p00 <- ggplot(to_plot_sf_avg) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray23") +
  geom_sf(aes(fill = avg_record, color = avg_record), size = 0.2) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  labs(x = NULL, y = NULL, fill = "Average Record Length [years]") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  scale_fill_fermenter(breaks = c(20, 30, 37, 41, 46, 50, 54, 59, 63, 71, 176),
                       palette = "Spectral") +
  scale_color_fermenter(breaks = c(20, 30, 37, 41, 46, 50, 54, 59, 63, 71, 176),
                        palette = "Spectral", guide = "none") +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 4) +
  theme_bw() +
  guides(fill = guide_colorsteps(title.position = "top",
                                 title.hjust = 0.5,
                                 label.position = "bottom")) +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        axis.text = element_blank(),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        legend.key.width = unit(dev.size()[1]/5, "inches"))

to_plot_sf_range <- stations[, .(lon, lat, range_record)] %>%
  rasterFromXYZ(res = c(2.5, 2.5),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p01 <- ggplot(to_plot_sf_range) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray23") +
  geom_sf(aes(fill = range_record, color = range_record), size = 0.2) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  labs(x = NULL, y = NULL, fill = "Record Length Range [years]") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  scale_fill_fermenter(breaks = c(5, 15, 25, 40, 50, 60, 70, 80, 90, 100), palette = "Spectral") +
  scale_color_fermenter(breaks = c(5, 15, 25, 40, 50, 60, 70, 80, 90, 100), palette = "Spectral", guide = "none") +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 4) +
  theme_bw() +
  guides(fill = guide_colorsteps(title.position = "top",
                                     title.hjust = 0.5,
                                     label.position = "bottom")) +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        axis.text = element_blank(),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.box = "horizontal",
        legend.key.width = unit(dev.size()[1]/5, "inches"))

to_plot_sf_count <- stations[, .(lon, lat, count)] %>%
  rasterFromXYZ(res = c(2.5, 2.5),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p02 <- ggplot(to_plot_sf_count) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray23") +
  geom_sf(aes(fill = count, color = count), size = 0.2) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  labs(x = NULL, y = NULL, fill = "Number of Stations") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  scale_fill_fermenter(breaks = c(1, 2, 3, 6, 10, 14, 22, 34, 46, 64, 95, 527),
                       palette = "Spectral") +
  scale_color_fermenter(breaks = c(1, 2, 3, 6, 10, 14, 22, 34, 46, 64, 95, 527),
                        palette = "Spectral", guide = "none") +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 4) +
  theme_bw() +
  guides(fill = guide_colorsteps(title.position = "top",
                                 title.hjust = 0.5,
                                 label.position = "bottom")) +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        axis.text = element_blank(),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 16),
        legend.position = "bottom",
        legend.key.width = unit(dev.size()[1]/5, "inches"))

p03 <- ggarrange(p00, p02, ncol = 1, labels = c("(a)", "(b)"))

### Save
ggsave(plot = p03, "figures/stations_new.png", width = 4.5*GOLDEN_RATIO,
       height = 4.5*2)
