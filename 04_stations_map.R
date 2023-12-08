install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)
library(data.table)
library(magrittr, include.only = "%>%")
library(raster)
library(ggplot2)

GOLDEN_RATIO <- (1 + sqrt(5))/2

stations <- readRDS("data/stations_meta.rds")
filenames <- list.files("data/raw/", pattern = ".csv") %>% sub(".csv", "", .)

stations <- stations[id %in% filenames]

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
to_plot_sf <- stations[, .(lon, lat, record)] %>%
  setorder(cols = "record") %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +no_defs")

### Map
p00 <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray23") +
  geom_sf(aes(fill = record, color = record), size = 0.2) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  labs(x = NULL, y = NULL, fill = "Record Length [years]") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  scale_fill_fermenter(breaks = c(seq(20, 200, 20), 243), palette = "Spectral") +
  scale_color_fermenter(breaks = c(seq(20, 200, 20), 243), palette = "Spectral", guide = "none") +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 4) +
  theme_bw() +
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

### Save
ggsave(plot = p00, "figures/stations.png", width = 4.5*GOLDEN_RATIO,
       height = 4.5)
