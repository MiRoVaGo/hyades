library(data.table)
library(magrittr, include.only = "%>%")

ghcn <- fread("~/shared/data_downloads/GHCNd/ghcnd-inventory.txt")

setnames(ghcn, c("id", "lat", "lon", "var", "start", "end"))

ghcn <- ghcn[var == "PRCP", ][, record := end - start + 1]

ghcn <- ghcn[record >= 20 & end <= 2022]

saveRDS(ghcn, file = "data/stations_meta.rds")
