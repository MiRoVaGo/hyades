library(ggplot2)
library(data.table)
library(doParallel)
library(magrittr, include.only = "%>%")
library(lubridate)
library(dplyr)

##
GOLDEN_RATIO <- (1 + sqrt(5))/2

N_CORES <- detectCores()

registerDoParallel(cores = N_CORES - 1)

filenames <- list.files("data/raw/", pattern = ".csv", full.names = TRUE)

prec <- foreach(idx = 1:length(filenames), .combine = rbind) %dopar% {
  dummie_name <- filenames[idx]
  dummie_name <- sub(".*//(.+).csv.*", "\\1", dummie_name)
  dummie <- fread(filenames[idx])
  dummie[, na_flag := ifelse(is.na(PRCP.VALUE), 1, NA)
         ][is.na(PRCP.VALUE), run := cumsum(na_flag), by = YEAR]
  dummie <- dummie[!is.na(run) & is.na(lead(run)),
                   .(id = dummie_name, YEAR, run)]
  return(dummie)
}

prec_plot <- prec[complete.cases(prec)]
prec_plot[run == 1, runs := "1"
          ][run == 2, runs := "2"
            ][run >= 3 & run <= 4, runs := "3-4"
              ][run >= 5 & run <= 7, runs := "5-7"
                ][run >= 8 & run <= 13, runs := "8-13"
                  ][run >= 14 & run <= 31, runs := "14-31"
                    ][run >= 32 & run <= 61, runs := "32-61"
                      ][run >= 62 & run <= 181, runs := "62-181"
                        ][run >= 182 & run <= 366, runs := "182-366"]
prec_plot$runs <- factor(prec_plot$runs, levels = c("1", "2", "3-4", "5-7",
                                                       "8-13", "14-31", "32-61",
                                                       "62-181", "182-366"))

p00 <- ggplot(prec_plot, aes(x = runs)) +
  geom_bar(aes(y = 100*after_stat(count / sum(count))),
                 fill = "#FFC20A", color = "gray23", width = 1) +
  scale_y_continuous(limits = c(0,25), breaks = seq(0, 35, 5)) +
  labs(x = "Length of missing value runs [days]",
       y = "Missing value runs [%]", title = NULL) +
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

ggsave(plot = p00, "figures/na_run_lengths.png", width = 4.5*GOLDEN_RATIO,
       height = 4.5)
