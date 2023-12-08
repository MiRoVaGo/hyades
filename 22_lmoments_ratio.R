library(ggplot2)
library(data.table)
library(LMoFit)
library(moments)

GOLDEN_RATIO <- (1 + sqrt(5))/2

hyades <- readRDS(file = "data/hyades_prelim.rds")

hyades <- hyades[, .(`Record Lenght` = max(YEAR) - min(YEAR) + 1,
                     Median = median(MAX.PRCP),
                     Mean = mean(MAX.PRCP),
                     SD = sd(MAX.PRCP),
                     Skew = skewness(MAX.PRCP),
                     L1 = get_sample_lmom(MAX.PRCP)$sl1,
                     `L-Scale` = get_sample_lmom(MAX.PRCP)$sl2,
                     `L-Skew` = get_sample_lmom(MAX.PRCP)$st3,
                     `L-Kurtosis` = get_sample_lmom(MAX.PRCP)$st4), ID]

theoretical_gev <- data.table(k = seq(-2.65, 1, 0.01))
theoretical_gev[, `:=`(x = (2*(1 - 3^k)/(1 - 2^k)) - 3,
                       y = (1 - 3*2^(1 + k) + 10*3^k - 5*4^k)/(1 - 2^k))]

p00 <- ggplot(hyades) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray23") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray23") +
  geom_point(aes(x= `L-Skew`, y = `L-Kurtosis`), color = "#E1BE6A") +
  geom_line(data = theoretical_gev, aes(x= x, y = y), linewidth = 1,
            color = "gray23") +
  geom_point(aes(x = mean(`L-Skew`), y = mean(`L-Kurtosis`)),
             color = "#e41a1c", shape = 18, size = 4) +
  theme_bw() +
  labs(x = "L-Skewness") +
  scale_x_continuous(limits = c(-0.75, 1), breaks = seq(-0.6, 0.95, 0.2),
                     labels = c("-0.6", "-0.4", "-0.2", "0.0", "0.2", "0.4",
                                "0.6", "0.8"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.25, 1), breaks = seq(-0.2, 1, 0.2)) +
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

ggsave(plot = p00, filename = "figures/lmoments_ratio.png",
       width = 4.5*GOLDEN_RATIO, height = 4.5)
###

hyades[, `:=`(alpha = fit_gev(sl1 = L1, sl2 = `L-Scale`,
                              st3 = `L-Skew`)$location,
              beta = fit_gev(sl1 = L1, sl2 = `L-Scale`, st3 = `L-Skew`)$scale,
              gamma = fit_gev(sl1 = L1, sl2 = `L-Scale`, st3 = `L-Skew`)$shape,
              se_gamma = fit_gev(sl1 = L1, sl2 = `L-Scale`,
                                 st3 = `L-Skew`)$se_shape),
       ID]

p01 <- ggplot(hyades, aes(x = gamma - 0.1)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(abs(x))),
                 binwidth = 0.02, color = "gray23") +
  geom_density(linewidth = 1) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, limits =c(0, 0.5),
                       na.value = "#B10026", guide = "none") +
  scale_x_continuous(breaks = seq(-2.4, 1, 0.2),
                     labels = round(seq(-2.4, 1, 0.2), 1) + 0.1) +
  labs(x = "GEV shape parameter \U03B3",
       y = "Empirical density", title = NULL) +
  scale_y_continuous(breaks = seq(0, 3.5, 0.5)) +
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

ggsave(plot = p01, filename = "figures/gev_dist.png",
       width = 4.5*GOLDEN_RATIO, height = 4.5)

gev_stats <- hyades[, .(ID, alpha, beta, gamma)]
gev_stats <- melt(gev_stats, id = "ID")
gev_stats <- gev_stats[, .(Min = round(min(value), 3),
                           Q5 = round(quantile(value, 0.05), 3),
                           Q25 = round(quantile(value, 0.25), 3),
                           Q50 = round(quantile(value, 0.5), 3),
                           Q75 = round(quantile(value, 0.75), 3),
                           Q95 = round(quantile(value, 0.95), 3),
                           Max = round(max(value), 3),
                           Mean = round(mean(value), 3),
                           SD = round(sd(value), 3),
                           Skew = round(skewness(value), 3),
                           `L-scale` = round(get_sample_lmom(value)$sl2, 3),
                           `L-skew` = round(get_sample_lmom(value)$st3, 3),
                           `L-kurt` = round(get_sample_lmom(value)$st4, 3)),
                       variable]

gev_stats <- transpose(gev_stats, keep.names = " ")

# 1: variable alpha   beta gamma
# 2:      Min  2.18    0.8 -1.84
# 3:       Q5 22.81   6.86 -0.22
# 4:      Q25 34.09  10.96 -0.03
# 5:      Q50 50.14  15.55  0.07
# 6:      Q75 69.44  22.26  0.17
# 7:      Q95 100.5  38.25  0.31
# 8:      Max 605.2 447.82   0.8
# 9:     Mean 54.48  18.12  0.06
# 10:       SD 26.65  10.62  0.16
# 11:     Skew  1.87   3.76 -0.54
# 12:  L-scale 14.13   5.31  0.09
# 13:   L-skew  0.18   0.27 -0.06
# 14:   L-kurt  0.12   0.18  0.16

