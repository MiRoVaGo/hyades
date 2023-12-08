library(data.table)
library(ggplot2)
library(moments)
library(LMoFit)

prec_miss <- readRDS(file = "data/hyades_prelim.rds")

prec_miss <- prec_miss[, .(`Record Length` = max(YEAR) - min(YEAR) + 1,
                           Trend = lm(MAX.PRCP ~ YEAR)$coefficients[[2]],
                           Median = median(MAX.PRCP, na.rm = TRUE),
                           Mean = mean(MAX.PRCP, na.rm = TRUE),
                           SD = sd(MAX.PRCP, na.rm = TRUE),
                           Skew = skewness(MAX.PRCP, na.rm = TRUE),
                           `L-Scale` = get_sample_lmom(MAX.PRCP)$sl2,
                           `L-Skew` = get_sample_lmom(MAX.PRCP)$st3,
                           `L-Kurtosis` = get_sample_lmom(MAX.PRCP)$st4),
                       by = ID]

prec_miss <- melt(prec_miss, id = "ID")
prec_miss <- prec_miss[, .(Min = round(min(value), 2),
                           Q5 = round(quantile(value, 0.05), 2),
                           Q25 = round(quantile(value, 0.25), 2),
                           Q50 = round(quantile(value, 0.5), 2),
                           Q75 = round(quantile(value, 0.75), 2),
                           Q95 = round(quantile(value, 0.95), 2),
                           Max = round(max(value), 2),
                           Mean = round(mean(value), 2),
                           SD = round(sd(value), 2),
                           Skew = round(skewness(value), 2),
                           `L-Scale` = round(get_sample_lmom(value)$sl2, 2),
                           `L-Skew` = round(get_sample_lmom(value)$st3, 2)),
                       variable]

prec_miss <- transpose(prec_miss, keep.names = " ")

# 1: variable Record Length Missing Years Median   Mean     SD  Skew L-Scale L-Skew L-Kurtosis
# 2:      Min            16             0      3   4.66    2.9 -1.84    1.47  -0.59       -0.2
# 3:       Q5            21             0     26  28.78   9.85  0.29    5.35   0.06       0.06
# 4:      Q25            34             0   38.4  42.05  15.87  0.81    8.49   0.16       0.13
# 5:      Q50            53             0   55.6   60.2  22.02  1.19   11.68   0.22       0.17
# 6:      Q75            70             2   77.1  82.91  32.26  1.66   16.93   0.28       0.22
# 7:      Q95           110          5.48  114.6  125.4  56.44  2.57   29.58   0.37        0.3
# 8:      Max           201         36.84    733 791.26 468.46  6.31  267.61    0.8        0.8
# 9:     Mean          56.1          1.33  61.11  66.39  26.18  1.28   13.83   0.22       0.18
# 10:       SD          26.9          2.36  30.17   32.5  15.18  0.71    7.96   0.09       0.07
# 11:     Skew          0.82          3.76   2.09   2.02   2.67   0.9    3.07  -0.18       0.43
# 12:  L-Scale         14.91          0.99  15.85  17.13   7.72  0.39    4.02   0.05       0.04
# 13:   L-Skew          0.15          0.54    0.2   0.21   0.28  0.13    0.28  -0.02       0.05

##
prec_full <- readRDS(file = "data/hyades_complete.rds")

prec_full <- prec_full[, .(`Record Length` = max(YEAR) - min(YEAR) + 1,
                           Median = median(MAX.PRCP),
                           Mean = mean(MAX.PRCP),
                           SD = sd(MAX.PRCP),
                           Skew = skewness(MAX.PRCP),
                           `L-Scale` = get_sample_lmom(MAX.PRCP)$sl2,
                           `L-Skew` = get_sample_lmom(MAX.PRCP)$st3,
                           `L-Kurtosis` = get_sample_lmom(MAX.PRCP)$st4), ID]

prec_full <- melt(prec_full, id = "ID")
prec_full <- prec_full[, .(Min = round(min(value), 2),
                           Q5 = round(quantile(value, 0.05), 2),
                           Q25 = round(quantile(value, 0.25), 2),
                           Q50 = round(quantile(value, 0.5), 2),
                           Q75 = round(quantile(value, 0.75), 2),
                           Q95 = round(quantile(value, 0.95), 2),
                           Max = round(max(value), 2),
                           Mean = round(mean(value), 2),
                           SD = round(sd(value), 2),
                           Skew = round(skewness(value), 2),
                           `L-scale` = round(get_sample_lmom(value)$sl2, 2),
                           `L-skew` = round(get_sample_lmom(value)$st3, 2)),
                       variable]

prec_full <- transpose(prec_full, keep.names = " ")

