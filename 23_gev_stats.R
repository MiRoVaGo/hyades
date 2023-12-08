library(ggplot2)
library(data.table)
library(LMoFit)
library(moments)

GOLDEN_RATIO <- (1 + sqrt(5))/2

hyades <- readRDS(file = "data/hyades_prelim.rds")

hyades_top <- copy(hyades)
hyades_top[, record := max(YEAR) - min(YEAR) + 1, ID]
hyades_top[record <= 40, `Record Lenght` := "16-40"
           ][record > 40 & record <= 60, `Record Lenght` := "41-60"
             ][record > 60 & record <= 80, `Record Lenght` := "61-80"
               ][record > 80 & record <= 100, `Record Lenght` := "81-100"
                 ][record > 100, `Record Lenght` := ">101"]

hyades_top <- unique(hyades_top[, .(ID, `Record Lenght`)])
hyades_top[, `Records Number` := .N, `Record Lenght`]

hyades <- hyades[, .(L1 = get_sample_lmom(MAX.PRCP)$sl1,
                     `L-Scale` = get_sample_lmom(MAX.PRCP)$sl2,
                     `L-Skew` = get_sample_lmom(MAX.PRCP)$st3), ID
                 ][, `:=`(alpha = fit_gev(sl1 = L1, sl2 = `L-Scale`,
                                          st3 = `L-Skew`)$location,
                          beta = fit_gev(sl1 = L1, sl2 = `L-Scale`,
                                         st3 = `L-Skew`)$scale,
                          gamma = fit_gev(sl1 = L1, sl2 = `L-Scale`,
                                          st3 = `L-Skew`)$shape,
                          se_gamma = fit_gev(sl1 = L1, sl2 = `L-Scale`,
                                             st3 = `L-Skew`)$se_shape),
                   ID]

gev_stats <- hyades[, .(ID, gamma)]
gev_stats <- merge(gev_stats, hyades_top[, .(ID, `Record Lenght`)], by = "ID")
gev_stats <- gev_stats[, .(Min = round(min(gamma), 3),
                           Q5 = round(quantile(gamma, 0.05), 3),
                           Q25 = round(quantile(gamma, 0.25), 3),
                           Q50 = round(quantile(gamma, 0.5), 3),
                           Q75 = round(quantile(gamma, 0.75), 3),
                           Q95 = round(quantile(gamma, 0.95), 3),
                           Max = round(max(gamma), 3),
                           Mean = round(mean(gamma), 3),
                           SD = round(sd(gamma), 3),
                           Skew = round(skewness(gamma), 3),
                           `L-scale` = round(get_sample_lmom(gamma)$sl2, 3),
                           `L-skew` = round(get_sample_lmom(gamma)$st3, 3),
                           `L-kurt` = round(get_sample_lmom(gamma)$st4, 3)),
                       `Record Lenght`]

gev_stats <- transpose(gev_stats, keep.names = " ")
