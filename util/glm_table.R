rm(list = ls()) # This clears everything from memory.
library(dplyr)
library(MASS)
load("~/Dropbox/MS/LES_MS/data/LMAps_res_20160503.RData")

PA2 <- PA2 %>%
  mutate(cell_area2 = cell_mass / 100 * LMA.leaf) %>%
  mutate(cell_vol2 = cell_area / LT / 100 / 100 / 0.1) %>%
  mutate(LD2 = LMA/)


# res1 <- lm(log(LL) ~ site + strata + scale(log(LMA.leaf)) + scale(log(cell_vol2)) + scale(log(Aarea)), PA2)
# summary(res1)
# AIC(res1)

res1 <- lm(log(LL) ~ scale(log(LMA)) + scale(log(cell_vol)) + scale(log(Aarea)) + site + strata, PA2)
summary(res1)
AIC(res1)

res2 <- lm(log(LL) ~ scale(log(LD)) + scale(log(cell_area)) + scale(log(Aarea)) + site + strata, PA2)
summary(res2)
AIC(res2)



# res2 <- lm(log(LL) ~ site + strata  + log(LD) + log(cell_area2) + log(Aarea), PA2)
# summary(res2)
# AIC(res2)
res3 <- lm(log(LL) ~ scale(log(LD)) + scale(log(cell_vol)) + scale(log(Aarea)) + site + strata , PA2)
summary(res3)
AIC(res3)

res4 <- lm(log(LL) ~ scale(log(LMA)) + scale(log(cell_area)) + scale(log(Aarea)) + site + strata, PA2)
summary(res4)
AIC(res4)


moge <- rbind(summary(res1)$coefficients,
  summary(res2)$coefficients,
  summary(res3)$coefficients,
  summary(res4)$coefficients)


write.csv(moge, "~/Dropbox/MS/LES_MS/data/glm_table.csv")



res1 <- lm(log(LL) ~ site + strata + log(LMA) + log(LD) + log(cell_vol) + log(Aarea), PA2)

res2 <- lm(log(LL) ~ site + strata + log(LMA) + log(LD) + log(cell_mass) + log(Aarea) + log(LT), PA2)


res3 <- lm(log(LL) ~ site + strata + log(LMA) + log(LD) + log(cell_area) + log(Aarea), PA2)


AIC(res1)
AIC(res2)
AIC(res3)

#
res1 <- lm(log(LL) ~ site + strata + log(LMA) + log(cell_vol) + log(Aarea), PA2)

summary(res1)

res2 <- lm(log(LL) ~ site + strata  + log(LD) + log(cell_area) + log(Aarea), PA2)

summary(res2)


res3 <- lm(log(LL) ~ site + strata  + log(LD) + log(cell_vol) + log(Aarea), PA2)
AIC(res3)


res3 <- lm(log(LL) ~ site + strata  + log(LMA) + log(cell_area) + log(Aarea), PA2)
AIC(res3)


# res3 <- lm(log(LL) ~ site + strata  + log(LD) + log(cell_mass) + log(Aarea), PA2)


AIC(res1)
AIC(res2)
AIC(res3)
