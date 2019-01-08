rm(list = ls()) # This clears everything from memory.
library(loo)

##model3r
#load("./data/PA_sitePL_LD_L_obs.rda")
load("./data/PA_sitePL_LD_L_MVN_obs.rda")

waic(extract_log_lik(res,"log_lik"))$waic
loo(extract_log_lik(res,"log_lik"))

summary_PA_LDL <- data.frame(summary(res)$summary)

PA <- dat %>%
  as_data_frame %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
  mutate(site_strata = paste(site2, strata, sep = "_"))


P_vec <- paste("p[", 1:nrow(PA), "]", sep = "")
mu2_vec <- paste("mu2[", 1:nrow(PA), "]", sep = "")
#Mu_vec <- paste("mu[", 1:nrow(PA), ",2]", sep = "")
temp_LMAp <- summary_PA_LDL[P_vec, "mean"] * PA$LMA
temp_LMAs <- PA$LMA - temp_LMAp
temp_LL <- exp(summary_PA_LDL[mu2_vec, "mean"])

PA <- PA %>%
  mutate(LMAp = temp_LMAp) %>%
  mutate(LMAs = temp_LMAs) %>%
  mutate(preLL = temp_LL) %>%
  mutate(LDs = LMAs/LT/1000)

cor.test(log(PA$LMAp), log(PA$Aarea))
cor.test(log(PA$LMAs), log(PA$Aarea))

cor.test(log(PA$LDs), log(PA$Aarea + PA$Rarea))

log_LMAp <- rstan::extract(res, "log_LMAp")[[1]]
log_LMAs <- rstan::extract(res, "log_LMAs")[[1]]
as <- rstan::extract(res, "alpha_s")[[1]]

log_LDs <- 1 * sweep(log_LMAs, 2, log(PA$LT))

tmp <- log_LDs %>% apply(., 2, median) %>% exp 

plot(tmp/1000, PA$LDs, log = "xy")
abline(a=0,b=1)

preLL <- rstan::extract(res, "mu2")[[1]]


apply(preLL, 1, function(x)cor(x, log(PA$LL))) %>% hist
apply(log_LMAp, 1, function(x)cor(x, log(PA$Aarea))) %>% hist
apply(log_LMAs, 1, function(x)cor(x, log(PA$Aarea))) %>% hist
apply(log_LDs, 1, function(x)cor(x, log(PA$Aarea))) %>% hist
apply(log_LDs, 1, function(x)cor(x, log(PA$Aarea + PA$Rarea))) %>% hist

cor(log(PA$LMA), log(PA$Aarea))
plot(LMA ~ LMAs, PA, log = "xy")
abline(a=0,b=1)


par(mfrow = c(1,2))
plot(Aarea ~ LMAp, PA, log = "xy")
plot(Aarea ~ LDs, PA, log = "xy")
par(mfrow = c(1,1))

plot(Rarea ~ LMAp, PA, log = "xy")

plot(Rarea ~ LMAs, PA, log = "xy")

plot(LL ~ LMAp, PA, log = "xy")

plot(LL ~ LMAs, PA, log = "xy")
plot(LL ~ preLL, PA, log = "xy")

# rand LMA
load("PA_model3r2_LMA.ra.data_2017-06-30_.RData")
# waic(extract_log_lik(res,"log_lik"))$waic

s3rLMA <- fit.summary.list[[null_n]]
temp <- LMA.ra.data[,null_n]
temp_LL <- exp(s3rLMA[Mu_vec, "mean"])

PA <- PA %>%
  mutate(LMA_ra = temp) %>%
  mutate(LMAp_ra =  s3rLMA[P_vec, "mean"] * LMA_ra) %>%
  mutate(LMAs_ra = LMA_ra - LMAp_ra) %>%
  mutate(preLL_ra = exp(s3rLMA[Mu_vec, "mean"])
         )

load("PA_model3r_LMA.all.data2_2017-07-12_.RData")
#load("PA_model3r6_LMA.all.data2_2017-09-14_.RData")
s3r_all <- fit.summary.list[[null_n]]

temp <- LMA.all.data[[null_n]]

#plot(Aarea ~ Rarea, log = "xy", temp)

PA <- PA %>%
  mutate(LMA_all = temp[ ,"LMA"]) %>%
  mutate(LMAp_all =  s3r_all[P_vec, "mean"] * LMA_all) %>%
  mutate(LMAs_all = LMA_all - LMAp_all) %>%
  mutate(LL_all = temp[ ,"LL"]) %>%
  mutate(Aarea_all = temp[, "Aarea"]) %>%
  mutate(Rarea_all = temp[, "Rarea"]) %>%
  mutate(preLL_all = exp(s3r_all[Mu_vec, "mean"])
         )

load("PA_model1r_2017-07-07_.RData")
s1r <- fit.summary

temp_LMAp <- s1r[P_vec, "mean"] * PA$LMA
temp_LMAs <- PA$LMA - temp_LMAp
temp_LL <- exp(s1r[Mu_vec, "mean"])

PA <- PA %>%
  mutate(LMAp_pot = temp_LMAp) %>%
  mutate(LMAs_pot = temp_LMAs) %>%
  mutate(preLL_pot = temp_LL)

load("PA_model1r_LMA.all.data_2017-07-07_.RData")
s1r_all <- fit.summary.list[[null_n]]

temp <- LMA.all.data[[null_n]]

PA <- PA %>%
  mutate(LMAp_pot_all =  s1r_all[P_vec, "mean"] * LMA_all) %>%
  mutate(LMAs_pot_all = LMA_all - LMAp_pot_all) %>%
  mutate(preLL_pot_all = exp(s1r_all[Mu_vec, "mean"])
         )


# fiber data -------------------------------------------------------------------
fiber <- read.csv("~/Dropbox/LES/fiber_analysis.csv")
trait <- read.csv("~/Dropbox/LES/LFTRAITS.csv")

trait2 <- trait %>%
  mutate(strata = ifelse(STRATA. == "CANOPY", "CAN", "UNDER")) %>%
  mutate(site = ifelse(trait$SITE. == "PNM", "PNM", "PNLS")) %>%
  mutate(site2 = ifelse(trait$SITE. == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(SP4., site2, strata, sep = "_")) %>%
  mutate(LMA_LEAF = 1/SLA_LEAF * 10000) %>%
  mutate(LMA = 1/SLA_DISC * 10000) %>%
  mutate(Narea = LMA_LEAF * N_PCT / 1000) %>%
  mutate(Parea = LMA_LEAF * P_PCT / 1000) %>%
  mutate(LD = LMA / LFTHICK / 1000) %>%
  mutate(LAMTUF = LAMTUF / 1000, MDRBTUF = MDRBTUF / 1000,
      VEINTUF = VEINTUF / 1000)

fiber2 <- fiber %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, position, sep = "_"))

fdata <- full_join(trait2, fiber2, by = "sp_site_strata") %>%
  select(sp_site_strata, X.ADF, X.NPE, X.Lignin,
     LFTHICK, LD, Narea, Parea, LAMTUF, MDRBTUF, VEINTUF) %>%
  rename(ADF = X.ADF, NPE = X.NPE, Lig = X.Lignin, LT = LFTHICK)

PA2 <- left_join(PA, fdata, by = "sp_site_strata") %>%
  mutate(cell_mass = ADF - Lig) %>%
  mutate(cell_area = cell_mass / 100 * LMA) %>%
  mutate(cell_vol = cell_area / LT / 100 / 100 / 0.1) # g/cm3

write.csv(PA2, "~/Dropbox/MS/LES_MS/LMApsModel/data/PA20170911.csv",
          row.names = FALSE)



