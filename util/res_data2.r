rm(list = ls()) # This clears everything from memory.
library(dplyr)
setwd("~/Dropbox/MS/LES_MS/data/")

#use null result 1
null_n <- 6

# GL data -------------------------------------------------------------------
load("GL_obs_model1_2016-07-19_.RData")

GL_summary <- fit.summary
GL <- data
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) %>%
  mutate(LMAp_pot =  fit.summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAs_pot = LMA - LMAp_pot)

# GL random across =========================================================
load("GL_rand_across_2016-07-19_.RData")
GL_summary_ra <- fit.summary1[[null_n]]

# P_GL <- sapply(fit.summary1, function(x){x[P_vec, "mean"]})
# LMAp_pot_ra <- apply(P_GL * LMA.ra.data, 1, mean)
# LMAs_pot_ra <- apply((1-P_GL) * LMA.ra.data, 1, mean)

# head(GL)
# head(data.n)

GL <- GL %>%
  mutate(LMA_ra = LMA.ra.data[, null_n]) %>%
  mutate(LMAp_pot_ra =  GL_summary_ra[P_vec, "mean"] * LMA_ra) %>%
  mutate(LMAs_pot_ra = LMA_ra - LMAp_pot_ra)


# GL2 <- GL %>%
#   mutate(LMA_ra = apply(LMA.ra.data, 1, mean)) %>%
#   mutate(LMAp_pot_ra =  LMAp_pot_ra) %>%
#   mutate(LMAs_pot_ra = LMAs_pot_ra)

dd <- data.frame(Narea = 10^d$log.Narea,
        Parea = 10^d$log.Parea,
        sp = d$Species) %>%
        group_by(sp) %>%
        summarize(Narea = mean(Narea, na.omit = T),
            Parea = mean(Parea, na.omit = T))

GL <- left_join(GL, dd, by = "sp")

# cor(log(GL$Aarea), log(GL$LMAp_pot_ra))

# PA data -------------------------------------------------------------------
#use null result 5 for PA
null_n <- 5
model_name <- c("model1", "model2", "model3")

model_name2 <- sapply(strsplit(model_name, "model"), "[[", 2)

temp_LMA <- NULL
for (j in 1:3){
  temp_name <- model_name[j]
  load(paste("PA_", temp_name, "_2016-07-19_.RData", sep = ""))

  if (j == 1){
    PA <- data %>%
      mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
      mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
      mutate(site_strata = paste(site2, strata, sep = "_"))

    P_vec <- paste("p[", 1:nrow(PA), "]", sep = "")
    Mu_vec <- paste("mu[", 1:nrow(PA), ",2]", sep = "")
  }

  assign(paste("PA_summary", model_name2[j], sep = "_"), fit.summary)

  temp_LMAp <- fit.summary[P_vec, "mean"] * PA$LMA
  temp_LMAs <- PA$LMA - temp_LMAp
  temp_LL <- exp(fit.summary[Mu_vec, "mean"])
  temp_LMA <- cbind(temp_LMA, temp_LMAp, temp_LMAs, temp_LL)
}

colnames(temp_LMA) <- c("LMAp_pot", "LMAs_pot", "preLL_pot",
  "LMAp_opt", "LMAs_opt", "preLL_opt",
  "LMAp_site", "LMAs_site", "preLL_site")

rand_name <- c("LMA_ra_data", "LMA_rc_data", "LMA_rw_data")
rand_name2 <- c("LMA.ra.data", "LMA.rc.data", "LMA.rw.data")
rand_name3 <- c("ra", "rc", "rw")
model_name3 <- c("pot", "opt", "site")
PA_rand <- NULL
temp_name <- NULL
for (j in 1:3){
  for (k in 1:3){
    load(paste("PA_", model_name[j], "_", rand_name[k], "_2016-07-19_.RData", sep = ""))
    temp_LMA_null <- get(rand_name2[k])[, null_n]
    temp_summary <- fit.summary.list[[null_n]]
    temp_LMAp <- temp_summary[P_vec, "mean"] * temp_LMA_null
    temp_LMAs <- temp_LMA_null - temp_LMAp
    temp_LL <- exp(temp_summary[Mu_vec, "mean"])
    PA_rand <- cbind(PA_rand, temp_LMAp, temp_LMAs, temp_LL)

    temp_name <- c(temp_name,
          paste("LMAp_", model_name3[j], "_", rand_name3[k], sep = ""),
          paste("LMAs_", model_name3[j], "_", rand_name3[k], sep = ""),
          paste("preLL_", model_name3[j], "_", rand_name3[k], sep = ""))
  }
}
colnames(PA_rand) <- temp_name

PA_temp <- cbind(PA, temp_LMA, PA_rand)

PA_temp <- PA_temp %>%
  mutate(LMA_ra = LMA.ra.data[ , null_n]) %>%
  mutate(LMA_rc = LMA.rc.data[ , null_n]) %>%
  mutate(LMA_rw = LMA.rc.data[ , null_n])





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

PA2 <- left_join(PA_temp, fdata, by = "sp_site_strata") %>%
  mutate(cell_mass = ADF - Lig) %>%
  mutate(cell_area = cell_mass / 100 * LMA) %>%
  mutate(cell_vol = cell_area / LT / 100 / 100 / 0.1) # g/cm3


#check LMA values
PA2 %>% group_by(site_strata) %>%
  summarize(ave = mean(LMA))

PA2 %>% group_by(site_strata) %>%
  summarize(ave = mean(LMA_rc))

PA2 %>% group_by(site_strata) %>%
  summarize(ave = mean(LMA_ra))

PA2 %>% group_by(site) %>%
  summarize(ave = mean(LMA))

PA2 %>% group_by(site) %>%
  summarize(ave = mean(LMA_rc))

PA2 %>% group_by(site) %>%
  summarize(ave = mean(LMA_ra))

rownames(PA2) <- NULL

save.image("LMAps_res_20160929.RData")
