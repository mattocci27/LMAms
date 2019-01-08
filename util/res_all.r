rm(list = ls()) # This clears everything from memory.
library(dplyr)
setwd("~/Dropbox/MS/LES_MS/LMApsModel/data/")

#use null result 1
null_n <- 3

# GL data -------------------------------------------------------------------
load("GL_model1_obs_2017-06-13_.RData")

GL_summary1 <- fit.summary
GL <- data.obs
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) %>%
  mutate(LMAp1 =  fit.summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAs1 = LMA - LMAp1)

load("GL_model1r_obs_2017-06-14_.RData")
temp1 <- fit.summary

load("GL_model1r2_obs_2017-06-14_.RData")
temp2 <- fit.summary

load("GL_model1r3_obs_2017-06-14_.RData")
temp3 <- fit.summary

load("GL_model1r4_obs_2017-06-14_.RData")
temp4 <- fit.summary

load("GL_model1r5_obs_2017-06-19_.RData")
temp5 <- fit.summary

GL_summary4 <- fit.summary

GL <- GL %>%
  mutate(LMAp4 =  fit.summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAs4 = LMA - LMAp4)


par(mfrow = c(2,2))
plot(LMAp1 ~ Aarea, log = "xy", GL)
plot(LMAp4 ~ Aarea, log = "xy", GL)
plot(LMAs1 ~ LL, log = "xy", GL)
plot(LMAs4 ~ LL, log = "xy", GL)


par(mfcol = c(3,3))
plot(Aarea ~ LMA, log = "xy", GL)
plot(Rarea ~ LMA, log = "xy", GL)
plot(LL ~ LMA, log = "xy", GL)
plot(Aarea ~ LMAp4, log = "xy", GL)
plot(Rarea ~ LMAp4, log = "xy", GL)
plot(LL ~ LMAp4, log = "xy", GL)
plot(Aarea ~ LMAs4, log = "xy", GL)
plot(Rarea ~ LMAs4, log = "xy", GL)
plot(LL ~ LMAs4, log = "xy", GL)

par(mfcol = c(3,3))
plot(Aarea ~ LMA, log = "xy", GL)
plot(Rarea ~ LMA, log = "xy", GL)
plot(LL ~ LMA, log = "xy", GL)
plot(Aarea ~ LMAp1, log = "xy", GL)
plot(Rarea ~ LMAp1, log = "xy", GL)
plot(LL ~ LMAp1, log = "xy", GL)
plot(Aarea ~ LMAs1, log = "xy", GL)
plot(Rarea ~ LMAs1, log = "xy", GL)
plot(LL ~ LMAs1, log = "xy", GL)

GL.each <- function(x, y, x_text = NULL, y_text = NULL, subscript = NULL){
  dec <- GL %>%
    filter(DE == "D")
  ev <- GL %>%
    filter(DE == "E")
  un <- GL %>%
    filter(DE == "U")

  options("scipen" = 100)
  if(x == "LMA") plot(GL[ , x], GL[ ,y],
              log = "xy", axes = F,
              xlim = lim_func(GL[ , x]), ylim = lim_func(GL[ ,y]),
              type = "n") else {
          plot(GL[ , x], GL[ ,y],
              log = "xy", axes = F,
              xlim = lim_func(GL[ , x]), ylim = lim_func(GL[ ,y]),
              type = "n")
    }
  mtext(subscript, side = 3, line = -1, adj = 0, cex = 0.65)

  box()

  points(un[ , x], un[ ,y], col = "black", pch = 21, bg = "white")
  points(ev[ , x], ev[ ,y], col = "black", pch = 21, bg = "green4")
  points(dec[ , x], dec[ ,y], col = "black", pch = 21, bg = "dark orange")

  if(is.null(y_text) != TRUE){
    axis(2, tick = FALSE,line = -0.8, cex.axis = 0.9)
    axis(2, tcl = 0.2, labels = FALSE)
    mtext(y_text, side = 2, line = 1.2, cex = 0.8)
  }

  if(is.null(x_text) != TRUE){
    axis(1, tick = FALSE, line = -0.8, cex.axis = 0.9)
    axis(1, tcl = 0.2, labels = FALSE, cex = 0.8)
    mtext(x_text, side = 1, line = 1.5, cex = 0.8)
  }
}


postscript("~/Desktop/temp.eps",
  width = 4.33, height = 4.33)
par(mfrow = c(3, 3))
par(mar = c(0, 0, 0, 0))
par(oma = c(3, 3, 2, 2))
GL.each(x = "LMA", y = "Aarea")
GL.each(x = "LMAp4", y = "Aarea")
GL.each(x = "LMAs4", y = "Aarea")

GL.each(x = "LMA", y = "Rarea")
GL.each(x = "LMAp4", y = "Rarea")
GL.each(x = "LMAs4", y = "Rarea")

GL.each(x = "LMA", y = "LL")
GL.each(x = "LMAp4", y = "LL")
GL.each(x = "LMAs4", y = "LL")
dev.off()

# GL random across =========================================================
load("GL_model1r_All_2017-06-13_.RData")
GL_summary4_all <- fit.summary.list[[null_n]]

temp <- LMA.ra2.data[[null_n]]

GL <- GL %>%
  mutate(LMA_all = temp[,"LMA"]) %>%
  mutate(LMAp4_all =  GL_summary4_all[P_vec, "mean"] * LMA_all) %>%
  mutate(LMAs4_all = LMA_all - LMAp4_all) %>%
  mutate(LL_all = temp[, "LL"]) %>%
  mutate(Rarea_all = temp[, "Rarea"]) %>%
  mutate(Aarea_all = temp[, "Aarea"]) 


load("GL_model1_All_2017-06-13_.RData")
GL_summary1_all <- fit.summary.list[[null_n]]


GL <- GL %>%
  mutate(LMAp1_all =  GL_summary1_all[P_vec, "mean"] * LMA_all) %>%
  mutate(LMAs1_all = LMA_all - LMAp1_all) 


load("GL_model1_LMA_2017-06-13_.RData")
GL_summary1_ra <- fit.summary.list[[null_n]]

temp <- LMA.ra.data[[null_n]]

GL <- GL %>%
  mutate(LMA_ra = temp) %>%
  mutate(LMAp1_ra =  GL_summary1_ra[P_vec, "mean"] * LMA_ra) %>%
  mutate(LMAs1_ra = LMA_ra - LMAp1_ra) 


load("GL_model1r_LMA_2017-06-13_.RData")
GL_summary4_ra <- fit.summary.list[[null_n]]


GL <- GL %>%
  mutate(LMAp4_ra =  GL_summary4_ra[P_vec, "mean"] * LMA_ra) %>%
  mutate(LMAs4_ra = LMA_ra - LMAp4_ra) 

par(mfrow=c(2,2))
plot(Aarea ~ LMAp1_ra, log = "xy", GL)
plot(Aarea_all ~ LMAp1_all, log = "xy", GL)
plot(Aarea ~ LMAp4_ra, log = "xy", GL)
plot(Aarea_all ~ LMAp4_all, log = "xy", GL)

par(mfrow=c(2,2))
plot(LL ~ LMAs1_ra, log = "xy", GL)
plot(LL_all ~ LMAs1_all, log = "xy", GL)
plot(LL ~ LMAs4_ra, log = "xy", GL)
plot(LL_all ~ LMAs4_all, log = "xy", GL)



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

rand_name <- c("LMA_ra_data", "LMA_rc_data")
rand_name2 <- c("LMA.ra.data", "LMA.rc.data")
rand_name3 <- c("ra", "rc")
model_name3 <- c("pot", "opt", "site")
PA_rand <- NULL
temp_name <- NULL
for (j in 1:3){
  for (k in 1:2){
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
  mutate(LMA_rc = LMA.rc.data[ , null_n])




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

save.image("LMAps_res_20160719.RData")
