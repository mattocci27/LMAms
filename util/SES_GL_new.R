rm(list = ls()) # This clears everything from memory.
library(dplyr)
library(rstan)
setwd("~/Dropbox/MS/LES_MS/data/")

#use null result 1
null_n <- 1

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

# head(GL)
# head(data.n)

GL <- GL %>%
  mutate(LMA_ra = LMA.ra.data[, null_n]) %>%
  mutate(LMAp_pot_ra =  GL_summary_ra[P_vec, "mean"] * LMA_ra) %>%
  mutate(LMAs_pot_ra = LMA_ra - LMAp_pot_ra)
# add N, P data====================================
d <- read.csv("~/Dropbox/LES/nature02403-s2.csv", skip = 10)

GL <- data_frame(sp = as.character(d[ , "Species"]),
         Narea = 10^d[ , "log.Narea"],
         Parea = 10^d[ , "log.Parea"]) %>%
    group_by(sp) %>%
    summarize(Narea = mean(Narea), Parea = mean(Parea)) %>%
    left_join(GL, .,  by = "sp")

GL <- as.data.frame(GL)


par_names <- rownames(fit.summary)

obs_p <- extract(res, pars = par_names[grepl("p\\[", par_names)])

#
# cor_A <- NULL
# cor_A_n <- matrix(0, 1500, 10)
#
# rand_p <- list()
# for (i in 1:10) rand_p[[i]] <- extract(res1[[i]], pars = par_names[grepl("p\\[", par_names)])
#
# for (i in 1:1500){
#   LMAp_temp <- GL$LMA  * sapply(obs_p, "[", i)
#   LMAs_temp <- GL$LMA - LMAp_temp
#   cor_A[i] <- cor(log(LMAs_temp), log(GL$Aarea))
#
#   for (j in 1:10){
#     LMAp_temp_n <- LMA.ra.data[, j] * sapply(rand_p[[j]], "[", i)
#     LMAs_temp_n <- LMA.ra.data[, j] - LMAp_temp_n
#     cor_A_n[i, j] <- cor(log(LMAs_temp_n), log(GL$Aarea))
#   }
# }
#
#
#
# rand_sd <- apply(cor_A_n, 2, sd) %>% mean
# rand_mean <- apply(cor_A_n, 2, mean) %>% mean
# SES_LMAs_A <- (mean(cor_A) - rand_mean) / rand_sd
#
#
#
# mo <- function(x){
#   GL[x]
# }
#
# mo("Aarea")

cor_func <- function(LMAps = c("LMAp", "LMAs"), trait){
  cor_A <- NULL
  cor_A_n <- matrix(0, 1500, 10)
  par_names <- rownames(fit.summary)
  rand_p <- list()
  for (i in 1:10) rand_p[[i]] <- extract(res1[[i]], pars = par_names[grepl("p\\[", par_names)])

  for (i in 1:1500){
    LMAp_temp <- GL$LMA  * sapply(obs_p, "[", i)
    LMAs_temp <- GL$LMA - LMAp_temp

    if(LMAps == "LMAp") LMA_temp <- log(LMAp_temp) else LMA_temp <- log(LMAs_temp)

    cor_A[i] <- cor.test(LMA_temp, log(GL[, trait]))$estimate

    for (j in 1:10){
      LMAp_temp_n <- LMA.ra.data[, j] * sapply(rand_p[[j]], "[", i)
      LMAs_temp_n <- LMA.ra.data[, j] - LMAp_temp_n
      if(LMAps == "LMAp") LMA_temp_n <- log(LMAp_temp_n) else LMA_temp_n <- log(LMAs_temp_n)
      cor_A_n[i, j] <- cor.test(LMA_temp_n, log(GL[, trait]))$estimate
    }
  }

  rand_sd <- apply(cor_A_n, 2, sd) %>% mean
  rand_mean <- apply(cor_A_n, 2, mean) %>% mean
  SES <- (mean(cor_A) - rand_mean) / rand_sd

  c(obs = mean(cor_A), rand_mean = rand_mean, rand_sd = rand_sd, SES = SES)
}


moge <- rbind(cor_func(LMAps = "LMAp", trait = "Aarea"),
  cor_func(LMAps = "LMAs", trait = "Aarea"),
  cor_func(LMAps = "LMAp", trait = "Rarea"),
  cor_func(LMAps = "LMAs", trait = "Rarea"),
  cor_func(LMAps = "LMAp", trait = "LL"),
  cor_func(LMAps = "LMAs", trait = "LL"),
  cor_func(LMAps = "LMAp", trait = "Narea"),
  cor_func(LMAps = "LMAs", trait = "Narea"),
  cor_func(LMAps = "LMAp", trait = "Parea"),
  cor_func(LMAps = "LMAs", trait = "Parea"))

moge <- as.data.frame(moge) %>%
  mutate(p = dnorm(SES))

moge_row <- expand.grid(c("LMAp", "LMAs"), c("A", "R", "LL", "N", "P"))
rownames(moge) <-  paste(moge_row[,1], moge_row[,2], sep = "_")

write.csv(moge, "~/Desktop/GL_rand.csv")
