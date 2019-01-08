rm(list = ls()) # This clears everything from memory.
setwd("~/Dropbox/MS/LES_MS/data/")
library(rstan)
library(dplyr)
# before <- proc.time()
# library(rstan)
# load("GL_obs_model1.RData")
load("LMAps_res_20160719.RData")


GL.summary <- fit.summary
# # GL <- data
# GL$DE <- ifelse(GL$DE=="","U",as.character(GL$DE))
#
# GL$LMAp_pot <- fit.summary[paste("p[",1:198,"]",sep=""),"mean"]*data$LMA
# GL$LMAs_pot <- GL$LMA -  GL$LMAp_pot


LL.cor.a <- cor(log10(GL$LL), log10(GL$LMA))
LL.cor.p <- cor(log10(GL$LL), log10(GL$LMAp_pot))
LL.cor.s <- cor(log10(GL$LL), log10(GL$LMAs_pot))

A.cor.a <- cor(log10(GL$Aarea), log10(GL$LMA))
A.cor.p <- cor(log10(GL$Aarea), log10(GL$LMAp_pot))
A.cor.s <- cor(log10(GL$Aarea), log10(GL$LMAs_pot))

R.cor.a <- cor(log10(GL$Rarea), log10(GL$LMA))
R.cor.p <- cor(log10(GL$Rarea), log10(GL$LMAp_pot))
R.cor.s <- cor(log10(GL$Rarea), log10(GL$LMAs_pot))


N.cor.a <- cor.test(log10(GL$Narea), log10(GL$LMA))$estimate
N.cor.p <- cor.test(log10(GL$Narea), log10(GL$LMAp_pot))$estimate
N.cor.s <- cor.test(log10(GL$Narea), log10(GL$LMAs_pot))$estimate

P.cor.a <- cor.test(log10(GL$Parea), log10(GL$LMA))$estimate
P.cor.p <- cor.test(log10(GL$Parea), log10(GL$LMAp_pot))$estimate
P.cor.s <- cor.test(log10(GL$Parea), log10(GL$LMAs_pot))$estimate

GL.obs <-cbind(c(A.cor.a, R.cor.a, LL.cor.a, N.cor.a, P.cor.a),
        c(A.cor.p, R.cor.p, LL.cor.p, N.cor.p, P.cor.p),
        c(A.cor.s, R.cor.s, LL.cor.s, N.cor.s, P.cor.s))

colnames(GL.obs) <- c("LMA","LMAp_pot","LMAs_pot")
GL.obs <- apply(GL.obs,2,function(x) round(x,3))
rownames(GL.obs) <- c("A", "R", "LL", "N", "P")

####GL random across
load("GL_rand_across_2016-07-19_.RData")


GL.ra <- list()
for (i in 1:10){
  GL.summary1.ra <- fit.summary1[[i]]
  GL$LMA_ra<- LMA.ra.data[,i]
  GL$LMAp_pot_ra <- GL.summary1.ra[paste("p[",1:198,"]",sep=""),"mean"] * GL$LMA_ra
  GL$LMAs_pot_ra <- GL$LMA_ra -  GL$LMAp_pot_ra

  LL.cor.a <- cor(log10(GL$LL), log10(GL$LMA_ra))
  LL.cor.p <- cor(log10(GL$LL), log10(GL$LMAp_pot_ra))
  LL.cor.s <- cor(log10(GL$LL), log10(GL$LMAs_pot_ra))

  A.cor.a <- cor(log10(GL$Aarea), log10(GL$LMA_ra))
  A.cor.p <- cor(log10(GL$Aarea), log10(GL$LMAp_pot_ra))
  A.cor.s <- cor(log10(GL$Aarea), log10(GL$LMAs_pot_ra))

  R.cor.a <- cor(log10(GL$Rarea), log10(GL$LMA_ra))
  R.cor.p <- cor(log10(GL$Rarea), log10(GL$LMAp_pot_ra))
  R.cor.s <- cor(log10(GL$Rarea), log10(GL$LMAs_pot_ra))

  N.cor.a <- cor.test(log10(GL$Narea), log10(GL$LMA_ra))$estimate
  N.cor.p <- cor.test(log10(GL$Narea), log10(GL$LMAp_pot_ra))$estimate
  N.cor.s <- cor.test(log10(GL$Narea), log10(GL$LMAs_pot_ra))$estimate

  P.cor.a <- cor.test(log10(GL$Parea), log10(GL$LMA_ra))$estimate
  P.cor.p <- cor.test(log10(GL$Parea), log10(GL$LMAp_pot_ra))$estimate
  P.cor.s <- cor.test(log10(GL$Parea), log10(GL$LMAs_pot_ra))$estimate

  temp <-cbind(c(A.cor.a, R.cor.a, LL.cor.a, N.cor.a, P.cor.a),
          c(A.cor.p, R.cor.p, LL.cor.p, N.cor.p, P.cor.p),
          c(A.cor.s, R.cor.s, LL.cor.s, N.cor.s, P.cor.s))

  colnames(temp) <- c("LMA","LMAp_pot","LMAs_pot")
  temp <- apply(temp,2,function(x) round(x,3))
  rownames(temp) <- c("A", "R", "LL", "N", "P")

  GL.ra[[i]] <- temp
}

temp2 <- NULL
GL.ra.r2 <- GL.obs
GL.ra.sd2 <- GL.obs
GL.ra.r <- GL.obs
GL.ra.sd <- GL.obs

for (i in 1:nrow(GL.ra[[1]])){
  for (j in 1:ncol(GL.ra[[1]])){
    for (k in 1:10){
      temp2[k] <- GL.ra[[k]][i,j]
    }
    GL.ra.r2[i,j] <- mean(temp2^2, na.rm = T)
    GL.ra.sd2[i,j] <- sd(temp2^2, na.rm = T)
    GL.ra.r[i,j] <- mean(temp2, na.rm = T)
    GL.ra.sd[i,j] <- sd(temp2, na.rm = T)
  }
}


GL.ra.SES2 <- round((GL.obs^2 - GL.ra.r2)/GL.ra.sd2,3)

GL.ra.SES <- round((GL.obs - GL.ra.r)/GL.ra.sd,3)

GL.ra.r <- round(GL.ra.r,3)
GL.ra.sd <- round(GL.ra.sd,3)

GL.ra.tb <- NULL
for (i in 1:3){
  res1 <- paste(GL.ra.r[,i]," (", GL.ra.sd[,i],")",sep="")
  GL.ra.tb <- cbind(GL.ra.tb, GL.obs[,i], res1, GL.ra.SES[,i], GL.ra.SES2[,i])
}


test <- cbind(rep(c("A", "R", "LL", "N", "P"), each = 3),
      rep(c("LMA","LMAp","LMAs")),
      as.vector(t(GL.obs)),
      as.vector(t(GL.ra.r)),
      as.vector(t(GL.ra.sd)),
      as.vector(t(GL.ra.SES)),
      as.vector(t(GL.ra.SES2))
      )

GL.tb <- as.data.frame(test)
colnames(GL.tb) <- c("trait","LMA","r_obs","r_ra","r_ra_sd","SES_ra", "SES2")

GL.tb[, 3:7] <- apply(GL.tb[, 3:7], 2, function(x)as.numeric(as.character(x)))
GL.tb %>%
  mutate(SES_re = ifelse(r_obs > 0, SES_ra, - SES_ra))
#

GL.tb2 <- GL.tb %>%
  mutate(SES_ra = as.numeric(as.character(SES_ra))) %>%
  mutate(r_ra = as.numeric(as.character(r_ra))) %>%
  mutate(r_ra_sd = as.numeric(as.character(r_ra_sd))) %>%
  mutate(p_val = pnorm(SES_ra, lower.tail = F) ) %>%
  # mutate(p_val2 = ifelse(SES_ra > 0,
    # 1 - pnorm(SES_ra, 0, 1), pnorm(SES_ra, 0, 1))) %>%
  mutate(p_val = round(p_val, 4))
  # mutate(p_val2 = round(p_val2, 4))

GL.tb2
write.csv(GL.tb2, "~/Dropbox/MS/LES_MS/data/GL_rand_table.csv", row.names = F)

#
# #new SES
# load("GL_obs_model1_2016-07-19_.RData")
# par_names <- rownames(fit.summary)
# obs_p <- extract(res, pars = par_names[grepl("p\\[", par_names)])
#
# cor_func <- function(LMAps = c("LMAp", "LMAs"), trait){
#   cor_A <- numeric(1500)
#   cor_A_n <- matrix(0, 1500, 10)
#   par_names <- rownames(fit.summary)
#   rand_p <- list()
#   for (i in 1:10) rand_p[[i]] <- extract(res1[[i]],
#               pars = par_names[grepl("p\\[", par_names)])
#
#   for (i in 1:1500){
#     LMAp_temp <- GL$LMA  * sapply(obs_p, "[", i)
#     LMAs_temp <- GL$LMA - LMAp_temp
#
#     if(LMAps == "LMAp") LMA_temp <- log(LMAp_temp) else LMA_temp <- log(LMAs_temp)
#
#     cor_A[i] <- cor.test(LMA_temp, log(GL[, trait]))$estimate
#
#     for (j in 1:10){
#       LMAp_temp_n <- LMA.ra.data[, j] * sapply(rand_p[[j]], "[", i)
#       LMAs_temp_n <- LMA.ra.data[, j] - LMAp_temp_n
#       if(LMAps == "LMAp") LMA_temp_n <- log(LMAp_temp_n) else LMA_temp_n <- log(LMAs_temp_n)
#       cor_A_n[i, j] <- cor.test(LMA_temp_n, log(GL[, trait]))$estimate
#     }
#   }
#
#   rand_sd <- apply(cor_A_n, 2, sd) %>% mean
#   rand_mean <- apply(cor_A_n, 2, mean) %>% mean
#   SES <- (mean(cor_A) - rand_mean) / rand_sd
#
#   c(obs = mean(cor_A), rand_mean = rand_mean, rand_sd = rand_sd, SES = SES)
# }
#
# moge <- rbind(cor_func(LMAps = "LMAp", trait = "Aarea"),
#   cor_func(LMAps = "LMAs", trait = "Aarea"),
#   cor_func(LMAps = "LMAp", trait = "Rarea"),
#   cor_func(LMAps = "LMAs", trait = "Rarea"),
#   cor_func(LMAps = "LMAp", trait = "LL"),
#   cor_func(LMAps = "LMAs", trait = "LL"),
#   cor_func(LMAps = "LMAp", trait = "Narea"),
#   cor_func(LMAps = "LMAs", trait = "Narea"),
#   cor_func(LMAps = "LMAp", trait = "Parea"),
#   cor_func(LMAps = "LMAs", trait = "Parea"))
#
# moge <- as.data.frame(moge) %>%
#   mutate(p = dnorm(SES))
#
# moge_row <- expand.grid(c("LMAp", "LMAs"), c("A", "R", "LL", "N", "P"))
# rownames(moge) <-  paste(moge_row[,1], moge_row[,2], sep = "_")
#
# write.csv(moge,"~/Dropbox/MS/LES_MS/data/GL_SES.csv", row.names = F)
