# the code is messy
rm(list = ls()) # This clears everything from memory.
before <- proc.time()
library(rstan)
library(dplyr)
setwd("~/Dropbox/MS/LES_MS/data/")

# Obs data-----------------------------------------------------------------
load("LMAps_res_20160719.RData")

# rp.obs <- PA_summary_2_2["rp", "mean"]
# rs.obs <- PA_summary_2_2["rs", "mean"]

LMA.dat <- PA2 %>%
  select(LMA, LMAp_pot, LMAs_pot,
     LMAp_opt, LMAs_opt,
     LMAp_site, LMAs_site)

L.cor <- NULL
A.cor <- NULL
R.cor <- NULL
N.cor <- NULL
P.cor <- NULL
C.cor <- NULL
TF.cor <- NULL
preLL.cor <- NULL

for (i in 1:ncol(LMA.dat)) {
  L.cor[i] <- cor.test(log10(PA2$LL), log10(LMA.dat[ , i]))$estimate
  A.cor[i] <- cor.test(log10(PA2$Aarea), log10(LMA.dat[ , i]))$estimate
  R.cor[i] <- cor.test(log10(PA2$Rarea), log10(LMA.dat[ , i]))$estimate
  N.cor[i] <- cor.test(log10(PA2$Narea), log10(LMA.dat[ , i]))$estimate
  P.cor[i] <- cor.test(log10(PA2$Parea), log10(LMA.dat[ , i]))$estimate
  C.cor[i] <- cor.test(log10(PA2$cell_area), log10(LMA.dat[ , i]))$estimate
  TF.cor[i] <- cor.test(log10(PA2$LAMTUF), log10(LMA.dat[ , i]))$estimate
}

preLL.cor <- c(NA, NA,
  cor.test(log10(PA2$LL), log10(PA2$preLL_pot))$estimate,
  NA, cor.test(log10(PA2$LL), log10(PA2$preLL_opt))$estimate,
  NA, cor.test(log10(PA2$LL), log10(PA2$preLL_site))$estimate)

# PA.obs <- rbind(A.cor, R.cor, L.cor, preLL.cor, N.cor, P.cor, C.cor, TF.cor)
PA.obs <- rbind(A.cor, R.cor, L.cor, preLL.cor, N.cor, P.cor, C.cor, TF.cor)
colnames(PA.obs) <- c("LMA", "LMAp_pot", "LMAs_pot",
   "LMAp_opt", "LMAs_opt",
   "LMAp_site", "LMAs_site")

# random data  -----------------------------------------------------
rand_name <- c("LMA_ra_data", "LMA_rs_data", "LMA_rw_data")
rand.name <- c("LMA.ra.data", "LMA.rs.data", "LMA.rw.data")
rand_name2 <- c("ra", "rs", "rw")
model_name3 <- c("1", "2", "21a", "21b", "22")

res_name <- NULL
for (i in 1:5) for (j in 1:3) res_name <- c(res_name, paste(model_name3[i], rand_name2[j], sep = "_"))

moge <- list()

file.list <- getwd() %>%
  list.files
file.list <- file.list[grep("_ra_|_rc_", file.list)]
file.list <- file.list[grep(".RData$", file.list)]
file.list


trait_name <- c("Aarea", "Rarea", "LL", "preLL",
        "Narea", "Parea", "cell_area", "LAMTUF")
trait_name2 <- c("LMA", "LMAp", "LMAs")
moge <- list() # each data.frame comes from each model

moge2 <- NULL
for (l in 1:8) {
  if (trait_name[l] == "preLL") {
    moge2 <- c(moge2, "preLL")
} else { for (m in 1:3) moge2 <- c(moge2, paste(trait_name[l], trait_name2[m], sep = "_")) }
}

# 3 models * 2 randomization
LMA_list <- rep(c("LMA.ra.data", "LMA.rc.data"), 3)
for (j in 1:6){
  load(file.list[j])
  temp_data <- NULL
  for (k in 1:10){ # k: numbers of replications
    temp_LMA_null <- get(LMA_list[j])[, k]
    temp_LMAp <- fit.summary.list[[k]][P_vec, "mean"] * temp_LMA_null
    temp_LMAs <- temp_LMA_null - temp_LMAp
    temp_LL <- exp(fit.summary.list[[k]][Mu_vec, "mean"])

    LMA_dat <- data.frame(temp_LMA_null, temp_LMAp, temp_LMAs)
    temp_cor_data <- NULL
    for (l in 1:8) { # numbers of traits
      if (trait_name[l] == "preLL") {
        temp_cor <- cor.test(log10(temp_LL), log10(PA$LL))$estimate
        temp_cor_data <- c(temp_cor_data, temp_cor)
      } else
      for (m in 1:3){ # numers of LMA types: LMA, LMAp and LMAs
        temp_cor <- cor.test(log10(PA2[ , trait_name[l]]),
                log10(LMA_dat[, m]))$estimate
        temp_cor_data <- c(temp_cor_data, temp_cor)
      }
    }

    temp_data <- cbind(temp_data, temp_cor_data)
    rownames(temp_data) <- moge2
   }
    colnames(temp_data) <- paste("rep", 1:10, sep = "_")
    moge[[j]] <- temp_data
}

names(temp_cor_data) <- paste(trait_name[l], m)
names(moge) <- file.list

PA_r2 <- lapply(moge, function(x) x^2 )

PA_r_sd <- sapply(moge, function(x) apply(x, 1, sd) ) %>% data.frame
PA_r_mean <- sapply(moge, function(x) apply(x, 1, mean)) %>% data.frame
PA_r2_sd <- sapply(PA_r2, function(x) apply(x, 1, sd) ) %>% data.frame
PA_r2_mean <- sapply(PA_r2, function(x) apply(x, 1, mean)) %>% data.frame

# make table (without preLL)=================================================
temp <- c("pot", "opt", "site")
temp2 <- c("model1", "model2", "model3")

res <- NULL
for (i in 1:3) { # numbers of models
  PA_data <- PA.obs %>%
    as.data.frame %>%
    mutate(trait = rownames(.)) %>%
    tidyr::gather("LMA_type", "r_value", 1:7) %>%
    mutate(model = c(rep("LMA", 8),
           rep(c("pot", "opt", "site"), each = 16))
           ) %>%
    mutate(temp = rep(1:8, 7)) %>%
    arrange(temp)

    PA_data <- PA_data[PA_data$model == temp[i] | PA_data$model == "LMA", ] %>%
          filter(trait != "preLL.cor")


  data_cbind <- PA_data %>%
    select(r_value) # using obs for the first col

  for (j in 1:2){ # numbers of randomization types
    PA_rand_mean <- PA_r_mean %>%
      select(contains(temp2[i])) %>%
      filter(rownames(.)!= "preLL") # drop preLL

    PA_rand_sd <- PA_r_sd %>%
      select(contains(temp2[i])) %>%
      filter(rownames(.)!= "preLL")

    PA_rand_mean2 <- PA_r2_mean %>%
      select(contains(temp2[i])) %>%
      filter(rownames(.)!= "preLL")

    PA_rand_sd2 <- PA_r2_sd %>%
      select(contains(temp2[i])) %>%
      filter(rownames(.)!= "preLL")

    # data_sub <- data.frame(rand.r = PA_rand_mean[ , j],
    #       rand.sd = PA_rand_sd[ , j],
    #       r.SES = (PA_data$r_value - PA_rand_mean[ , j])
    #           / PA_rand_sd[ , j])
    data_sub <- data.frame(rand.r = PA_rand_mean[ , j],
          rand.sd = PA_rand_sd[ , j],
          r.SES = (PA_data$r_value^2 - PA_rand_mean2[ , j])
              / PA_rand_sd2[ , j])
    data_cbind <- cbind(data_cbind, as.matrix(data_sub))
  }
    res <- rbind(res, data_cbind)
}

res_row1 <- c("Aarea", "Rarea", "LL", "Narea", "Parea", "Cell_area", "TF") %>%
  rep(each = 3) %>% # numbers of LMA types
  rep(3) # numbers of models

res_row2 <- rep(temp, each = 21) # numbers of traits
res_row3 <- rep(c("LMA", "LMAp", "LMAs"), 21)

res_m  <- cbind(res_row1, res_row3, res_row2, round(res,3))

colnames(res_m) <- c("triat", "LMA", "model", "r_obs", "r_ra", "sd_ra", "SES_ra", "r_rc", "sd_rc", "SES_rc")

res_data <- as.data.frame(res_m) %>%
    filter(LMA != "LMA" | model != "opt") %>%
    filter(LMA != "LMA" | model != "site") %>%
    mutate(model = as.character(model)) %>%
    mutate(model = ifelse(.$LMA == "LMA", NA, model)) %>%
    # mutate(p_rc = ifelse(r_obs > 0 , 1 - pnorm(SES_rc, 0, 1),
    #   pnorm(SES_rc, 0, 1))) %>%
    mutate(p_rc = pnorm(SES_rc, lower.tail = F)) %>%
    # mutate(p_ra = ifelse(r_obs > 0 , 1 - pnorm(SES_ra, 0, 1),
    #   pnorm(SES_ra, 0, 1))) %>%
    mutate(p_ra = pnorm(SES_ra, lower.tail = F)) %>%
    mutate(p_rc = round(p_rc, 4)) %>%
    mutate(p_ra = round(p_ra, 4))

res_data
# edit tables from exel

# table for predicted LL ------------------------------------------------------
res_LL_m <- NULL
for (i in 1:3) { # i: numbers of models
  PA_data <- PA.obs %>%
      as.data.frame %>%
      mutate(trait = rownames(.)) %>%
      tidyr::gather("LMA_type", "r_value", 1:7) %>%
      mutate(model = c(rep("LMA", 8),
             rep(c("pot", "opt", "site"), each = 16))
             ) %>%
      mutate(temp = rep(1:8, 7)) %>%
      arrange(temp)

  PA_data <- PA_data[PA_data$model == temp[i] | PA_data$model == "LMA", ] %>%
        filter(trait == "preLL.cor")


  data_c <- PA_data %>%
    select(r_value) %>% # using obs for the first col
    slice(3) # select LMAps

  for (j in 1:2){ # j: numbers of rand types
    PA_rand_mean <- PA_r_mean %>%
      select(contains(temp2[i])) %>%
      filter(rownames(.) == "preLL") # drop preLL

    PA_rand_sd <- PA_r_sd %>%
      select(contains(temp2[i])) %>%
      filter(rownames(.) == "preLL")

    PA_rand_mean2 <- PA_r2_mean %>%
      select(contains(temp2[i])) %>%
      filter(rownames(.) == "preLL")

    PA_rand_sd2 <- PA_r2_sd %>%
      select(contains(temp2[i])) %>%
      filter(rownames(.) == "preLL")

    data_sub <- data.frame(rand.r = PA_rand_mean[ , j],
          rand.sd = PA_rand_sd[ , j],
          r2.SES = (PA_data$r_value[3] - PA_rand_mean[ , j])
              / PA_rand_sd[ , j])
    data_c <- c(data_c, data_sub) %>% as.numeric
  }
    res_LL_m <- rbind(res_LL_m, data_c)
}

rownames(res_LL_m) <- NULL
# colnames(res_LL_m) <- NULL

res_LL <- cbind(temp, as.data.frame(res_LL_m) %>% round(3))

colnames(res_LL) <- c("model", "r_obs", "r_ra", "sd_ra", "SES_ra", "r_rc", "sd_rc", "SES_rc")

res_LL <- res_LL %>%
  mutate(p_rc = pnorm(SES_rc, lower.tail = F)) %>%
  mutate(p_ra = pnorm(SES_ra, lower.tail = F)) %>%
  mutate(p_rc = round(p_rc, 4)) %>%
  mutate(p_ra = round(p_ra, 4))

# wirte files
write.csv(res_data, "~/Dropbox/MS/LES_MS/data/PA_tb1.csv", row.names = F)
write.csv(res_LL, "~/Dropbox/MS/LES_MS/data/PA_tb_LL.csv", row.names = F)

after <- proc.time()
after - before


# SES new ------------------------------------------
#new SES
file_list_obs <- paste("PA_model", 1:3, "_2016-07-19_.RData", sep = "")

file_list_obs <- rep(file_list_obs, each = 2) # 6

# model1-3
# rand ra or rc
cor_func <- function(LMAps = c("LMAp", "LMAs"), trait, rand = c("ra", "rc")){
  cor_A <- numeric(1500)
  cor_A_n <- matrix(0, 1500, 10)
  par_names <- rownames(fit.summary)
  if (rand == "ra") LMA_rand <- LMA.ra.data else LMA_rand <- LMA.rc.data
  rand_p <- list()
  for (i in 1:10) rand_p[[i]] <- extract(res.list[[i]],
              pars = par_names[grepl("p\\[", par_names)])
  for (i in 1:1500){
    LMAp_temp <- PA2$LMA  * sapply(obs_p, "[", i)
    LMAs_temp <- PA2$LMA - LMAp_temp

    if(LMAps == "LMAp") LMA_temp <- log(LMAp_temp) else LMA_temp <- log(LMAs_temp)

    cor_A[i] <- cor.test(LMA_temp, log(PA2[, trait]))$estimate

    for (j in 1:10){
      LMAp_temp_n <- LMA_rand[, j] * sapply(rand_p[[j]], "[", i)
      LMAs_temp_n <- LMA_rand[, j] - LMAp_temp_n
      if(LMAps == "LMAp") LMA_temp_n <- log(LMAp_temp_n) else LMA_temp_n <- log(LMAs_temp_n)
      cor_A_n[i, j] <- cor.test(LMA_temp_n, log(PA2[, trait]))$estimate
    }
  }

  rand_sd <- apply(cor_A_n, 2, sd) %>% mean
  rand_mean <- apply(cor_A_n, 2, mean) %>% mean
  SES <- (mean(cor_A) - rand_mean) / rand_sd
  c(obs = mean(cor_A), rand_mean = rand_mean, rand_sd = rand_sd, SES = SES)
}
#
# moge1 <- cor_func(LMAps = "LMAs", trait = "LL", rand = "rc")
#
#
#
#
#
# # test
# load(file_list_obs[6])
# load(file.list[6])
#
# par_names <- rownames(fit.summary)
# obs_p <- extract(res, pars = par_names[grepl("p\\[", par_names)])
# moge <- cor_func(LMAps = "LMAs", trait = "LL", rand = "rc")
#
# cor_A <- NULL
# cor_A_n <- NULL
#
# LMA_rand <- LMA.rc.data
#
# temp <- data_frame(cor_a = as.numeric(moge),
#       gr = rep(paste("rand", 1:10), each = 1500, sep = "_"))
# temp2 <- data_frame(cor_a = moge1, gr = rep("obs", 1500))
#
# temp3 <- rbind(temp, temp2)
#
# p <- ggplot(temp3, aes(x = cor_a, fill = gr, colour = gr))
# p + geom_density(alpha = 0.3)



before <- proc.time()
res_SES <- list()

for (k in 1:6){
  load(file.list[k]) # res, res.list
  load(file_list_obs[k]) # res: overwrite
  par_names <- rownames(fit.summary)
  obs_p <- extract(res, pars = par_names[grepl("p\\[", par_names)])

  if (k %/% 2 == 1) rand_temp <- "ra" else rand_temp <- "rc"
  res_SES[[k]] <- rbind(
    cor_func(LMAps = "LMAp", trait = "Aarea", rand = rand_temp),
    cor_func(LMAps = "LMAs", trait = "Aarea", rand = rand_temp),
    cor_func(LMAps = "LMAp", trait = "Rarea", rand = rand_temp),
    cor_func(LMAps = "LMAs", trait = "Rarea", rand = rand_temp),
    cor_func(LMAps = "LMAp", trait = "LL", rand = rand_temp),
    cor_func(LMAps = "LMAs", trait = "LL", rand = rand_temp),
    cor_func(LMAps = "LMAp", trait = "Narea", rand = rand_temp),
    cor_func(LMAps = "LMAs", trait = "Narea", rand = rand_temp),
    cor_func(LMAps = "LMAp", trait = "Parea", rand = rand_temp),
    cor_func(LMAps = "LMAs", trait = "Parea", rand = rand_temp),
    cor_func(LMAps = "LMAp", trait = "cell_area", rand = rand_temp),
    cor_func(LMAps = "LMAs", trait = "cell_area", rand = rand_temp))
    res_SES[[k]] <- as.data.frame(res_SES[[k]]) %>%
      mutate(p = dnorm(SES))

    moge_row <- expand.grid(c("LMAp", "LMAs"), c("A", "R", "LL", "N", "P", "CL"))
    rownames(res_SES[[k]]) <-  paste(moge_row[,1], moge_row[,2], sep = "_")

}

after <- proc.time()
after - before

names(res_SES) <- file.list

for (i in 1:6) write.csv(res_SES[[i]], paste("~/Dropbox/MS/LES_MS/data/SES_", file.list[i], ".csv", sep = ""))
