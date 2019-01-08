
# R value check
rm(list = ls()) # This clears everything from memory.
setwd("~/Dropbox/MS/LES_MS/LMApsModel/data/")

load("GL_model1_obs_2017-06-19_.RData")

GL <- data.obs
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) %>%
  mutate(LMAp1 =  fit.summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAs1 = LMA - LMAp1)

LMA_dat <- GL %>%
  arrange(sp) %>%
  select(LMA, LMAp1, LMAs1)

trait_name <- c("Aarea", "Rarea", "LL")
trait_name2 <- c("LMA", "LMAp", "LMAs")

temp_cor_data <- NULL

    for (l in 1:3){
      for (m in 1:3){
        temp_cor <- cor.test(log10(GL[ , trait_name[l]]),
                log10(LMA_dat[, m]))$estimate
        temp_cor_data <- c(temp_cor_data, temp_cor)
      }
    }

moge2 <- NULL

for (l in 1:3) {
  if (trait_name[l] == "preLL") {
    moge2 <- c(moge2, "preLL")
} else { for (m in 1:3) moge2 <- c(moge2, paste(trait_name[l], trait_name2[m], sep = "_")) }
}

names(temp_cor_data) <- moge2

GL1_obs_r <- temp_cor_data
GL1_obs_r2 <- GL1_obs_r^2

load("GL_model1_LMA_2017-06-19_.RData")

GL <- GL %>% arrange(sp)


moge <- list() # each data.frame comes from each model

temp_data <- NULL
for (k in 1:10){
    temp_LMA_null <- LMA.ra.data[, k]
    temp_LMAp <- fit.summary.list[[k]][P_vec, "mean"] * temp_LMA_null
    temp_LMAs <- temp_LMA_null - temp_LMAp
    LMA_dat <- data.frame(temp_LMA_null, temp_LMAp, temp_LMAs)
    temp_cor_data <- NULL
    for (l in 1:3){
      for (m in 1:3){
        temp_cor <- cor.test(log10(GL[ , trait_name[l]]),
                log10(LMA_dat[, m]))$estimate
        temp_cor_data <- c(temp_cor_data, temp_cor)
      }
    }
    temp_data <- cbind(temp_data, temp_cor_data)
    rownames(temp_data) <- moge2
}

GL1_ra_r2 <- apply(temp_data, 2, function(x) x^2 )
GL1_ra_r_max <- apply(temp_data, 1, max)
GL1_ra_r_min <- apply(temp_data, 1, min)

GL1_ra_r_mean <- apply(temp_data, 1, mean)
GL1_ra_r_sd <- apply(temp_data, 1, sd)
GL1_ra_r2_mean <- apply(GL1_ra_r2, 1, mean)
GL1_ra_r2_sd <- apply(GL1_ra_r2, 1, sd)

GL1_ra_SES <- (GL1_obs_r - GL1_ra_r_mean) / GL1_ra_r_sd
GL1_ra_SES

GL1_ra_SES2 <- (GL1_obs_r2 - GL1_ra_r2_mean) / GL1_ra_r2_sd
GL1_ra_SES2

load("GL_model1_All_2017-06-19_.RData")

moge <- list() # each data.frame comes from each model

temp_data <- NULL
for (k in 1:10){
    temp <- LMA.ra2.data[[k]]
    s1ALL <- fit.summary.list[[k]]

    GL <- GL %>%
      mutate(LL = temp[, "LL"]) %>%
      mutate(Rarea = temp[, "Rarea"]) %>%
      mutate(Aarea = temp[, "Aarea"])
    temp_LMA_null <- temp$LMA
    temp_LMAp <- fit.summary.list[[k]][P_vec, "mean"] * temp_LMA_null
    temp_LMAs <- temp_LMA_null - temp_LMAp
    LMA_dat <- data.frame(temp_LMA_null, temp_LMAp, temp_LMAs)
    temp_cor_data <- NULL
    for (l in 1:3){
      for (m in 1:3){
        temp_cor <- cor.test(log10(GL[ , trait_name[l]]),
                log10(LMA_dat[, m]))$estimate
        temp_cor_data <- c(temp_cor_data, temp_cor)
      }
    }
    temp_data <- cbind(temp_data, temp_cor_data)
    rownames(temp_data) <- moge2
}

GL1_all_r_max <- apply(temp_data, 1, max)
GL1_all_r_min <- apply(temp_data, 1, min)
GL1_all_r2 <- apply(temp_data, 2, function(x) x^2 )

GL1_all_r_mean <- apply(temp_data, 1, mean)
GL1_all_r_sd <- apply(temp_data, 1, sd)
GL1_all_r2_mean <- apply(GL1_all_r2, 1, mean)
GL1_all_r2_sd <- apply(GL1_all_r2, 1, sd)

GL1_all_SES <- (GL1_obs_r - GL1_all_r_mean) / GL1_all_r_sd
GL1_all_SES

GL1_all_SES2 <- (GL1_obs_r2 - GL1_all_r2_mean) / GL1_all_r2_sd
GL1_all_SES2

#power-law
setwd("~/Dropbox/MS/LES_MS/LMApsModel/data/")

load("GL_model1r4_obs_2017-06-19_.RData")

GL <- data.obs
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) %>%
  mutate(LMAp4 =  fit.summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAs4 = LMA - LMAp4)

LMA_dat <- GL %>%
  arrange(sp) %>%
  select(LMA, LMAp4, LMAs4)

trait_name <- c("Aarea", "Rarea", "LL")
trait_name2 <- c("LMA", "LMAp", "LMAs")

temp_cor_data <- NULL

    for (l in 1:3){
      for (m in 1:3){
        temp_cor <- cor.test(log10(GL[ , trait_name[l]]),
                log10(LMA_dat[, m]))$estimate
        temp_cor_data <- c(temp_cor_data, temp_cor)
      }
    }

moge2 <- NULL

for (l in 1:3) {
  if (trait_name[l] == "preLL") {
    moge2 <- c(moge2, "preLL")
} else { for (m in 1:3) moge2 <- c(moge2, paste(trait_name[l], trait_name2[m], sep = "_")) }
}

names(temp_cor_data) <- moge2

GL1r4_obs_r <- temp_cor_data
GL1r4_obs_r2 <- GL1r4_obs_r^2

load("GL_model1r4_LMA_2017-06-19_.RData")
moge <- list() # each data.frame comes from each model

temp_data <- NULL
for (k in 1:10){
    temp_LMA_null <- LMA.ra.data[, k]
    temp_LMAp <- fit.summary.list[[k]][P_vec, "mean"] * temp_LMA_null
    temp_LMAs <- temp_LMA_null - temp_LMAp
    LMA_dat <- data.frame(temp_LMA_null, temp_LMAp, temp_LMAs)
    temp_cor_data <- NULL
    for (l in 1:3){
      for (m in 1:3){
        temp_cor <- cor.test(log10(GL[ , trait_name[l]]),
                log10(LMA_dat[, m]))$estimate
        temp_cor_data <- c(temp_cor_data, temp_cor)
      }
    }
    temp_data <- cbind(temp_data, temp_cor_data)
    rownames(temp_data) <- moge2
}

GL1r4_ra <- temp_data 

GL1r4_ra_r_max <- apply(temp_data, 1, max)
GL1r4_ra_r_min <- apply(temp_data, 1, min)


GL1r4_ra_r2 <- apply(temp_data, 2, function(x) x^2 )

GL1r4_ra_r_mean <- apply(temp_data, 1, mean)
GL1r4_ra_r_sd <- apply(temp_data, 1, sd)
GL1r4_ra_r2_mean <- apply(GL1r4_ra_r2, 1, mean)
GL1r4_ra_r2_sd <- apply(GL1r4_ra_r2, 1, sd)

GL1r4_ra_SES <- (GL1r4_obs_r - GL1r4_ra_r_mean) / GL1r4_ra_r_sd
GL1r4_ra_SES

GL1r4_ra_SES2 <- (GL1r4_obs_r2 - GL1r4_ra_r2_mean) / GL1r4_ra_r2_sd
GL1r4_ra_SES2

load("GL_model1r4_All_2017-06-19_.RData")

moge <- list() # each data.frame comes from each model

temp_data <- NULL
for (k in 1:10){
    temp <- LMA.ra2.data[[k]]
    s1ALL <- fit.summary.list[[k]]

    GL <- GL %>%
      mutate(LL = temp[, "LL"]) %>%
      mutate(Rarea = temp[, "Rarea"]) %>%
      mutate(Aarea = temp[, "Aarea"])

    temp_LMA_null <- temp$LMA
    temp_LMAp <- fit.summary.list[[k]][P_vec, "mean"] * temp_LMA_null
    temp_LMAs <- temp_LMA_null - temp_LMAp
    LMA_dat <- data.frame(temp_LMA_null, temp_LMAp, temp_LMAs)
    temp_cor_data <- NULL
    for (l in 1:3){
      for (m in 1:3){
        temp_cor <- cor.test(log10(GL[ , trait_name[l]]),
                log10(LMA_dat[, m]))$estimate
        temp_cor_data <- c(temp_cor_data, temp_cor)
      }
    }
    temp_data <- cbind(temp_data, temp_cor_data)
    rownames(temp_data) <- moge2
}

GL1r4_all_r_max <- apply(temp_data, 1, max)
GL1r4_all_r_min <- apply(temp_data, 1, min)

GL1r4_all_r2 <- apply(temp_data, 2, function(x) x^2 )

GL1r4_all_r_mean <- apply(temp_data, 1, mean)
GL1r4_all_r_sd <- apply(temp_data, 1, sd)
GL1r4_all_r2_mean <- apply(GL1r4_all_r2, 1, mean)
GL1r4_all_r2_sd <- apply(GL1r4_all_r2, 1, sd)

GL1r4_all_SES <- (GL1r4_obs_r - GL1r4_all_r_mean) / GL1r4_all_r_sd
GL1r4_all_SES

GL1r4_all_SES2 <- (GL1r4_obs_r2 - GL1r4_all_r2_mean) / GL1r4_all_r2_sd
GL1r4_all_SES2
