rm(list = ls()) # This clears everything from memory.
library(rstan)
library(dplyr)
load("~/Dropbox/LES/GL_sim.RData")
library(loo)

P_vec <- paste("p[", 1:n_sample, "]" ,sep = "")
LL_vec <- paste("mu[", 1:n_sample, ",2]" ,sep = "")


p_est <- extract(res1, pars = P_vec)
obs_cor_A <- NULL
obs_cor_LL <- NULL
obs_cor_R <- NULL

true_cor_A <- cor(log(LMAp), log(A))
true_cor_LL <- cor(log(LMAs), log(LL))
true_cor_R <- cor(log(LMAp), log(R))

for (i in 1:1000){
  temp <- sapply(p_est, function(x)sample(x,1)) %>% as.numeric
  LMAp_temp <- temp * LMA
  LMAs_temp <- LMA - LMAp_temp
  obs_cor_A[i] <- cor(log(LMAp_temp), log(A))
  obs_cor_LL[i] <- cor(log(LMAs_temp), log(LL))
  obs_cor_R[i] <- cor(log(LMAp_temp), log(R))
}


est_cor_A <- mean(obs_cor_A)
est_cor_LL <- mean(obs_cor_LL)
est_cor_R <- mean(obs_cor_R)

#null GL ================================================================
load("~/Dropbox/LES/GL_sim_null.RData")

cor_A_m <- NULL
cor_LL_m <- NULL
cor_R_m <- NULL

before <- proc.time()

p_est <- list()
for (i in 1:10){
  cor_A <- NULL
  cor_LL <- NULL
  cor_R <- NULL
  p_est <- extract(res1[[i]], pars = P_vec)
  for (j in 1:1000){
    temp <- sapply(p_est, function(x)sample(x,1)) %>% as.numeric
    LMAp_temp <- temp * LMA_null[[i]]
    LMAs_temp <- LMA_null[[i]] - LMAp_temp
    cor_A[j] <- cor(log(LMAp_temp), log(A))
    cor_LL[j] <- cor(log(LMAs_temp), log(LL))
    cor_R[j] <- cor(log(LMAp_temp), log(R))
  }
  cor_A_m <- cbind(cor_A_m, cor_A)
  cor_LL_m <- cbind(cor_LL_m, cor_LL)
  cor_R_m <- cbind(cor_R_m, cor_R)
}

after <- proc.time()
after - before

null_cor_A <- apply(cor_A_m, 1, mean) %>% mean
null_cor_LL <- apply(cor_LL_m, 1, mean) %>% mean
null_cor_R <- apply(cor_R_m, 1, mean) %>% mean

GL <- data_frame(cor_A = c(obs_cor_A, as.vector(cor_A_m)),
        cor_LL = c(obs_cor_LL, as.vector(cor_LL_m)),
        cor_R = c(obs_cor_R, as.vector(cor_R_m)),
        model = rep(c("Est", paste("Rand", 1:10)), each = 1000),
        model2 = c(rep("Est", 1000), rep("Rand", 10000))) %>%
      mutate(dat = "Simulated GL1")


#null obs-------------------
load("~/Dropbox/LES/null_sim.RData")
library(loo)

p_est <- extract(res1, pars = P_vec)
obs_cor_A <- NULL
obs_cor_LL <- NULL
obs_cor_R <- NULL

true_cor_A2 <- cor(log(LMAp), log(A))
true_cor_LL2 <- cor(log(LMAs), log(LL))
true_cor_R2 <- cor(log(LMAp), log(R))

for (i in 1:1000){
  temp <- sapply(p_est, function(x)sample(x,1)) %>% as.numeric
  LMAp_temp <- temp * LMA
  LMAs_temp <- LMA - LMAp_temp
  obs_cor_A[i] <- cor(log(LMAp_temp), log(A))
  obs_cor_R[i] <- cor(log(LMAp_temp), log(R))
  obs_cor_LL[i] <- cor(log(LMAs_temp), log(LL))
}

est_cor_A2 <- mean(obs_cor_A)
est_cor_LL2 <- mean(obs_cor_LL)
est_cor_R2 <- mean(obs_cor_R)

# nul null ============================================================
load("~/Dropbox/LES/null_sim_null.RData")
cor_A_m <- NULL
cor_R_m <- NULL
cor_LL_m <- NULL
before <- proc.time()

p_est <- list()
for (i in 1:10){
  cor_A <- NULL
  cor_LL <- NULL
  cor_R <- NULL
  p_est <- extract(res1[[i]], pars = P_vec)
  for (j in 1:1000){
    temp <- sapply(p_est, function(x)sample(x,1)) %>% as.numeric
    LMAp_temp <- temp * LMA
    LMAs_temp <- LMA - LMAp_temp
    cor_A[j] <- cor(log(LMAp_temp), log(A))
    cor_LL[j] <- cor(log(LMAs_temp), log(LL))
    cor_R[j] <- cor(log(LMAp_temp), log(R))
  }
  cor_A_m <- cbind(cor_A_m, cor_A)
  cor_LL_m <- cbind(cor_LL_m, cor_LL)
  cor_R_m <- cbind(cor_R_m, cor_R)
}

null_cor_A2 <- apply(cor_A_m, 1, mean) %>% mean
null_cor_LL2 <- apply(cor_LL_m, 1, mean) %>% mean
null_cor_R2 <- apply(cor_R_m, 1, mean) %>% mean

after <- proc.time()
after - before

weak_dat <- data_frame(cor_A = c(obs_cor_A, as.vector(cor_A_m)),
        cor_LL = c(obs_cor_LL, as.vector(cor_LL_m)),
        cor_R = c(obs_cor_R, as.vector(cor_R_m)),
        model = rep(c("Est", paste("Rand", 1:10)), each = 1000),
        model2 = c(rep("Est", 1000), rep("Rand", 10000))) %>%
      mutate(dat = "Simulated WC")


# PA sim ---------------------------------------------------------------
load("~/Dropbox/LES/PA_sim.RData")
obs_cor_A <- NULL
obs_cor_LL <- NULL
obs_cor_R <- NULL

true_cor_A_PA <- cor(log(LMAp), log(A))
# true_cor_LL_PA <- cor(log_mu2, log(LL))

true_cor_LL_PA <- cor(log(LMAs), log(LL))

true_cor_R_PA <- cor(log(LMAp), log(R))

p_est <- extract(res3, pars = P_vec)
LL_est <- extract(res3, pars = LL_vec)

for (i in 1:1000){
  temp <- sapply(p_est, function(x)sample(x,1)) %>% as.numeric
  # temp2 <- sapply(LL_est, function(x)sample(x,1)) %>% as.numeric
  LMAp_temp <- temp * LMA
  LMAs_temp <- LMA - LMAp_temp
  obs_cor_A[i] <- cor(log(LMAp_temp), log(A))
  obs_cor_LL[i] <- cor(log(LMAs_temp), log(LL))
  obs_cor_R[i] <- cor(log(LMAp_temp), log(R))
}



est_cor_A_PA <- mean(obs_cor_A)
est_cor_LL_PA <- mean(obs_cor_LL)
# est_cor_LL_PA <- cor(m3[LL_vec, "mean"], log(LL))

est_cor_R_PA <- mean(obs_cor_R)

#null PA ================================================================
load("~/Dropbox/LES/PA_sim_null.RData")

cor_A_m <- NULL
cor_LL_m <- NULL
cor_R_m <- NULL

before <- proc.time()
p_est <- list()
LL_est <- list()
for (i in 1:10){
  cor_A <- NULL
  cor_LL <- NULL
  cor_R <- NULL
  p_est <- extract(res3[[i]], pars = P_vec)
  LL_est <- extract(res3[[i]], pars = LL_vec)
  for (j in 1:1000){
    temp <- sapply(p_est, function(x)sample(x,1)) %>% as.numeric
    # temp2 <- sapply(LL_est, function(x)sample(x,1)) %>% as.numeric
    LMAp_temp <- temp * LMA.rc.data[,i]
    LMAs_temp <- LMA.rc.data[, i] - LMAp_temp
    cor_A[j] <- cor(log(LMAp_temp), log(A))
    cor_LL[j] <- cor(log(LMAs_temp), log(LL))
    cor_R[j] <- cor(log(LMAp_temp), log(R))
  }
  cor_A_m <- cbind(cor_A_m, cor_A)
  cor_LL_m <- cbind(cor_LL_m, cor_LL)
  cor_R_m <- cbind(cor_R_m, cor_R)
}


plot(m3[[1]][P_vec, "mean"] * LMA.rc.data[,10], log(A), log = "xy")
plot(m3[[1]][LL_vec, "mean"], log(LL))

after <- proc.time()
after - before

null_cor_A_PA <- apply(cor_A_m, 1, mean) %>% mean
null_cor_LL_PA <- apply(cor_LL_m, 1, mean) %>% mean
null_cor_R_PA <- apply(cor_R_m, 1, mean) %>% mean

PA <- data_frame(cor_A = c(obs_cor_A, as.vector(cor_A_m)),
        cor_LL = c(obs_cor_LL, as.vector(cor_LL_m)),
        cor_R = c(obs_cor_R, as.vector(cor_R_m)),
        model = rep(c("Est", paste("Rand", 1:10)), each = 1000),
        model2 = c(rep("Est", 1000), rep("Rand", 10000))) %>%
      mutate(dat = "Simulated PA")

fig_dat <- bind_rows(GL, PA, weak_dat)

############

# vline_dat <- data_frame(dat = c("Simulated GL1", "Simulated WC"),
#   true_cor = c(true_cor_R, true_cor_R_PA, _R2),
#   est_cor = c(est_cor_R, est_cor_R2),
#   null_cor = c(null_cor_R, null_cor_R2))
#
#
#   vline_dat2 <- data_frame(dat = c("Simulated GL1", "Simulated PA", "Simulated WC"),
#     true_cor = c(true_cor_R, true_cor_R2),
#     est_cor = c(est_cor_R, est_cor_R2),
#     null_cor = c(null_cor_R, null_cor_R2))
#
# # text_dat <- data_frame(dat = c("Simulated GL1", "Simulated WC"),
# #   y = c(0.2, 0.2), x =  c(true_cor_A, true_cor_A2),
# #   label = c("hoge", "boke"))
# #

# pdf("~/Dropbox/Download/cor_A_sim.PDF", width=5.2, height=3.45)
p <- ggplot(fig_dat, aes(x = cor_R, fill = model, colour = model )) + facet_grid(~dat)

p <- p + geom_density(aes(y=0.1*..density..), alpha = 0.3) + theme_bw()
p <- p + geom_vline(data = vline_dat, aes(xintercept = true_cor),
  color = "red", lty =2)
p <- p + geom_vline(data = vline_dat, aes(xintercept = est_cor))
p <- p + geom_vline(data = vline_dat, aes(xintercept = null_cor),
  lty = 2, color = "black")
# p <- p + geom_text(data = text_dat, aes(x, y, label = label))
p + xlab("Correlation coefficeint") + ylab("Density")
# dev.off()


add_fac <- function(vec){
  vec2 <- as.numeric(vec)
  ifelse(vec2==1,"LMAp vs Aarea", ifelse(vec2==2, "LMAs vs LL", "LMAp vs Rarea")) %>% as.factor
}

fig_dat2 <- fig_dat %>%
  tidyr::gather("group", "val", 1:3) %>%
  mutate(group = as.factor(group)) %>%
  filter(model != "Rand 10") %>%
  mutate(group2 = add_fac(group))



vline_dat2 <- data_frame(dat = rep(c("Simulated GL1",
                  "Simulated PA",
                  "Simulated WC"), each = 3),
            group2 = rep(c("LMAp vs Aarea", "LMAp vs Rarea", "LMAs vs LL"), 3),
            true_cor = c(true_cor_A, true_cor_R, true_cor_LL,
                  true_cor_A_PA, true_cor_R_PA, true_cor_LL_PA,
                  true_cor_A2, true_cor_R2, true_cor_LL2),
            est_cor = c(est_cor_A, est_cor_R, est_cor_LL,
                  est_cor_A_PA, est_cor_R_PA, est_cor_LL_PA,
                  est_cor_A2, est_cor_R2, est_cor_LL2),
            null_cor = c(null_cor_A, null_cor_R, null_cor_LL,
                  null_cor_A_PA, null_cor_R_PA, null_cor_LL_PA,
                  null_cor_A2, null_cor_R2, null_cor_LL2))

text_dat <- data_frame(dat = rep(c("Simulated GL1",
                  "Simulated PA",
                  "Simulated WC"), each = 3),
            group2 = rep(c("LMAp vs Aarea", "LMAp vs Rarea", "LMAs vs LL"), 3),
            x = c(true_cor_A, true_cor_R, true_cor_LL,
                  true_cor_A_PA, true_cor_R_PA, true_cor_LL_PA,
                  true_cor_A2, true_cor_R2, true_cor_LL2),
            y = rep(c(4, 1.5, 1), 3),
            lab = rep("True", 9),
            model = NA)

text_dat2 <- data_frame(dat = rep(c("Simulated GL1",
                  "Simulated PA",
                  "Simulated WC"), each = 3),
            group2 = rep(c("LMAp vs Aarea", "LMAp vs Rarea", "LMAs vs LL"), 3),
            x = c(est_cor_A, est_cor_R, est_cor_LL,
                  est_cor_A_PA, est_cor_R_PA, est_cor_LL_PA,
                  est_cor_A2, est_cor_R2, est_cor_LL2),
            y = rep(c(6, 2.5, 1.5), 3),
            lab = rep("Est", 9),
            model = NA)


text_dat3 <- data_frame(dat = rep(c("Simulated GL1",
                  "Simulated PA",
                  "Simulated WC"), each = 3),
            group2 = rep(c("LMAp vs Aarea", "LMAp vs Rarea", "LMAs vs LL"), 3),
            x = c(null_cor_A, null_cor_R, null_cor_LL,
                  null_cor_A_PA, null_cor_R_PA, null_cor_LL_PA,
                  null_cor_A2, null_cor_R2, null_cor_LL2),
            y = rep(c(4, 1.5, 1), 3),
            lab = rep("Rand", 9),
            model = NA)


# SES and p-values
temp <- fig_dat2 %>%
  group_by(model, group, dat) %>%
  summarize(moge = mean(val)) %>%
  mutate(model2 = sapply(strsplit(as.character(model), " "), "[",1)) %>%
  mutate(dat = as.factor(dat))

ses_temp <- matrix(0, 3, 3)
ses_temp2 <- matrix(0, 3, 3)

for (i in 1:3){
  for (j in 1:3){
    obs <- temp %>%
      filter(model == "Est") %>%
      filter(group == levels(temp$group)[i]) %>%
      filter(dat == levels(temp$dat)[j])

    rand <- temp %>%
      filter(model2 == "Rand") %>%
      filter(group == levels(temp$group)[i]) %>%
      filter(dat == levels(temp$dat)[j])

    ses_temp[i, j] <- (obs$moge^2 - mean(rand$moge^2)) / sd(rand$moge^2)
    ses_temp2[i, j] <- paste(levels(temp$group)[i], levels(temp$dat)[j])
  }
}

# p-values

# no longer used
# dnorm(ses_temp)

ses_temp %>% round(., 4)

# > ses_temp %>% round(., 4)
#          GL1  PA     WC
#        1,1]    [,2]    [,3]
# [1,] 2.9534  3.8334  0.5108 LMAp - Aarea
# [3,] 3.3662  5.0124 -0.1452 LMAp - Rarea
# [2,] 4.2117 -2.9146  0.4070 LMAs - LL

1 - pnorm(ses_temp) %>% round(., 4)

# > 1 - pnorm(ses_temp) %>% round(., 4)
#        [,1]   [,2]   [,3]
# [1,] 0.0016 0.0001 0.3047
# [3,] 0.0004 0.0000 0.5577
# [2,] 0.0000 0.9982 0.3420
#


pdf("~/Dropbox/MS/LES_MS/fig/sim_3.pdf", width = 8.66, height = 5.76)
p <- ggplot(fig_dat2, aes(x = val, fill = model, colour = model )) + facet_grid(group2 ~ dat, scales = "free")

p <- p + geom_density(aes(y=0.09*..density..), alpha = 0.3) + theme_bw()
p <- p + geom_vline(data = vline_dat2, aes(xintercept = true_cor),
  color = "red", lty =1, alpha = 0.8)
p <- p + geom_vline(data = vline_dat2, aes(xintercept = est_cor), color = "blue", lty = 2, alpha = 0.8)
p <- p + geom_vline(data = vline_dat2, aes(xintercept = null_cor),
  lty = 2, color = "black", alpha = 0.8)
# p <- p +  guides(fill=F, colour=guide_legend(title=NULL))
p <- p +  guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL))

p <- p + geom_text(data = text_dat, aes(x = x, y = y, label = lab, angle = 90, vjust = -0.5, hjust = 0), colour  = "red")
p <- p + geom_text(data = text_dat2, aes(x = x, y = y, label = lab, angle = 90, vjust = -0.5, hjust = 0), colour = "blue")
p <- p + geom_text(data = text_dat3, aes(x = x, y = y, label = lab, angle = 90, vjust = -0.5, hjust = 0),  colour = "black")
p + xlab("Correlation coefficeint") + ylab("Density")
dev.off()


#------------
