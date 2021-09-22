# y: mass vs area (b: in PNAS)
# x: var(LMAp) / var(LMAs)
#
# var(LMA) = var(LMAs) + var(LMAp) + 2Cov()
# LMA: approx. fixed:
#
# Vp, Vs: low, med, high
# Cov: negative, zero, positive
#
# basically 6 patterns + 3 (Vps, cov<0 , cov =0, cov >0)
#
# Vtotal = Vp_hi + Vs_med , cov = 0...
library(MASS)
library(tidyverse)
# library(cowplot)
library(latex2exp)

set.seed(5)

rho0 <- c(0, 0.1, 0.2, 0.3, 0.4)
rho <- c(-rho0, rho0) %>% unique %>% sort
rho <- 0

# need to change sigma values
sigma1 <- c(0.5, 1, 1.5, 2)
sigma2 <- sigma1
#sigma1 <- seq(0.1, 1, length = 10)
#sigma2 <- seq(0.1, 1, length = 10)
sigma3 <- 0.3
alpha <- seq(0.91, 1, length = 10)
para <- expand.grid(rho = rho,
                    sigma1 = sigma1,
                    sigma2 = rev(sigma2),
                    alpha = alpha)


# mu1: LMAs, mu2: LMAp

var_func <- function(mu1, mu2){
  b <- NULL
  v_vec2 <- NULL
  for (i in 1:nrow(para)){
    rho <- para$rho[i]
    sigma <- c(LMAs = para$sigma1[i], LMAp = para$sigma2[i])
    mu <- c(LAMs = mu1 - 0.5 * sigma[1]^2, LMAp = mu2 - 0.5 * sigma[2]^2)
    Sigma <- matrix(c(sigma[1]^2, sigma[1]*sigma[2]*rho,
                      sigma[1]*sigma[2]*rho,sigma[2]^2), 2, 2)

    LMA_dat <- mvrnorm(n = 100, mu, Sigma) %>% exp

    # LMA_dat <- rlnorm.rplus(n = 100, mu, Sigma)
    LMAs <- LMA_dat[,1]
    LMAp <- LMA_dat[,2]
    # Aarea <- 0.23 * LMAp
    log_Aarea <- rnorm(100,
                       #log(1.44) + 0.281 * log(LMAp) - 0.5 * sigma^2,
                       log(1.44) + alpha * log(LMAp) - 0.5 * sigma^2,
                       sigma3)
    # log_Aarea <- rnorm(100, 0.83 + 0.37 * log(LMAp), sigma3)
    Aarea <- exp(log_Aarea)
    LMA <- LMAp + LMAs
    Amass <- Aarea / LMA

    v_vec <- c(Vtotal = var(LMA) , Vs = var(LMAs) , Vp = var(LMAp) ,
               Cov = cov(LMAp, LMAs), Covs = cov(LMA, LMAs),
               Covp = cov(LMA, LMAp))

    # print(sd(log(LMAs)))
    #Osnas et al.
    res <- lm(log(Aarea) ~ log(LMA))
    b[i] <- res$coefficients[[2]]
    v_vec2 <- rbind(v_vec2, v_vec)
    }
    rownames(v_vec2) <- NULL

    para2 <- data.frame(para, v_vec2, b) %>%
      mutate(ratio = Vs/Vp) %>%
      mutate(LMAs = mu1) %>%
      mutate(LMAp = mu2)
    para2
}
# var_func(mu1 = log(98), mu2 = log(70))

library(snowfall)
sfInit(parallel = T, cpu = 4)
sfLibrary(tidyverse)
sfLibrary(MASS)
sfExportAll()


system.time(GL_sim <- sfLapply(1:100, 
                               function(x)var_func(mu1 = log(94),
                                                   mu2 = log(73))))

GL_sim2 <- NULL

for (i in 1:100){
  GL_sim2 <- rbind(GL_sim2, GL_sim[[i]])
}

GL_sim2 <- GL_sim2 %>%
  mutate(ratio2 = Covs / Vtotal * 100) %>%
  as_data_frame

save.image("./data/GL_sim_var.rda")
