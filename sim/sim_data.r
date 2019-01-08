set.seed(10)

library(tidyverse)
library(mvtnorm)

para <- yaml::yaml.load_file("parameter.yml")
n_sample <- 200

# GL sim data -----------------------------------------------------------------
GL <- read_csv("./data/GL20170911.csv") 

GL_para <- GL %>%
  mutate(ratio = LMAp4/LMA) %>%
  group_by(DE) %>%
  dplyr::summarize(mu_LMA = mean(log(LMA)),
            mid_LMA = median(log(LMA)),
            sigma_LMA = sd(log(LMA)),
            mu_f = mean(ratio),
            var_f = var(ratio),
            n = n()) %>%
  ungroup %>%
 # mutate(mu = mu_LMA - 0.5 * sigma_LMA^2) %>%
 # mutate(mu2 = mu_LMA2 - 0.5 * sigma_LMA^2) %>%
  mutate(n = as.integer(n / sum(n) * n_sample)) 

# 100 sample
#GL_para[1, "n"] <- 8
GL_para[1, "n"] <- 16

LMA <- mapply(rlnorm, 
              GL_para$n, 
             # GL_para$mu_LMA - 0.5 * GL_para$sigma_LMA^2, 
              GL_para$mid_LMA,
              GL_para$sigma_LMA) %>% 
  unlist

beta_est <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = c(alpha = alpha, beta = beta))
}

f_dat <- mapply(beta_est,
       GL_para$mu_f,
       GL_para$var_f) %>% 
  t %>%
  as_data_frame %>%
  mutate(n = GL_para$n)

f_val <- mapply(rbeta, 
       f_dat$n, 
       f_dat$alpha, 
       f_dat$beta) %>%
  unlist

# without power law
#LMA2 <- LMA[9:75]
#LMA2 <- LMA[17:150]
#phi <- 0.12 + log(LMA2) * 0.04 # alpha / (alpha + beta)
#lambda <- 10 # alpha + beta
#
#f2 <- NULL
#for (i in 1:length(LMA2)) f2[i] <- rbeta(1, phi[i] * lambda, lambda*(1-phi[i]))
#
#f_val[17:150] <- f2

LMAp <- LMA * f_val
LMAs <- LMA - LMAp

sigma1 <- para$GL$sigma1
sigma2 <- para$GL$sigma2
sigma3 <- para$GL$sigma3
rho12 <- para$GL$rho12
rho23 <- para$GL$rho23
rho13 <- para$GL$rho13
log_alpha <- para$GL$log_alpha
log_beta <- para$GL$log_beta
log_r <- para$GL$log_r
alpha2 <- para$GL$alpha2
beta2 <- para$GL$beta2
rp <- para$GL$rp
rs <- para$GL$rs

Sigma <- matrix(c(sigma1^2, rho12*sigma1*sigma2, rho13*sigma1*sigma3,
    rho12*sigma1*sigma2, sigma2^2, rho23*sigma2*sigma3,
    rho13*sigma1*sigma3, rho23*sigma2*sigma3, sigma3^2), ncol =3)

# leaf data -------------------------------------------------------------
obs <- NULL
for (i in 1:n_sample){
  mu_temp <- rmvnorm(1,
    mean = c(log_alpha + alpha2 * log(LMAp[i]) - 0.5 * sigma1^2,
        log_beta + beta2 * log(LMAs[i]) - 0.5 * sigma2^2,
        log_r + rp * log(LMAp[i]) + rs * log(LMAs[i]) - 0.5 * sigma3^2),
    #mean = c(log_alpha + log(LMAp[i]) - 0.5 * sigma1^2,
    #    log_beta + log(LMAs[i]) - 0.5 * sigma2^2,
    #    log(rp * LMAp[i] + rs * LMAs[i]) - 0.5 * sigma3^2),
    sigma = Sigma
    )
  obs <- rbind(obs, mu_temp)
}

GL_sim_dat <- data_frame(LMA, f_val, LMAp, LMAs) %>%
  mutate(gr = rep(c("D", "E", "U"), GL_para$n)) %>%
  mutate(Aarea = exp(obs[,1]) - exp(obs[,3])) %>%
  mutate(LL = exp(obs[,2])) %>%
  mutate(Rarea = exp(obs[,3])) 

ggplot(GL_sim_dat, aes(f_val, fill = gr)) +
  geom_histogram() +
  facet_grid(gr~.)

# GL swaped data --------------------------------------------------------------
# evergreen have lower LMAs

GL_para <- GL_para %>%
  mutate(mu_LMA2 = mu_LMA)

GL_para[GL_para$DE == "D", "mu_LMA2"] <- GL_para[GL_para$DE == "E", "mu_LMA"]
GL_para[GL_para$DE == "E", "mu_LMA2"] <- GL_para[GL_para$DE == "D", "mu_LMA"]

# use mu_LMA2 
LMA <- mapply(rlnorm, 
              GL_para$n, 
              GL_para$mu_LMA2 - 0.5 * GL_para$sigma_LMA, 
              GL_para$sigma_LMA) %>% 
  unlist

f_dat <- mapply(beta_est,
       GL_para$mu_f,
       GL_para$var_f) %>% 
  t %>%
  as_data_frame %>%
  mutate(n = GL_para$n)

f_val <- mapply(rbeta, 
       f_dat$n, 
       f_dat$alpha, 
       f_dat$beta) %>%
  unlist

LMAp <- LMA * f_val
LMAs <- LMA - LMAp

obs <- NULL
for (i in 1:nrow(GL_sim_dat)){
  mu_temp <- rmvnorm(1,
    mean = c(log_alpha + alpha2 * log(LMAp[i]) - 0.5 * sigma1^2,
        log_beta + beta2 * log(LMAs[i]) - 0.5 * sigma2^2,
        log_r + rp * log(LMAp[i]) + rs * log(LMAs[i]) - 0.5 * sigma3^2),
    sigma = Sigma
    )
  obs <- rbind(obs, mu_temp)
}

GL_sim_dat2 <- data_frame(LMA, f_val, LMAp, LMAs) %>%
  mutate(gr = rep(c("D", "E", "U"), GL_para$n)) %>%
  #mutate(Aarea = exp(obs[,1]) - exp(obs[,3])) %>%
  mutate(Aarea = exp(obs[,1])) %>%
  mutate(LL = exp(obs[,2])) %>%
  mutate(Rarea = exp(obs[,3])) 

# PA sim data -----------------------------------------------------------------

PA <- read_csv("./data/PA20170911.csv") %>%
  filter(!is.na(cell_area)) 

PA_para <- PA %>%
  mutate(ratio = LMAp/LMA) %>%
  group_by(site,strata) %>%
  dplyr::summarize(mu_LMA = mean(log(LMA)),
            sigma_LMA = sd(log(LMA)),
            mu_f = mean(ratio),
            var_f = var(ratio),
            n = n()) %>%
  ungroup %>%
  mutate(n = as.integer(n / sum(n) * n_sample))

LMA <- mapply(rlnorm, 
              PA_para$n, 
              PA_para$mu_LMA - 0.5 * PA_para$sigma_LMA, 
              PA_para$sigma_LMA) %>% 
  unlist

f_dat <- mapply(beta_est,
       PA_para$mu_f,
       PA_para$var_f) %>% 
  t %>%
  as_data_frame %>%
  mutate(n = PA_para$n)

f_val <- mapply(rbeta, 
       f_dat$n, 
       f_dat$alpha, 
       f_dat$beta) %>%
  unlist

LMAp <- LMA * f_val
LMAs <- LMA - LMAp

sigma1 <- para$PA$sigma1
sigma2 <- para$PA$sigma2
sigma3 <- para$PA$sigma3
rho12 <- para$PA$rho12
rho23 <- para$PA$rho23
rho13 <- para$PA$rho13
log_alpha <- para$PA$log_alpha
log_beta <- para$PA$log_beta
log_r <- para$PA$log_r
alpha2 <- para$PA$alpha2
beta2 <- para$PA$beta2
rp <- para$PA$rp
rp <- para$PA$rp
q <- para$PA$q
log_site <- para$PA$log_site

# prepare A and R first  ======================================================
Sigma2 <- matrix(c(sigma1^2, rho13*sigma1*sigma3,
        rho13*sigma1*sigma3, sigma3^2), ncol = 2)

obs <- NULL
log_mu2 <- NULL
for (i in 1:n_sample){
  mu_temp <- rmvnorm(1,
    mean = c(log_alpha + alpha2 * log(LMAp[i]) - 0.5 * sigma1^2,
        log_r + rp * log(LMAp[i]) + rs * log(LMAs[i]) - 0.5 * sigma3^2),
    sigma = Sigma2
    )
  obs <- rbind(obs, mu_temp)
}

AR <- exp(obs[, 1])
R <- exp(obs[, 2])
A <- AR - R

Sigma <- matrix(c(sigma1^2, rho12*sigma1*sigma2, rho13*sigma1*sigma3,
    rho12*sigma1*sigma2, sigma2^2, rho23*sigma2*sigma3,
    rho13*sigma1*sigma3, rho23*sigma2*sigma3, sigma3^2), ncol =3)

# leaf data -------------------------------------------------------------

PA_sim_dat <- data_frame(LMA, f_val, LMAp, LMAs) %>%
  mutate(site = rep(c("Dry","Dry", "Wet","Wet"), PA_para$n)) %>%
  mutate(strata = rep(c("Sun","Shade", "Sun","Shade"), PA_para$n)) 

dry <- ifelse(PA_sim_dat$site == "Dry", 1, 0)
leaf <- ifelse(PA_sim_dat$strata == "Shade", 1, 0)

obs2 <- NULL
log_mu2 <- NULL

for (i in 1:n_sample){
  q <- para$PA$q
  if (leaf[i] == 1) { # shade
    if (q * A[i] - R[i] < 0) {q <- R[i] / A[i] + 0.01} # avoid 0
    log_mu2[i] <- log_beta + log_site * dry[i] + log(LMA[i]) + 
                  0.5 * log(1 - f_val[i]) -
                  0.5 * log(q * A[i] - R[i]) -
                  0.5 * sigma2^2
    } else {
    log_mu2[i] <- log_beta + log_site * dry[i] + log(LMA[i]) +
                 0.5 * log(1 - f_val[i]) -
                 0.5 * log(A[i] - R[i]) -
                 0.5 * sigma2^2
    }

  mu_temp <- rmvnorm(1,
    mean = c(log_alpha + alpha2 * log(LMAp[i]) - 0.5 * sigma1^2,
        log_mu2[i],
        log_r + rp * log(LMAp[i]) + rs * log(LMAs[i]) - 0.5 * sigma3^2),
    sigma = Sigma
    )
  obs2 <- rbind(obs2, mu_temp)
}

PA_sim_dat2 <- PA_sim_dat %>%
  mutate(Aarea = exp(obs2[,1]) - exp(obs2[,3])) %>%
  mutate(LL = exp(obs2[,2])) %>%
  mutate(Rarea = exp(obs2[,3])) 

#res <- NULL
#res2 <- NULL
#for (i in 1:100) {
#  a <- rnorm(100, 30, 10)
#  a2 <- rnorm(100, a, 0.1)
#  res <- c(res, sd(a))
#  res2 <- c(res2, sd(a2))
#}
#
#plot(res, res2) 
#abline(a=0,b=1)

# weak data -------------------------------------------------------------------

sigma1 <- para$WC$sigma1
sigma2 <- para$WC$sigma2
sigma3 <- para$WC$sigma3
rho12 <- para$WC$rho12
rho23 <- para$WC$rho23
rho13 <- para$WC$rho13
log_alpha <- para$WC$log_alpha
log_beta <- para$WC$log_beta
log_r <- para$WC$log_r
alpha2 <- para$WC$alpha2
beta2 <- para$WC$beta2
rp <- para$WC$rp
rs <- para$WC$rs

LMA <- rlnorm(n_sample, log(120) - 0.5 * 0.3^2, 0.3)
f_val <- rbeta(n_sample, 5, 5)

LMAp <- LMA * f_val
LMAs <- LMA - LMAp

Sigma <- matrix(c(sigma1^2, rho12*sigma1*sigma2, rho13*sigma1*sigma3,
    rho12*sigma1*sigma2, sigma2^2, rho23*sigma2*sigma3,
    rho13*sigma1*sigma3, rho23*sigma2*sigma3, sigma3^2), ncol =3)

# leaf data -------------------------------------------------------------
obs <- NULL
for (i in 1:n_sample){
  mu_temp <- rmvnorm(1,
    mean = c(log_alpha + alpha2 * log(LMAp[i]) - 0.5 * sigma1^2,
        log_beta + beta2 * log(LMAs[i]) - 0.5 * sigma2^2,
        log_r + rp * log(LMAp[i]) + rs * log(LMAs[i]) - 0.5 * sigma3^2),
    sigma = Sigma
    )
  obs <- rbind(obs, mu_temp)
}

WC_sim_dat <- data_frame(LMA, f_val, LMAp, LMAs) %>%
  mutate(Aarea = exp(obs[,1]) - exp(obs[,3])) %>%
  mutate(LL = exp(obs[,2])) %>%
  mutate(Rarea = exp(obs[,3])) 
 
