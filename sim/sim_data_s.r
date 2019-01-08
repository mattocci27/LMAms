# single

#set.seed(10)
library(tidyverse)
library(rstan)

para <- yaml::yaml.load_file("parameter.yml")
n_sample <- 200

# GL sim data -----------------------------------------------------------------
#GL <- read_csv("./data/GL20170911.csv") 
load("./data/GL_potPL_obs.rda")

p <- extract(res, par = "p") 
p <- apply(p[[1]], 2, median)

GL <- dat %>%
  mutate(p_val = p) %>%
  mutate(LMAp = LMA * p_val) %>%
  mutate(LMAs = LMA - LMAp) %>%
  as_data_frame

GL_para <- GL %>%
  mutate(ratio = LMAp/LMA) %>%
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

LMAp <- LMA * f_val
LMAs <- LMA - LMAp

sigma1 <- para$GL$sigma1
sigma2 <- para$GL$sigma2
sigma3 <- para$GL$sigma3
log_alpha <- para$GL$log_alpha
log_beta <- para$GL$log_beta
log_r <- para$GL$log_r
alpha2 <- para$GL$alpha2
beta2 <- para$GL$beta2
rp <- para$GL$rp
rs <- para$GL$rs


# leaf data -------------------------------------------------------------


mu1 <- rnorm(n_sample,
              log_alpha + alpha2 * log(LMAp) - 0.5 * sigma1^2,
              sigma1)
mu2 <- rnorm(n_sample,
              log_beta + beta2 * log(LMAs) - 0.5 * sigma2^2,
              sigma2)
mu3 <- rnorm(n_sample,
              log_r + rp * log(LMAp) + rs * log(LMAs) - 0.5 * sigma3^2,
              sigma3)

GL_sim_dat <- data_frame(LMA, f_val, LMAp, LMAs) %>%
  mutate(gr = rep(c("D", "E", "U"), GL_para$n)) %>%
  mutate(Aarea = exp(mu1) - exp(mu3)) %>%
  mutate(LL = exp(mu2)) %>%
  mutate(Rarea = exp(mu3)) 

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

mu1 <- rnorm(n_sample,
              log_alpha + alpha2 * log(LMAp) - 0.5 * sigma1^2,
              sigma1)
mu2 <- rnorm(n_sample,
              log_beta + beta2 * log(LMAs) - 0.5 * sigma2^2,
              sigma2)
mu3 <- rnorm(n_sample,
              log_r + rp * log(LMAp) + rs * log(LMAs) - 0.5 * sigma3^2,
              sigma3)

GL_sim_dat2 <- data_frame(LMA, f_val, LMAp, LMAs) %>%
  mutate(gr = rep(c("D", "E", "U"), GL_para$n)) %>%
  #mutate(Aarea = exp(obs[,1]) - exp(obs[,3])) %>%
  mutate(Aarea = exp(mu1)) %>%
  mutate(LL = exp(mu2)) %>%
  mutate(Rarea = exp(mu3)) 

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
log_alpha <- para$PA$log_alpha
log_beta <- para$PA$log_beta
log_r <- para$PA$log_r
rp <- para$PA$rp
rp <- para$PA$rp
q <- para$PA$q
log_site <- para$PA$log_site

# prepare A and R first  ======================================================
mu1 <- rnorm(n_sample,
              log_alpha + alpha2 * log(LMAp) - 0.5 * sigma1^2,
              sigma1)
mu3 <- rnorm(n_sample,
              log_r + rp * log(LMAp) + rs * log(LMAs) - 0.5 * sigma3^2,
              sigma3)

AR <- exp(mu1)
R <- exp(mu3)
A <- AR - R

while (min(A-R) < 0) {
  mu1 <- rnorm(n_sample,
                log_alpha + alpha2 * log(LMAp) - 0.5 * sigma1^2,
                sigma1)
  mu3 <- rnorm(n_sample,
                log_r + rp * log(LMAp) + rs * log(LMAs) - 0.5 * sigma3^2,
                sigma3)

  AR <- exp(mu1)
  R <- exp(mu3)
  A <- AR - R
}

# leaf data -------------------------------------------------------------

PA_sim_dat <- data_frame(LMA, f_val, LMAp, LMAs) %>%
  mutate(site = rep(c("Dry","Dry", "Wet","Wet"), PA_para$n)) %>%
  mutate(strata = rep(c("Sun","Shade", "Sun","Shade"), PA_para$n)) 

dry <- ifelse(PA_sim_dat$site == "Dry", 1, 0)
leaf <- ifelse(PA_sim_dat$strata == "Shade", 1, 0)

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

}

PA_sim_dat2 <- PA_sim_dat %>%
  mutate(Aarea = A) %>%
  mutate(LL = exp(log_mu2)) %>%
  mutate(Rarea = R)

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

# leaf data -------------------------------------------------------------
mu1 <- rnorm(n_sample,
              log_alpha + alpha2 * log(LMAp) - 0.5 * sigma1^2,
              sigma1)
mu2 <- rnorm(n_sample,
              log_beta + beta2 * log(LMAs) - 0.5 * sigma2^2,
              sigma2)
mu3 <- rnorm(n_sample,
              log_r + rp * log(LMAp) + rs * log(LMAs) - 0.5 * sigma3^2,
              sigma3)

AR <- exp(mu1)
R <- exp(mu3)
A <- AR - R

while (min(A-R) < 0) {
  mu1 <- rnorm(n_sample,
                log_alpha + alpha2 * log(LMAp) - 0.5 * sigma1^2,
                sigma1)
  mu3 <- rnorm(n_sample,
                log_r + rp * log(LMAp) + rs * log(LMAs) - 0.5 * sigma3^2,
                sigma3)

  AR <- exp(mu1)
  R <- exp(mu3)
  A <- AR - R
}

WC_sim_dat <- data_frame(LMA, f_val, LMAp, LMAs) %>%
  mutate(Aarea = exp(mu1) - exp(mu3)) %>%
  mutate(LL = exp(mu2)) %>%
  mutate(Rarea = exp(mu3)) 
 
