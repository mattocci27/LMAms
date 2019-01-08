library(tidyverse)
library(rstan)
library(stringr)
library(loo)
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
options(mc.cores = 3)

set.seed(5)

n_chains <- 3

dat <- read_csv("./data/GL_data.csv") %>%
    mutate(DE = ifelse(is.na(DE), "U", DE)) %>%
    as.data.frame

list_dat <- list(n_sample = nrow(dat),
                 #obs = cbind(log(dat[ , "Aarea"] + dat[ , "Rarea"]),
                 obs = cbind(log(dat[ , "Aarea"]),
                             log(dat[ , "LL"]),
                             log(dat[ , "Rarea"])),
                 LMA = dat[ , "LMA"],
                 A = dat[ , "Aarea"],
                 R = dat[ , "Rarea"],
                 q_lim = 1,
                 leaf = 1,
                 dry = 1,
                 DE = as.numeric(as.factor(dat$DE)))


list_dat2 <- list_dat
list_dat2$obs <- cbind(log(dat[ , "Aarea"] + dat[, "Rarea"]),
                             log(dat[ , "LL"]),
                             log(dat[ , "Rarea"]))


modelMVN <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    row_vector[3] obs[n_sample];
  }
  parameters{
    vector[7] beta;
    vector<lower=0>[3] sigma;
    cholesky_factor_corr[3] L_Omega;
    vector<lower=0, upper=1>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    for (i in 1:n_sample){
      mu[i,1] = beta[1] + beta[2] * (log(LMA[i]) + log(p[i]))
            - 0.5 * square(sigma[1]);
      mu[i,2] = beta[3] + beta[4] * (log(LMA[i]) + log(1-p[i]))
             - 0.5 * square(sigma[2]);
      mu[i,3] = beta[5] +  beta[6] * (log(LMA[i]) + log(p[i]))
              + beta[7] * (log(LMA[i]) + log(1-p[i]))
               - 0.5 * square(sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(sigma, L_Omega));
    beta ~ normal(0, 10); 
    p ~ uniform(0, 1);
    sigma ~ cauchy(0, 5);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    cov_matrix[3] Sigma;
    Sigma = diag_pre_multiply(sigma, L_Omega)
       * diag_post_multiply(L_Omega', sigma);
    rho12 = Sigma[1, 2] * inv(sigma[1] * sigma[2]);
    rho23 = Sigma[2, 3] * inv(sigma[2] * sigma[3]);
    rho13 = Sigma[1, 3] * inv(sigma[1] * sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] = multi_normal_cholesky_lpdf(obs[i] | mu[i], diag_pre_multiply(sigma, L_Omega));
   }
"


modelN <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    vector[3] obs[n_sample];
  }
  parameters{
    vector[7] beta;
    vector<lower=0>[3] sigma;
    vector<lower=0, upper=1>[n_sample] p;
  }
  transformed parameters{
    vector[3] mu[n_sample];
    for (i in 1:n_sample){
      mu[i,1] = beta[1] + beta[2] * (log(LMA[i]) + log(p[i]))
            - 0.5 * square(sigma[1]);
      mu[i,2] = beta[3] + beta[4] * (log(LMA[i]) + log(1-p[i]))
             - 0.5 * square(sigma[2]);
      mu[i,3] = beta[5] +  beta[6] * (log(LMA[i]) + log(p[i]))
              + beta[7] * (log(LMA[i]) + log(1-p[i]))
               - 0.5 * square(sigma[3]);
    }
  }
  model{
    for (i in 1:n_sample) {
      obs[i,1] ~ normal(mu[i,1], sigma[1]);
      obs[i,2] ~ normal(mu[i,2], sigma[2]);
      obs[i,3] ~ normal(mu[i,3], sigma[3]);
    }
    beta ~ normal(0, 10); 
    p ~ uniform(0, 1);
    sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    vector[n_sample] log_lik1;
    vector[n_sample] log_lik2;
    vector[n_sample] log_lik3;
    for (i in 1:n_sample){
     log_lik1[i] = normal_lpdf(obs[i, 1] | mu[i, 1], sigma[1]);
     log_lik2[i] = normal_lpdf(obs[i, 2] | mu[i, 2], sigma[2]);
     log_lik3[i] = normal_lpdf(obs[i, 3] | mu[i, 3], sigma[3]);
   }
    log_lik = log_lik1 + log_lik2 + log_lik3;
  }
"

modelMVNH <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    row_vector[3] obs[n_sample];
    int DE[n_sample];
  }
  parameters{
    vector[7] beta[3];
    vector[7] beta_hat;
    vector<lower=0>[7] sigma_beta;
    vector<lower=0>[3] sigma;
    cholesky_factor_corr[3] L_Omega;
    vector<lower=0, upper=1>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    for (i in 1:n_sample){
      mu[i,1] = beta[DE[i], 1] + beta[DE[i], 2] * (log(LMA[i]) + log(p[i]))
            - 0.5 * square(sigma[1]);
      mu[i,2] = beta[DE[i], 3] + beta[DE[i], 4] * (log(LMA[i]) + log(1-p[i]))
             - 0.5 * square(sigma[2]);
      mu[i,3] = beta[DE[i], 5] +  beta[DE[i], 6] * (log(LMA[i]) + log(p[i]))
              + beta[DE[i], 7] * (log(LMA[i]) + log(1-p[i]))
               - 0.5 * square(sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(sigma, L_Omega));
    for (i in 1:3){
      beta[i] ~ normal(beta_hat, sigma_beta[i]); 
    }
    beta_hat ~ normal(0, 10);
    sigma_beta ~ cauchy(0, 5);
    p ~ uniform(0, 1);
    sigma ~ cauchy(0, 5);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    cov_matrix[3] Sigma;
    Sigma = diag_pre_multiply(sigma, L_Omega)
       * diag_post_multiply(L_Omega', sigma);
    rho12 = Sigma[1, 2] * inv(sigma[1] * sigma[2]);
    rho23 = Sigma[2, 3] * inv(sigma[2] * sigma[3]);
    rho13 = Sigma[1, 3] * inv(sigma[1] * sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] = multi_normal_cholesky_lpdf(obs[i] | mu[i], diag_pre_multiply(sigma, L_Omega));
   }
"


modelNH <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    vector[3] obs[n_sample];
    int DE[n_sample];
  }
  parameters{
    vector[7] beta[3];
    vector[7] beta_hat;
    vector<lower=0>[3] sigma;
    vector<lower=0>[7] sigma_beta;
    vector<lower=0, upper=1>[n_sample] p;
  }
  transformed parameters{
    vector[3] mu[n_sample];
    for (i in 1:n_sample){
      mu[i,1] = beta[DE[i], 1] + beta[DE[i], 2] * (log(LMA[i]) + log(p[i]))
            - 0.5 * square(sigma[1]);
      mu[i,2] = beta[DE[i], 3] + beta[DE[i], 4] * (log(LMA[i]) + log(1-p[i]))
             - 0.5 * square(sigma[2]);
      mu[i,3] = beta[DE[i], 5] +  beta[DE[i], 6] * (log(LMA[i]) + log(p[i]))
              + beta[DE[i], 7] * (log(LMA[i]) + log(1-p[i]))
               - 0.5 * square(sigma[3]);
    }
  }
  model{
    for (i in 1:n_sample) {
      obs[i,1] ~ normal(mu[i,1], sigma[1]);
      obs[i,2] ~ normal(mu[i,2], sigma[2]);
      obs[i,3] ~ normal(mu[i,3], sigma[3]);
    }
    for (i in 1:3){
      beta[i] ~ normal(beta_hat, sigma_beta[i]); 
    }
    beta_hat ~ normal(0, 10);
    sigma_beta ~ cauchy(0, 5);
    p ~ uniform(0, 1);
    sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    vector[n_sample] log_lik1;
    vector[n_sample] log_lik2;
    vector[n_sample] log_lik3;
    for (i in 1:n_sample){
     log_lik1[i] = normal_lpdf(obs[i, 1] | mu[i, 1], sigma[1]);
     log_lik2[i] = normal_lpdf(obs[i, 2] | mu[i, 2], sigma[2]);
     log_lik3[i] = normal_lpdf(obs[i, 3] | mu[i, 3], sigma[3]);
   }
    log_lik = log_lik1 + log_lik2 + log_lik3;
  }
"


# MVN + A

n_iter <- 1000
n_warm <- 500
n_thin <- 1

system.time(fit <- stan(model_code = modelMVN,
            data = list_dat,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(res <- stan(fit = fit,
           data = list_dat,
           iter = n_iter,
           warmup = n_warm,
           thin = n_thin,
           chains = n_chains,
           control = list(adapt_delta = 0.95, max_treedepth = 10)))

summary1 <- data.frame(summary(res)$summary)

head(summary1, 10)
tail(summary1, 10)



# MVN + AR


system.time(res2 <- stan(fit = fit,
           data = list_dat2,
           iter = n_iter,
           warmup = n_warm,
           thin = n_thin,
           chains = n_chains,
           control = list(adapt_delta = 0.95, max_treedepth = 10)))

summary2 <- data.frame(summary(res2)$summary)

head(summary2, 10)
tail(summary2, 10)


# N + A

system.time(fit <- stan(model_code = modelN,
            data = list_dat,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(res3 <- stan(fit = fit,
           data = list_dat,
           iter = n_iter,
           warmup = n_warm,
           thin = n_thin,
           chains = n_chains,
           control = list(adapt_delta = 0.95, max_treedepth = 10)))

summary3 <- data.frame(summary(res3)$summary)

head(summary3, 10)
tail(summary3, 10)


# N + AR


system.time(res4 <- stan(fit = fit,
           data = list_dat2,
           iter = n_iter,
           warmup = n_warm,
           thin = n_thin,
           chains = n_chains,
           control = list(adapt_delta = 0.95, max_treedepth = 10)))

summary4 <- data.frame(summary(res4)$summary)

head(summary4, 10)
tail(summary4, 10)



# MVN + A + H


system.time(fit <- stan(model_code = modelMVNH,
            data = list_dat,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(res5 <- stan(fit = fit,
           data = list_dat,
           iter = n_iter,
           warmup = n_warm,
           thin = n_thin,
           chains = n_chains,
           control = list(adapt_delta = 0.95, max_treedepth = 10)))

summary5 <- data.frame(summary(res5)$summary)

head(summary5, 10)
tail(summary5, 10)


# MVN + A + H



system.time(res6 <- stan(fit = fit,
           data = list_dat2,
           iter = n_iter,
           warmup = n_warm,
           thin = n_thin,
           chains = n_chains,
           control = list(adapt_delta = 0.95, max_treedepth = 10)))

summary6 <- data.frame(summary(res6)$summary)

head(summary6, 10)
tail(summary6, 10)


# N + A + H


system.time(fit <- stan(model_code = modelNH,
            data = list_dat,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(res7 <- stan(fit = fit,
           data = list_dat,
           iter = n_iter,
           warmup = n_warm,
           thin = n_thin,
           chains = n_chains,
           control = list(adapt_delta = 0.95, max_treedepth = 10)))

summary7 <- data.frame(summary(res7)$summary)

head(summary7, 22)
tail(summary7, 10)


# N + AR + H

system.time(res8 <- stan(fit = fit,
           data = list_dat2,
           iter = n_iter,
           warmup = n_warm,
           thin = n_thin,
           chains = n_chains,
           control = list(adapt_delta = 0.95, max_treedepth = 10)))

summary8 <- data.frame(summary(res8)$summary)

head(summary8, 22)
tail(summary8, 10)

save.image("GL_test.rda")
system.time(res8 <- stan(fit = fit,
           data = list_dat2,
           iter = n_iter,
           warmup = n_warm,
           thin = n_thin,
           chains = n_chains,
           control = list(adapt_delta = 0.95, max_treedepth = 10)))

summary8 <- data.frame(summary(res8)$summary)

head(summary8, 22)
tail(summary8, 10)

save.image("GL_test.rda")
