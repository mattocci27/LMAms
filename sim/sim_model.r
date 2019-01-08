library(tidyverse)
library(rstan)
library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = 3)
print(parallel::detectCores())

set.seed(5)
source("./sim/sim_data.r")
print("load sim_data.r")

n_chains <- 3

#argv <- commandArgs(trailingOnly = TRUE)
#n_model <- noquote(argv[1])
#data_name <- argv[2]
#n_iter <- as.numeric(argv[3])
#n_warm <- as.numeric(argv[4])
#n_thin <- as.numeric(argv[5])
#obs <- argv[6]

n_model <- noquote("potLL")
data_name <- "GL1"
n_iter <- 2000
n_warm <- 1000
n_thin <- 20
obs <- "obs"

print(paste("Model:", n_model))
print(paste("Data:", data_name))
print(paste("n_iter =", n_iter))
print(paste("n_warm =", n_warm))
print(paste("n_thin =", n_thin))
print(paste("n_chains =", n_chains))

if (data_name == "GL1") {
  dat <- GL_sim_dat %>% as.data.frame
  list_dat <- list(n_sample = nrow(dat),
            obs = cbind(log(dat[ , "Aarea"] + dat[ , "Rarea"]),
                  log(dat[ , "LL"]),
                  log(dat[ , "Rarea"])),
            A = dat[ , "Aarea"],
            R = dat[ , "Rarea"],
            q_lim = 1,
            leaf = 1,
            dry = 1,
            DE = as.numeric(as.factor(dat[,"gr"])),
            LMA = dat[ , "LMA"])
  
} else if (data_name == "GL2") {
  dat <- GL_sim_dat2 %>% as.data.frame
  list_dat <- list(n_sample = nrow(dat),
            obs = cbind(log(dat[ , "Aarea"] + dat[ , "Rarea"]),
                  log(dat[ , "LL"]),
                  log(dat[ , "Rarea"])),
            A = dat[ , "Aarea"],
            R = dat[ , "Rarea"],
            q_lim = 1,
            leaf = 1,
            dry = 1,
            DE = as.numeric(as.factor(dat[,"gr"])),
            LMA = dat[ , "LMA"])
} else if (data_name == "PA") {
  dat <- PA_sim_dat2 %>% 
    filter(Aarea - Rarea > 0) %>%
    mutate(gr = paste(strata, site, sep = "_")) %>%
    as.data.frame

  q_lim <- dat %>%
    filter(strata == "Shade") %>%
    mutate(q = Rarea/Aarea) %>%
    summarize(max(q)) %>%
    as.numeric()

  list_dat <- list(n_sample = nrow(dat),
          obs = cbind(log(dat[ , "Aarea"] + dat[ , "Rarea"]),
                log(dat[ , "LL"]),
                log(dat[ , "Rarea"])),
          LMA = dat[ , "LMA"],
          A = dat[ , "Aarea"],
          R = dat[ , "Rarea"],
          q_lim = q_lim,
          DE = as.numeric(as.factor(dat[,"gr"])),
          leaf = as.numeric(as.factor(dat$strata)),
          dry = ifelse(dat$site == "Dry", 1 , 0))
} else if (data_name == "WC") {
  dat <- WC_sim_dat %>% as.data.frame
  list_dat <- list(n_sample = nrow(dat),
            obs = cbind(log(dat[ , "Aarea"] + dat[ , "Rarea"]),
                  log(dat[ , "LL"]),
                  log(dat[ , "Rarea"])),
            A = dat[ , "Aarea"],
            R = dat[ , "Rarea"],
            q_lim = 1,
            leaf = 1,
            dry = 1,
            DE = 1,
            LMA = dat[ , "LMA"])
}

list_dat_n <- list()
for (i in 1:10){
    temp <- data.frame(dat[,1:3],
             LMA = sample(dat$LMA),
             LL = sample(dat$LL),
             Aarea = sample(dat$Aarea),
             Rarea = sample(dat$Rarea)
             ) 
    while (min(temp$Aarea - temp$Rarea) < 0){
    temp <- data.frame(dat[,1:3],
             LMA = sample(dat$LMA),
             LL = sample(dat$LL),
             Aarea = sample(dat$Aarea),
             Rarea = sample(dat$Rarea)
             ) 
    }

  rand <- cbind(log(temp[ , "Aarea"] + temp[ , "Rarea"]),
        log(temp[ , "LL"]),
        log(temp[ , "Rarea"]))
  temp$A_R <- temp$A - temp$R

  list_dat_n[[i]] <- list(n_sample = list_dat$n_sample,
            obs = rand,
            A = temp$Aarea,
            R = temp$Rarea,
            q_lim = list_dat$q_lim,
            leaf = list_dat$leaf,
            dry = list_dat$dry,
            DE = list_dat$DE,
            LMA = list_dat$LMA)
} 

# opt model with power law (GL)
potLL <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    row_vector[3] obs[n_sample];
  }
  parameters{
    real log_alpha;
    real alpha2;
    real log_beta;
    real beta2;
    real log_r;
    real rp;
    real rs;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector<lower=0, upper=1>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    for (i in 1:n_sample){
      mu[i,1] = log_alpha + alpha2 * (log(LMA[i]) + log(p[i]))
             - 0.5 * square(L_sigma[1]);
      mu[i,2] = log_beta + beta2 * (log(LMA[i]) + log(1-p[i]))
             - 0.5 * square(L_sigma[2]);
      mu[i,3]  = log_r +  rp * (log(LMA[i]) + log(p[i]))
              + rs * (log(LMA[i]) + log(1-p[i]))
               - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega));
    p ~ uniform(0, 1);
    log_alpha ~ normal(0, 10);
    log_beta ~ normal(0, 10);
    alpha2 ~ normal(0, 10);
    beta2 ~ normal(0, 10);
    rp ~ normal(0, 10);
    rs ~ normal(0, 10);
    log_r ~ normal(0, 10);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
    L_sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    matrix[3,3] Omega;
    cov_matrix[3] Sigma;
    cov_matrix[3] Sigma2;
    Omega = multiply_lower_tri_self_transpose(L_Omega);
    Sigma = quad_form_diag(Omega, L_sigma);
    Sigma2 = diag_pre_multiply(L_sigma, L_Omega)
       * diag_post_multiply(L_Omega', L_sigma);
    rho12 = Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
    rho23 = Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
    rho13 = Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] = multi_normal_cholesky_lpdf(obs[i] | mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"
#using z pot
pot_com <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    vector<lower=0>[n_sample] A;
    vector<lower=0>[n_sample] R;
    row_vector[3] obs[n_sample];
  }
  parameters{
    real log_alpha;
    real alpha2;
    real log_beta;
    real beta2;
    real log_r;
    real rp;
    real rs;
    real mu_z;
    real<lower=0> sigma_z;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector[n_sample] z;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    vector<lower=0, upper=1.0>[n_sample] p;
    p = 1 ./ (1 + exp(-z)); // vector
    for (i in 1:n_sample){
      mu[i,1] = log_alpha + alpha2 * (log(LMA[i]) + log(p[i]))
             - 0.5 * square(L_sigma[1]);
      mu[i,2] = log_beta + beta2 * (log(LMA[i]) + log(1-p[i]))
             - 0.5 * square(L_sigma[2]);
      mu[i,3]  = log_r +  rp * (log(LMA[i]) + log(p[i]))
              + rs * (log(LMA[i]) + log(1-p[i]))
               - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega)); // vector
    z ~ normal(mu_z, sigma_z);
    log_alpha ~ normal(0, 10);
    log_beta ~ normal(0, 10);
    alpha2 ~ normal(0, 10);
    beta2 ~ normal(0, 10);
    rp ~ normal(0, 10);
    rs ~ normal(0, 10);
    mu_z ~ normal(0, 10); 
    sigma_z ~ cauchy(0, 5);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
    L_sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    cov_matrix[3] Sigma;
    Sigma = diag_pre_multiply(L_sigma, L_Omega)
       * diag_post_multiply(L_Omega', L_sigma);
    rho12 = Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
    rho23 = Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
    rho13 = Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] = multi_normal_cholesky_lpdf(obs[i] | mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"

#using z pot
pot_diff <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    vector<lower=0>[n_sample] A;
    vector<lower=0>[n_sample] R;
    row_vector[3] obs[n_sample];
    int<lower=0> DE[n_sample];
  }
  parameters{
    real log_alpha;
    real alpha2;
    real log_beta;
    real beta2;
    real log_r;
    real rp;
    real rs;
    real mu_z;
    real<lower=0> sigma_z;
    real<lower=0> sigma_DE;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector[3] u;
    vector[n_sample] z;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    vector<lower=0, upper=1.0>[n_sample] p;
    p = 1 ./ (1 + exp(-z)); // vector
    for (i in 1:n_sample){
      mu[i,1] = log_alpha + alpha2 * (log(LMA[i]) + log(p[i]))
             - 0.5 * square(L_sigma[1]);
      mu[i,2] = log_beta + beta2 * (log(LMA[i]) + log(1-p[i]))
             - 0.5 * square(L_sigma[2]);
      mu[i,3]  = log_r +  rp * (log(LMA[i]) + log(p[i]))
              + rs * (log(LMA[i]) + log(1-p[i]))
               - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega)); // vector
    for (i in 1:n_sample) z[i] ~ normal(mu_z + u[DE[i]], sigma_z);
    u ~ normal(0, sigma_DE); // vec
    log_alpha ~ normal(0, 10);
    log_beta ~ normal(0, 10);
    alpha2 ~ normal(0, 10);
    beta2 ~ normal(0, 10);
    rp ~ normal(0, 10);
    rs ~ normal(0, 10);
    log_r ~ normal(0, 10);
    mu_z ~ normal(0, 10); 
    sigma_z ~ cauchy(0, 5);
    sigma_DE ~ cauchy(0, 5);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
    L_sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    cov_matrix[3] Sigma;
    Sigma = diag_pre_multiply(L_sigma, L_Omega)
       * diag_post_multiply(L_Omega', L_sigma);
    rho12 = Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
    rho23 = Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
    rho13 = Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] = multi_normal_cholesky_lpdf(obs[i] | mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"

# site model with power law (PA)
siteLL <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    vector<lower=0>[n_sample] A;
    vector<lower=0>[n_sample] R;
    row_vector[3] obs[n_sample];
    real<lower=0> q_lim;
    int<lower=0> leaf[n_sample];
    int<lower=0> dry[n_sample];
  }
  parameters{
    real log_alpha;
    real log_beta;
    real rp;
    real rs;
    real alpha2;
    real log_r;
    real log_site;
    real<lower=q_lim, upper=0.999> q;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector<lower=0.001, upper=0.999>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    for (i in 1:n_sample){
			mu[i,1] = log_alpha + alpha2 * (log(LMA[i]) + log(p[i]))
             - 0.5 * square(L_sigma[1]);
    if (leaf[i]==2) // sun
      mu[i,2] = log_beta + log_site * dry[i] + log(LMA[i])
            + 0.5 * log(1 - p[i])
            - 0.5 * log(A[i] - R[i])
            - 0.5 * square(L_sigma[2]);
    else
      mu[i,2] = log_beta + log_site * dry[i] + log(LMA[i])
            + 0.5 * log(1 - p[i])
            - 0.5 * log(q * A[i] - R[i])
            - 0.5 * square(L_sigma[2]);
		mu[i,3]  = log_r + rp * (log(LMA[i]) + log(p[i]))
						+ rs * (log(LMA[i]) + log(1-p[i]))
						- 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega));
    p ~ uniform(0.001, 0.999);
    log_alpha ~ normal(0, 10);
    log_beta ~ normal(0, 10);
    rp ~ normal(0, 10);
    rs ~ normal(0, 10);
    alpha2 ~ normal(0, 10);
    log_r ~ normal(0, 10);
    log_site ~ normal(0, 10);
    q ~ uniform(q_lim, 0.999);
    L_Omega ~ lkj_corr_cholesky(1);
    L_sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    cov_matrix[3] Sigma;
    Sigma = diag_pre_multiply(L_sigma, L_Omega)
       * diag_post_multiply(L_Omega', L_sigma);
    rho12 = Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
    rho23 = Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
    rho13 = Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
    for (i in 1:n_sample)
      log_lik[i] = multi_normal_cholesky_lpdf(obs[i] | mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"

#using z site
site_com <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    vector<lower=0>[n_sample] A;
    vector<lower=0>[n_sample] R;
    row_vector[3] obs[n_sample];
    real<lower=0> q_lim;
    int<lower=0> DE[n_sample];
    int<lower=0> leaf[n_sample];
    int<lower=0> dry[n_sample];
  }
  parameters{
    real log_alpha;
    real alpha2;
    real log_beta;
    real rp;
    real rs;
    real log_r;
    real log_site;
    real mu_z;
    real<lower=q_lim, upper=1> q;
    real<lower=0> sigma_z;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector[n_sample] z;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    vector<lower=0, upper=1.0>[n_sample] p;
    p = 1 ./ (1 + exp(-z)); // vector
    for (i in 1:n_sample){
			mu[i,1] = log_alpha + alpha2 * (log(LMA[i]) + log(p[i]))
             - 0.5 * square(L_sigma[1]);
      if (leaf[i]==2) // sun
        mu[i,2] = log_beta + log_site * dry[i] + log(LMA[i])
              + 0.5 * log(1 - p[i])
              - 0.5 * log(A[i] - R[i])
              - 0.5 * square(L_sigma[2]);
      else
        mu[i,2] = log_beta + log_site * dry[i] + log(LMA[i])
              + 0.5 * log(1 - p[i])
              - 0.5 * log(q * A[i] - R[i])
              - 0.5 * square(L_sigma[2]);
      mu[i,3]  = log_r + rp * (log(LMA[i]) + log(p[i]))
              + rs * (log(LMA[i]) + log(1-p[i]))
              - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega)); // vector
    z ~ normal(mu_z, sigma_z);
    log_alpha ~ cauchy(0, 5);
    log_beta ~ cauchy(0, 5);
    log_site ~ cauchy(0, 5);
    rp ~ cauchy(0, 5); //
    rs ~ cauchy(0, 5);
    mu_z ~ normal(0, 10); 
    sigma_z ~ cauchy(0, 5);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
    L_sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    cov_matrix[3] Sigma;
    Sigma = diag_pre_multiply(L_sigma, L_Omega)
       * diag_post_multiply(L_Omega', L_sigma);
    rho12 = Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
    rho23 = Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
    rho13 = Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] = multi_normal_cholesky_lpdf(obs[i] | mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"
#using z opt
site_diff <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    vector<lower=0>[n_sample] A;
    vector<lower=0>[n_sample] R;
    real<lower=0> q_lim;
    row_vector[3] obs[n_sample];
    int<lower=0> DE[n_sample];
    int<lower=0> leaf[n_sample];
    int<lower=0> dry[n_sample];
  }
  parameters{
    real log_alpha;
    real log_beta;
    real alpha2;
    real rp;
    real rs;
    real log_r;
    real log_site;
    real mu_z;
    real<lower=q_lim, upper=1> q;
    real<lower=0> sigma_z;
    real<lower=0> sigma_DE;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector[4] u;
    vector[n_sample] z;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    vector<lower=0, upper=1.0>[n_sample] p;
    p = 1 ./ (1 + exp(-z)); // vector
    for (i in 1:n_sample){
			mu[i,1] = log_alpha + alpha2 * (log(LMA[i]) + log(p[i]))
             - 0.5 * square(L_sigma[1]);
      if (leaf[i]==2) // sun
        mu[i,2] = log_beta + log_site * dry[i] + log(LMA[i])
              + 0.5 * log(1 - p[i])
              - 0.5 * log(A[i] - R[i])
              - 0.5 * square(L_sigma[2]);
      else
        mu[i,2] = log_beta + log_site * dry[i] + log(LMA[i])
              + 0.5 * log(1 - p[i])
              - 0.5 * log(q * A[i] - R[i])
              - 0.5 * square(L_sigma[2]);
      mu[i,3]  = log_r + rp * (log(LMA[i]) + log(p[i]))
              + rs * (log(LMA[i]) + log(1-p[i]))
              - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega)); // vector
    for (i in 1:n_sample) z[i] ~ normal(mu_z + u[DE[i]], sigma_z);
    u ~ normal(0, sigma_DE); // vec
    log_alpha ~ cauchy(0, 5);
    log_beta ~ cauchy(0, 5);
    log_site ~ cauchy(0, 5);
    rp ~ cauchy(0, 5); //
    rs ~ cauchy(0, 5);
    mu_z ~ normal(0, 10); 
    sigma_z ~ cauchy(0, 5);
    sigma_DE ~ cauchy(0, 5);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
    L_sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    cov_matrix[3] Sigma;
    Sigma = diag_pre_multiply(L_sigma, L_Omega)
       * diag_post_multiply(L_Omega', L_sigma);
    rho12 = Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
    rho23 = Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
    rho13 = Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] = multi_normal_cholesky_lpdf(obs[i] | mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"

if (obs == "obs") {
system.time(fit <- stan(model_code = get(n_model),
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

save_name <- paste("./data/", data_name, "_", n_model, "_sim.rda", sep = "")
}


if (obs == "rand") {
  res_list <- list()
  summary_list <- list()
  n_iter <- n_iter / 2
  n_warm <- n_warm / 2
  n_thin <- n_thin / 2
  for (i in 1:10){

    system.time(fit1 <- stan(model_code = get(n_model),
                data = list_dat_n[[i]],
                iter = 1,
                warmup = 0,
                thin = 1,
                chains = 1))

    system.time(res_list[[i]] <- stan(fit = fit1,
               data = list_dat_n[[i]],
               iter = n_iter,
               warmup = n_warm,
               thin = n_thin,
               chains = n_chains,
               control = list(adapt_delta = 0.9,
                  max_treedepth = 10)))

    summary_list[[i]] <- data.frame(summary(res_list[[i]])$summary)
  }
  save_name <- paste("./data/", data_name, "_", n_model, "_sim_r.rda", sep = "")
}

save.image(save_name)
