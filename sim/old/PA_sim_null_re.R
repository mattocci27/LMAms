#test
rm(list = ls()) # This clears everything from memory.
library(rstan)
library(dplyr)
# set_cppo('debug') # make debug easier
set_cppo('fast') # make debug easier
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(mvtnorm)

# paramter setting -------------------------------------------------------
set.seed(14)
LMA1 <- rlnorm(25, log(94) - 0.5 * 0.32^2, 0.32) #sun wet
LMA2 <- rlnorm(25, log(67) - 0.5 * 0.33^2, 0.33) #sun dry
LMA3 <- rlnorm(25, log(40) - 0.5 * 0.18^2, 0.18) #shade wet
LMA4 <- rlnorm(25, log(30) - 0.5 * 0.36^2, 0.36) #shade dry

LMA <- c(LMA1, LMA2, LMA3, LMA4)
f1 <- rbeta(25, 6, 4)
f2 <- rbeta(25, 9, 4)

# f1 <- rep(0.99, 25)
# f2 <- rep(0.99, 25)
f3 <- rbeta(25, 3.5, 8)
f4 <- rbeta(25, 14, 9)

f <- c(f1,f2, f3, f4)

par(mfrow=c(2,2))
hist(f1)
hist(f2)
hist(f3)
hist(f4)
par(mfrow=c(1,1))

LMAp <- LMA * f
LMAs <- LMA * (1-f)
plot(LMAp ~ LMAs, log = "xy")

sigma1 <- 0.2
sigma2 <- 0.5
sigma3 <- 0.7
rho12 <- 0.8
rho13 <- 0
rho23 <- 0.1
alpha <- 0.28
beta <- 0.7
q <- 0.4
log_site <- log(0.7)

Sigma2 <- matrix(c(sigma1^2, rho13*sigma1*sigma3,
        rho13*sigma1*sigma3, sigma3^2), ncol = 2)


DE = rep(1:4, each = 25)
dry = rep(c(0,1,0,1), each = 25)
leaf = rep(1:2, each = 50)


# leaf data -------------------------------------------------------------
obs <- NULL
log_mu2 <- NULL
for (i in 1:100){
  temp <- rmvnorm(1,
      mean = c(log(alpha) + log(LMA[i]) + log(f[i])　- 0.5 * sigma1^2,
          log(0.02 * LMA[i] * f[i] + 0.001 * LMA[i] * ( 1- f[i]))
          - 0.5 * sigma3^2),
      sigma = Sigma2
      )
      obs <- rbind(obs, temp)
}

sigma1_re <- 0
sigma3_re <- 0
Sigma <- matrix(
    c(sigma1_re^2, rho12*sigma1_re*sigma2, rho13*sigma1_re*sigma3_re,
    rho12*sigma1_re*sigma2, sigma2^2, rho23*sigma2*sigma3_re,
    rho13*sigma1_re*sigma3_re, rho23*sigma2*sigma3_re, sigma3_re^2), ncol =3)

obs2 <- NULL
log_mu2 <- NULL
for (i in 1:100){
  {if (leaf[i] == 2) { # shade
    log_mu2[i] <- log(beta) + log_site * dry[i] + log(LMA[i]) + 0.5 * log(1 - f[i]) - 0.5 * log(q * exp(obs[i, 1]) - exp(obs[i, 2])) - 0.5 * sigma2^2
    }
  else {
    log_mu2[i] <- log(beta) + log_site * dry[i] + log(LMA[i]) +  0.5 * log(1 - f[i]) - 0.5 * log(exp(obs[i, 1]) - exp(obs[i, 2])) - 0.5 * sigma2^2
    }}

  temp <- rmvnorm(1,
    mean = c(obs[i, 1],
        log_mu2[i],
        obs[i, 2]),
    sigma = Sigma
    )
  obs2 <- rbind(obs2, temp)
}
AR <- exp(obs2[,1])
LL <- exp(obs2[,2])
R <- exp(obs2[,3])
A <- AR - R

data <- data.frame(A, R, LL, LMA, LMAs, LMAp,
  DE = rep(c("sun_wet","sun_dry","shade_wet","shade_dry"), each =25),
  leaf = rep(c("sun", "shade"), each =50),
  site = rep(c("wet", "dry", "wet", "dry"), each = 25)) %>%
  mutate(DE2 = factor(DE, levels = c("sun_dry", 'shade_dry',"sun_wet", "shade_wet")))

boxplot(LL ~ DE2, data, log = "y")




# check how data looks like
cor.test(log(LL),log(A))
par(mfrow=c(1,2))
plot(LL ~ A, log = "xy")
plot(LL ~ R, log = "xy")
par(mfrow=c(1,1))

p <- ggplot(data, aes(colour = DE))
p <- p + geom_point(aes(x = LMA, y = LL))
p <- p + scale_y_log10()+ scale_x_log10()
p

p <- ggplot(data, aes(colour = DE))
p <- p + geom_point(aes(x = LMA, y = A))
p <- p + scale_y_log10()+ scale_x_log10()
p

par(mfrow=c(3,3))
plot(A ~ LMA, log = "xy")
plot(A ~ LMAp, log = "xy")
plot(A ~ LMAs, log = "xy")
plot(R~ LMA, log = "xy")
plot(R ~ LMAp, log = "xy")
plot(R ~ LMAs, log = "xy")
plot(LL ~ LMA, log = "xy")
plot(LL ~ LMAp, log = "xy")
plot(LL ~ LMAs, log = "xy")
par(mfrow=c(1,1))
# models -------------------------------------------------------------------
model_unif_pot <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    row_vector[3] obs[n_sample];
  }
  parameters{
    real log_alpha;
    real log_beta;
    real rp;
    real rs;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector<lower=0, upper=1>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    real<lower=0> mu3;

    for (i in 1:n_sample){
      mu[i,1] <- log_alpha + log(LMA[i]) + log(p[i])
             - 0.5 * square(L_sigma[1]);
      mu[i,2] <- log_beta + log(LMA[i]) + log(1-p[i])
             - 0.5 * square(L_sigma[2]);
      mu3 <- rp * LMA[i] * p[i] + rs * LMA[i] * (1 - p[i]);
      mu[i, 3]  <- log(mu3) - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega));
    p ~ uniform(0, 1);
    log_alpha ~ cauchy(0, 5);
    log_beta ~ cauchy(0, 5);
    rp ~ cauchy(0,5);
    rs ~ cauchy(0,5);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
    L_sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    cov_matrix[3] Sigma;
    Sigma <- diag_pre_multiply(L_sigma, L_Omega)
       * diag_post_multiply(L_Omega', L_sigma);
    rho12 <- Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
    rho23 <- Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
    rho13 <- Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] <- multi_normal_cholesky_log(obs[i], mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"
model_unif_opt <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    vector<lower=0>[n_sample] A;
    vector<lower=0>[n_sample] R;
    real<lower=0> q_lim;
    row_vector[3] obs[n_sample];
    int<lower=0> DE[n_sample];
    int<lower=0> leaf[n_sample];
  }
  parameters{
    real log_alpha;
    real log_beta;
    real rp;
    real rs;
    real<lower=q_lim, upper=1> q;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector<lower=0, upper=1>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    real<lower=0> mu3;

    for (i in 1:n_sample){
      mu[i,1] <- log_alpha + log(LMA[i]) + log(p[i])
             - 0.5 * square(L_sigma[1]);
      if (leaf[i]==1) // sun
        mu[i,2] <- log_beta + log(LMA[i])
              + 0.5 * log(1 - p[i])
              - 0.5 * log(A[i] - R[i])
              - 0.5 * square(L_sigma[2]);
      else
        mu[i,2] <- log_beta + log(LMA[i])
              + 0.5 * log(1 - p[i])
              - 0.5 * log(q * A[i] - R[i])
              - 0.5 * square(L_sigma[2]);
      mu3 <- rp * LMA[i] * p[i] + rs * LMA[i] * (1 - p[i]);
      mu[i, 3]  <- log(mu3) - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega));
    p ~ uniform(0, 1);
    q ~ uniform(q_lim, 1);
    log_alpha ~ cauchy(0, 5);
    log_beta ~ cauchy(0, 5);
    rp ~ cauchy(0,5);
    rs ~ cauchy(0,5);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
    L_sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    cov_matrix[3] Sigma;
    Sigma <- diag_pre_multiply(L_sigma, L_Omega)
       * diag_post_multiply(L_Omega', L_sigma);
    rho12 <- Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
    rho23 <- Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
    rho13 <- Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] <- multi_normal_cholesky_log(obs[i], mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"
model_unif_site <- "
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
    real rp;
    real rs;
    real log_site;
    real<lower=q_lim, upper=1> q;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector<lower=0, upper=1>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    real<lower=0> mu3;

    for (i in 1:n_sample){
      mu[i,1] <- log_alpha + log(LMA[i]) + log(p[i])
             - 0.5 * square(L_sigma[1]);
       if (leaf[i]==1) // sun
         mu[i,2] <- log_beta + log_site * dry[i] + log(LMA[i])
               + 0.5 * log(1 - p[i])
               - 0.5 * log(A[i] - R[i])
               - 0.5 * square(L_sigma[2]);
       else
         mu[i,2] <- log_beta + log_site * dry[i] + log(LMA[i])
               + 0.5 * log(1 - p[i])
               - 0.5 * log(q * A[i] - R[i])
               - 0.5 * square(L_sigma[2]);
      mu3 <- rp * LMA[i] * p[i] + rs * LMA[i] * (1 - p[i]);
      mu[i, 3]  <- log(mu3) - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega));
    p ~ uniform(0, 1);
    q ~ uniform(q_lim, 1);
    log_alpha ~ cauchy(0, 5);
    log_beta ~ cauchy(0, 5);
    log_site ~ cauchy(0, 5);
    rp ~ cauchy(0,5);
    rs ~ cauchy(0,5);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
    L_sigma ~ cauchy(0, 5);
  }
  generated quantities {
    vector[n_sample] log_lik;
    real<lower=-1, upper=1> rho12;
    real<lower=-1, upper=1> rho23;
    real<lower=-1, upper=1> rho13;
    cov_matrix[3] Sigma;
    Sigma <- diag_pre_multiply(L_sigma, L_Omega)
       * diag_post_multiply(L_Omega', L_sigma);
    rho12 <- Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
    rho23 <- Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
    rho13 <- Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] <- multi_normal_cholesky_log(obs[i], mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"

n.iter <- 10000
n.warm <- 5000
n.thin <- 5
n.chains <- 3

data.obs <- data

LMA.rc.data <- NULL
for (i in 1:10) {
  data1 <- data.obs %>%
    filter(site == "wet") %>%
    mutate(LMA.rc = sample(LMA))

  data2 <- data.obs %>%
    filter(site == "dry") %>%
    mutate(LMA.rc = sample(LMA))

  data.n <- rbind(data1, data2) %>%
    arrange(desc(site)) %>%
    arrange(desc(leaf))
  LMA.rc.data <- cbind(LMA.rc.data, data.n$LMA.rc)
}

# data <- as.data.frame(data)

# obs_null <- obs
#
# obs_null[,2] <- sample(obs_null[,2])
#
# LMA_null <- sample(LMA)

q_lim <- data %>%
　　filter(leaf == "shade") %>%
　　mutate(q = R/A) %>%
　　summarize(max(q)) %>%
　　as.numeric()

res1 <- list()
res2 <- list()
res3 <- list()
m1 <- list()
m2 <- list()
m3 <- list()

for (i in 1:10){
  list.data1 <- list(n_sample = 100,
            obs = obs2,
            LMA = LMA.rc.data[ , i],
            A = A,
            R = R,
            DE = DE,
            leaf = leaf,
            dry = dry,
            q_lim = q_lim)

  system.time(fit1 <- stan(model_code = model_unif_pot,
              data = list.data1,
              iter = 1,
              warmup = 0,
              thin = 1,
              chains = 1))

  system.time(fit2 <- stan(model_code = model_unif_opt,
              data = list.data1,
              iter = 1,
              warmup = 0,
              thin = 1,
              chains = 1))

  system.time(fit3 <- stan(model_code = model_unif_site,
              data = list.data1,
              iter = 1,
              warmup = 0,
              thin = 1,
              chains = 1))

  system.time(res1[[i]] <- stan(fit = fit1,
             data = list.data1,
             iter = n.iter,
             warmup = n.warm,
             thin = n.thin,
             chains = n.chains,
             control = list(stepsize = 0.01, adapt_delta = 0.9,
                max_treedepth = 10)))

  system.time(res2[[i]] <- stan(fit = fit2,
             data = list.data1,
             iter = n.iter,
             warmup = n.warm,
             thin = n.thin,
             chains = n.chains,
             control = list(stepsize = 0.01, adapt_delta = 0.9,
                max_treedepth = 10)))

  system.time(res3[[i]] <- stan(fit = fit3,
             data = list.data1,
             iter = n.iter,
             warmup = n.warm,
             thin = n.thin,
             chains = n.chains,
             control = list(stepsize = 0.01, adapt_delta = 0.9,
                max_treedepth = 10)))

  m1[[i]] <- data.frame(summary(res1[[i]])$summary)
  m2[[i]] <- data.frame(summary(res2[[i]])$summary)
  m3[[i]] <- data.frame(summary(res3[[i]])$summary)

}

# save.image("~/Dropbox/LES/negative_cov_model2.RData")

save.image("~/Dropbox/LES/PA_sim_null.RData")
