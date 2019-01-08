#test
rm(list = ls()) # This clears everything from memory.
library(rstan)
library(dplyr)

set_cppo('fast') # make debug easier
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(mvtnorm)

# paramter setting -------------------------------------------------------
set.seed(10)
n_dec <- 7
n_ev <- 70
n_u <- 23
n_sample <- n_dec + n_ev + n_u
LMA1 <- rlnorm(n_dec, log(80) - 0.5 * 0.31^2, 0.31) #dec
LMA2 <- rlnorm(n_ev, log(170) - 0.5 * 0.50^2, 0.50) #ev
LMA3 <- rlnorm(n_u, log(80) - 0.5 * 0.51^2, 0.51) #unknown

LMA <- c(LMA1, LMA2, LMA3)
phi <- 0.8 + log(LMA2) * -0.09
lambda <- 3.2
# f <- NULL
# for (i in 1:n_sample) f[i] <- rbeta(1, phi[i] * lambda, lambda*(1-phi[i]))
# hist(f)
# f1 <- f[1:n_dec]
# f2 <- f[(n_dec+1):(n_dec+n_ev+1)]
# f3 <- f[(n_dec+n_ev+2):n_sample]
f1 <- rbeta(n_dec, 19, 10)
f2 <- NULL
for (i in 1:n_ev) f2[i] <- rbeta(1, phi[i] * lambda, lambda*(1-phi[i]))
# f2 <- rbeta(n_ev, 5.5, 9.2)
f3 <- rbeta(n_u, 6.8, 2.6)
#
f <- c(f1, f2, f3)

par(mfrow=c(2,2))
hist(f1)
hist(f2)
hist(f3)
par(mfrow=c(1,1))
# mean(f2)

LMAp <- LMA * f
LMAs <- LMA * (1-f)
par(mfrow=c(1,2))
plot(LMAs ~ LMAp, log = "xy")
plot(f ~ LMA, log = "x")
par(mfrow=c(1,1))



sigma1 <- 0.25
sigma2 <- 0.65
sigma3 <- 0.6
rho12 <- -0.66
rho13 <- -0.61
rho23 <- 0.58
log_alpha <- 0.95
alpha2 <- 0.4
log_beta <- 0
beta2 <- 0.6


Sigma <- matrix(c(sigma1^2, rho12*sigma1*sigma2, rho13*sigma1*sigma3,
    rho12*sigma1*sigma2, sigma2^2, rho23*sigma2*sigma3,
    rho13*sigma1*sigma3, rho23*sigma2*sigma3, sigma3^2), ncol =3)

DE <- rep(1:3, each = 30)
DE <- c(rep(1,n_dec), rep(2, n_ev), rep(3, n_u))
# leaf data -------------------------------------------------------------
obs <- NULL
log_mu2 <- NULL
for (i in 1:n_sample){
# log_mu2[i] <- log_beta + 0.5 * log(LMA[i]) + 1.5 * log(1-f[i]) - 1.5 * log(f[i])- 0.5 * sigma2^2
#
  temp <- rmvnorm(1,
    mean = c(log_alpha + alpha2 * (log(LMA[i]) + log(f[i]))　- 0.5 * sigma1^2,
        log_beta + beta2 * (log(LMA[i]) + log(1 - f[i]))　- 0.5 * sigma2^2,
        log(0.02 * LMA[i] * f[i] + 0.001 * LMA[i] * ( 1- f[i])) - 0.5 * sigma3^2),
    sigma = Sigma
    )
  obs <- rbind(obs, temp)
}

AR <- exp(obs[,1])
LL <- exp(obs[,2])
R <- exp(obs[,3])
A <- AR - R

data <- data.frame(A, R, LL, LMA,LMAs,LMAp,
  DE = c(rep("D", n_dec), rep("E", n_ev), rep("U", n_u)))


# p <- ggplot(data, aes(colour = DE))
# p <- p + geom_point(aes(x = A, y = LL)) + facet_wrap(~ DE)
# p <- p + scale_y_log10()+ scale_x_log10()
# p
#
# p <- ggplot(data, aes(colour = DE))
# p <- p + geom_point(aes(x = LMA, y = A))
# p <- p + scale_y_log10()+ scale_x_log10()
# p

cor.test(log(LMAp),log(A))

cor.test(log(LL),log(A))
cor.test(log(LL),log(R))


# check how data looks like
par(mfrow=c(1,2))
plot(LL ~ A, log = "xy")
plot(LL ~ R, log = "xy")
par(mfrow=c(1,1))

# p <- ggplot(data, aes(colour = DE))
# p <- p + geom_point(aes(x = LMA, y = LL))
# p <- p + scale_y_log10()+ scale_x_log10()
# p
#
# p <- ggplot(data, aes(colour = DE))
# p <- p + geom_point(aes(x = LMAs, y = LL))
# p <- p + scale_y_log10()+ scale_x_log10()
# p

data2 <- data %>% filter(DE != "U")
par(mfrow=c(3,3))
plot(A ~ LMA, data, log = "xy")
plot(A ~ LMAp, data, log = "xy")
plot(A ~ LMAs, data, log = "xy")
plot(R~ LMA, data, log = "xy")
plot(R ~ LMAp, data, log = "xy")
plot(R ~ LMAs, data, log = "xy")
plot(LL ~ LMA, data, log = "xy")
plot(LL ~ LMAp, data, log = "xy")
plot(LL ~ LMAs, data, log = "xy")
par(mfrow=c(1,1))

# cor.test(log(LMA),log(A))
# cor.test(log(LMAp),log(A))
# par(mfrow=c(1,3))
# boxplot(LMA ~ DE, log = "y", data)
# boxplot(LMAp ~ DE, log = "y", data)
# boxplot(LMAs ~ DE, log = "y", data)


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

#using z pot
model_com_pot <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    vector<lower=0>[n_sample] A;
    vector<lower=0>[n_sample] R;
    row_vector[3] obs[n_sample];
  }
  parameters{
    real log_alpha;
    real log_beta;
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
    real<lower=0> mu3;
    vector<lower=0, upper=1.0>[n_sample] p;
    p <- 1 ./ (1 + exp(-z)); // vector
    for (i in 1:n_sample){
      mu[i, 1] <- log_alpha + log(LMA[i]) + log(p[i])
             - 0.5 * square(L_sigma[1]);
      mu[i,2] <- log_beta + log(LMA[i]) + log(1 - p[i])
            - 0.5 * square(L_sigma[2]);
      mu3 <- rp * LMA[i] * p[i] + rs * LMA[i] * (1 - p[i]);
      mu[i, 3]  <- log(mu3) - 0.5 * square(L_sigma[3]);
    }

  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega)); // vector
    z ~ normal(mu_z, sigma_z);
    log_alpha ~ cauchy(0, 5);
    log_beta ~ cauchy(0, 5);
    rp ~ cauchy(0, 5); //
    rs ~ cauchy(0, 5);
    mu_z ~ cauchy(0, 5); // vector
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
    Sigma <- diag_pre_multiply(L_sigma, L_Omega)
       * diag_post_multiply(L_Omega', L_sigma);
    rho12 <- Sigma[1, 2] * inv(L_sigma[1] * L_sigma[2]);
    rho23 <- Sigma[2, 3] * inv(L_sigma[2] * L_sigma[3]);
    rho13 <- Sigma[1, 3] * inv(L_sigma[1] * L_sigma[3]);
    for (i in 1:n_sample)
     log_lik[i] <- multi_normal_cholesky_log(obs[i], mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"

#using z pot
model_dif_pot <- "
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
    real log_beta;
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
    real<lower=0> mu3;
    vector<lower=0, upper=1.0>[n_sample] p;
    p <- 1 ./ (1 + exp(-z)); // vector
    for (i in 1:n_sample){
      mu[i, 1] <- log_alpha + log(LMA[i]) + log(p[i])
             - 0.5 * square(L_sigma[1]);
      mu[i,2] <- log_beta + log(LMA[i]) + log(1 - p[i])
            - 0.5 * square(L_sigma[2]);
      mu3 <- rp * LMA[i] * p[i] + rs * LMA[i] * (1 - p[i]);
      mu[i, 3]  <- log(mu3) - 0.5 * square(L_sigma[3]);
    }

  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega)); // vector
    for (i in 1:n_sample) z[i] ~ normal(mu_z + u[DE[i]], sigma_z);
    u ~ normal(0, sigma_DE); // vec
    log_alpha ~ cauchy(0, 5);
    log_beta ~ cauchy(0, 5);
    rp ~ cauchy(0, 5); //
    rs ~ cauchy(0, 5);
    mu_z ~ cauchy(0, 5); // vector
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

# obs_null <- obs
#
# obs_null[,2] <- sample(obs_null[,2])
#
# LMA_null <- sample(LMA)



list.data1 <- list(n_sample = n_sample,
          obs = obs,
          LMA = LMA,
          A = A,
          R = R,
          DE = DE)

system.time(fit1 <- stan(model_code = model_unif_pot,
            data = list.data1,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(fit2 <- stan(model_code = model_com_pot,
            data = list.data1,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(fit3 <- stan(model_code = model_dif_pot,
            data = list.data1,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))


system.time(res1 <- stan(fit = fit1,
           data = list.data1,
           iter = n.iter,
           warmup = n.warm,
           thin = n.thin,
           chains = n.chains,
           control = list(stepsize = 0.01, adapt_delta = 0.99,
              max_treedepth = 10)))

system.time(res2 <- stan(fit = fit2,
           data = list.data1,
           iter = n.iter,
           warmup = n.warm,
           thin = n.thin,
           chains = n.chains,
           control = list(stepsize = 0.01, adapt_delta = 0.99,
              max_treedepth = 10)))

system.time(res3 <- stan(fit = fit3,
           data = list.data1,
           iter = n.iter,
           warmup = n.warm,
           thin = n.thin,
           chains = n.chains,
           control = list(stepsize = 0.01, adapt_delta = 0.99,
              max_treedepth = 10)))


m1 <- data.frame(summary(res1)$summary)
m2 <- data.frame(summary(res2)$summary)
m3 <- data.frame(summary(res3)$summary)


# save.image("~/Dropbox/LES/negative_cov_model2.RData")

save.image("~/Dropbox/LES/GL_sim.RData")
