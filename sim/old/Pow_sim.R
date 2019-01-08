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
LMA3 <- rlnorm(25, log(40) - 0.5 * 0.18^2, 0.18) # shade wet
LMA4 <- rlnorm(25, log(30) - 0.5 * 0.36^2, 0.36) #shade dry

LMA <- c(LMA1, LMA2, LMA3, LMA4)
f1 <- rbeta(25, 6, 4)
f2 <- rbeta(25, 9, 4)
f3 <- rbeta(25, 3.5, 6)
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
plot(LMAs ~ LMAp, log = "xy")

sigma1 <- 0.3
sigma2 <- 0.5
sigma3 <- 0.7
rho12 <- 0.8
rho13 <- 0
rho23 <- 0
log_alpha <- 0.7
alpha2 <- 0.5
log_beta <- -2
beta2 <- 1.2
log_r <- -2.2
rp <- 0.6
rs <- 0
q <- 0.3
log_site <- log(0.7)

Sigma <- matrix(c(sigma1^2, rho12*sigma1*sigma2, rho13*sigma1*sigma3,
    rho12*sigma1*sigma2, sigma2^2, rho23*sigma2*sigma3,
    rho13*sigma1*sigma3, rho23*sigma2*sigma3, sigma3^2), ncol =3)

ss = rep(1:4, each = 25)
dry = rep(c(0,1,0,1), each = 25)
leaf = rep(1:2, each = 50)

# leaf data -------------------------------------------------------------
obs <- NULL
log_mu2 <- NULL
for (i in 1:100){
  if (leaf[i] == 2){ # shade
  log_mu2[i] <- log_beta + log_site * dry[i] + 0.5 * (beta2 + 1) *  log(LMA[i]) + beta2 * log(1-f[i]) - 2 * log(f[i])- 0.5 * sigma2^2
  } else{ # sun
  log_mu2[i] <- log_beta  - log(q) + log_site * dry[i] + 0.5 * (beta2 + 1) *  log(LMA[i]) + beta2 * log(1-f[i]) - 2 * log(f[i])- 0.5 * sigma2^2
  }

  temp <- rmvnorm(1,
    mean = c(log_alpha + alpha2 * (log(LMA[i]) + log(f[i]))　- 0.5 * sigma1^2,
        log_mu2[i],
        log_r + rp * log(LMAp[i]) + rs * log(LMAs[i]) - 0.5 * sigma3^2),
    sigma = Sigma
    )
  obs <- rbind(obs, temp)
}

AR <- exp(obs[,1])
LL <- exp(obs[,2])
R <- exp(obs[,3])
A <- AR - R

data <- data.frame(A, R, LL, LMA,LMAs,LMAp,
  ss = rep(c("sun_wet","sun_dry","shade_wet","shade_dry"), each =25),
  leaf = rep(c("sun", "shade"), each =50))

# check how data looks like
cor.test(log(LL),log(A))
par(mfrow=c(1,2))
plot(LL ~ A, log = "xy")
plot(LL ~ R, log = "xy")
par(mfrow=c(1,1))

p <- ggplot(data, aes(colour = ss))
p <- p + geom_point(aes(x = LMA, y = LL))
p <- p + scale_y_log10()+ scale_x_log10()
p

p <- ggplot(data, aes(colour = ss))
p <- p + geom_point(aes(x = LMAs, y = LL))
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

# plot(exp(log_mu2), LL, log = "xy")

# models -------------------------------------------------------------------

model2prp <- "
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
    real alpha2;
    real beta2;
    real log_r;
    real log_site;
    real rp;
    real rs;
    real<lower=q_lim, upper=1> q;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector<lower=0.0, upper=1.0>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    for (i in 1:n_sample){
      mu[i, 1] <- log_alpha + alpha2 * (log(LMA[i]) + log(p[i]))
             - 0.5 * square(L_sigma[1]);
       if (leaf[i]==1) // sun
         mu[i,2] <- log_beta - log(q) + log_site * dry[i]
             + 0.5 * (beta2 + 1) *  log(LMA[i])
             + beta2 * log(1-p[i])
             - 1 * log(p[i])
             - 0.5 *  square(L_sigma[2]);
       else
         mu[i,2] <- log_beta + log_site * dry[i]
             + 0.5 * (beta2 + 1) *  log(LMA[i])
             + beta2 * log(1-p[i])
             - 1 * log(p[i])
             - 0.5 *  square(L_sigma[2]);
        mu[i, 3]  <- log_r +  rp * (log(LMA[i]) + log(p[i]))
            + rs * (log(LMA[i]) + log(1-p[i]))
             - 0.5 * square(L_sigma[3]);
    }

  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega)); // vector
    p ~ uniform(0, 1);
    log_alpha ~ cauchy(0, 5);
    log_beta ~ cauchy(0, 5);
    log_site ~ cauchy(0, 5);
    alpha2 ~ cauchy(0, 5); //
    beta2 ~ cauchy(0, 5); //
    log_r ~ cauchy(0, 5); //
    rp ~ cauchy(0, 5); //
    rs ~ cauchy(0, 5);
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
#using z opt
model2pzrp <- "
  data{
    int<lower=0> n_sample;
    vector<lower=0>[n_sample] LMA;
    vector<lower=0>[n_sample] A;
    vector<lower=0>[n_sample] R;
    real<lower=0> q_lim;
    row_vector[3] obs[n_sample];
    int<lower=0> ss[n_sample];
    int<lower=0> leaf[n_sample];
    int<lower=0> dry[n_sample];
  }
  parameters{
    real log_alpha;
    real alpha2;
    real log_beta;
    real beta2;
    real log_r;
    real log_site;
    real rp;
    real rs;
    real mu_z;
    real<lower=q_lim, upper=1> q;
    real<lower=0> sigma_z;
    real<lower=0> sigma_ss;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector[4] u;
    vector[n_sample] z;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    vector<lower=0, upper=1.0>[n_sample] p;
    p <- 1 ./ (1 + exp(-z)); // vector
    for (i in 1:n_sample){
      mu[i, 1] <- log_alpha + alpha2 * (log(LMA[i]) + log(p[i]))
             - 0.5 * square(L_sigma[1]);
       if (leaf[i]==1) // sun
         mu[i,2] <- log_beta - log(q) + log_site * dry[i]
             + 0.5 * (beta2 + 1) *  log(LMA[i])
             + beta2 * log(1-p[i])
             - 1 * log(p[i])
             - 0.5 *  square(L_sigma[2]);
       else
         mu[i,2] <- log_beta + log_site * dry[i]
             + 0.5 * (beta2 + 1) *  log(LMA[i])
             + beta2 * log(1-p[i])
             - 1 * log(p[i])
             - 0.5 *  square(L_sigma[2]);
      mu[i, 3]  <- log_r +  rp * (log(LMA[i]) + log(p[i]))
      + rs * (log(LMA[i]) + log(1-p[i]))
       - 0.5 * square(L_sigma[3]);
    }

  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega)); // vector
    for (i in 1:n_sample) z[i] ~ normal(mu_z + u[ss[i]], sigma_z);
    u ~ normal(0, sigma_ss); // vec
    log_alpha ~ cauchy(0, 5);
    alpha2 ~ cauchy(0, 5);
    log_beta ~ cauchy(0, 5);
    log_site ~ cauchy(0, 5);
    beta2 ~ cauchy(0, 5);
    log_r ~ cauchy(0, 5); //
    rp ~ cauchy(0, 5); //
    rs ~ cauchy(0, 5);
    mu_z ~ cauchy(0, 5); // vector
    sigma_z ~ cauchy(0, 5);
    sigma_ss ~ cauchy(0, 5);
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



n.iter <- 1000
n.warm <- 500
n.thin <- 1
n.chains <- 3

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


list.data1 <- list(n_sample = 100,
          obs = obs,
          LMA = LMA,
          A = A,
          R = R,
          ss = ss,
          leaf = leaf,
          dry = dry,
          q_lim = q_lim)

system.time(fit2prp <- stan(model_code = model2prp,
            data = list.data1,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(fit2pzrp <- stan(model_code = model2pzrp,
            data = list.data1,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))


system.time(res2prp <- stan(fit = fit2prp,
           data = list.data1,
           iter = n.iter,
           warmup = n.warm,
           thin = n.thin,
           chains = n.chains,
           control = list(stepsize = 0.01, adapt_delta = 0.8,
              max_treedepth = 10)))

system.time(res2pzrp <- stan(fit = fit2pzrp,
           data = list.data1,
           iter = n.iter,
           warmup = n.warm,
           thin = n.thin,
           chains = n.chains,
           control = list(stepsize = 0.01, adapt_delta = 0.8,
              max_treedepth = 10)))

m2prp <- data.frame(summary(res2prp)$summary)
m2pzrp <- data.frame(summary(res2pzrp)$summary)


# save.image("~/Dropbox/LES/negative_cov_model2.RData")

save.image("~/Dropbox/LES/Pow_sim.RData")
