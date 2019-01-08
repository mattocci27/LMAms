# Stan codes for LMAp/LMAs model
# Mastoshi Katabuchi
# mattocci27@gmail.com
# June 30, 2016
#
# Weakly informaive pirors were used for speed up,
# which yields the almost identical results with non-infomrative priors.

# Simluated GLOPNET data -------------------------------------------------
rm(list = ls()) # This clears everything from memory.
library(rstan)
library(dplyr)
library(mvtnorm)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# paramter setting ------------------------------------------------------
set.seed(10)
n_dec <- 7 # number of deciduous species
n_ev <- 70 # number of evergreen species
n_u <- 23 # number of unknown species
n_sample <- n_dec + n_ev + n_u
LMA1 <- rlnorm(n_dec, log(80) - 0.5 * 0.31^2, 0.31) # dec
LMA2 <- rlnorm(n_ev, log(170) - 0.5 * 0.50^2, 0.50) # ev
LMA3 <- rlnorm(n_u, log(80) - 0.5 * 0.51^2, 0.51) # unknown

LMA <- c(LMA1, LMA2, LMA3)

# f is assummed to have beta distirubion
# Negative covariance between f and LMA in evergreen
phi <- 0.8 + log(LMA2) * -0.09
lambda <- 3.2

f1 <- rbeta(n_dec, 19, 10) #
f2 <- NULL
for (i in 1:n_ev) f2[i] <- rbeta(1, phi[i] * lambda, lambda*(1-phi[i]))
f3 <- rbeta(n_u, 6.8, 2.6)
f <- c(f1, f2, f3)


# check how f look like
par(mfrow=c(2,2))
hist(f1)
hist(f2)
hist(f3)
par(mfrow=c(1,1))


# paramter setting ------------------------------------------------------
LMAp <- LMA * f
LMAs <- LMA * (1-f)
sigma1 <- 0.5
sigma2 <- 0.5
sigma3 <- 0.5
rho12 <- 0
rho13 <- 0.7
rho23 <- 0
log_alpha <- -1.46
log_beta <- -1.47
rp <- 0.02
rs <- 0.001

Sigma <- matrix(c(sigma1^2, rho12*sigma1*sigma2, rho13*sigma1*sigma3,
    rho12*sigma1*sigma2, sigma2^2, rho23*sigma2*sigma3,
    rho13*sigma1*sigma3, rho23*sigma2*sigma3, sigma3^2), ncol =3)

DE <- rep(1:3, each = 30)
DE <- c(rep(1,n_dec), rep(2, n_ev), rep(3, n_u))

# leaf data -------------------------------------------------------------
obs <- NULL
log_mu2 <- NULL
for (i in 1:n_sample){
  temp <- rmvnorm(1,
    mean = c(log_alpha + log(LMA[i]) + log(f[i]) - 0.5 * sigma1^2,
        log_beta + log(LMA[i]) + log(1 - f[i]) - 0.5 * sigma2^2,
        log(rp * LMA[i] * f[i] + rs * LMA[i] * ( 1- f[i])) - 0.5 * sigma3^2),
    sigma = Sigma
    )
  obs <- rbind(obs, temp)
}

AR <- exp(obs[,1])
LL <- exp(obs[,2])
R <- exp(obs[,3])
A <- AR - R

# simluated GLOPNET1
data <- data.frame(A, R, LL, LMA,LMAs,LMAp,
  DE = c(rep("D", n_dec), rep("E", n_ev), rep("U", n_u)))


# check how data looks like ---------------------------------------------
fig_dat <- data %>%
  tidyr::gather("LMA_type", "LMA", 4:6) %>%
  tidyr::gather("trait_type", "trait", 1:3) %>%
  mutate(trait_type2 = factor(trait_type,
    levels = c("A", "R", "LL"))) %>%
  mutate(DE2 = ifelse(DE == "D", "Deciduous",
        ifelse(DE == "E", "Evergreen", "Unclassified")))

p <- ggplot(fig_dat, aes(x = LMA, y = trait, fill = DE2))
p <- p + geom_point(colour = "black", shape = 21) + facet_grid(trait_type2 ~ LMA_type, scales = "free")
p <- p + scale_y_log10() + scale_x_log10() +  theme_bw() + ylab("Simulated traits") + xlab("Simluated LMA") + guides(colour = guide_legend(title = NULL))
p <- p + scale_fill_manual(values = c("dark orange", "green4", "white"))
p <- p + guides(fill = guide_legend(title = NULL))
print(p)

# Stan codes  ---------------------------------------------------------------

# Cauchy or half-cauhy priors were used for speed up.
# Some paratemeters were vectorized for speed up.

# Non-hierarchal model
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
    rp ~ cauchy(0, 5);
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

# Hierarchal model
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
    rp ~ cauchy(0, 5);
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

# R codes to run stan -------------------------------------------------------
n.iter <- 20000
n.warm <- 10000
n.thin <- 20
n.chains <- 3

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

system.time(fit2 <- stan(model_code = model_dif_pot,
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

# summary tables of the results
m1 <- data.frame(summary(res1)$summary)
m2 <- data.frame(summary(res2)$summary)


# End ---------------------------------------------------------------------
save.image("/scratch/lfs/mattocci27/LES/GL_sim1_20160630.RData")
