rm(list = ls()) # This clears everything from memory.

library(rstan)
# set_cppo('debug') # make debug easier
# set_cppo('fast') # make debug easier
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
options(mc.cores = 3)

# setwd("~/Dropbox/LES/")
setwd("./data")
d <- read.csv("nature02403-s2.csv",skip=10)

data <- data.frame(sp = d[,"Species"],
         DE = d[,"Decid.E.green"],
         GF = d[,"GF"],
         BIOME = d[,"BIOME"],
         LL = 10^d[,"log.LL"],
         LMA = 10^d[,"log.LMA"],
         Aarea = 10^d[,"log.Aarea"],
         Rarea = 10^d[,"log.Rdarea"],
         NB = d[,"Needle.Broad.lf"])

data <- na.omit(data)
rownames(data)<-NULL

data$sp <- as.factor(as.character(data$sp))

##each sample corresponds to each species
data2 <- data.frame(LL = tapply(data$LL, data$sp,mean),
          LMA = tapply(data$LMA, data$sp,mean),
          Aarea = tapply(data$Aarea, data$sp,mean),
          Rarea = tapply(data$Rarea, data$sp,mean))
data2$sp <- rownames(data2)

data3 <- data[,1:4]
data3 <- unique(data3)

data4 <- merge(data3,data2,by="sp")

#observed data
#one sample = one species
data.obs <- na.omit(data4)
rownames(data.obs)<-NULL


#potential LL
model1 <- "
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
    vector<lower=0.001, upper=0.999>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    real<lower=0> mu3;
    for (i in 1:n_sample){
      mu[i,1] = log_alpha + log(LMA[i]) + log(p[i])
             - 0.5 * square(L_sigma[1]);
      mu[i,2] = log_beta + log(LMA[i]) + log(1-p[i])
             - 0.5 * square(L_sigma[2]);
      mu3 = rp * LMA[i] * p[i] + rs * LMA[i] * (1 - p[i]);
      mu[i,3]  = log(mu3) - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega));
    p ~ uniform(0.001, 0.999);
    log_alpha ~ normal(0, 1.0e+4);
    log_beta ~ normal(0, 1.0e+4);
    rp ~ normal(0, 1.0e+4);
    rs ~ normal(0, 1.0e+4);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
    L_sigma ~ uniform(0, 1.0e+4);
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
     log_lik[i] = multi_normal_cholesky_log(obs[i], mu[i], diag_pre_multiply(L_sigma, L_Omega));
   }
"

model1r <- "
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
    real rp;
    real rs;
    real rp2;
    real rs2;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector<lower=0, upper=1>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    real<lower=0> mu3;

    for (i in 1:n_sample){
      mu[i,1] = log_alpha + alpha2 * (log(LMA[i]) + log(p[i]))
             - 0.5 * square(L_sigma[1]);
      mu[i,2] = log_beta + beta2 * (log(LMA[i]) + log(1-p[i]))
             - 0.5 * square(L_sigma[2]);
      mu3 = rp * pow(LMA[i] * p[i], rp2) + rs * pow(LMA[i] * (1 - p[i]), rs2);
      mu[i, 3]  = log(mu3) - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega));
    p ~ uniform(0, 1);
    log_alpha ~ cauchy(0, 5);
    log_beta ~ cauchy(0, 5);
    alpha2 ~ cauchy(0,5);
    beta2 ~ cauchy(0,5);
    rp ~ cauchy(0,5);
    rs ~ cauchy(0,5);
    rp2 ~ cauchy(0,5);
    rs2 ~ cauchy(0,5);
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
model2r <- "
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
    real rp;
    real rs;
    real rp2;
    real rs2;
    vector<lower=0>[3] L_sigma;
    cholesky_factor_corr[3] L_Omega;
    vector<lower=0, upper=1>[n_sample] p;
  }
  transformed parameters{
    row_vector[3] mu[n_sample];
    real<lower=0> mu3;

    for (i in 1:n_sample){
      mu[i,1] = log_alpha + alpha2 * (log(LMA[i]) + log(p[i]))
             - 0.5 * square(L_sigma[1]);
      mu[i,2] = log_beta + beta2 * (log(LMA[i]) + log(1-p[i]))
             - 0.5 * square(L_sigma[2]);
      mu3 = rp * pow(LMA[i] * p[i], rp2) + rs * pow(LMA[i] * (1 - p[i]), rs2);
      mu[i, 3]  = log(mu3) - 0.5 * square(L_sigma[3]);
    }
  }
  model{
    obs ~ multi_normal_cholesky(mu, diag_pre_multiply(L_sigma, L_Omega));
    p ~ uniform(0, 1);
    log_alpha ~ normal(0, 1.0e+4);
    log_beta ~ normal(0, 1.0e+4);
    alpha2 ~ normal(0,1.0e+4);
    beta2 ~ normal(0,1.0e+4);
    rp ~ normal(0,1.0e+4);
    rs ~ normal(0,1.0e+4);
    rp2 ~ normal(0,1.0e+4);
    rs2 ~ normal(0,1.0e+4);
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega'
    L_sigma ~ cauchy(0, 1.0e+4);
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

list.data <- list(n_sample = nrow(data),
          obs = cbind(log(data[ , "Aarea"] + data[ , "Rarea"]),
                log(data[ , "LL"]),
                log(data[ , "Rarea"])),
          LMA = data[ , "LMA"],
          DE = as.numeric(data$DE))


n.iter <- 2000
n.warm <- 1000
n.thin <- 2
n.chains <- 3


system.time(fit <- stan(model_code = model1r,
            data = list.data,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))


system.time(fit2 <- stan(model_code = model2r,
            data = list.data,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(res <- stan(fit = fit,
             data = list.data,
             iter = n.iter,
             warmup = n.warm,
             thin = n.thin,
             chains = n.chains,
             control = list(stepsize = 0.01,
                   adapt_delta = 0.9,
                   max_treedepth = 15)))

system.time(res2 <- stan(fit = fit2,
             data = list.data,
             iter = n.iter,
             warmup = n.warm,
             thin = n.thin,
             chains = n.chains,
             control = list(stepsize = 0.01,
                   adapt_delta = 0.9,
                   max_treedepth = 15)))

fit.summary <- data.frame(summary(res)$summary)
fit.summary2 <- data.frame(summary(res2)$summary)

n.chains <- 3
n.iter <- 20000
n.warm <- 10000
n.thin <- 20

fit.summary1 <- list()
res1 <- list()

LMA.ra.data <- NULL

for (i in 1:10){
  data.n <- data.frame(data.obs[1:5],
             LMA = sample(data.obs$LMA,nrow(data.obs)),
             Aarea = data.obs$Aarea,
             Rarea = data.obs$Rarea)

  LMA.ra.data <- cbind(LMA.ra.data,data.n$LMA)

  list.data<-list(n_sample = nrow(data.n),
                  obs = cbind(log(data.n[,"Aarea"] + data.n[,"Rarea"]),
                        log(data.n[,"LL"]),
                        log(data.n[,"Rarea"])),
                  LMA = data.n[,"LMA"])

  system.time(fit1<- stan(model_code = model1,
              data = list.data,
              iter = 1,
              warmup = 0,
              thin = 1,
              chains = 1))

  system.time(res1[[i]] <- stan(fit = fit1,
              data=list.data,
              iter = n.iter,
              warmup = n.warm,
              thin = n.thin,
              chains = n.chains))

  fit.summary1[[i]] <- data.frame(summary(res1[[i]])$summary)
}

save.image(paste("GL_rand_across", Sys.Date(), ".RData", sep = "_"))
