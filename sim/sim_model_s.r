library(tidyverse)
library(rstan)
library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = 3)
print(parallel::detectCores())

set.seed(5)
source("./sim/sim_data_s.r")
print("load sim_data_s.r")

n_chains <- 3

argv <- commandArgs(trailingOnly = TRUE)
n_model <- argv[1]
n_model2 <- paste0("./model/", n_model, ".stan")
data_name <- argv[2]
n_iter <- as.numeric(argv[3])
n_warm <- as.numeric(argv[4])
n_thin <- as.numeric(argv[5])
obs <- argv[6]

#n_model <- noquote("potLL")
#n_model <- "potPL"
#n_model <- "site_diff"
#n_model2 <- paste0("./model/", n_model, ".stan")
#
#data_name <- "PA"
#n_iter <- 200
#n_warm <- 100
#n_thin <- 1
#obs <- "obs"

print(paste("Model:", n_model))
print(paste("Model dir:", n_model2))
print(paste("Data:", data_name))
print(paste("n_iter =", n_iter))
print(paste("n_warm =", n_warm))
print(paste("n_thin =", n_thin))
print(paste("n_chains =", n_chains))

if (data_name == "GL1") {
  dat <- GL_sim_dat %>% as.data.frame
  list_dat <- list(n_sample = nrow(dat),
            A = dat[ , "Aarea"],
            LL = dat[ , "LL"],
            R = dat[ , "Rarea"],
            q_lim = 1,
            leaf = 1,
            dry = 1,
            DE = as.numeric(as.factor(dat[,"gr"])),
            LMA = dat[ , "LMA"])
  
} else if (data_name == "GL2") {
  dat <- GL_sim_dat2 %>% as.data.frame
  list_dat <- list(n_sample = nrow(dat),
            A = dat[ , "Aarea"],
            LL = dat[ , "LL"],
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
          LMA = dat[ , "LMA"],
          A = dat[ , "Aarea"],
          LL = dat[ , "LL"],
          R = dat[ , "Rarea"],
          q_lim = q_lim,
          DE = as.numeric(as.factor(dat[,"gr"])),
          leaf = as.numeric(as.factor(dat$strata)),
          dry = ifelse(dat$site == "Dry", 1 , 0))

} else if (data_name == "WC") {
  dat <- WC_sim_dat %>% as.data.frame
  list_dat <- list(n_sample = nrow(dat),
            A = dat[ , "Aarea"],
            LL = dat[ , "LL"],
            R = dat[ , "Rarea"],
            q_lim = 1,
            leaf = 1,
            dry = 1,
            DE = 1,
            LMA = dat[ , "LMA"])
}


rand_dat <- data_frame(null_model = 1:10)

rand_fun <- function(n){
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

  temp$A_R <- temp$A - temp$R

  list(n_sample = list_dat$n_sample,
            A = temp$Aarea,
            LL = temp$LL,
            R = temp$Rarea,
            q_lim = list_dat$q_lim,
            leaf = list_dat$leaf,
            dry = list_dat$dry,
            DE = list_dat$DE,
            LMA = list_dat$LMA)
}

rand_dat <- rand_dat %>%
  mutate(data = map(1:10, rand_fun))

if (obs == "obs") {
  system.time(fit <- stan(file = n_model2,
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
             control = list(adapt_delta = 0.99, max_treedepth = 10)))

  save_name <- paste("./data/", data_name, "_", n_model, "_sim.rda", sep = "")
}

if (obs == "rand") {
 
  # compile  
  system.time(fit <- stan(file = n_model2,
              data = rand_dat$data[[1]],
              iter = 1,
              warmup = 0,
              thin = 1,
              chains = 1))

  mod_fun <- function(data){
    res <- stan(fit = fit,
            data = data,
            iter = n_iter,
            warmup = n_warm,
            thin = n_thin,
            chains = n_chains,
            control = list(adapt_delta = 0.99, max_treedepth = 10))
  res
  }
  
  summary_fun <- function(model) {
     data.frame(summary(model)$summary)
  }

  rand_res <- rand_dat %>%
    mutate(model = map(data, mod_fun)) %>%
    mutate(summary = map(model, summary_fun)) 

  save_name <- paste("./data/", data_name, "_", n_model, "_sim_r.rda", sep = "")
}

save.image(save_name)
