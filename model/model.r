library(tidyverse)
library(rstan)
library(stringr)
library(loo)
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
options(mc.cores = 5)

set.seed(123)

n_chains <- 4
n_rand <- 10
argv <- commandArgs(trailingOnly = TRUE)
n_model <- argv[1]
n_model2 <- paste0("./model/", n_model, ".stan")
data_name <- argv[2]
n_iter <- as.numeric(argv[3])
n_warm <- as.numeric(argv[4])
n_thin <- as.numeric(argv[5])
obs <- argv[6]

#n_model <- noquote("potLL")
#n_model <- noquote("sitePL2")
#n_model <- noquote("LMAm_LMAsLT2")
#n_model2 <- paste0("./model/", n_model, ".stan")
#data_name <- "GL"
#n_iter <- 200
#n_warm <- 100
#n_thin <- 1
#obs <- "rand"

print(paste("Model:", n_model))
print(paste("Model dir:", n_model2))
print(paste("Data:", data_name))
print(paste("n_iter =", n_iter))
print(paste("n_warm =", n_warm))
print(paste("n_thin =", n_thin))
print(paste("n_chains =", n_chains))
print(str_glue("{str_to_upper(obs)} data"))

if (data_name == "GL") {
  dat <- read_csv("./data/GL_data.csv") %>%
    mutate(DE = ifelse(is.na(DE), "U", DE)) %>%
    as.data.frame

  list_dat <- list(N = nrow(dat),
                   obs = cbind(log(dat[ , "Aarea"] + dat[ , "Rarea"]),
                               log(dat[ , "LL"]),
                               log(dat[ , "Rarea"])),
                   LMA = dat[ , "LMA"],
                   A = dat[ , "Aarea"],
                   LL = dat[ , "LL"],
                   R = dat[ , "Rarea"],
                   q_lim = 1,
                   leaf = 1,
                   dry = 1,
                   gr = as.numeric(as.factor(dat$DE))) 

  list_dat$J <- list_dat$gr %>% unique %>% length

  list_dat$obs2 <- list_dat$obs
  list_dat$E <- ifelse(dat$DE == "E", 1, 0)
  list_dat$U <- ifelse(dat$DE == "U", 1, 0)

} else if (data_name == "PA") {

  dat <- read_csv("./data/PA_data.csv") %>%
    filter(!is.na(LMA)) %>%
    filter(!is.na(Aarea)) %>%
    filter(!is.na(Rarea)) %>%
    filter(!is.na(LL)) %>%
    as.data.frame %>%
    mutate(gr = paste(site, strata) %>% 
           as.factor %>%
           as.numeric)

  q_lim <- dat %>%
    filter(strata == "UNDER") %>%
    mutate(q = Rarea/Aarea) %>%
    summarize(max(q)) %>%
    as.numeric()

  list_dat <- list(N = nrow(dat),
                   obs = cbind(log(dat[ , "Aarea"] + dat[ , "Rarea"]),
                               log(dat[ , "LL"]),
                               log(dat[ , "Rarea"])),
                   LMA = dat[ , "LMA"],
                   LT = dat[ , "LT"],
                   A = dat[ , "Aarea"],
                   LL = dat[ , "LL"],
                   R = dat[ , "Rarea"],
                   DE = 1,
                   gr = dat$gr,
                   J = dat$gr %>% unique %>% length,
                   q_lim = q_lim,
                   #leaf = as.numeric(as.factor(dat$strata)),
                   leaf = ifelse(dat$strata == "CAN", 1, 0),
                   dry = ifelse(dat$site == "PNM", 1 , 0))
}

rand_dat <- tibble(null_model = 1:n_rand)

rand_fun <- function(n){
  temp <- data.frame(dat[,1:3],
           LMA = sample(dat$LMA),
           LL = sample(dat$LL),
           Aarea = sample(dat$Aarea),
           Rarea = sample(dat$Rarea),
           LT = sample(dat$Rarea)
           ) 
  while (min(temp$Aarea - temp$Rarea) < 0){
  temp <- data.frame(dat[,1:3],
           LMA = sample(dat$LMA),
           LL = sample(dat$LL),
           Aarea = sample(dat$Aarea),
           Rarea = sample(dat$Rarea),
           LT = sample(dat$Rarea)
           ) 
  }

  temp$A_R <- temp$A - temp$R

  list(N = list_dat$N,
            A = temp$Aarea,
            LL = temp$LL,
            R = temp$Rarea,
            LT = temp$LT,
            q_lim = list_dat$q_lim,
            leaf = list_dat$leaf,
            dry = list_dat$dry,
            DE = list_dat$DE,
            gr = list_dat$gr,
            J = list_dat$J,
            LMA = temp$LMA)
}

rand_dat <- rand_dat %>%
  mutate(data = map(1:n_rand, rand_fun))

# setwd("~/Dropbox/LES/")
if (obs == "obs") {

  #system.time(fit <- stan(file = n_model2,
  #            data = list_dat,
  #            iter = 1,
  #            warmup = 0,
  #            thin = 1,
  #            chains = 1))

  system.time(res <- stan(file = n_model2,
             data = list_dat,
             iter = n_iter,
             warmup = n_warm,
             thin = n_thin,
             chains = n_chains,
             seed = 123,
             control = list(adapt_delta = 0.99, max_treedepth = 20)))

  save_name <- paste("./rda/", n_model, "_obs.rda", sep = "")
}

if (obs == "rand") {

  #compile
  #system.time(fit <- stan(file = n_model2,
  #            data = rand_dat$data[[1]],
  #            iter = 1,
  #            warmup = 0,
  #            thin = 1,
  #            chains = 1))

  mod_fun <- function(data){
    res <- stan(file = n_model2,
            data = data,
            iter = n_iter,
            warmup = n_warm,
            thin = n_thin,
            chains = n_chains,
            control = list(adapt_delta = 0.99, max_treedepth = 20))
    res
  }
  
  summary_fun <- function(model) {
     data.frame(summary(model)$summary)
  }

  rand_res <- rand_dat %>%
    mutate(model = map(data, mod_fun)) %>%
    mutate(summary = map(model, summary_fun)) 

  save_name <- paste("./rda/", n_model, "_rand.rda", sep = "")
}

save.image(save_name)
print(str_glue("Saved `{save_name}`!!"))
