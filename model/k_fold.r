library(tidyverse)
library(rstan)
library(stringr)
library(loo)
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
options(mc.cores = 4)

set.seed(5)

n_chains <- 4
argv <- commandArgs(trailingOnly = TRUE)
n_model <- argv[1]
n_model2 <- paste0("./model/", n_model, ".stan")
data_name <- argv[2]
n_iter <- as.numeric(argv[3])
n_warm <- as.numeric(argv[4])
n_thin <- as.numeric(argv[5])
obs <- argv[6]

print(paste("Model:", n_model))
print(paste("Model dir:", n_model2))
print(paste("Data:", data_name))
print(paste("n_iter =", n_iter))
print(paste("n_warm =", n_warm))
print(paste("n_thin =", n_thin))
print(paste("n_chains =", n_chains))

if (data_name == "PA") {
  #dat <- read_csv("./data/PA_data_more.csv") %>%
  dat <- read_csv("./data/PA_data.csv") %>%
    filter(!is.na(LMA)) %>%
    filter(!is.na(Aarea)) %>%
    filter(!is.na(Rarea)) %>%
    filter(!is.na(LL)) %>%
    as.data.frame %>%
    mutate(gr = paste(site, strata) %>% 
           as.factor %>%
           as.numeric)


  list_dat <- list(N = nrow(dat),
                   obs = cbind(log(dat[ , "Aarea"] + dat[ , "Rarea"]),
                               log(dat[ , "LL"]),
                               log(dat[ , "Rarea"])),
                   LMA = dat[ , "LMA"],
                   A = dat[ , "Aarea"],
                   LL = dat[ , "LL"],
                   R = dat[ , "Rarea"],
                   LT = dat[ , "LT"],
                   DE = 1,
                   gr = dat$gr,
                   jj = dat$gr,
                   J = dat$gr %>% unique %>% length,
                   leaf = ifelse(dat$strata == "CAN", 1, 0),
                   dry = ifelse(dat$site == "PNM", 1 , 0))

} else if (data_name == "GL") {
  dat <- read_csv("./data/GL_data.csv") %>%
    mutate(DE = ifelse(is.na(DE), "U", DE)) %>%
    mutate(gr = DE %>% as.factor %>% as.numeric) %>%
    as.data.frame

  list_dat <- list(N = nrow(dat),
                   obs = cbind(log(dat[ , "Aarea"] + dat[ , "Rarea"]),
                               log(dat[ , "LL"]),
                               log(dat[ , "Rarea"])),
                   LMA = dat[ , "LMA"],
                   A = dat[ , "Aarea"],
                   LL = dat[ , "LL"],
                   R = dat[ , "Rarea"],
                   jj = dat$gr,
                   J = dat$gr %>% unique %>% length,
                   q_lim = 1,
                   leaf = 1,
                   dry = 1,
                   DE = as.numeric(as.factor(dat$DE)))

}

N <- nrow(dat)
n_fold <- 10

hh <- kfold_split_random(n_fold, N) #hh index the fold ID of each data point

holdout_10 <- matrix(0, nrow = N, ncol = n_fold)
for (i in 1:N) holdout_10[i, hh[i]] <- 1

#turn into a list
holdout_10 <- split(holdout_10, rep(1:ncol(holdout_10), each = nrow(holdout_10)))

data_l <- rep(list(list_dat),10)
#add the holdout index to it
for(i in 1:10) data_l[[i]]$holdout <- holdout_10[[i]]

#functions slightly modified from: https://github.com/stan-dev/stancon_talks/blob/master/2017/Contributed-Talks/07_nicenboim/kfold.Rmd

#function to parrallelize all computations
#need at least two chains !!!
stan_kfold <- function(file, list_of_datas, chains, cores,...){
  library(pbmcapply)
  badRhat <- 1.1 # don't know why we need this?
  n_fold <- length(list_of_datas)
  model <- stan_model(file=file)
  # First parallelize all chains:
  sflist <- 
    pbmclapply(1:(n_fold*chains), mc.cores = cores, 
               function(i){
                 # Fold number:
                 k <- ceiling(i / chains)
                 s <- sampling(model, data = list_of_datas[[k]], 
                               chains = 1, chain_id = i,...)
                 return(s)
               })

  # Then merge the K * chains to create K stanfits:
  stanfit <- list()
  for(k in 1:n_fold){
    inchains <- (chains*k - (chains - 1)):(chains*k)
  #  Merge `chains` of each fold
    stanfit[[k]] <- sflist2stanfit(sflist[inchains])
  }  
  return(stanfit) 
}

#extract log-likelihoods of held-out data
extract_log_lik_K <- function(list_of_stanfits, list_of_holdout, ...){
  require(loo)
  K <- length(list_of_stanfits)
  list_of_log_liks <- plyr::llply(1:K, function(k){
    extract_log_lik(list_of_stanfits[[k]],...)
  })
  # `log_lik_heldout` will include the loglike of all the held out data of all the folds.
  # We define `log_lik_heldout` as a (samples x N_obs) matrix
  # (similar to each log_lik matrix)
  log_lik_heldout <- list_of_log_liks[[1]] * NA
  for(k in 1:K){
    log_lik <- list_of_log_liks[[k]]
    samples <- dim(log_lik)[1] 
    N_obs <- dim(log_lik)[2]
    # This is a matrix with the same size as log_lik_heldout
    # with 1 if the data was held out in the fold k
    heldout <- matrix(rep(list_of_holdout[[k]], each = samples), nrow = samples)
    # Sanity check that the previous log_lik is not being overwritten:
    if(any(!is.na(log_lik_heldout[heldout==1]))){
      warning("Heldout log_lik has been overwritten!!!!")
    }
    # We save here the log_lik of the fold k in the matrix:
    log_lik_heldout[heldout==1] <- log_lik[heldout==1]
  }
  return(log_lik_heldout)
}

#compute ELPD
kfold <- function(log_lik_heldout)  {
  library(matrixStats)
  logColMeansExp <- function(x) {
    # should be more stable than log(colMeans(exp(x)))
    S <- nrow(x)
    colLogSumExps(x) - log(S)
  }
  # See equation (20) of @VehtariEtAl2016
  pointwise <-  matrix(logColMeansExp(log_lik_heldout), ncol= 1)
  colnames(pointwise) <- "elpd"
  # See equation (21) of @VehtariEtAl2016
  elpd_kfold <- sum(pointwise)
  se_elpd_kfold <-  sqrt(ncol(log_lik_heldout) * var(pointwise))
  out <- list(
    pointwise = pointwise,
    elpd_kfold = elpd_kfold,
    se_elpd_kfold = se_elpd_kfold)
  #structure(out, class = "loo")
  return(out)
}

#ss <- stan_kfold(file = "model/LMA_CV.stan",
ss <- stan_kfold(file = n_model2,
                 list_of_datas = data_l,
                 chains = n_chains,
                 iter = n_iter,
                 warmup = n_warm,
                 thin = 1,
                 cores = 4,
                 control = list(adapt_delta = 0.99, max_treedepth = 20))

save_name <- paste("./data/", n_model, "_obs_cv.rda", sep = "")
save.image(save_name)

#ss <- stan(file = "model/GL_LMA_CV.stan",
#ss <- stan(file = "model/GL_LMAms_CV.stan",
#                 data = data_l[[1]],
#                 chains = 1,
#                 iter = 1,
#                 warmup = 1,
#                 thin = 1,
#                 control = list(adapt_delta = 0.99, max_treedepth = 20))
#

ee <- extract_log_lik_K(ss, holdout_10)
kk <- kfold(ee)
#compare with official loo results
ll <- loo(ee)
#x <- matrix(runif(10), ncol =2)
#colLogSumExps(log(x))
#apply(x, 2, function(x)sum((x))) %>% log
  # See equation (21) of @VehtariEtAl2016
