library(tidyverse)
setwd("./data/")
# use null result 1
null_n <- 1

LMAp_fun <- function(data, res) {
  par <- rstan::extract(res, pars = c("p","log_LMAp","log_LMAs"))
  p_mid <- apply(par$p, 2, median)
  LMAp_mid <- apply(par$log_LMAp, 2, function(x) median(exp(x)))
  data %>% 
    mutate(p_mid = p_mid)  %>%
    mutate(LMAp = LMAp_mid) %>%
    mutate(LMAs = data$LMA - LMAp) %>%
    as_data_frame
}

LMAp_fun_rand <- function(data, rand_res, n) {
  res <- rand_res$model[[n]]
  list_dat <- rand_res$data[[n]]
  par <- rstan::extract(res, pars = c("p","log_LMAp","log_LMAs"))
  p_rand <- apply(par$p, 2, median)
  LMAp_rand <- apply(par$log_LMAp, 2, function(x) median(exp(x)))
  data %>% 
    mutate(p_rand = p_rand)  %>%
    mutate(LMAp_rand = LMAp_rand) %>%
    mutate(LMAs_rand = list_dat$LMA - LMAp_rand) %>%
    mutate(LL_rand = list_dat$LL) %>%
    mutate(Aarea_rand = list_dat$A) %>%
    mutate(Rarea_rand = list_dat$R) %>%
    as_data_frame
}

preLL_fun <- function(data, res) {
  par <- rstan::extract(res, pars = c("mu2"))
  data %>%
    mutate(preLL = apply(par$mu2, 2, median) %>% exp) 
}

preLL_fun_rand <- function(data, rand_res, n) {
  res <- rand_res$model[[n]]
  par <- rstan::extract(res, pars = c("mu2"))
  data %>%
    mutate(preLL_rand = apply(par$mu2, 2, median) %>% exp) 
}

# GL =============================================================

load("GL_potPL_obs.rda")
obs_res <- res
load("GL_potPL_rand.rda")

# for Narea
d <- read.csv("nature02403-s2.csv", skip = 10)

dd <- data.frame(Narea = 10^d$log.Narea,
        Parea = 10^d$log.Parea,
        sp = d$Species) %>%
        group_by(sp) %>%
        summarize(Narea = mean(Narea, na.omit = T),
            Parea = mean(Parea, na.omit = T))

GL <- dat

GL_pot <- GL %>%
  LMAp_fun(., obs_res) %>%
  LMAp_fun_rand(., rand_res, n) %>%
  as_data_frame %>%
  inner_join(., dd, by = "sp")


# PA =========================================

load("PA_sitePL_obs.rda")
obs_res <- res
load("PA_sitePL_rand.rda")

PA <- dat %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
  mutate(site_strata = paste(site2, strata, sep = "_")) 

PA_site <- PA %>%
  LMAp_fun(., obs_res) %>%
  LMAp_fun_rand(., rand_res, n) %>%
  preLL_fun(., res) %>%
  preLL_fun_rand(., rand_res,n) %>%
  as_data_frame

load("PA_optPL_obs.rda")
obs_res <- res
load("PA_optPL_rand.rda")

PA_opt <- PA %>%
  LMAp_fun(., obs_res) %>%
  LMAp_fun_rand(., rand_res, n) %>%
  preLL_fun(., res) %>%
  preLL_fun_rand(., rand_res,n) %>%
  as_data_frame

load("PA_potPL_obs.rda")
obs_res <- res
load("PA_potPL_rand.rda")

PA_pot <- PA %>%
  LMAp_fun(., obs_res) %>%
  LMAp_fun_rand(., rand_res, n) %>%
  preLL_fun(., res) %>%
  preLL_fun_rand(., rand_res,n) %>%
  as_data_frame

write.csv(GL_pot, "GL_pot.csv", row.names = FALSE)
write.csv(PA_pot, "PA_pot.csv", row.names = FALSE)
write.csv(PA_site, "PA_site.csv", row.names = FALSE)
write.csv(PA_opt, "PA_opt.csv", row.names = FALSE)

