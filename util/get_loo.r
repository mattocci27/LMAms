library(tidyverse)
library(rstan)
library(stringr)
library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#load("./rda/GL_LMAms_more_obs.rda")
#GL_LMAms <- res

obs_files <- list.files("rda") %>%
  str_subset("obs.rda") 

obs_files <- obs_files[!str_detect(obs_files, "_more") | 
                 !str_detect(obs_files, "PA")] 

tmp <- obs_files %>%
  str_split_fixed("_obs", 2)

models <- tmp[,1]
n_samp <- NULL
for (i in 1:length(models)) {
  load(str_c("rda/", obs_files[i]))
  assign(models[i], res)
  n_samp[i] <- nrow(dat)
}


#tmp2 <- models %>% str_split_fixed("_more", 2)
#models_clean <- tmp2[, 1]

dat <- tibble(LMA = models) %>%
# head(2) %>%
 mutate(N = n_samp) %>%
 mutate(elpd_list = map(LMA, ~ loo(get(.)))) %>%
 mutate(elpd = map_dbl(elpd_list, ~ .$estimates[1, 1]))

GL_tb <- dat %>%
  dplyr::select(LMA, elpd, N) %>%
  filter(str_detect(LMA, "GL")) %>%
  arrange(desc(elpd)) %>%
  # drop _more 
  mutate(LMA = str_split_fixed(LMA, "_more", 2)[, 1])

PA_tb <- dat %>%
  dplyr::select(LMA, elpd, N) %>%
  filter(str_detect(LMA, "PA")) %>%
  arrange(desc(elpd))

write_csv(GL_tb, "./data/GL_elpd.csv")
write_csv(PA_tb, "./data/PA_elpd.csv")
