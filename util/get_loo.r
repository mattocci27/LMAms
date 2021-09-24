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


files <- list.files("log")
data.frame(obs_files, files) |> print()

div <- NULL
for (i in 1:length(files)) {
	tmp <- readLines(paste0("log/", files[i]))
	tmp2 <- str_detect(tmp, "divergent transitions")
	tmp3 <- unique(tmp2) |> length()
	if (tmp3 == 1) {
        div[i] <- "OK"
	} else div[i] <- "divergent"
}


dat <- tibble(LMA = models) %>%
# head(2) %>%
 mutate(N = n_samp) %>%
 mutate(elpd_list = map(LMA, ~ loo(get(.)))) %>%
 mutate(looic = map_dbl(elpd_list, ~ .$estimates[3, 1])) %>%
 mutate(div)

GL_tb <- dat %>%
  dplyr::select(LMA, looic, N, div) %>%
  filter(str_detect(LMA, "GL")) %>%
  arrange(looic) %>%
  # drop _more
  mutate(LMA = str_split_fixed(LMA, "_more", 2)[, 1])

PA_tb <- dat %>%
  dplyr::select(LMA, looic, N, div) %>%
  filter(str_detect(LMA, "PA")) %>%
  arrange(looic)

write_csv(GL_tb, "./data/GL_elpd.csv")
write_csv(PA_tb, "./data/PA_elpd.csv")
