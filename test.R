library(tidyverse)
targets::tar_load(loo_model)

d <- read_csv("data-raw/leaf_traits.csv")

d2 <- d |>
  janitor::clean_names() |>
  mutate(lma = 1 / sla_leaf * 10000) |>
  mutate(amax_re = lma * amaxmass / 1000) |>
  mutate(resp_re = lma * respmass / 1000)

d2 |>
  dplyr::select(resp, resp_re) |>
  #dplyr::select(amax, amax_re, resp, resp_re) |>
  # mutate(Aarea = ifelse(is.na(amax), amax_re, amax)) |>
  mutate(Rarea = ifelse(is.na(resp), resp_re, resp)) |>
  mutate(Rarea = ifelse(resp < 0, resp_re, resp)) |>
  as.data.frame()

names(loo_model)
sapply(loo_model, "[[", "looic")

d <- read_csv("data/pa_res_ld.csv")
d <- read_csv("data/pa_res.csv")

ggplot(d, aes(x = LMAs, y = Aarea)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()
