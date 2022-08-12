library(tidyverse)

targets::tar_load(fit_20_draws_PA_Ap_LLs_opt)
targets::tar_load(fit_20_summary_PA_Ap_LLs_opt)
pa_draws <- fit_20_draws_PA_Ap_LLs_opt


targets::tar_load(pa_res_csv)
pa <- read_csv(pa_res_csv)


light <- ifelse(pa$strata == "CAN", 1, 0)
log_LMAs_mat <- pa_draws |>
  dplyr::select(contains("LMAs")) |>
  as.matrix()

res_fun <- function(x, light) {
  fit_s <- lm(x ~ light)
  res_s <- residuals(fit_s)
  res_s
}

tic()
res_s_mat <- apply(log_LMAs_mat, 1, res_fun,  light)
toc()

res_s <- apply(hoge, 1, mean)
fit_Ls <- lm(log(pa$LL) ~ light)
res_Ls <- residuals(fit_Ls)
tibble(res_LL = res_Ls, res_LMAs = res_s)
