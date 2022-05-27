#' @title Generate stan data for GLOPNET
generate_gl_stan <- function(data) {
  dat <- read_csv(data) %>%
    mutate(DE = ifelse(is.na(DE), "U", DE)) %>%
    as.data.frame()

  list_dat <- list(
    N = nrow(dat),
    obs = cbind(
      log(dat[, "Aarea"] + dat[, "Rarea"]),
      log(dat[, "LL"]),
      log(dat[, "Rarea"])
    ),
    LMA = dat[, "LMA"],
    A = dat[, "Aarea"],
    LL = dat[, "LL"],
    R = dat[, "Rarea"],
    q_lim = 1,
    leaf = 1,
    dry = 1,
    gr = as.numeric(as.factor(dat$DE))
  )

  list_dat$J <- list_dat$gr %>%
    unique() %>%
    length()

  list_dat$obs2 <- list_dat$obs
  list_dat$E <- ifelse(dat$DE == "E", 1, 0)
  list_dat$U <- ifelse(dat$DE == "U", 1, 0)
  list_dat
}

#' @title Generate stan data for Panama
#' @par full TRUE removes LT from the list (default = FALSE)
generate_pa_stan <- function(data, full = FALSE) {
  # data <- pa_full_csv
  dat <- read_csv(data) %>%
    filter(!is.na(LMA)) %>%
    filter(!is.na(Aarea)) %>%
    filter(!is.na(Rarea)) %>%
    filter(!is.na(LL)) %>%
    as.data.frame() %>%
    mutate(gr = paste(site, strata) %>%
      as.factor() %>%
      as.numeric())

  q_lim <- dat %>%
    filter(strata == "UNDER") %>%
    mutate(q = Rarea / Aarea) %>%
    summarize(max(q)) %>%
    as.numeric()

  list_dat <- list(
    N = nrow(dat),
    obs = cbind(
      log(dat[, "Aarea"] + dat[, "Rarea"]),
      log(dat[, "LL"]),
      log(dat[, "Rarea"])
    ),
    LMA = dat[, "LMA"],
    LT = dat[, "LT"],
    A = dat[, "Aarea"],
    LL = dat[, "LL"],
    R = dat[, "Rarea"],
    DE = 1,
    gr = dat$gr,
    J = dat$gr %>% unique() %>% length(),
    q_lim = q_lim,
    # leaf = as.numeric(as.factor(dat$strata)),
    leaf = ifelse(dat$strata == "CAN", 1, 0),
    dry = ifelse(dat$site == "PNM", 1, 0)
  )
  if (full) {
    # drop LT from the full data
    list_dat <- list_dat[names(list_dat) != "LT"]
  }
  list_dat
}

#' @title Generate a txt file for _targets.R
generate_tar_stan <- function(model, model_lma) {
  model <- fromJSON(model)$config
  # model_lma <- fromJSON("templates/model_LMA.json")$config
  # model <- fromJSON("templates/model.json")$config
  model_lma <- fromJSON(model_lma)$config
  model_lma2 <- model_lma |>
    mutate(site = ifelse(str_detect(model, "GL"), "GL", "PA"))

  model2 <- full_join(model_lma2, model, by = c("site", "model", "opt"))

  model_n <- nrow(model2)

  model3 <- model2 |>
    mutate(fit = paste("fit", 1:model_n, sep = "_")) |>
    mutate(stan = paste0("stan/", model, ".stan")) |>
    mutate(data = ifelse(site == "GL", "gl_stan_dat", "pa_stan_dat"))

  tmp <- "templates/tar_stan_mcmc.txt"
  for (i in 1:nrow(model3)) {
    if (i == 1) {
      write_lines("  tar_stan_mcmc(", tmp, append = FALSE)
    } else {
      write_lines("  tar_stan_mcmc(", tmp, append = TRUE)
    }
    write_lines(
      paste0("    ", model3$fit[i]), tmp, ",\n",
      append = TRUE
    )
    write_lines(
      paste0('    "', model3$stan[i]), tmp, '",\n',
      append = TRUE
    )
    write_lines(
      paste0("    data = ", model3$data[i]), tmp, ",\n",
      append = TRUE
    )
    write_lines('    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 1,
    iter_sampling = 1,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),',
      tmp,
      append = TRUE
    )
  }
  paste(tmp)
}

#' @title Check divergence from draws
div_check <- function(diags) {
  n1 <- diags |>
    filter(divergent__ == 1) |>
    nrow()
  n2 <- diags |>
    nrow()
  print(paste(
    n1, "of", n2,
    "iterations ended with a divergence", n1 / n2 * 100, "%"
  ))
}


#' @title Generates csv file of GLOPNET for the subsequent analysis
generate_gl_dat <- function(gl_csv, draws) {
  # targets::tar_load(gl_csv)
  GL <- read_csv(gl_csv)
  draws <- draws |>
    janitor::clean_names()
  p_dat <- draws |>
    dplyr::select(contains("p_"))
  p_vec <- apply(p_dat, 2, mean)
  p_vec_lo <- apply(p_dat, 2, function(x) quantile(x, 0.025))
  p_vec_up <- apply(p_dat, 2, function(x) quantile(x, 0.975))
  GL <- GL |>
    mutate(DE = ifelse(GL$DE == "", "U", as.character(DE)))
  GL |>
    mutate(LMAp = LMA * p_vec) |>
    mutate(LMAs = LMA - LMAp) |>
    mutate(LMAp_lo = LMA * p_vec_lo) |>
    mutate(LMAp_up = LMA * p_vec_up) |>
    mutate(LMAs_lo = LMA - LMAp_up) |>
    mutate(LMAs_up = LMA - LMAp_lo) |>
    write_csv("data/GL_res.csv")
  paste("data/GL_res.csv")
}

#' @title Generates csv file of Panama for the subsequent analysis
generate_pa_dat <- function(pa_full_csv, draws) {
  # library(tidyverse)
  # targets::tar_load(pa_full_csv)
  # targets::tar_load(fit_20_draws_PA_Ap_LLs_opt)
  # draws <- fit_20_draws_PA_Ap_LLs_opt
  draws <- draws |>
    janitor::clean_names()
  p_dat <- draws |>
    dplyr::select(contains("p_"))
  p_vec <- apply(p_dat, 2, mean)
  p_vec_lo <- apply(p_dat, 2, function(x) quantile(x, 0.025))
  p_vec_up <- apply(p_dat, 2, function(x) quantile(x, 0.975))
  mu_dat <- draws |>
    dplyr::select(contains("mu_")) |>
    dplyr::select(ends_with("_2"))
  PA <- read_csv(pa_full_csv)
  PA <- PA |>
    mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) |>
    mutate(site_strata = paste(site2, strata, sep = "_"))
  PA |>
    mutate(LMAp = LMA * p_vec) |>
    mutate(LMAs = LMA - LMAp) |>
    mutate(LMAp_lo = LMA * p_vec_lo) |>
    mutate(LMAp_up = LMA * p_vec_up) |>
    mutate(LMAs_lo = LMA - LMAp_up) |>
    mutate(LMAs_up = LMA - LMAp_lo) |>
    mutate(Mu2 = apply(mu_dat, 2, mean)) |>
    mutate(Mu2_lo = apply(mu_dat, 2, \(x) quantile(x, 0.025))) |>
    mutate(Mu2_up = apply(mu_dat, 2, \(x) quantile(x, 0.975))) |>
    write_csv("data/PA_res.csv")

  paste("data/PA_res.csv")
}
