#' @title Generate stan data for GLOPNET
generate_gl_stan <- function(data) {
  list_data <- list(
    N = nrow(data),
    obs = cbind(
      log(data$Aarea + data$Rarea),
      log(data$LL),
      log(data$Rarea)
    ),
    LMA = data$LMA,
    A = data$Aarea,
    LL = data$LL,
    R = data$Rarea,
    q_lim = 1,
    leaf = 1,
    dry = 1,
    DE = as.numeric(as.factor(data$DE)),
    gr = as.numeric(as.factor(data$DE))
  )

  list_data$J <- list_data$gr %>%
    unique() %>%
    length()

  list_data$obs2 <- list_data$obs
  list_data$E <- ifelse(data$DE == "E", 1, 0)
  list_data$U <- ifelse(data$DE == "U", 1, 0)
  list_data
}

#' @title Generate stan data for Panama
#' @par full TRUE removes LT from the list (default = FALSE)
generate_pa_stan <- function(data, full = FALSE) {
  data <- data %>%
    filter(!is.na(LMA)) %>%
    filter(!is.na(Aarea)) %>%
    filter(!is.na(Rarea)) %>%
    filter(!is.na(LL)) %>%
    as.data.frame() %>%
    mutate(gr = paste(site, strata) %>%
      as.factor() %>%
      as.numeric())

  q_lim <- data %>%
    filter(strata == "UNDER") %>%
    mutate(q = Rarea / Aarea) %>%
    summarize(max(q)) %>%
    as.numeric()

  list_data <- list(
    N = nrow(data),
    obs = cbind(
      log(data$Aarea + data$Rarea),
      log(data$LL),
      log(data$Rarea)
    ),
    LMA = data$LMA,
    LT = data$LT,
    A = data$Aarea,
    LL = data$LL,
    R = data$Rarea,
    DE = 1,
    gr = data$gr,
    J = data$gr %>% unique() %>% length(),
    q_lim = q_lim,
    leaf = ifelse(data$strata == "CAN", 1, 0),
    dry = ifelse(data$site == "PNM", 1, 0)
  )
  if (full) {
    # drop LT from the full data
    list_data <- list_data[names(list_data) != "LT"]
  }
  list_data
}

#' @title Fit the Stan model to randomized data.
#' @return list of cmdstan summary, draws, and diagnostics
#' @param data Data frame, a single simulated dataset.
#' @param model_file Path to the Stan model source file.
#' @ref https://github.com/wlandau/targets-stan
fit_rand_model <- function(stan_data, model_file,
                            iter_warmup = 2000,
                            iter_sampling = 2000,
                            adapt_delta = 0.999,
                            #adapt_delta = 0.8,
                            max_treedepth = 15) {
  model <- cmdstan_model(model_file)
  fit <- model$sample(
    data = stan_data,
    seed = 123,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    chains = 4,
    parallel_chains = 4,
    refresh = 0)
  list(summary = fit$summary(), draws = fit$draws(), diagnostics = fit$sampler_diagnostics())
}

#' @title Compile a Stan model and return a path to the compiled model output.
#' @description We return the paths to the Stan model specification
#'   and the compiled model file so `targets` can treat them as
#'   dynamic files and compile the model if either file changes.
#' @return Path to the compiled Stan model, which is just an RDS file.
#'   To run the model, you can read this file into a new R session with
#'   `readRDS()` and feed it to the `object` argument of `sampling()`.
#' @param model_file Path to a Stan model file.
#'   This is a text file with the model spceification.
#' @ref https://github.com/wlandau/targets-stan
#' @examples
#' library(cmdstanr)
#' compile_model("stan/model.stan")
compile_model <- function(model_file) {
  quiet(cmdstan_model(model_file))
  model_file
}

#' @title Suppress output and messages for code.
#' @description Used in the pipeline.
#' @return The result of running the code.
#' @param code Code to run quietly.
#' @ref https://github.com/wlandau/targets-stan
#' @examples
#' library(cmdstanr)
#' library(tidyverse)
#' compile_model("stan/model.stan")
#' quiet(fit_model("stan/model.stan", simulate_data_discrete()))
#' out
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  suppressMessages(code)
}

#' @title Generate randomized dataset
#' @return list of randomized dataset
#' @description We make sure that randomized data has
#' zero covariane among tratis
#' @example
# library(tidyverse)
# n <- 2
# targets::tar_load(gl_stan_dat)
# list_data <- gl_stan_dat
# targets::tar_load(gl_csv)
# data <- read_csv(gl_csv)
# rand_fun(2, data, list_data)
rand_fun <- function(n, data, list_data){
  a_pval <- cor.test(log(data$LMA), log(data$Aarea))$p.val
  l_pval <- cor.test(log(data$LMA), log(data$LL))$p.val
  r_pval <- cor.test(log(data$LMA), log(data$Rarea))$p.val
  al_pval <- cor.test(log(data$LL), log(data$Aarea))$p.val
  rl_pval <- cor.test(log(data$Rarea), log(data$LL))$p.val
  ar_pval <- cor.test(log(data$Aarea), log(data$Rarea))$p.val

  ar_min <- min(data$Aarea - data$Rarea)

  while (ar_min < 0 | a_pval < 0.1 | l_pval < 0.1 | r_pval < 0.1 |
    al_pval < 0.1 | rl_pval < 0.1 | ar_pval < 0.1) {
    tmp <- data.frame(data[, 1:3],
             LMA = sample(data$LMA),
             LL = sample(data$LL),
             Aarea = sample(data$Aarea),
             Rarea = sample(data$Rarea)
             )
    ar_min <- min(tmp$Aarea - tmp$Rarea)
    a_pval <- cor.test(log(tmp$LMA), log(tmp$Aarea))$p.val
    l_pval <- cor.test(log(tmp$LMA), log(tmp$LL))$p.val
    r_pval <- cor.test(log(tmp$LMA), log(tmp$Rarea))$p.val
    al_pval <- cor.test(log(tmp$LL), log(tmp$Aarea))$p.val
    rl_pval <- cor.test(log(tmp$Rarea), log(tmp$LL))$p.val
    ar_pval <- cor.test(log(tmp$Aarea), log(tmp$Rarea))$p.val
    # paste("Aarea", a_pval) |> print()
    # paste("Rarea", r_pval) |> print()
    # paste("LL", l_pval) |> print()
    # paste("Aarea-LL", al_pval) |> print()
    # paste("Rarea-LL", rl_pval) |> print()
    # paste("Aarea-Rarea", ar_pval) |> print()
  }

  tmp$A_R <- tmp$A - tmp$R

  list(N = list_data$N,
            A = tmp$Aarea,
            LL = tmp$LL,
            R = tmp$Rarea,
            q_lim = list_data$q_lim,
            leaf = list_data$leaf,
            dry = list_data$dry,
            DE = list_data$DE,
            LMA = tmp$LMA)
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
    adapt_delta = 0.9,
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
  LMAp_dat <- draws |>
    dplyr::select(contains("LMAp"))
  LMAs_dat <- draws |>
    dplyr::select(contains("LMAs"))
  LMAp_mean <- apply(exp(LMAp_dat), 2, mean)
  LMAp_lwr <- apply(exp(LMAp_dat), 2, \(x) quantile(x, 0.025))
  LMAp_upr <- apply(exp(LMAp_dat), 2, \(x) quantile(x, 0.975))
  LMAs_mean <- apply(exp(LMAs_dat), 2, mean)
  LMAs_lwr <- apply(exp(LMAs_dat), 2, \(x) quantile(x, 0.025))
  LMAs_upr <- apply(exp(LMAs_dat), 2, \(x) quantile(x, 0.975))
  GL <- GL |>
    mutate(DE = ifelse(GL$DE == "", "U", as.character(DE)))
  GL |>
    mutate(LMAp = LMAp_mean) |>
    mutate(LMAp_lwr = LMAp_lwr) |>
    mutate(LMAp_upr = LMAp_upr) |>
    mutate(LMAs = LMAs_mean) |>
    mutate(LMAs_lwr = LMAs_lwr) |>
    mutate(LMAs_upr = LMAs_upr) |>
    mutate(id = paste0("gl_", 1:nrow(GL))) |>
    write_csv("data/GL_res.csv")
  paste("data/GL_res.csv")
}

#' @title Generates csv file of Panama for the subsequent analysis
generate_pa_dat <- function(pa_full_csv, draws) {
  # library(tidyverse)
  # targets::tar_load(pa_full_csv)
  # targets::tar_load(fit_20_draws_PA_Ap_LLs_opt)
  # draws <- fit_20_draws_PA_Ap_LLs_opt
  LMAp_dat <- draws |>
    dplyr::select(contains("LMAp"))
  LMAs_dat <- draws |>
    dplyr::select(contains("LMAs"))
  LMAp_mean <- apply(exp(LMAp_dat), 2, mean)
  LMAp_lwr <- apply(exp(LMAp_dat), 2, \(x) quantile(x, 0.025))
  LMAp_upr <- apply(exp(LMAp_dat), 2, \(x) quantile(x, 0.975))
  LMAs_mean <- apply(exp(LMAs_dat), 2, mean)
  LMAs_lwr <- apply(exp(LMAs_dat), 2, \(x) quantile(x, 0.025))
  LMAs_upr <- apply(exp(LMAs_dat), 2, \(x) quantile(x, 0.975))

  mu_dat <- draws |>
    janitor::clean_names() |>
    dplyr::select(contains("mu_")) |>
    dplyr::select(ends_with("_2"))
  PA <- read_csv(pa_full_csv)
  PA <- PA |>
    mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) |>
    mutate(site_strata = paste(site2, strata, sep = "_"))
  PA |>
    mutate(LMAp = LMAp_mean) |>
    mutate(LMAp_lwr = LMAp_lwr) |>
    mutate(LMAp_upr = LMAp_upr) |>
    mutate(LMAs = LMAs_mean) |>
    mutate(LMAs_lwr = LMAs_lwr) |>
    mutate(LMAs_upr = LMAs_upr) |>
    mutate(Mu2 = apply(mu_dat, 2, mean)) |>
    mutate(Mu2_lo = apply(mu_dat, 2, \(x) quantile(x, 0.025))) |>
    mutate(Mu2_up = apply(mu_dat, 2, \(x) quantile(x, 0.975))) |>
    mutate(id = paste0("pa_", 1:nrow(PA))) |>
    write_csv("data/PA_res.csv")

  paste("data/PA_res.csv")
}

#' @title Generates csv file of GLOPNET for the subsequent analysis
clean_gl_res <- function(gl_res_csv) {
  gl <- read_csv(gl_res_csv)
  gl |>
    mutate(frac = LMAp / LMA) |>
    mutate(DE = ifelse(is.na(DE), "U", DE)) |>
    mutate(gr = factor(DE,
                       labels = c("Deciduous",
                                  "Evergreen",
                                  "Unclassified"
                                  )))
}

#' @title Generates csv file of paOPNET for the subsequent analysis
clean_pa_res <- function(pa_res_csv) {
  pa <- read_csv(pa_res_csv)
  pa |>
    mutate(frac = LMAp / LMA) |>
    mutate(site_strata = factor(site_strata,
            levels = c("WET_CAN", "DRY_CAN", "WET_UNDER", "DRY_UNDER"))) %>%
    mutate(site_strata2 = factor(site_strata,
      labels = c("Sun-Wet",
                 "Sun-Dry",
                 "Shade-Wet",
                 "Shade-Dry"
                        ))) |>
    mutate(gr = factor(site_strata2,
      labels = c("Sun\nDry", "Shade\nDry", "Sun\nWet", "Shade\nWet")))
}

#' @title Table of the main text
create_para_tbl <- function(gl_draws, pa_draws) {
  # targets::tar_load(fit_7_draws_GL_Aps_LLs)
 #draws <- fit_7_draws_GL_Aps_LLs
  gl_draws2 <- gl_draws |>
    dplyr::select(c("ap", "as", "bs", "gp", "gs"))
  gl_tab <- bind_cols(
    mean_ = apply(gl_draws2, 2, mean),
    low = apply(gl_draws2, 2, \(x)quantile(x, 0.025)),
    up = apply(gl_draws2, 2, \(x)quantile(x, 0.975))) |>
    round(3) |>
    mutate(sig = ifelse(low * up > 0, "sig", "ns")) |>
    mutate(est = paste0(mean_, " [", low, ", ", up, "]")) |>
    mutate(para = c("Effect of LMAp on *A*~area~ ($\\alpha_p$)",
                    "Effect of LMAs on *A*~area~ ($\\alpha_s$)",
                    "Effect of LMAs on LL ($\\beta_s$)",
                    "Effect of LMAp on *R*~area~ ($\\gamma_p$)",
                    "Effect of LMAs on *R*~area~ ($\\gamma_s$)"
                    )) |>
    dplyr::select(para, GLOPNET = est, sig1 = sig)

  pa_draws2 <- pa_draws |>
    dplyr::select(c("ap", "bs", "gp", "gs", "theta"))
  pa_tab <- bind_cols(
    mean_ = apply(pa_draws2, 2, mean),
    low = apply(pa_draws2, 2, \(x)quantile(x, 0.025)),
    up = apply(pa_draws2, 2, \(x)quantile(x, 0.975))) |>
    round(3) |>
    mutate(sig = ifelse(low * up > 0, "sig", "ns")) |>
    mutate(est = paste0(mean_, " [", low, ", ", up, "]")) |>
    mutate(para = c("Effect of LMAp on *A*~area~ ($\\alpha_p$)",
                    "Effect of LMAs on LL ($\\beta_s$)",
                    "Effect of LMAp on *R*~area~ ($\\gamma_p$)",
                    "Effect of LMAs on *R*~area~ ($\\gamma_s$)",
                    "Effect of light on LL ($\\theta$)"
                    )) |>
    dplyr::select(para, Panama = est, sig2 = sig)

  glpa_tab <- full_join(gl_tab, pa_tab, by = "para") |>
    dplyr::rename(Parameters = para) |>
    mutate(GLOPNET = ifelse(sig1 == "sig",
                          paste0("**", GLOPNET, "**"),
                          GLOPNET)) |>
    mutate(Panama = ifelse(sig2 == "sig",
                          paste0("**", Panama, "**"),
                          Panama)) |>
    dplyr::select(Parameters, GLOPNET, Panama)

  glpa_tab[is.na(glpa_tab)] <- "-"
  glpa_tab |> write_csv("./data/para_tbl.csv")
  paste("./data/para_tbl.csv")
}

#' @para para parameter name (e.g., "ap")
#' @para rand_fit e.g., gl_rand_fit
#' @para n sim ID
#' @example
#' list("a0", "ap", "as", "b0", "bs", "g0", "gp", "gs") |>
#'  map_dfr(rand_summary, gl_rand_fit, 6)
rand_summary <- function(para, rand_fit, n) {
  tmp <- rand_fit[[n]]$draws
  para_dbl <- tmp[, ,para] |> as.numeric()
  tibble(para = para,
    mean = mean(para_dbl),
    lwr = quantile(para_dbl, 0.025),
    upr = quantile(para_dbl, 0.975),
    sim_id = n) |>
    mutate(sig = ifelse(lwr * upr > 0, "sig", "ns"))
}

#' @title later
create_sim_dat <- function() {
  rho12 <- -0.58
  rho23 <- -0.62
  rho13 <- 0.55
  sigma1 <- 0.4
  sigma2 <- 0.3
  sigma3 <- 0.3
  x1 <- rnorm(n)
  x2 <- rnorm(n, x1, 3) %>%
    scale %>% as.numeric

  cor.test(x1, x2)

  mu1 <- a1 + a2 * x1
  mu2 <- b1 + b2 * x2
  mu3 <- c1 + c2 * x1 + c3 * x2

  Sigma <- matrix(c(sigma1^2, rho12*sigma1*sigma2, rho13*sigma1*sigma3,
      rho12*sigma1*sigma2, sigma2^2, rho23*sigma2*sigma3,
      rho13*sigma1*sigma3, rho23*sigma2*sigma3, sigma3^2), ncol =3)

  y_new <- NULL
  for (i in 1:n){
    y <- rmvnorm(1, c(mu1[i] + mu3[i], mu2[i], mu3[i]), Sigma)
    y_new <- rbind(y_new, y)
  }
}

#' @para loo_tbl csv file of loo
write_model_selction <- function(loo_tbl) {
  output <- "data/model_selection.csv"
  d <- read_csv(loo_tbl)
  d |>
    mutate(no1 = case_when(
      str_detect(Model, "LMA_opt") ~ 3,
      str_detect(Model, "LL.*opt") ~ 4,
      str_detect(Model, "LD.*opt") ~ 5,
      str_detect(Model, "LL") ~ 2,
      str_detect(Model, "LMA") ~ 1
    )) |>
  # because Ap is included Aps, this order is correct
    mutate(no2 = case_when(
      str_detect(Model, "Aps.*LLs|Aps.*LDs") ~ "b",
      str_detect(Model, "Ap.*LLs|Ap.*LDs") ~ "a",
      str_detect(Model, "Aps.*LLps|Aps.*LDps") ~ "d",
      str_detect(Model, "Ap.*LLps|Ap.*LDps") ~ "c",
      TRUE ~ ""
    )) |>
    filter(!is.na(no1)) |>
    rename(model_ori = Model) |>
    mutate(Model = paste0(no1, no2)) |>
    mutate(Data = ifelse(site == "PA", "Panama", "GLOPNET")) |>
    dplyr::select(model_ori, Model, Data, N, LOOIC) |>
    mutate(LOOIC = LOOIC |> round(1)) |>
    write_csv(output)

    paste(output)
}


#' @para gl_rand_sig data including 95% CI
#' @para gl_rand_check data with rhat and divergence
coef_rand <- function(gl_rand_sig, gl_rand_check, site = site) {
  data <- gl_rand_sig  |>
   # filter(!str_detect(para, "0")) |>
    full_join(gl_rand_check, by = "sim_id") |>
    mutate(cov = ifelse(rhat == 0, "Converged", "Not converged")) |>
    mutate(cov = factor(cov, levels = c("Not converged", "Converged"))) |>
    mutate(sim_id_chr = paste0("sim-", sim_id)) |>
    mutate(para = case_when(
      para == "a0" ~ "alpha[0]",
      para == "ap" ~ "alpha[p]",
      para == "as" ~ "alpha[s]",
      para == "b0" ~ "beta[0]",
      para == "bs" ~ "beta[s]",
      para == "g0" ~ "gamma[0]",
      para == "gp" ~ "gamma[p]",
      para == "gs" ~ "gamma[s]",
      TRUE ~ para
    ))
  ggplot(data) +
    geom_pointrange(aes(x = sim_id_chr,
     y = mean, ymin = lwr, ymax = upr, group = sim_id, col = cov)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~para, scale = "free", labeller = label_parsed) +
    xlab("Simulation ID") +
    ylab("Standardized coefficents") +
    ggtitle(site) +
    coord_flip() +
    theme_bw() +
    theme(
       legend.position = c(0.8, 0.2),
       legend.title = element_blank(),
       axis.text.x = element_text(angle = 45, vjust = 0.8)
    )
}
