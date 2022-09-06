
#' @title Generate stan file names
generate_stan_names <- function(model_json, model_lma_json) {
  model <- fromJSON(model_json)$config
  model_lma <- fromJSON(model_lma_json)$config
  model_lma2 <- model_lma |>
    mutate(site = ifelse(str_detect(model, "GL"), "GL", "PA"))
  model2 <- full_join(model_lma2, model, by = c("site", "model", "opt"))

  gl_stan_names <- str_c("stan/",
    model2 |>
      filter(site == "GL") |>
      pull(model),
    ".stan")
  pa_stan_names <- str_c("stan/",
    model2 |>
      filter(site == "PA") |>
      pull(model),
    ".stan")
  diagnostics_names <- str_c(
    str_to_lower(model2$site),
    "diagnostics",
    model2$model,
    sep = "_"
    )
  summary_names <- str_c(
    str_to_lower(model2$site),
    "summary",
    model2$model,
    sep = "_"
    )
  list(
    gl_stan_names = gl_stan_names,
    pa_stan_names = pa_stan_names,
    summary_names = summary_names,
    diagnostics_names = diagnostics_names
    )
}

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
    leaf_habit = as.numeric(as.factor(data$leaf_habit)),
    gr = as.numeric(as.factor(data$leaf_habit))
  )

  list_data$J <- list_data$gr %>%
    unique() %>%
    length()

  list_data$obs2 <- list_data$obs
  list_data$E <- ifelse(data$leaf_habit == "E", 1, 0)
  list_data$U <- ifelse(data$leaf_habit == "U", 1, 0)
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
    leaf_habit = 1,
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
#' @return dataframe of cmdstan customized summary with diagnostics
#' @param data Data frame, a single simulated dataset.
#' @param model_file Path to the Stan model source file.
#' @ref https://github.com/wlandau/targets-stan
fit_sim_model <- function(stan_data, model_file,
                            iter_warmup = 1,
                            iter_sampling = 1,
                            adapt_delta = 0.9,
                            max_treedepth = 15,
                            chains = 4,
                            parallel_chains = 1,
                            refresh = 0,
                            seed = 123) {
  model <- cmdstan_model(model_file)
  fit <- model$sample(
    data = stan_data,
    seed = seed,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    chains = chains,
    parallel_chains = parallel_chains,
    refresh = refresh)

  summary_ <- posterior::summarise_draws(fit,
    mean = ~mean(.x),
    sd = ~sd(.x),
    mad = ~mad(.x),
    ~posterior::quantile2(.x, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)),
    posterior::default_convergence_measures())

  diagnostic_summary_ <- fit$diagnostic_summary()
  summary_ |>
    mutate(num_divergent = sum(diagnostic_summary_$num_divergent)) |>
    mutate(num_max_treedepth = sum(diagnostic_summary_$num_max_treedepth)) |>
    mutate(data_id = targets::tar_name())
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
# targets::tar_load(gl_csv)
# data <- read_csv(gl_csv)
# generate_sim_data(data)
generate_sim_data <- function(data, gl = TRUE){
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
  }

  tmp$A_R <- tmp$A - tmp$R

  list_data <- list(N = nrow(tmp),
            A = tmp$Aarea,
            LL = tmp$LL,
            R = tmp$Rarea,
            LMA = tmp$LMA)
  if (gl) {
    list_data$leaf <- 1
  } else {
    list_data$leaf <- ifelse(data$strata == "CAN", 1, 0)
  }
  # if (ld) list_data$LT <- data$LT
  list_data
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
    mutate(leaf_habit = ifelse(GL$leaf_habit == "", "U", as.character(leaf_habit)))
  GL |>
    mutate(LMAp = LMAp_mean) |>
    mutate(LMAp_lwr = LMAp_lwr) |>
    mutate(LMAp_upr = LMAp_upr) |>
    mutate(LMAs = LMAs_mean) |>
    mutate(LMAs_lwr = LMAs_lwr) |>
    mutate(LMAs_upr = LMAs_upr) |>
    mutate(id = paste0("gl_", 1:nrow(GL))) |>
    write_csv("data/gl_res.csv")
  paste("data/gl_res.csv")
}

#' @title Generates csv file of Panama for the subsequent analysis
#' @ld  Use LDs or LMAs. LMAs has 130 rows and LDS has 108 rows (default = FALSE)
generate_pa_dat <- function(pa_full_csv, pa_csv, draws, ld = FALSE) {
  # library(tidyverse)
  # targets::tar_load(pa_full_csv)
  # targets::tar_load(pa_csv)
  # targets::tar_load(fit_20_draws_PA_Ap_LLs_opt)
  # targets::tar_load(fit_18_draws_PA_Ap_LDs_opt)
  # draws_ll <- fit_20_draws_PA_Ap_LLs_opt
  # draws <- fit_18_draws_PA_Ap_LDs_opt
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

  pa <- read_csv(pa_full_csv)
  if (ld) {
    LDs_dat <- draws |>
      dplyr::select(contains("LDs"))
    LDs_mean <- apply(exp(LDs_dat), 2, mean)
    LDs_lwr <- apply(exp(LDs_dat), 2, \(x) quantile(x, 0.025))
    LDs_upr <- apply(exp(LDs_dat), 2, \(x) quantile(x, 0.975))
    pa <- read_csv(pa_csv)
  }

  mu_dat <- draws |>
    janitor::clean_names() |>
    dplyr::select(contains("mu_")) |>
    dplyr::select(ends_with("_2"))

  # for the partial correlation of LL vs. LMAs, controlling for light.
  light <- ifelse(pa$strata == "CAN", 1, 0)
  log_LMAs_mat <- draws |>
    dplyr::select(contains("LMAs")) |>
    as.matrix()

  res_fun <- function(x, light) {
    fit_s <- lm(x ~ light)
    res_s <- residuals(fit_s)
    res_s
  }

  if (ld) {
    log_LDs_mat <- draws |>
      dplyr::select(contains("LDs")) |>
      as.matrix()
    res_s_mat <- apply(log_LDs_mat, 1, res_fun,  light)
  } else {
    res_s_mat <- apply(log_LMAs_mat, 1, res_fun,  light)
  }

  res_s <- apply(res_s_mat, 1, mean)
  fit_Ls <- lm(log(pa$LL) ~ light)
  res_Ls <- residuals(fit_Ls)

  pa <- pa |>
    mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) |>
    mutate(site_strata = paste(site2, strata, sep = "_"))
  pa2 <- pa |>
    mutate(LMAp = LMAp_mean) |>
    mutate(LMAp_lwr = LMAp_lwr) |>
    mutate(LMAp_upr = LMAp_upr) |>
    mutate(LMAs = LMAs_mean) |>
    mutate(LMAs_lwr = LMAs_lwr) |>
    mutate(LMAs_upr = LMAs_upr) |>
    mutate(Mu2 = apply(mu_dat, 2, mean)) |>
    mutate(Mu2_lo = apply(mu_dat, 2, \(x) quantile(x, 0.025))) |>
    mutate(Mu2_up = apply(mu_dat, 2, \(x) quantile(x, 0.975))) |>
    mutate(id = paste0("pa_", 1:nrow(pa)))
  if (ld) {
   pa2 <- pa2 |>
    mutate(LDs = LDs_mean) |>
    mutate(LDs_lwr = LDs_lwr) |>
    mutate(LDs_upr = LDs_upr) |>
    mutate(res_LL_light = res_Ls, res_LDs_light = res_s) |>
    write_csv("data/pa_res_ld.csv")
   paste("data/pa_res_ld.csv")
  } else {
   pa2 <- pa2 |>
    mutate(res_LL_light = res_Ls, res_LMAs_light = res_s) |>
    write_csv("data/pa_res.csv")
   paste("data/pa_res.csv")
  }
}

#' @title Generates csv file of GLOPNET for the subsequent analysis
clean_gl_res <- function(gl_res_csv) {
  gl <- read_csv(gl_res_csv)
  gl |>
    mutate(frac = LMAp / LMA) |>
    mutate(leaf_habit = ifelse(is.na(leaf_habit), "U", leaf_habit)) |>
    mutate(gr = factor(leaf_habit,
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

#' @para gl_rand_sig data including 95% CI
#' @para gl_rand_check data with rhat and divergence
coef_sim <- function(sim_para_summary, site) {
  data <- sim_para_summary  |>
   # filter(!kkstr_detect(para, "0")) |>
    # mutate(cov = ifelse(rhat == 0, "Converged", "Not converged")) |>
    # mutate(cov = factor(cov, levels = c("Not converged", "Converged"))) |>
    mutate(sim_id_no = as.factor(data_id) |>
       as.numeric() |>
       str_pad(2, pad = 0)) |>
    mutate(sim_id = paste0("sim-", sim_id_no)) |>
    mutate(para = case_when(
      variable == "a0" ~ "alpha[0]",
      variable == "ap" ~ "alpha[p]",
      variable == "as" ~ "alpha[s]",
      variable == "b0" ~ "beta[0]",
      variable == "bs" ~ "beta[s]",
      variable == "g0" ~ "gamma[0]",
      variable == "gp" ~ "gamma[p]",
      variable == "gs" ~ "gamma[s]",
      TRUE ~ variable
    )) |>
    mutate(sig = ifelse(q2.5 * q97.5 > 0, "Significant", "Non-significant"))

  ggplot(data) +
    geom_hline(yintercept = 0) +
    geom_pointrange(aes(x = sim_id,
     y = mean, ymin = q2.5, ymax = q97.5, group = sim_id, col = sig)) +
    # scale_colour_manual(values = c("#008B00", "#1874CD")) +
    facet_wrap(~para, scale = "free", labeller = label_parsed) +
    xlab("Simulation ID") +
    ylab("Coefficients") +
    ggtitle(site) +
    labs(colour = "") +
    coord_flip() +
    theme_bw() +
    theme(
       legend.position = c(0.8, 0.2),
       axis.text.x = element_text(angle = 45, vjust = 0.8)
    )
}

#' @title Extract parameters from dynamic branches of sim_summary
#' @para sim_summary dynamic branches of sim_summary
extract_sim_summary <- function(sim_summary) {
  para <- expand_grid(a = c("a", "b", "g"), b = c("0", "p", "s")) |>
    mutate(para = str_c(a, b)) |>
    pull(para)
  sim_summary |>
    # filter(q2.5 * q97.5 > 0) |>
    filter(variable %in% c(para, "theta"))
}

#' @title Extract summary diagnostics from dynamic branches of sim_summary
#' @para sim_summary dynamic branches of sim_summary
extract_sim_diagnostics <- function(sim_summary) {
  sim_summary |>
    filter(rhat > 1.1) |>
    count(data_id) |>
    full_join(sim_summary |>
      filter(variable == "lp__")) |>
    dplyr::select(data_id, num_rhat = n, num_divergent)
}

#' @title Generates summary diagnostics
#' @para gl_sim_diagnostics
#' @para pa_sim_diagnostics
generate_summary_diagnostics <- function(gl_sim_diagnostics, pa_sim_diagnostics, file) {
  gl <- gl_sim_diagnostics |>
      mutate(sim_id_no = as.factor(data_id) |>
         as.numeric() |>
         str_pad(2, pad = 0)) |>
      mutate(sim_id = paste0("sim-", sim_id_no)) |>
      mutate(Data = "GLOPNET")

  pa <- pa_sim_diagnostics |>
      mutate(sim_id_no = as.factor(data_id) |>
         as.numeric() |>
         str_pad(2, pad = 0)) |>
      mutate(sim_id = paste0("sim-", sim_id_no)) |>
      mutate(Data = "Panama")

  d <- bind_rows(gl, pa) |>
    arrange(sim_id) |>
    arrange(Data) |>
    mutate(num_rhat = replace_na(num_rhat, 0)) |>
    dplyr::select(
      Data,
      Simulation_ID = sim_id,
      No_large_Rhat = num_rhat,
      No_divergence = num_divergent
    )

    my_write_csv(d, file)

}

#' @title Get posterior estimates mcmc summary
#' @param data data frame, summary of mcmc
#' @param row variable name (e.g., "theta")
#' @param col summary name (e.g., "mean", "q50")
#' @param digits integer indicating the number of decimal places
#' @param nsmall the minimum number of digits to the right of the decimal point
get_post_para <- function(data, row, col, digits = 2, nsmall = 2) {
  data |>
    mutate_if(is.numeric, \(x) round(x, digits = digits)) |>
    mutate_if(is.numeric, \(x) format(x, nsmall = nsmall)) |>
    filter(variable == {{row}}) |>
    pull({{col}})
}

#' @title write_csv for targets
#' @inheritParams readr::write_csv
my_write_csv <- function(x, path, append = FALSE, col_names = !append) {
    write_csv(x, path, append = FALSE, col_names = !append)
    paste(path)
}


#' @title Generate data for LL partial plot

