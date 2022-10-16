
#' @para gl_para MCMC summary for GLOPNET (e.g., gl_summary_am_bs)
#' @para pa_para MCMC summary for Panama (e.g. pa_summary_am_bs_opt)
gen_mass_point_dat <- function(gl_res_csv, pa_res_csv, gl_para, pa_para) {
  # targets::tar_load(pa_res_csv)
  # targets::tar_load(gl_res_csv)
  pa <- read_csv(pa_res_csv)
  gl <- read_csv(gl_res_csv)

  pa <- pa |>
    count(sp) |>
    filter(n >= 2) |>
    inner_join(pa, by = "sp")

  sun <- pa |>
    filter(strata == "CAN")
  shade <- pa |>
    filter(strata != "CAN")

  get_mean <- function(data, para_str)  {
   data |>
    filter(variable == para_str) |>
    pull(mean)
  }

  point_dat <- tibble(
    site = c("GLOPNET", "Sun", "Shade") |>
                    factor(levels =  c("GLOPNET", "Sun", "Shade")),
    am = c(get_mean(gl_para, "am"), get_mean(pa_para, "am"), get_mean(pa_para, "am")),
    as = c(get_mean(gl_para, "as"), 0, 0)
  )

  gl_b <- lm(log(Aarea) ~ log(LMA), gl)$coefficients[2]
  sun_b <- lm(log(Aarea) ~ log(LMA), sun)$coefficients[2]
  shade_b <- lm(log(Aarea) ~ log(LMA), shade)$coefficients[2]

  gl_vars <- cov(gl$LMA, gl$LMAs) / var(gl$LMA) * 100
  sun_vars <- cov(sun$LMA, sun$LMAs) / var(sun$LMA) * 100
  shade_vars <- cov(shade$LMA, shade$LMAs) / var(shade$LMA) * 100

  point_dat  |>
    mutate(b = c(gl_b, sun_b, shade_b)) |>
    mutate(vars = c(gl_vars, sun_vars, shade_vars))
}


#' @title sd in log-scale
sig_fun <- function(x) {
  mu <- mean(x)
  sig <- sd(x)
  sqrt(log((sig/mu)^2 + 1))
}

#' @title mean in log-scale
mu_fun <- function(x) {
  mu <- mean(x)
  sig <- sd(x)
  log(mu^2 / sqrt(sig^2 + mu^2))
}

gl_sim_fit <- function(log_LMAs, log_LMAm, a0, am, as) {
  log_Aarea <- a0 + am * log_LMAm + as * log_LMAs
  LMA <- exp(log_LMAm) + exp(log_LMAs)
  fit <- lm(log_Aarea ~ log(LMA))
  fit$coefficients[2]
}

#' @para ap only ap was significant
pa_sim_fit <- function(log_LMAs, log_LMAm, am) {
  log_Aarea <- am * log_LMAm
  LMA <- exp(log_LMAm) + exp(log_LMAs)
  fit <- lm(log_Aarea ~ log(LMA))
  fit$coefficients[2]
}

#' @title variance contribution on non-log scale
var_fun <- function(log_LMAs, log_LMAm) {
  LMAm <- exp(log_LMAm)
  LMAs <- exp(log_LMAs)
  LMA <- LMAm + LMAs
  cov(LMA, LMAs) / var(LMA) * 100
}

get_mean <- function(data, para_str)  {
  data |>
    filter(variable == para_str) |>
    pull(mean)
}

#' @para data gl_res_csv or pa_res_csv
#' @para para mcmc summary (e.g.  fit_7_summary_GL_Aps_LLs)
#' @example
#' targets::tar_load(fit_7_summary_GL_Aps_LLs)
#' targets::tar_load(gl_res_csv)
#' data <- read_csv(gl_res_csv)
#' para <- fit_7_summary_GL_Aps_LLs
#' var_fun(log_LMAm, log_LMAs[[1]])
#' LMAm <- exp(log_LMAm + 0.5 * sig^2)
#' tmp <- rnorm(n_samp, mu + 0.5 * sig^2, sig)
#' tmp2 <- rnorm(n_samp, mu + 0.5 * sig^2, sig)
#' exp(tmp) |> tmp2
 mass_prop_sim <- function(data, para, n_sim = 1000, n_samp = 100,
                   x_len = 20, site = "GLOPNET", gl = TRUE, seed = 123) {
  set.seed(seed)
  # mu is mean for log_LMAm but not arithmetic mean of LMAm
  mu <- mean(log(data$LMAm))
  mu2 <- mean(log(data$LMAs))
  sig <- sd(log(data$LMAm))
  LMAs_var <- NULL
  b <- NULL
  for (i in 1:n_sim) {
    log_LMAm <- rnorm(n_samp, mu, sig)
    tmp <- seq(log(1.01), log(10), length = x_len)
    log_LMAs <- map(tmp, \(x) rnorm(n_samp, mu2, x))
    LMAs_var <- cbind(LMAs_var, map_dbl(log_LMAs, var_fun, log_LMAm))
    if (gl) {
      b <- cbind(b, map_dbl(log_LMAs, gl_sim_fit,
        log_LMAm,
        get_mean(para, "a0"),
        get_mean(para, "am"),
        get_mean(para, "as")
      ))
    } else {
      b <- cbind(b, map_dbl(log_LMAs, pa_sim_fit,
        log_LMAm,
        get_mean(para, "am")
      ))
    }
  }
  LMAs_var_mean <- apply(LMAs_var, 1, mean)
  mean_ <- apply(b, 1, mean)
  upr <- apply(b, 1, \(x)(quantile(x, 0.975)))
  lwr <- apply(b, 1, \(x)(quantile(x, 0.025)))
  tibble(mean = mean_, upr = upr, lwr = lwr,
              LMAs_var_mean, site = site)
}

#' @para data gl_res_csv or pa_res_csv
#' @para para mcmc summary (e.g.  fit7_summary_GL_Aps_LLs)
mass_prop_sim_grad_each <- function(am, as, a0, data, n_sim = 1000, n_samp = 100,
                   x_len = 20, site = "GLOPNET", seed = 123) {
  set.seed(seed)
  mu <- mean(log(data$LMAm))
  mu2 <- mean(log(data$LMAs))
  sig <- sd(log(data$LMAm))
  LMAs_var <- NULL
  b <- NULL
  for (i in 1:n_sim) {
    log_LMAm <- rnorm(n_samp, mu, sig)
    tmp <- seq(log(1.01), log(10), length = x_len)
    log_LMAs <- map(tmp, \(x) rnorm(n_samp, mu2, x))
    LMAs_var <- cbind(LMAs_var, map_dbl(log_LMAs, var_fun, log_LMAm))
    b <- cbind(b, map_dbl(log_LMAs, gl_sim_fit,
      log_LMAm,
      a0,
      am,
      as
    ))
  }
  LMAs_var_mean <- apply(LMAs_var, 1, mean)
  mean_ <- apply(b, 1, mean)
  upr <- apply(b, 1, \(x)(quantile(x, 0.975)))
  lwr <- apply(b, 1, \(x)(quantile(x, 0.025)))
  tibble(mean = mean_, upr = upr, lwr = lwr,
              LMAs_var_mean, site = site, a0, am, as)
}

#' @para gl_res_csv gl_res_csv
#' @para summary_mcmc mcmc summary (e.g.  fit_7_summary_GL_Aps_LLs)
#' @para ap vector for ap (e.g., c(0.1, 0.5, 1.0))
#' @para as vector for as e.g., get_mean(fit_7_summary_GL_Aps_LLs, "as") |> rep(3)
mass_prop_sim_grad <- function(gl_res_csv, summary_mcmc, am, as, n_sim = 1000, x_len = 20) {
  para_id <- rep(seq(1, length(am)), each = x_len)
  a0 <- get_mean(summary_mcmc, "a0")
  pmap_dfr(list(am, as), mass_prop_sim_grad_each,
    a0 = a0,
    data = read_csv(gl_res_csv),
    n_sim = n_sim) |>
      mutate(para_id = para_id)
}

#' @title Simulation for mass prop (MVN)
#' @para data pa_res_csv
#' @para para mcmc summary (e.g.  fit_20_summary_PA_Ap_LLs_opt)
#' @example
# targets::tar_load(pa_res_csv)
# targets::tar_load(fit_20_summary_PA_Ap_LLs_opt)
# para  <- fit_20_summary_PA_Ap_LLs_opt
# data <- read_csv(pa_res_csv) |> filter(strata != "CAN")
# mass_prop_sim_mv(data, para, n_sim = 10)
mass_prop_sim_mv <- function(data, para, n_sim = 1000, n_samp = 100,
                   x_len = 20, site = "Shade", seed = 123) {
  set.seed(seed)
  Mu <- c(mean(log(data$LMAm)), mean(log(data$LMAs)))
  rho <- -0.4
  sig1 <- sd(log(data$LMAm))
  sig2 <- seq(log(1.01), log(10), length = x_len)
  LMAs_var <- NULL
  b <- NULL
  for (j in 1:n_sim) {
    log_LMAm <- NULL
    log_LMAs <- NULL
    for (i in 1:10) {
      S <- matrix(c(sig1^2, rho*sig1*sig2[i],
      rho*sig1*sig2[i], sig2[i]^2), ncol = 2)
      tmp <- MASS::mvrnorm(n_samp, Mu, S)
      log_LMAm[[i]] <- tmp[, 1]
      log_LMAs[[i]] <- tmp[, 2]
    }
    LMAs_var <- cbind(LMAs_var, map2_dbl(log_LMAs, log_LMAm, var_fun))
    b <- cbind(b, map2_dbl(log_LMAs, log_LMAm, pa_sim_fit,
      get_mean(para, "am")
    ))
  }

  LMAs_var_mean <- apply(LMAs_var, 1, mean)
  mean_ <- apply(b, 1, mean)
  upr <- apply(b, 1, \(x)(quantile(x, 0.975)))
  lwr <- apply(b, 1, \(x)(quantile(x, 0.025)))
  tibble(mean = mean_, upr = upr, lwr = lwr,
              LMAs_var_mean, site = site)
}

#' @para mass_obs_dat dataframe with obsreved mass-prop
#' @para sim1 GLOPNET
#' @para sim2 Panama sun
#' @para sim3 Panama shade
mass_prop_point <- function(mass_obs_dat, sim1, sim2, sim3) {
  # targets::tar_load(mass_obs_dat)
  # targets::tar_load(gl_mass_prop)
  # targets::tar_load(sun_mass_prop)
  # targets::tar_load(shade_mass_prop)
  # sim1 <- gl_mass_prop
  # sim2 <- sun_mass_prop
  # sim3 <- shade_mass_prop
  sim_dat <- bind_rows(sim1, sim2, sim3)
  #sim3[20 ,3] <- -1
  #sim_dat[20 ,3] <- -1

  fills <- c("GLOPNET" = "#008B00",
              "Sun" = "#1874CD",
              "Shade" = "gray"
  )

  cols <- c("GLOPNET" = "#008B00",
              "Sun" = "#1874CD",
              "Shade" = "black"
  )

  ggplot(data = sim_dat) +
    geom_ribbon(aes(ymin = lwr, ymax = upr,
                    x = LMAs_var_mean,
                    fill = site),
                alpha = 0.4)  +
    geom_line(aes(y = mean, x = LMAs_var_mean, col = site)) +
    geom_point(data = mass_obs_dat , aes(x = vars, y = b,
     shape = site, col = site)) +
    scale_fill_manual(values = fills, name = "Parameter") +
    scale_y_continuous(breaks = c(0, 0.5, 1, 2)) +
    scale_color_manual(values = cols, name = "Parameter") +
    scale_shape_discrete(name = "Parameter")  +
    ylab(expression(Mass~dependency~(italic(b)))) +
    xlab("Relative variance of LMAs (%)") +
    theme_LES() +
    theme(legend.position = c(0.75, 0.66),
          legend.key.size = unit(0.5, "cm"),
          legend.spacing.y = unit(0.1, "cm"),
          legend.text.align = 0,
          legend.key.height = unit(0.2, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)
    )
}

#' @title mass deps with changing am and as
#' @para am_sim_dat e.g., mass_prop_grad_am
#' @para as_sim_dat e.g., mass_prop_grad_as
mass_sim_point <- function(am_sim_dat, as_sim_dat) {
  p1 <- ggplot(data = am_sim_dat) +
    geom_ribbon(aes(ymin = lwr, ymax = upr,
                    x = LMAs_var_mean,
                    fill = factor(am)),
                alpha = 0.4)  +
    geom_line(aes(y = mean, x = LMAs_var_mean, col = factor(am))) +
    labs(
      color = expression(alpha[m]),
      fill = expression(alpha[m]))

  p2 <- ggplot(data = as_sim_dat) +
    geom_ribbon(aes(ymin = lwr, ymax = upr,
                    x = LMAs_var_mean,
                    fill = factor(as)),
                alpha = 0.4)  +
    geom_line(aes(y = mean, x = LMAs_var_mean, col = factor(as))) +
    labs(
      color = expression(alpha[s]),
      fill = expression(alpha[s]))

  p1 + p2 +
    plot_annotation(tag_levels = "a",
      tag_prefix = "(",
      tag_suffix = ")") &
    ylab(expression(Mass~dependency~(italic(b)))) &
    xlab("Relative variance of LMAs (%)") &
    theme_LES() &
    theme(legend.title = element_text(size = 8))
}

#' @para sim1 Panama shade mv
#' @para sim2 Panama shade normal
mass_prop_comp_point <- function(sim1, sim2) {
  sim1 <- sim1 |>
    mutate(site = "MVN")
  sim2 <- sim2 |>
    mutate(site = "N")
  sim_dat <- bind_rows(sim1, sim2)

  ggplot(data = sim_dat) +
    geom_ribbon(aes(ymin = lwr, ymax = upr,
                    x = LMAs_var_mean,
                    fill = site),
                alpha = 0.4)  +
    geom_line(aes(y = mean, x = LMAs_var_mean, col = site)) +
#    scale_fill_manual(values = fills, name = "Parameter") +
    scale_y_continuous(breaks = c(0, 0.5, 1, 2)) +
    # scale_color_manual(values = cols, name = "Parameter") +
    # scale_shape_discrete(name = "Parameter")  +
    ylab(expression(Mass~dependency~(italic(b)))) +
    xlab("Relative variance of LMAs (%)") +
    labs(color = "Model", fill = "Model") +
    theme_LES() +
    theme(legend.position = c(0.75, 0.66),
          legend.key.size = unit(0.5, "cm"),
          legend.spacing.y = unit(0.1, "cm"),
          legend.text.align = 0,
          legend.key.height = unit(0.2, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)
    )
}
