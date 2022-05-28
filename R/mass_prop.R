
gen_point_dat <- function() {
  targets::tar_load(pa_res_csv)
  targets::tar_load(gl_res_csv)
  pa <- read_csv(pa_res_csv)
  gl <- read_csv(gl_res_csv)

  targets::tar_load(fit_7_summary_GL_Aps_LLs)
  gl_para <- fit_7_summary_GL_Aps_LLs

  targets::tar_load(fit_20_summary_PA_Ap_LLs_opt)
  pa_para <- fit_20_summary_PA_Ap_LLs_opt

  sun <- pa |>
    filter(strata == "CAN")
  shade <- pa |>
    filter(strata != "CAN")

  get_mean <- function(data, para_str)  {
   data |>
    filter(variable == para_str) |>
    pull(mean)
  }

  point_dat <- tribble(~ type, ~ ap, ~ as,
  "GLOPNET", get_mean(gl_para, "ap"), get_mean(gl_para, "as"),
  "sun", get_mean(pa_para, "ap"), 0,
  "shade", get_mean(pa_para, "ap"), 0)

  gl_b <- lm(log(Aarea) ~ log(LMA), gl)$coefficients[2]
  sun_b <- lm(log(Aarea) ~ log(LMA), sun)$coefficients[2]
  shade_b <- lm(log(Aarea) ~ log(LMA), shade)$coefficients[2]

  point_dat  |>
    mutate(b = c(gl_b, sun_b, shade_b))
}


my_rlnorm <- function(n, mu, sigma)rlnorm(n, mu - 0.5 * sigma^2, sigma)

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

gl_sim_fit <- function(LMAs, LMAp, a0, ap, as) {
  A <- exp(a0 + ap * log(LMAp) + as * log(LMAs))
  fit <- lm(log(A) ~ log(LMAp + LMAs))
  fit$coefficients[2]
}

#' @para ap only ap was significant
pa_sim_fit <- function(LMAs, LMAp, ap) {
  A <- exp(ap * log(LMAp))
  fit <- lm(log(A) ~ log(LMAp + LMAs))
  fit$coefficients[2]
}

var_fun <- function(LMAs, LMAp) {
  LMA <- LMAp + LMAs
  cov(LMA, LMAs) / var(LMA) * 100
}

get_mean <- function(data, para_str)  {
 data |>
  filter(variable == para_str) |>
  pull(mean)
}

#' @para data gl_res_csv or pa_res_csv
#' @para para mcmc summary (e.g.  fit_7_summary_GL_Aps_LLs)
mass_prop_sim <- function(data, para, n_sim = 1000, n_samp = 100,
                   x_len = 20, site_name = "GLOPNET", gl = TRUE, seed = 123) {
  set.seed(seed)
  mu <- mu_fun(data$LMAp)
  mu2 <- mu_fun(data$LMAs)
  sig <- sig_fun(data$LMAp)
  LMAs_var <- NULL
  b <- NULL
  for (i in 1:n_sim) {
    LMAp <- rlnorm(n_samp, mu, sig)
    tmp <- seq(log(1.01), log(10), length = x_len)
    LMAs <- map(tmp, \(x) rlnorm(n_samp, mu2, x))
    LMAs_var <- cbind(LMAs_var, map_dbl(LMAs, var_fun, LMAp))
    if (gl) {
      b <- cbind(b, map_dbl(LMAs, gl_sim_fit,
        LMAp,
        get_mean(para, "a0"),
        get_mean(para, "ap"),
        get_mean(para, "as")
      ))
    } else {
      b <- cbind(b, map_dbl(LMAs, pa_sim_fit,
        LMAp,
        get_mean(para, "ap")
      ))
    }
  }

  LMAs_var_mean <- apply(LMAs_var, 1, mean)
  mean_ <- apply(b, 1, mean)
  upr <- apply(b, 1, \(x)(quantile(x, 0.975)))
  lwr <- apply(b, 1, \(x)(quantile(x, 0.025)))
  tibble(mean = mean_, upr = upr, lwr = lwr,
              LMAs_var_mean, site = site_name)
}
