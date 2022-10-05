#' @title quantile
quant_fun <- function(x) c(mean = mean(x),
                           quantile(x, 0.025),
                           quantile(x, 0.975)) |> round(2)

#' @title Generates yml file for r-vaules
write_r2 <- function(gl_res_csv, gl_draws, pa_res_csv, pa_draws) {
  #registerDoParallel(cores = 24)
  # targets::tar_load(fit_7_draws_GL_Aps_LLs)
  # gl_draws <- fit_7_draws_GL_Aps_LLs
  # targets::tar_load(gl_res_csv)
  GL <- read_csv(gl_res_csv)

  log_LMAm_mat <- gl_draws |>
    dplyr::select(contains("LMAm")) |>
    as.matrix()
  log_LMAs_mat <- gl_draws |>
    dplyr::select(contains("LMAs")) |>
    as.matrix()
  n <- nrow(gl_draws)

  res_fun <- function(i){
    log_LMAm <- log_LMAm_mat[i,]
    log_LMAs <- log_LMAs_mat[i,]

    fit_m <- lm(log_LMAs ~ log_LMAm)
    fit_s <- lm(log_LMAm ~ log_LMAs)
    fit_Am <- lm(log(GL$Aarea) ~ log_LMAm)
    fit_As <- lm(log(GL$Aarea) ~ log_LMAs)
    fit_Rm <- lm(log(GL$Rarea) ~ log_LMAm)
    fit_Rs <- lm(log(GL$Rarea) ~ log_LMAs)
    fit_Lm <- lm(log(GL$LL) ~ log_LMAm)
    fit_Ls <- lm(log(GL$LL) ~ log_LMAs)

    res_m  <- residuals(fit_m)
    res_s  <- residuals(fit_s)
    res_Am  <- residuals(fit_Am)
    res_As  <- residuals(fit_As)
    res_Rm  <- residuals(fit_Rm)
    res_Rs  <- residuals(fit_Rs)

    r_LMAm_LMAs <- cor(log_LMAm, log_LMAs)
    r_Am <- cor(res_s, res_As)# Aarea-LMAm
    r_As <- cor(res_m, res_Am)# Aarea-LMAs
    r_Rm <- cor(res_s, res_Rs)
    r_Rs <- cor(res_m, res_Rm)
    r_Ls <- cor(log_LMAs, log(GL$LL))
    c(r_Am, r_As, r_Rm, r_Rs, r_Ls, r_LMAm_LMAs)
  }

  bb <- foreach (i = 1:n, .combine = rbind) %do% res_fun(i)
  rownames(bb) <- NULL
  r_Am <- bb[,1]
  r_As <- bb[,2]
  r_Rm <- bb[,3]
  r_Rs <- bb[,4]
  r_Ls <- bb[,5]
  r_LMAm_LMAs <- bb[,6]

  GL_cor_tbl <- rbind(
    quant_fun(r_LMAm_LMAs),
    quant_fun(r_As),
    quant_fun(r_Am),
    quant_fun(r_Ls),
    #quant_fun(r_Lm),
    quant_fun(r_Rs),
    quant_fun(r_Rm)) |>
    as_tibble() |>
    mutate(name = c("LMAm_LMAs", "A_LMAs", "A_LMAm",
                    "LL_LMAs",
                   # "LL_LMAm",
                    "R_LMAs", "R_LMAm"))

  GL_LMAsLMAm <- paste0(GL_cor_tbl[1,1],
                          " [", GL_cor_tbl[1,2], ", ",
                          GL_cor_tbl[1,3], "]")
  # PA data --------------------------------------------
  # library(tidyverse)
  # targets::tar_load(fit_20_draws_PA_Ap_LLs_opt)
  # pa_draws <- fit_20_draws_PA_Ap_LLs_opt
  # targets::tar_load(pa_res_csv)
  PA <- read_csv(pa_res_csv)

  log_LMAm_mat <- pa_draws |>
    dplyr::select(contains("LMAm")) |>
    as.matrix()
  log_LMAs_mat <- pa_draws |>
    dplyr::select(contains("LMAs")) |>
    as.matrix()
  # clean names at this point
  pa_draws <- pa_draws |>
    janitor::clean_names()
  mu2 <- pa_draws |>
    dplyr::select(contains("mu")) |>
    as.matrix()

  # for the partial correlation of LL vs. LMAs, controlling for light.
  light <- ifelse(PA$strata == "CAN", 1, 0)

  par_fun <- function(x, light) {
    fit_s <- lm(x ~ light)
    res_s <- residuals(fit_s)
    res_s
  }

  res_s_mat <- apply(log_LMAs_mat, 1, par_fun,  light)

  res_s <- apply(res_s_mat, 1, mean)
  fit_Ls <- lm(log(PA$LL) ~ light)
  res_Ls <- residuals(fit_Ls)

  par_LMAs_LL <- cor(res_s, res_Ls) |> round(2)
  # ------------------------------------

  res_fun2 <- \(i){
    log_LMAm <- log_LMAm_mat[i,]
    log_LMAs <- log_LMAs_mat[i,]

    fit_m <- lm(log_LMAs ~ log_LMAm)
    fit_s <- lm(log_LMAm ~ log_LMAs)
    fit_Rm <- lm(log(PA$Rarea) ~ log_LMAm)
    fit_Rs <- lm(log(PA$Rarea) ~ log_LMAs)

    res_m  <- residuals(fit_m)
    res_s  <- residuals(fit_s)
    res_Rm  <- residuals(fit_Rm)
    res_Rs  <- residuals(fit_Rs)

    r_LMAm_LMAs <- cor(log_LMAm, log_LMAs)
    r_Am <- cor(log_LMAm, log(PA$Aarea))# Aarea-LMAm
    r_Rm <- cor(res_s, res_Rs)
    r_Rs <- cor(res_m, res_Rm)
    r_Ls <- cor(log_LMAs, log(PA$LL))
    c(r_Am, r_Rm, r_Rs, r_Ls, r_LMAm_LMAs)
  }

  bb2 <- foreach(i = 1:n, .combine = rbind) %do% res_fun2(i)
  rownames(bb2) <- NULL
  r_Am <- bb2[,1]
  r_Rm <- bb2[,2]
  r_Rs <- bb2[,3]
  r_Ls <- bb2[,4]
  r_LMAm_LMAs <- bb2[,5]

  PA_cor_tbl <- rbind(
    quant_fun(r_LMAm_LMAs),
    quant_fun(r_Am),
    quant_fun(r_Ls),
    quant_fun(r_Rs),
    quant_fun(r_Rm)) |>
    as_tibble() |>
    mutate(name = c("LMAm_LMAs", "A_LMAm", "LL_LMAs", "R_LMAs", "R_LMAm"))

  PA_LMAsLMAm <- paste0(PA_cor_tbl[1,1],
                          " [", PA_cor_tbl[1,2], ", ",
                          PA_cor_tbl[1,3], "]")

  # R2 for LL
  # targets::tar_load(fit_20_draws_PA_Ap_LLs_opt)
  # pa_draws <- fit_20_draws_PA_Ap_LLs_opt |>
  #   janitor::clean_names()
  mu_dat <- pa_draws |>
    dplyr::select(contains("mu_")) |>
    dplyr::select(ends_with("_2"))

  l_sigma <- pa_draws |>
    dplyr::select(l_sigma_2)

  var_fit <- apply(mu_dat, 1, var)
  var_res <- unlist(l_sigma^2)
  #var_res <- unlist(L_sigma[, 2]^2)
  R2 <- var_fit / (var_fit + var_res)
  PA_LL_R2 <- quant_fun(R2) |> round(2)

# R values ================================================================
  output <- "yml/r_val.yml"
  out <- file(paste(output), "w") # write

  writeLines(paste0("r_vals:"),
             out,
             sep = "\n")
  writeLines(paste0("  GL:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Aarea: 'italic(r) == ",
                    cor.test(log(GL$Aarea), log(GL$LMA))$estimate |> round(2),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Aarea: 'italic(bar(rho)) == ",
                    GL_cor_tbl |>
                      filter(name == "A_LMAm") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Aarea: 'italic(bar(rho)) == ",
                    GL_cor_tbl |>
                      filter(name == "A_LMAs") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Rarea: 'italic(r) == ",
                    cor.test(log(GL$Rarea), log(GL$LMA))$estimate |> round(2),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Rarea: 'italic(bar(rho)) == ",
                    GL_cor_tbl |>
                      filter(name == "R_LMAm") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Rarea: 'italic(bar(rho)) == ",
                    GL_cor_tbl |>
                      filter(name == "R_LMAs") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_LL: 'italic(r) == ",
                    cor.test(log(GL$LL), log(GL$LMA))$estimate |> round(2),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0('    LMAm_LL: "italic(bar(r)) == ', "'NA'", '"'),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_LL: 'italic(bar(r)) == ",
                    GL_cor_tbl |>
                      filter(name == "LL_LMAs") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("  GL_NP:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Narea: 'italic(r) == ",
                    cor.test(log(GL$Narea), log(GL$LMA))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Narea: 'italic(r) == ",
                    cor.test(log(GL$Narea), log(GL$LMAm))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Narea: 'italic(r) == ",
                    cor.test(log(GL$Narea), log(GL$LMAs))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Parea: 'italic(r) == ",
                    cor.test(log(GL$Parea), log(GL$LMA))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Parea: 'italic(r) == ",
                    cor.test(log(GL$Parea), log(GL$LMAm))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Parea: 'italic(r) == ",
                    cor.test(log(GL$Parea), log(GL$LMAs))$estimate %>% round(2),"'"),
             out,
             sep = "\n")


  writeLines(paste0("  GL_LMAms:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_LMAm: ", "'", GL_LMAsLMAm, "'"),
             out,
             sep = "\n")

# writeLines(paste0("  GL_R2:"),
#            out,
#            sep = "\n")
# writeLines(paste0("    A_R2: '",
#            bayes_R2_GL("A", chr = TRUE),
#            "'"),
#            out,
#            sep = "\n")
# writeLines(paste0("    LL_R2: '",
#            bayes_R2_GL("LL", chr = TRUE),
#            "'"),
#            out,
#            sep = "\n")
# writeLines(paste0("    R_R2: '",
#            bayes_R2_GL("R", chr = TRUE),
#            "'"),
#            out,
#            sep = "\n")

  writeLines(paste0("  PA:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Aarea: 'italic(r) == ",
                    cor.test(log(PA$Aarea), log(PA$LMA))$estimate |> round(2),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Aarea: 'italic(bar(r)) == ",
                   PA_cor_tbl |>
                      filter(name == "A_LMAm") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0('    LMAs_Aaera: "italic(bar(r)) == ', "'NA'", '"'),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Rarea: 'italic(r) == ",
                    cor.test(log(PA$Rarea), log(PA$LMA))$estimate |> round(2),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Rarea: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "R_LMAm") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Rarea: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "R_LMAs") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_LL: 'italic(r) == ",
                    cor.test(log(PA$LL), log(PA$LMA))$estimate |> round(2),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0('    LMAm_LL: "italic(bar(r)) == ', "'NA'", '"'),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_LL: 'italic(bar(r)) == ",
                    PA_cor_tbl |>
                      filter(name == "LL_LMAs") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")

  writeLines(paste0("  PA_LMAms:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_LMAm: ", "'", PA_LMAsLMAm, "'"),
             out,
             sep = "\n")


  writeLines(paste0("  PA_NP:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Narea: 'italic(r) == ",
                    cor.test(log(PA$Narea), log(PA$LMA))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Narea: 'italic(r) == ",
                    cor.test(log(PA$Narea), log(PA$LMAm))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Narea: 'italic(r) == ",
                    cor.test(log(PA$Narea), log(PA$LMAs))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Parea: 'italic(r) == ",
                    cor.test(log(PA$Parea), log(PA$LMA))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Parea: 'italic(r) == ",
                    cor.test(log(PA$Parea), log(PA$LMAm))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Parea: 'italic(r) == ",
                    cor.test(log(PA$Parea), log(PA$LMAs))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_cell_area: 'italic(r) == ",
                    cor.test(log(PA$cell_area), log(PA$LMA))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_cell_area: 'italic(r) == ",
                    cor.test(log(PA$cell_area), log(PA$LMAm))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_cell_area: 'italic(r) == ",
                    cor.test(log(PA$cell_area), log(PA$LMAs))$estimate %>% round(2),"'"),
             out,
             sep = "\n")

  writeLines(paste0("  PA_R2:"),
             out,
             sep = "\n")
  writeLines(paste0("    LL_R2: 'italic(R^2) == ",
                    PA_LL_R2[1]
                    ,"'"),
             out,
             sep = "\n")
  writeLines(paste0("  PA_par:"),
             out,
             sep = "\n")
  # writeLines(paste0("    LMAs_LL: 'italic(rho) == ",
  writeLines(paste0("    LMAs_LL: 'italic(bar(\u03c1)) == ",
                    par_LMAs_LL,
                    "'"),
             out,
             sep = "\n")
  close(out)
  paste("yml/r_val.yml")
}

#' @title Generates yml file for GL estimates
#' @para gl_summary cmdstan summary (e.g., fit_7_summary_GL_Aps_LLs)
#' @para pa_summary cmdstan summary
write_para_yml <- function(gl_summary, pa_summary, gl_res_csv, pa_res_csv) {
  gl <- read_csv(gl_res_csv)
  pa <- read_csv(pa_res_csv)
  sun <- pa |>
    filter(strata == "CAN")
  shade <- pa |>
    filter(strata != "CAN")

  LMAm_mu_gl <- log(gl$LMAm) |> mean() |> exp() |> round(1)
  LMAs_mu_gl <- log(gl$LMAs) |> mean() |> exp() |> round(1)
  LMAm_sig_gl <- log(gl$LMAm) |> sd() |> exp() |> round(2)

  LMAm_mu_sun <- log(sun$LMAm) |> mean() |> exp() |> round(1)
  LMAs_mu_sun <- log(sun$LMAs) |> mean() |> exp() |> round(1)
  LMAm_sig_sun <- log(sun$LMAm) |> sd() |> exp() |> round(2)

  LMAm_mu_shade <- log(shade$LMAm) |> mean() |> exp() |> round(1)
  LMAs_mu_shade <- log(shade$LMAs) |> mean() |> exp() |> round(1)
  LMAm_sig_shade <- log(shade$LMAm) |> sd() |> exp() |> round(2)

  # targets::tar_load(fit_7_summary_GL_Aps_LLs)
  # fit_summary <- fit_7_summary_GL_Aps_LLs

  a0 <- gl_summary |> filter(variable == "a0") |> pull(mean) |> round(2)
  ap <- gl_summary |> filter(variable == "ap") |> pull(mean) |> round(2)
  as <- gl_summary |> filter(variable == "as") |> pull(mean) |> round(2)
  sig1 <- gl_summary |> filter(variable == "L_sigma[1]") |> pull(mean) |> round(2)

  a0_pa <- pa_summary |> filter(variable == "a0") |> pull(mean) |> round(2)
  ap_pa <- pa_summary |> filter(variable == "ap") |> pull(mean) |> round(2)
  as_pa <- pa_summary |> filter(variable == "as") |> pull(mean) |> round(2)
  sig1_pa <- pa_summary |> filter(variable == "L_sigma[1]") |> pull(mean) |> round(2)

  output <- "yml/para.yml"
  out <- file(paste(output), "w") # write
  writeLines(paste0("GL:"),
             out,
             sep = "\n")
  writeLines(paste0("  a0: ", a0),
             out,
             sep = "\n")
  writeLines(paste0("  ap: ", ap),
             out,
             sep = "\n")
  writeLines(paste0("  as: ", as),
             out,
             sep = "\n")
  writeLines(paste0("  sig1: ", sig1),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm_mu_gl: ", LMAm_mu_gl),
             out,
             sep = "\n")
  writeLines(paste0("  LMAs_mu_gl: ", LMAs_mu_gl),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm_sig_gl: ", LMAm_sig_gl),
             out,
             sep = "\n")

  writeLines(paste0("PA:"),
             out,
             sep = "\n")
  writeLines(paste0("  a0: ", a0_pa),
             out,
             sep = "\n")
  writeLines(paste0("  ap: ", ap_pa),
             out,
             sep = "\n")
  writeLines(paste0("  as: ", as_pa),
             out,
             sep = "\n")
  writeLines(paste0("  sig1: ", sig1_pa),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm_mu_sun: ", LMAm_mu_sun),
             out,
             sep = "\n")
  writeLines(paste0("  LMAs_mu_sun: ", LMAs_mu_sun),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm_sig_sun: ", LMAm_sig_sun),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm_mu_shade: ", LMAm_mu_shade),
             out,
             sep = "\n")
  writeLines(paste0("  LMAs_mu_shade: ", LMAs_mu_shade),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm_sig_shade: ", LMAm_sig_shade),
             out,
             sep = "\n")
  close(out)
  paste(output)
  paste("yml/para.yml")
}



#' @para draws cmdstan draws
#' @para data mcmc tibble (e.g., gl_res_dat )
pmat_fun <- function(draws, data) {
  id_dat <- data |>
    pull(id) |>
    str_split_fixed("_", 2)
  id <- id_dat[,2] |> as.numeric()
  pmat0 <- draws |>
    janitor::clean_names() |>
    dplyr::select(starts_with("p_")) |>
    as.matrix()
  pmat <- pmat0[, id]
  pmat
}

#' @title wrapper function for write_var_yml
#' @para pmat output of pmat_fun
loop_fun <- function(i, pmat, LMA) {
  LMAm <- pmat[i, ] * LMA
  LMAs <- (1 - pmat[i, ]) * LMA
  LMAm_var <- cov(LMAm, LMA)
  LMAs_var <- cov(LMAs, LMA)
  LMAs_var / var(LMA) * 100
}

#' @title write var yml
#' @para gl_draws MCMC output (e.g., gl_draws_GL_Aps_LLs)
#' @para gl_res_dat mcmc tibble
#' @para pa_full_draws MCMC output (e.g., pa_full_draws_PA_Ap_LLs_opt)
#' @para gl_pa_da mcmc tibble
write_var_yml <- function(gl_draws, gl_res_dat, pa_full_draws, pa_res_dat) {
  niter <- nrow(gl_draws)

  sun <- pa_res_dat |>
    filter(strata == "CAN")
  shade <-  pa_res_dat |>
    filter(strata != "CAN")

  gl_res_dat2 <- gl_res_dat |>
   filter(leaf_habit != "U")

  pa_res_dat_intra <- pa_res_dat |>
    count(sp) |>
    filter(n > 1) |>
    left_join(pa_res_dat)

  sun_intra <- pa_res_dat_intra |>
    filter(strata == "CAN")
  shade_intra <-  pa_res_dat_intra |>
    filter(strata != "CAN")

  output <- "yml/var.yml"
  out <- file(paste(output), "w") # write
  writeLines(paste0("GL: ",
    sapply(1:niter, loop_fun, pmat_fun(gl_draws, gl_res_dat), gl_res_dat$LMA) |>
     mean() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("GL_DE: ",
    sapply(1:niter, loop_fun, pmat_fun(gl_draws, gl_res_dat2), gl_res_dat2$LMA) |>
     mean() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("sun: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, sun), sun$LMA) |>
      mean() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("shade: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, shade), shade$LMA) |>
      mean() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("PA: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, pa_res_dat), pa_res_dat$LMA) |>
      mean() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("sun_intra: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, sun_intra), sun_intra$LMA) |>
      mean() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("shade_intra: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, shade_intra), shade_intra$LMA) |>
      mean() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("PA_intra: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, pa_res_dat_intra), pa_res_dat_intra$LMA) |>
      mean() |> round(1)),
             out,
             sep = "\n")
  close(out)
  paste(output)
}
