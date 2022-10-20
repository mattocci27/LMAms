#' @title quantile
quant_fun <- function(x) c(q50 = median(x),
                           quantile(x, 0.025),
                           quantile(x, 0.975)) |>
                           round(2) |> format(nsmall = 2)


#' @title Generates yml file for r-vaules
write_r2 <- function(gl_res_csv, gl_draws, pa_res_csv, pa_draws) {
  # library(foreach)
  # library(doParallel)
  # gl_draws <- tar_read(gl_draws_ams_bs)
  # pa_draws <- tar_read(pa_full_draws_am_bs_opt)
  # targets::tar_load(gl_res_csv)
  # targets::tar_load(pa_res_csv)

  registerDoParallel(cores = 24)

  GL <- read_csv(gl_res_csv)
  PA <- read_csv(pa_res_csv)

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

    fit_Nm <- lm(log(GL$Narea) ~ log_LMAm)
    fit_Ns <- lm(log(GL$Narea) ~ log_LMAs)
    fit_Pm <- lm(log(GL$Parea) ~ log_LMAm)
    fit_Ps <- lm(log(GL$Parea) ~ log_LMAs)

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
    r_Nm <- cor.test(log_LMAm, log(GL$Narea))$estimate
    r_Ns <- cor.test(log_LMAs, log(GL$Narea))$estimate
    r_Pm <- cor.test(log_LMAm, log(GL$Parea))$estimate
    r_Ps <- cor.test(log_LMAs, log(GL$Parea))$estimate

    c(r_Am, r_As, r_Rm, r_Rs, r_Ls, r_LMAm_LMAs, r_Nm, r_Ns, r_Pm, r_Ps)

  }

  bb <- foreach (i = 1:n, .combine = rbind) %dopar% res_fun(i)
  rownames(bb) <- NULL
  r_Am <- bb[,1]
  r_As <- bb[,2]
  r_Rm <- bb[,3]
  r_Rs <- bb[,4]
  r_Ls <- bb[,5]
  r_LMAm_LMAs <- bb[,6]
  r_Nm <- bb[,7]
  r_Ns <- bb[,8]
  r_Pm <- bb[,9]
  r_Ps <- bb[,10]

  GL_cor_tbl <- rbind(
    quant_fun(r_LMAm_LMAs),
    quant_fun(r_As),
    quant_fun(r_Am),
    quant_fun(r_Ls),
    quant_fun(r_Rs),
    quant_fun(r_Rm),
    quant_fun(r_Ns),
    quant_fun(r_Nm),
    quant_fun(r_Ps),
    quant_fun(r_Pm)
    ) |>
    as_tibble() |>
    mutate(name = c("LMAm_LMAs", "A_LMAs", "A_LMAm",
                    "LL_LMAs",
                   # "LL_LMAm",
                    "R_LMAs", "R_LMAm",
                    "N_LMAs", "N_LMAm",
                    "P_LMAs", "P_LMAm"
                    ))

  GL_LMAsLMAm <- paste0(GL_cor_tbl[1, 1],
                          " [", GL_cor_tbl[1, 2], ", ",
                          GL_cor_tbl[1, 3], "]")
  # PA data --------------------------------------------

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

  res_s <- apply(res_s_mat, 1, median)
  fit_Ls <- lm(log(PA$LL) ~ light)
  res_Ls <- residuals(fit_Ls)

  par_LMAs_LL <- cor(res_s, res_Ls) |> round(2)
  # ------------------------------------

  res_fun2 <- function(i){
    log_LMAm <- log_LMAm_mat[i,]
    log_LMAs <- log_LMAs_mat[i,]

    fit_m <- lm(log_LMAs ~ log_LMAm)
    fit_s <- lm(log_LMAm ~ log_LMAs)
    fit_Rm <- lm(log(PA$Rarea) ~ log_LMAm)
    fit_Rs <- lm(log(PA$Rarea) ~ log_LMAs)

    fit_Nm <- lm(log(PA$Narea) ~ log_LMAm)
    fit_Ns <- lm(log(PA$Narea) ~ log_LMAs)
    fit_Pm <- lm(log(PA$Parea) ~ log_LMAm)
    fit_Ps <- lm(log(PA$Parea) ~ log_LMAs)
    fit_Cm <- lm(log(PA$cell_area) ~ log_LMAm)
    fit_Cs <- lm(log(PA$cell_area) ~ log_LMAs)

    res_m  <- residuals(fit_m)
    res_s  <- residuals(fit_s)
    res_Rm  <- residuals(fit_Rm)
    res_Rs  <- residuals(fit_Rs)
    res_Nm  <- residuals(fit_Nm)
    res_Ns  <- residuals(fit_Ns)
    res_Pm  <- residuals(fit_Pm)
    res_Ps  <- residuals(fit_Ps)
    res_Cm  <- residuals(fit_Cm)
    res_Cs  <- residuals(fit_Cs)

    cell_num <- names(res_Cm) |> as.numeric()
    p_num <- names(res_Pm) |> as.numeric()
    n_num <- names(res_Nm) |> as.numeric()


    r_LMAm_LMAs <- cor(log_LMAm, log_LMAs)
    r_Am <- cor(log_LMAm, log(PA$Aarea))# Aarea-LMAm
    r_Rm <- cor(res_s, res_Rs)
    r_Rs <- cor(res_m, res_Rm)
    r_Ls <- cor(log_LMAs, log(PA$LL))
    r_Nm <- cor.test(log_LMAm, log(PA$Narea))$estimate
    r_Ns <- cor.test(log_LMAs, log(PA$Narea))$estimate
    r_Pm <- cor.test(log_LMAm, log(PA$Parea))$estimate
    r_Ps <- cor.test(log_LMAs, log(PA$Parea))$estimate
    r_Cm <- cor.test(log_LMAm, log(PA$cell_area))$estimate
    r_Cs <- cor.test(log_LMAs, log(PA$cell_area))$estimate
    rho_Nm <- cor(res_s[n_num], res_Ns)
    rho_Ns <- cor(res_m[n_num], res_Nm)
    rho_Pm <- cor(res_s[p_num], res_Ps)
    rho_Ps <- cor(res_m[p_num], res_Pm)
    rho_Cm <- cor(res_s[cell_num], res_Cs)
    rho_Cs <- cor(res_m[cell_num], res_Cm)

    c(r_Am, r_Rm, r_Rs, r_Ls, r_LMAm_LMAs,
      r_Nm, r_Ns, r_Pm, r_Ps, r_Cm, r_Cs,
      rho_Nm, rho_Ns, rho_Pm, rho_Ps, rho_Cm, rho_Cs)
  }

  bb2 <- foreach(i = 1:n, .combine = rbind) %dopar% res_fun2(i)
  rownames(bb2) <- NULL
  r_Am <- bb2[,1]
  r_Rm <- bb2[,2]
  r_Rs <- bb2[,3]
  r_Ls <- bb2[,4]
  r_LMAm_LMAs <- bb2[,5]
  r_Nm <- bb2[,6]
  r_Ns <- bb2[,7]
  r_Pm <- bb2[,8]
  r_Ps <- bb2[,9]
  r_Cm <- bb2[,10]
  r_Cs <- bb2[,11]
  rho_Nm <- bb2[,12]
  rho_Ns <- bb2[,13]
  rho_Pm <- bb2[,14]
  rho_Ps <- bb2[,15]
  rho_Cm <- bb2[,16]
  rho_Cs <- bb2[,17]

  PA_cor_tbl <- rbind(
    quant_fun(r_LMAm_LMAs),
    quant_fun(r_Am),
    quant_fun(r_Ls),
    quant_fun(r_Rs),
    quant_fun(r_Rm),
    quant_fun(r_Ns),
    quant_fun(r_Nm),
    quant_fun(r_Ps),
    quant_fun(r_Pm),
    quant_fun(r_Cs),
    quant_fun(r_Cm),
    quant_fun(rho_Ns),
    quant_fun(rho_Nm),
    quant_fun(rho_Ps),
    quant_fun(rho_Pm),
    quant_fun(rho_Cs),
    quant_fun(rho_Cm)) |>
    as_tibble() |>
    mutate(name = c("LMAm_LMAs", "A_LMAm", "LL_LMAs",
                    "R_LMAs", "R_LMAm",
                    "N_LMAs", "N_LMAm",
                    "P_LMAs", "P_LMAm",
                    "CL_LMAs", "CL_LMAm",
                    "N_LMAs_rho", "N_LMAm_rho",
                    "P_LMAs_rho", "P_LMAm_rho",
                    "CL_LMAs_rho", "CL_LMAm_rho"))

  PA_LMAsLMAm <- paste0(PA_cor_tbl[1, 1],
                          " [", PA_cor_tbl[1, 2], ", ",
                          PA_cor_tbl[1, 3], "]")

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
  PA_LL_R2 <- quant_fun(R2)

  # cor(pa_rho_dat$Narea_LMAs_rm, log(pa_rho_dat$Narea))
  # cor(pa_rho_dat$Narea_LMAm_rm, log(pa_rho_dat$Narea))
  # cor(pa_rho_dat$Parea_LMAs_rm, log(pa_rho_dat$Parea))

  get_cor2 <- function(x, y) {
    cor.test(x, y)$estimate |>
      round(2) |>
      format(nsmall = 2)
  }

  # get_cor2(pa_rho_dat$Parea_LMAm_rm, log(pa_rho_dat$Parea))

  # cor(pa_rho_dat$Parea_LMAm_rm, log(pa_rho_dat$Parea))
  # cor.test(pa_rho_dat$cell_area_LMAs_rm, log(pa_rho_dat$cell_area))
  # cor.test(pa_rho_dat$cell_area_LMAm_rm, log(pa_rho_dat$cell_area))
  # cor.test(pa_rho_dat$LMAs, log(pa_rho_dat$cell_area))




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
                    get_cor2(log(GL$Aarea), log(GL$LMA)),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Aarea: 'italic(bar(rho)) == ",
                    GL_cor_tbl |>
                      filter(name == "A_LMAm") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Aarea: 'italic(bar(rho)) == ",
                    GL_cor_tbl |>
                      filter(name == "A_LMAs") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Rarea: 'italic(r) == ",
                    get_cor2(log(GL$Rarea), log(GL$LMA)),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Rarea: 'italic(bar(rho)) == ",
                    GL_cor_tbl |>
                      filter(name == "R_LMAm") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Rarea: 'italic(bar(rho)) == ",
                    GL_cor_tbl |>
                      filter(name == "R_LMAs") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_LL: 'italic(r) == ",
                    get_cor2(log(GL$LL), log(GL$LMA)),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0('    LMAm_LL: "italic(bar(r)) == ', "'NA'", '"'),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_LL: 'italic(bar(r)) == ",
                    GL_cor_tbl |>
                      filter(name == "LL_LMAs") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("  GL_NP:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Narea: 'italic(r) == ",
                    get_cor2(log(GL$Narea), log(GL$LMA)), "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Narea: 'italic(bar(r)) == ",
                    GL_cor_tbl |>
                      filter(name == "N_LMAm") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Narea: 'italic(bar(r)) == ",
                    GL_cor_tbl |>
                      filter(name == "N_LMAs") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Parea: 'italic(r) == ",
                    get_cor2(log(GL$Parea), log(GL$LMA)), "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Parea: 'italic(bar(r)) == ",
                    GL_cor_tbl |>
                      filter(name == "P_LMAm") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Parea: 'italic(bar(r)) == ",
                    GL_cor_tbl |>
                      filter(name == "P_LMAs") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")

  writeLines(paste0("  GL_LMAms:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_LMAm: ", "'", GL_LMAsLMAm, "'"),
             out,
             sep = "\n")


  writeLines(paste0("  PA:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Aarea: 'italic(r) == ",
                    get_cor2(log(PA$Aarea), log(PA$LMA)),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Aarea: 'italic(bar(r)) == ",
                   PA_cor_tbl |>
                      filter(name == "A_LMAm") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0('    LMAs_Aaera: "italic(bar(r)) == ', "'NA'", '"'),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Rarea: 'italic(r) == ",
                    get_cor2(log(PA$Rarea), log(PA$LMA)),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Rarea: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "R_LMAm") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Rarea: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "R_LMAs") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_LL: 'italic(r) == ",
                    get_cor2(log(PA$LL), log(PA$LMA)),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0('    LMAm_LL: "italic(bar(r)) == ', "'NA'", '"'),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_LL: 'italic(bar(r)) == ",
                    PA_cor_tbl |>
                      filter(name == "LL_LMAs") |>
                      pull(q50),
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
                    get_cor2(log(PA$Narea), log(PA$LMA)), "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Narea: 'italic(bar(r)) == ",
                    PA_cor_tbl |>
                      filter(name == "N_LMAm") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Narea: 'italic(bar(r)) == ",
                    PA_cor_tbl |>
                      filter(name == "N_LMAs") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Parea: 'italic(r) == ",
                    get_cor2(log(PA$Parea), log(PA$LMA)), "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Parea: 'italic(bar(r)) == ",
                    PA_cor_tbl |>
                      filter(name == "P_LMAm") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Parea: 'italic(bar(r)) == ",
                    PA_cor_tbl |>
                      filter(name == "P_LMAs") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_cell_area: 'italic(r) == ",
                    get_cor2(log(PA$cell_area), log(PA$LMA)), "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_cell_area: 'italic(bar(r)) == ",
                    PA_cor_tbl |>
                      filter(name == "CL_LMAm") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_cell_area: 'italic(bar(r)) == ",
                    PA_cor_tbl |>
                      filter(name == "CL_LMAs") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")

  writeLines(paste0("  PA_NP_par:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Narea: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "N_LMAm_rho") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Narea: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "N_LMAs_rho") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_Parea: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "P_LMAm_rho") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Parea: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "P_LMAs_rho") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAm_cell_area: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "CL_LMAm_rho") |>
                      pull(q50),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_cell_area: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "CL_LMAs_rho") |>
                      pull(q50),
                    "'"),
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
  writeLines(paste0("  PA_LL_par:"),
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

  LMAm_mu_gl <- log(gl$LMAm) |> median() |> exp() |> round(1)
  LMAs_mu_gl <- log(gl$LMAs) |> median() |> exp() |> round(1)
  LMAm_sig_gl <- log(gl$LMAm) |> sd() |> exp() |> round(2)
  rho_gl <- cor(log(gl$LMAm), log(gl$LMAs)) |> round(2)

  LMAm_mu_sun <- log(sun$LMAm) |> median() |> exp() |> round(1)
  LMAs_mu_sun <- log(sun$LMAs) |> median() |> exp() |> round(1)
  LMAm_sig_sun <- log(sun$LMAm) |> sd() |> exp() |> round(2)
  rho_sun <- cor(log(sun$LMAm), log(sun$LMAs)) |> round(2)

  LMAm_mu_shade <- log(shade$LMAm) |> median() |> exp() |> round(1)
  LMAs_mu_shade <- log(shade$LMAs) |> median() |> exp() |> round(1)
  LMAm_sig_shade <- log(shade$LMAm) |> sd() |> exp() |> round(2)
  rho_shade <- cor(log(shade$LMAm), log(shade$LMAs)) |> round(2)

  # targets::tar_load(fit_7_summary_GL_Aps_LLs)
  # fit_summary <- fit_7_summary_GL_Aps_LLs

  a0 <- gl_summary |> filter(variable == "a0") |> pull(q50) |> round(2)
  am <- gl_summary |> filter(variable == "am") |> pull(q50) |> round(2)
  as <- gl_summary |> filter(variable == "as") |> pull(q50) |> round(2)
  sig1 <- gl_summary |> filter(variable == "L_sigma[1]") |> pull(q50) |> round(2)

  a0_pa <- pa_summary |> filter(variable == "a0") |> pull(q50) |> round(2)
  am_pa <- pa_summary |> filter(variable == "am") |> pull(q50) |> round(2)
  as_pa <- pa_summary |> filter(variable == "as") |> pull(q50) |> round(2)
  sig1_pa <- pa_summary |> filter(variable == "L_sigma[1]") |> pull(q50) |> round(2)

  output <- "yml/para.yml"
  out <- file(paste(output), "w") # write
  writeLines(paste0("GL:"),
             out,
             sep = "\n")
  writeLines(paste0("  a0: ", a0),
             out,
             sep = "\n")
  writeLines(paste0("  am: ", am),
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
  writeLines(paste0("  rho_gl: ", rho_gl),
             out,
             sep = "\n")

  writeLines(paste0("PA:"),
             out,
             sep = "\n")
  writeLines(paste0("  a0: ", a0_pa),
             out,
             sep = "\n")
  writeLines(paste0("  am: ", am_pa),
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
  writeLines(paste0("  rho_sun: ", rho_sun),
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
  writeLines(paste0("  rho_shade: ", rho_shade),
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
     median() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("GL_DE: ",
    sapply(1:niter, loop_fun, pmat_fun(gl_draws, gl_res_dat2), gl_res_dat2$LMA) |>
     median() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("sun: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, sun), sun$LMA) |>
      median() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("shade: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, shade), shade$LMA) |>
      median() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("PA: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, pa_res_dat), pa_res_dat$LMA) |>
      median() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("sun_intra: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, sun_intra), sun_intra$LMA) |>
      median() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("shade_intra: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, shade_intra), shade_intra$LMA) |>
      median() |> round(1)),
             out,
             sep = "\n")
  writeLines(paste0("PA_intra: ",
    sapply(1:niter, loop_fun, pmat_fun(pa_full_draws, pa_res_dat_intra), pa_res_dat_intra$LMA) |>
      median() |> round(1)),
             out,
             sep = "\n")
  close(out)
  paste(output)
}

#' @title Write min and max LMA
write_lma_yml <- function(gl_csv_raw, pa_full_csv_raw, output) {
  gl <- read_csv(gl_csv_raw) |>
    dplyr::select(LMA) |>
    mutate(site = "GLOPNET")
  pa <- read_csv(pa_full_csv_raw) |>
    dplyr::select(LMA) |>
    mutate(site = "Panama")

  dat <- bind_rows(gl, pa ) |>
    group_by(site) |>
    summarise(min = min(LMA), max = max(LMA)) |>
    mutate(min = round(min, 1) |> format(nsmall = 1)) |>
    mutate(max = round(max, 0))

  out <- file(paste(output), "w") # write
  writeLines(paste0("GL:"), out, sep = "\n")
  writeLines(paste0("  min: ", dat$min[1]), out, sep = "\n")
  writeLines(paste0("  max: ", dat$max[1]), out, sep = "\n")
  writeLines(paste0("PA:"), out, sep = "\n")
  writeLines(paste0("  min: ", dat$min[2]), out, sep = "\n")
  writeLines(paste0("  max: ", dat$max[2]), out, sep = "\n")
  close(out)
  paste(output)
}

write_frac_yml <- function(gl_box_dat, pa_inter_box_dat, pa_intra_box_dat) {
  gl <- gl_box_dat |>
    dplyr::select(frac, leaf_habit) |>
    mutate(site_strata = NA) |>
    mutate(site = "gl")
  pa <- pa_inter_box_dat |>
    dplyr::select(frac, leaf_habit, site_strata) |>
    mutate(site = "pa_inter")
  pa2 <- pa_intra_box_dat |>
    dplyr::select(frac, leaf_habit, site_strata) |>
    mutate(site = "pa_intra")

  dat <- bind_rows(gl, pa, pa2) |>
    mutate(site_strata = str_replace_all(site_strata, "CAN", "sun")) |>
    mutate(site_strata = str_replace_all(site_strata, "UNDER", "shade")) |>
    mutate(site_strata = str_to_lower(site_strata))

  dat1 <- dat |>
    group_by(site, leaf_habit) |>
    summarise(mid = median(frac * 100) |> round(1))

  output <- "yml/frac_de.yml"
  out <- file(paste(output), "w") # write
  writeLines(paste0("gl:"), out, sep = "\n")
  writeLines(paste0("  dec: ", dat1 |> filter(site == "gl" & leaf_habit == "D") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("  eve: ", dat1 |> filter(site == "gl" & leaf_habit == "E") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("pa_inter:"), out, sep = "\n")
  writeLines(paste0("  dec: ", dat1 |> filter(site == "pa_inter" & leaf_habit == "D") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("  eve: ", dat1 |> filter(site == "pa_inter" & leaf_habit == "E") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("pa_intra:"), out, sep = "\n")
  writeLines(paste0("  dec: ", dat1 |> filter(site == "pa_intra" & leaf_habit == "D") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("  eve: ", dat1 |> filter(site == "pa_intra" & leaf_habit == "E") |> pull(mid)),
    out,
    sep = "\n")
  close(out)

  dat2 <- dat |>
    group_by(site, site_strata) |>
    summarise(mid = median(frac * 100) |> round(1))

  output <- "yml/frac_light.yml"
  out <- file(paste(output), "w") # write
  writeLines(paste0("pa_inter:"), out, sep = "\n")
  writeLines(paste0("  sun_dry: ", dat2 |> filter(site == "pa_inter" & site_strata == "dry_sun") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("  shade_dry: ", dat2 |> filter(site == "pa_inter" & site_strata == "dry_shade") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("  sun_wet: ", dat2 |> filter(site == "pa_inter" & site_strata == "wet_sun") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("  shade_wet: ", dat2 |> filter(site == "pa_inter" & site_strata == "wet_shade") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("pa_intra:"), out, sep = "\n")
  writeLines(paste0("  sun_dry: ", dat2 |> filter(site == "pa_intra" & site_strata == "dry_sun") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("  shade_dry: ", dat2 |> filter(site == "pa_intra" & site_strata == "dry_shade") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("  sun_wet: ", dat2 |> filter(site == "pa_intra" & site_strata == "wet_sun") |> pull(mid)),
    out,
    sep = "\n")
  writeLines(paste0("  shade_wet: ", dat2 |> filter(site == "pa_intra" & site_strata == "wet_shade") |> pull(mid)),
    out,
    sep = "\n")
  close(out)

  paste0("yml/frac_", c("de", "light"), ".yml")
}
