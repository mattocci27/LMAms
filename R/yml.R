#' @title quantile
quant_fun <- function(x) c(mean = mean(x),
                           quantile(x, 0.025),
                           quantile(x, 0.975)) |> round(2)

#' @title Generates yml file for r-vaules
write_r2 <- function(gl_res_csv, gl_draws, pa_res_csv, pa_draws) {
  registerDoParallel(cores = 24)
  # targets::tar_load(fit_7_draws_GL_Aps_LLs)
  # targets::tar_load(gl_res_csv)
  GL <- read_csv(gl_res_csv)

  # gl_draws <- fit_7_draws_GL_Aps_LLs
  gl_draws <- gl_draws |>
    janitor::clean_names()
  N <- nrow(gl_draws)
  pmat <- gl_draws |>
    dplyr::select(contains("p_")) |>
    as.matrix()

  res_fun <- \(i){
    LMAp <- pmat[i,] * GL$LMA
    LMAs <- GL$LMA - LMAp

    fit_m <- lm(log(LMAs) ~ log(LMAp))
    fit_s <- lm(log(LMAp) ~ log(LMAs))
    fit_Am <- lm(log(GL$Aarea) ~ log(LMAp))
    fit_As <- lm(log(GL$Aarea) ~ log(LMAs))
    fit_Rm <- lm(log(GL$Rarea) ~ log(LMAp))
    fit_Rs <- lm(log(GL$Rarea) ~ log(LMAs))
    fit_Lm <- lm(log(GL$LL) ~ log(LMAp))
    fit_Ls <- lm(log(GL$LL) ~ log(LMAs))

    res_m  <- residuals(fit_m)
    res_s  <- residuals(fit_s)
    res_Am  <- residuals(fit_Am)
    res_As  <- residuals(fit_As)
    res_Rm  <- residuals(fit_Rm)
    res_Rs  <- residuals(fit_Rs)

    r_LMAp_LMAs <- cor(log(LMAp), log(LMAs))
    r_Am <- cor(res_s, res_As)# Aarea-LMAm
    r_As <- cor(res_m, res_Am)# Aarea-LMAs
    r_Rm <- cor(res_s, res_Rs)
    r_Rs <- cor(res_m, res_Rm)
    r_Ls <- cor(log(LMAs), log(GL$LL))
    c(r_Am, r_As, r_Rm, r_Rs, r_Ls, r_LMAp_LMAs)
  }

  bb <- foreach (i = 1:N, .combine = rbind)  %dopar% res_fun(i)
  rownames(bb) <- NULL
  r_Am <- bb[,1]
  r_As <- bb[,2]
  r_Rm <- bb[,3]
  r_Rs <- bb[,4]
  r_Ls <- bb[,5]
  r_LMAp_LMAs <- bb[,6]

  GL_cor_tbl <- rbind(
    quant_fun(r_LMAp_LMAs),
    quant_fun(r_As),
    quant_fun(r_Am),
    quant_fun(r_Ls),
    #quant_fun(r_Lm),
    quant_fun(r_Rs),
    quant_fun(r_Rm)) |>
    as_tibble() |>
    mutate(name = c("LMAp_LMAs", "A_LMAs", "A_LMAp",
                    "LL_LMAs",
                   # "LL_LMAp",
                    "R_LMAs", "R_LMAp"))

  GL_LMAsLMAp <- paste0(GL_cor_tbl[1,1],
                          " [", GL_cor_tbl[1,2], ", ",
                          GL_cor_tbl[1,3], "]")
  #PA data --------------------------------------------
  # targets::tar_load(fit_20_draws_PA_Ap_LLs_opt)
  # targets::tar_load(pa_res_csv)
  PA <- read_csv(pa_res_csv)

#  pa_draws <- fit_20_draws_PA_Ap_LLs_opt
  pa_draws <- pa_draws |>
    janitor::clean_names()

  pmat <- pa_draws |>
    dplyr::select(contains("p_")) |>
    as.matrix()
  mu2 <- pa_draws |>
    dplyr::select(contains("mu")) |>
    as.matrix()

  res_fun2 <- \(i){
    LMAp <- pmat[i,] * PA$LMA
    LMAs <- PA$LMA - LMAp

    fit_m <- lm(log(LMAs) ~ log(LMAp))
    fit_s <- lm(log(LMAp) ~ log(LMAs))
    fit_Rm <- lm(log(PA$Rarea) ~ log(LMAp))
    fit_Rs <- lm(log(PA$Rarea) ~ log(LMAs))

    res_m  <- residuals(fit_m)
    res_s  <- residuals(fit_s)
    res_Rm  <- residuals(fit_Rm)
    res_Rs  <- residuals(fit_Rs)

    r_LMAp_LMAs <- cor(log(LMAp), log(LMAs))
    r_Am <- cor(log(LMAp), log(PA$Aarea))# Aarea-LMAm
    r_Rm <- cor(res_s, res_Rs)
    r_Rs <- cor(res_m, res_Rm)
    r_Ls <- cor(log(LMAs), log(PA$LL))
    c(r_Am, r_Rm, r_Rs, r_Ls, r_LMAp_LMAs)
  }

  bb2 <- foreach(i = 1:N, .combine = rbind)  %dopar% res_fun2(i)
  rownames(bb2) <- NULL
  r_Am <- bb2[,1]
  r_Rm <- bb2[,2]
  r_Rs <- bb2[,3]
  r_Ls <- bb2[,4]
  r_LMAp_LMAs <- bb2[,5]

  PA_cor_tbl <- rbind(
    quant_fun(r_LMAp_LMAs),
    quant_fun(r_Am),
    quant_fun(r_Ls),
    quant_fun(r_Rs),
    quant_fun(r_Rm)) |>
    as_tibble() |>
    mutate(name = c("LMAp_LMAs", "A_LMAp", "LL_LMAs", "R_LMAs", "R_LMAp"))

  PA_LMAsLMAp <- paste0(PA_cor_tbl[1,1],
                          " [", PA_cor_tbl[1,2], ", ",
                          PA_cor_tbl[1,3], "]")

  # R2 for LL
  mu_dat <- pa_draws |>
    dplyr::select(contains("mu_")) |>
    dplyr::select(ends_with("_2"))

  L_sigma <- pa_draws |>
    dplyr::select(contains("L_sigma")) |>
    dplyr::select(ends_with("_2"))

  var_fit <- apply(mu_dat, 1, var)
  var_res <- unlist(L_sigma[, 2]^2)
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
  writeLines(paste0("    LMAp_Aarea: 'italic(bar(rho)) == ",
                    GL_cor_tbl |>
                      filter(name == "A_LMAp") |>
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
  writeLines(paste0("    LMAp_Rarea: 'italic(bar(rho)) == ",
                    GL_cor_tbl |>
                      filter(name == "R_LMAp") |>
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
  writeLines(paste0("    LMAp_LL: 'italic(bar(r)) == NA'"),
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
  writeLines(paste0("    LMAp_Narea: 'italic(r) == ",
                    cor.test(log(GL$Narea), log(GL$LMAp))$estimate %>% round(2),"'"),
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
  writeLines(paste0("    LMAp_Parea: 'italic(r) == ",
                    cor.test(log(GL$Parea), log(GL$LMAp))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Parea: 'italic(r) == ",
                    cor.test(log(GL$Parea), log(GL$LMAs))$estimate %>% round(2),"'"),
             out,
             sep = "\n")


  writeLines(paste0("  GL_LMAps:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_LMAp: ", "'", GL_LMAsLMAp, "'"),
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
  writeLines(paste0("    LMAp_Aarea: 'italic(bar(r)) == ",
                    PA_cor_tbl |>
                      filter(name == "A_LMAp") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_Aarea: 'italic(bar(r)) == NA'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Rarea: 'italic(r) == ",
                    cor.test(log(PA$Rarea), log(PA$LMA))$estimate |> round(2),
                    "'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAp_Rarea: 'italic(bar(rho)) == ",
                    PA_cor_tbl |>
                      filter(name == "R_LMAp") |>
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
  writeLines(paste0("    LMAp_LL: 'italic(bar(r)) == NA'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_LL: 'italic(bar(r)) == ",
                    PA_cor_tbl |>
                      filter(name == "LL_LMAs") |>
                      pull(mean),
                    "'"),
             out,
             sep = "\n")


  writeLines(paste0("  PA_LMAps:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAs_LMAp: ", "'", PA_LMAsLMAp, "'"),
             out,
             sep = "\n")


  writeLines(paste0("  PA_NP:"),
             out,
             sep = "\n")
  writeLines(paste0("    LMA_Narea: 'italic(r) == ",
                    cor.test(log(PA$Narea), log(PA$LMA))$estimate %>% round(2),"'"),
             out,
             sep = "\n")
  writeLines(paste0("    LMAp_Narea: 'italic(r) == ",
                    cor.test(log(PA$Narea), log(PA$LMAp))$estimate %>% round(2),"'"),
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
  writeLines(paste0("    LMAp_Parea: 'italic(r) == ",
                    cor.test(log(PA$Parea), log(PA$LMAp))$estimate %>% round(2),"'"),
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
  writeLines(paste0("    LMAp_cell_area: 'italic(r) == ",
                    cor.test(log(PA$cell_area), log(PA$LMAp))$estimate %>% round(2),"'"),
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
# writeLines(paste0("    A_R2: '",
#            bayes_R2_PA("A", chr = TRUE),
#            "'"),
#            out,
#            sep = "\n")
# writeLines(paste0("    R_R2: '",
#            bayes_R2_PA("R", chr = TRUE),
#            "'"),
#            out,
#            sep = "\n")
  close(out)
  paste("yml/r_val.yml")
}
