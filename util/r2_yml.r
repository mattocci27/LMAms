library(rstan)
library(tidyverse)
library(tictoc)

d <- read_csv("./data-raw/nature02403-s2.csv", skip = 10)

dd <- data.frame(Narea = 10^d$`log Narea`,
        Parea = 10^d$`log Parea`,
        sp = d$Species) %>%
        group_by(sp) %>%
        summarize(Narea = mean(Narea, na.omit = T),
            Parea = mean(Parea, na.omit = T))

load("./rda/GL_Aps_LLs_obs.rda")

resGL <- res
GL_summary <- data.frame(summary(res)$summary)
GL <- dat
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) 

mu2_vec <- paste("mu2[", 1:nrow(GL), "]", sep = "")
temp_LMAp <- GL_summary[P_vec, "mean"] * GL$LMA
temp_LMAs <- GL$LMA - temp_LMAp
temp_LL <- exp(GL_summary[mu2_vec, "mean"])

GL <- GL %>%
  mutate(LMAp = temp_LMAp) %>%
  mutate(LMAs = temp_LMAs) %>%
  mutate(preLL = temp_LL)

GL <- left_join(GL, dd, by = "sp") %>%
  mutate(gr = factor(DE,
    labels = c("Deciduous",
               "Evergreen",
               "Unclassified"
                      ))) %>%
  arrange(sp)

write_csv(GL, "./data/GL_Aps_LLs_obs.csv")

# R for each panel -----------------------------------------------------
tic()
N <- 4000
pmat <- rstan::extract(res, "p")[[1]]
r_As <- numeric(N)
r_Am <- r_Rs <- r_Rm <- r_Lm <- r_Ls <- r_As
r_LMAp_LMAs <- numeric(N)
for (i in 1:N) {
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
  res_Lm  <- residuals(fit_Lm)
  res_Ls  <- residuals(fit_Ls)

  r_LMAp_LMAs[i] <- cor(log(LMAp), log(LMAs)) 
  r_Am[i] <- cor(res_s, res_As)# Aarea-LMAm
  r_As[i] <- cor(res_m, res_Am)# Aarea-LMAs
  r_Rm[i] <- cor(res_s, res_Rs)
  r_Rs[i] <- cor(res_m, res_Rm)
  r_Lm[i] <- cor(res_s, res_Ls)
  r_Ls[i] <- cor(res_m, res_Lm)
}
toc()


quant_fun <- function(x) c(mean = mean(x), 
                           quantile(x, 0.025),
                           quantile(x, 0.975)) |> round(2)

GL_cor_tbl <- rbind(
  quant_fun(r_LMAp_LMAs),
  quant_fun(r_As),
  quant_fun(r_Am),
  quant_fun(r_Ls),
  quant_fun(r_Lm),
  quant_fun(r_Rs),
  quant_fun(r_Rm)) |>
  as_tibble() |>
  mutate(name = c("LMAp_LMAs", "A_LMAs", "A_LMAp",
                  "LL_LMAs", "LL_LMAp", "R_LMAs", "R_LMAp"))

# R2------------------------------------------------------------------------
Mu <- rstan::extract(resGL, "Mu")[[1]]
L_sigma <- rstan::extract(resGL, "L_sigma")[[1]]

bayes_R2_GL <- \(trait = c("Amax", "LL", "Rdark"), chr = FALSE){
  if (trait == "Amax") {
    var_fit <- apply(Mu[,,1], 1, var)
    var_res <- L_sigma[, 1]^2
  } else if (trait == "LL") {
    var_fit <- apply(Mu[,,2], 1, var)
    var_res <- L_sigma[, 2]^2
  } else {
    var_fit <- apply(Mu[,,3], 1, var)
    var_res <- L_sigma[, 3]^2
  }
  R2 <- var_fit / (var_fit + var_res)
  tmp <- quant_fun(R2) |> round(2)
  if (chr) {
    paste0(tmp[1], 
           " [", tmp[2], ", ",
           tmp[3], "]")

  } else {
    tmp
  }
}

bayes_R2_GL("Amax")
bayes_R2_GL("Amax", chr = TRUE)

GL_cor_tbl |>
  filter(name == "A_LMAs") |>
  pull(mean)

GL_LMAsLMAp <- paste0(GL_cor_tbl[1,1], 
                        " [", GL_cor_tbl[1,2], ", ",
                        GL_cor_tbl[1,3], "]")

#bayes_R2_GL("Amax")
#bayes_R2_GL("LL")
#bayes_R2_GL("Rdark")

#PA ==========================================================================

load("./rda/PA_Ap_LLs_opt_more_obs.rda")
resPA <- res
#summary_PA_LDL <- data.frame(summary(res)$summary)

summary_PA_LDL <- data.frame(summary(res)$summary)

PA <- dat %>%
  as_tibble %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
  mutate(site_strata = paste(site2, strata, sep = "_"))

P_vec <- paste("p[", 1:nrow(PA), "]", sep = "")
mu2_vec <- paste("mu2[", 1:nrow(PA), "]", sep = "")

#Mu_vec <- paste("mu[", 1:nrow(PA), ",2]", sep = "")
temp_LMAp <- summary_PA_LDL[P_vec, "mean"] * PA$LMA
temp_LMAs <- PA$LMA - temp_LMAp
temp_LL <- exp(summary_PA_LDL[mu2_vec, "mean"])


PA <- PA %>%
  mutate(LMAp = temp_LMAp) %>%
  mutate(LMAs = temp_LMAs) %>%
  mutate(preLL = temp_LL) %>%
  mutate(LDs = LMAs/LT/1000)

write_csv(PA, "./data/PA_Ap_LLs_opt_more.csv")

# R------------------------------------------------------------------------
# note LL and Amax are not multiple regressoins
tic()
N <- 4000
pmat <- rstan::extract(resPA, "p")[[1]]
r_As <- numeric(N)
r_Am <- r_Rs <- r_Rm <- r_Lm <- r_Ls <- r_As
r_LMAp_LMAs <- numeric(N)
for (i in 1:N) {
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

  r_LMAp_LMAs[i] <- cor(log(LMAp), log(LMAs)) 
  r_Am[i] <- cor(log(LMAp), log(PA$Aarea))# Aarea-LMAm
  r_Rm[i] <- cor(res_s, res_Rs)
  r_Rs[i] <- cor(res_m, res_Rm)
  r_Ls[i] <- cor(log(LMAs), log(PA$LL))
}
toc()


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

# R2 ------------------------------------------------------------------------
Mu2 <- rstan::extract(resPA, "Mu")[[1]]
L_sigma2 <- rstan::extract(resPA, "L_sigma")[[1]]

bayes_R2_PA <- \(trait = c("Amax", "LL", "Rdark"), chr = FALSE){
  if (trait == "Amax") {
    var_fit <- apply(Mu2[,,1], 1, var)
    var_res <- L_sigma[, 1]^2
  } else if (trait == "LL") {
    var_fit <- apply(Mu2[,,2], 1, var)
    var_res <- L_sigma[, 2]^2
  } else {
    var_fit <- apply(Mu2[,,3], 1, var)
    var_res <- L_sigma2[, 3]^2
  }
  R2 <- var_fit / (var_fit + var_res)
  tmp <- quant_fun(R2) |> round(2)
  if (chr) {
    paste0(tmp[1], 
           " [", tmp[2], ", ",
           tmp[3], "]")

  } else {
    tmp
  }
}


#bayes_R2_PA("Amax")
#bayes_R2_PA("LL")
#bayes_R2_PA("Rdark")


# R values ===================================================================
output <- "r_val.yml"
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

writeLines(paste0("  GL_R2:"),
           out,
           sep = "\n")
writeLines(paste0("    A_R2: '",
           bayes_R2_GL("A", chr = TRUE),
           "'"),
           out,
           sep = "\n")
writeLines(paste0("    LL_R2: '",
           bayes_R2_GL("LL", chr = TRUE),
           "'"),
           out,
           sep = "\n")
writeLines(paste0("    R_R2: '",
           bayes_R2_GL("R", chr = TRUE),
           "'"),
           out,
           sep = "\n")

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
                  bayes_R2_PA("LL")[[1]]
                  ,"'"),
           out,
           sep = "\n")
writeLines(paste0("    A_R2: '",
           bayes_R2_PA("A", chr = TRUE),
           "'"),
           out,
           sep = "\n")
writeLines(paste0("    R_R2: '",
           bayes_R2_PA("R", chr = TRUE),
           "'"),
           out,
           sep = "\n")
close(out)
