library(rstan)
library(tidyverse)

## -------------------------------------------------------------------------------------------------
load("./rda/GL_Aps_LLs_obs.rda")
GL <- dat
GLres <- res
pmat <- rstan::extract(res, "p")[[1]]
n <- 4000

LMAp_var <- numeric(n)
LMAs_var <- numeric(n)

for (i in 1:n) {
  LMAp <- pmat[i,] * GL$LMA
  LMAs <- (1 - pmat[i,]) * GL$LMA
  LMAp_var[i] <- cov(LMAp, GL$LMA)
  LMAs_var[i] <- cov(LMAs, GL$LMA)
}

# additional variance partition 
#LMAp <- apply(pmat, 2, mean) * GL$LMA
#GL2 <- GL |>
#  mutate(LMAp) |>
#  mutate(LMAs = LMA - LMAp) |>
#  filter(DE != "U")
#
#library(rstanarm)
#rstan::rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores()) # Run on multiple cores
#
#fit_stan <- stan_lmer(log(LMAs) ~ 1 + (1 | DE), GL2,
#                       seed = 123,
#                       adapt_delta = 0.99)
#as_tibble(VarCorr(fit_stan))
#
#fit_stan2 <- stan_lmer(log(LMAp) ~ 1 + (1 | DE), GL2,
#                       seed = 123,
#                       adapt_delta = 0.99)
#
#as_tibble(VarCorr(fit_stan2))

## -------------------------------------------------------------------------------------------------
load("./rda/PA_Ap_LLs_opt_more_obs.rda")
PA <- dat |>
  mutate(tmp = 1:n())
PAres <- res
pmat <- rstan::extract(res, "p")[[1]]
n <- 4000

sun <- PA |> filter(strata == "CAN")
shade <- PA |> filter(strata != "CAN")

#LMAp_PA_var <- numeric(n)
LMAp_sun_var <- numeric(n)
LMAs_sun_var <- numeric(n)

for (i in 1:n) {
  LMAp_sun <- pmat[i, sun$tmp] * sun$LMA
  LMAs_sun <- (1 - pmat[i, sun$tmp]) * sun$LMA
  LMAp_sun_var[i] <- cov(LMAp_sun, sun$LMA)
  LMAs_sun_var[i] <- cov(LMAs_sun, sun$LMA)
}

#hist(LMAs_sun_var / var(sun$LMA) * 100)
#hist(LMAp_sun_var / var(sun$LMA) * 100)
#
#summary(LMAs_sun_var / var(sun$LMA) * 100)
#summary(LMAp_sun_var / var(sun$LMA) * 100)

LMAp_sun <- apply(1 - pmat[, sun$tmp], 2, median) * sun$LMA 
#cov(LMAp_sun, sun$LMA) / var(sun$LMA)



## -------------------------------------------------------------------------------------------------
LMAp_shade_var <- numeric(n)
LMAs_shade_var <- numeric(n)

for (i in 1:n) {
  LMAp_shade <- pmat[i, shade$tmp] * shade$LMA
  LMAs_shade <- (1 - pmat[i, shade$tmp]) * shade$LMA
  LMAp_shade_var[i] <- cov(LMAp_shade, shade$LMA)
  LMAs_shade_var[i] <- cov(LMAs_shade, shade$LMA)
}


LMAp_PA_var <- numeric(n)
LMAs_PA_var <- numeric(n)

for (i in 1:n) {
  LMAp_PA <- pmat[i,] * PA$LMA
  LMAs_PA <- (1 - pmat[i, ]) * PA$LMA
  LMAp_PA_var[i] <- cov(LMAp_PA, PA$LMA)
  LMAs_PA_var[i] <- cov(LMAs_PA, PA$LMA)
}

#hist(LMAs_PA_var / var(PA$LMA) * 100)
#hist(LMAp_PA_var / var(PA$LMA) * 100)
#summary(LMAs_PA_var / var(PA$LMA) * 100)
#summary(LMAp_PA_var / var(PA$LMA) * 100)
#
#plot(LMAs_PA_var/var(PA$LMA), LMAp_PA_var/var(PA$LMA))
#
#lm(log(Aarea) ~ log(LMA), PA)


LMAp <- apply(pmat, 2, mean) * PA$LMA
PA2 <- PA |>
  mutate(LMAp) |>
  mutate(LMAs = LMA - LMAp)

#library(rstanarm)
#rstan::rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores()) # Run on multiple cores
#
#fit_stan <- stan_lmer(log(LMAs) ~ 1 + (1 | site/strata), PA2,
#                       seed = 123,
#                       adapt_delta = 0.99)
#as_tibble(VarCorr(fit_stan))
#
#aov(log(LMAp) ~ site + strata, PA2) |> summary()
#tmp <- aov(log(LMAs) ~ site + strata, PA2) |> summary()
#
#tmp[[1]][2] / sum(tmp[[1]][[2]]) * 100
#
#
#fit_stan2 <- stan_lmer(log(LMAp) ~ 1 + (1 | DE), PA2,
#                       seed = 123,
#                       adapt_delta = 0.99)
#
#as_tibble(VarCorr(fit_stan2))

## -------------------------------------------------------------------------------------------------
# R values


output <- "var_val.yml"
out <- file(paste(output), "w") # write
writeLines(paste0("GL: ",
  median(LMAs_var / var(GL$LMA) * 100) |> round(0)),
           out,
           sep = "\n")
writeLines(paste0("sun: ",
  median(LMAs_sun_var / var(sun$LMA) * 100) |> round(0)),
           out,
           sep = "\n")
writeLines(paste0("shade: ",
  median(LMAs_shade_var / var(shade$LMA) * 100) |> round(0)),
           out,
           sep = "\n")
writeLines(paste0("PA: ",
  median(LMAs_PA_var / var(PA$LMA) * 100) |> round(0)),
           out,
           sep = "\n")
close(out)


