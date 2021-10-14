library(rstan)
library(tidyverse)

load("./rda/GL_Aps_LLs_obs.rda")

## GL ----------------------------------------------
## summary table

s_GL <- data.frame(summary(res)$summary) %>%
  mutate(para = rownames(.)) |>
  mutate(sig = ifelse(`X2.5.` < 0 & `X97.5.` < 0 ,
         "sig",
         "nonsig")) |>
  mutate(sig = ifelse(`X2.5.` > 0 & `X97.5.` > 0 ,
         "sig",
         sig))

write_csv(s_GL, "./data/GLpara.csv")

get_val <- \(data, x)  {
  data |>
    filter(para == {{x}})  |>
    pull(mean) |>
    round(2)
}


# parameter values ===================================================================
output <- "para_val.yml"
out <- file(paste(output), "w") # write
writeLines(paste0("GL:"),
           out,
           sep = "\n")
writeLines(paste0(" a0: ",
           get_val(s_GL, "a0")),
           out,
           sep = "\n")
writeLines(paste0(" ap: ",
           get_val(s_GL, "ap")),
           out,
           sep = "\n")
writeLines(paste0(" as: ",
           get_val(s_GL, "as")),
           out,
           sep = "\n")
writeLines(paste0(" sig1: ",
           get_val(s_GL, "L_sigma[1]")),
           out,
           sep = "\n")
close(out)

## dataset

GL_summary <- data.frame(summary(res)$summary) |>
  round(3)
#DT::datatable(GL_summary)
GL <- dat
p_mat <- rstan::extract(res, "p")[[1]] 
p_vec <- apply(p_mat, 2, median)

p_vec_lo <- apply(p_mat, 2, function(x)quantile(x, 0.025))
p_vec_up <- apply(p_mat, 2, function(x)quantile(x, 0.975))

GL <- GL |>
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE)))

GL <- GL |>
  mutate(LMAp = LMA * p_vec) |>
  mutate(LMAs = LMA - LMAp)  |>
  mutate(LMAp_lo = LMA * p_vec_lo) |>
  mutate(LMAp_up = LMA * p_vec_up) |>
  mutate(LMAs_lo = LMA - LMAp_up) |>
  mutate(LMAs_up = LMA - LMAp_lo)

d <- read_csv("./data-raw/nature02403-s2.csv", skip = 10)
dd <- tibble(Narea = 10^d$`log Narea`,
        Parea = 10^d$`log Parea`,
        sp = d$Species) |>
        group_by(sp) |>
        summarize(Narea = mean(Narea, na.omit = T),
            Parea = mean(Parea, na.omit = T))
GL <- left_join(GL, dd, by = "sp") |>
  mutate(gr = factor(DE,
    labels = c("Deciduous",
               "Evergreen",
               "Unclassified"
                      ))) 
write_csv(GL, "./data/GL_res.csv")

## PA ----------------------------------------------
## summary table

load("./rda/PA_Ap_LLs_opt_more_obs.rda")

s_PA <- data.frame(summary(res)$summary) %>%
  mutate(para = rownames(.)) |>
  mutate(sig = ifelse(`X2.5.` < 0 & `X97.5.` < 0 ,
         "sig",
         "nonsig")) |>
  mutate(sig = ifelse(`X2.5.` > 0 & `X97.5.` > 0 ,
         "sig",
         sig))

write_csv(s_PA, "./data/PApara.csv")


## dataset

summary_PA_LDL <- data.frame(summary(res)$summary)

PA <- dat |>
  as_tibble() |>
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) |>
  mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) |>
  mutate(site_strata = paste(site2, strata, sep = "_"))

PA_p_mat <- rstan::extract(res, "p")[[1]]
PA_p_vec <- apply(PA_p_mat, 2, median)
PA_p_vec_lo <- apply(PA_p_mat, 2, function(x)quantile(x, 0.025))
PA_p_vec_up <- apply(PA_p_mat, 2, function(x)quantile(x, 0.975))

Mu <- rstan::extract(res, "Mu")[[1]]
Mu2_mat <- Mu[, , 2]

PA <- PA |>
  mutate(LMAp = LMA * PA_p_vec) |>
  mutate(LMAs = LMA - LMAp)  |>
  mutate(LMAp_lo = LMA * PA_p_vec_lo) |>
  mutate(LMAp_up = LMA * PA_p_vec_up) |>
  mutate(LMAs_lo = LMA - LMAp_up) |>
  mutate(LMAs_up = LMA - LMAp_lo) |>
  mutate(Mu2 = apply(Mu2_mat, 2, mean)) |>
  mutate(Mu2_lo = apply(Mu2_mat, 2, \(x)quantile(x, 0.025))) |>
  mutate(Mu2_up = apply(Mu2_mat, 2, \(x)quantile(x, 0.975)))

write_csv(PA, "./data/PA_res.csv")

