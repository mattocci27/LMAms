
library(rstan)

load("./data/GL_LMAms_rand.rda")

GL_summary <- rand_res$summary[[2]]
dat_r <- rand_res$data[[2]]

GL <- dat %>%
  mutate(LMA = dat_r$LMA,
         LL = dat_r$LL,
         Aarea = dat_r$A,
         Rarea = dat_r$R) %>%
  as_data_frame %>%
  mutate(LMAp = GL_summary[stringr::str_detect(rownames(GL_summary),
                            "log_LMAp"), "X50."] %>%  exp) %>%
  mutate(LMAs = GL_summary[stringr::str_detect(rownames(GL_summary),
                            "log_LMAs"), "X50."] %>%  exp) #%>%
GL2 <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) %>%
  mutate(gr = factor(DE,
    labels = c("Deciduous",
               "Evergreen",
               "Unclassified"
                      ))) %>%
  arrange(sp)


write.csv(GL2, "./data/GL_m0_N_rand.csv", row.names = FALSE)

load("./data/PA_m1q_more_N_rand.rda")

PA_summary <- rand_res$summary[[3]]
dat_r <- rand_res$data[[3]]

PA <- dat %>%
  mutate(LMA = dat_r$LMA,
         LL = dat_r$LL,
         Aarea = dat_r$A,
         Rarea = dat_r$R) %>%
  as_data_frame %>%
  mutate(LMAp = PA_summary[str_detect(rownames(PA_summary),
                            "log_LMAp"), "X50."] %>%  exp) %>%
  mutate(LMAs = PA_summary[str_detect(rownames(PA_summary),
                            "log_LMAs"), "X50."] %>%  exp) %>%
  mutate(preLL = PA_summary[str_detect(rownames(PA_summary),
                            "mu2"), "X50."] %>%  exp) %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
  mutate(site_strata = paste(site2, strata, sep = "_"))

write.csv(PA, "./data/PA_m1q_more_N_rand.csv", row.names = FALSE)

load("./data/PA_m1q_more_NS_rand.rda")

PA_summary <- rand_res$summary[[1]]
dat_r <- rand_res$data[[1]]

PA <- dat %>%
  mutate(LMA = dat_r$LMA,
         LL = dat_r$LL,
         Aarea = dat_r$A,
         Rarea = dat_r$R) %>%
  as_data_frame %>%
  mutate(LMAp = PA_summary[str_detect(rownames(PA_summary),
                            "log_LMAp"), "X50."] %>%  exp) %>%
  mutate(LMAs = PA_summary[str_detect(rownames(PA_summary),
                            "log_LMAs"), "X50."] %>%  exp) %>%
  mutate(preLL = PA_summary[str_detect(rownames(PA_summary),
                            "mu2"), "X50."] %>%  exp) %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
  mutate(site_strata = paste(site2, strata, sep = "_"))

write.csv(PA, "./data/PA_m1q_more_NS_rand.csv", row.names = FALSE)

## R2 
y <- PA$LL %>% log
ypred <- rstan::extract(rand_res$model[[1]], "mu2")[[1]]
e <- -1 * sweep(ypred, 2, y)
var_ypred <- apply(ypred, 1, var)
var_e <- apply(e, 1, var)
r2 <- var_ypred / (var_ypred + var_e) 
hist(r2)
quantile(r2, 0.975)

moge <- ypred %>% apply(2,mean)

var_hat <- sum((y- moge)^2)
var_ <- sum((y - mean(y))^2)
var_hat / var_

apply(e,2,mean) %>% var

cor.test(log(PA$preLL), log(PA$LL))
plot(LL ~ preLL, PA, log = "xy")
