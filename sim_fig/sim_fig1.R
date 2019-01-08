rm(list = ls()) # This clears everything from memory.
library(rstan)
library(dplyr)
load("/Users/mattocci/Dropbox/LES/GL_sim.RData")
library(loo)
#unif, z, z hier
waic(extract_log_lik(res1,"log_lik"))$waic
waic(extract_log_lik(res2,"log_lik"))$waic
waic(extract_log_lik(res3,"log_lik"))$waic


sigma1 <- 0.5
sigma2 <- 0.5
sigma3 <- 0.5
rho12 <- 0
rho13 <- 0.7
rho23 <- 0
log_alpha <- -1.46
log_beta <- -1.47
L_sigma <- NULL
"L_sigma[1]" <- sigma1
"L_sigma[2]" <- sigma2
"L_sigma[3]" <- sigma3

rp <- 0.02
rs <- 0.001


GL_NH <- data_frame(alpha = extract(res1, pars = "log_alpha")[[1]] %>% exp,
      beta = extract(res1, pars = "log_beta")[[1]] %>% exp,
      rp = extract(res1, pars = "rp")[[1]],
      rs = extract(res1, pars = "rs")[[1]],
      rho12 = extract(res1, pars = "rho12")[[1]],
      rho13 = extract(res1, pars = "rho13")[[1]],
      rho23 = extract(res1, pars = "rho23")[[1]],
      sigma1 = extract(res1, pars = "L_sigma[1]")[[1]],
      sigma2 = extract(res1, pars = "L_sigma[2]")[[1]],
      sigma3 = extract(res1, pars = "L_sigma[3]")[[1]]) %>%
      mutate(model = "Non-hierarchical")

GL_H <- data_frame(alpha = extract(res3, pars = "log_alpha")[[1]] %>% exp,
      beta = extract(res3, pars = "log_beta")[[1]] %>% exp,
      rp = extract(res3, pars = "rp")[[1]],
      rs = extract(res3, pars = "rs")[[1]],
      rho12 = extract(res3, pars = "rho12")[[1]],
      rho13 = extract(res3, pars = "rho13")[[1]],
      rho23 = extract(res3, pars = "rho23")[[1]],
      sigma1 = extract(res3, pars = "L_sigma[1]")[[1]],
      sigma2 = extract(res3, pars = "L_sigma[2]")[[1]],
      sigma3 = extract(res3, pars = "L_sigma[3]")[[1]]) %>%
      mutate(model = "Hierarchical")

GL <- bind_rows(GL_NH, GL_H) %>%
  tidyr::gather("est", "val", 1:10) %>%
  mutate(est2 = factor(est,
    levels = c("alpha", "beta", "rp", "rs", "rho12", "rho13", "rho23",
          "sigma1", "sigma2", "sigma3"))) %>%
  mutate(model = factor(model, levels = c("Non-hierarchical", "Hierarchical")))

vline_dat <- data_frame(est2 = c("alpha", "beta", "rp", "rs",
            "rho12", "rho13", "rho23",
            "sigma1", "sigma2", "sigma3"),
            true_val = c(exp(log_alpha), exp(log_beta), rp, rs,
            rho12, rho13, rho23, sigma1, sigma2, sigma3)
            )

pdf("~/Dropbox/MS/LES_MS/fig/GL1_para.PDF", width = 8.6, height = 5.2)
p <- ggplot(GL, aes(x = val, fill = model, colour = model )) + facet_wrap( ~ est2, scales = "free")

p <- p + geom_density(aes(y=0.09*..density..), alpha = 0.3) + theme_bw()

p <- p + ylab("Density") + xlab("")

p <- p + geom_vline(data = vline_dat, aes(xintercept = true_val),
  color = "red", lty =1, alpha = 0.8)

p　+ theme_bw(base_size = 11)  + theme(legend.position=c(0.7,0.2))
dev.off()



#PA like------------
load("~/Dropbox/LES/PA_sim.RData")
library(loo)
#unif pot, opt, site
waic(extract_log_lik(res1,"log_lik"))$waic
waic(extract_log_lik(res2,"log_lik"))$waic
waic(extract_log_lik(res3,"log_lik"))$waic
#com
waic(extract_log_lik(res4,"log_lik"))$waic
waic(extract_log_lik(res5,"log_lik"))$waic
waic(extract_log_lik(res6,"log_lik"))$waic
#dif
waic(extract_log_lik(res7,"log_lik"))$waic
waic(extract_log_lik(res8,"log_lik"))$waic
waic(extract_log_lik(res9,"log_lik"))$waic

# library(shinystan)
# moge <- launch_shinystan(res3)

# sigma1 <- 0.2
# sigma2 <- 0.5
# sigma3 <- 0.7
L_sigma <- NULL
"L_sigma[1]" <- sigma1
"L_sigma[2]" <- sigma2
"L_sigma[3]" <- sigma3

# rho12 <- 0.8
# rho13 <- 0.03
# rho23 <- 0.1
# alpha <- 0.28
# # log_alpha <- log(alpha)
# beta <- 0.6
# log_beta <- log(beta)
rp <- 0.02
rs <- 0.001
q <- 0.5
log_site <- log(0.7)


PA_NH <- data_frame(alpha = extract(res3, pars = "log_alpha")[[1]] %>% exp,
      beta = extract(res3, pars = "log_beta")[[1]] %>% exp,
      rp = extract(res3, pars = "rp")[[1]],
      rs = extract(res3, pars = "rs")[[1]],
      rho12 = extract(res3, pars = "rho12")[[1]],
      rho13 = extract(res3, pars = "rho13")[[1]],
      rho23 = extract(res3, pars = "rho23")[[1]],
      sigma1 = extract(res3, pars = "L_sigma[1]")[[1]],
      sigma2 = extract(res3, pars = "L_sigma[2]")[[1]],
      sigma3 = extract(res3, pars = "L_sigma[3]")[[1]]) %>%
      mutate(model = "Non-hierarchical")

PA_H <- data_frame(alpha = extract(res9, pars = "log_alpha")[[1]] %>% exp,
      beta = extract(res9, pars = "log_beta")[[1]] %>% exp,
      rp = extract(res9, pars = "rp")[[1]],
      rs = extract(res9, pars = "rs")[[1]],
      rho12 = extract(res9, pars = "rho12")[[1]],
      rho13 = extract(res9, pars = "rho13")[[1]],
      rho23 = extract(res9, pars = "rho23")[[1]],
      sigma1 = extract(res9, pars = "L_sigma[1]")[[1]],
      sigma2 = extract(res9, pars = "L_sigma[2]")[[1]],
      sigma3 = extract(res9, pars = "L_sigma[3]")[[1]]) %>%
      mutate(model = "Hierarchical")

PA <- bind_rows(PA_NH, PA_H) %>%
  tidyr::gather("est", "val", 1:10) %>%
  mutate(est2 = factor(est,
    levels = c("alpha", "beta", "rp", "rs", "rho12", "rho13", "rho23",
          "sigma1", "sigma2", "sigma3"))) %>%
  mutate(model = factor(model, levels = c("Non-hierarchical", "Hierarchical")))


vline_dat <- data_frame(est2 = c("alpha", "beta", "rp", "rs",
            "rho12", "rho13", "rho23",
            "sigma1", "sigma2", "sigma3"),
            true_val = c(alpha, beta, rp, rs,
            rho12, rho13, rho23, sigma1, sigma2, sigma3)
            )

pdf("~/Dropbox/MS/LES_MS/fig/PA_para.PDF", width = 8.6, height = 5.2)
p <- ggplot(PA, aes(x = val, fill = model, colour = model )) + facet_wrap( ~ est2, scales = "free")

p <- p + geom_density(aes(y=0.09*..density..), alpha = 0.3) + theme_bw()

p <- p + ylab("Density") + xlab("")

p <- p + geom_vline(data = vline_dat, aes(xintercept = true_val),
  color = "red", lty =1, alpha = 0.8)

p　+ theme_bw(base_size = 12)  + theme(legend.position=c(0.7,0.2))
dev.off()



#null ----------------------------------------------------------------------
load("/Users/mattocci/Dropbox/LES/null_sim.RData")

alpha <- 1
beta <- 1

WC_NH <- data_frame(alpha = extract(res1, pars = "log_alpha")[[1]] %>% exp,
      beta = extract(res1, pars = "log_beta")[[1]] %>% exp,
      rp = extract(res1, pars = "rp")[[1]],
      rs = extract(res1, pars = "rs")[[1]],
      rho12 = extract(res1, pars = "rho12")[[1]],
      rho13 = extract(res1, pars = "rho13")[[1]],
      rho23 = extract(res1, pars = "rho23")[[1]],
      sigma1 = extract(res1, pars = "L_sigma[1]")[[1]],
      sigma2 = extract(res1, pars = "L_sigma[2]")[[1]],
      sigma3 = extract(res1, pars = "L_sigma[3]")[[1]]) %>%
      mutate(model = "Non-hierarchical")

WC_H <- data_frame(alpha = extract(res2, pars = "log_alpha")[[1]] %>% exp,
      beta = extract(res2, pars = "log_beta")[[1]] %>% exp,
      rp = extract(res2, pars = "rp")[[1]],
      rs = extract(res2, pars = "rs")[[1]],
      rho12 = extract(res2, pars = "rho12")[[1]],
      rho13 = extract(res2, pars = "rho13")[[1]],
      rho23 = extract(res2, pars = "rho23")[[1]],
      sigma1 = extract(res2, pars = "L_sigma[1]")[[1]],
      sigma2 = extract(res2, pars = "L_sigma[2]")[[1]],
      sigma3 = extract(res2, pars = "L_sigma[3]")[[1]]) %>%
      mutate(model = "Hierarchical")

WC <- bind_rows(WC_NH, WC_H) %>%
  tidyr::gather("est", "val", 1:10) %>%
  mutate(est2 = factor(est,
    levels = c("alpha", "beta", "rp", "rs", "rho12", "rho13", "rho23",
          "sigma1", "sigma2", "sigma3")))

vline_dat <- data_frame(est2 = c("alpha", "beta", "rp", "rs",
            "rho12", "rho13", "rho23",
            "sigma1", "sigma2", "sigma3"),
            true_val = c(alpha, beta, rp, rs,
            rho12, rho13, rho23, sigma1, sigma2, sigma3)
            )

pdf("~/Dropbox/MS/LES_MS/fig/WC_para.PDF", width = 8.6, height = 5.2)
WC2 <- WC %>% filter(model == "Non-hierarchical")

# p <- ggplot(WC2, aes(x = val, fill = model, colour = model )) + facet_wrap( ~ est2, scales = "free")

p <- ggplot(WC2, aes(x = val, fill = model, colour = model )) + facet_wrap( ~ est2, scales = "free")

p <- p + geom_density(aes(y=0.09*..density..), alpha = 0.3) + theme_bw()

p <- p + ylab("Density") + xlab("")

p <- p + geom_vline(data = vline_dat, aes(xintercept = true_val),
  color = "red", lty =1, alpha = 0.8)

p　+ theme_bw(base_size = 12)  + theme(legend.position=c(0.7,0.2))
dev.off()
