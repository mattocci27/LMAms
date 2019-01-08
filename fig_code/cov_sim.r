# y: mass vs area (b: in PNAS)
# x: var(LMAp) / var(LMAs)
#
# var(LMA) = var(LMAs) + var(LMAp) + 2Cov()
# LMA: approx. fixed:
#
# Vp, Vs: low, med, high
# Cov: negative, zero, positive
#
# basically 6 patterns + 3 (Vps, cov<0 , cov =0, cov >0)
#
# Vtotal = Vp_hi + Vs_med , cov = 0...
library(MASS)
library(tidyverse)
library(cowplot)
library(latex2exp)
load("./data/GL_sim_var.rda")
source("fig_theme.r")
#raw
my_breaks <- function(...){
  c(0.001, 0.01, 0.1, 1, 10, 100, 500)
}

fmt_dcimals <- function(x) format(x, nsmall = 1, scientific = FALSE)

# covs or covp < 0 is weird
plot_fn <- function(data){
  data <- data %>% filter(Covs > 0 & Covp > 0)
  p1 <- ggplot(data, aes(x = ratio, y = b, col = alpha)) +
    geom_point(alpha = 1, size = 0.5) +
    #geom_smooth(col = "blue", lty = 2) +
    scale_colour_gradient2(low = "blue", mid = "gray", high = "red",
                           midpoint = 0.5,
                           guide = FALSE) +
    scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
       labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    xlab("Var(LMAs)/Var(LMAp)") +
    ylab("Mass-dependence")# +
   # coord_cartesian(ylim=c(-0.2, 0.8))

  p2 <- ggplot(data, aes(x = ratio, y = ratio2, col = alpha)) +
    geom_point(size = 0.5) +
    scale_colour_gradient2(low = "blue", mid = "gray", high = "red",
                           midpoint = 0.5,
                           #name = "Correlation \ncoeffcient") +
                           name = "Scaling \nparameter") +
    scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
       labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    ylim(0, 100) +
    xlab("Var(LMAs)/Var(LMAp)") +
    ylab("% of LMA variation due to LMAs") +
    theme(legend.justification=c(1,0), legend.position=c(0.95, 0)) 

  plot_grid(p1, p2, labels = c("(a)", "(b)"), align = c("hv"))
}

GL_sim2 <- GL_sim2 %>%
  arrange(desc(alpha))

GL_sim3 <- GL_sim2 %>%
#   filter(sigma1 > 0.1 & sigma2 > 0.1)
  filter(ratio < 100 & ratio > 0.01) %>%
  filter(b < 1 & b > 0) %>%
  filter(rho == 0)
  #mutate(b = ifelse(b < 0, 0, b))

sim_var_plt <- plot_fn(GL_sim3)
my_ggsave("./figs/sim_var_plt.png", sim_var_plt,
          width = 22,
          height = 11)

sim_var_plt2 <- plot_fn(GL_sim2)
my_ggsave("./figs/sim_var_plt2.png", sim_var_plt2,
          width = 22,
          height = 11)

#temp <- GL_sim3 %>%
#  filter(ratio > 0.5) 
#
#  filter(b > 0.75)
#
#log(ratio) %>%
#  filter(b > 0.8) 
#
#
#GL_sim3 %>%
#  filter(ratio > 0.5 & ratio < 2) %>%
#  filter(b > 0.9 %
#  filter(rho > 0)
#
#a <- rlnorm(100, log(1000), 1)
#b <- rlnorm(100, log(10), 1)
#
#var(a %>% log)
#var(b %>% log)
#
#sun <- PA_trim %>%
#  filter(strata == "CAN")
#shade <- PA_trim %>%
#  filter(strata == "UNDER")
#
#
#var(shade$LMAp)
#var(shade$LMAs)
#
#var(sun$LMAp)
#var(sun$LMAs)
#
#mu1 <- log(90)
#mu2 <- log(70)
#sigma3 <- 0.28
#sigma <- c(LMAs = 1.5, LMAp = 1)
#mu <- c(LAMs = mu1 - 0.5 * sigma[1]^2, LMAp = mu2 - 0.5 * sigma[2]^2)
#Sigma <- matrix(c(sigma[1]^2, sigma[1]*sigma[2]*rho,
#                  sigma[1]*sigma[2]*rho,sigma[2]^2), 2, 2)
#
#LMA_dat <- mvrnorm(n = 200, mu, Sigma) %>% exp
#
## LMA_dat <- rlnorm.rplus(n = 100, mu, Sigma)
#LMAs <- LMA_dat[,1]
#LMAp <- LMA_dat[,2]
#LMAs <- LMAp
## Aarea <- 0.23 * LMAp
#
#mu_temp <- exp(1.44) * LMAp^0.28 - exp(-0.96) * LMAp^0.58
#
#log_Aarea <- rnorm(200, log(mu_temp) - 0.5 * sigma3^2,
#                   sigma3)
#
## log_Aarea <- rnorm(100, 0.83 + 0.37 * log(LMAp), sigma3)
#Aarea <- exp(log_Aarea)
#LMA <- LMAp + LMAs
#LMA <- LMAp + LMAs
#Amass <- Aarea / LMA
#
#v_vec <- c(Vtotal = var(LMA) , Vs = var(LMAs) , Vp = var(LMAp) ,
#           Cov = cov(LMAp, LMAs), Covs = cov(LMA, LMAs),
#           Covp = cov(LMA, LMAp))
#
## print(sd(log(LMAs)))
## Osnas et al.
#res <- lm(log(Aarea) ~ log(LMA))
#
#res %>% summary
#
#var(LMAs)
#var(LMAp)
#
#plot(Aarea ~ LMAp, log = "xy")
#
#
