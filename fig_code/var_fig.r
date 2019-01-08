library(tidyverse)
library(rstan)
library(ggrepel)

settings <- yaml::yaml.load_file("settings.yml")
source("fig_theme.r")
var_val <- read_csv("./data/var_val.csv")
#PA <- read_csv("./data/PA_m1q_more_N.csv")
#GL <- read_csv("./data/GL_m0_N.csv")
#PA <- read_csv("./data/PAparaN.csv")
#GL <- read_csv("./data/GLparaN.csv")
PA <- read_csv("./data/PApara.csv")
GL <- read_csv("./data/GLpara.csv")

PA2 <- PA %>%
  mutate(dataset = "PA") 

GL2 <- GL %>%
  mutate(dataset = "GL")

slope_dat <- bind_rows(PA2, GL2) %>%
  filter(para == "beta[2]") %>%
  mutate(mid = round(`X50.`, 3)) %>%
  mutate(low = round(`X2.5.`, 3)) %>%
  mutate(up = round(`X97.5.`, 3)) %>%
  dplyr::select(mid, low, up, dataset)

#load("./data/GL_m0_N_obs.rda")
load("./data/GL_m0_obs.rda")
GLres <- res

var_fun  <- function(LMAp, LMAs) {
  #LMAp <- rstan::extract(res, "log_LMAp")[[1]] %>% exp
  #LMAs <- rstan::extract(res, "log_LMAs")[[1]] %>% exp
  var_LMAp <- apply(LMAp, 1, var)
  var_LMAs <- apply(LMAs, 1, var)
  res2 <- var_LMAp / var_LMAs

  var_low <- quantile(res2, 0.025)
  var_mid <- quantile(res2, 0.5)
  var_up <- quantile(res2, 0.975)

  data_frame(var_low, var_mid, var_up)
}

GL_LMAp <- rstan::extract(GLres, "log_LMAp")[[1]] %>% exp
GL_LMAs <- rstan::extract(GLres, "log_LMAs")[[1]] %>% exp
GLv <- var_fun(GL_LMAp, GL_LMAs)

#load("./data/PA_m1q_more_N_obs.rda")
load("./data/PA_m1q_more_obs.rda")
PAres <- res
PA_LMAp <- rstan::extract(PAres, "log_LMAp")[[1]] %>% exp
PA_LMAs <- rstan::extract(PAres, "log_LMAs")[[1]] %>% exp

PA3 <- read_csv("./data/PA_m1q_more_N.csv")

PA_LMAp_sun <- PA_LMAp[, PA3$strata=="CAN"]
PA_LMAs_sun <- PA_LMAs[, PA3$strata=="CAN"]
PA_LMAp_shade <- PA_LMAp[, PA3$strata=="UNDER"]
PA_LMAs_shade <- PA_LMAs[, PA3$strata=="UNDER"]

PAv1 <- var_fun(PA_LMAp_sun, PA_LMAs_sun)
PAv2 <- var_fun(PA_LMAp_shade, PA_LMAs_shade)

var_dat <-bind_rows(PAv1, PAv2, GLv) %>%
  mutate(gr = c("PA Sun","PA Shade", "GL")) %>%
  mutate(dataset = c("PA", "PA", "GL"))

var_dat2 <- full_join(slope_dat, var_dat, by = "dataset") %>%
  mutate(b = 0) %>%
  mutate(Aarea_scale = 0)

# functions =======================================
log_var <- function(mu, sigma) {
  (exp(sigma^2) - 1) * exp(2 * mu + sigma^2)
}

#log_var(2, 1)

log_var_inv <- function(v, sigma) {
  tmp <- 0.5 * (log(v) - sigma^2 - log(exp(sigma^2) - 1))
  exp(tmp)
}

log_mu_inv <- function(v, mu) {
  esig2 <- 0.5 * (1 + sqrt(1 + 4*exp(log(v) - 2 *mu)))
  log(esig2) %>% sqrt
}


sigma1 <- seq(0.3, 1, by = 0.05)
mu1 <- c(40)
const <- 10^seq(-1,1, by = 0.05) 
#p_scale <- seq(0.1, 1, length = 40)
p_scale <- seq(0.1, 1, by = 0.02) #60
mu2 <- c(40)

para <- expand.grid(sigma1, mu1, p_scale, const, mu2) %>%
  as_data_frame
colnames(para) <- c("sigma1", 
                    "mu1", 
                    "Aarea_scale",
                    "const",
                    "mu2")

para2 <- para %>%
  mutate(LMAp_var = log_var(log(mu1), sigma1)) %>%
  mutate(sigma2 = log_mu_inv(LMAp_var * 1/const, log(mu2))) %>%
  mutate(LMAs_var = log_var(log(mu2), sigma2)) %>%
  mutate(var_ratio = LMAp_var / (LMAs_var + LMAp_var)) %>%
  mutate(var_ratio2 = (LMAp_var/LMAs_var)) 


dat_fun <- function(n){
  LMAp <- rlnorm(100, log(para2$mu1[n]), para2$sigma1[n])
  LMAs <- rlnorm(100, log(para2$mu2[n]), para2$sigma2[n])
  log_mu <- 1.5 + para$Aarea_scale[n] * log(LMAp)
  Aarea <- rlnorm(100, log_mu, 0.5)
  dat <- data_frame(LMAp, LMAs, Aarea) %>%
    mutate(LMA = LMAp + LMAs) %>%
    filter(is.finite(Aarea))
  dat
}

cor_fun <- function(data) {
  cor(log(data$Aarea), log(data$LMA))
}

b_fun <- function(data) {
  res <- lm(log(Aarea) ~ log(LMA), data = data)
  res[[1]][2]
}

before <- proc.time()
para3 <- para2 %>%
  mutate(res = map(1:nrow(.), dat_fun)) %>%
  as_data_frame
after <- proc.time()
after - before

para4 <- para3 %>%
  #mutate(r = map_dbl(res, cor_fun)) %>%
  mutate(b = map_dbl(res,b_fun)) %>%
  #mutate(var_LMAm = map_dbl(res, var_fun)) %>%
  dplyr::select(-res) 


simvar <- ggplot(para4 %>% filter(b < 1.5 & b > 0),
#simvar <- ggplot(para4,
                 aes(x = const, y = Aarea_scale, fill = b)) +
  geom_raster() +
  scale_fill_gradient2(midpoint = 0.5,
                       name = "Mass \nproportionality") +
  theme_LES() +
  theme(panel.background = element_rect(fill = "#073642"),
        legend.title = element_text(colour = "black"),
        legend.position = "right",
        axis.title.y = element_text(margin = margin(t = 0,
                                                    b = 0,
                                                    l = 0,
                                                    r = 5),
                                    angle = 90),
        axis.title.x = element_text(margin = margin(t = 5,
                                                    b = -5,
                                                    l = -5,
                                                    r = -5))
        ) +
  xlab("Var(LMAm) / Var(LMAs)") +
  ylab("Scaling slope") +
#  scale_x_log10(limits=c(0.1, 10)) 
  scale_x_log10()  +
  #ylim(0.05, 1) +
  geom_errorbar(data = var_dat2, aes(x = var_mid, 
                                     ymin = low,
                                     ymax = up),
                col = "grey40",
                width = 0.05) + 
  geom_errorbarh(data = var_dat2, aes(y = mid,
                                      x = var_mid,
                                     xmin = var_low,
                                     xmax = var_up),
                 height = 0.02,
                 col = "grey40") +
  geom_point(data = var_dat2, 
             aes(x = var_mid, y = mid),
             #fill = "#FFEB3B",
             fill = "#607D8B",
             shape = 21) +
  #scale_shape_manual(values = c(21, 22, 23)) +
  geom_text_repel(data = var_dat2,
            aes(y = mid,
                x = var_mid,
                label = gr)) 

simvar

my_ggsave("./figs/simvar.png",
          simvar,
          height = 6,
          width = 8)
