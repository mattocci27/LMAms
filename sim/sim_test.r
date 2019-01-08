
log_alpha <- 1.6
alpha2 <- 0.23
sigma1 <- 0.36

log_alpha <- 1.43
alpha2 <- 0.28
sigma1 <- 0.29

log_mu <- log_alpha + alpha2 * log(GL$LMAp)  - 0.5 * sigma1^2

AR <- rlnorm(200, log_mu, sigma1)

fit <- lm(log(AR) ~ log(GL$LMAp))

summary(fit)

plot(AR ~ LMA, log = "xy")

GL <- read_csv("./data/GL20170911.csv")


fit <- lm(log(Aarea + Rarea) ~ log(LMAp4),
          data = GL)

GL$AR <- GL$Aarea + GL$Rarea
lm(log(AR) ~ log(LMAp), GL_sim_dat)
lm(log(LL) ~ log(LMAs), GL_sim_dat)
lm(log(Rarea) ~ log(LMAp) + log(LMAs), GL_sim_dat)

fit <- stan_glm(log(LL) ~ log(LMAs4),
               family = gaussian,
               prior = normal(0, 5),
               prior_intercept = normal(0,5),
               prior_aux = cauchy(0, 5),
               chains = 3,
               iter = 250,
               data = GL)

load("model_test.rda")
load("model_beta.rda")
load("model_h.rda")

load("model_single.rda")

GL <- dat
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) %>%
  mutate(LMAp =  GL_summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAs = LMA - LMAp)

plot(Aarea ~ LMAp, log = "xy", data = GL)
plot(Rarea ~ LMAp, log = "xy", data = GL)



para <- extract(res)

log_alpha <- para$log_alpha
alpha2 <- para$alpha2
sigma1 <- para$L_sigma[,1]

xx <- seq(5, 500, length = 200) %>% log

yy2 <- NULL
for (i in 1:1500) {
  yy <- log_alpha[i] + alpha2[i] * xx
  yy2 <- cbind(yy2, yy)
}

mid_yy <- apply(yy2, 1, median)
lo_yy <- apply(yy2, 1, function(x)quantile(x, 0.025))
up_yy <- apply(yy2, 1, function(x)quantile(x, 0.975))


obs_yy <- rnorm(200,
                 mid_yy - 0.5 * median(sigma1),
                 median(sigma1)
                 ) %>% exp

temp <- data_frame(xx = xx %>% exp,
                   mid_yy = mid_yy %>% exp, 
                   lo_yy = lo_yy %>% exp, 
                   up_yy = up_yy %>% exp,
                   obs_yy)

ggplot(temp, aes(x = xx, y = mid_yy)) +
  geom_point(aes(x = xx, y = obs_yy)) +
  geom_line() +
  geom_line(aes(y = lo_yy)) +
  geom_line(aes(y = up_yy)) +
  scale_x_log10() +
  scale_y_log10()

para$p[1,]

para$L_sigma[1,]

x <- para$p

p_list <- split(x, rep(1:ncol(x), each = nrow(x))) 

P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

LMAp <- NULL
for (i in 1:1500) {
 LMAp <- rbind(LMAp, GL$LMA * para$p[i,])
}

mid_LMAp <- apply(LMAp, 2, median)
lo_LMAp <- apply(LMAp, 2, function(x)quantile(x, 0.025))
up_LMAp <- apply(LMAp, 2, function(x)quantile(x, 0.975))
LMAp1 <- LMAp[122,]

temp2 <- data_frame(AR = GL$Aarea + GL$Rarea,
                   mid_LMAp = mid_LMAp, 
                   lo_LMAp = lo_LMAp, 
                   up_LMAp = up_LMAp, 
                   LMAp1) %>%
  mutate(mid_p = apply(para$p, 2, median)) %>%
  mutate(DE = GL$DE)

ggplot(temp2, aes(x = mid_LMAp, y = AR)) +
  geom_point(aes(x = LMAp1), col = "blue", alpha = 0.3) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

lm(log(AR) ~ log(mid_LMAp), temp2)
lm(log(AR) ~ log(LMAp1), temp2)

GL2 <- GL %>%
  mutate(LMAp =  para$p[4,] * LMA) %>%
  mutate(LMAs = LMA - LMAp) %>%
  mutate(AR = Aarea + Rarea) 


plot(Aarea ~ LMAp, log = "xy", GL2)
plot(Rarea ~ LMAp, log = "xy", GL2)
plot(LL ~ LMAs, log = "xy", GL2)

lm(log(AR) ~ log(LMAp), GL2) %>% summary




load("model_type2.rda")



GL_summary %>% 
  filter()


LMAp ~ normal(TrueLMAp, sigma)

LMAp = LMA * f

##
n <- 100

beta_est <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = c(alpha = alpha, beta = beta))
}


beta_est(0.6, 0.013)

truef <- rbeta(n, 10, 7)
trueLMA <- rlnorm(n, 4.5, 0.3)

trueLMAp <- trueLMA * truef
trueLMAs <- trueLMA * turef

LMAp <- rlnorm(n, log(trueLMAp), 0.5)
LMAs <- rlnorm(n, log(trueLMAs), 0.5)

plot(LMAp, trueLMAp, log = "xy")

LMAp_obs = LMA * f
LMAp_true ~ normal(0, 10)


LMAp_obs ~ normal(LMAp, sigma)


0.8 / 0.2

inv_logit <- function(x) 1/(1+exp(-x))


nohup Rscript ./model/model_type2.r > model_type2.log &


dnorm(0)

