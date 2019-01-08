library(tidyverse)
library(rstan)
library(smatr)
#use null result 1
null_n <- 3

load("./data/GL_potPL_obs.rda")
load("./data/GL_potPL2_obs.rda")
load("./data/GL_potPL_type2_obs.rda")

load("./data/GL1_potLL_sim.rda")

GL_summary <- data.frame(summary(res)$summary)
GL <- dat
GL <- GL_sim_dat %>%
  mutate(DE = gr)
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

trueLMA1 <- GL_summary %>%
  filter(str_detect(rownames(.), "trueLMA1")) %>%
  select(mean) %>%
  unlist %>%
  exp

trueLMA2 <- GL_summary %>%
  filter(str_detect(rownames(.), "trueLMA1")) %>%
  select(mean) %>%
  unlist %>%
  exp

plot(trueLMA1 ~ trueLMA2, log = "xy")


plot(trueLMA1 ~ GL$LMA, log = "xy")
abline(a=0,b=1)

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) %>%
  mutate(LMAp_pot =  GL_summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAp_hi =  GL_summary[P_vec, "X97.5."] * LMA) %>%
  mutate(LMAp_hi2 = LMAp_pot + GL_summary[P_vec, "sd"] * LMA) %>% 
  mutate(LMAp_lo =  GL_summary[P_vec, "X2.5."] * LMA) %>%
  mutate(LMAp_lo2 = LMAp_pot - GL_summary[P_vec, "sd"] * LMA) %>% 
  mutate(LMAs_pot = LMA - LMAp_pot) %>%
  mutate(LMAs_hi = LMA - LMAp_hi) %>%
  mutate(LMAs_lo = LMA - LMAp_lo) %>%
  mutate(mu1 = GL_summary[paste("mu[",1:nrow(GL),",1]",sep=""),"mean"]*2-2.5) %>%
  mutate(mu1 = GL_summary[paste("mu[",1:nrow(GL),",1]",sep=""),"mean"]) %>%
  mutate(mu2 = GL_summary[paste("mu[",1:nrow(GL),",2]",sep=""),"mean"]) %>%
  mutate(mu3 = GL_summary[paste("mu[",1:nrow(GL),",3]",sep=""),"mean"]) %>%
  mutate(res = Aarea + Rarea - exp(mu1)) %>%
  mutate(f = LMAp_pot / LMA)

write.csv(GL_summary, "moge.csv")

l5 <- lm(log(Aarea+Rarea) ~ log(LMAp_pot), GL)
s5 <- sma(log(Aarea+Rarea) ~ log(LMAp_pot), GL)

l6 <- lm(log(LL) ~ log(LMAs_pot), GL)
s6 <- sma(log(LL) ~ log(LMAs_pot), GL)

l7 <- lm(log(Rarea) ~ log(LMAp_pot) + log(LMAs_pot), GL)

moge <- data_frame(int = extract(res, "log_alpha") %>% unlist,
  slope = extract(res, "alpha2") %>% unlist)

ggplot(GL, 
       aes(x = log(LMAp_pot), y = log(Aarea + Rarea), col = DE)) +
  geom_errorbarh(aes(xmax = log(LMAp_hi), xmin = log(LMAp_lo)), 
                 alpha = 0.3) +
  geom_point() +
  geom_abline(slope = moge$slope, intercept = moge$int, alpha = 0.05, col = "grey") +
  geom_point(aes(y = mu1), col = "black") +
  geom_abline(slope=GL_summary["alpha2", "mean"],
              intercept= GL_summary["log_alpha", "mean"]) +
  geom_abline(slope=l5[[1]][2], intercept=l5[[1]][1], lty = 2) +
  geom_abline(slope=s5[[1]][[1]][2,1], 
              intercept=s5[[1]][[1]][1,1], lty = 2, col = "red") +
  theme_light()

moge2 <- data_frame(int = extract(res, "log_beta") %>% unlist,
  slope = extract(res, "beta2") %>% unlist)

ggplot(GL, 
       aes(x = log(LMAs_pot), y = log(LL), col = DE)) +
  geom_errorbarh(aes(xmax = log(LMAs_hi), xmin = log(LMAs_lo)), 
                 alpha = 0.3) +
  geom_point() +
  geom_point(aes(y = mu2), col = "black") +
  #geom_abline(slope=0.23, intercept= 1.57) +
  geom_abline(slope=GL_summary["beta2", "mean"],
              intercept= GL_summary["log_beta", "mean"]) +
  geom_abline(slope = moge2$slope, intercept = moge2$int, alpha = 0.1) +
  geom_abline(slope=l6[[1]][2], intercept=l6[[1]][1], lty = 2) +
  geom_abline(slope=s6[[1]][[1]][2,1], 
              intercept=s6[[1]][[1]][1,1], lty = 2, col = "red") +
  theme_light()



ggplot(GL, 
       aes(x = log(LMAp_pot), y = log(Aarea), col = DE)) +
  geom_point() +
  theme_light()

l6 <- lm(log(Aarea+Rarea) ~ log(LMAp), GL_sim_dat)
s6 <- sma(log(Aarea+Rarea) ~ log(LMAp), GL_sim_dat)

l7 <- lm(log(Rarea) ~ log(LMAp) + log(LMAs), GL_sim_dat)

ggplot(GL, 
       aes(x = log(LMAp_pot), y = log(Rarea), col = DE)) +
  geom_errorbarh(aes(xmax = log(LMAp_hi), xmin = log(LMAp_lo)), 
                 alpha = 0.3) +
  geom_point() +
  geom_point(aes(y = mu3), col = "black") 

ggplot(GL, 
       aes(x = log(LMAp_pot), y = log(Aarea), col = DE)) +
  geom_errorbarh(aes(xmax = log(LMAp_hi), xmin = log(LMAp_lo)), 
                 alpha = 0.3) +
  geom_point()

ggplot(GL, 
       aes(x = log(LMAs_pot), y = log(LL), col = DE)) +
  geom_errorbarh(aes(xmax = log(LMAs_hi), xmin = log(LMAs_lo)), 
                 alpha = 0.3) +
  geom_point() +
  geom_point(aes(y = mu2), col = "black") 

ggplot(GL, 
       aes(x = mu3, y = log(Rarea), col = DE)) +
  geom_point() +
  geom_abline(slope=1, intercept= 0) 

ggplot(GL, 
       aes(x = mu1, y = log(Aarea + Rarea), col = DE)) +
  geom_point() +
  geom_abline(slope=1, intercept= 0) +
  coord_fixed()

moge <- data_frame(alpha2 = extract(res, "alpha2") %>% unlist,
  log_alpha = extract(res, "log_alpha") %>% unlist)

moge %>% sample_n(1000)

extract(res, "p[1]") %>% unlist %>% hist

GL2 <- GL %>%
  gather(LMA, Val, c(LMA, LMAp_pot,LMAs_pot)) %>%
  gather(low, Val3, c(LMAp_lo, LMAs_lo)) %>%
  gather(high, Val4, c(LMAp_hi, LMAs_hi)) %>%
  gather(Trait, Val2, c(Aarea, Rarea, LL)) %>%
  filter(!(LMA == "LMAs_pot" & str_detect(high, "LMAp"))) %>%
  filter(!(LMA == "LMAs_pot" & str_detect(low, "LMAp"))) %>%
  filter(!(LMA == "LMAp_pot" & str_detect(high, "LMAs"))) %>%
  filter(!(LMA == "LMAp_pot" & str_detect(low, "LMAs"))) %>%
  as_data_frame


ggplot(GL2, aes(x = Val, y = Val2, col = DE)) +
  geom_errorbarh(data = GL2 %>% filter(LMA !="LMA"), 
                 aes(xmax = Val4, xmin = Val3), alpha = 0.3) +
  geom_point() +
  facet_grid(Trait ~ LMA, scale = "free") +
  scale_x_log10() +
  scale_y_log10()

load("./data/GL_pot_com_obs.rda")
load("./data/GL_pot_diff_obs.rda")
load("./data/GL_optGL_obs.rda")

GL_summary <- data.frame(summary(res)$summary)
GL <- dat
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) %>%
  mutate(LMAp_pot =  GL_summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAs_pot = LMA - LMAp_pot) %>%
  mutate(f = LMAp_pot / LMA)

plot(f ~ LMA, GL, log = "x")
plot(f_val ~ LMA, GL_sim_dat, log = "x")

cor.test(log(GL$f), log(GL$LMA))
cor.test(log(GL_sim_dat$LMA), log(GL_sim_dat$f_val))

plot(log(Aarea+Rarea) ~ log(LMAp_pot), GL)
points(mu1 ~ log(LMAp_pot), GL,col ="orange")
points(mu1 ~ log(LMAp_hi), GL,col ="red")
points(mu1 ~ log(LMAp_lo), GL,col ="blue")
abline(a=0.61, b=0.44)
abline(a=1.6, b=0.23, lty = 2)

plot(log(Rarea) ~ log(LMAp_pot), GL)
points(mu3 ~ log(LMAp_pot), GL,col ="orange")

plot(res ~ LMAp_pot, log = "x", GL)

plot(Rarea ~ LMAp_pot, log = "xy", GL)
plot(LL ~ LMAs_pot, log = "xy", GL)

plot(log(LL) ~ log(LMAs_pot), GL)
points(mu2 ~ log(LMAs_pot), GL, col = "orange")
points(mu2 ~ log(LMAs_lo), GL, col = "blue")
points(mu2 ~ log(LMAs_hi), GL, col = "red")
abline(a=-1.1, b=0.88)
abline(a=-.64, b=0.8, lty = 2)

l1 <- lm(log(LL) ~ log(LMAs_pot), GL)
l2 <- lm(log(LL) ~ log(LMAs), GL_sim_dat)

l3 <- lm(log(Aarea) ~ log(LMAp_pot), GL)
l5 <- lm(log(Aarea+Rarea) ~ log(LMAp_pot), GL)
l4 <- lm(log(Aarea) ~ log(LMAp), GL_sim_dat)

l6 <- lm(log(Rarea) ~ log(LMAp_pot) + log(LMAs_pot), GL)
p

ggplot(GL, aes(x = LMA, y = f, col = DE)) +
  geom_point() +
  scale_x_log10()

GL %>% 
#  filter(DE == "E") %>%
  ggplot(., aes(x = f)) +
  geom_histogram()

ggplot(GL_sim_dat, aes(x = LMA, y = f_val, col = gr)) +
  geom_point() +
  scale_x_log10()

cor.test(log(GL$LMA), log(GL$LL))
cor.test(log(GL_sim_dat$LMA), log(GL_sim_dat$LL))

cor.test(log(GL$LMAs), log(GL$LL))
cor.test(log(GL_sim_dat$LMAs), log(GL_sim_dat$LL))

cor.test(log(GL$LMAp), log(GL$Aarea))
cor.test(log(GL_sim_dat$LMAp), log(GL_sim_dat$Aarea))

GL_sim_dat %>%
  group_by(gr) %>%
  summarize(mean = mean(LMA),
            mid = median(LMA),
            sd = sd(LMA),
            lower = quantile(LMA, 0.1))
GL %>%
  group_by(DE) %>%
  summarize(mean = mean(LMA),
            mid = median(LMA),
            sd = sd(LMA),
            lower = quantile(LMA, 0.1))

mod_fun <- function(df) {
  lm(f ~ log(LMA), data = df)
}

moge <- GL %>%
  group_by(DE) %>%
  nest %>%
  mutate(model = map(data, mod_fun)) %>%
  mutate(sum = map(model, summary))

xx <- rlnorm(200, log(80))
yy <- rlnorm(200, 1.6 + 0.23 * log(xx), 0.33)

plot(yy ~ xx,log = "xy")
cor.test(log(xx), log(yy))

par(mfrow=c(5,4))
for (i in 1:20) extract(res, paste("p[",i,"]",sep="")) %>% unlist %>% hist
par(mfrow=c(1,1))



ggplot(GL, 
       aes(x = LMAp_pot/LMA, y = 1:nrow(GL), col = DE)) +
      # aes(x = log(LMAp_pot), y = log(Aarea), col = DE)) +
  geom_errorbarh(aes(xmax = LMAp_hi/LMA, xmin = LMAp_lo/LMA), 
                 alpha = 0.3) +
  geom_point() +
  theme_bw()



p1 <- extract(res, "p[1]") %>% unlist  %>% .[1]

-0.16 + 0.64 * log(GL$LMA[1] * p1) 

-0.16 + 0.64 * (log(GL$LMA[1]) + log(p1))

extract(res, "mu[2,1]") %>% unlist %>% .[1]

load("./data/GL_potPL_vec_obs.rda")

load("./data/GL_pot_obs.rda")

GL_summary <- data.frame(summary(res)$summary)
GL <- dat
#GL <- GL_sim_dat %>%
#  mutate(DE = gr)
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) %>%
  mutate(LMAp_pot =  GL_summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAp_hi =  GL_summary[P_vec, "X97.5."] * LMA) %>%
  mutate(LMAp_hi2 = LMAp_pot + GL_summary[P_vec, "sd"] * LMA) %>% 
  mutate(LMAp_lo =  GL_summary[P_vec, "X2.5."] * LMA) %>%
  mutate(LMAp_lo2 = LMAp_pot - GL_summary[P_vec, "sd"] * LMA) %>% 
  mutate(LMAs_pot = LMA - LMAp_pot) %>%
  mutate(LMAs_hi = LMA - LMAp_hi) %>%
  mutate(LMAs_lo = LMA - LMAp_lo) %>%
  mutate(mu1 = GL_summary[paste("mu[",1:nrow(GL),",1]",sep=""),"mean"]) %>%
  mutate(mu12 = -0.16 + 0.64 * log(LMAp_pot)) %>%
  mutate(mu2 = GL_summary[paste("mu[",1:nrow(GL),",2]",sep=""),"mean"]) %>%
  mutate(mu3 = GL_summary[paste("mu[",1:nrow(GL),",3]",sep=""),"mean"]) %>%
  mutate(res = Aarea + Rarea - exp(mu1)) %>%
  mutate(f = LMAp_pot / LMA)

l5 <- lm(log(Aarea + Rarea) ~ log(LMAp_pot), GL)
l5 <- lm(log(Aarea) ~ log(LMAp_pot), GL)
s5 <- sma(log(Aarea) ~ log(LMAp_pot), GL)

l6 <- lm(log(LL) ~ log(LMAs_pot), GL)
s6 <- sma(log(LL) ~ log(LMAs_pot), GL)

l7 <- lm(log(Rarea) ~ log(LMAp_pot) + log(LMAs_pot), GL)

moge <- data_frame(int = extract(res, "log_alpha") %>% unlist,
  slope = extract(res, "alpha2") %>% unlist)


ggplot(GL, 
       aes(x = log(LMAp_pot), y = log(Aarea), col = DE)) +
  geom_errorbarh(aes(xmax = log(LMAp_hi), xmin = log(LMAp_lo)), 
                 alpha = 0.3) +
  geom_point() +
  geom_abline(slope = moge$slope, intercept = moge$int, alpha = 0.02, col = "grey") +
  geom_point(aes(y =  mu1), col = "black") +
  geom_abline(slope=GL_summary["alpha2", "mean"],
  #geom_abline(slope= 0.6,
  #            intercept = -.16) +
              intercept= GL_summary["log_alpha", "mean"]) +
  geom_abline(slope=l5[[1]][2], intercept=l5[[1]][1], lty = 2) +
  geom_abline(slope=s5[[1]][[1]][2,1], 
              intercept=s5[[1]][[1]][1,1], lty = 2, col = "red") +
  theme_light()


l5 <- lm(log(Rarea) ~ log(LMAp_pot), GL)
s5 <- sma(log(Rarea) ~ log(LMAp_pot), GL)
ggplot(GL, 
       aes(x = log(LMAp_pot), y = log(Rarea), col = DE)) +
  geom_errorbarh(aes(xmax = log(LMAp_hi), xmin = log(LMAp_lo)), 
                 alpha = 0.3) +
  geom_point() +
  geom_abline(slope = moge$slope, intercept = moge$int, alpha = 0.02, col = "grey") +
  geom_point(aes(y =  mu3), col = "black") +
  geom_abline(slope=GL_summary["alpha2", "mean"],
  #geom_abline(slope= 0.6,
  #            intercept = -.16) +
              intercept= GL_summary["log_alpha", "mean"]) +
  geom_abline(slope=l5[[1]][2], intercept=l5[[1]][1], lty = 2) +
  geom_abline(slope=s5[[1]][[1]][2,1], 
              intercept=s5[[1]][[1]][1,1], lty = 2, col = "red") +
  theme_light()

l6 <- lm(log(LL) ~ log(LMAs_pot), GL)
s6 <- sma(log(LL) ~ log(LMAs_pot), GL)

ggplot(GL, 
       aes(x = log(LMAs_pot), y = log(LL), col = DE)) +
  geom_errorbarh(aes(xmax = log(LMAs_hi), xmin = log(LMAs_lo)), 
                 alpha = 0.3) +
  geom_point() +
  geom_point(aes(y = mu2), col = "black") +
  #geom_abline(slope=0.23, intercept= 1.57) +
  geom_abline(slope=GL_summary["beta2", "mean"],
              intercept= GL_summary["log_beta", "mean"]) +
  geom_abline(slope = moge2$slope, intercept = moge2$int, alpha = 0.02, col = "grey") +
  geom_abline(slope=l6[[1]][2], intercept=l6[[1]][1], lty = 2) +
  geom_abline(slope=s6[[1]][[1]][2,1], 
              intercept=s6[[1]][[1]][1,1], lty = 2, col = "red") +
  theme_light()

plot(Aarea ~ LMAp_pot, GL, log = "xy")

GL %>%
  mutate(Amass = Aarea * LMA) %>%
  mutate(Rmass = Rarea * LMA) %>%
  plot(Amass ~ Rmass,data= ., log = "xy")


lm(log(LL) ~ log(LMA) + log(Aarea) + log(Rarea), GL) %>% summary



GL$Aarea - GL$Rarea

  ggplot(GL, 
       aes(x = mu1, y = log(Aarea) + log(Rarea), col = DE)) +
    geom_point() +
    theme_light() 

  ggplot(GL, 
       aes(x = mu2, y = log(LL), col = DE)) +
    geom_point() +
    theme_light() 

  ggplot(GL, 
       aes(x = mu3, y = log(Rarea), col = DE)) +
    geom_point() +
    theme_light() 




GL <- read_csv("./data/GL20170911.csv") 

GL_para <- GL %>%
  mutate(ratio = LMAp4/LMA) %>%
  summarize(mu_LMA = mean(log(LMA)),
            mid_LMA = median(log(LMA)),
            sigma_LMA = sd(log(LMA)),
            mu_f = mean(ratio),
            var_f = var(ratio))

  ungroup %>%
 # mutate(mu = mu_LMA - 0.5 * sigma_LMA^2) %>%
 # mutate(mu2 = mu_LMA2 - 0.5 * sigma_LMA^2) %>%
  mutate(n = as.integer(n / sum(n) * n_sample)) 

lm(log(Aarea + Rarea) ~log(LMAp), GL_sim_dat) %>% summary
lm(log(Aarea) ~log(LMAp), GL_sim_dat) %>% summary
lm(log(LL) ~log(LMAs), GL_sim_dat) %>% summary
lm(log(Rarea) ~log(LMAp) + log(LMAs), GL_sim_dat) %>% summary

lm(log(Aarea + Rarea) ~log(LMAp_pot), GL) %>% summary
lm(log(Aarea) ~log(LMAp_pot), GL) %>% summary
lm(log(Rarea) ~log(LMAp_pot) + log(LMAs_pot), GL) %>% summary
lm(log(LL) ~log(LMAs_pot), GL) %>% summary

aa<-cov(GL_sim_dat$LMAp, GL_sim_dat$LMA)
bb<-cov(GL_sim_dat$LMAs, GL_sim_dat$LMA)
var(GL_sim_dat$LMA)




lm(log(Aarea) ~log(Rarea), GL) %>% summary
lm(log(Aarea) ~log(LL), GL) %>% summary
lm(log(LL) ~log(Rarea), GL) %>% summary
set.seed(666)
Omega <- rbind(
  c(1, 0.3, 0.2),
  c(0.3, 1, 0.1),
  c(0.2, 0.1, 1)
)

sigma <- c(1, 2, 3)
Sigma <- diag(sigma) %*% Omega %*% diag(sigma)
N <- 100
y <- mvtnorm::rmvnorm(N, c(0,0,0), Sigma)



GL2 <- GL %>%
  mutate(LMAp2 = Aarea^(1/0.64) /exp(-0.16) + rlnorm(198,2)) %>%
  mutate(LMAs2 = LMA - LMAp2)

plot(Aarea ~ LMAp2, GL2, log = "xy")

par(mfrow=c(1,2))
plot(LL ~ LMAs2, GL2, log = "xy")
plot(LL ~ LMA, GL2, log = "xy")
par(mfrow=c(1,1))

cor.test(log(GL2$LMAs2), log(GL$LL))
cor.test(log(GL2$LMA), log(GL$LL))
