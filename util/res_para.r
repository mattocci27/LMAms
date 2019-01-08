library(rstan)
load("./data/GL_m0_obs.rda")

s_GL <- data.frame(summary(res)$summary) %>%
  mutate(para = rownames(.)) %>%
  mutate(sig = ifelse(`X2.5.` < 0 & `X97.5.` < 0 ,
         "sig",
         "nonsig")) %>%
  mutate(sig = ifelse(`X2.5.` > 0 & `X97.5.` > 0 ,
         "sig",
         sig))

write.csv(s_GL, "./data/GLpara.csv")

load("./data/GL_m0_N_obs.rda")

s_GL <- data.frame(summary(res)$summary) %>%
  mutate(para = rownames(.)) %>%
  mutate(sig = ifelse(`X2.5.` < 0 & `X97.5.` < 0 ,
         "sig",
         "nonsig")) %>%
  mutate(sig = ifelse(`X2.5.` > 0 & `X97.5.` > 0 ,
         "sig",
         sig))

write.csv(s_GL, "./data/GLparaN.csv")

load("./data/PA_m1q_more_obs.rda")
s_PA <- data.frame(summary(res)$summary) %>%
  mutate(para = rownames(.)) %>%
  mutate(sig = ifelse(`X2.5.` < 0 & `X97.5.` < 0 ,
         "sig",
         "nonsig")) %>%
  mutate(sig = ifelse(`X2.5.` > 0 & `X97.5.` > 0 ,
         "sig",
         sig))
write.csv(s_PA, "./data/PApara.csv")

load("./data/PA_m1q_more_N_obs.rda")
s_PA <- data.frame(summary(res)$summary) %>%
  mutate(para = rownames(.)) %>%
  mutate(sig = ifelse(`X2.5.` < 0 & `X97.5.` < 0 ,
         "sig",
         "nonsig")) %>%
  mutate(sig = ifelse(`X2.5.` > 0 & `X97.5.` > 0 ,
         "sig",
         sig))
write.csv(s_PA, "./data/PAparaN.csv")


load("./data/PA_m1q_more_NS_more_obs.rda")
s_PA <- data.frame(summary(res)$summary) %>%
  mutate(para = rownames(.)) %>%
  mutate(sig = ifelse(`X2.5.` < 0 & `X97.5.` < 0 ,
         "sig",
         "nonsig")) %>%
  mutate(sig = ifelse(`X2.5.` > 0 & `X97.5.` > 0 ,
         "sig",
         sig))
write.csv(s_PA, "./data/PAparaNS.csv")
