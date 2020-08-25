library(rstan)
library(tidyverse)

load("./rda/GL_LMAms_more_obs.rda")

s_GL <- data.frame(summary(res)$summary) %>%
  mutate(para = rownames(.)) %>%
  mutate(sig = ifelse(`X2.5.` < 0 & `X97.5.` < 0 ,
         "sig",
         "nonsig")) %>%
  mutate(sig = ifelse(`X2.5.` > 0 & `X97.5.` > 0 ,
         "sig",
         sig))

write_csv(s_GL, "./data/GLpara.csv")


load("./rda/PA_LMAms_L0_more_obs.rda")

s_PA <- data.frame(summary(res)$summary) %>%
  mutate(para = rownames(.)) %>%
  mutate(sig = ifelse(`X2.5.` < 0 & `X97.5.` < 0 ,
         "sig",
         "nonsig")) %>%
  mutate(sig = ifelse(`X2.5.` > 0 & `X97.5.` > 0 ,
         "sig",
         sig))

write_csv(s_PA, "./data/PApara.csv")

