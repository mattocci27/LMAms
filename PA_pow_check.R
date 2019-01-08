library(dplyr)
library(rstan)
library(loo)

temp <- c("model_LMA1", "model_LMA2", "model_LMA3", "model_LMA1p", "model_LMA2p", "model_LMA3p")
temp2 <- paste("PA_LMA", temp, "2017-05-23", ".RData", sep = "_")

waic_vec <- NULL
for (j in 1:length(temp2)){
  load(temp2[j])
  waic_vec[j] <- waic_res
  print(paste(j,j,j,j,j,j,j,j,j,sep= "="))
  fit.summary %>% summary %>% print
}


temp3<- c("model1_p", "model2_p", "model3_p", "model4_p")
temp4 <- paste("./data/PA", temp3, "2017-05-24", ".RData", sep = "_")


waic_vec2 <- NULL
for (j in 1:length(temp4)){
  load(temp4[j])
  waic_vec2[j] <- waic_res
  print(paste(j,j,j,j,j,j,j,j,j,sep= "="))
  fit.summary %>% summary %>% print
}

waic_vec2



