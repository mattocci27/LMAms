rm(list = ls()) # This clears everything from memory.
load("~/Dropbox/MS/LES_MS/data/LMAps_res_20160719.RData")

library(loo)
library(rstan)
library(dplyr)

model_name <- c("model1", "model2", "model3")

model_name2 <- 1:3


waic_temp <- NULL
for (j in 1:3){
  temp_name <- model_name[j]
  load(paste("PA_", temp_name, "_2016-07-19_.RData", sep = ""))
  waic_temp[j] <- waic(extract_log_lik(res,"log_lik"))$waic
}

load("~/Dropbox/MS/LES_MS/data/GL_obs_model1_2016-07-19_.RData")
GL_WAIC <- waic(extract_log_lik(res,"log_lik"))$waic
# "2k - 2ln(L)"
#

model <- c("GL_potential_LL", "PA_potential_LL",
  "PA_optimal_LL", "PA_optimal_LL_site_effect")

n.obs <- c(nrow(GL), rep(nrow(PA), 3))
npar <- c(5, 5, 6, 7)

WAIC <- round(c(GL_WAIC, waic_temp), 0)
para <- "log_alpha"
val <- "97.5%"


# rownames(PA.summary2.1a)[5] <- "q2"

# head(PA.summary2.1b)
# head(PA.summary2.1a)

Make.table <- function(para,val=c("mean", "2.5", "97.5"), exp = FALSE) {
   val2 <- switch(val, "mean" = 1, "2.5" = 4, "97.5" = 8)
   sum.vec <- c("GL_summary", "PA_summary_1",
       "PA_summary_2", "PA_summary_3")

  res2 <- NULL
  for (j in 1:4){ # numbers of models
    temp <- get(sum.vec[j])
    res <- try(temp[paste(para),val2])
    if (class(res) == "try-error") res <- NA

   res2 <- c(res2,res)
  }
  if (exp == TRUE) return(round(exp(res2),3)) else return(round(res2,3))
}


par.vec <- c("log_alpha", "log_beta", "rp", "rs", "q", "log_site")
val.vec <- c("mean", "2.5", "97.5")
ex.vec <- c(T, T, F, F, F, T)

res0 <- NULL
for (j in 1:6){ # numbers of paramters
  for (k in 1:3){ # mean, lower or upper
    res <- Make.table(par.vec[j], val.vec[k], ex.vec[j])
    res0 <- cbind(res0, res)
  }
}

res0
moge0 <- NULL
for (j in 1:6){ # numbers of paramters
  I1 <- 3 * j - 2
  I2 <- 3 * j - 1
  I3 <- 3 * j
 moge <- paste(res0[,I1], " [", res0[,I2], ", ", res0[,I3],"]", sep = "")
 moge0 <- cbind(moge0, moge)
}



res.tb <- data.frame(data = c("GLOPNET", rep("Panama", 3)),
         model = c("potential", "potential", "optimal", "optimal + site"),
          n.obs, npar, WAIC, moge0)

colnames(res.tb) <- c("data", "model", "n.obs", "npar", "WAIC",
          "alpha", "beta", "rp", "rs", "theta", "dry")

write.csv(res.tb,
     "/Users/mattocci/Dropbox/MS/LES_MS/data/result_parameter.csv",
     row.names = F)
