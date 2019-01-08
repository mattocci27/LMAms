
load("./data/GL1_potLL_obs.rda")
load("./data/PA_sitePL_obs.rda")
load("./data/PA_sitePL2_obs.rda")

pairs(res, pars = c("p[20]", "beta[1]", "lp__"), las = 1) # below the diagonal


param_tmp <- as.data.frame(extract(res, permuted=FALSE))
samppars_tmp <- get_sampler_params(res, inc_warmup = FALSE)
chain <- 2
param <- "beta[1]"

plot(param_tmp[,paste("chain:",chain,".",param,sep="")],
samppars_tmp[[chain]][,"energy__"], 
col = my_col[chain],
pch=16, xlab=param, ylab="Energy", cex.lab=1.5)


my_col <- c("red", "blue", "green")

pairs_stan <- function(chain, stan_model, pars) {
  energy <- as.matrix(sapply(get_sampler_params(stan_model, inc_warmup = F), 
                             function(x) x[,"energy__"]))
  pars <- extract(stan_model, pars = pars, permuted = F)
  df <- data.frame(energy[,chain], pars[,chain,])
  names(df)[1] <- "energy"
  GGally::ggpairs(df, title = paste0("Chain", chain), 
                  lower = list(continuous = GGally::wrap("points", alpha = 0.2)))                    
}

pairs_stan(1, res, pars = c("beta[1]", "beta[2]"))
pairs_stan(1, res, pars = c("beta[3]", "beta[4]"))
pairs_stan(1, res, pars = c("beta[5]","beta[6]", "beta[7]"))
