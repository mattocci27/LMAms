d <- read_csv("./data/nature02403-s2.csv", skip = 10)

dd <- data.frame(Narea = 10^d$`log Narea`,
        Parea = 10^d$`log Parea`,
        sp = d$Species) %>%
        group_by(sp) %>%
        summarize(Narea = mean(Narea, na.omit = T),
            Parea = mean(Parea, na.omit = T))

load("./data/GL_m0_N_obs.rda")
resGL <- res
GL_summary <- data.frame(summary(res)$summary)
GL <- dat
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) 

mu2_vec <- paste("mu2[", 1:nrow(GL), "]", sep = "")
temp_LMAp <- GL_summary[P_vec, "mean"] * GL$LMA
temp_LMAs <- GL$LMA - temp_LMAp
temp_LL <- exp(GL_summary[mu2_vec, "mean"])

GL <- GL %>%
  mutate(LMAp = temp_LMAp) %>%
  mutate(LMAs = temp_LMAs) %>%
  mutate(preLL = temp_LL) 

GL <- left_join(GL, dd, by = "sp") %>%
  mutate(gr = factor(DE,
    labels = c("Deciduous",
               "Evergreen",
               "Unclassified"
                      ))) %>%
  arrange(sp)


load("./data/PA_m1q_obs.rda")
resPA <- res
#summary_PA_LDL <- data.frame(summary(res)$summary)

summary_PA_LDL <- data.frame(summary(res)$summary)

PA <- dat %>%
  as_data_frame %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
  mutate(site_strata = paste(site2, strata, sep = "_"))


P_vec <- paste("p[", 1:nrow(PA), "]", sep = "")
mu2_vec <- paste("mu2[", 1:nrow(PA), "]", sep = "")
#Mu_vec <- paste("mu[", 1:nrow(PA), ",2]", sep = "")
temp_LMAp <- summary_PA_LDL[P_vec, "mean"] * PA$LMA
temp_LMAs <- PA$LMA - temp_LMAp
temp_LL <- exp(summary_PA_LDL[mu2_vec, "mean"])


PA <- PA %>%
  mutate(LMAp = temp_LMAp) %>%
  mutate(LMAs = temp_LMAs) %>%
  mutate(preLL = temp_LL) %>%
  mutate(LDs = LMAs/LT/1000)

# R values

bayes_R2 <- function(y, LMA) {
#  y <- log(GL$LL)
#  LMA <- "log_LMAp"
  ypred <- rstan::extract(res, paste(LMA)) %>%
    as.data.frame %>%
    as.matrix 

  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  r2 <- var_ypred / (var_ypred + var_e)

  mid <- median(r2) %>% round(3)
  up <- quantile(r2, 0.975) %>% round(3)
  lo <- quantile(r2, 0.025) %>% round(3)
  
  paste0(mid, " [", lo, ", ", up, "]")
}

# similar 
bayes_R22 <- function(y, LMA) {
  ypred <- rstan::extract(res, paste(LMA)) %>%
    as.data.frame %>%
    as.matrix 
  r2 <- apply(ypred, 1, function(x)cor.test(x, y))
  mid <- median(r2) %>% round(3)
  up <- quantile(r2, 0.975) %>% round(3)
  lo <- quantile(r2, 0.025) %>% round(3)
  
  paste0(mid, " [", lo, ", ", up, "]")
}



# similar 
bayes_R23 <- function(y, LMA) {
  ypred <- rstan::extract(res, paste(LMA)) %>%
    as.data.frame %>%
    as.matrix 
  
  r2 <- apply(ypred, 1, function(x)cor(x, y))
  mid <- median(r2) %>% round(3)
  
  r2 <- apply(ypred, 1, function(x)cor.test(x, y)$conf.int[1])
  lo <- median(r2) %>% round(3)
  
  r2 <- apply(ypred, 1, function(x)cor.test(x, y)$conf.int[2])
  up <- median(r2) %>% round(3)

  paste0(mid, " [", lo, ", ", up, "]")
}



# similar 
bayes_R24 <- function(y, LMA, res) {
  ypred <- rstan::extract(res, paste(LMA)) %>%
    as.data.frame %>%
    as.matrix 
  r2 <- apply(ypred, 1, function(x)cor(x, y))
  median(r2) %>% round(2)
}

# NA 
bayes_R25 <- function(y, LMA, res_data) {
 # res <- resGL
 # LMA <- "log_LMAs"
 # y <- log(GL$Narea)
  ypred <- rstan::extract(res_data, paste(LMA)) %>%
    as.data.frame %>%
    as.matrix 
  ypred2 <- rbind(y, ypred) %>% t
  ypred3 <- na.omit(ypred2) %>% as.matrix %>% t
  ypred4 <- ypred3[-1,]
  y2 <- ypred3[1,]
  r2 <- apply(ypred4, 1, function(x)cor(x, y2))
  median(r2) %>% round(2)
}

output <- "moge.yml"
out <- file(paste(output), "w") # write

log_LMAp <- rstan::extract(resGL, "log_LMAp")[[1]]
log_LMAs <- rstan::extract(resGL, "log_LMAs")[[1]]

writeLines(paste0("r_vals:"),
           out,
           sep = "\n")
writeLines(paste0("\tGL:"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_Aarea: 'italic(r) == ", cor.test(log(GL$Aarea), log(GL$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_Aarea: 'italic(r) == ", bayes_R25(log(GL$Aarea), "log_LMAp", resGL),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_Aarea: 'italic(r) == ", bayes_R25(log(GL$Aarea), "log_LMAs", resGL),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_Rarea: 'italic(r) == ", cor.test(log(GL$Rarea), log(GL$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_Rarea: 'italic(r) == ", bayes_R25(log(GL$Rarea), "log_LMAp", resGL),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_Rarea: 'italic(r) == ", bayes_R25(log(GL$Rarea), "log_LMAs", resGL),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_LL: 'italic(r) == ", cor.test(log(GL$LL), log(GL$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_LL: 'italic(r) == ", bayes_R25(log(GL$LL), "log_LMAp", resGL),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_LL: 'italic(r) == ", bayes_R25(log(GL$LL), "log_LMAs", resGL),"'"),
           out,
           sep = "\n")

# GL NP
writeLines(paste0("\tGL:"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_Narea: 'italic(r) == ", cor.test(log(GL$Narea), log(GL$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_Narea: 'italic(r) == ", bayes_R25(log(GL$Narea), "log_LMAp", resGL),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_Narea: 'italic(r) == ", bayes_R25(log(GL$Narea), "log_LMAs", resGL),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_Parea: 'italic(r) == ", cor.test(log(GL$Parea), log(GL$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_Parea: 'italic(r) == ", bayes_R25(log(GL$Parea), "log_LMAp", resGL),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_Parea: 'italic(r) == ", bayes_R25(log(GL$Parea), "log_LMAs", resGL),"'"),
           out,
           sep = "\n")


log_LMAp <- rstan::extract(resPA, "log_LMAp")[[1]]
log_LMAs <- rstan::extract(resPA, "log_LMAs")[[1]]

writeLines(paste0("\tPA:"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_Aarea: 'italic(r) == ", cor.test(log(PA$Aarea), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_Aarea: 'italic(r) == ", bayes_R25(log(PA$Aarea), "log_LMAp", resPA),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_Aarea: 'italic(r) == ", bayes_R25(log(PA$Aarea), "log_LMAs", resPA),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_Rarea: 'italic(r) == ", cor.test(log(PA$Rarea), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_Rarea: 'italic(r) == ", bayes_R25(log(PA$Rarea), "log_LMAp", resPA),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_Rarea: 'italic(r) == ", bayes_R25(log(PA$Rarea), "log_LMAs", resPA),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_LL: 'italic(r) == ", cor.test(log(PA$LL), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_LL: 'italic(r) == ", bayes_R25(log(PA$LL), "log_LMAp", resPA),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_LL: 'italic(r) == ", bayes_R25(log(PA$LL), "log_LMAs", resPA),"'"),
           out,
           sep = "\n")

# PA NP
writeLines(paste0("\tPA:"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_Narea: 'italic(r) == ", cor.test(log(PA$Narea), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_Narea: 'italic(r) == ", bayes_R25(log(PA$Narea), "log_LMAp", resPA),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_Narea: 'italic(r) == ", bayes_R25(log(PA$Narea), "log_LMAs", resPA),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_Parea: 'italic(r) == ", cor.test(log(PA$Parea), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_Parea: 'italic(r) == ", bayes_R25(log(PA$Parea), "log_LMAp", resPA),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_Parea: 'italic(r) == ", bayes_R25(log(PA$Parea), "log_LMAs", resPA),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMA_cell_area: 'italic(r) == ", cor.test(log(PA$cell_area), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAp_cell_area: 'italic(r) == ", bayes_R25(log(PA$cell_area), "log_LMAp", resPA),"'"),
           out,
           sep = "\n")
writeLines(paste0("\t\tLMAs_cell_area: 'italic(r) == ", bayes_R25(log(PA$cell_area), "log_LMAs", resPA),"'"),
           out,
           sep = "\n")

close(out)


#bayes_R25 <- function(y, LMA, res) {
#  res <- resPA
#  y <- log(PA$cell_area)
#  ypred <- rstan::extract(res, paste(LMA)) %>%
#    as.data.frame %>%
#    as.matrix 
#  ypred2 <- rbind(y, ypred) %>% as.data.frame %>% t
#  ypred3 <- na.omit(ypred2) %>% as.matrix %>% t
#  ypred4 <- ypred3[-1,]
#  y2 <- ypred3[1,]
#  r2 <- apply(ypred4, 1, function(x)cor(x, y2))
#  median(r2) %>% round(2)
#}
#
