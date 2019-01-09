library(rstan)
library(dplyr)

d <- read_csv("./data/nature02403-s2.csv", skip = 10)

dd <- data.frame(Narea = 10^d$`log Narea`,
        Parea = 10^d$`log Parea`,
        sp = d$Species) %>%
        group_by(sp) %>%
        summarize(Narea = mean(Narea, na.omit = T),
            Parea = mean(Parea, na.omit = T))

load("./data/GL_m0_obs.rda")
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

write.csv(GL, "./data/GL_m0.csv", row.names = FALSE)


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

write.csv(GL, "./data/GL_m0_N.csv", row.names = FALSE)

load("./data/PA_m1q_more_obs.rda")
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

write.csv(PA, "./data/PA_m1q_more.csv", row.names = FALSE)

load("./data/PA_m1q_more_N_obs.rda")
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

write.csv(PA, "./data/PA_m1q_more_N.csv", row.names = FALSE)

load("./data/PA_m1q_more_NS_more_obs.rda")
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

write.csv(PA, "./data/PA_m1q_more_NS.csv", row.names = FALSE)

y <- PA$LL %>% log
ypred <- rstan::extract(res, "mu2")[[1]]
plot(y ~ apply(ypred,2,mean))
e <- -1 * sweep(ypred, 2, y)
var_ypred <- apply(ypred, 1, var)
var_e <- apply(e, 1, var)
r2 <- var_ypred / (var_ypred + var_e) 
hist(r2)

log_LMAp <- rstan::extract(res, "log_LMAp")[[1]]
log_LMAs <- rstan::extract(res, "log_LMAs")[[1]]

my_cor <- function(x, y){
  lo <- apply(x, 1, function(x)cor(x, y)) %>%
    quantile(0.025) %>%
    round(3)
  mid <- apply(x, 1, function(x)cor(x, y)) %>%
    quantile(0.5) %>%
    round(3)
  up <- apply(x, 1, function(x)cor(x, y)) %>%
    quantile(0.975) %>%
    round(3)
  paste0(mid, " [", lo, ", ", up, "]")
}


#cor.test(log_LMAs[2,], log(GL$Rarea))
#
#my_cor(log_LMAs, log(GL$Rarea))
#
#moge <- apply(log_LMAs, 2, median)
#cor.test(moge, log(GL$Rarea))

# R values
output <- "r_val.yml"
out <- file(paste(output), "w") # write

writeLines(paste0("r_vals:"),
           out,
           sep = "\n")
writeLines(paste0("  GL:"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_Aarea: 'italic(r) == ", cor.test(log(GL$Aarea), log(GL$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_Aarea: 'italic(r) == ", cor.test(log(GL$Aarea), log(GL$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_Aarea: 'italic(r) == ", cor.test(log(GL$Aarea), log(GL$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_Rarea: 'italic(r) == ", cor.test(log(GL$Rarea), log(GL$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_Rarea: 'italic(r) == ", cor.test(log(GL$Rarea), log(GL$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_Rarea: 'italic(r) == ", cor.test(log(GL$Rarea), log(GL$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_LL: 'italic(r) == ", cor.test(log(GL$LL), log(GL$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_LL: 'italic(r) == ", cor.test(log(GL$LL), log(GL$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_LL: 'italic(r) == ", cor.test(log(GL$LL), log(GL$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")

writeLines(paste0("  GL_NP:"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_Narea: 'italic(r) == ", cor.test(log(GL$Narea), log(GL$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_Narea: 'italic(r) == ", cor.test(log(GL$Narea), log(GL$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_Narea: 'italic(r) == ", cor.test(log(GL$Narea), log(GL$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_Parea: 'italic(r) == ", cor.test(log(GL$Parea), log(GL$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_Parea: 'italic(r) == ", cor.test(log(GL$Parea), log(GL$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_Parea: 'italic(r) == ", cor.test(log(GL$Parea), log(GL$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")


writeLines(paste0("  GL_LMAms:"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_LMAm: ", cor.test(log(GL$LMAp), log(GL$LMAs))$estimate %>% round(2)),
           out,
           sep = "\n")
writeLines(paste0("    p_val: ", cor.test(log(GL$LMAp), log(GL$LMAs))$p.value %>% round(2)),
           out,
           sep = "\n")

writeLines(paste0("  PA:"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_Aarea: 'italic(r) == ", cor.test(log(PA$Aarea), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_Aarea: 'italic(r) == ", cor.test(log(PA$Aarea), log(PA$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_Aarea: 'italic(r) == ", cor.test(log(PA$Aarea), log(PA$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_Rarea: 'italic(r) == ", cor.test(log(PA$Rarea), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_Rarea: 'italic(r) == ", cor.test(log(PA$Rarea), log(PA$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_Rarea: 'italic(r) == ", cor.test(log(PA$Rarea), log(PA$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_LL: 'italic(r) == ", cor.test(log(PA$LL), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_LL: 'italic(r) == ", cor.test(log(PA$LL), log(PA$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_LL: 'italic(r) == ", cor.test(log(PA$LL), log(PA$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")


writeLines(paste0("  PA_LMAms:"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_LMAm: ", cor.test(log(PA$LMAp), log(PA$LMAs))$estimate %>% round(2)),
           out,
           sep = "\n")
writeLines(paste0("    p_val: ", cor.test(log(PA$LMAp), log(PA$LMAs))$p.value %>% round(2)),
           out,
           sep = "\n")


writeLines(paste0("  PA_NP:"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_Narea: 'italic(r) == ", cor.test(log(PA$Narea), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_Narea: 'italic(r) == ", cor.test(log(PA$Narea), log(PA$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_Narea: 'italic(r) == ", cor.test(log(PA$Narea), log(PA$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_Parea: 'italic(r) == ", cor.test(log(PA$Parea), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_Parea: 'italic(r) == ", cor.test(log(PA$Parea), log(PA$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_Parea: 'italic(r) == ", cor.test(log(PA$Parea), log(PA$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMA_cell_area: 'italic(r) == ", cor.test(log(PA$cell_area), log(PA$LMA))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAp_cell_area: 'italic(r) == ", cor.test(log(PA$cell_area), log(PA$LMAp))$estimate %>% round(2),"'"),
           out,
           sep = "\n")
writeLines(paste0("    LMAs_cell_area: 'italic(r) == ", cor.test(log(PA$cell_area), log(PA$LMAs))$estimate %>% round(2),"'"),
           out,
           sep = "\n")

writeLines(paste0("  PA_LL:"),
           out,
           sep = "\n")
writeLines(paste0("    preLL_LL: 'italic(r) == ", cor.test(log(PA$LL), log(PA$preLL))$estimate %>% round(2),"'"),
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