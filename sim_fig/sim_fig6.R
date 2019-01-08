rm(list = ls()) # This clears everything from memory.
library(rstan)
library(dplyr)
load("~/Dropbox/LES/GL_sim.RData")
library(loo)

P_vec <- paste("p[", 1:n_sample, "]" ,sep = "")
LL_vec <- paste("mu[", 1:n_sample, ",2]" ,sep = "")
my_breaks <- function(...){
  c(0.1, 0.5, 1, 5, 10, 20,  50, 100, 200, 500)
}
# LMA <- list.data.n$LMA
moge <- data_frame(LMA, LMAp, LMAs, DE = data$DE, LMAp1 = LMA * m1[P_vec, "mean"]) %>%
  mutate(LMAs1 = LMA - LMAp1) %>%
  mutate(LMAp2 = LMA * m2[P_vec, "mean"]) %>%
  mutate(LMAs2 = LMA - LMAp2) %>%
  mutate(LMAp3 = LMA * m3[P_vec, "mean"]) %>%
  mutate(LMAs3 = LMA - LMAp3) %>%
  mutate(preLL1 = exp(m1[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(preLL2 = exp(m2[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(preLL3 = exp(m3[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(p_true = f)


moge_fig <- moge %>%
  tidyr::gather("LMA_type", "val", c(1:3, 5:6)) %>%
  mutate(est = c(rep("True", 300), rep("Estimated", 200))) %>%
  mutate(est = factor(est, levels = c("True", "Estimated"))) %>%
  mutate("LMA_type2" = sapply(strsplit(LMA_type, "1"), "[", 1)) %>%
  mutate(dat = "f1") %>%
  mutate(DE = ifelse(DE == "D", "Deciduous", ifelse(DE == "E", "Evergreen", "U")))

pdf("~/Dropbox/MS/LES_MS/fig/mean_GL.PDF", width=5.2, height=5.2)
p <- moge_fig %>% filter(DE != "U") %>%
  filter(LMA_type2 != "LMA") %>% ggplot(., aes(y = val, factor(DE)))
p <- p + geom_boxplot(fill = "grey") + facet_grid(LMA_type2 ~ est, scales = "free") + scale_y_log10(breaks = my_breaks(), expand = c(0.05, 0))
p <- p + xlab("") + ylab("") + ggtitle("Simulated GL1")
p + theme_bw()
dev.off()
# # p
# z <- ggplotGrob(p)
# gtable_show_layout(z)
#
# z <- gtable_add_grob(z,
#   list(rectGrob(gp = gpar(col = NA, fill = grey(0.8), size = .5)),
#   textGrob("moge", vjust = .27,
#    gp = gpar(cex = .75, fontface = 'bold', col = "black"))),
#     1, 4, 1, 6, name = c("a", "b"))
# z <- gtable_add_rows(z, unit(2/10, "line"), 2)
#
# grid.draw(z)




load("~/Dropbox/MS/LES_MS/Data/GL_sim_dif_f.RData")
library(loo)

P_vec <- paste("p[", 1:n_sample, "]" ,sep = "")
LL_vec <- paste("mu[", 1:n_sample, ",2]" ,sep = "")

# LMA <- list.data.n$LMA
moge2 <- data_frame(LMA, LMAp, LMAs, DE = data$DE,  LMAp1 = LMA * m1[P_vec, "mean"]) %>%
  mutate(LMAs1 = LMA - LMAp1) %>%
  mutate(LMAp2 = LMA * m2[P_vec, "mean"]) %>%
  mutate(LMAs2 = LMA - LMAp2) %>%
  mutate(LMAp3 = LMA * m3[P_vec, "mean"]) %>%
  mutate(LMAs3 = LMA - LMAp3) %>%
  mutate(preLL1 = exp(m1[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(preLL2 = exp(m2[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(preLL3 = exp(m3[paste("mu[", 1:100, ",2]" ,sep = ""), "mean"])) %>%
  mutate(p_true = f)


moge_fig2 <- moge2 %>%
  tidyr::gather("LMA_type", "val", c(1:3, 5:6)) %>%
  mutate(est = c(rep("True", 300), rep("Estimated", 200))) %>%
  mutate(est = factor(est, levels = c("True", "Estimated"))) %>%
  mutate("LMA_type2" = sapply(strsplit(LMA_type, "1"), "[", 1)) %>%
  mutate(dat = "f2") %>%
  mutate(DE = ifelse(DE == "D", "Deciduous", ifelse(DE == "E", "Evergreen", "U")))
#
pdf("~/Dropbox/MS/LES_MS/fig/mean_GL2.PDF", width=5.2, height=5.2)
p2 <- moge_fig2 %>% filter(DE != "U") %>%
  filter(LMA_type2 != "LMA") %>% ggplot(., aes(y = val, factor(DE)))
p2 <- p2 + geom_boxplot(fill = "grey") + facet_grid(LMA_type2 ~ est, scales = "free") + scale_y_log10(breaks = my_breaks(), expand = c(0.05, 0))
p2 <- p2 + xlab("") + ylab("") + ggtitle("Simulated GL2")
p2 + theme_bw()
dev.off()
#
# moge_fig3 <- bind_rows(moge_fig, moge_fig2) %>%
#   mutate(est = factor(est, levels = c("True", "Estimated")))
#
# p3 <- moge_fig3 %>% filter(DE != "U") %>% ggplot(., aes(y = val, factor(DE)))
# p3 <- p3 + geom_boxplot() + facet_grid(LMA_type2 ~ dat  + est, scales = "free") + scale_y_log10()
# p3 <- p3 + xlab("") + ylab("")
# p3


#var --------------------------------
GLa <- var(moge$LMA)
GLp <- cov(moge$LMAp1,moge$LMA)
GLs <- cov(moge$LMAs1,moge$LMA)

m1_true <- c(GLs,GLp)/GLa

GLa <- var(moge$LMA)
GLp <- cov(moge$LMAp,moge$LMA)
GLs <- cov(moge$LMAs,moge$LMA)

m1_est <- c(GLs,GLp)/GLa

GLa <- var(moge2$LMA)
GLp <- cov(moge2$LMAp1,moge2$LMA)
GLs <- cov(moge2$LMAs1,moge2$LMA)

m2_true <- c(GLs,GLp)/GLa


GLa <- var(moge2$LMA)
GLp <- cov(moge2$LMAp,moge2$LMA)
GLs <- cov(moge2$LMAs,moge2$LMA)

m2_est <- c(GLs,GLp)/GLa

GLs / GLa
GLp / GLa

pdf("~/Dropbox/MS/LES_MS/fig/var_GL.PDF", width=6, height=4)
bar_dat <- data_frame(val = c(m1_true, m1_est, m2_true, m2_est) * 100,
  est = rep(c("True", "True", "Estimated", "Estimated"), 2),
  LMA = rep(c("LMAs", "LMAp"), 4)) %>%
  mutate(dat = rep(c("d1", "d2"), each = 4)) %>%
  mutate(est = factor(est, levels = c("True", "Estimated")))

bar_dat$label <- with(bar_dat,
    rep(c("Simulated GL1",
     "Simulated GL2"),each = 4))

p3 <- ggplot(bar_dat, aes(y = val, x = est, fill = LMA))
p3 <- p3 + geom_bar(stat = "identity") + facet_grid(~label)
p3 <- p3 + ylab("% interspecific LMA variation") + xlab("")
p3 <- p3 +  guides(fill=guide_legend(title=NULL))

p3 + theme_bw()
dev.off()
