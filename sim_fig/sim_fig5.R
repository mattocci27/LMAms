rm(list = ls()) # This clears everything from memory.
library(rstan)
library(dplyr)
library(loo)


name_func <- function(x) ifelse(x == "A", "Aarea", ifelse(x == "R", "Rarea", "LL"))

my_breaks <- function(...){
  c(0.1, 0.5, 1, 5, 10, 50, 100, 200, 500)
}

my_breaks_x <- function(...){
  c(0.1, 0.5, 1, 5, 10, 50, 100, 200, 500)
}

# GL sim ---------------------------------------------------------------
load("~/Dropbox/LES/GL_sim.RData")
moge <- data %>%
  tidyr::gather("LMA_type", "LMA", 4:6) %>%
  tidyr::gather("trait_type", "trait", 1:3) %>%
  mutate(trait_type2 = name_func(trait_type)) %>%
  mutate(trait_type2 = factor(trait_type2,
    levels = c("Aarea", "Rarea", "LL"))) %>%
  mutate(DE2 = ifelse(DE == "D", "Deciduous", ifelse(DE == "E", "Evergreen", "Unclassified")))

pdf("~/Dropbox/MS/LES_MS/fig/GL_sim1.PDF", width=7, height=5.5)
p <- ggplot(moge, aes(x = LMA, y = trait, fill = DE2))
p <- p + geom_point(colour = "black", shape = 21) +
  facet_grid(trait_type2 ~ LMA_type, scales = "free") +
  theme_bw() +
  ylab("Simulated traits") +
  xlab("Simluated LMA") +
  guides(colour=guide_legend(title=NULL)) +
  scale_fill_manual(values = c("dark orange", "green4", "white")) +
  guides(fill=guide_legend(title=NULL)) +
  scale_x_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  theme(axis.text.x = element_text(angle =45))
p
dev.off()

#GL sim different f---------------------------------------------------
load("~/Dropbox/MS/LES_MS/data/GL_sim_dif_f.RData")
moge2 <- data %>%
  tidyr::gather("LMA_type", "LMA", 4:6) %>%
  tidyr::gather("trait_type", "trait", 1:3) %>%
  mutate(trait_type2 = name_func(trait_type)) %>%
  mutate(trait_type2 = factor(trait_type2,
    levels = c("Aarea", "Rarea", "LL"))) %>%
  mutate(DE2 = ifelse(DE == "D", "Deciduous", ifelse(DE == "E", "Evergreen", "Unclassified")))

pdf("~/Dropbox/MS/LES_MS/fig/GL_sim2.PDF", width=7, height=5.5)
p <- ggplot(moge2, aes(x = LMA, y = trait, fill = DE2))
p <- p + geom_point(colour = "black", shape = 21) +
  facet_grid(trait_type2 ~ LMA_type, scales = "free") +
  theme_bw() +
  ylab("Simulated traits") +
  xlab("Simluated LMA") +
  guides(colour=guide_legend(title=NULL)) +
  scale_fill_manual(values = c("dark orange", "green4", "white")) +
  guides(fill=guide_legend(title=NULL)) +
  scale_x_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  theme(axis.text.x = element_text(angle =45))
p
dev.off()
#PA --------------------
load("~/Dropbox/LES/PA_sim.RData")
moge3 <- data %>%
  tidyr::gather("LMA_type", "LMA", 4:6) %>%
  tidyr::gather("trait_type", "trait", 1:3) %>%
  mutate(trait_type2 = name_func(trait_type)) %>%
  mutate(trait_type2 = factor(trait_type2,
    levels = c("Aarea", "Rarea", "LL"))) %>%
  mutate(DE2 = factor(DE, levels = c("sun_wet", "sun_dry", "shade_wet", "shade_dry")))

pdf("~/Dropbox/MS/LES_MS/fig/PA_sim.PDF", width=8, height=7)
p <- ggplot(moge3, aes(x = LMA, y = trait, fill = DE2))
p <- p + geom_point(colour = "black", shape = 21) +
  facet_grid(trait_type2 ~ LMA_type, scales = "free") +
  theme_bw() +
  ylab("Simulated traits") +
  xlab("Simluated LMA") +
  guides(colour=guide_legend(title=NULL)) +
  guides(fill=guide_legend(title=NULL)) +
  scale_colour_manual(values =
    c("dodgerblue3", "burlywood4", "black", "black")) +
  scale_fill_manual(values =
    c("white", "white", "dodgerblue3", "burlywood4")) +
  scale_x_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  theme(axis.text.x = element_text(angle =45))
p
dev.off()


pdf("~/Dropbox/MS/LES_MS/fig/PA_sim_LL.PDF", width=4.5, height=3)
data2 <- data %>%
    mutate(DE2 = factor(DE, levels = c("sun_wet", "sun_dry", "shade_wet", "shade_dry")))
p <- ggplot(data2, aes(x = exp(log_mu2), y = LL, colour = DE2, fill = DE2))
p <- p + geom_point(shape = 21) + scale_y_log10() + scale_x_log10()
p <- p + theme_bw() + ylab("LL") + xlab("Optimal LL")
p <- p + guides(colour=guide_legend(title=NULL))
p <- p + scale_colour_manual(values = c("dodgerblue3", "burlywood4", "black", "black"))
p <- p + scale_fill_manual(values = c("white", "white", "dodgerblue3", "burlywood4"))
p + guides(fill=guide_legend(title=NULL))

dev.off()

#null --------------------
load("~/Dropbox/LES/null_sim.RData")
moge4 <- data %>%
  tidyr::gather("LMA_type", "LMA", 4:6) %>%
  tidyr::gather("trait_type", "trait", 1:3) %>%
  mutate(trait_type2 = name_func(trait_type)) %>%
  mutate(trait_type2 = factor(trait_type2,
    levels = c("Aarea", "Rarea", "LL")))

pdf("~/Dropbox/MS/LES_MS/fig/null_sim.PDF", width=8, height=7)
p <- ggplot(moge4, aes(x = LMA, y = trait))
p <- p + geom_point() +
  facet_grid(trait_type2 ~ LMA_type, scales = "free") +
  theme_bw() +
  ylab("Simulated traits") +
  xlab("Simluated LMA") +
  guides(colour=guide_legend(title=NULL)) +
  scale_x_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  theme(axis.text.x = element_text(angle =45))
p
dev.off()
