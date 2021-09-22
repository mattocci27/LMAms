library(tidyverse)
library(stringr)
library(lazyeval)
library(scales)
library(cowplot)
library(ggrepel)
library(ggthemes)

settings <- yaml::yaml.load_file("settings.yml")
r_vals <- yaml::yaml.load_file("r_val.yml")
p_letters <- yaml::yaml.load_file("letters.yml")
source("fig_theme.r")

# cor fig -----------------------------------------------------------------------

load("data/GL_m0_N_rand.rda")

r <- NULL
for (i in 1:10) {
  GL_summary <- rand_res$summary[[i]]
  dat_r <- rand_res$data[[i]]

  GL <- dat %>%
    mutate(LMA = dat_r$LMA,
           LL = dat_r$LL,
           Aarea = dat_r$A,
           Rarea = dat_r$R) %>%
    as_data_frame %>%
    mutate(LMAp = GL_summary[stringr::str_detect(rownames(GL_summary),
                              "log_LMAp"), "X50."] %>%  exp) %>%
    mutate(LMAs = GL_summary[stringr::str_detect(rownames(GL_summary),
                              "log_LMAs"), "X50."] %>%  exp) #%>%

  r_new <- c(cor(log(GL$LMAp), log(GL$Aarea)),
    cor(log(GL$LMAp), log(GL$Rarea)),
    cor(log(GL$LMAp), log(GL$LL)),
    cor(log(GL$LMAs), log(GL$Aarea)),
    cor(log(GL$LMAs), log(GL$Rarea)),
    cor(log(GL$LMAs), log(GL$LL)))
  
  r <- rbind(r, r_new)
}




colnames(r) <- c("LMAm-Aarea", "LMAm-Rarea", "LMAm-LL", "LMAs-Aarea", "LMAs-Rarea", "LMAs-LL")

r2 <- r %>%
  as.data.frame %>%
  gather(trait,val,1:6)

ggplot(r2, aes(x = val, y = trait)) +
  geom_point(col = "grey") 

#load("./data/PA_m1q_more_NS_more_rand.rda")
load("./data/PA_m1q_more_N_more_rand.rda")
load("./data/PA_m1q_more_more_rand.rda")

r_PA <- NULL

for (i in 1:10) {
  PA_summary <- rand_res$summary[[i]]
  dat_r <- rand_res$data[[i]]

  PA <- dat %>%
    mutate(LMA = dat_r$LMA,
           LL = dat_r$LL,
           Aarea = dat_r$A,
           Rarea = dat_r$R) %>%
    as_data_frame %>%
    mutate(LMAp = PA_summary[stringr::str_detect(rownames(PA_summary),
                              "log_LMAp"), "X50."] %>%  exp) %>%
    mutate(LMAs = PA_summary[stringr::str_detect(rownames(PA_summary),
                              "log_LMAs"), "X50."] %>%  exp) %>%
    mutate(preLL = PA_summary[stringr::str_detect(rownames(PA_summary),
                              "mu2"), "X50."] %>%  exp) #%>%

  r_new <- c(cor(log(PA$LMAp), log(PA$Aarea)),
    cor(log(PA$LMAp), log(PA$Rarea)),
    cor(log(PA$LMAp), log(PA$LL)),
    cor(log(PA$LMAs), log(PA$Aarea)),
    cor(log(PA$LMAs), log(PA$Rarea)),
    cor(log(PA$LMAs), log(PA$LL)),
    cor(log(PA$preLL), log(PA$LL)))
  
  r_PA <- rbind(r_PA, r_new)
}

colnames(r_PA) <- c("LMAm-Aarea", "LMAm-Rarea", "LMAm-LL", "LMAs-Aarea", "LMAs-Rarea", "LMAs-LL", "preLL-LL")

r_PA2 <- r_PA %>%
  as.data.frame %>%
  gather(trait,val,1:7)

r_obs <- c(0.82, 0.85, -0.4, 0.11, -0.26, 0.55, 0.79)

r_obs2 <- data_frame(r_obs, 
                     trait = colnames(r_PA))

ggplot(r_PA2, aes(x = val, y = trait)) +
  geom_point(col = "grey") +
  geom_vline(xintercept = 0) +
  geom_point(data = r_obs2, aes(x = r_obs, y = trait))


##

get_rp <- function(data){
  beta1 <- rstan::extract(data, par = "beta[1]")[[1]]
  beta2 <- rstan::extract(data, par = "beta[2]")[[1]]
  beta3 <- rstan::extract(data, par = "beta[3]")[[1]]
  beta4 <- rstan::extract(data, par = "beta[4]")[[1]]
  beta5 <- rstan::extract(data, par = "beta[5]")[[1]]
  beta6 <- rstan::extract(data, par = "beta[6]")[[1]]

  data_frame(par = paste0("beta", 1:6) %>% 
              rep(each = length(beta1)),
             beta = c(beta1, beta2, beta3, beta4, beta5, beta6)
             )
}

moge <- map(rand_res$model, get_rp) %>% unnest


moge2 <- rand_res %>%
   transmute(beta = map(rand_res$model, get_rp)) %>% 
   mutate(rep = 1:nrow(.)) %>%
   mutate(rep = as.factor(rep)) %>%
   unnest 

ggplot(moge2 %>% filter(rep == 8), aes(beta)) +
 geom_histogram() +
 facet_wrap(~par, scale = "free") +
 geom_vline(xintercept = 0)

ggplot(moge2 %>% filter(par == "beta7"), aes(beta)) +
 geom_histogram() +
 facet_wrap(~ rep, scale = "free") +
 geom_vline(xintercept = 0)

ggplot(moge2, aes(beta)) +
 geom_histogram() +
 facet_wrap(~par, scale = "free") +
 geom_vline(xintercept = 0)



rand_res$summary[[6]] %>% head

 
 unlist(moge) %>% hist
unlist(moge) %>% quantile(0.025)
unlist(moge) %>% quantile(0.975)

plot(rand_res$model[[2]], pars = "beta")



r_dat3 <- read_csv("./data/r2_fig_dat.csv")

cor_plot <- ggplot(r_dat3, aes(x = obs, 
                     y = rand, 
                     label = trait2, 
                     col = ex)) +
      geom_vline(xintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_hline(yintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_abline(aes(slope = 1, intercept = 0), 
                  lty = 1, 
                  lwd = 0.25, 
                  col = "gray20") +
      geom_point() +
      geom_errorbar(aes(ymin = min, ymax = max), width = 0) + 
      geom_text_repel(size = 2, col = "gray20",
                      point.padding = unit(0.1, "lines"),
                      segment.color = "grey50",
                      segment.size = 0.25,
                      angle = 90,
                      max.iter = 20000, # try more
                      #aes(angle = ifelse(obs < 0, 0, 90)),
                      parse = TRUE
                      ) +
      facet_wrap(~site2, scale = "free") +
      theme_classic() +
      theme(axis.ticks.length = unit(-0.15, "cm"),
            axis.ticks = element_line(size = 0.3),
            axis.text.x = element_text(margin = margin(0.25, unit = "cm"),
                                       size = unit(9, "pt")),
            axis.text.y = element_text(hjust = 0.5,
                                       margin = margin(r = 0.25, unit = "cm"),
                                       debug = FALSE,
                                       angle = 90,
                                       size = unit(9, "pt")),
            panel.spacing = unit(1, "lines"),
            axis.line = element_line(colour = "black", size = 0.5),
            plot.background = element_blank(),
            strip.text.x = element_text(face = "bold"),
            #strip.background = element_blank(),
            strip.background = element_blank(),
            panel.grid = element_blank()) +
      xlim(-0.6, 1) +
      ylim(-0.6, 1) +
      guides(col = FALSE) +
      ylab(expression(paste(italic("r"[rand])))) +
      xlab(expression(paste(italic("r"[obs]))))

my_ggsave("./figs/cor_plot.png", cor_plot,
          width = 11,
          height = 5.5)

cor_plot2 <- ggplot(r_dat3, aes(x = rand, 
                     y = obs, 
                     label = trait2, 
                     col = ex)) +
      geom_vline(xintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_hline(yintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_abline(aes(slope = 1, intercept = 0), 
                  lty = 1, 
                  lwd = 0.25, 
                  col = "gray20") +
      geom_point() +
      geom_errorbarh(aes(xmin = min, xmax = max), height = 0) + 
      geom_text_repel(size = 6 * 5/14, col = "gray20",
                      point.padding = unit(0.1, "lines"),
                      segment.color = "grey50",
                      segment.size = 0.25,
                      angle = 0,
                      max.iter = 50000, # try more
                      #aes(angle = ifelse(obs < 0, 0, 90)),
                      parse = TRUE
                      ) +
      facet_wrap(~site2, scale = "free") +
      theme_classic() +
      theme(axis.ticks.length = unit(-0.15, "cm"),
            axis.ticks = element_line(size = 0.3),
            axis.text.x = element_text(margin = margin(0.25, unit = "cm"),
                                       size = unit(9, "pt")),
            axis.text.y = element_text(hjust = 0.5,
                                       margin = margin(r = 0.25, unit = "cm"),
                                       debug = FALSE,
                                       angle = 90,
                                       size = unit(9, "pt")),
            panel.spacing = unit(1, "lines"),
            axis.line = element_line(colour = "black", size = 0.5),
            plot.background = element_blank(),
            strip.text.x = element_text(face = "bold"),
            #strip.background = element_blank(),
            strip.background = element_blank(),
            panel.grid = element_blank()) +
      xlim(-0.6, 1) +
      ylim(-0.6, 1) +
      guides(col = FALSE) +
      ylab(expression(paste(italic("r"[obs])))) +
      xlab(expression(paste(italic("r"[rand]))))

my_ggsave("./figs/cor_plot2.png", cor_plot2,
          width = 11,
          height = 5.5)

cor_plot3 <- ggplot(r_dat3, aes(x = rand, 
                     y = obs, 
                     label = trait2, 
                     col = ex)) +
      geom_vline(xintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_hline(yintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_abline(aes(slope = 1, intercept = 0), 
                  lty = 1, 
                  lwd = 0.25, 
                  col = "gray20") +
      geom_point() +
      geom_errorbarh(aes(xmin = min, xmax = max), height = 0) + 
      geom_text(size = 6 * 5/14, col = "gray20",
                      aes(x = 0.7),
                      angle = 0,
                      #aes(angle = ifelse(obs < 0, 0, 90)),
                      parse = TRUE
                      ) +
      facet_wrap(~site2, scale = "free") +
      theme_classic() +
      theme(axis.ticks.length = unit(-0.15, "cm"),
            axis.ticks = element_line(size = 0.3),
            axis.text.x = element_text(margin = margin(0.25, unit = "cm"),
                                       size = unit(9, "pt")),
            axis.text.y = element_text(hjust = 0.5,
                                       margin = margin(r = 0.25, unit = "cm"),
                                       debug = FALSE,
                                       angle = 90,
                                       size = unit(9, "pt")),
            panel.spacing = unit(1, "lines"),
            axis.line = element_line(colour = "black", size = 0.5),
            plot.background = element_blank(),
            strip.text.x = element_text(face = "bold"),
            #strip.background = element_blank(),
            strip.background = element_blank(),
            panel.grid = element_blank()) +
      xlim(-0.6, 1) +
      ylim(-0.6, 1) +
      guides(col = FALSE) +
      ylab(expression(paste(italic("r"[obs])))) +
      xlab(expression(paste(italic("r"[rand]))))

my_ggsave("./figs/cor_plot3.png", cor_plot3,
          width = 11,
          height = 5.5)

cor_plot4 <- ggplot(r_dat3, aes(x = rand, 
                     y = obs, 
                     label = trait2, 
                     col = ex)) +
      geom_vline(xintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_hline(yintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_abline(aes(slope = 1, intercept = 0), 
                  lty = 1, 
                  lwd = 0.25, 
                  col = "gray20") +
      geom_point() +
      geom_errorbarh(aes(xmin = min, xmax = max), height = 0) + 
      geom_text_repel(size = 6 * 5/14, col = "gray20",
                      xlim = c(0.45, 1),
                      point.padding = unit(0, "lines"),
                      segment.color = "grey50",
                      segment.size = 0,
                      angle = 0,
                      max.iter = 50000, # try more
                      #aes(angle = ifelse(obs < 0, 0, 90)),
                      parse = TRUE
                      ) +
      facet_wrap(~site2, scale = "free") +
      theme_classic() +
      theme(axis.ticks.length = unit(-0.15, "cm"),
            axis.ticks = element_line(size = 0.3),
            axis.text.x = element_text(margin = margin(0.25, unit = "cm"),
                                       size = unit(9, "pt")),
            axis.text.y = element_text(hjust = 0.5,
                                       margin = margin(r = 0.25, unit = "cm"),
                                       debug = FALSE,
                                       angle = 90,
                                       size = unit(9, "pt")),
            panel.spacing = unit(1, "lines"),
            axis.line = element_line(colour = "black", size = 0.5),
            plot.background = element_blank(),
            strip.text.x = element_text(face = "bold"),
            #strip.background = element_blank(),
            strip.background = element_blank(),
            panel.grid = element_blank()) +
      xlim(-0.6, 1) +
      ylim(-0.6, 1) +
      guides(col = FALSE) +
      ylab(expression(paste(italic("r"[obs])))) +
      xlab(expression(paste(italic("r"[rand]))))

my_ggsave("./figs/cor_plot4.png", cor_plot4,
          width = 11,
          height = 5.5)

cor_plot5 <- ggplot(r_dat3, aes(x = rand, 
                     y = obs, 
                     label = trait2, 
                     col = ex)) +
      geom_vline(xintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_hline(yintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_abline(aes(slope = 1, intercept = 0), 
                  lty = 1, 
                  lwd = 0.25, 
                  col = "gray20") +
      geom_point() +
      geom_errorbarh(aes(xmin = min, xmax = max), height = 0) + 
      geom_text_repel(size = 6 * 5/14, col = "gray20",
                      xlim = c(0.45, 1),
                      #aes(y = obs, x = 0.5, label = trait2),
                      nudge_x = -0.5,
                      point.padding = unit(0.1, "lines"),
                      segment.color = "grey50",
                      segment.size = 0.25,
                      angle = 0,
                      max.iter = 50000, # try more
                      parse = TRUE
                      ) +
      facet_wrap(~site2, scale = "free") +
      theme_classic() +
      theme(axis.ticks.length = unit(-0.15, "cm"),
            axis.ticks = element_line(size = 0.3),
            axis.text.x = element_text(margin = margin(0.25, unit = "cm"),
                                       size = unit(9, "pt")),
            axis.text.y = element_text(hjust = 0.5,
                                       margin = margin(r = 0.25, unit = "cm"),
                                       debug = FALSE,
                                       angle = 90,
                                       size = unit(9, "pt")),
            panel.spacing = unit(1, "lines"),
            axis.line = element_line(colour = "black", size = 0.5),
            plot.background = element_blank(),
            strip.text.x = element_text(face = "bold"),
            #strip.background = element_blank(),
            strip.background = element_blank(),
            panel.grid = element_blank()) +
      xlim(-0.6, 1) +
      ylim(-0.6, 1) +
      guides(col = FALSE) +
      ylab(expression(paste(italic("r"[obs])))) +
      xlab(expression(paste(italic("r"[rand]))))

my_ggsave("./figs/cor_plot5.png", cor_plot5,
          width = 11,
          height = 5.5)

cor_plot6 <- ggplot(r_dat3, aes(x = rand, 
                     y = obs, 
                     label = trait2, 
                     col = ex)) +
      geom_vline(xintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_hline(yintercept = 0, lty = 2, lwd = 0.25, col = "gray20") +
      geom_abline(aes(slope = 1, intercept = 0), 
                  lty = 1, 
                  lwd = 0.25, 
                  col = "gray20") +
      geom_point() +
      geom_errorbarh(aes(xmin = min, xmax = max), height = 0) + 
      geom_text_repel(size = 6 * 5/14, col = "gray20",
                      xlim = c(0.45, 1),
                      #aes(y = obs, x = 0.5, label = trait2),
                      nudge_x = 0,
                      nudge_y = 0,
                      point.padding = unit(0, "lines"),
                      segment.color = "grey50",
                      segment.size = 0.25,
                      angle = 0,
                      max.iter = 50000, # try more
                      parse = TRUE
                      ) +
      facet_wrap(~site2, scale = "free") +
      theme_classic() +
      theme(axis.ticks.length = unit(-0.15, "cm"),
            axis.ticks = element_line(size = 0.3),
            axis.text.x = element_text(margin = margin(0.25, unit = "cm"),
                                       size = unit(9, "pt")),
            axis.text.y = element_text(hjust = 0.5,
                                       margin = margin(r = 0.25, unit = "cm"),
                                       debug = FALSE,
                                       angle = 90,
                                       size = unit(9, "pt")),
            panel.spacing = unit(1, "lines"),
            axis.line = element_line(colour = "black", size = 0.5),
            plot.background = element_blank(),
            strip.text.x = element_text(face = "bold"),
            #strip.background = element_blank(),
            strip.background = element_blank(),
            panel.grid = element_blank()) +
      xlim(-0.6, 1) +
      ylim(-0.6, 1) +
      guides(col = FALSE) +
      ylab(expression(paste(italic("r"[obs])))) +
      xlab(expression(paste(italic("r"[rand]))))

my_ggsave("./figs/cor_plot6.png", cor_plot6,
          width = 11,
          height = 5.5)

