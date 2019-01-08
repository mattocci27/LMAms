library(tidyverse)
library(stringr)
library(lazyeval)
library(scales)
library(cowplot)
library(ggrepel)
library(ggthemes)

source("./sim/sim_data.r")
source("fig_theme.r")
#load("./data/GL_model1r4_obs_2017-06-19_.RData")

settings <- yaml::yaml.load_file("settings.yml")

GL_dat <- GL_sim_dat %>% 
  gather(LMA, Val, c(LMA, LMAs, LMAp)) %>%
  gather(Trait, Val2, c(Aarea, Rarea, LL)) %>%
  mutate(LMA = factor(LMA,
    labels = c("LMA~(~g~m^{-2})", "LMAm~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) %>%
  mutate(Trait = factor(Trait,
    levels = c("Aarea", "Rarea", "LL"))) %>%
  mutate(Trait2 = factor(Trait,
    labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "LL~(months)"))) %>%
  mutate(gr = factor(gr,
    labels = c("Deciduous",
               "Evergreen",
               "Unclassified")
                      )) %>%
  arrange(desc(gr))

lim_GL <- lim_func(GL_dat, 
                   trait = "Val")
lim_GL2 <- lim_func(GL_dat,
                    trait = "Val2", LMA = FALSE)

lab1 <- data_frame(lab = paste("(", letters[1:9], ")", sep = ""),
                   Val2 = lim_GL2$max_val %>% rep(each = 3),
                   Val = lim_GL$min_val %>% rep(3),
                   Val_max = lim_GL$max_val %>% rep(3),
                   gr = "Deciduous",
                   Trait2 = rep(lim_GL2$Trait2, each = 3),
                   LMA = rep(lim_GL$LMA, 3)
                   )


fills <- c("Deciduous" = settings$fills$D,
          "Evergreen" = settings$fills$E,
          "Unclassified" = settings$fills$U)

cols <- c("Deciduous" = settings$colors$D,
          "Evergreen" = settings$colors$E,
          "Unclassified" = settings$colors$U)

scatter_sim <- function(data) {
  ggplot(data, aes(x = Val, y = Val2, 
                               fill = gr, 
                               col = gr)) +
  geom_point(shape = 21) +
  facet_grid(Trait2 ~ LMA,
             scales = "free",
             switch = "both", 
             labeller = labeller(Trait2 = label_parsed,
                                 LMA = label_parsed)) +
  geom_text(data = lab1, aes(label = lab), 
            hjust = 0.25,
            vjust = 0.25,
            size = 8 * 5/14,
            show.legend = FALSE,
            color = "black") +
  scale_fill_manual(values = fills) +
  scale_colour_manual(values = cols) +
 # scale_x_log10(breaks = my_breaks_x(), expand = c(0.1, 0)) +
 # scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  #xlab("") +
  #ylab("") +
  theme_LES()
}

GL_plot <- scatter_sim(GL_dat)

my_ggsave("./figs/GL_sim.png", GL_plot)
my_ggsave("./figs/GL_sim.pdf", GL_plot)



# PA --------------------------------------------------------------------------

PA_dat <- PA_sim_dat2 %>% 
  mutate(site_strata = paste(site, strata)) %>%
  gather(LMA, Val, 
         c(LMA, LMAs, LMAp)) %>%
  gather(Trait, Val2, c(Aarea, Rarea, LL)) %>%
  mutate(LMA = factor(LMA,
    labels = c("LMA~(~g~m^{-2})", "LMAm~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) %>%
  mutate(Trait = factor(Trait,
    levels = c("Aarea", "Rarea", "LL"))) %>%
  mutate(Trait2 = factor(Trait,
    labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "LL~(months)"
               ))) %>% 
  mutate(gr = factor(site_strata,
    labels = c("Shade-Dry",
               "Sun-Dry",
               "Shade-Wet",
               "Sun-Wet"
                      ))) %>%
  #arrange(desc(gr))
  arrange(gr) 

lim_PA <- lim_func(PA_dat, 
                   trait = "Val")
lim_PA2 <- lim_func(PA_dat,
                    trait = "Val2", LMA = FALSE)

lab1 <- data_frame(lab = paste("(", letters[1:9], ")", sep = ""),
                   Val2 = lim_PA2$max_val %>% rep(each = 3),
                   Val = lim_PA$min_val %>% rep(3),
                   Val_max = lim_PA$max_val %>% rep(3),
                   gr = "Sun-Wet",
                   Trait2 = rep(lim_PA2$Trait2, each = 3),
                   LMA = rep(lim_PA$LMA, 3)
                   )

fills <- c("Sun-Dry" = settings$fills$sun_dry,
          "Sun-Wet" = settings$fills$sun_wet,
          "Shade-Dry" = settings$fills$shade_dry,
          "Shade-Wet" = settings$fills$shade_wet)

cols <- c("Sun-Dry" = settings$colors$sun_dry,
          "Sun-Wet" = settings$colors$sun_wet,
          "Shade-Dry" = settings$colors$shade_dry,
          "Shade-Wet" = settings$colors$shade_wet)

PA_plot <- scatter_sim(PA_dat)

my_ggsave("./figs/PA_sim.png", PA_plot)


# WC --------------------------------------------------------------------------

WC_dat <- WC_sim_dat %>% 
  gather(LMA, Val, 
         c(LMA, LMAs, LMAp)) %>%
  gather(Trait, Val2, c(Aarea, Rarea, LL)) %>%
  mutate(Trait = factor(Trait,
    levels = c("Aarea", "Rarea", "LL"))) %>%
  mutate(Trait2 = factor(Trait,
    labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "LL~(months)"
               ))) 

lim_WC <- lim_func(WC_dat, 
                   trait = "Val")
lim_WC2 <- lim_func(WC_dat,
                    trait = "Val2", LMA = FALSE)

lab1 <- data_frame(lab = paste("(", letters[1:9], ")", sep = ""),
                   Val2 = lim_WC2$max_val %>% rep(each = 3),
                   Val = lim_WC$min_val %>% rep(3),
                   Val_max = lim_WC$max_val %>% rep(3),
                   Trait2 = rep(lim_WC2$Trait2, each = 3),
                   LMA = rep(lim_WC$LMA, 3)
                   )

scatter_sim_wc <- function(data) {
  ggplot(data, aes(x = Val, y = Val2)) +
  geom_point(shape = 21, fill = "gray20") +
  facet_grid(Trait2 ~ LMA,
             scales = "free",
             switch = "both", 
             labeller = labeller(Trait2 = label_parsed)) +
  geom_text(data = lab1, aes(label = lab), 
            hjust = 0.25,
            vjust = 0.25,
            size = 8 * 5/14,
            show.legend = FALSE,
            color = "black") +
  scale_fill_manual(values = fills) +
  scale_colour_manual(values = cols) +
  scale_x_log10(breaks = my_breaks_x(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  xlab("") +
  ylab("") +
  theme_LES()
}

WC_plot <- scatter_sim_wc(WC_dat)

my_ggsave("./figs/WC_sim.png", WC_plot)


