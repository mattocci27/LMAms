library(tidyverse)
library(stringr)
library(lazyeval)
library(scales)
library(cowplot)
library(ggrepel)
library(ggthemes)
library(rstan)

settings <- yaml::yaml.load_file("settings.yml")
r_vals <- yaml::yaml.load_file("r_val.yml")
p_letters <- yaml::yaml.load_file("letters.yml")
source("fig_theme.r")

#GL <- read_csv("./data/GL_m0_N_rand.csv")
load("./data/GL_m0_N_rand.rda")

n <- 2

GL_summary <- rand_res$summary[[n]]
dat_r <- rand_res$data[[n]]

GL <- dat %>%
  mutate(LMA = dat_r$LMA,
         LL = dat_r$LL,
         Aarea = dat_r$A,
         Rarea = dat_r$R) %>%
  as_data_frame %>%
  mutate(LMAp = GL_summary[stringr::str_detect(rownames(GL_summary),
                            "log_LMAp"), "X50."] %>%  exp) %>%
  mutate(LMAs = GL_summary[stringr::str_detect(rownames(GL_summary),
                            "log_LMAs"), "X50."] %>%  exp) %>%
  mutate(DE = ifelse(dat$DE == "", "U", as.character(DE))) %>%
  mutate(gr = factor(DE,
    labels = c("Deciduous",
               "Evergreen",
               "Unclassified"
                      ))) %>%
  arrange(sp)

#get_LMA_GL <- function(n) {
#    GL_summary <- rand_res$summary[[n]]
#    dat_r <- rand_res$data[[n]]
#    GL <- dat %>%
#      mutate(LMA = dat_r$LMA,
#             LL = dat_r$LL,
#             Aarea = dat_r$A,
#             Rarea = dat_r$R) %>%
#      as_data_frame %>%
#      mutate(LMAp = GL_summary[str_detect(rownames(GL_summary),
#                                "log_LMAp"), "X50."] %>%  exp) %>%
#      mutate(LMAs = GL_summary[str_detect(rownames(GL_summary),
#                                "log_LMAs"), "X50."] %>%  exp)
#
#    GL
#}
#
#GL <- data_frame(n = 1:10) %>%
#    group_by(n) %>%
#    nest %>%
#    transmute(res = map(1:10, get_LMA_GL)) %>%
#    mutate(n = 1:10) %>%
#    unnest %>%
#    group_by(sp, DE) %>%
#    summarize(LMA = mean(LMA),
#              LMAp_up = quantile(LMAp, 0.975),
#              LMAp_lo = quantile(LMAp, 0.025),
#              LMAp = mean(LMAp),
#              LMAs_up = quantile(LMAs, 0.975),
#              LMAs_lo = quantile(LMAs, 0.025),
#              LMAs = mean(LMAs),
#              LL = mean(LL),
#              Aarea = mean(Aarea),
#              Rarea = mean(Rarea)
#              )  %>%
#    as.data.frame %>%
#    mutate(DE = ifelse(DE == "", "U", as.character(DE))) %>%
#    mutate(gr = factor(DE,
#                       labels = c("Deciduous",
#                                  "Evergreen",
#                                  "Unclassified"
#                                  ))) %>%
#    as_data_frame


GL_dat <- GL %>%
  gather(LMA, Val, c(LMA, LMAs, LMAp)) %>%
  gather(Trait, Val2, c(Aarea, Rarea, LL)) %>%
  mutate(DE = factor(DE,
          levels = c("D", "E", "U"))) %>%
  mutate(LMA = factor(LMA,
    labels = c("LMA", "LMAm", "LMAs"))) %>%
  mutate(LMA = factor(LMA,
    labels = c("LMA~(~g~m^{-2})", "LMAm~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) %>%
  mutate(Trait = factor(Trait,
    levels = c("Aarea", "Rarea", "LL"))) %>%
  mutate(Trait2 = factor(Trait,
    labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "LL~(months)")))

# GL all scatterplot ---------------------------------------------------------

GL_dat1 <- GL_dat %>%
  filter(Trait == "LL" |
         Trait == "Aarea" |
         Trait == "Rarea")


lim_GL <- lim_func(GL_dat1,
                   trait = "Val")
lim_GL2 <- lim_func(GL_dat1,
                    trait = "Val2", LMA = FALSE)

lab1 <- data_frame(lab = paste("(", letters[1:9], ")", sep = ""),
                   Val2 = lim_GL2$max_val %>% rep(each = 3),
                  # Val2 = Inf,
                   Val = lim_GL$min_val %>% rep(3),
                   Val_max = lim_GL$max_val %>% rep(3),
                   gr = "Evergreen",
                   r_vals = r_vals$r_vals$GL %>% unlist,
                   Trait2 = rep(lim_GL2$Trait2, each = 3),
                   LMA = rep(lim_GL$LMA, 3)
                   )

fills <- c("Deciduous" = settings$fills$D,
          "Evergreen" = settings$fills$E,
          "Unclassified" = settings$fills$U)

cols <- c("Deciduous" = settings$colors$D,
          "Evergreen" = settings$colors$E,
          "Unclassified" = settings$colors$U)

scatter_plt <- function(data) {
  ggplot(data, aes(x = Val, y = Val2,
                               fill = gr,
                               col = gr)) +
  geom_point(shape = 21) +
  facet_grid(Trait2 ~ LMA,
             scales = "free",
             switch = "both",
             labeller = labeller(Trait2 = label_parsed,
                                 LMA = label_parsed
                                 )) +
  geom_text(data = lab1, aes(label = lab),
            hjust = 0.25,
            vjust = 0.25,
            size = 8 * 5/14,
            show.legend = FALSE,
            color = "black") +
  #geom_text(data = lab1, aes(label = r_vals, x = Val_max),
  #          hjust = 1,
  #          vjust= 0.25,
  #          size = 8 * 5/14,
  #          parse = TRUE,
  #          color = "black",
  #          show.legend = FALSE) +
  scale_fill_manual(values = fills) +
  scale_colour_manual(values = cols) +
  scale_x_log10(breaks = my_breaks_x(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  xlab("") +
  ylab("") +
  theme_LES()
}

GL_plot <- scatter_plt(GL_dat1)

my_ggsave("./figs/GL_rand.png", GL_plot)
my_ggsave("./figs/GL_rand.pdf", GL_plot)


# PA data ---------------------------------------------------------------------
#PA <- read_csv("./data/PA_m1q_more_N_rand.csv")
#PA <- read_csv("./data/PA_m1q_more_rand.csv")
#load("./data/PA_m1q_more_NS_more_rand.rda")
#load("./data/PA_m1q_N_more_rand.rda")

load("./data/PA_m1q_more_rand.rda")

n <- 5
PA_summary <- rand_res$summary[[n]]
dat_r <- rand_res$data[[n]]

DT::datatable(PA_summary)
traceplot(rand_res$model[[n]])

PA <- dat %>%
  mutate(LMA = dat_r$LMA,
         LL = dat_r$LL,
         Aarea = dat_r$A,
         Rarea = dat_r$R) %>%
  as_data_frame %>%
  mutate(LMAp = PA_summary[str_detect(rownames(PA_summary),
                            "log_LMAp"), "X50."] %>%  exp) %>%
  mutate(LMAs = PA_summary[str_detect(rownames(PA_summary),
                            "log_LMAs"), "X50."] %>%  exp) %>%
  mutate(preLL = PA_summary[str_detect(rownames(PA_summary),
                            "mu2"), "X50."] %>%  exp) %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
  mutate(site_strata = paste(site2, strata, sep = "_"))

#get_LMA <- function(n) {
#    PA_summary <- rand_res$summary[[n]]
#    dat_r <- rand_res$data[[n]]
#    PA <- dat %>%
#      mutate(LMA = dat_r$LMA,
#             LL = dat_r$LL,
#             Aarea = dat_r$A,
#             Rarea = dat_r$R) %>%
#      as_data_frame %>%
#      mutate(LMAp = PA_summary[str_detect(rownames(PA_summary),
#                                "log_LMAp"), "X50."] %>%  exp) %>%
#      mutate(LMAs = PA_summary[str_detect(rownames(PA_summary),
#                                "log_LMAs"), "X50."] %>%  exp) %>%
#      mutate(preLL = PA_summary[str_detect(rownames(PA_summary),
#                                "mu2"), "X50."] %>%  exp) %>%
#      mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
#      mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
#      mutate(site_strata = paste(site2, strata, sep = "_"))
#
#    PA
#}
#
#PA <- data_frame(n = 1:9) %>%
#    group_by(n) %>%
#    nest %>%
#    transmute(res = map(1:9, get_LMA)) %>%
#    mutate(n = 1:9) %>%
#    unnest %>%
#    group_by(sp_site_strata, sp, site2, site, strata) %>%
#    summarize(LMA = mean(LMA),
#              LMAp_up = quantile(LMAp, 0.975),
#              LMAp_lo = quantile(LMAp, 0.025),
#              LMAp = mean(LMAp),
#              LMAs_up = quantile(LMAs, 0.975),
#              LMAs_lo = quantile(LMAs, 0.025),
#              LMAs = mean(LMAs),
#              preLL_lo = quantile(preLL, 0.975),
#              preLL_up = quantile(preLL, 0.025),
#              preLL = mean(preLL),
#              LL = mean(LL),
#              Aarea = mean(Aarea),
#              Rarea = mean(Rarea)
#             ) %>%
#    mutate(site_strata = paste(site2, strata, sep = "_"))


PA_dat <- PA %>%
  gather(LMA, Val,
         c(LMA, LMAs, LMAp)) %>%
  gather(Trait, Val2, c(Aarea, Rarea, LL)) %>%
  as.data.frame %>%
  mutate(LMA = factor(LMA,
    labels = c("LMA", "LMAm", "LMAs"))) %>%
  mutate(LMA = factor(LMA,
    labels = c("LMA~(~g~m^{-2})", "LMAm~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) %>%
  mutate(site_strata = factor(site_strata,
          levels = c("WET_CAN", "DRY_CAN", "WET_UNDER", "DRY_UNDER"))) %>%
  mutate(Trait = factor(Trait,
    levels = c("Aarea", "Rarea", "LL"))) %>%
  mutate(Trait2 = factor(Trait,
    labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "LL~(months)"
               ))) %>%
  mutate(gr = factor(site_strata,
    labels = c("Sun-Wet",
               "Sun-Dry",
               "Shade-Wet",
               "Shade-Dry"
                      ))) %>%
  #arrange(desc(gr))
  arrange(gr) %>% as_data_frame


# PA LES plot ----------------------------------------------------------------0
PA_dat1 <- PA_dat %>%
  filter(Trait == "LL" |
         Trait == "Aarea" |
         Trait == "Rarea")

lim_PA <- lim_func(PA_dat1 %>% filter(site_strata != "Rand"),
                   trait = "Val")
lim_PA2 <- lim_func(PA_dat1 %>% filter(site_strata != "Rand"),
                    trait = "Val2", LMA = FALSE)

lab1 <- data_frame(lab = paste("(", letters[1:9], ")", sep = ""),
                   Val2 = lim_PA2$max_val %>% rep(each = 3),
                   Val = lim_PA$min_val %>% rep(3),
                   Val_max = lim_PA$max_val %>% rep(3),
                   gr = "Sun-Dry",
                   r_vals = r_vals$r_vals$PA %>% unlist,
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

PA_plot <- scatter_plt(PA_dat1)

my_ggsave("./figs/PA_rand.png", PA_plot)


# LL plot ---------------------------------------------------------------------

LL_dat <- PA %>%
  as.data.frame %>%
  mutate(site_strata = factor(site_strata,
          levels = c("WET_CAN", "DRY_CAN", "WET_UNDER", "DRY_UNDER"))) %>%
  mutate(gr = factor(site_strata,
    labels = c("Sun-Wet",
               "Sun-Dry",
               "Shade-Wet",
               "Shade-Dry"
                      )))

labLL <- data_frame(
                   LL = Inf,
                   Val = min(PA$LL),
                   Val_max = max(PA$LL),
                   gr = "Sun-Dry",
                   r_vals = r_vals$r_vals$PA_LL$preLL_LL
                   )

LL_plot <- ggplot(LL_dat, aes(x = preLL, y = LL,
                              fill = gr, col = gr)) +
  geom_point(shape = 21) +
  scale_fill_manual(values = fills) +
  scale_colour_manual(values = cols) +
  scale_x_log10(breaks = my_breaks(), expand = c(0.1, 0),
                limits = c(min(labLL$Val), max(labLL$Val_max))) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0),
                limits = c(min(labLL$Val), max(labLL$Val_max))) +
  xlab("Predicted LL (months)") +
  ylab("Observed LL (months)") +
  geom_abline(aes(slope = 1, intercept = 0),
              lty = 2,
              lwd = 0.25,
              col = "gray20") +
 # geom_text(data = labLL, aes(label = r_vals, x = Val_max, y = 2.5),
 #           colour = "black",
 #           hjust = 1,
 #           vjust= 0,
 #           parse = TRUE,
 #           show.legend = FALSE) +
  coord_fixed() +
  theme_LES() +
  theme(
    legend.position = c(0.2, 0.85),
    axis.title.x = element_text(margin = margin(t = 1,
                                                b = 1,
                                                l = 0,
                                                r = 0)),
    axis.title.y = element_text(margin = margin(t = 1,
                                                b = 1,
                                                l = 1,
                                                r = 1))
        )

my_ggsave("./figs/LL_rand.png", LL_plot,
          width = 6.7,
          height = 6.7)

## LMAm vs LMAs
