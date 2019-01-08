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

# GL data ---------------------------------------------------------------------

#GL <- read_csv("./data/GL_m0_N.csv")
GL <- read_csv("./data/GL_m0.csv")

GL_dat <- GL %>% 
  gather(LMA, Val, c(LMA, LMAs, LMAp)) %>%
  gather(Trait, Val2, c(Aarea, Rarea, LL, 
                        Parea, Narea)) %>%
  mutate(DE = factor(DE,
          levels = c("D", "E", "U"))) %>%
  mutate(LMA = factor(LMA,
    labels = c("LMA", "LMAm", "LMAs"))) %>%
  mutate(LMA = factor(LMA,
    labels = c("LMA~(~g~m^{-2})", "LMAm~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) %>%
  mutate(Trait = factor(Trait,
    levels = c("Aarea", "Rarea", "LL", "Narea", "Parea"))) %>%
  mutate(Trait2 = factor(Trait,
    labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "LL~(months)",
               "italic(N)[area]~(~g~m^{-2})",
               "italic(P)[area]~(~g~m^{-2})"))) 

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
                                 LMA = label_parsed)) +
  geom_text(data = lab1, aes(label = lab), 
            hjust = 0.25,
            vjust = 0.25,
            size = 8 * 5/14,
            show.legend = FALSE,
            color = "black") +
  geom_text(data = lab1, aes(label = r_vals, x = Val_max), 
            hjust = 1,
            vjust= 0.25,
            size = 8 * 5/14,
            parse = TRUE,
            color = "black",
            show.legend = FALSE) +
  scale_fill_manual(values = fills) +
  scale_colour_manual(values = cols) +
  scale_x_log10(breaks = my_breaks_x(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  xlab("") +
  ylab("") +
  theme_LES()
}

GL_plot <- scatter_plt(GL_dat1)

my_ggsave("./figs/GL.png", GL_plot)
my_ggsave("./figs/GL.pdf", GL_plot)

# GL NP scatterplot -----------------------------------------------------------

GL_dat2 <- GL_dat %>%
  filter(Trait == "Narea" |
         Trait == "Parea")

lim_GL <- lim_func(GL_dat2 %>% filter(DE != "Rand"), 
                   trait = "Val")
lim_GL2 <- lim_func(GL_dat2 %>% filter(DE != "Rand"),
                    trait = "Val2", LMA = FALSE)

lab1 <- data_frame(lab = paste("(", letters[1:6], ")", sep = ""),
                   Val2 = lim_GL2$max_val %>% rep(each = 3),
                   Val = lim_GL$min_val %>% rep(2),
                   Val_max = lim_GL$max_val %>% rep(2),
                   gr = "Evergreen",
                   r_vals = r_vals$r_vals$GL_NP %>% unlist,
                   Trait2 = rep(lim_GL2$Trait2, each = 3),
                   LMA = rep(lim_GL$LMA, 2)
                   )

GL_NP_plot <- scatter_plt(GL_dat2)

my_ggsave("./figs/GL_NP.png", GL_NP_plot, height = 8.1)
my_ggsave("./figs/GL_NP.pdf", GL_NP_plot, height = 8.1)


# PA data ---------------------------------------------------------------------
#PA <- read_csv("./data/PA_m1q_more_N.csv")
PA <- read_csv("./data/PA_m1q_more_NS.csv")

PA_dat <- PA %>% 
  gather(LMA, Val, 
         c(LMA, LMAs, LMAp)) %>%
  gather(Trait, Val2, c(Aarea, Rarea, LL, 
                        Parea, Narea, cell_area)) %>%
  mutate(LMA = factor(LMA,
    labels = c("LMA", "LMAm", "LMAs"))) %>%
  mutate(LMA = factor(LMA,
    labels = c("LMA~(~g~m^{-2})", "LMAm~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) %>%
  mutate(site_strata = factor(site_strata,
          levels = c("WET_CAN", "DRY_CAN", "WET_UNDER", "DRY_UNDER"))) %>%
  mutate(Trait = factor(Trait,
    levels = c("Aarea", "Rarea", "LL", "Narea", "Parea", "cell_area"))) %>%
  mutate(Trait2 = factor(Trait,
    labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
               "LL~(months)",
               "italic(N)[area]~(~g~m^{-2})",
               "italic(P)[area]~(~g~m^{-2})",
               "italic(CL)[area]~(~g~m^{-2})"
               ))) %>% 
  mutate(gr = factor(site_strata,
    labels = c("Sun-Wet",
               "Sun-Dry",
               "Shade-Wet",
               "Shade-Dry"
                      ))) %>%
  #arrange(desc(gr))
  arrange(gr) 


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

my_ggsave("./figs/PA.png", PA_plot)

# PA NPC plot ----------------------------------------------------------------
PA_dat2 <- PA_dat %>%
  filter(Trait == "Narea" |
         Trait == "Parea" |
         Trait == "cell_area")


lim_PA <- lim_func(PA_dat2 %>% filter(site_strata != "Rand"), 
                   trait = "Val")
lim_PA2 <- lim_func(PA_dat2 %>% filter(site_strata != "Rand"),
                    trait = "Val2", LMA = FALSE)

lab1 <- data_frame(lab = paste("(", letters[1:9], ")", sep = ""),
                   Val2 = lim_PA2$max_val %>% rep(each = 3),
                   Val = lim_PA$min_val %>% rep(3),
                   Val_max = lim_PA$max_val %>% rep(3),
                   gr = "Sun-Dry",
                   r_vals = r_vals$r_vals$PA_NP %>% unlist,
                   Trait2 = rep(lim_PA2$Trait2, each = 3),
                   LMA = rep(lim_PA$LMA, 3)
                   )

PA_NPC_plot <- scatter_plt(PA_dat2)

my_ggsave("./figs/PA_NPC.png", PA_NPC_plot)

# LL plot ---------------------------------------------------------------------

LL_dat <- PA %>%
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
  geom_text(data = labLL, aes(label = r_vals, x = Val_max, y = 2.5), 
            colour = "black",
            hjust = 1,
            vjust= 0,
            parse = TRUE,
            show.legend = FALSE) +
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

my_ggsave("./figs/LL_plot.png", LL_plot,
          width = 6.7,
          height = 6.7)


# cov plot --------------------------------------------------------------------- 

var_p <- function(data)cov(data$LMA, data$LMAp)
var_s <- function(data)cov(data$LMA, data$LMAs)
var_t <- function(data)var(data$LMA)

var_p <- function(data)var(data$LMAp)
var_s <- function(data)var(data$LMAs)
cov_ps <- function(data)cov(data$LMAp, data$LMAs)
var_t <- function(data)var(data$LMA)


PA_cov <- PA %>% 
  count(sp) %>% 
#  filter(n >= 2) %>%
  inner_join(., PA, by = "sp") %>%
  mutate(gr = strata)


GL_cov <- GL %>%
  #filter(DE != "U") %>%
  mutate(gr = "GL")

var_dat <- bind_rows(PA_cov, GL_cov) %>%
  group_by(gr) %>%
  nest %>%
  mutate(var_LMAm = map_dbl(data, var_p)) %>%
  mutate(var_LMAs = map_dbl(data, var_s)) %>%
  mutate(cov_ms = 2 * map_dbl(data, cov_ps)) %>%
  mutate(var_total = map_dbl(data, var_t)) %>%
  dplyr::select(-data) %>%
  mutate(gr = ifelse(gr == "CAN", "PA Sun", gr))  %>%
  mutate(gr = ifelse(gr == "UNDER", "PA Shade", gr))  %>%
  mutate(gr = factor(gr,
    levels = c("GL", "PA Sun", "PA Shade")))

#var_dat_s <- var_dat %>%
#  mutate(LMAm = var_LMAm / (var_LMAm + var_LMAs) * 100) %>%
#  mutate(LMAs = var_LMAs / (var_LMAm + var_LMAs) * 100) %>%
#  mutate(var_ratio = LMAm/LMAs)# %>%

# just ratio
var_dat_s <- var_dat %>%
  mutate(var_ratio = var_LMAm / var_LMAs)# %>%


  #gather(var, val, c(LMAm, LMAs, var_ratio))

# R values
var_dat_s2 <- var_dat_s %>%
  dplyr::select(gr, var_ratio)

write.csv(var_dat_s2, "./data/var_val.csv", row.names = F)



#my_gray <- gray.colors(2, start = 0.5, end = 0.8, gamma = 2.2, alpha = NULL)
#
#cov_plot <- ggplot(var_dat_s, aes(x = gr, y = val, fill = var)) +
#  geom_bar(position = "fill",stat = "identity") +
#  scale_fill_manual(values = rev(my_gray)) +
#  scale_y_continuous(labels = percent_format()) +
#  theme_LES()  +
#  theme(
#    #panel.border = element_rect(fill = NA, colour = "white")
#    axis.line = element_line(size = 0.25, colour = "black"),
#    #axis.line=element_blank(),
#    panel.border = element_blank(),
#    axis.title.y = element_text(margin = margin(t = 0,
#                                                b = 0,
#                                                l = 0,
#                                                r = 5),
#                                angle = 90),
#    axis.title.x = element_text(margin = margin(t = 0,
#                                                b = 0,
#                                                l = 0,
#                                                r = 0))
#        ) +
#  ylab("% interspecific LMA variation") +
#  xlab("")
#
#my_ggsave("./figs/cov_plot.png", cov_plot,
#          width = 8.1,
#          height = 8.1)
#
# boxplot ---------------------------------------------------------------------
fills <- c("D" = settings$fills$D,
          "E" = settings$fills$E)

cols <- c("D" = settings$colors$D,
          "E" = settings$colors$E)

GL_dat_box <- GL_dat %>%
  filter(gr != "Unclassified") %>%
  dplyr::select(DE, Val, LMA) %>%
  mutate(gr = factor(DE,
                     levels = c("D", "E"))) %>%
  unique

theme_box <- function(base_size = 9,
                      base_family = "sans") {
  ret <- theme_LES() %+replace%
  theme(
    panel.border = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    #plot.margin = margin(1,10,1,1),
    axis.line = element_line(colour = "black", size = 0.25),
    axis.title.y = element_text(margin = margin(t = 0,
                                                b = 0,
                                                l = -4,
                                                r = -4),
                                angle = 90)
        )
}

lab1 <- GL_dat_box %>%
  group_by(gr, LMA) %>%
  summarize(Val = max(Val)) %>%
  ungroup %>%
  arrange(LMA) %>%
  mutate(lab = p_letters$GL 
         %>% unlist)

GL_box <- ggplot(GL_dat_box, aes(x = gr, y = Val, fill = gr)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1) +
  geom_text(data= lab1, aes(label = lab),
            vjust = -1,
            size = 8 * 5/14) +
  facet_grid(.~ LMA) +
  ylab(expression(atop("GLOPNET",
                LMA~(g~m^{-2})))) +
  xlab("") +
  scale_fill_manual(values = fills, guide = FALSE) +
  #guides(fill = FALSE, colour = FALSE) +
  #scale_x_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  theme_box()

fills2 <- c("Sun\nDry" = settings$fills$sun_dry,
          "Sun\nWet" = settings$fills$sun_wet,
          "Shade\nDry" = settings$fills$shade_dry,
          "Shade\nWet" = settings$fills$shade_wet,
          "Rand" = settings$fills$R)

cols2 <- c("Sun\nDry" = settings$colors$sun_dry,
          "Sun\nWet" = settings$colors$sun_wet,
          "Shade\nDry" = settings$colors$shade_dry,
          "Shade\nWet" = settings$colors$shade_wet,
          "Rand" = settings$colors$R)

PA_trim_dat <- PA_dat %>% 
  count(sp) %>% 
  filter(n >= 2) %>%
  inner_join(., PA_dat, by = "sp") %>%
  dplyr::select(gr, Val, LMA) %>%
  mutate(gr = factor(gr,
    levels = c("Sun-Dry", "Shade-Dry", "Sun-Wet", "Shade-Wet"))) %>%
  mutate(gr = factor(gr,
    labels = c("Sun\nDry", "Shade\nDry", "Sun\nWet", "Shade\nWet"))) %>%
  unique

lab2 <- PA_trim_dat %>%
  group_by(gr, LMA) %>%
  summarize(Val = max(Val)) %>%
  ungroup %>%
  arrange(LMA) %>%
  mutate(lab = p_letters$PA 
         %>% unlist)

PA_box <- ggplot(PA_trim_dat, aes(x = gr, y = Val, fill = gr, col = gr)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1) +
  geom_text(data= lab2, aes(label = lab),
            colour = "black",
            vjust = -1,
            size = 8 * 5/14) +
  facet_grid(.~ LMA) +
  #ylab(expression(Panama\nLMA~(g~m^{-2}~))) +
 # ylab(as.list(expression("Panama", paste("LMA (g ",m^{-2},")")))) +
  ylab(expression(atop("Panama",
                LMA~(g~m^{-2})))) +
  xlab("") +
 # labs(title = "moge") +
  scale_fill_manual(values = fills2, guide = FALSE) +
  scale_colour_manual(values = cols2, guide = FALSE) +
  #scale_x_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  #scale_y_log10(#breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  theme_box() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(colour = NA) # invinsible strip
        ) # +
 # annotation_logticks(size = 0.25,
 #   short = unit(0.1, "cm"), 
 #   mid = unit(0.0, "cm"), 
 #   long = unit(0.2, "cm")
 #                     )  
#boxplot(LMAs ~ site_strata, PA, log = "y", ylim = c(10,120))

box_main <- plot_grid(GL_box, PA_box,
          nrow = 2,
          #labels = c("(a)", "(b)"),
          label_size = 9)

my_ggsave("./figs/box_main.png", box_main,
          width = 11.7,
          height = 11.7)


# PA all =================================
PA_nontrim_dat <- PA_dat %>% 
  dplyr::select(gr, Val, LMA) %>%
  mutate(gr = factor(gr,
    levels = c("Sun-Dry", "Shade-Dry", "Sun-Wet", "Shade-Wet"))) %>%
  mutate(gr = factor(gr,
    labels = c("Sun\nDry", "Shade\nDry", "Sun\nWet", "Shade\nWet"))) %>%
  unique

lab2 <- PA_nontrim_dat %>%
  group_by(gr, LMA) %>%
  summarize(Val = max(Val)) %>%
  ungroup %>%
  arrange(LMA) %>%
  mutate(lab = p_letters$PA 
         %>% unlist)

PA_box2 <- ggplot(PA_nontrim_dat, aes(x = gr, y = Val, fill = gr, col = gr)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1) +
  geom_text(data= lab2, aes(label = lab),
            colour = "black",
            vjust = -1,
            size = 8 * 5/14) +
  facet_grid(.~ LMA) +
  ylab(expression(atop("Panama",
                LMA~(g~m^{-2})))) +
  xlab("") +
  scale_fill_manual(values = fills2, guide = FALSE) +
  scale_colour_manual(values = cols2, guide = FALSE) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  theme_box() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(colour = NA) # invinsible strip
        ) # +

my_ggsave("./figs/box_SI.png", PA_box2,
          width = 11.7,
          height = 6)

## LMAm vs LMAs =====================================

fills1 <- c("Deciduous" = settings$fills$D,
          "Evergreen" = settings$fills$E,
          "Unclassified" = settings$fills$U)

cols1 <- c("Deciduous" = settings$colors$D,
          "Evergreen" = settings$colors$E,
          "Unclassified" = settings$colors$U)

fills2 <- c("Sun-Dry" = settings$fills$sun_dry,
          "Sun-Wet" = settings$fills$sun_wet,
          "Shade-Dry" = settings$fills$shade_dry,
          "Shade-Wet" = settings$fills$shade_wet)

cols2 <- c("Sun-Dry" = settings$colors$sun_dry,
          "Sun-Wet" = settings$colors$sun_wet,
          "Shade-Dry" = settings$colors$shade_dry,
          "Shade-Wet" = settings$colors$shade_wet)

GL_LMAms <- ggplot(GL, aes(x = LMAp, y = LMAs, 
                               fill = gr, 
                               col = gr)) +
  geom_point(shape = 21) +
  scale_fill_manual(values = fills1, guide = FALSE) +
  scale_colour_manual(values = cols1, guide = FALSE) +
  scale_x_log10(breaks = my_breaks_x(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  xlab(expression(atop(
                LMAm~(g~m^{-2})))) +
  ylab(expression(atop(
                LMAs~(g~m^{-2})))) +
 labs(title = "GLOPNET \n",
       subtitle = bquote(italic("r")~"="~.(r_vals$r_vals$GL_LMAms$LMAs_LMAm)
                         ~", "~italic("p")~"="~.(r_vals$r_vals$GL_LMAms$p_val))) +
  theme_LES() +
  theme(
          plot.title = element_text(margin=margin(b = -2, unit = "pt")),
          axis.title.y = element_text(margin = margin(t = 0,
                                                      b = 0,
                                                      l = 5,
                                                      r = -15),
                                      angle = 90),
          axis.title.x = element_text(margin = margin(t = 0,
                                                      b = -10,
                                                      l = -5,
                                                      r = -5)))


PA_tmp <- PA %>%
  mutate(gr = factor(site_strata,
    labels = c("Sun-Wet",
               "Sun-Dry",
               "Shade-Wet",
               "Shade-Dry"
                      ))) 


PA_LMAms <- ggplot(PA_tmp,
                   aes(x = LMAp, y = LMAs, 
                               fill = gr, 
                               col = gr)) +
  geom_point(shape = 21) +
  scale_fill_manual(values = fills2, guide = FALSE) +
  scale_colour_manual(values = cols2, guide = FALSE) +
  scale_x_log10(breaks = my_breaks_x(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  #labs(title = "Panama \n",
  #     subtitle = expression(italic("r")~"= 0.06,"~
  #                           italic("p")~"= 0.6",
  #                           )) +
 labs(title = "Panama \n",
       subtitle = bquote(italic("r")~"="~.(r_vals$r_vals$PA_LMAms$LMAs_LMAm)~", "~italic("p")~"="~.(r_vals$r_vals$PA_LMAms$p_val))) +
  xlab(expression(atop(
                LMAm~(g~m^{-2})))) +
  ylab(expression(atop(
                LMAs~(g~m^{-2})))) +
  theme_LES() +
  theme(
        plot.title = element_text(margin=margin(b = -2, unit = "pt")),
        axis.title.y = element_text(margin = margin(t = 0,
                                                    b = 0,
                                                    l = 5,
                                                    r = -15),
                                    angle = 90),
        axis.title.x = element_text(margin = margin(t = 0,
                                                    b = -10,
                                                    l = -5,
                                                    r = -5)))



PA_LMAms

LMAms_plt <- plot_grid(GL_LMAms, PA_LMAms,
          labels = c("A", "B"))

my_ggsave("./figs/LMAms.png", LMAms_plt,
          width = 11.7,
          height = 7)

