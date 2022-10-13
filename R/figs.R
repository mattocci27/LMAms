
#' @title ggsave for targets
my_ggsave <- function(filename, plot, units = c("in", "cm",
        "mm", "px"), height = NA, width = NA, dpi = 300, ...) {
  ggsave(
    filename = paste0(filename, ".png"),
    plot = plot,
    height = height,
    width = width,
    units = units,
    dpi = dpi,
    ...
  )
  ggsave(
    filename = paste0(filename, ".pdf"),
    plot = plot,
    height = height,
    width = width,
    units = units,
    dpi = dpi,
    ...
  )
  str_c(filename, c(".png", ".pdf"))
}

#' @title Hypothetical relationships
hypo_point <- function(para_yml, n = 200, seed = 123) {
 targets::tar_load(para_yml)
  para <- yaml::yaml.load_file(para_yml)
  a0 <- para$GL$a0
  am <- para$GL$am
  as <- para$GL$as
  sig1 <- para$GL$sig1

  set.seed(seed)
  N <- n
  LMAm <- rlnorm(N, log(80), 0.8)
  LMAs <- rlnorm(N, log(80), 0.7)
  LMA <- LMAm + LMAs
  log_Aarea <- rnorm(N, log(a0 * LMAm^am * LMAs^as) - 0.5 * sig1^2, sig1)
  Aarea <- exp(log_Aarea)
#Aarea <- rlnorm(N, log(a0 * LMAm^am * LMAs^as), sig1)
  tmp <- tibble(LMA, LMAm, LMAs, Aarea)

  cor.test(log(Aarea), log(LMA))
  cor.test(log(Aarea/LMA), log(LMA))

  p1 <- ggplot(tmp, aes(LMAm, LMAs, color = LMA)) +
    geom_point(alpha = 0.9) +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_viridis_c(trans = "log10",
                          breaks = c(50, 100, 200 ,500)
                          ) +
    #scale_color_gradient2(midpoint = median(LMA),
    #                     low = "#e66101",
    #                     high = "#5e3c99") +
    xlab(expression(LMAm~(g~m^{-2}))) +
    ylab(expression(LMAs~(g~m^{-2}))) +
    theme_LES() +
    theme(legend.position = "none")

  p2 <- ggplot(tmp, aes(LMA, Aarea, color = LMA)) +
    geom_point(alpha = 0.9) +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_viridis_c(trans = "log10") +
    xlab(expression(Total~LMA~(g~m^{-2}))) +
    ylab(expression(italic(A)[area]~(~mu~mol~m^{-2}~s^{-1}))) +
    theme_LES() +
    theme(legend.position = "none")

  p3 <- ggplot(tmp, aes(LMA, y = Aarea / LMA, color = LMA)) +
    geom_point(alpha = 0.9) +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_viridis_c(trans = "log10",
                          breaks = c(50, 100, 300, 500),
                          name = "Total LMA"
                          ) +
    xlab(expression(Total~LMA~(g~m^{-2}))) +
    ylab(expression(italic(A)[mass]~(~mu~mol~g^{-1}~s^{-1}))) +
    theme_LES() +
    theme(legend.position = "right",
          legend.key.size = unit(0.3, "cm"),
          legend.spacing.y = unit(0.1, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8))


  p4 <- p1 + p2  + p3 +
    plot_annotation(tag_levels = "a") &
    theme(plot.tag = element_text(face = "bold"),
          legend.background =  element_blank())
  p4
}

#' @title Breaks for tratis (scatter plots)
my_breaks <- function(...){
  c(0.002, 0.005, 0.02, 0.05, 0.1, 0.2,  0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500)
}

#' @title Breaks for LMS (scatter plots)
my_breaks_x <- function(...){
  c(1, 2, 5, 10, 20, 50, 100, 500)
}

#' @title Creates axis lim from long-data (scatter plots)
lim_func <- \(data, LMA = TRUE) {
  if (LMA) {
    tmp <- data |>
      dplyr::select(LMA, val) |>
      group_by(LMA) |>
      summarize(
          min_val = log10(min(val, na.rm = TRUE)),
          max_val = log10(max(val, na.rm = TRUE)))
  } else {
    tmp <- data |>
      dplyr::select(trait2, val2) |>
      group_by(trait2) |>
      summarize(
          min_val = log10(min(val2, na.rm = TRUE)),
          max_val = log10(max(val2, na.rm = TRUE)))
  }
  tmp |>
    ungroup() |>
    mutate(mid_val = (max_val - min_val) / 2) |>
    mutate(min_val = 10^(min_val - abs(mid_val) * 0.15)) |>
   # mutate(max_val = 10^(max_val)
    mutate(max_val = 10^(max_val + abs(mid_val) * 0.15))
}

#' @title Base scatter plots for GL and Panama
scatter_plt <- function(data, lab1, settings_yml, GL = TRUE) {
#scatter_plt <- function(data, lab1, fills, cols) {
  settings <- yaml::yaml.load_file(settings_yml)
  if (GL) {
    fills <- c("Deciduous" = settings$fills$D,
              "Evergreen" = settings$fills$E,
              "Unclassified" = settings$fills$U)

    cols <- c("Deciduous" = settings$colors$D,
              "Evergreen" = settings$colors$E,
              "Unclassified" = settings$colors$U)
  } else {
    fills <- c("Sun-Dry" = settings$fills$sun_dry,
              "Sun-Wet" = settings$fills$sun_wet,
              "Shade-Dry" = settings$fills$shade_dry,
              "Shade-Wet" = settings$fills$shade_wet)

    cols <- c("Sun-Dry" = settings$colors$sun_dry,
              "Sun-Wet" = settings$colors$sun_wet,
              "Shade-Dry" = settings$colors$shade_dry,
              "Shade-Wet" = settings$colors$shade_wet)
  }

  ggplot(data, aes(x = val, y = val2,
                               fill = gr,
                               col = gr)) +
  geom_point(shape = 21) +
  facet_grid(trait2 ~ LMA,
             scales = "free",
             switch = "both",
             labeller = labeller(trait2 = label_parsed,
                                 LMA = label_parsed)) +
  geom_text(data = lab1, aes(label = lab),
            hjust = 0.25,
            vjust = 0.25,
            size = 8 * 5/14,
            show.legend = FALSE,
            color = "black") +
  geom_text(data = lab1, aes(label = r_vals, x = val_max, y = val2),
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

#' @title Generates long data for ggplot (GLOPNET)
gen_gl_long <- function(gl_res_csv) {
  read_csv(gl_res_csv) |>
    mutate(leaf_habit = ifelse(is.na(leaf_habit), "U", as.character(leaf_habit))) |>
    mutate(gr = factor(leaf_habit,
                       labels = c("Deciduous",
                                  "Evergreen",
                                  "Unclassified"
                                  ))) |>
    pivot_longer(c(LMA, LMAs, LMAm), names_to = "LMA", values_to = "val") |>
    pivot_longer(c(Aarea, Rarea, LL, Narea, Parea), names_to = "trait", values_to = "val2") |>
    # mutate(leaf_habit = factor(leaf_habit,
    #         levels = c("D", "E", "U"))) %>%
    mutate(LMA = factor(LMA,
      labels = c("LMA", "LMAm", "LMAs"))) %>%
    mutate(LMA = factor(LMA,
      labels = c("LMA~(~g~m^{-2})", "LMAm~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) %>%
    mutate(trait = factor(trait,
      levels = c("Aarea", "Rarea", "LL", "Narea", "Parea"))) %>%
    mutate(trait2 = factor(trait,
      labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
                 "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
                 "LL~(months)",
                 "italic(N)[area]~(~g~m^{-2})",
                 "italic(P)[area]~(~g~m^{-2})")))
}

#' @title Generates long data for ggplot (Panama)
gen_pa_long <- function(pa_res_csv) {
  #targets::tar_load(pa_res_csv)
  read_csv(pa_res_csv) |>
    pivot_longer(c(LMA, LMAs, LMAm), names_to = "LMA", values_to = "val") |>
    pivot_longer(c(Aarea, Rarea, LL, Narea, Parea, cell_area),
      names_to = "trait", values_to = "val2") |>
    # mutate(leaf_habit = factor(leaf_habit,
    #         levels = c("D", "E", "U"))) |>
    mutate(LMA = factor(LMA,
      labels = c("LMA", "LMAm", "LMAs"))) |>
    mutate(LMA = factor(LMA,
      labels = c("LMA~(~g~m^{-2})", "LMAm~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) |>
    mutate(site_strata = factor(site_strata,
            levels = c("WET_CAN", "DRY_CAN", "WET_UNDER", "DRY_UNDER"))) |>
    mutate(trait = factor(trait,
      levels = c("Aarea", "Rarea", "LL", "Narea", "Parea", "cell_area"))) |>
    mutate(trait2 = factor(trait,
      labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
                 "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
                 "LL~(months)",
                 "italic(N)[area]~(~g~m^{-2})",
                 "italic(P)[area]~(~g~m^{-2})",
                 "CL[area]~(~g~m^{-2})"
                 ))) |>
    mutate(gr = factor(site_strata,
      labels = c("Sun-Wet",
                 "Sun-Dry",
                 "Shade-Wet",
                 "Shade-Dry"
                        ))) |>
    arrange(gr)
}

#' @title Generates long data for ggplot (Panama)
gen_pa_ld_long <- function(pa_res_ld_csv) {
  #targets::tar_load(pa_res_csv)
  read_csv(pa_res_ld_csv) |>
    pivot_longer(c(LMA, LMAs, LDs, LMAm), names_to = "LMA", values_to = "val") |>
    pivot_longer(c(Aarea, Rarea, LL, Narea, Parea, cell_area),
      names_to = "trait", values_to = "val2") |>
    # mutate(leaf_habit = factor(leaf_habit,
    #         levels = c("D", "E", "U"))) |>
    mutate(LMA = factor(LMA,
      labels = c("LMA", "LMAm", "LMAs", "LDs"))) |>
    mutate(LMA = factor(LMA,
      labels = c("LMA~(~g~m^{-2})", "LMAm~(~g~m^{-2})",
       "LMAs~(~g~m^{-2})", "LDs~(~g~cm^{-3})"))) |>
    mutate(site_strata = factor(site_strata,
            levels = c("WET_CAN", "DRY_CAN", "WET_UNDER", "DRY_UNDER"))) |>
    mutate(trait = factor(trait,
      levels = c("Aarea", "Rarea", "LL", "Narea", "Parea", "cell_area"))) |>
    mutate(trait2 = factor(trait,
      labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
                 "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
                 "LL~(months)",
                 "italic(N)[area]~(~g~m^{-2})",
                 "italic(P)[area]~(~g~m^{-2})",
                 "CL[area]~(~g~m^{-2})"
                 ))) |>
    mutate(gr = factor(site_strata,
      labels = c("Sun-Wet",
                 "Sun-Dry",
                 "Shade-Wet",
                 "Shade-Dry"
                        ))) |>
    arrange(gr)
}

#' @title Scatter plots for GL
gl_point <- function(gl_long_dat, settings_yml, r_vals_yml) {
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  dat <- gl_long_dat |>
    filter(trait %in% c("LL", "Aarea", "Rarea"))

  lim_gl <- lim_func(dat)
  lim_gl2 <- lim_func(dat, LMA = FALSE)

  lab1 <- tibble(lab = paste("(", letters[1:9], ")", sep = ""),
                     val2 = lim_gl2$max_val %>% rep(each = 3),
                    # val2 = Inf,
                     val = lim_gl$min_val %>% rep(3),
                     val_max = lim_gl$max_val %>% rep(3),
                     gr = "Evergreen",
                     r_vals = r_vals$r_vals$GL %>% unlist,
                     trait2 = rep(lim_gl2$trait2, each = 3),
                     LMA = rep(lim_gl$LMA, 3)
                     ) |>
    mutate(r_vals = str_replace_all(r_vals, "rho", "\u03C1"))

  scatter_plt(dat, lab1, settings_yml)
}

#' @title Scatter plots for GL (NP)
# targets::tar_load(gl_long_dat)
# targets::tar_load(settings_yml)
# targets::tar_load(r_vals_yml)
gl_point_np <- function(gl_long_dat, settings_yml, r_vals_yml) {
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  dat <- gl_long_dat |>
    filter(trait %in% c("Narea", "Parea")) |>
    filter(!is.na(val2))

  # dat |>
  #   ggplot(aes(x = val, y = val2))  +
  #   geom_point() +
  #   facet_wrap(trait2 ~ LMA, scale = "free") +
  #   scale_x_log10() +
  #   scale_y_log10()

  lim_gl <- lim_func(dat)
  lim_gl2 <- lim_func(dat, LMA = FALSE)
  r_vals <- r_vals$r_vals$GL_NP %>% unlist

  lab1 <- tibble(lab = paste("(", letters[1:6], ")", sep = ""),
                     val2 = lim_gl2$max_val %>% rep(each = 3),
                    # val2 = Inf,
                     val = lim_gl$min_val %>% rep(2),
                     val_max = lim_gl$max_val %>% rep(2),
                     gr = "Evergreen",
                     r_vals = r_vals,
                     #kr_vals = r_vals[4:6],
                     trait2 = rep(lim_gl2$trait2, each = 3),
                     LMA = rep(lim_gl$LMA, 2)
                     ) |>
    mutate(r_vals = str_replace_all(r_vals, "rho", "\u03C1"))

  scatter_plt(dat, lab1, settings_yml) +
    scale_x_log10()
   # scale_x_log10(breaks = my_breaks_x(), expand = c(0.1, 0))
}

#' @title Scatter plots for GL (P)
gl_point_p <- function(gl_long_dat, settings_yml, r_vals_yml) {
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  dat <- gl_long_dat |>
    filter(trait %in% c("Parea")) |>
    filter(!is.na(val2))

  lim_gl <- lim_func(dat)
  lim_gl2 <- lim_func(dat, LMA = FALSE)
  r_vals <- r_vals$r_vals$GL_NP %>% unlist

  lab1 <- tibble(lab = paste("(", letters[4:6], ")", sep = ""),
                     val2 = lim_gl2$max_val %>% rep(each = 3),
                    # val2 = Inf,
                     val = lim_gl$min_val %>% rep(1),
                     val_max = lim_gl$max_val %>% rep(1),
                     gr = "Evergreen",
                     r_vals = r_vals[4:6],
                     trait2 = rep(lim_gl2$trait2, each = 3),
                     LMA = rep(lim_gl$LMA, 1)
                     ) |>
    mutate(r_vals = str_replace_all(r_vals, "rho", "\u03C1"))

  scatter_plt(dat, lab1, settings_yml)
}

#' @title Scatter plots for GL (N)
gl_point_n <- function(gl_long_dat, settings_yml, r_vals_yml) {
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  dat <- gl_long_dat |>
    filter(trait %in% c("Narea")) |>
    filter(!is.na(val2))

  lim_gl <- lim_func(dat)
  lim_gl2 <- lim_func(dat, LMA = FALSE)
  r_vals <- r_vals$r_vals$GL_NP %>% unlist

  lab1 <- tibble(lab = paste("(", letters[1:3], ")", sep = ""),
                     val2 = lim_gl2$max_val %>% rep(each = 3),
                    # val2 = Inf,
                     val = lim_gl$min_val %>% rep(1),
                     val_max = lim_gl$max_val %>% rep(1),
                     gr = "Evergreen",
                     r_vals = r_vals[1:3],
                     trait2 = rep(lim_gl2$trait2, each = 3),
                     LMA = rep(lim_gl$LMA, 1)
                     ) |>
    mutate(r_vals = str_replace_all(r_vals, "rho", "\u03C1"))

  scatter_plt(dat, lab1, settings_yml)
}

#' @title Scatter plots for GL (Np) seperated version
gl_point_np2 <- function(gl_long_dat, settings_yml, r_vals_yml) {
  p1 <- gl_point_n(gl_long_dat, settings_yml, r_vals_yml)
  p2 <- gl_point_p(gl_long_dat, settings_yml, r_vals_yml)
  p1 / p2
}

#' @title Scatter plots for Panama
pa_point <- function(pa_long_dat, settings_yml, r_vals_yml) {
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  dat <- pa_long_dat |>
    filter(trait %in% c("LL", "Aarea", "Rarea"))

  lim_pa <- lim_func(dat)
  lim_pa2 <- lim_func(dat, LMA = FALSE)

  lab1 <- tibble(lab = paste("(", letters[1:9], ")", sep = ""),
                     val2 = lim_pa2$max_val %>% rep(each = 3),
                    # val2 = Inf,
                     val = lim_pa$min_val %>% rep(3),
                     val_max = lim_pa$max_val %>% rep(3),
                     gr = "Sun-Dry",
                     r_vals = r_vals$r_vals$PA %>% unlist,
                     trait2 = rep(lim_pa2$trait2, each = 3),
                     LMA = rep(lim_pa$LMA, 3)
                     ) |>
    mutate(r_vals = str_replace_all(r_vals, "rho", "\u03C1"))

  scatter_plt(dat, lab1, settings_yml, GL = FALSE)
}

#' @title Scatter plots for Panama
pa_ld_point <- function(pa_long_dat, settings_yml, r_vals_yml) {
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  dat <- pa_long_dat |>
    filter(trait %in% c("LL", "Aarea", "Rarea")) |>
    filter(LMA != "LDs~(~g~cm^{-3})")

  lim_pa <- lim_func(dat)
  lim_pa2 <- lim_func(dat, LMA = FALSE)

  lab1 <- tibble(lab = paste("(", letters[1:9], ")", sep = ""),
                     val2 = lim_pa2$max_val %>% rep(each = 3),
                    # val2 = Inf,
                     val = lim_pa$min_val %>% rep(3),
                     val_max = lim_pa$max_val %>% rep(3),
                     gr = "Sun-Dry",
                     r_vals = r_vals$r_vals$PA %>% unlist,
                     trait2 = rep(lim_pa2$trait2, each = 3),
                     LMA = rep(lim_pa$LMA, 3)
                     ) |>
    mutate(r_vals = str_replace_all(r_vals, "rho", "\u03C1"))

  scatter_plt(dat, lab1, settings_yml, GL = FALSE)
}

#' @title Scatter plots for Panama (NPC)
pa_point_npc <- function(pa_long_dat, settings_yml, r_vals_yml) {
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  dat <- pa_long_dat |>
    filter(trait %in% c("Narea", "Parea", "cell_area")) |>
    filter(!is.na(val2))

  lim_pa <- lim_func(dat)
  lim_pa2 <- lim_func(dat, LMA = FALSE)

  lab1 <- tibble(lab = paste("(", letters[1:9], ")", sep = ""),
                     val2 = lim_pa2$max_val %>% rep(each = 3),
                    # val2 = Inf,
                     val = lim_pa$min_val %>% rep(3),
                     val_max = lim_pa$max_val %>% rep(3),
                     gr = "Sun-Dry",
                     r_vals = r_vals$r_vals$PA_NP %>% unlist,
                     trait2 = rep(lim_pa2$trait2, each = 3),
                     LMA = rep(lim_pa$LMA, 3)
                     ) |>
    mutate(r_vals = str_replace_all(r_vals, "rho", "\u03C1"))

  scatter_plt(dat, lab1, settings_yml, GL = FALSE)
}

#' @title Scatter plots for Panama (LL)
pa_point_ll <- function(pa_res_csv, settings_yml, r_vals_yml) {
  dat <- read_csv(pa_res_csv) %>%
  mutate(site_strata = factor(site_strata,
          levels = c("WET_CAN", "DRY_CAN", "WET_UNDER", "DRY_UNDER"))) %>%
  mutate(gr = factor(site_strata,
    labels = c("Sun-Wet",
               "Sun-Dry",
               "Shade-Wet",
               "Shade-Dry"
                      )))
  settings <- yaml::yaml.load_file(settings_yml)
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  fills <- c(
   "Sun-Dry" = settings$fills$sun_dry,
   "Sun-Wet" = settings$fills$sun_wet,
   "Shade-Dry" = settings$fills$shade_dry,
   "Shade-Wet" = settings$fills$shade_wet
  )

  cols <- c(
   "Sun-Dry" = settings$colors$sun_dry,
   "Sun-Wet" = settings$colors$sun_wet,
   "Shade-Dry" = settings$colors$shade_dry,
   "Shade-Wet" = settings$colors$shade_wet
  )


  labLL <- tibble(
     LL = Inf,
     val = min(dat$LL),
     val_max = max(dat$LL),
     gr = "Sun-Dry",
     r_vals = r_vals$r_vals$PA_R2$LL_R2
   )

   ggplot(dat, aes(y = exp(Mu2), x = LL,
                                fill = gr, col = gr)) +
    geom_point(shape = 21) +
    scale_fill_manual(values = fills) +
    scale_colour_manual(values = cols) +
    scale_x_log10(breaks = my_breaks(), expand = c(0.1, 0),
                  limits = c(min(labLL$val), max(labLL$val_max))) +
    scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0),
                  limits = c(min(labLL$val), max(labLL$val_max))) +
    ylab("Predicted LL (months)") +
    xlab("Observed LL (months)") +
    geom_abline(aes(slope = 1, intercept = 0),
                lty = 2,
                lwd = 0.25,
                col = "gray20") +
    geom_text(data = labLL, aes(label = r_vals, x = val_max, y = 2.5),
              colour = "black",
              hjust = 1,
              vjust = 0,
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
}

#' @title Scatter plots for Panama (LL partial)
pa_point_par_ll <- function(pa_res_csv, settings_yml, r_vals_yml) {
  # library(tidyverse)
  # targets::tar_load(pa_res_csv)
  # targets::tar_load(settings_yml)
  # targets::tar_load(r_vals_yml)
  dat <- read_csv(pa_res_csv) |>
  mutate(site_strata = factor(site_strata,
          levels = c("WET_CAN", "DRY_CAN", "WET_UNDER", "DRY_UNDER"))) |>
  mutate(gr = factor(site_strata,
    labels = c("Sun-Wet",
               "Sun-Dry",
               "Shade-Wet",
               "Shade-Dry"
                      )))
  settings <- yaml::yaml.load_file(settings_yml)
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  fills <- c(
   "Sun-Dry" = settings$fills$sun_dry,
   "Sun-Wet" = settings$fills$sun_wet,
   "Shade-Dry" = settings$fills$shade_dry,
   "Shade-Wet" = settings$fills$shade_wet
  )

  cols <- c(
   "Sun-Dry" = settings$colors$sun_dry,
   "Sun-Wet" = settings$colors$sun_wet,
   "Shade-Dry" = settings$colors$shade_dry,
   "Shade-Wet" = settings$colors$shade_wet
  )

  labLL <- tibble(
     res_LMAs_light = 1,
     val = min(dat$res_LMAs_light),
     val_max = max(dat$res_LMAs_light),
     gr = "Sun-Dry",
     r_vals = r_vals$r_vals$PA_par$LMAs_LL
   )

  ggplot(dat, aes(y = res_LL_light,
                  x = res_LMAs_light,
                  fill = gr, col = gr)) +
   geom_point(shape = 21) +
   scale_fill_manual(values = fills) +
   scale_colour_manual(values = cols) +
   ylab("Residuals of LL regressed on light") +
   xlab("Residuals of LMAs regressed on light") +
   geom_abline(aes(slope = 1, intercept = 0),
               lty = 2,
               lwd = 0.25,
               col = "gray20") +
   geom_text(data = labLL,
             aes(label = r_vals, x = val_max, y = -1.7),
             colour = "black",
             hjust = 1.2,
             vjust = 0,
             parse = TRUE,
             show.legend = FALSE) +
   coord_fixed() +
   coord_cartesian(xlim = c(-2, max(labLL$val_max)),
    ylim = c(-2, labLL$val_max)) +
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
}

prep_gl_box_list <- function(gl_res_dat, letters_yml) {
  data <- gl_res_dat |>
    filter(leaf_habit != "U") |>
    dplyr::select(sp, leaf_habit, gr, LMA, LMAm, LMAs) |>
    pivot_longer(LMA:LMAs, names_to = "LMA", values_to = "val") |>
    unique() |>
    mutate(gr = ifelse(gr == "Deciduous", "Dec", "Eve"))
  p_letters <- yaml::yaml.load_file(letters_yml)
  lab1 <- data |>
    group_by(gr, LMA) |>
    summarize(val = max(val) * 0.9) |>
    ungroup() |>
    arrange(LMA) |>
    mutate(lab = p_letters$GL
           |> unlist())
  list(data = data, lab = lab1)
}

#prep_pa_box_list <- function(pa_inter_box_dat, letters_yml, trim = TRUE, de = FALSE) {
prep_pa_box_list <- function(pa_inter_box_dat, letters_yml, type = c("PA_inter", "PA_intra", "PA_inter_de", "PA_intra_de")) {
  # targets::tar_load(pa_inter_box_dat)
  # targets::tar_load(letters_yml)
  # pa_inter_box_dat$leaf_habit
  # pa_inter_box_dat$n
  p_letters <- yaml::yaml.load_file(letters_yml)
  data <- pa_inter_box_dat |>
    dplyr::select(sp, n, leaf_habit, gr, LMA, LMAm, LMAs) |>
    filter(!is.na(leaf_habit)) |>
    mutate(leaf_habit = ifelse(leaf_habit == "D", "Dec", "Eve")) |>
    pivot_longer(LMA:LMAs, names_to = "LMA", values_to = "val") |>
    #dplyr::select(gr, val, LMA) |>
    unique()

  if (str_detect(type, "intra")) {
    data <- data |> filter(n >= 2)
  }
  if (str_detect(type, "de")) {
    data <- data |> mutate(gr = leaf_habit)
  }

  # type <- "PA_intra"
  # p_letters[paste(type)]
  lab1 <- data |>
    group_by(gr, LMA) |>
    summarize(val = max(val) * 0.9) |>
    ungroup() |>
    arrange(LMA) |>
    mutate(lab = p_letters[paste(type)]
           |> unlist())
  list(data = data, lab = lab1)
}

#' @title Theme for boxplot
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
                                                r = 0),
                                angle = 90)
        )
}


#' @title boxplot
box_fun <- function(gl_box_list, cols, fills, ylab = "GLOPNET") {
  ggplot(gl_box_list$data, aes(x = gr, y = val)) +
    geom_boxplot(aes(fill = gr, col = gr), outlier.shape = 21, outlier.size = 1) +
    geom_text(data = gl_box_list$lab,
              aes(label = lab),
              vjust = 0,
              hjust = -0.5,
              size = 8 * 5/14) +
    facet_grid(.~ LMA) +
    xlab("") +
    scale_fill_manual(values = fills, guide = "none") +
    scale_colour_manual(values = cols, guide = "none") +
    scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
    theme_box()
}

#' @title Boxplot for eve/dec
#' @para gl_box_list list with data and lab
box_de <- function(gl_box_list, pa_box_trim_de_list, settings_yml) {
  settings <- yaml::yaml.load_file(settings_yml)
  cols <- rep("black", 3)
  fills <- c("Dec" = settings$fills$D,
            "Eve" = settings$fills$E,
            "Unclassified" = settings$fills$U)

  # tar_load(gl_box_list)
  # gl_box_list
  p1 <- box_fun(gl_box_list, cols, fills) +
       ylab(expression(atop("GLOPNET",
                   LMA~(g~m^{-2}))))

  p2 <- box_fun(pa_box_trim_de_list, cols, fills) +
      #  scale_colour_manual(values = cols2, guide = "none") +
       ylab(expression(atop("Panama",
                   LMA~(g~m^{-2})))) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(colour = NA) # invinsible strip
          )

  p1 / p2  +
    plot_annotation(tag_levels = "a")
}

#' @title Boxplot for Panama intra sun/shade
#' @para gl_box_list list with data and lab
box_pa <- function(pa_box_trim_list, settings_yml) {
  settings <- yaml::yaml.load_file(settings_yml)
  fills <- c("Sun\nDry" = settings$fills$sun_dry,
            "Sun\nWet" = settings$fills$sun_wet,
            "Shade\nDry" = settings$fills$shade_dry,
            "Shade\nWet" = settings$fills$shade_wet,
            "Rand" = settings$fills$R)
  cols <- c("Sun\nDry" = settings$colors$sun_dry,
            "Sun\nWet" = settings$colors$sun_wet,
            "Shade\nDry" = settings$colors$shade_dry,
            "Shade\nWet" = settings$colors$shade_wet,
            "Rand" = settings$colors$R)


  p <- box_fun(pa_box_trim_list, cols, fills) +
       ylab(expression(atop("Panama",
                   LMA~(g~m^{-2})))) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(colour = NA) # invinsible strip
          )
  p
}


#' @title Boxplot for Panama inter eve/dec and sun/shade
#' @para gl_box_list list with data and lab
box_intra <- function(gl_box_list, pa_box_trim_de_list, pa_box_trim_list, settings_yml) {
  settings <- yaml::yaml.load_file(settings_yml)
  cols <- rep("black", 3)
  fills <- c("Dec" = settings$fills$D,
            "Eve" = settings$fills$E,
            "Unclassified" = settings$fills$U)
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

  p1 <- box_fun(gl_box_list, cols, fills) +
       ylab(expression(atop("GLOPNET",
                   LMA~(g~m^{-2})))) +
       coord_cartesian(ylim = c(min(gl_box_list$data$val),
       max(gl_box_list$data$val) * 1.2))

  p2 <- box_fun(pa_box_trim_de_list, cols, fills) +
      #  scale_colour_manual(values = cols2, guide = "none") +
       ylab(expression(atop("Panama",
                   LMA~(g~m^{-2})))) +
       coord_cartesian(ylim = c(min(pa_box_trim_de_list$data$val),
       max(pa_box_trim_de_list$data$val) * 1.2)) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(colour = NA) # invinsible strip
          )
  p3 <- box_fun(pa_box_trim_list, cols2, fills2) +
      #  scale_colour_manual(values = cols2, guide = "none") +
       coord_cartesian(ylim = c(min(pa_box_trim_list$data$val),
       max(pa_box_trim_list$data$val) * 1.2)) +
       ylab(expression(atop("Panama",
                   LMA~(g~m^{-2})))) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(colour = NA) # invinsible strip
          )

  p1 / p2 / p3 +
    plot_annotation(tag_levels = "a")
    # theme(plot.margin = margin(-1, 0, 0, 0))
}

#' @title boxplot for SI
#' @para pa_box_list list with data and lab
box_inter <- function(pa_box_de_list, pa_box_list, settings_yml) {
  settings <- yaml::yaml.load_file(settings_yml)
  cols <- rep("black", 3)
  fills <- c("Dec" = settings$fills$D,
            "Eve" = settings$fills$E,
            "Unclassified" = settings$fills$U)
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

  p1 <- box_fun(pa_box_de_list, cols, fills) +
       ylab(expression(atop("Panama",
                   LMA~(g~m^{-2})))) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(colour = NA) # invinsible strip
          )

  p2 <- box_fun(pa_box_list, cols2, fills2) +
       ylab(expression(atop("Panama",
                   LMA~(g~m^{-2})))) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(colour = NA) # invinsible strip
        )
  p1 / p2 +
    plot_annotation(tag_levels = "a")
}

#' @title Boxplot for LMAm fraction (LMAm / LMA)
box_frac <- function(gl_box_dat, pa_intra_box_dat, settings_yml, letters_yml) {
  # targets::tar_load(pa_inter_box_dat)
  # targets::tar_load(gl_box_dat)
  # targets::tar_load(settings_yml)
  # targets::tar_load(letters_yml)
  settings <- yaml::yaml.load_file(settings_yml)
  p_letters <- yaml::yaml.load_file(letters_yml)
  fills <- c("D" = settings$fills$D,
            "E" = settings$fills$E)
  cols <- c("D" = settings$fills$D,
            "E" = settings$fills$E)

  my_y_title <- bquote(atop("The fraction of total LMA",
                           "comprised by LMAm ("*italic(f)*")"))

  lab1 <- gl_box_dat |>
    group_by(leaf_habit) |>
    summarize(frac = max(frac)) |>
    ungroup() |>
    mutate(lab = p_letters$Frac$GL
           |> unlist())

  lab2 <- pa_intra_box_dat |>
    filter(!is.na(leaf_habit)) |>
    group_by(leaf_habit) |>
    summarize(frac = max(frac)) |>
    ungroup() |>
    mutate(lab = p_letters$Frac$PA
           |> unlist())

  p1 <- ggplot(gl_box_dat,
    aes(x = leaf_habit, y = frac, fill = leaf_habit)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1) +
    geom_text(data = lab1, aes(label = lab),
              vjust = -1,
              size = 8 * 5/14) +
    ylab(my_y_title) +
    xlab("") +
    scale_fill_manual(values = fills, guide = "none") +
    ylim(c(0.0, 1.0)) +
#    scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
    theme_box() +
    theme(
      strip.background = element_blank()
          )
  p2 <- ggplot(pa_intra_box_dat,
    aes(x = leaf_habit, y = frac, fill = leaf_habit)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1) +
    geom_text(data = lab2, aes(label = lab),
              vjust = -1,
              size = 8 * 5/14) +
    ylab(my_y_title) +
    xlab("") +
    scale_fill_manual(values = fills, guide = "none") +
    # scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) +
    ylim(c(0.0, 1.0)) +
    theme_box() +
    theme(
      strip.background = element_blank()
          )
  p1 + p2 +
    plot_annotation(tag_levels = "a")
}

#' @title Boxplot for LMAm fraction (for panama)
box_frac_pa <- function(pa_intra_box_dat, settings_yml, letters_yml) {
  # targets::tar_load(pa_intra_box_dat)
  # targets::tar_load(settings_yml)
  # targets::tar_load(letters_yml)
  settings <- yaml::yaml.load_file(settings_yml)
  p_letters <- yaml::yaml.load_file(letters_yml)
  fills <- c("Sun\nDry" = settings$fills$sun_dry,
            "Sun\nWet" = settings$fills$sun_wet,
            "Shade\nDry" = settings$fills$shade_dry,
            "Shade\nWet" = settings$fills$shade_wet)

  cols <- c("Sun\nDry" = settings$colors$sun_dry,
            "Sun\nWet" = settings$colors$sun_wet,
            "Shade\nDry" = settings$colors$shade_dry,
            "Shade\nWet" = settings$colors$shade_wet)

  my_y_title <- bquote(atop("The fraction of total LMA",
                           "comprised by LMAm ("*italic(f)*")"))

  lab <- pa_intra_box_dat |>
    filter(!is.na(gr)) |>
    group_by(gr) |>
    summarize(frac = max(frac)) |>
    ungroup() |>
    mutate(lab = p_letters$Frac$PA_light
           |> unlist())

  p <- ggplot(pa_intra_box_dat,
    aes(x = gr, y = frac)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1,
      aes(fill = gr, col = gr)) +
    geom_text(data = lab, aes(label = lab),
              vjust = -1,
              size = 8 * 5/14) +
    ylab(my_y_title) +
    xlab("") +
    scale_fill_manual(values = fills, guide = "none") +
    scale_color_manual(values = cols, guide = "none") +
    # scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) +
    ylim(c(0.0, 1.0)) +
    theme_box() +
    theme(
      strip.background = element_blank()
          )
  p

}


#' @para data GLOPNET or Panama data with factored gr
ps_point_wrapper <- function(data, settings, GL = TRUE, ci = FALSE) {
  if (GL) {
    fills <- c("Deciduous" = settings$fills$D,
              "Evergreen" = settings$fills$E,
              "Unclassified" = settings$fills$U)

    cols <- c("Deciduous" = settings$colors$D,
              "Evergreen" = settings$colors$E,
              "Unclassified" = settings$colors$U)
  } else {
    fills <- c("Sun-Dry" = settings$fills$sun_dry,
              "Sun-Wet" = settings$fills$sun_wet,
              "Shade-Dry" = settings$fills$shade_dry,
              "Shade-Wet" = settings$fills$shade_wet)

    cols <- c("Sun-Dry" = settings$colors$sun_dry,
              "Sun-Wet" = settings$colors$sun_wet,
              "Shade-Dry" = settings$colors$shade_dry,
              "Shade-Wet" = settings$colors$shade_wet)
  }
  p <- ggplot(data, aes(x = LMAm, y = LMAs,
                               fill = gr,
                               col = gr)) +
  xlab(expression(atop(
                LMAm~(g~m^{-2})))) +
  ylab(expression(atop(
                LMAs~(g~m^{-2})))) +
  scale_fill_manual(values = fills, guide = "none") +
  scale_colour_manual(values = cols, guide = "none") +
  scale_x_log10(breaks = my_breaks_x(), expand = c(0.1, 0)) +
  scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
  theme_LES()
  if (ci) {
    p +
      geom_segment(aes(x = LMAm_lwr, xend = LMAm_upr, y = LMAs, yend = LMAs),
       col = "grey60", size = 0.25) +
      geom_segment(aes(x = LMAm, xend = LMAm, y = LMAs_lwr, yend = LMAs_upr),
      col = "grey60", size = 0.25) +
      geom_point(shape = 21)
  } else {
    p +  geom_point(shape = 21)
  }
}

#' @para gl_res_dat GLOPNET res tibble
#' @para pa_res_dat GLOPNET res tibble
ps_point <- function(gl_res_dat, pa_res_dat, settings_yml, r_vals_yml, ci = FALSE) {
  pa <- pa_res_dat |>
    mutate(gr = factor(site_strata,
      labels = c("Sun-Wet",
                 "Sun-Dry",
                 "Shade-Wet",
                 "Shade-Dry"
                        )))
  print(pa$gr) |> summary()
  print(pa$site_strata) |> summary()
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  settings <- yaml::yaml.load_file(settings_yml)
  p1 <- ps_point_wrapper(gl_res_dat, settings, GL = TRUE, ci = ci) +
   labs(title = "GLOPNET \n",
         subtitle = bquote(italic("r")~"="~.(r_vals$r_vals$GL_LMAms$LMAs_LMAm)))
  p2 <- ps_point_wrapper(pa, settings, GL = FALSE, ci = ci) +
   labs(title = "Panama \n",
         subtitle = bquote(italic("r")~"="~.(r_vals$r_vals$PA_LMAms$LMAs_LMAm)))
  p1 + p2 +
    plot_annotation(tag_levels = "a")
}

#' @title Paired LMAm fraction for Panama
pair_frac_line <- function(pa_res_dat) {
  y_title <- bquote(atop("The fraction of total LMA",
                             "comprised by LMAm ("*italic(f)*")"))
  data <- pa_res_dat |>
    count(sp) |>
    filter(n >= 2) |>
    inner_join(pa_res_dat, by = "sp") |>
    mutate(LMAm_frac = LMAm / LMA) |>
    mutate(site_strata = factor(site_strata,
            levels = c("DRY_CAN",
                       "DRY_UNDER",
                       "WET_CAN",
                       "WET_UNDER"))) |>
    mutate(gr = factor(site_strata,
      labels = c("Sun\nDry",
                 "Shade\nDry",
                 "Sun\nWet",
                 "Shade\nWet"
                        ))) |>
    mutate(data = "(c) Panama: intra")

  data |>
    dplyr::select(sp, strata, site2, LMAm_frac) |>
    mutate(strata = ifelse(strata == "CAN", "Sun", "Shade")) |>
    mutate(strata = factor(strata, levels = c("Sun", "Shade"))) |>
    mutate(site2 = ifelse(site2 == "DRY", "Dry", "Wet")) |>
    ggplot(aes(gr = sp)) +
    geom_line(aes(group = sp, x = strata, y = LMAm_frac)) +
    geom_point(aes(x = strata, y = LMAm_frac)) +
    ylab(y_title) +
    xlab("") +
    facet_wrap(~ site2) +
    guides(col = "none") +
    theme_LES()
}


#' @title Paired LMAm and LMAs for Panama
pair_lma_line <- function(pa_res_dat) {
  data <- pa_res_dat |>
    count(sp) |>
    filter(n >= 2) |>
    inner_join(pa_res_dat, by = "sp") |>
    dplyr::select(sp, strata, site2, LMA, LMAm, LMAs) |>
    pivot_longer(cols = 4:6, names_to = "LMA", values_to = "val") |>
    mutate(strata = ifelse(strata == "CAN", "Sun", "Shade")) |>
    mutate(strata = factor(strata, levels = c("Sun", "Shade"))) |>
    mutate(site2 = ifelse(site2 == "DRY", "Dry", "Wet")) |>
    mutate(LMA = ifelse(LMA == "LMAm", "LMAm", LMA))

  ggplot(data, aes(gr = sp)) +
    geom_line(aes(group = sp, x = strata, y = val)) +
    geom_point(aes(x = strata, y = val)) +
    #ylab(my_y_title) +
    xlab("") +
    #facet_grid(LMA ~ site2) +
    facet_grid(site2 ~ LMA) +
    guides(col = "none") +
    scale_y_log10() +
    ylab(expression(atop(LMA~(g~m^{-2})))) +
    theme_LES()
}

