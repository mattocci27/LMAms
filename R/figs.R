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
    mutate(mid_val = (max_val - min_val) / 2) %>%
    mutate(min_val = 10^(min_val - mid_val * 0.1)) |>
    mutate(max_val = 10^(max_val + mid_val * 0.1))
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
  geom_text(data = lab1, aes(label = r_vals, x = val_max),
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
    mutate(DE = ifelse(is.na(DE), "U", as.character(DE))) |>
    mutate(gr = factor(DE,
                       labels = c("Deciduous",
                                  "Evergreen",
                                  "Unclassified"
                                  ))) |>
    pivot_longer(c(LMA, LMAs, LMAp), names_to = "LMA", values_to = "val") |>
    pivot_longer(c(Aarea, Rarea, LL, Narea, Parea), names_to = "trait", values_to = "val2") |>
    # mutate(DE = factor(DE,
    #         levels = c("D", "E", "U"))) %>%
    mutate(LMA = factor(LMA,
      labels = c("LMA", "LMAp", "LMAs"))) %>%
    mutate(LMA = factor(LMA,
      labels = c("LMA~(~g~m^{-2})", "LMAp~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) %>%
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
    pivot_longer(c(LMA, LMAs, LMAp), names_to = "LMA", values_to = "val") |>
    pivot_longer(c(Aarea, Rarea, LL, Narea, Parea, cell_area),
      names_to = "trait", values_to = "val2") |>
    # mutate(DE = factor(DE,
    #         levels = c("D", "E", "U"))) |>
    mutate(LMA = factor(LMA,
      labels = c("LMA", "LMAp", "LMAs"))) |>
    mutate(LMA = factor(LMA,
      labels = c("LMA~(~g~m^{-2})", "LMAp~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) |>
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
                 "italic(CL)[area]~(~g~m^{-2})"
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
    # targets::tar_load(gl_long_dat)
    # targets::tar_load(settings_yml)
    # targets::tar_load(r_vals_yml)
  # settings <- yaml::yaml.load_file(settings_yml)
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
                     r_vals = r_vals$r_vals$gl %>% unlist,
                     trait2 = rep(lim_gl2$trait2, each = 3),
                     LMA = rep(lim_gl$LMA, 3)
                     ) |>
    mutate(r_vals = str_replace_all(r_vals, "rho", "\u03C1"))

  scatter_plt(dat, lab1, settings_yml)
}


#' @title Scatter plots for Panama
pa_point <- function(pa_long_dat, settings_yml, r_vals_yml) {
    # targets::tar_load(pa_long_dat)
    # targets::tar_load(settings_yml)
    # targets::tar_load(r_vals_yml)
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


#' @title Scatter plots for Panama (NPC)
pa_point_npc <- function(pa_long_dat, settings_yml, r_vals_yml) {
    # targets::tar_load(pa_long_dat)
    # targets::tar_load(settings_yml)
    # targets::tar_load(r_vals_yml)
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  dat <- pa_long_dat |>
    filter(trait %in% c("Narea", "Parea", "cell_area"))

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


prep_gl_box_list <- function(gl_res_dat, letters_yml) {
  data <- gl_res_dat |>
    filter(DE != "U") |>
    dplyr::select(sp, DE, gr, LMA, LMAp, LMAs) |>
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

prep_pa_box_list <- function(pa_inter_box_dat, letters_yml, trim = TRUE) {
  # targets::tar_load(pa_inter_box_dat)
  # targets::tar_load(letters_yml)
  # pa_inter_box_dat$DE
  # pa_inter_box_dat$n
  p_letters <- yaml::yaml.load_file(letters_yml)
  data <- pa_inter_box_dat |>
    dplyr::select(sp, n, DE, gr, LMA, LMAp, LMAs) |>
    pivot_longer(LMA:LMAs, names_to = "LMA", values_to = "val") |>
    #dplyr::select(gr, val, LMA) |>
    unique()
  if (trim) {
    data <- data |> filter(n >= 2)
  }
  lab1 <- data |>
    group_by(gr, LMA) |>
    summarize(val = max(val) * 0.9) |>
    ungroup() |>
    arrange(LMA) |>
    mutate(lab = p_letters$PA
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
box_fun <- function(gl_box_list, fills, ylab = "GLOPNET") {
  ggplot(gl_box_list$data, aes(x = gr, y = val, fill = gr)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1) +
    geom_text(data = gl_box_list$lab, aes(label = lab),
              vjust = -1,
              size = 8 * 5/14) +
    facet_grid(.~ LMA) +
    xlab("") +
    scale_fill_manual(values = fills, guide = "none") +
    scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
    theme_box()
}

#' @title boxplot
#' @para gl_box_list list with data and lab
box_main <- function(gl_box_list, pa_box_trim_list, settings_yml) {
  settings <- yaml::yaml.load_file(settings_yml)
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
  p1 <- box_fun(gl_box_list, fills) +
       ylab(expression(atop("GLOPNET",
                   LMA~(g~m^{-2}))))
  p2 <- box_fun(pa_box_trim_list, fills2) +
       scale_colour_manual(values = cols2, guide = "none") +
       ylab(expression(atop("Panama",
                   LMA~(g~m^{-2})))) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(colour = NA) # invinsible strip
        ) # +
  p1 / p2
}

#' @title Boxplot for LMAp fraction (LMAp / LMA)
box_frac <- function(gl_box_dat, pa_intra_box_dat, settings_yml, letters_yml) {
  # targets::tar_load(pa_inter_box_dat)
  # targets::tar_load(gl_box_dat)
  settings <- yaml::yaml.load_file(settings_yml)
  p_letters <- yaml::yaml.load_file(letters_yml)
  fills <- c("D" = settings$fills$D,
            "E" = settings$fills$E)
  cols <- c("D" = settings$fills$D,
            "E" = settings$fills$E)

  my_y_title <- bquote(atop("The fraction of total LMA",
                           "comprised by LMAp ("*italic(f)*")"))

  lab1 <- gl_box_dat |>
    group_by(DE) |>
    summarize(frac = max(frac)) |>
    ungroup() |>
    mutate(lab = p_letters$Frac$GL
           |> unlist())

  lab2 <- pa_intra_box_dat |>
    filter(!is.na(DE)) |>
    group_by(DE) |>
    summarize(frac = max(frac)) |>
    ungroup() |>
    mutate(lab = p_letters$Frac$PA
           |> unlist())

  p1 <- ggplot(gl_box_dat,
    aes(x = DE, y = frac, fill = DE)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1) +
    geom_text(data = lab1, aes(label = lab),
              vjust = -1,
              size = 8 * 5/14) +
    ylab(my_y_title) +
    xlab("") +
    scale_fill_manual(values = fills, guide = "none") +
#    scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
    theme_box() +
    theme(
      strip.background = element_blank()
          )
  p2 <- ggplot(pa_intra_box_dat,
    aes(x = DE, y = frac, fill = DE)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1) +
    geom_text(data = lab2, aes(label = lab),
              vjust = -1,
              size = 8 * 5/14) +
    ylab(my_y_title) +
    xlab("") +
    scale_fill_manual(values = fills, guide = "none") +
   # scale_y_log10(breaks = my_breaks(), expand = c(0.1, 0)) +
    theme_box() +
    theme(
      strip.background = element_blank()
          )
  p1 + p2 +
    plot_annotation(tag_levels = "a")
}
# box_main(gl_box_list, pa_box_trim_list, settings_yml)

# targets::tar_load(gl_box_list)
# targets::tar_load(pa_box_trim_list)

#box_main(gl_box_list, fills = fills)

# targets::tar_load(pa_res_csv)
# pa_dat <- read_csv(pa_res_csv)

#  targets::tar_load(gl_res_dat)
#  targets::tar_load(pa_res_dat)

#pa_res_dat$gr

# gl_dat <- read_csv(gl_res_csv)
