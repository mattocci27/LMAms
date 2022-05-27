
my_breaks <- function(...){
  c(0.002, 0.005, 0.02, 0.05, 0.1, 0.2,  0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500)
}

my_breaks_x <- function(...){
  c(1, 2, 5, 10, 20, 50, 100, 500)
}

my_ggsave = function(filename, plot, height = 11.4, width = 11.4, units = "cm", ...){
  ggsave(filename = filename,
         plot = plot,
         height = height,
         width = width,
         units = units,
         dpi = 400,
         ...)
}

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

theme_LES <- function(base_size = 9,
                     base_family = "sans") {
  # based on theme_bw which is based on theme_grey
  ret <- theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "black", size = 0.25),
          #panel.grid.major = element_line(colour = "grey87"),
         # panel.grid.minor = element_line(colour = "grey87", size = 0.25),
          panel.grid.major = element_line(colour = NA),
          panel.grid.minor = element_line(colour = NA, size = 0.25),
          panel.spacing = unit(0, "lines"),
          strip.background = element_blank(),
         # strip.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = NA, colour = NA),
          legend.position = "top",
          legend.margin = margin(t = 0, unit = "cm"),
          legend.key.size = unit(0.2, "cm"),
          legend.box = "horizontal",
          legend.box.spacing = unit(0.1, "cm"),
          #plot.background=element_rect(fill="red"), # fix
          #plot.background=element_rect(fill=NA), # fix
          #plot.background = element_blank(),
          strip.text = element_text(size = 9),
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0, "cm"),
          axis.ticks.length = unit(-0.15, "cm"),
          axis.ticks = element_line(size = 0.25),
          axis.title = element_text(size = 8),
          axis.text.x = element_text(margin = margin(0.25, unit = "cm"),
                                     size = unit(8, "pt"),
                                     colour = "black"),
          axis.text.y = element_text(hjust = 0.5,
                                     margin = margin(r = 0.25, unit = "cm"),
                                     debug = FALSE,
                                     size = unit(8, "pt"),
                                     colour = "black"),
          legend.text = element_text(size = 8),
          legend.title = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0,
                                                      b = 0,
                                                      l = -10,
                                                      r = 0),
                                      angle = 90),
          axis.title.x = element_text(margin = margin(t = -5,
                                                      b = -5,
                                                      l = -5,
                                                      r = -5)),
          complete = TRUE)
}


#' @title scatter plots for GL and Panama
scatter_plt <- function(data, lab1, fills, cols) {
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

#' @title scatter plots for GL
gl_point <- function(gl_long_dat, settings_yml, r_vals_yml) {
    # targets::tar_load(gl_long_dat)
    # targets::tar_load(settings_yml)
    # targets::tar_load(r_vals_yml)
  settings <- yaml::yaml.load_file(settings_yml)
  r_vals <- yaml::yaml.load_file(r_vals_yml)
  dat <- gl_long_dat |>
    filter(trait %in% c("LL", "Aarea", "Rarea"))

  lim_GL <- lim_func(dat)
  lim_GL2 <- lim_func(dat, LMA = FALSE)

  lab1 <- tibble(lab = paste("(", letters[1:9], ")", sep = ""),
                     val2 = lim_GL2$max_val %>% rep(each = 3),
                    # val2 = Inf,
                     val = lim_GL$min_val %>% rep(3),
                     val_max = lim_GL$max_val %>% rep(3),
                     gr = "Evergreen",
                     r_vals = r_vals$r_vals$GL %>% unlist,
                     trait2 = rep(lim_GL2$trait2, each = 3),
                     LMA = rep(lim_GL$LMA, 3)
                     ) |>
    mutate(r_vals = str_replace_all(r_vals, "rho", "\u03C1"))

  fills <- c("Deciduous" = settings$fills$D,
            "Evergreen" = settings$fills$E,
            "Unclassified" = settings$fills$U)

  cols <- c("Deciduous" = settings$colors$D,
            "Evergreen" = settings$colors$E,
            "Unclassified" = settings$colors$U)

  scatter_plt(dat, lab1, fills, cols)
}
