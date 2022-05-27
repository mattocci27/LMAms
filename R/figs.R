#' @title scatter plots for GL and Panama
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


#' @title scatter plots for GL
gl_point <- function(data, settings, r_vals) {
  settings <- yaml::yaml.load_file(settings)
  GL <- read_csv(data) |>
    filter(Trait %in% c("LL", "Aarea", "Rarea"))

  GL_dat <- GL %>%
    gather(LMA, Val, c(LMA, LMAs, LMAp)) %>%
    gather(Trait, Val2, c(Aarea, Rarea, LL,
                          Parea, Narea)) %>%
    mutate(DE = factor(DE,
            levels = c("D", "E", "U"))) %>%
    mutate(LMA = factor(LMA,
      labels = c("LMA", "LMAp", "LMAs"))) %>%
    mutate(LMA = factor(LMA,
      labels = c("LMA~(~g~m^{-2})", "LMAp~(~g~m^{-2})", "LMAs~(~g~m^{-2})"))) %>%
    mutate(Trait = factor(Trait,
      levels = c("Aarea", "Rarea", "LL", "Narea", "Parea"))) %>%
    mutate(Trait2 = factor(Trait,
      labels = c("italic(A)[area]~(~mu~mol~m^{-2}~s^{-1})",
                 "italic(R)[area]~(~mu~mol~m^{-2}~s^{-1})",
                 "LL~(months)",
                 "italic(N)[area]~(~g~m^{-2})",
                 "italic(P)[area]~(~g~m^{-2})")))

  GL_dat1 <- GL_dat |>
    filter(Trait %in% c("LL", "Aarea", "Rarea"))

  lim_GL <- lim_func(GL_dat1)

  lim_GL2 <- lim_func(GL_dat1, LMA = FALSE)

  lab1 <- tibble(lab = paste("(", letters[1:9], ")", sep = ""),
                     Val2 = lim_GL2$max_val %>% rep(each = 3),
                    # Val2 = Inf,
                     Val = lim_GL$min_val %>% rep(3),
                     Val_max = lim_GL$max_val %>% rep(3),
                     gr = "Evergreen",
                     r_vals = r_vals$r_vals$GL %>% unlist,
                     Trait2 = rep(lim_GL2$Trait2, each = 3),
                     LMA = rep(lim_GL$LMA, 3)
                     ) |>
    mutate(r_vals = str_replace_all(r_vals, "rho", "\u03C1"))

  fills <- c("Deciduous" = settings$fills$D,
            "Evergreen" = settings$fills$E,
            "Unclassified" = settings$fills$U)

  cols <- c("Deciduous" = settings$colors$D,
            "Evergreen" = settings$colors$E,
            "Unclassified" = settings$colors$U)

  scatter_plt(GL_dat1)
}
