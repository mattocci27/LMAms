# fig functions

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

lim_func <- function(data, trait, LMA = TRUE) {
  trait <- as.name(trait)
  if (LMA) LMA <- as.name("LMA") else LMA <- as.name("Trait2")
  data %>%
    dplyr::select_(LMA, trait) %>%
    dplyr::group_by_(LMA) %>%
    summarize_(min_val = interp(~log10(min(v, na.rm = TRUE)), v = as.name(trait)),
              max_val = interp(~log10(max(v, na.rm = TRUE)), v = as.name(trait))) %>%
    mutate(mid_val = (max_val - min_val) / 2) %>%
    ungroup %>%
    mutate(min_val = 10^(min_val - mid_val * 0.1)) %>%
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
          plot.background = element_blank(),
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

