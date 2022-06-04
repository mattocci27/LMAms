# fig functions
my_ggsave = function(filename, plot, height = 11.4, width = 11.4, units = "cm", ...){
  ggsave(filename = filename,
         plot = plot,
         height = height,
         width = width,
         units = units,
         dpi = 400,
         ...)
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
          axis.ticks.length = unit(-0.1, "cm"),
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
                                                      l = -5,
                                                      r = 0),
                                      angle = 90),
          axis.title.x = element_text(margin = margin(t = 0,
                                                      b = -5,
                                                      l = -5,
                                                      r = -5)),
          complete = TRUE)
}
