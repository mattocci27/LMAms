#' @title Variance patitoning
vpart_bar <- function(gl_res_csv, pa_res_csv, intra = FALSE) {
#  targets::tar_load(pa_res_csv)
#  targets::tar_load(gl_res_csv)
  PA <- read_csv(pa_res_csv)

  if (intra) {
    PA <- PA |>
        count(sp) |>
        filter(n >= 2) |>
        inner_join(PA, by = "sp")
  }

  GL <- read_csv(gl_res_csv)

  tmp <- aov(log(LMAp) ~ site + strata, PA) |>
    summary()
  tmp2 <- tmp[[1]][[2]]
  PA_LMAp <- tmp2/sum(tmp2) * 100

  tmp <- aov(log(LMAs) ~ site + strata, PA) |>
    summary()
  tmp2 <- tmp[[1]][[2]]
  PA_LMAs <- tmp2/sum(tmp2) * 100

  my_col <- RColorBrewer::brewer.pal(4, "RdBu")
  my_col <- my_col[-1]
  my_col <- c(my_col, "#9E9E9E")
  names(my_col) <- c("Eve/Dec", "Sun/Shade", "Wet/Dry", "Residuals")

#"#F4A582" "#92C5DE" "#0571B0" "#9E9E9E"

  vpart_PA <- bind_rows(LMAp = PA_LMAp, LMAs = PA_LMAs,
                        fct = c("Wet/Dry", "Sun/Shade", "Residuals")) |>
             pivot_longer(1:2) |>
             ggplot(aes(x = name, y = value, fill = fct)) +
               geom_col() +
               ggtitle("Panama") +
               scale_fill_manual(values = my_col) +
               theme_LES() +
               theme(
                     legend.position = "right",
                     legend.text = element_text(size = 8),
               )

  tmp <- aov(log(LMAp) ~ leaf_habit, GL) |>
    summary()
  tmp2 <- tmp[[1]][[2]]
  GL_LMAp <- tmp2/sum(tmp2) * 100

  tmp <- aov(log(LMAs) ~ leaf_habit, GL) |>
    summary()
  tmp2 <- tmp[[1]][[2]]
  GL_LMAs <- tmp2/sum(tmp2) * 100

  vpart_GL <- bind_rows(LMAp = GL_LMAp, LMAs = GL_LMAs,
                        fct = c("Eve/Dec", "Residuals")) |>
             mutate(fct = factor(fct, c("Residuals", "Eve/Dec"))) |>
             pivot_longer(1:2) |>
             ggplot(aes(x = name, y = value, fill = fct)) +
               geom_col() +
               ggtitle("GLOPNET") +
               theme_LES() +
               scale_fill_manual(values = my_col) +
               theme(legend.spacing.x = unit(0.1, "cm"),
                     legend.position = "none"
               )

  vpart_GL + vpart_PA &
    ylab("Explained variance (%)") &
    xlab("")
}

#' @title rlnorm simluation
#'
