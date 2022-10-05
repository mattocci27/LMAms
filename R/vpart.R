#' @title Variance partitioning
vpart_bar <- function(gl_res_csv, pa_res_csv, intra = FALSE) {
  # library(targets)
  # library(tidyverse)
  # targets::tar_load(pa_res_csv)
  # targets::tar_load(gl_res_csv)
  pa <- read_csv(pa_res_csv)

  if (intra) {
    pa <- pa |>
        count(sp) |>
        filter(n >= 2) |>
        inner_join(pa, by = "sp")
  }

  gl <- read_csv(gl_res_csv) |>
    filter(leaf_habit != "U")

  var_ <- function(data, y, x) {
    fo <- str_c("log(", y, ") ~", x)
    tmp <- aov(as.formula(fo), data) |>
     summary()
    tmp2 <- tmp[[1]]$`Sum Sq`
    tmp2 / sum(tmp2) * 100
  }

  gl_eve_var <- bind_rows(
    LMAm = var_(gl, "LMAm", "leaf_habit"),
    LMAs = var_(gl, "LMAs", "leaf_habit"),
    fct = c("Eve/Dec", "Residuals"))

  pa_eve_var <- bind_rows(
    LMAm = var_(pa, "LMAm", "leaf_habit"),
    LMAs = var_(pa, "LMAs", "leaf_habit"),
    fct = c("Eve/Dec", "Residuals"))

  pa_leaf_var <- bind_rows(
    LMAm = var_(pa, "LMAm", "site + strata"),
    LMAs = var_(pa, "LMAs", "site + strata"),
    fct = c("Wet/Dry", "Sun/Shade", "Residuals"))

  my_col <- RColorBrewer::brewer.pal(4, "RdBu")
  my_col <- my_col[-1]
  my_col <- c(my_col, "#9E9E9E")
  names(my_col) <- c("Eve/Dec", "Sun/Shade", "Wet/Dry", "Residuals")

  vpart_gl_eve <- gl_eve_var |>
    pivot_longer(1:2) |>
    mutate(fct = factor(fct, c("Residuals", "Eve/Dec"))) |>
    ggplot(aes(x = name, y = value, fill = fct)) +
      geom_col() +
      ggtitle("GLOPNET") +
      scale_fill_manual(values = my_col) +
      theme_LES() +
      theme(
            legend.position = "none",
            legend.text = element_text(size = 8),
      )

  vpart_pa_eve <- pa_eve_var |>
    pivot_longer(1:2) |>
    mutate(fct = factor(fct, c("Residuals", "Eve/Dec"))) |>
    ggplot(aes(x = name, y = value, fill = fct)) +
      geom_col() +
      ggtitle("Panama") +
      scale_fill_manual(values = my_col) +
      theme_LES() +
      theme(
            legend.position = "none",
            legend.text = element_text(size = 8),
      )

  vpart_pa_leaf <- pa_leaf_var |>
    pivot_longer(1:2) |>
    ggplot(aes(x = name, y = value, fill = fct)) +
      geom_col() +
      ggtitle("Panama") +
      scale_fill_manual(values = my_col) +
      theme_LES() +
      theme(
            legend.position = "bottom",
            legend.justification = 1,
            legend.text = element_text(size = 8),
      )

   vpart_gl_eve + vpart_pa_eve + vpart_pa_leaf +
      plot_annotation(tag_levels = "a") &
      ylab("Explained variance (%)") &
      xlab("")
}

write_vpart_csv <- function(gl_res_csv, pa_res_csv, file, intra = FALSE) {
  # library(targets)
  # library(tidyverse)
  # targets::tar_load(pa_res_csv)
  # targets::tar_load(gl_res_csv)
  pa <- read_csv(pa_res_csv)

  if (intra) {
    pa <- pa |>
        count(sp) |>
        filter(n >= 2) |>
        inner_join(pa, by = "sp")
  }

  gl <- read_csv(gl_res_csv) |>
    filter(leaf_habit != "U")

  var_ <- function(data, y, x) {
    fo <- str_c("log(", y, ") ~", x)
    tmp <- aov(as.formula(fo), data) |>
     summary()
    tmp2 <- tmp[[1]]$`Sum Sq`
    tmp2 / sum(tmp2) * 100
  }

  gl_eve_var <- bind_rows(
    LMAm = var_(gl, "LMAm", "leaf_habit"),
    LMAs = var_(gl, "LMAs", "leaf_habit"),
    fct = c("Eve/Dec", "Residuals")) |>
    mutate(dataset = "gl_de")

  pa_eve_var <- bind_rows(
    LMAm = var_(pa, "LMAm", "leaf_habit"),
    LMAs = var_(pa, "LMAs", "leaf_habit"),
    fct = c("Eve/Dec", "Residuals")) |>
    mutate(dataset = "pa_de")

  pa_leaf_var <- bind_rows(
    LMAm = var_(pa, "LMAm", "site + strata"),
    LMAs = var_(pa, "LMAs", "site + strata"),
    fct = c("Wet/Dry", "Sun/Shade", "Residuals")) |>
    mutate(dataset = "pa_leaf")

  bind_rows(gl_eve_var, pa_eve_var, pa_leaf_var) |>
    write_csv(file)
  paste(file)
}


# d <- read_csv("data/vpart_intra.csv")
# file <- "data/vpart_intra.csv"
get_vpart_para <- function(file, LMA, group, dataset, digits = 2, nsmall = 2) {
  d <- read_csv(file)
  d |>
    mutate_if(is.numeric, \(x) round(x, digits = digits)) |>
    mutate_if(is.numeric, \(x) format(x, nsmall = nsmall)) |>
    filter(dataset == {{dataset}}) |>
    filter(fct == {{group}}) |>
    pull({{LMA}})
}

