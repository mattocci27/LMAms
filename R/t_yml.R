#' @title Pairwise t-test for LMA
#' @para LMA vector of LMA
#' @para group group name (e.g., "leaf_habit")
p_group <- function(LMA, group, log = TRUE) {
  if (log) LMA <- log(LMA)
  moge <- pairwise.t.test(LMA, group, p.adjust.method = "bonferroni")$p.value

  dif_name <- NULL
  for (i in 1:ncol(moge)) {
    for (j in 1:nrow(moge)) {
      dif_name_new <- paste(colnames(moge)[i], rownames(moge)[j], sep = "-")
      dif_name <- c(dif_name, dif_name_new)
    }
  }

  pval <- moge %>% as.vector
  p_dat <- tibble(dif_name, pval) %>%
    na.omit
  p_vec <- p_dat$pval
  names(p_vec) <- p_dat$dif_name

  multcompView::multcompLetters(p_vec)$Letters
}

#' @title Pairwise t-test for MCMC
#' @para draws MCMC output (e.g., fit_7_draws_GL_Aps_LLs)
#' @para data boxplot data (e.g., pa_inter_box_dat)
#' @para x name of variancle (e.g., "frac", "LMAm", "LMAs")
#' @para group group name (e.g., "leaf_habit")
p_group2 <- function(draws,
          data,
          x,
          group = c("leaf_habit", "site_strata2")
          ){
  id_dat <- data |>
    pull(id) |>
    str_split_fixed("_", 2)
  id <- id_dat[,2] |> as.numeric()

  draws <- draws |>
    janitor::clean_names()
  pmat0 <- draws |>
    dplyr::select(contains("p_")) |>
    as.matrix()
  pmat <- pmat0[, paste0("p_", id)]

  moge <- p_post(pmat, data, x, group = group)
#  print(moge)
  dif_name <- NULL
  for (i in 1:ncol(moge)) {
    for (j in 1:nrow(moge)) {
      dif_name_new <- paste(colnames(moge)[i], rownames(moge)[j], sep = "-")
      dif_name <- c(dif_name, dif_name_new)
    }
  }
  pval <- moge %>% as.vector
  p_dat <- tibble(dif_name, pval) %>%
    na.omit
  p_vec <- p_dat$pval
  names(p_vec) <- p_dat$dif_name
  multcompView::multcompLetters(p_vec)$Letters
}


#' @title Wrapper function for p_goup2
#' @para pmat matrix of latent variable p (iters * n_sample)
p_post <- function(pmat,
          data,
          x,
          group = c("leaf_habit", "site_strata2")
          ){

  if (group == "leaf_habit") {
    m <- array(dim = c(2, 2, nrow(pmat)))
    alpha <- 1
  } else {
    m <- array(dim = c(4, 4, nrow(pmat)))
    alpha <- 6
  }

  group2 <- data |>
    pull(group)
  LMA <- data |>
    pull(LMA)

  for (i in 1:nrow(pmat)) {
    frac <- pmat[i,]
    LMAm <- log(frac * LMA)
    LMAs <- log(LMA - LMAm)
    xbar <- tapply(get(x), group2, \(x)mean(x, na.rm = TRUE))
    for (j in 1:length(xbar)) {
      for (k in 1:length(xbar)) {
        dif <- xbar[j] - xbar[k]
        m[j, k, i] <- dif
      }
    }
   }

  lwr <- apply(m,  1:2, \(x)quantile(x, 0.025 / alpha, na.rm = TRUE))
  upr <- apply(m,  1:2, \(x)quantile(x, 1 - 0.025 / alpha, na.rm = TRUE))
  sig_m <- lwr * upr
  colnames(sig_m) <- rownames(sig_m) <- names(xbar)
  sig_m[sig_m > 0] <- 0.01
  sig_m[sig_m == 0] <- NA
  sig_m[sig_m < 0] <- 1

  il_tri <- lower.tri(sig_m, TRUE)
  if (group == "site_strata2") {
    m2 <- matrix(sig_m[il_tri][-1], ncol = 3, nrow = 3)
    m2[3, 3] <- m2[2,3]
    m2[2, 3] <- NA
    rownames(m2) <- rownames(sig_m)[2:4]
    colnames(m2) <- colnames(sig_m)[1:3]
  } else {
    m2 <- matrix(sig_m[il_tri][2], ncol = 1, nrow = 1)
    rownames(m2) <- rownames(sig_m)[2]
    colnames(m2) <- colnames(sig_m)[1]
  }
  m2
}

#' @title Prepare data for boxplot and t-test (Panama - short)
#' This returns inter or intra data for sun/shade or eve/dec
prep_pa_box_dat <- function(pa_res_csv, intra = TRUE, sun = TRUE) {
  # targets::tar_load(pa_res_csv)
  # pa <- read_csv(pa_res_csv)
  pa <- read_csv(pa_res_csv) |>
    mutate(frac = LMAm / LMA) |>
    mutate(leaf_habit = ifelse(leaf_habit == "evergreen", "E", leaf_habit)) |>
    mutate(leaf_habit = ifelse(leaf_habit == "deciduous", "D", leaf_habit)) |>
    mutate(site_strata2 = case_when(
      site_strata == "DRY_CAN" ~ "Sun_Dry",
      site_strata == "DRY_UNDER" ~ "Shade_Dry",
      site_strata == "WET_CAN" ~ "Sun_Wet",
      site_strata == "WET_UNDER" ~ "Shade_Wet"
    )) |>
    mutate(site_strata2 = factor(site_strata2,
                                 levels = c("Sun_Dry",
                                            "Shade_Dry",
                                            "Sun_Wet",
                                            "Shade_Wet")))
  pa_inter <- pa |>
    count(sp) |>
   # filter(n >= 2) |>
    inner_join(pa, by = "sp")

  if (sun) {
   pa_inter <- pa_inter  |>
    mutate(gr = site_strata2) |>
    mutate(gr = factor(gr,
      labels = c("Sun\nDry", "Shade\nDry", "Sun\nWet", "Shade\nWet")))
  } else {
   pa_inter <- pa_inter  |>
    mutate(gr = leaf_habit) |>
    mutate(gr = factor(gr,
      labels = c("D", "E")))
  }

  if (intra) {
   pa_inter |>
    filter(n >= 2)
  } else {
   pa_inter
  }
}

#' @title Prepare data for boxplot and t-test (GLOPNET - short)
prep_gl_box_dat <- function(gl_res_csv) {
  gl <- read_csv(gl_res_csv) |>
    mutate(frac = LMAm / LMA) |>
    mutate(gr = factor(leaf_habit,
                     levels = c("D", "E")))
  gl_de <- gl |>
    filter(leaf_habit != "U")
  gl_de
}

# targets::tar_load(pa_inter_box_dat)
# targets::tar_load(pa_intra_box_dat)
# targets::tar_load(gl_box_dat)
# pa_draws <- targets::tar_read(pa_draws_am_bs_opt)
# gl_draws <- targets::tar_read(gl_draws_ams_bs)
# yml file ==================================================================

#'@ title Write letters for multiple comparisons
write_t <- function(gl_box_dat, pa_inter_box_dat, pa_intra_box_dat,
  pa_inter_de_box_dat, pa_intra_de_box_dat,
  gl_draws, pa_draws) {

# all Panama
  PA_LMA <- p_group(pa_inter_box_dat$LMA, pa_inter_box_dat$site_strata2)
  PA_LMAm <- p_group2(pa_draws, "LMAm",
                      group = "site_strata2", data = pa_inter_box_dat)
  PA_LMAs <- p_group2(pa_draws, "LMAs",
                      group = "site_strata2", data = pa_inter_box_dat)


# both sun and shade leaves are available
 # targets::tar_load(pa_intra_box_dat)
  PA_intra_LMA <- p_group(pa_intra_box_dat$LMA, pa_intra_box_dat$site_strata2)
  PA_intra_LMAm <- p_group2(pa_draws, "LMAm",
                      group = "site_strata2", data = pa_intra_box_dat)
  PA_intra_LMAs <- p_group2(pa_draws, "LMAs",
                      group = "site_strata2", data = pa_intra_box_dat)


  #targets::tar_load(pa_inter_de_box_dat)
  pa_inter_de_box_dat <- pa_inter_de_box_dat |>
     filter(!is.na(gr))
  # pa_inter_de_box_dat$gr
  PA_de_LMA <- p_group(pa_inter_de_box_dat$LMA, pa_inter_de_box_dat$leaf_habit)

  PA_de_LMAm <- p_group2(pa_draws, "LMAm",
                      group = "leaf_habit", data = pa_inter_de_box_dat)
  PA_de_LMAs <- p_group2(pa_draws, "LMAs",
                      group = "leaf_habit", data = pa_inter_de_box_dat)


# both sun and shade leaves are available (EveDec)
#  targets::tar_load(pa_intra_de_box_dat)
  PA_intra_de_LMA <- p_group(pa_intra_de_box_dat$LMA, pa_intra_de_box_dat$leaf_habit)

  PA_intra_de_LMAm <- p_group2(pa_draws, "LMAm",
                      group = "leaf_habit", data = pa_intra_de_box_dat)
  PA_intra_de_LMAs <- p_group2(pa_draws, "LMAs",
                      group = "leaf_habit", data = pa_intra_de_box_dat)

# GL
  # targets::tar_load(gl_box_dat)
  GL_LMA <- p_group(gl_box_dat$LMA, gl_box_dat$leaf_habit)
  GL_LMAm <- p_group2(gl_draws, "LMAm",
                      group = "leaf_habit", data = gl_box_dat)
  GL_LMAs <- p_group2(gl_draws, "LMAs",
                      group = "leaf_habit", data = gl_box_dat)

# leaf_habit
  GL_frac <- p_group2(gl_draws, "frac",
                      group = "leaf_habit", data = gl_box_dat)
  PA_frac <- p_group2(pa_draws, "frac",
                      group = "leaf_habit", data = pa_intra_box_dat)
  PA_frac_inter <- p_group2(pa_draws, "frac",
                      group = "leaf_habit", data = pa_inter_box_dat)

# sun shade
  PA_frac_sun <- p_group2(pa_draws, "frac",
                      group = "site_strata2", data = pa_intra_box_dat)
  PA_frac_sun_inter <- p_group2(pa_draws, "frac",
                      group = "site_strata2", data = pa_inter_box_dat)

# all
  PA_frac2 <- p_group2(pa_draws, "frac",
                      group = "leaf_habit", data = pa_inter_box_dat)
  PA_cell2 <- p_group(pa_inter_box_dat$cell_mass, pa_inter_box_dat$site_strata2)

# intra
  PA_frac3 <- p_group2(pa_draws, "frac",
                      group = "leaf_habit", data = pa_intra_box_dat)
  PA_cell3 <- p_group(pa_intra_box_dat$cell_mass, pa_intra_box_dat$site_strata2)


  output <- "yml/letters.yml"
  out <- file(paste(output), "w") # write
  writeLines(paste0("PA_inter:"),
             out,
             sep = "\n")
  writeLines(paste0("  LMA:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_LMA["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_LMA["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_LMA["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_LMA["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_LMAm["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_LMAm["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_LMAm["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_LMAm["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAs:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_LMAs["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_LMAs["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_LMAs["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_LMAs["Shade_Wet"]),
             out,
             sep = "\n")


  writeLines(paste0("PA_intra:"),
             out,
             sep = "\n")
  writeLines(paste0("  LMA:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_intra_LMA["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_intra_LMA["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_intra_LMA["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_intra_LMA["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_intra_LMAm["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_intra_LMAm["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_intra_LMAm["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_intra_LMAm["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAs:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_intra_LMAs["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_intra_LMAs["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_intra_LMAs["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_intra_LMAs["Shade_Wet"]),
             out,
             sep = "\n")


  writeLines(paste0("PA_inter_de:"),
             out,
             sep = "\n")
  writeLines(paste0("  LMA:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", PA_de_LMA["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", PA_de_LMA["E"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", PA_de_LMAm["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", PA_de_LMAm["E"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAs:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", PA_de_LMAs["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", PA_de_LMAs["E"]),
             out,
             sep = "\n")
  writeLines(paste0("PA_intra_de:"),
             out,
             sep = "\n")
  writeLines(paste0("  LMA:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", PA_intra_de_LMA["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", PA_intra_de_LMA["E"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", PA_intra_de_LMAm["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", PA_intra_de_LMAm["E"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAs:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", PA_intra_de_LMAs["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", PA_intra_de_LMAs["E"]),
             out,
             sep = "\n")

  writeLines(paste0("GL:"),
             out,
             sep = "\n")
  writeLines(paste0("  LMA:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", GL_LMA["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", GL_LMA["E"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAm:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", GL_LMAm["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", GL_LMAm["E"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAs:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", GL_LMAs["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", GL_LMAs["E"]),
             out,
             sep = "\n")

  writeLines(paste0("Frac:"),
             out,
             sep = "\n")
  writeLines(paste0("  GL:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", GL_frac["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", GL_frac["E"]),
             out,
             sep = "\n")
  writeLines(paste0("  PA:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", PA_frac["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", PA_frac["E"]),
             out,
             sep = "\n")
  writeLines(paste0("  PA_inter:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", PA_frac_inter["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", PA_frac_inter["E"]),
             out,
             sep = "\n")
  writeLines(paste0("  PA_light:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_frac_sun["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_frac_sun["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_frac_sun["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_frac_sun["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("  PA_light_inter:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_frac_sun_inter["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_frac_sun_inter["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_frac_sun_inter["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_frac_sun_inter["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("Cell:"),
             out,
             sep = "\n")
  writeLines(paste0("  PA_intra:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_cell3["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_cell3["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_cell3["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_cell3["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("  PA_all:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_cell2["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_cell2["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_cell2["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_cell2["Shade_Wet"]),
             out,
             sep = "\n")
  close(out)
  paste(output)
}

