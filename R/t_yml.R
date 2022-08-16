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
#' @para x name of variancle (e.g., "frac", "LMAp", "LMAs")
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
  pmat <- pmat0[, id]

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
    LMAp <- log(frac * LMA)
    LMAs <- log(LMA - LMAp)
    xbar <- tapply(get(x), group2, \(x)mean(x, na.rm = TRUE))
    for (j in 1:length(xbar)) {
      for (k in 1:length(xbar)) {
        dif <- xbar[j] - xbar[k]
        m[j, k, i] <- dif
      }
    }
   }

  lwr <- apply(m,  1:2, \(x)quantile(x, 0.025 / alpha))
  upr <- apply(m,  1:2, \(x)quantile(x, 1 - 0.025 / alpha))
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

#' @title Preaprea data for boxplot and t-test (Panama - short)
prep_pa_box_dat <- function(pa_res_csv, intra = TRUE) {
  pa <- read_csv(pa_res_csv) |>
    mutate(frac = LMAp / LMA) |>
    mutate(leaf_habit = ifelse(leaf_habit == "evergreen", "E", leaf_habit)) |>
    mutate(leaf_habit = ifelse(leaf_habit == "deciduous", "D", leaf_habit))

  pa_inter <- pa |>
    count(sp) |>
   # filter(n >= 2) |>
    inner_join(pa, by = "sp") |>
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
                                            "Shade_Wet"))) |>
    mutate(gr = site_strata2) |>
    mutate(gr = factor(gr,
      labels = c("Sun\nDry", "Shade\nDry", "Sun\nWet", "Shade\nWet")))

  if (intra) {
   pa_inter |>
    filter(n >= 2)
  } else {
   pa_inter
  }
}

#' @title Preaprea data for boxplot and t-test (GLOPNET - short)
prep_gl_box_dat <- function(gl_res_csv) {
  gl <- read_csv(gl_res_csv) |>
    mutate(frac = LMAp / LMA) |>
    mutate(gr = factor(leaf_habit,
                     levels = c("D", "E")))
  gl_de <- gl |>
    filter(leaf_habit != "U")
  gl_de
}

# targets::tar_load(pa_inter_box_dat)
# targets::tar_load(pa_intra_box_dat)
#  targets::tar_load(gl_box_dat)
# targets::tar_load(fit_20_draws_PA_Ap_LLs_opt)
#  targets::tar_load(fit_7_draws_GL_Aps_LLs)
#  gl_draws <- fit_7_draws_GL_Aps_LLs
# yml file ==================================================================

#'@ title Write letters for multiple comparisons
write_t <- function(gl_box_dat, pa_inter_box_dat, pa_intra_box_dat,
  gl_draws, pa_draws) {

# all Panama

  PA_LMA <- p_group(pa_inter_box_dat$LMA, pa_inter_box_dat$site_strata2)
  PA_LMAp <- p_group2(pa_draws, "LMAp",
                      group = "site_strata2", data = pa_inter_box_dat)
  PA_LMAs <- p_group2(pa_draws, "LMAs",
                      group = "site_strata2", data = pa_inter_box_dat)


# both sun and shade leaves are available
  PA2_LMA <- p_group(pa_intra_box_dat$LMA, pa_intra_box_dat$site_strata2)
  PA2_LMAp <- p_group2(pa_draws, "LMAp",
                      group = "site_strata2", data = pa_intra_box_dat)
  PA2_LMAs <- p_group2(pa_draws, "LMAs",
                      group = "site_strata2", data = pa_intra_box_dat)


# GL
  GL_LMA <- p_group(gl_box_dat$LMA, gl_box_dat$leaf_habit)
  GL_LMAp <- p_group2(gl_draws, "LMAp",
                      group = "leaf_habit", data = gl_box_dat)
  GL_LMAs <- p_group2(gl_draws, "LMAs",
                      group = "leaf_habit", data = gl_box_dat)

# leaf_habit
  GL_frac <- p_group2(gl_draws, "frac",
                      group = "leaf_habit", data = gl_box_dat)
  PA_frac <- p_group2(pa_draws, "frac",
                      group = "leaf_habit", data = pa_intra_box_dat)


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
  writeLines(paste0("PA_SI:"),
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
  writeLines(paste0("  LMAp:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_LMAp["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_LMAp["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_LMAp["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_LMAp["Shade_Wet"]),
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


  writeLines(paste0("PA:"),
             out,
             sep = "\n")
  writeLines(paste0("  LMA:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA2_LMA["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA2_LMA["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA2_LMA["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA2_LMA["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAp:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA2_LMAp["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA2_LMAp["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA2_LMAp["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA2_LMAp["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("  LMAs:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA2_LMAs["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA2_LMAs["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA2_LMAs["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA2_LMAs["Shade_Wet"]),
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
  writeLines(paste0("  LMAp:"),
             out,
             sep = "\n")
  writeLines(paste0("    D: ", GL_LMAp["D"]),
             out,
             sep = "\n")
  writeLines(paste0("    E: ", GL_LMAp["E"]),
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
  writeLines(paste0("  PA-intra:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_frac3["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_frac3["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_frac3["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_frac3["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("  PA-all:"),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Dry: ", PA_frac2["Sun_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Dry: ", PA_frac2["Shade_Dry"]),
             out,
             sep = "\n")
  writeLines(paste0("    Sun_Wet: ", PA_frac2["Sun_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("    Shade_Wet: ", PA_frac2["Shade_Wet"]),
             out,
             sep = "\n")
  writeLines(paste0("Cell:"),
             out,
             sep = "\n")
  writeLines(paste0("  PA-intra:"),
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
  writeLines(paste0("  PA-all:"),
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

