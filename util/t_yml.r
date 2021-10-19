library(tidyverse)
library(rstan)
library(multcompView)

GL <- read_csv("./data/GL_res.csv") %>%
  mutate(frac = LMAp / LMA)
PA <- read_csv("./data/PA_res.csv") %>%
  mutate(frac = LMAp / LMA)

tmp <- read_csv("./data/PA_LH.csv") %>%
  rename(DE = LeafHabit) %>%
  mutate(DE = ifelse(DE == "evergreen", "E", DE)) %>%
  mutate(DE = ifelse(DE == "deciduous", "D", DE)) %>%
  dplyr::select(sp, DE)

PA1 <- full_join(PA, tmp, by = "sp")

GL2 <- GL %>% 
  filter(DE != "U")

PA2 <- PA1 %>%
  count(sp) %>%
 # filter(n >= 2) %>%
  inner_join(., PA1, by = "sp") %>%
  mutate(site_strata2 = "Shade_Wet") %>%
  mutate(site_strata2 = ifelse(site_strata == "DRY_CAN",
                               "Sun_Dry", site_strata2)) %>%
  mutate(site_strata2 = ifelse(site_strata == "DRY_UNDER",
                               "Shade_Dry", site_strata2)) %>%
  mutate(site_strata2 = ifelse(site_strata == "WET_CAN",
                               "Sun_Wet", site_strata2)) %>%
  mutate(site_strata2 = factor(site_strata2,
                               levels = c("Sun_Dry",
                                          "Shade_Dry",
                                          "Sun_Wet",
                                          "Shade_Wet")))

# both sun and shade leaves are available
PA3 <- PA2 %>%
  filter(n >= 2)

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

pairwise.t.test(GL2$LMA, GL2$DE, p.adjust.method = "bonferroni")

load("./rda/GL_Aps_LLs_obs.rda")
resGL <- res
GL_summary <- data.frame(summary(res)$summary)
GL <- dat
P_vec <- paste("p[", 1:nrow(GL), "]", sep = "")
GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) 

pmat_GL <- rstan::extract(res, "p")[[1]]


load("./rda/PA_Ap_LLs_opt_more_obs.rda")
pmat_PA <- rstan::extract(res, "p")[[1]]

PA <- PA %>% 
  count(sp) %>% 
 # filter(n >= 2) %>%
  inner_join(., PA1, by = "sp") %>%
  mutate(site_strata2 = "Shade_Wet") %>%
  mutate(site_strata2 = ifelse(site_strata == "DRY_CAN",
                               "Sun_Dry", site_strata2)) %>%
  mutate(site_strata2 = ifelse(site_strata == "DRY_UNDER",
                               "Shade_Dry", site_strata2)) %>%
  mutate(site_strata2 = ifelse(site_strata == "WET_CAN",
                               "Sun_Wet", site_strata2)) %>%
  mutate(site_strata2 = factor(site_strata2,
                               levels = c("Sun_Dry",
                                          "Shade_Dry",
                                          "Sun_Wet",
                                          "Shade_Wet")))
# PA
pmat_PA <- rstan::extract(res, "p")[[1]]

PA_intra <- PA3
PA_inter <- PA2 |>
  filter(!is.na(LL))

p_post <- \(pmat,
          x,
          group = c("DE", "site_strata2"),
          model = c("GL2", "PA_intra", "PA_inter")
          ){
  if (group == "DE") {
    m <- array(dim = c(2, 2, 4000))
    alpha <- 1
  } else {
    m <- array(dim = c(4, 4, 4000))
    alpha <- 6
  }
  group2 <- get(model) |>
    pull(group)
  LMA <- get(model) |>
    pull(LMA)
#  LMA |> length() |> print()
#  print(group2)
  if (model == "GL2") {
    pmat <- pmat[, GL$DE != "U"]
  } else if (model == "PA_intra") {
    pmat <- pmat[, PA_inter$n >= 2]
  }
  #pmat |> dim() |> print()
    for (i in 1:4000) {
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

    il.tri <- lower.tri(sig_m, TRUE)
    if (group == "site_strata2") {
      m2 <- matrix(sig_m[il.tri][-1], ncol = 3, nrow =3)
      m2[3, 3] <- m2[2,3]
      m2[2, 3] <- NA
      rownames(m2) <- rownames(sig_m)[2:4]
      colnames(m2) <- colnames(sig_m)[1:3]
    } else {
      m2 <- matrix(sig_m[il.tri][-1], ncol = 1, nrow = 1)
      rownames(m2) <- rownames(sig_m)[2]
      colnames(m2) <- colnames(sig_m)[1]
    }
  m2
}

p_group2 <- \(pmat,
          x,
          group = c("DE", "site_strata2"),
          model = c("GL2", "PA_intra", "PA_inter")
          ){
  moge <- p_post(pmat, x, group = group, model = model)
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

# yml file ==================================================================
# all Panama 
PA_LMA <- p_group(PA2$LMA, PA2$site_strata2)
PA_LMAp <- p_group2(pmat_PA, "LMAp",
                    group = "site_strata2", model = "PA_inter")
PA_LMAs <- p_group2(pmat_PA, "LMAs",
                    group = "site_strata2", model = "PA_inter")


# both sun and shade leaves are available
PA2_LMA <- p_group(PA3$LMA, PA3$site_strata2)
PA2_LMAp <- p_group2(pmat_PA, "LMAp", 
                    group = "site_strata2", model = "PA_intra")
PA2_LMAs <- p_group2(pmat_PA, "LMAs", 
                    group = "site_strata2", model = "PA_intra")

# GL
GL_LMA <- p_group(GL2$LMA, GL2$DE)
GL_LMAp <- p_group2(pmat_GL, "LMAp", 
                    group = "DE", model = "GL2")
GL_LMAs <- p_group2(pmat_GL, "LMAs", 
                    group = "DE", model = "GL2")

# DE
GL_frac <- p_group2(pmat_GL, "frac", 
                    group = "DE", model = "GL2")
PA_frac <- p_group2(pmat_PA, "frac", 
                    group = "DE", model = "PA_intra")
# all
PA_frac2 <- p_group2(pmat_PA, "frac", 
                    group = "site_strata2", model = "PA_inter")
PA_cell2 <- p_group(PA2$cell_mass, PA2$site_strata2)
# intra
PA_frac3 <- p_group2(pmat_PA, "frac", 
                    group = "site_strata2", model = "PA_intra")
PA_cell3 <- p_group(PA3$cell_mass, PA3$site_strata2)

#PA4 <- PA3 %>%
#  dplyr::select(sp, frac, site_strata2) %>%
#  spread(site_strata2, frac)
#
#t.test(PA4$Sun_Dry, PA4$Shade_Dry, pair = TRUE)
#t.test(PA4$Sun_Wet, PA4$Shade_Wet, pair = TRUE)

output <- "letters.yml"
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
