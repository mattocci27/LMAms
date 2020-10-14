library(tidyverse)
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

pairwise.t.test(log(GL2$LMA), GL2$DE)

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

p_group <- function(LMA, group) {
  moge <- pairwise.t.test(LMA, group)$p.value

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

# all
PA_LMA <- p_group(PA2$LMA, PA2$site_strata2)
PA_LMAp <- p_group(PA2$LMAp, PA2$site_strata2)
PA_LMAs <- p_group(PA2$LMAs, PA2$site_strata2)

# both sun and shade leaves are available
PA2_LMA <- p_group(PA3$LMA, PA3$site_strata2)
PA2_LMAp <- p_group(PA3$LMAp, PA3$site_strata2)
PA2_LMAs <- p_group(PA3$LMAs, PA3$site_strata2)

GL_LMA <- p_group(GL2$LMA, GL2$DE)
GL_LMAp <- p_group(GL2$LMAp, GL2$DE)
GL_LMAs <- p_group(GL2$LMAs, GL2$DE)

GL_frac <- p_group(GL2$frac, GL2$DE)
PA_frac <- p_group(PA3$frac, PA3$DE)
# all
PA_frac2 <- p_group(PA2$frac, PA2$site_strata2)
# intra
PA_frac3 <- p_group(PA3$frac, PA3$site_strata2)

PA4 <- PA3 %>%
  dplyr::select(sp, frac, site_strata2) %>%
  spread(site_strata2, frac)

t.test(PA4$Sun_Dry, PA4$Shade_Dry, pair = TRUE)
t.test(PA4$Sun_Wet, PA4$Shade_Wet, pair = TRUE)


output <- "letters.yml"
out <- file(paste(output), "w") # write
writeLines(paste0("PA:"),
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


writeLines(paste0("PA-SI:"),
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
close(out)
