get_LMA <- function(n) {
    PA_summary <- rand_res$summary[[n]]
    dat_r <- rand_res$data[[n]]
    PA <- dat %>%
      mutate(LMA = dat_r$LMA,
             LL = dat_r$LL,
             Aarea = dat_r$A,
             Rarea = dat_r$R) %>%
      as_data_frame %>%
      mutate(LMAp = PA_summary[str_detect(rownames(PA_summary),
                                "log_LMAp"), "X50."] %>%  exp) %>%
      mutate(LMAs = PA_summary[str_detect(rownames(PA_summary),
                                "log_LMAs"), "X50."] %>%  exp) %>%
      mutate(preLL = PA_summary[str_detect(rownames(PA_summary),
                                "mu2"), "X50."] %>%  exp) %>%
      mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
      mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
      mutate(site_strata = paste(site2, strata, sep = "_"))
   
      PA
}


LMA_dat <- data_frame(n = 1:3) %>% 
    group_by(n) %>%
    nest %>%
    transmute(res = map(1:3, get_LMA)) %>%
    mutate(n = 1:3) %>%
    unnest %>%
    group_by(sp_site_strata, sp, site2, site_strata) %>%
    summarize(LMA = mean(LMA),
              LMAp = mean(LMAp),
              LMAs = mean(LMAs),
              preLL = mean(preLL),
              LL = mean(LL),
              Aarea = mean(Aarea),
              Rarea = mean(Rarea)
             )

LMA_dat

LL_dat <- LMA_dat %>%
  mutate(site_strata = factor(site_strata,
          levels = c("WET_CAN", "DRY_CAN", "WET_UNDER", "DRY_UNDER"))) %>%
  mutate(gr = factor(site_strata,
    labels = c("Sun-Wet",
               "Sun-Dry",
               "Shade-Wet",
               "Shade-Dry"
                      ))

