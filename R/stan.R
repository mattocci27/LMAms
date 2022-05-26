generate_gl_stan <- function(data) {
  dat <- read_csv(data) %>%
    mutate(DE = ifelse(is.na(DE), "U", DE)) %>%
    as.data.frame()

  list_dat <- list(
    N = nrow(dat),
    obs = cbind(
      log(dat[, "Aarea"] + dat[, "Rarea"]),
      log(dat[, "LL"]),
      log(dat[, "Rarea"])
    ),
    LMA = dat[, "LMA"],
    A = dat[, "Aarea"],
    LL = dat[, "LL"],
    R = dat[, "Rarea"],
    q_lim = 1,
    leaf = 1,
    dry = 1,
    gr = as.numeric(as.factor(dat$DE))
  )

  list_dat$J <- list_dat$gr %>%
    unique() %>%
    length()

  list_dat$obs2 <- list_dat$obs
  list_dat$E <- ifelse(dat$DE == "E", 1, 0)
  list_dat$U <- ifelse(dat$DE == "U", 1, 0)
  list_dat
}

generate_pa_stan <- function(data) {
  dat <- read_csv(data) %>%
    filter(!is.na(LMA)) %>%
    filter(!is.na(Aarea)) %>%
    filter(!is.na(Rarea)) %>%
    filter(!is.na(LL)) %>%
    as.data.frame() %>%
    mutate(gr = paste(site, strata) %>%
      as.factor() %>%
      as.numeric())

  q_lim <- dat %>%
    filter(strata == "UNDER") %>%
    mutate(q = Rarea / Aarea) %>%
    summarize(max(q)) %>%
    as.numeric()

  list_dat <- list(
    N = nrow(dat),
    obs = cbind(
      log(dat[, "Aarea"] + dat[, "Rarea"]),
      log(dat[, "LL"]),
      log(dat[, "Rarea"])
    ),
    LMA = dat[, "LMA"],
    LT = dat[, "LT"],
    A = dat[, "Aarea"],
    LL = dat[, "LL"],
    R = dat[, "Rarea"],
    DE = 1,
    gr = dat$gr,
    J = dat$gr %>% unique() %>% length(),
    q_lim = q_lim,
    # leaf = as.numeric(as.factor(dat$strata)),
    leaf = ifelse(dat$strata == "CAN", 1, 0),
    dry = ifelse(dat$site == "PNM", 1, 0)
  )
  list_dat
}

generate_tar_stan <- function(model, model_lma) {
  model <- fromJSON(model)$config
  # model_lma <- fromJSON("templates/model_LMA.json")$config
  # model <- fromJSON("templates/model.json")$config
  model_lma <- fromJSON(model_lma)$config
  model_lma2 <- model_lma |>
    mutate(site = ifelse(str_detect(model, "GL"), "GL", "PA"))

  model2 <- full_join(model, model_lma2, by = c("site", "model", "opt"))

  model_n <- nrow(model2)

  model3 <- model2 |>
    mutate(fit = paste("fit", 1:model_n, sep = "_")) |>
    mutate(stan = paste0("stan/", model, ".stan")) |>
    mutate(data = ifelse(site == "GL", "gl_stan_dat", "pa_stan_dat"))

  tmp <- "templates/tar_stan_mcmc.txt"
  for (i in 1:nrow(model3)) {
    if (i == 1) {
      write_lines("  tar_stan_mcmc(", tmp, append = FALSE)
    } else {
      write_lines("  tar_stan_mcmc(", tmp, append = TRUE)
    }
    write_lines(
      paste0("    ", model3$fit[i]), tmp, ",\n",
      append = TRUE
    )
    write_lines(
      paste0('    "', model3$stan[i]), tmp, '",\n',
      append = TRUE
    )
    write_lines(
      paste0("    data = ", model3$data[i]), tmp, ",\n",
      append = TRUE
    )
    write_lines('    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 20,
    iter_sampling = 20,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),',
      tmp,
      append = TRUE
    )
  }
  paste(tmp)
}
