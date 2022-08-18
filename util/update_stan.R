#' @title This creates tar_stan_mcmc_list.R
library(tidyverse)
library(jsonlite)

targets::tar_load(model_json)
targets::tar_load(model_lma_json)

model <- fromJSON(model_json)$config
model_lma <- fromJSON(model_lma_json)$config
model_lma2 <- model_lma |>
  mutate(site = ifelse(str_detect(model, "GL"), "GL", "PA"))

model2 <- full_join(model_lma2, model, by = c("site", "model", "opt"))

model_n <- nrow(model2)

model3 <- model2 |>
  mutate(fit = paste("fit", 1:model_n, sep = "_")) |>
  mutate(stan = paste0("stan/", model, ".stan")) |>
  mutate(data = ifelse(site == "GL", "gl_stan_dat", "pa_stan_dat"))

tmp <- "R/tar_stan_mcmc_list.R"
write_lines("#' Note: this list is generated via `generate_tar_stan`", tmp, append = FALSE)
write_lines("tar_stan_mcmc_list <- list(", tmp, append = TRUE)
for (i in 1:nrow(model3)) {
  write_lines("  tar_stan_mcmc(", tmp, append = TRUE)
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

  if (model3$site[i] == "GL") {
    write_lines('    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.9,
    max_treedepth = 15,
    seed = 123),',
      tmp,
      append = TRUE
    )
  } else {
    write_lines('    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.999,
    max_treedepth = 15,
    seed = 123),',
      tmp,
      append = TRUE
    )
  }
}
write_lines("NULL", tmp, append = TRUE)
write_lines("  )", tmp, append = TRUE)
