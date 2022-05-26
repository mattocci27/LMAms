# sudo singularity build singularity.sif singularity.def

targets::tar_read(gl_stan_dat)

targets::tar_read(fit_1_summary_GL_Aps_LLs)
targets::tar_read(fit_1_diagnostics_GL_Aps_LLs)
targets::tar_load(fit_1_diagnostics_GL_Aps_LLs)


fit_1_diagnostics_GL_Aps_LLs |> summary()


library(jsonlite)

model <- fromJSON("templates/model.json")$config

model_n <- nrow(model)

model <- model |>
  mutate(model2 = paste("fit", 1:model_n, sep = "_")) |>
  mutate(stan = paste0("stan/", model, ".stan")) |>
  mutate(data = ifelse(site == "GL", "gl_stan_dat", "pa_stan_dat"))


tmp <- "templates/tar_stan_mcmc.txt"
for (i in 1:nrow(model)) {
  if (i == 1) {
    write_lines("  tar_stan_mcmc(", tmp, append = FALSE)
  } else {
    write_lines("  tar_stan_mcmc(", tmp, append = TRUE)
  }
  write_lines(
    paste0("    ", model$model2[i]), tmp, ",\n",
    append = TRUE
  )
  write_lines(
    paste0('    "', model$stan[i]), tmp, '",\n',
    append = TRUE
  )
  write_lines(
    paste0("    data = ", model$data[i]), tmp, ",\n",
    append = TRUE
  )
  write_lines('    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 1,
    iter_sampling = 1,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),',
    tmp,
    append = TRUE
  )
}

model4 <- model3 |>
  mutate(hoge = paste(fit, "summary", model, sep = "_"))

