library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
library(cmdstanr)

source("R/data_clean.R")
source("R/stan.R")

options(clustermq.scheduler = "multicore")

tar_option_set(packages = c(
  "tidyverse",
  "patchwork",
  "parallel",
  "janitor",
  "extrafont",
  "loo",
  "jsonlite"
))

# check if it's inside a container
# if (file.exists("/.dockerenv") | file.exists("/.singularity.d/startscript")) {
#   Sys.setenv(CMDSTAN = "/opt/cmdstan/cmdstan-2.29.0")
#   set_cmdstan_path("/opt/cmdstan/cmdstan-2.29.0")
# }

cmdstan_version()

list(
  # data cleaning ----------------------------------
  tar_target(
    fiber_file,
    "data-raw/fiber_analysis.csv",
    format = "file"
  ),
  tar_target(
    gl_file,
    "data-raw/nature02403-s2.csv",
    format = "file"
  ),
  tar_target(
    pa_file,
    "data-raw/leaf_traits.csv",
    format = "file"
  ),
  tar_target(
    leafhabit_file,
    "data-raw/Osnas2018_S1.csv",
    format = "file"
  ),
  tar_target(
    model_json,
    "templates/model.json",
    format = "file"
  ),
  tar_target(
    model_lma_json,
    "templates/model_LMA.json",
    format = "file"
  ),
  tar_target(
    gl_csv,
    prepare_gl(gl_file),
    format = "file"
  ),
  tar_target(
    pa_more_csv,
    prepare_pa(fiber_file, pa_file),
    format = "file"
  ),
  tar_target(
    pa_csv,
    {
      d <- read_csv("data/PA_data_more.csv")
      d |>
        filter(!is.na(LD)) |>
        filter(!is.na(LT)) |>
        write_csv("data/PA_data.csv")
      paste("data/PA_data.csv")
    },
    format = "file"
  ),
  tar_target(
    pa_lh_csv,
    prepare_leafhabit(pa_file, leafhabit_file),
    format = "file"
  ),
  tar_target(
    tar_stan_txt,
    generate_tar_stan(model_json, model_lma_json),
    format = "file"
  ),

  # stan -------------------------------------------------

  tar_target(
    gl_stan_dat,
    generate_gl_stan(gl_csv),
  ),
  tar_target(
    pa_stan_dat,
    generate_pa_stan(pa_csv),
  ),
  tar_stan_mcmc(
    fit_1,
    "stan/GL_LMA.stan",
    data = gl_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_2,
    "stan/PA_LMA.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_3,
    "stan/PA_LMA_opt.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_4,
    "stan/GL_Ap_LLs.stan",
    data = gl_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_5,
    "stan/GL_Aps_LLps.stan",
    data = gl_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_6,
    "stan/GL_Ap_LLps.stan",
    data = gl_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 3000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_7,
    "stan/GL_Aps_LLs.stan",
    data = gl_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_8,
    "stan/PA_Ap_LLs.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_9,
    "stan/PA_Aps_LLps.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_10,
    "stan/PA_Ap_LLps.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_11,
    "stan/PA_Aps_LLs.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_12,
    "stan/PA_Ap_LLs_opt.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_13,
    "stan/PA_Aps_LLps_opt.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_14,
    "stan/PA_Ap_LLps_opt.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_15,
    "stan/PA_Aps_LLs_opt.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_16,
    "stan/PA_Ap_LDs.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_17,
    "stan/PA_Ap_LDps.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_18,
    "stan/PA_Ap_LDs_opt.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_stan_mcmc(
    fit_19,
    "stan/PA_Ap_LDps_opt.stan",
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),
  tar_target(
    loo_model,
    mclapply(
      list(
        fit_1_mcmc_GL_LMA    = fit_1_mcmc_GL_LMA,
        fit_2_mcmc_PA_LMA    = fit_2_mcmc_PA_LMA,
        fit_3_mcmc_PA_LMA_opt = fit_3_mcmc_PA_LMA_opt,
        fit_4_mcmc_GL_Ap_LLs  = fit_4_mcmc_GL_Ap_LLs,
        fit_5_mcmc_GL_Aps_LLps = fit_5_mcmc_GL_Aps_LLps,
        fit_6_mcmc_GL_Ap_LLps = fit_6_mcmc_GL_Ap_LLps,
        fit_7_mcmc_GL_Aps_LLs = fit_7_mcmc_GL_Aps_LLs,
        fit_8_mcmc_PA_Ap_LLs  = fit_8_mcmc_PA_Ap_LLs,
        fit_9_mcmc_PA_Aps_LLps = fit_9_mcmc_PA_Aps_LLps,
        fit_10_mcmc_PA_Ap_LLps = fit_10_mcmc_PA_Ap_LLps,
        fit_11_mcmc_PA_Aps_LLs = fit_11_mcmc_PA_Aps_LLs,
        fit_12_mcmc_PA_Ap_LLs_opt = fit_12_mcmc_PA_Ap_LLs_opt,
        fit_13_mcmc_PA_Aps_LLps_opt = fit_13_mcmc_PA_Aps_LLps_opt,
        fit_14_mcmc_PA_Ap_LLps_opt = fit_14_mcmc_PA_Ap_LLps_opt,
        fit_15_mcmc_PA_Aps_LLs_opt = fit_15_mcmc_PA_Aps_LLs_opt,
        fit_16_mcmc_PA_Ap_LDs = fit_16_mcmc_PA_Ap_LDs,
        fit_17_mcmc_PA_Ap_LDps = fit_17_mcmc_PA_Ap_LDps,
        fit_18_mcmc_PA_Ap_LDs_opt = fit_18_mcmc_PA_Ap_LDs_opt,
        fit_19_mcmc_PA_Ap_LDps_opt = fit_19_mcmc_PA_Ap_LDps_opt
        ),
    \(x)x$loo(cores = parallel::detectCores())
    )
  ),
  tar_render(
    report,
    "report.Rmd"
  )

  # generated by tar_stan_txt

  # tar_target(
  #   petiole_plot, {
  #     p <- petiole_point(yaku_sp)
  #     ggsave(
  #       "figs/petiole.png",
  #       p,
  #       dpi = 300,
  #       width = 8,
  #       height = 4
  #     )
  #     ggsave(
  #       "figs/petiole.pdf",
  #       p,
  #       device = cairo_pdf,
  #       width = 8,
  #       height = 4)
  #       paste0("figs/petiole", c(".png", ".pdf"))
  #   },
  #   format = "file"
  # ),
)

