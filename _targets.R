library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
library(cmdstanr)

source("R/data_clean.R")

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
        write_csv("data/PA_data_more.csv")
      paste("data/PA_data_more.csv")
    },
    format = "file"
  ),
  tar_target(
    pa_lh_csv,
    prepare_leafhabit(pa_file, leafhabit_file),
    format = "file"
  )

  # stan -------------------------------------------------




  # tar_stan_mcmc(
  #   fit_sim_sma,
  #   "stan/test.stan",
  #   data = create_dummy_sma_data(300),
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   adapt_delta = 0.99,
  #   max_treedepth = 15,
  #   seed = 123
  #  ),

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
