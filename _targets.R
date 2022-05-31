library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
library(cmdstanr)
library(furrr)

source("R/data_clean.R")
source("R/stan.R")
source("R/fig_theme.R")
source("R/figs.R")
source("R/vpart.R")
source("R/mass_prop.R")
source("R/yml.R")
source("R/t_yml.R")

plan(multicore)
options(clustermq.scheduler = "multicore")

tar_option_set(packages = c(
  "tidyverse",
  "patchwork",
  "parallel",
  "janitor",
  "extrafont",
  "loo",
  "jsonlite",
  "doParallel",
  "foreach",
  "httpgd",
  "multcompView"
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
    pa_full_csv,
    prepare_pa(fiber_file, pa_file),
    format = "file"
  ),
  tar_target(
    pa_csv,
    {
      d <- read_csv(pa_full_csv)
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
  tar_target(
    settings_yml,
    "yml/settings.yml",
    format = "file"
  ),

  # stan -------------------------------------------------

  tar_target(
    gl_stan_dat,
    generate_gl_stan(read_csv(gl_csv)),
  ),
  tar_target(
    pa_stan_dat,
    generate_pa_stan(read_csv(pa_csv)),
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

  tar_target(
    loo_tbl, {
      tibble(Model = names(loo_model),
       LOOIC = sapply(loo_model, "[[", "looic"),
       elpd = sapply(loo_model, "[[", "elpd_loo"),
       N = lapply(loo_model, "[[", "pointwise") |> sapply(nrow)) |>
        mutate(site = str_split_fixed(Model, "mcmc_", 2)[,2]) |>
        mutate(site = str_split_fixed(site, "_", 2)[,1]) |>
        arrange(LOOIC) |>
        write_csv("data/loo.csv")
      paste("data/loo.csv")
    },
    format = "file"
  ),

  # random ------------------------
  tar_target(
    gl_rand_list, {
    data <- read_csv(gl_csv)
    rand_data <- tibble(null_model = 1:10)
    rand_data |>
    mutate(data = future_map(1:10, rand_fun, data, gl_stan_dat))
    }
  ),
  tar_target(
    pa_rand_list, {
    data <- read_csv(pa_csv)
    rand_data <- tibble(null_model = 1:10)
    rand_data |>
    mutate(data = future_map(1:10, rand_fun, data, pa_stan_dat))
    }
  ),
  tar_target(
    GL_Aps_LLs,
    compile_model("model/GL_Aps_LLs.stan"),
    format = "file"
  ),
  tar_target(
    gl_rand_fit,
    future_map(gl_rand_list$data, fit_rand_model, GL_Aps_LLs, 2000, 2000,
    .options = furrr_options(seed = 123))
  ),
  tar_target(
    PA_Ap_LLs_opt,
    compile_model("model/PA_Ap_LLs_opt.stan"),
    format = "file"
  ),
  tar_target(
    pa_rand_fit,
    #fit_rand_model(pa_rand_list$data[[1]], PA_Ap_LLs_opt, 1, 1)
    future_map(pa_rand_list$data, fit_rand_model, PA_Ap_LLs_opt, 2000, 2000,
    .options = furrr_options(seed = 123))
  ),

  # best model for the full data
  tar_stan_mcmc(
    fit_20,
    "stan/PA_Ap_LLs_opt.stan",
    data = generate_pa_stan(read_csv(pa_full_csv), full = TRUE),
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    seed = 123),

  tar_target(
    gl_res_csv,
    generate_gl_dat(gl_csv, fit_7_draws_GL_Aps_LLs),
    format = "file"
  ),
  tar_target(
    pa_res_csv,
    generate_pa_dat(pa_full_csv, fit_20_draws_PA_Ap_LLs_opt),
    format = "file"
  ),
  tar_target(
    gl_res_dat,
    clean_gl_res(gl_res_csv)
  ),
  tar_target(
    pa_res_dat,
    clean_pa_res(pa_res_csv)
  ),

  tar_target(
    para_tbl,
    create_para_tbl(fit_7_draws_GL_Aps_LLs, fit_20_draws_PA_Ap_LLs_opt),
    format = "file"
  ),

  tar_target(
    r_vals_yml,
    write_r2(gl_res_csv, fit_7_draws_GL_Aps_LLs,
      pa_res_csv, fit_20_draws_PA_Ap_LLs_opt),
    format = "file"
  ),

  tar_target(
    gl_long_dat,
    gen_gl_long(gl_res_csv)
  ),
  tar_target(
    pa_long_dat,
    gen_pa_long(pa_res_csv)
  ),
  tar_target(
    gl_point_plot, {
      p <- gl_point(gl_long_dat, settings_yml, r_vals_yml)
      ggsave(
        "figs/gl_point.png",
       p,
       dpi = 300,
       height = 11.4,
       width = 11.4,
       units = "cm"
      )
      # ggsave(
      #   "figs/petiole.pdf",
      #   p,
      #   device = cairo_pdf,
      #   width = 8,
      #   height = 4)
        paste0("figs/gl_point", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    pa_point_plot, {
      p <- pa_point(pa_long_dat, settings_yml, r_vals_yml)
      ggsave(
        "figs/pa_point.png",
       p,
       dpi = 300,
       height = 11.4,
       width = 11.4,
       units = "cm"
      )
      # ggsave(
      #   "figs/petiole.pdf",
      #   p,
      #   device = cairo_pdf,
      #   width = 8,
      #   height = 4)
        paste0("figs/pa_point", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    pa_point_npc_plot, {
      p <- pa_point_npc(pa_long_dat, settings_yml, r_vals_yml)
      ggsave(
        "figs/pa_point_npc.png",
       p,
       dpi = 300,
       height = 11.4,
       width = 11.4,
       units = "cm"
      )
      paste0("figs/pa_point_npc", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    pa_point_ll_plot, {
      p <- pa_point_ll(pa_res_csv, settings_yml, r_vals_yml)
      ggsave(
        "figs/pa_point_ll.png",
       p,
       dpi = 300,
       height = 6.7,
       width = 6.7,
       units = "cm"
      )
      paste0("figs/pa_point_ll", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    vpart_plot, {
      p <- vpart_bar(gl_res_csv, pa_res_csv)
      ggsave(
        "figs/vpart.png",
       p,
       dpi = 300,
       height = 6,
       width = 10,
       units = "cm"
      )
      paste0("figs/vpart", c(".png"))
    },
    format = "file"
  ),
  # mass prop ----------------------------------------------------
  tar_target(
    mass_obs_dat,
    gen_mass_point_dat(
      gl_res_csv, pa_res_csv,
      fit_7_summary_GL_Aps_LLs, fit_20_summary_PA_Ap_LLs_opt
    )
  ),
  tar_target(
    gl_mass_prop,
    mass_prop_sim(
      read_csv(gl_res_csv), fit_7_summary_GL_Aps_LLs, n_sim = 1000)
  ),
  tar_target(
    sun_mass_prop,
    mass_prop_sim(read_csv(pa_res_csv) |> filter(strata == "CAN"),
      fit_20_summary_PA_Ap_LLs_opt,
      gl = FALSE, n_sim = 1000,
      site = "Sun"
    )
  ),
  tar_target(
    shade_mass_prop,
    mass_prop_sim(read_csv(pa_res_csv) |> filter(strata != "CAN"),
      fit_20_summary_PA_Ap_LLs_opt,
      gl = FALSE, n_sim = 1000,
      site = "Shade"
    )
  ),
  tar_target(
    shade_mass_prop_mv,
    mass_prop_sim_mv(read_csv(pa_res_csv) |> filter(strata != "CAN"),
      fit_20_summary_PA_Ap_LLs_opt,
      n_sim = 1000,
      site = "Shade"
    )
  ),
  tar_target(
    mass_prop_plot, {
      p <- mass_prop_point(mass_obs_dat,
        gl_mass_prop, sun_mass_prop, shade_mass_prop)
      ggsave(
        "figs/mass_prop.png",
       p,
       dpi = 300,
       height = 6,
       width = 6,
       units = "cm"
      )
      paste0("figs/mass_prop", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    mass_prop_mv_plot, {
      p <- mass_prop_point(mass_obs_dat,
        gl_mass_prop, sun_mass_prop, shade_mass_prop_mv)
      ggsave(
        "figs/mass_prop_mv.png",
       p,
       dpi = 300,
       height = 6,
       width = 6,
       units = "cm"
      )
      paste0("figs/mass_prop_mv", c(".png"))
    },
    format = "file"
  ),
  # boxplot ------------------------------------------------------
  tar_target(
    gl_box_dat,
    prep_gl_box_dat(gl_res_csv)
  ),
  tar_target(
    pa_inter_box_dat,
    prep_pa_box_dat(pa_res_csv, pa_lh_csv, intra = FALSE)
  ),
  tar_target(
    pa_intra_box_dat,
    prep_pa_box_dat(pa_res_csv, pa_lh_csv, intra = TRUE)
  ),
  tar_target(
    letters_yml,
    write_t(gl_box_dat, pa_inter_box_dat, pa_intra_box_dat, fit_7_draws_GL_Aps_LLs, fit_20_draws_PA_Ap_LLs_opt),
    format = "file"
  ),
  tar_target(
    gl_box_list,
    prep_gl_box_list(gl_res_dat, letters_yml)
  ),
  tar_target(
    pa_box_trim_list,
    prep_pa_box_list(pa_intra_box_dat, letters_yml)
  ),
  tar_target(
    pa_box_list,
    prep_pa_box_list(pa_inter_box_dat, letters_yml, trim = FALSE)
  ),
  tar_target(
    box_main_plot, {
      p <- box_main(gl_box_list, pa_box_trim_list, settings_yml)
      ggsave(
        "figs/box_main.png",
       p,
       dpi = 300,
       height = 11.7,
       width = 11.7,
       units = "cm"
      )
      paste0("figs/box_main", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    box_frac_plot, {
      p <- box_frac(gl_box_dat, pa_intra_box_dat, settings_yml, letters_yml)
      ggsave(
        "figs/box_frac.png",
       p,
       dpi = 300,
       height = 6,
       width = 10,
       units = "cm"
      )
      paste0("figs/box_frac", c(".png"))
    },
    format = "file"
  ),
  tar_render(
    report,
    "report.Rmd"
  )

)

