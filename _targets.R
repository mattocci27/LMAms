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

tar_option_set(
  garbage_collection = TRUE,
  memory = "transient"
)

# check if it's inside a container
if (file.exists("/.dockerenv") | file.exists("/.singularity.d/startscript")) {
  Sys.setenv(CMDSTAN = "/opt/cmdstan/cmdstan-2.29.2")
  set_cmdstan_path("/opt/cmdstan/cmdstan-2.29.2")
}

cmdstan_version()

# data cleaning ----------------------------------
# this list can be removed later
raw_data_list <- list(
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
    gl_csv_raw,
    prepare_gl(gl_file),
    format = "file"
  ),
  tar_target(
    pa_full_csv_raw,
    prepare_pa(fiber_file, pa_file, leafhabit_file),
    format = "file"
  )
)

# main analysis ----------------------------------
main_list <- list(
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
    "data/gl_data.csv",
    format = "file"
  ),
  tar_target(
    pa_full_csv,
    "data/pa_data_full.csv",
    format = "file"
  ),
  tar_target(
    pa_csv,
    {
      d <- read_csv(pa_full_csv)
      d |>
        filter(!is.na(LD)) |>
        filter(!is.na(LT)) |>
        write_csv("data/pa_data.csv")
      paste("data/pa_data.csv")
    },
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
  tar_target(
    pa_stan_dat_full,
    generate_pa_stan(read_csv(pa_full_csv), full = TRUE),
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
    adapt_delta = 0.9,
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
    adapt_delta = 0.9,
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
    adapt_delta = 0.9,
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
    adapt_delta = 0.9,
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
    adapt_delta = 0.9,
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
    adapt_delta = 0.9,
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
    adapt_delta = 0.9,
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
    adapt_delta = 0.999,
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
    adapt_delta = 0.999,
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
    adapt_delta = 0.999,
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
    adapt_delta = 0.9,
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
    adapt_delta = 0.9,
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
    adapt_delta = 0.999,
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
    adapt_delta = 0.999,
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
    adapt_delta = 0.9,
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
    adapt_delta = 0.999,
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
    adapt_delta = 0.999,
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
    adapt_delta = 0.999,
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
    adapt_delta = 0.999,
    max_treedepth = 15,
    seed = 123),

  tar_target(
    loo_model,
    lapply(
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

  # tar_stan_mcmc(
  #   fit_beta,
  #   "stan/GL_beta.stan",
  #   data = gl_stan_dat,
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   adapt_delta = 0.99,
  #   max_treedepth = 15,
  #   seed = 123),

  # tar_stan_mcmc(
  #   fit_h,
  #   "stan/GL_h.stan",
  #   data = gl_stan_dat,
  #   refresh = 0,
  #   chains = 4,
  #   parallel_chains = getOption("mc.cores", 4),
  #   iter_warmup = 2000,
  #   iter_sampling = 2000,
  #   adapt_delta = 0.9,
  #   max_treedepth = 15,
  #   seed = 123),

  tar_target(
    model_selection_csv,
    write_model_selction(loo_tbl),
    format = "file"
  ),

  tar_target(
    para_yml,
    write_para_yml(fit_7_summary_GL_Aps_LLs, fit_20_summary_PA_Ap_LLs_opt,
      gl_res_csv, pa_res_csv),
    format = "file"
  ),
  tar_target(
    var_yml,
    write_var_yml(fit_7_draws_GL_Aps_LLs, gl_res_dat, fit_20_draws_PA_Ap_LLs_opt, pa_res_dat),
    format = "file"
  ),

  # random ------------------------
  tar_target(
    gl_rand_list, {
    data <- read_csv(gl_csv)
    rand_data <- tibble(null_model = 1:10)
    rand_data |>
    mutate(data = future_map(1:10, rand_fun, data, gl_stan_dat,
      .options = furrr_options(seed = 123)))
    }
  ),
  tar_target(
    pa_rand_list, {
    data <- read_csv(pa_full_csv)
    rand_data <- tibble(null_model = 1:10)
    rand_data |>
    mutate(data = future_map(1:10, rand_fun, data, pa_stan_dat_full,
      .options = furrr_options(seed = 123)))
    }
  ),
  tar_target(
    GL_Aps_LLs,
    compile_model("stan/GL_Aps_LLs.stan"),
    format = "file"
  ),
  tar_target(
    gl_rand_fit,
    future_map(gl_rand_list$data, fit_rand_model, GL_Aps_LLs, 2000, 2000, 0.9,
    .options = furrr_options(seed = 123))
  ),
  tar_target(
    PA_Ap_LLs_opt,
    compile_model("stan/PA_Ap_LLs_opt.stan"),
    format = "file"
  ),
  tar_target(
    pa_rand_fit,
    #fit_rand_model(pa_rand_list$data[[1]], PA_Ap_LLs_opt, 1, 1)
    future_map(pa_rand_list$data, fit_rand_model, PA_Ap_LLs_opt, 2000, 2000, 0.9,
    .options = furrr_options(seed = 123))
  ),
  tar_target(
    gl_rand_check,
    tibble(rhat = sapply(gl_rand_fit, \(x) x$summary |> filter(rhat > 1.05) |> nrow()),
      div = sapply(gl_rand_fit, \(x) x$diagnostics[,,2] |> apply(1, sum) |> sum()),
      sim_id = 1:10,
      data = "GLOPNET")
  ),
  tar_target(
    pa_rand_check,
    tibble(rhat = sapply(pa_rand_fit, \(x) x$summary |> filter(rhat > 1.05) |> nrow()),
      div = sapply(pa_rand_fit, \(x) x$diagnostics[,,2] |> apply(1, sum) |> sum()),
      sim_id = 1:10,
      data = "Panama")
  ),
  tar_target(
    rand_csv, {
      bind_rows(gl_rand_check, pa_rand_check) |>
      write_csv("data/rand.csv")
      paste("data/rand.csv")
    },
    format = "file"
  ),

  tar_target(
    gl_rand_sig, {
      tmp <- NULL
      for (i in 1:10) {
        tmp <- bind_rows(tmp,
        list("a0", "ap", "as", "b0", "bs", "g0", "gp", "gs") |>
        map_dfr(rand_summary, gl_rand_fit, i))
        }
      tmp
    }
  ),
  tar_target(
    pa_rand_sig, {
      tmp <- NULL
      for (i in 1:10) {
        tmp <- bind_rows(tmp,
        list("a0", "ap", "b0", "bs", "g0", "gp", "gs") |>
        map_dfr(rand_summary, pa_rand_fit, i))
        }
      tmp
    }
  ),

  tar_target(
    coef_rand_gl_plot, {
      p <- coef_rand(gl_rand_sig, gl_rand_check, site = "GLOPNET")
      ggsave(
        "figs/coef_rand.png",
       p,
       dpi = 300,
       height = 15,
       width = 15,
       units = "cm"
      )
        paste0("figs/coef_rand", c(".png"))
    },
    format = "file"
  ),

  tar_target(
    coef_rand_pa_plot, {
      p <- coef_rand(pa_rand_sig, pa_rand_check, site = "Panama")
      ggsave(
        "figs/coef_rand_pa.png",
       p,
       dpi = 300,
       height = 15,
       width = 15,
       units = "cm"
      )
        paste0("figs/coef_rand_pa", c(".png"))
    },
    format = "file"
  ),

  # best model for the full data
  tar_stan_mcmc(
    fit_20,
    "stan/PA_Ap_LLs_opt.stan",
    data = pa_stan_dat_full,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.999,
    max_treedepth = 15,
    seed = 123),

  tar_target(
    gl_res_csv,
    generate_gl_dat(gl_csv, fit_7_draws_GL_Aps_LLs),
    format = "file"
  ),

  tar_target(
    pa_res_csv,
    generate_pa_dat(pa_full_csv, pa_csv, fit_20_draws_PA_Ap_LLs_opt),
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
    hypo_plot, {
      p <- hypo_point(para_yml)
      ggsave(
        "figs/hypo.png",
       p,
       dpi = 300,
       height = 5,
       width = 16,
       units = "cm"
      )
        paste0("figs/hypo", c(".png"))
    },
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
    gl_point_np_plot, {
      p <- gl_point_np(gl_long_dat, settings_yml, r_vals_yml)
      ggsave(
        "figs/gl_point_np.png",
       p,
       dpi = 300,
       height = 7.6,
       width = 11.4,
       units = "cm"
      )
        paste0("figs/gl_point_np", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    gl_point_np2_plot, {
      p <- gl_point_np2(gl_long_dat, settings_yml, r_vals_yml)
      ggsave(
        "figs/gl_point_np2.png",
       p,
       dpi = 300,
       height = 11.4,
       width = 11.4,
       units = "cm"
      )
        paste0("figs/gl_point_np2", c(".png"))
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
    pa_point_par_ll_plot, {
      p <- pa_point_par_ll(pa_res_csv, settings_yml, r_vals_yml)
      ggsave(
        "figs/pa_point_par_ll.png",
       p,
       dpi = 300,
       height = 6.7,
       width = 6.7,
       units = "cm"
      )
      ggsave(
        "figs/pa_point_par_ll.pdf",
        p,
        height = 6.7,
        width = 6.7,
        units = "cm"
        )
        paste0("figs/pa_point_par_ll", c(".png", ".pdf"))
    },
    format = "file"
  ),
  tar_target(
    ps_point_plot, {
      p <- ps_point(gl_res_dat, pa_res_dat, settings_yml, r_vals_yml)
      ggsave(
        "figs/ps_point.png",
       p,
       dpi = 300,
       height = 7,
       width = 11.4,
       units = "cm"
      )
        paste0("figs/ps_point", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    pair_frac_plot, {
      p <- pair_frac_line(pa_res_dat)
      ggsave(
        "figs/pair_frac.png",
       p,
       dpi = 300,
       height = 7,
       width = 7,
       units = "cm"
      )
        paste0("figs/pair_frac", c(".png"))
    },
    format = "file"
  ),
  tar_target(
    pair_lma_plot, {
      p <- pair_lma_line(pa_res_dat)
      ggsave(
        "figs/pair_lma.png",
       p,
       dpi = 300,
       height = 7,
       width = 11.4,
       units = "cm"
      )
        paste0("figs/pair_lma", c(".png"))
    },
    format = "file"
  ),




  tar_target(
    ps_point_95_ci_plot, {
      p <- ps_point(gl_res_dat, pa_res_dat, settings_yml, r_vals_yml, ci = TRUE)
      ggsave(
        "figs/ps_point_95ci.png",
       p,
       dpi = 300,
       height = 7,
       width = 11.4,
       units = "cm"
      )
        paste0("figs/ps_point_95ci", c(".png"))
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
  tar_target(
    vpart_intra_plot, {
      p <- vpart_bar(gl_res_csv, pa_res_csv, intra = TRUE)
      ggsave(
        "figs/vpart_intra.png",
       p,
       dpi = 300,
       height = 6,
       width = 10,
       units = "cm"
      )
      paste0("figs/vpart_intra", c(".png"))
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
    mass_prop_grad_ap,
    mass_prop_sim_grad(gl_res_csv, fit_7_summary_GL_Aps_LLs,
                     ap = c(0.1, 0.5, 1.0),
                     as = get_mean(fit_7_summary_GL_Aps_LLs, "as") |> rep(3),
                     n_sim = 1000)
  ),

  tar_target(
    mass_prop_grad_as,
    mass_prop_sim_grad(gl_res_csv, fit_7_summary_GL_Aps_LLs,
                     ap = get_mean(fit_7_summary_GL_Aps_LLs, "ap") |> rep(3),
                     as = c(-0.5, 0, 0.5),
                     n_sim = 1000)
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
  tar_target(
    mass_prop_comp_plot, {
      p <- mass_prop_comp_point(shade_mass_prop_mv, shade_mass_prop)
      ggsave(
        "figs/mass_prop_comp.png",
       p,
       dpi = 300,
       height = 6,
       width = 6,
       units = "cm"
      )
      paste0("figs/mass_prop_comp", c(".png"))
    },
    format = "file"
  ),

  tar_target(
    mass_sim_plot, {
      p <- mass_sim_point(mass_prop_grad_ap, mass_prop_grad_as)
      ggsave(
        "figs/mass_prop_sim.png",
       p,
       dpi = 300,
       height = 5,
       width = 10,
       units = "cm"
      )
      paste0("figs/mass_prop_sim", c(".png"))
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
    prep_pa_box_dat(pa_res_csv, intra = FALSE)
  ),
  tar_target(
    pa_intra_box_dat,
    prep_pa_box_dat(pa_res_csv, intra = TRUE)
  ),
  tar_target(
    pa_inter_de_box_dat,
    prep_pa_box_dat(pa_res_csv, intra = FALSE, sun = FALSE)
  ),
  tar_target(
    pa_intra_de_box_dat,
    prep_pa_box_dat(pa_res_csv, intra = TRUE, sun = FALSE)
  ),
  tar_target(
    letters_yml,
    write_t(gl_box_dat, pa_inter_box_dat, pa_intra_box_dat,
      pa_inter_de_box_dat, pa_intra_de_box_dat,
      fit_7_draws_GL_Aps_LLs, fit_20_draws_PA_Ap_LLs_opt),
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
    box_inter_plot, {
      p <- box_inter(pa_box_list, settings_yml)
      ggsave(
        "figs/box_inter.png",
       p,
       dpi = 300,
       height = 6,
       width = 11.7,
       units = "cm"
      )
      paste0("figs/box_inter", c(".png"))
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

append(raw_data_list, main_list)
