library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
library(cmdstanr)
library(furrr)
library(jsonlite)

source("R/data_clean.R")
source("R/stan.R")
source("R/fig_theme.R")
source("R/figs.R")
source("R/vpart.R")
source("R/mass_prop.R")
source("R/yml.R")
source("R/t_yml.R")
# source("R/tar_stan_mcmc_list.R")

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
  "multcompView",
  "quarto"
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
  tar_target(
    stan_names,
    generate_stan_names(model_json, model_lma_json)
  ),

  tar_stan_mcmc(
    gl,
    generate_stan_names("templates/model.json",
      "templates/model_LMA.json")$gl_stan_names,
    data = gl_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = 1,
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.9,
    max_treedepth = 15,
    seed = 123,
    return_draws = TRUE,
    return_diagnostics = TRUE,
    return_summary = TRUE,
    summaries = list(
      mean = ~mean(.x),
      sd = ~sd(.x),
      mad = ~mad(.x),
      ~posterior::quantile2(.x, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)),
      posterior::default_convergence_measures()
      )
  ),
  tar_stan_mcmc(
    pa,
    generate_stan_names("templates/model.json",
      "templates/model_LMA.json")$pa_stan_names,
    data = pa_stan_dat,
    refresh = 0,
    chains = 4,
    parallel_chains = 1,
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.999,
    max_treedepth = 15,
    seed = 123,
    return_draws = TRUE,
    return_diagnostics = TRUE,
    return_summary = TRUE,
    summaries = list(
      mean = ~mean(.x),
      sd = ~sd(.x),
      mad = ~mad(.x),
      ~posterior::quantile2(.x, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)),
      posterior::default_convergence_measures()
      )
  ),
  tar_target(
    loo_gl,
    lapply(
      list(
        gl_mcmc_GL_LMA    = gl_mcmc_GL_LMA,
        gl_mcmc_GL_Ap_LLs  = gl_mcmc_GL_Ap_LLs,
        gl_mcmc_GL_Aps_LLps = gl_mcmc_GL_Aps_LLps,
        gl_mcmc_GL_Ap_LLps = gl_mcmc_GL_Ap_LLps,
        gl_mcmc_GL_Aps_LLs = gl_mcmc_GL_Aps_LLs
        ),
    \(x)x$loo(cores = parallel::detectCores())
    )
  ),

  tar_target(
    loo_pa,
    lapply(
      list(
        pa_mcmc_PA_LMA    = pa_mcmc_PA_LMA,
        pa_mcmc_PA_LMA_opt = pa_mcmc_PA_LMA_opt,
        pa_mcmc_PA_Ap_LLs  = pa_mcmc_PA_Ap_LLs,
        pa_mcmc_PA_Aps_LLps = pa_mcmc_PA_Aps_LLps,
        pa_mcmc_PA_Ap_LLps = pa_mcmc_PA_Ap_LLps,
        pa_mcmc_PA_Aps_LLs = pa_mcmc_PA_Aps_LLs,
        pa_mcmc_PA_Ap_LLs_opt = pa_mcmc_PA_Ap_LLs_opt,
        pa_mcmc_PA_Aps_LLps_opt = pa_mcmc_PA_Aps_LLps_opt,
        pa_mcmc_PA_Ap_LLps_opt = pa_mcmc_PA_Ap_LLps_opt,
        pa_mcmc_PA_Aps_LLs_opt = pa_mcmc_PA_Aps_LLs_opt,
        pa_mcmc_PA_Ap_LDs = pa_mcmc_PA_Ap_LDs,
        pa_mcmc_PA_Ap_LDps = pa_mcmc_PA_Ap_LDps,
        pa_mcmc_PA_Ap_LDs_opt = pa_mcmc_PA_Ap_LDs_opt,
        pa_mcmc_PA_Ap_LDps_opt = pa_mcmc_PA_Ap_LDps_opt
        ),
    \(x)x$loo(cores = parallel::detectCores())
    )
  ),

  tar_map(
    values = list(diagnostics = rlang::syms(
    generate_stan_names("templates/model.json",
      "templates/model_LMA.json")$diagnostics_names)),
    tar_target(div_check_list, div_check(diagnostics))
  ),

  tar_map(
    values = list(summary = rlang::syms(
    generate_stan_names("templates/model.json",
      "templates/model_LMA.json")$summary_names)),
    tar_target(summary_rhat, {
      summary |> filter(rhat > 1.05)
    })
  ),

  tar_target(
    loo_tbl, {
      loo_model <- append(loo_gl, loo_pa)
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

  tar_target(
    model_selection_csv,
    write_model_selction(loo_tbl),
    format = "file"
  ),

  tar_target(
    para_yml,
    write_para_yml(gl_summary_GL_Aps_LLs, pa_summary_PA_Ap_LLs_opt,
      gl_res_csv, pa_res_csv),
    format = "file"
  ),
  tar_target(
    var_yml,
    write_var_yml(gl_draws_GL_Aps_LLs, gl_res_dat, pa_draws_PA_Ap_LLs_opt, pa_res_dat),
    format = "file"
  ),

  tar_target(
    gl_para_all_csv,
    my_write_csv(gl_summary_GL_Aps_LLs, "data/gl_para_all.csv"),
    format = "file"
  ),
  tar_target(
    pa_para_all_csv,
    my_write_csv(pa_summary_PA_Ap_LLs_opt, "data/pa_para_all.csv"),
    format = "file"
  ),

  # simluation ------------------------

  # use `sim_rep` as `pattern`
  tar_target(sim_rep, seq(1, 10)),

  tar_target(
    GL_Aps_LLs,
    compile_model("stan/GL_Aps_LLs.stan"),
    format = "file"
  ),
  tar_target(
    PA_Ap_LLs_opt,
    compile_model("stan/PA_Ap_LLs_opt.stan"),
    format = "file"
  ),

  tar_target(
    gl_sim_data,
    generate_sim_data(data = read_csv(gl_csv), gl = TRUE),
    pattern = map(sim_rep)
  ),
  tar_target(
    pa_sim_data,
    generate_sim_data(data = read_csv(pa_csv), gl = FALSE),
    pattern = map(sim_rep)
  ),
  tar_target(
    gl_sim_summary,
    command = fit_sim_model(
      gl_sim_data,
      GL_Aps_LLs,
      iter_warmup = 2000,
      iter_sampling = 2000,
      adapt_delta = 0.999,
      max_treedepth = 15,
      parallel_chains = 1,
      seed = 123),
    pattern = map(gl_sim_data)
  ),
  tar_target(
    pa_sim_summary,
    command = fit_sim_model(
      pa_sim_data,
      PA_Ap_LLs_opt,
      iter_warmup = 2000,
      iter_sampling = 2000,
      adapt_delta = 0.999,
      max_treedepth = 15,
      parallel_chains = 1,
      seed = 123),
    pattern = map(pa_sim_data)
  ),
  tar_target(
    gl_para_summary,
    extract_sim_summary(gl_sim_summary)
  ),
  tar_target(
    pa_para_summary,
    extract_sim_summary(pa_sim_summary)
  ),
  tar_target(
    gl_sim_diagnostics,
    extract_sim_diagnostics(gl_sim_summary)
  ),
  tar_target(
    pa_sim_diagnostics,
    extract_sim_diagnostics(pa_sim_summary)
  ),


  tar_target(
    coef_sim_gl_plot, {
      p <- coef_sim(gl_para_summary, site = "GLOPNET")
      my_ggsave(
        "figs/coef_sim_gl",
       p,
       dpi = 300,
       height = 15,
       width = 15,
       units = "cm"
      )
    },
    format = "file"
  ),

  tar_target(
    coef_sim_pa_plot, {
      p <- coef_sim(pa_para_summary, site = "Panama")
      my_ggsave(
        "figs/coef_sim_pa",
       p,
       dpi = 300,
       height = 15,
       width = 15,
       units = "cm"
      )
    },
    format = "file"
  ),

  # best model for the full data
  tar_stan_mcmc(
    pa_full,
    "stan/PA_Ap_LLs_opt.stan",
    data = pa_stan_dat_full,
    refresh = 0,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.999,
    max_treedepth = 15,
    seed = 123,
    summaries = list(
      mean = ~mean(.x),
      sd = ~sd(.x),
      mad = ~mad(.x),
      ~posterior::quantile2(.x, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)),
      posterior::default_convergence_measures()
      )
    ),
  tar_target(
    gl_res_csv,
    generate_gl_dat(gl_csv, gl_draws_GL_Aps_LLs),
    format = "file"
  ),

  tar_target(
    pa_res_csv,
    generate_pa_dat(pa_full_csv, pa_csv, pa_full_draws_PA_Ap_LLs_opt),
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
    create_para_tbl(gl_draws_GL_Aps_LLs, pa_full_draws_PA_Ap_LLs_opt),
    format = "file"
  ),

  tar_target(
    r_vals_yml,
    write_r2(gl_res_csv, gl_draws_GL_Aps_LLs,
      pa_res_csv, pa_full_draws_PA_Ap_LLs_opt),
    format = "file"
  ),
  tar_target(
    hypo_plot, {
      p <- hypo_point(para_yml)
      my_ggsave(
        "figs/hypo",
       p,
       dpi = 300,
       height = 5,
       width = 16,
       units = "cm"
      )
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
      my_ggsave(
        "figs/gl_point",
       p,
       dpi = 300,
       height = 11.4,
       width = 11.4,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    gl_point_np_plot, {
      p <- gl_point_np(gl_long_dat, settings_yml, r_vals_yml)
      my_ggsave(
        "figs/gl_point_np",
       p,
       dpi = 300,
       height = 7.6,
       width = 11.4,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    gl_point_np2_plot, {
      p <- gl_point_np2(gl_long_dat, settings_yml, r_vals_yml)
      my_ggsave(
        "figs/gl_point_np2",
       p,
       dpi = 300,
       height = 11.4,
       width = 11.4,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    pa_point_plot, {
      p <- pa_point(pa_long_dat, settings_yml, r_vals_yml)
      my_ggsave(
        "figs/pa_point",
       p,
       dpi = 300,
       height = 11.4,
       width = 11.4,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    pa_point_npc_plot, {
      p <- pa_point_npc(pa_long_dat, settings_yml, r_vals_yml)
      my_ggsave(
        "figs/pa_point_npc",
       p,
       dpi = 300,
       height = 11.4,
       width = 11.4,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    pa_point_ll_plot, {
      p <- pa_point_ll(pa_res_csv, settings_yml, r_vals_yml)
      my_ggsave(
        "figs/pa_point_ll",
       p,
       dpi = 300,
       height = 6.7,
       width = 6.7,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    pa_point_par_ll_plot, {
      p <- pa_point_par_ll(pa_res_csv, settings_yml, r_vals_yml)
      my_ggsave(
        "figs/pa_point_par_ll",
       p,
       dpi = 300,
       height = 6.7,
       width = 6.7,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    ps_point_plot, {
      p <- ps_point(gl_res_dat, pa_res_dat, settings_yml, r_vals_yml)
      my_ggsave(
        "figs/ps_point",
       p,
       dpi = 300,
       height = 7,
       width = 11.4,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    pair_frac_plot, {
      p <- pair_frac_line(pa_res_dat)
      my_ggsave(
        "figs/pair_frac",
       p,
       dpi = 300,
       height = 7,
       width = 7,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    pair_lma_plot, {
      p <- pair_lma_line(pa_res_dat)
      my_ggsave(
        "figs/pair_lma",
       p,
       dpi = 300,
       height = 7,
       width = 11.4,
       units = "cm"
      )
    },
    format = "file"
  ),




  tar_target(
    ps_point_95_ci_plot, {
      p <- ps_point(gl_res_dat, pa_res_dat, settings_yml, r_vals_yml, ci = TRUE)
      my_ggsave(
        "figs/ps_point_95ci",
       p,
       dpi = 300,
       height = 7,
       width = 11.4,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    vpart_plot, {
      p <- vpart_bar(gl_res_csv, pa_res_csv)
      my_ggsave(
        "figs/vpart",
       p,
       dpi = 300,
       height = 6,
       width = 11,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    vpart_intra_plot, {
      p <- vpart_bar(gl_res_csv, pa_res_csv, intra = TRUE)
      my_ggsave(
        "figs/vpart_intra",
       p,
       dpi = 300,
       height = 6,
       width = 10,
       units = "cm"
      )
    },
    format = "file"
  ),
  # mass prop ----------------------------------------------------
  tar_target(
    mass_obs_dat,
    gen_mass_point_dat(
      gl_res_csv, pa_res_csv,
      gl_summary_GL_Aps_LLs, pa_full_summary_PA_Ap_LLs_opt
    )
  ),
  tar_target(
    gl_mass_prop,
    mass_prop_sim(
      read_csv(gl_res_csv), gl_summary_GL_Aps_LLs, n_sim = 1000)
  ),

  tar_target(
    mass_prop_grad_ap,
    mass_prop_sim_grad(gl_res_csv, gl_summary_GL_Aps_LLs,
                     ap = c(0.1, 0.5, 1.0),
                     as = get_mean(gl_summary_GL_Aps_LLs, "as") |> rep(3),
                     n_sim = 1000)
  ),

  tar_target(
    mass_prop_grad_as,
    mass_prop_sim_grad(gl_res_csv, gl_summary_GL_Aps_LLs,
                     ap = get_mean(gl_summary_GL_Aps_LLs, "ap") |> rep(3),
                     as = c(-0.5, 0, 0.5),
                     n_sim = 1000)
  ),

  tar_target(
    sun_mass_prop,
    mass_prop_sim(read_csv(pa_res_csv) |> filter(strata == "CAN"),
      pa_full_summary_PA_Ap_LLs_opt,
      gl = FALSE, n_sim = 1000,
      site = "Sun"
    )
  ),
  tar_target(
    shade_mass_prop,
    mass_prop_sim(read_csv(pa_res_csv) |> filter(strata != "CAN"),
      pa_full_summary_PA_Ap_LLs_opt,
      gl = FALSE, n_sim = 1000,
      site = "Shade"
    )
  ),
  tar_target(
    shade_mass_prop_mv,
    mass_prop_sim_mv(read_csv(pa_res_csv) |> filter(strata != "CAN"),
      pa_full_summary_PA_Ap_LLs_opt,
      n_sim = 1000,
      site = "Shade"
    )
  ),
  tar_target(
    mass_prop_plot, {
      p <- mass_prop_point(mass_obs_dat,
        gl_mass_prop, sun_mass_prop, shade_mass_prop)
      my_ggsave(
        "figs/mass_prop",
       p,
       dpi = 300,
       height = 6,
       width = 6,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    mass_prop_mv_plot, {
      p <- mass_prop_point(mass_obs_dat,
        gl_mass_prop, sun_mass_prop, shade_mass_prop_mv)
      my_ggsave(
        "figs/mass_prop_mv",
       p,
       dpi = 300,
       height = 6,
       width = 6,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    mass_prop_comp_plot, {
      p <- mass_prop_comp_point(shade_mass_prop_mv, shade_mass_prop)
      my_ggsave(
        "figs/mass_prop_comp",
       p,
       dpi = 300,
       height = 6,
       width = 6,
       units = "cm"
      )
    },
    format = "file"
  ),

  tar_target(
    mass_sim_plot, {
      p <- mass_sim_point(mass_prop_grad_ap, mass_prop_grad_as)
      my_ggsave(
        "figs/mass_prop_sim",
       p,
       dpi = 300,
       height = 5,
       width = 10,
       units = "cm"
      )
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
      gl_draws_GL_Aps_LLs, pa_full_draws_PA_Ap_LLs_opt),
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
      my_ggsave(
        "figs/box_main",
       p,
       dpi = 300,
       height = 11.7,
       width = 11.7,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    box_inter_plot, {
      p <- box_inter(pa_box_list, settings_yml)
      my_ggsave(
        "figs/box_inter",
       p,
       dpi = 300,
       height = 6,
       width = 11.7,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_target(
    box_frac_plot, {
      p <- box_frac(gl_box_dat, pa_intra_box_dat, settings_yml, letters_yml)
      my_ggsave(
        "figs/box_frac",
       p,
       dpi = 300,
       height = 6,
       width = 10,
       units = "cm"
      )
    },
    format = "file"
  ),
  tar_quarto(
    report,
    "report.qmd"
  ),
  NULL
)

# targets <- append(raw_data_list, tar_stan_mcmc_list)
# targets <- append(targets, tar_stan_mcmc_list)
append(raw_data_list, main_list)
