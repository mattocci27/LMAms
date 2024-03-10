library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
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

plan(multicore)
options(clustermq.scheduler = "multicore")

tar_option_set(packages = c(
  "tidyverse",
  "cmdstanr",
  "patchwork",
  "parallel",
  "janitor",
  "loo",
  "jsonlite",
  "foreach",
  "doParallel",
  "multcompView"
))

# tar_option_set(
#   garbage_collection = TRUE,
#   memory = "transient"
# )

stan_names <- generate_stan_names("templates/model.json",
      "templates/model_LMA.json")

loo_map <- tar_map(
    values = list(mcmc = rlang::syms(stan_names$mcmc_names)),
    tar_target(
      loo,
      my_loo(mcmc)
    )
)

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
        my_write_csv("data/pa_data.csv")
    },
    format = "file"
  ),
  tar_target(
    settings_yml,
    "yml/settings.yml",
    format = "file"
  ),
  tar_target(
    lma_yml,
    write_lma_yml(gl_csv_raw, pa_full_csv_raw, "yml/lma.yml"),
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
    gl,
    stan_names$gl_stan_names,
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
    stan_names$pa_stan_names,
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


  tar_map(
    values = list(diagnostics = rlang::syms(stan_names$diagnostics_names)),
    tar_target(div_check_list, div_check(diagnostics))
  ),

  tar_map(
    values = list(summary = rlang::syms(stan_names$summary_names)),
    tar_target(summary_rhat, {
      summary |> filter(rhat > 1.05)
    })
  ),

  # we have many loo list
  # combine them in to a single list
  loo_map,
  tar_combine(
    loo_list,
    loo_map,
    command = list(!!!.x)
  ),

  tar_target(
    loo_tbl, {
      tibble(model = names(loo_list),
      looic = lapply(loo_list, \(x)x$estimate) |>
        sapply(\(x)x[3, 1]),
      elpd = lapply(loo_list, \(x)x$estimate) |>
        sapply(\(x)x[1, 1]),
       n = lapply(loo_list, "[[", "pointwise") |> sapply(nrow)) |>
        mutate(site = str_split_fixed(model, "_", 3)[,2]) |>
        mutate(site = str_split_fixed(site, "_", 2)[,1]) |>
        arrange(looic) |>
        arrange(site) |>
        my_write_csv("data/loo.csv")
    },
    format = "file"
  ),

  tar_target(
    model_selection_csv,
    write_model_selection(loo_tbl, "data/model_selection.csv"),
    format = "file"
  ),

  tar_target(
    para_yml,
    write_para_yml(gl_summary_ams_bs, pa_summary_am_bs_opt,
      gl_res_csv, pa_res_csv),
    format = "file"
  ),
  tar_target(
    var_yml,
    write_var_yml(gl_draws_ams_bs, gl_res_dat, pa_full_draws_am_bs_opt, pa_res_dat),
    format = "file"
  ),

  tar_target(
    gl_para_all_csv,
    my_write_csv(gl_summary_ams_bs, "data/gl_para_all.csv"),
    format = "file"
  ),
  tar_target(
    pa_para_all_csv,
    my_write_csv(pa_summary_am_bs_opt, "data/pa_para_all.csv"),
    format = "file"
  ),

  # simulation ------------------------

  # use `sim_rep` as `pattern`
  tar_target(sim_rep, seq(1, 10)),

  tar_target(
    ams_bs,
    compile_model("stan/ams_bs.stan"),
    format = "file"
  ),
  tar_target(
    am_bs_opt,
    compile_model("stan/am_bs_opt.stan"),
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
      ams_bs,
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
      am_bs_opt,
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
    sim_summary_diagnostics_csv,
    generate_summary_diagnostics(gl_sim_diagnostics, pa_sim_diagnostics, "data/sim_summary_diagnostics.csv"),
    format = "file"
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
    "stan/am_bs_opt.stan",
    data = pa_stan_dat_full,
    refresh = 0,
    chains = 4,
    parallel_chains = 1,
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
    generate_gl_dat(gl_csv, gl_draws_ams_bs),
    format = "file"
  ),

  tar_target(
    pa_res_csv,
    generate_pa_dat(pa_full_csv, pa_csv, pa_full_draws_am_bs_opt),
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
    create_para_tbl(gl_draws_ams_bs, pa_full_draws_am_bs_opt),
    format = "file"
  ),

  tar_target(
    pa_rho_dat,
    generate_pa_rho_data(pa_res_csv)
  ),

  tar_target(
    r_vals_yml,
    write_r2(gl_res_csv, gl_draws_ams_bs,
      pa_res_csv, pa_full_draws_am_bs_opt),
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
       width = 17,
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
    pa_point_npc_rho_plot, {
      p <- pa_point_npc_rho(pa_rho_dat, settings_yml, r_vals_yml)
      my_ggsave(
        "figs/pa_point_npc_par",
       p,
       dpi = 300,
       height = 11.4,
       width = 12.4,
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
       width = 11.4,
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
       width = 11.4,
       units = "cm"
      )
    },
    format = "file"
  ),

  tar_target(
    vpart_csv,
    write_vpart_csv(gl_res_csv, pa_res_csv, "data/vpart_inter.csv", intra = FALSE),
    format = "file"
  ),
  tar_target(
    vpart_intra_csv,
    write_vpart_csv(gl_res_csv, pa_res_csv, "data/vpart_intra.csv", intra = TRUE),
    format = "file"
  ),

  # mass prop ----------------------------------------------------
  tar_target(
    mass_obs_dat,
    gen_mass_point_dat(
      gl_res_csv, pa_res_csv,
      gl_summary_ams_bs, pa_full_summary_am_bs_opt
    )
  ),
  tar_target(
    gl_mass_prop,
    mass_prop_sim(
      read_csv(gl_res_csv), gl_summary_ams_bs, n_sim = 1000)
  ),

  tar_target(
    mass_prop_grad_am,
    mass_prop_sim_grad(gl_res_csv, gl_summary_ams_bs,
                     am = c(0.1, 0.5, 1.0),
                     as = get_mean(gl_summary_ams_bs, "as") |> rep(3),
                     n_sim = 1000)
  ),

  tar_target(
    mass_prop_grad_as,
    mass_prop_sim_grad(gl_res_csv, gl_summary_ams_bs,
                     am = get_mean(gl_summary_ams_bs, "am") |> rep(3),
                     as = c(-0.5, 0, 0.5),
                     n_sim = 1000)
  ),

  tar_target(
    sun_mass_prop,
    mass_prop_sim(read_csv(pa_res_csv) |> filter(strata == "CAN"),
      pa_full_summary_am_bs_opt,
      gl = FALSE, n_sim = 1000,
      site = "Sun"
    )
  ),
  tar_target(
    shade_mass_prop,
    mass_prop_sim(read_csv(pa_res_csv) |> filter(strata != "CAN"),
      pa_full_summary_am_bs_opt,
      gl = FALSE, n_sim = 1000,
      site = "Shade"
    )
  ),
  tar_target(
    shade_mass_prop_mv,
    mass_prop_sim_mv(read_csv(pa_res_csv) |> filter(strata != "CAN"),
      pa_full_summary_am_bs_opt,
      para_yml,
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
      p <- mass_sim_point(mass_prop_grad_am, mass_prop_grad_as)
      my_ggsave(
        "figs/mass_prop_sim",
       p,
       dpi = 300,
       height = 5.5,
       width = 11,
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
      gl_draws_ams_bs, pa_full_draws_am_bs_opt),
    format = "file"
  ),
  tar_target(
    gl_box_list,
    prep_gl_box_list(gl_res_dat, letters_yml)
  ),
  tar_target(
    pa_box_trim_list,
    prep_pa_box_list(pa_intra_box_dat, letters_yml, type = "PA_intra")
  ),
  tar_target(
    pa_box_list,
    prep_pa_box_list(pa_inter_box_dat, letters_yml, type = "PA_inter")
  ),
  tar_target(
    pa_box_trim_de_list,
    prep_pa_box_list(pa_intra_box_dat, letters_yml, type = "PA_intra_de")
  ),
  tar_target(
    pa_box_de_list,
    prep_pa_box_list(pa_inter_box_dat, letters_yml, type = "PA_inter_de")
  ),
  tar_target(
    frac_yml,
    write_frac_yml(gl_box_dat, pa_inter_box_dat, pa_intra_box_dat),
    format = "file"
  ),
  tar_target(
    box_intra_plot, {
      p <- box_intra(gl_box_list, pa_box_trim_de_list, pa_box_trim_list, settings_yml)
      my_ggsave(
        "figs/box_intra",
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
    box_de_plot, {
      p <- box_de(gl_box_list, pa_box_trim_de_list, settings_yml)
      my_ggsave(
        "figs/box_de",
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
    box_pa_plot, {
      p <- box_pa(pa_box_trim_list, settings_yml)
      my_ggsave(
        "figs/box_pa",
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
    box_inter_plot, {
      p <- box_inter(pa_box_de_list, pa_box_list, settings_yml)
      my_ggsave(
        "figs/box_inter",
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
    box_frac_de_plot, {
      p <- box_frac(gl_box_dat, pa_intra_box_dat, pa_inter_box_dat, settings_yml, letters_yml)
      my_ggsave(
        "figs/box_frac_de",
       p,
       dpi = 300,
       height = 6,
       width = 13,
       units = "cm"
      )
    },
    format = "file"
  ),

  tar_target(
    box_frac_pa_plot, {
      p <- box_frac_pa(pa_intra_box_dat, pa_inter_box_dat, settings_yml, letters_yml)
      my_ggsave(
        "figs/box_frac_pa",
       p,
       dpi = 300,
       height = 6,
       width = 12,
       units = "cm"
      )
    },
    format = "file"
  ),

  # tar_quarto(
  #   report,
  #   "report.qmd"
  # ),
  NULL
)

append(raw_data_list, main_list)
