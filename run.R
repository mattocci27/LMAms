 targets::tar_make_clustermq(
#   workers = 6
  workers = parallel::detectCores()
#  reporter = "silent"
)

# targets::tar_make()

tar_load(pa_rand_summary_data)
tar_load(pa_rand_diagnostics_data)

names(pa_rand_summary_data)
names(pa_rand_diagnostics_data)

pa_rand_summary_data[[1]] |> length()

pa_rand_summary_data[[1]][[1]]$LMA
pa_rand_summary_data[[1]][[2]]$LMA
pa_rand_summary_data[[2]][[2]]$LMA

pa_rand_summary_data |> str()


pa_rand_summary_data[[1]][[1]]$LMA
pa_rand_diagnostics_data[[1]][[1]]$LMA
