 targets::tar_make_clustermq(
#   workers = 6
  workers = parallel::detectCores()
#  reporter = "silent"
)

# targets::tar_make()
