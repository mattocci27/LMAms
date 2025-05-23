#' @title write_csv for targets
#' @inheritParams readr::write_csv
my_write_csv <- function(x, path, append = FALSE, col_names = !append) {
    write_csv(x, path, append = FALSE, col_names = !append)
    paste(path)
}

prepare_gl <- function(data) {
  d <- read.csv(data, skip = 10)
  data <- tibble(
    sp = d[, "Species"] %>% unlist(),
    leaf_habit = d[, "Decid.E.green"],
    growth_form = d[, "GF"],
    BIOME = d[, "BIOME"],
    LL = 10^d[, "log.LL"],
    LMA = 10^d[, "log.LMA"],
    Aarea = 10^d[, "log.Aarea"],
    Rarea = 10^d[, "log.Rdarea"],
    Narea = 10^d[, "log.Narea"],
    Parea = 10^d[, "log.Parea"]
  ) |>
    filter(!is.na(LL)) |>
    filter(!is.na(LMA)) |>
    filter(!is.na(Aarea)) |>
    filter(!is.na(Rarea)) |>
    mutate(leaf_habit = ifelse(leaf_habit == "", "U", leaf_habit))

  ## each sample corresponds to each species
  data_clean <- data |>
    group_by(sp, leaf_habit, growth_form) |>
    summarise_at(
      .vars = vars(
        LL,
        LMA,
        Aarea,
        Rarea,
        Narea,
        Parea
      ),
      .funs = \(x)mean(x, na.rm = TRUE)
    )

  write_csv(data_clean, "./data/gl_data.csv")
  paste("./data/gl_data.csv")
}

# there two types of SLA. In our analysis, both mass and area-normalization are
# based on SLA_DISC (not leaf) following Osnas et al. 2018
# AMAXMASS: Mass-based Amax
# AMAX: Area-based Amax
# RESPMASS: Mass-based RESP
# RESP: Area-based RESP
#
# `mutate(Aarea = LMA * Amass / 1000)` works too.
prepare_pa <- function(fiber, leaf, habit) {
  fiber <- read_csv(fiber)

  fiber2 <- fiber %>%
    #  mutate(sp_site_strata = paste(sp, site2, position, sep = "_")) %>%
    rename(ADF = `%ADF`, Lig = `%Lignin`)

  d <- read_csv(leaf)

  d2 <- d |>
    rename(genus = "GENUS$") |>
    rename(species = "SPECIES$") |>
    rename(Amass = AMAXMASS) |>
    rename(Rmass = RESPMASS) |>
    rename(LT = LFTHICK) |>
    rename(sp = `SP4$`) |>
    rename(site = `SITE$`) |>
    rename(strata = `STRATA$`) |>
    mutate(site = ifelse(site == "PNM", "PNM", "PNSL")) |>
    mutate(strata = ifelse(strata == "CANOPY", "CAN", "UNDER")) |>
    mutate(LMA = 1 / SLA_DISC * 10000) |>
    mutate(LL = LIFETIME * 12 / 365) |>
    mutate(AMAX_re = LMA * Amass / 1000) |>
    mutate(Aarea = ifelse(is.na(AMAX), AMAX_re, AMAX)) |>
    mutate(RESP_re = LMA * Rmass / 1000) |>
    mutate(Rarea = ifelse(is.na(RESP), RESP_re, RESP)) |>
    mutate(Rarea = ifelse(Rarea < 0, RESP_re, Rarea)) |>
    mutate(sp_site_strata = paste(sp, site, strata, sep = "_")) |>
    mutate(Narea = LMA * N_PCT / 1000) |>
    mutate(Parea = LMA * P_PCT / 1000) |>
    mutate(LD = LMA / LT / 1000) |>
    mutate(
      LAMTUF = LAMTUF / 1000, MDRBTUF = MDRBTUF / 1000,
      VEINTUF = VEINTUF / 1000
    ) |>
  filter(Rarea > 0)

  d3 <- left_join(d2, fiber2, by = c(
    "sp" = "sp",
    "site" = "site",
    "strata" = "position"
  )) %>%
    mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) |>
    mutate(name = paste0(genus, "_", species))

  # removing strange data
  d4 <- d3 |>
    filter(sp != "PHRC") |> # 3 data
    filter(sp != "MARL") |> # Rarea of sun leaves < Rarea of shade leaves
    filter(sp != "TRAA") |> # LL measurement error
    filter(sp != "ABUP") |> # LL measurement error
    filter(sp != "VIR1") |> # Rdark is too low
    filter(!is.na(LMA)) |>
    filter(!is.na(Aarea)) |>
    filter(!is.na(Rarea)) |>
    filter(!is.na(LL))

  habit2 <- read_csv(habit) |>
    # dplyr::select(Genus, Species, LeafHabit, Site, Stratum)
    janitor::clean_names() |>
    dplyr::select(genus = genus, species = species, leaf_habit) |>
    mutate(name = str_c(genus, "_", species)) |>
    dplyr::select(name, leaf_habit) |>
    unique()

  d5 <- d4 |>
    left_join(habit2, by = "name") |>
    mutate(cell_mass = ADF - Lig) |>
    mutate(cell_area = cell_mass / 100 * LMA) |>
    mutate(cell_vol = cell_area / LT / 100 / 100 / 0.1)  |> # g/cm3
    dplyr::select(
      sp, genus, species, leaf_habit, site, site2, strata, sp_site_strata,
      LT, LD, LMA, LL, Amass, Rmass, Aarea, Rarea, Narea, Parea,
      cell_mass, cell_area, cell_vol
    )

  write_csv(d5, "./data/pa_data_full.csv")
  paste("./data/pa_data_full.csv")
}
