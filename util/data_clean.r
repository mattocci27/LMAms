library(tidyverse)
# GLOPNET
d <- read.csv("./data-raw/nature02403-s2.csv", skip = 10)

data <- tibble(sp = d[ , "Species"] %>% unlist,
         DE = d[ , "Decid.E.green"],
         GF = d[ , "GF"],
         BIOME = d[ , "BIOME"],
         LL = 10^d[ , "log.LL"],
         LMA = 10^d[ , "log.LMA"],
         Aarea = 10^d[ , "log.Aarea"],
         Rarea = 10^d[ , "log.Rdarea"])

data <- na.omit(data)
rownames(data) <- NULL

##each sample corresponds to each species
data <- data %>%
  group_by(sp, DE, GF) %>%
  summarize(LL = mean(LL),
            LMA = mean(LMA),
            Aarea = mean(Aarea),
            Rarea = mean(Rarea)) %>%
  ungroup 

write.csv(data, "./data/GL_data.csv", row.names = FALSE)


# Panama
fiber <- read_csv("./data-raw/fiber_analysis.csv")

fiber2 <- fiber %>%
#  mutate(sp_site_strata = paste(sp, site2, position, sep = "_")) %>%
  rename(ADF = `%ADF`, Lig = `%Lignin`)

d <- read_csv("./data-raw/LFTRAITS.csv")

# there two types of SLA. In our analysis, both mass and area-normalization are
# based on SLA_LEAF

d2 <- d %>%
  select("SP4$", "SITE$", "STRATA$",
         SLA_LEAF, SLA_DISC, LIFETIME, AMAXMASS, RESPMASS, AMAX,
         N_PCT, P_PCT, LFTHICK, LAMTUF, LFTHICK, MDRBTUF, VEINTUF
         ) %>%
  rename(Amass = AMAXMASS) %>%
  rename(Rmass = RESPMASS) %>%
  rename(Amax = AMAX) %>%
  rename(LT = LFTHICK) %>%
  rename(sp = `SP4$`) %>%
  rename(site = `SITE$`) %>%
  rename(strata = `STRATA$`) %>%
  mutate(site = ifelse(site == "PNM", "PNM", "PNSL")) %>%
  mutate(strata = ifelse(strata == "CANOPY", "CAN", "UNDER")) %>%
  mutate(LMA_LEAF = 1/SLA_LEAF * 10000) %>%
  mutate(LMA = 1/SLA_DISC * 10000) %>%
  mutate(LL = LIFETIME * 12/365) %>%
  mutate(Aarea = LMA_LEAF * Amass / 1000) %>%
  mutate(Aarea_DISC = LMA * Amass / 1000) %>%
  mutate(Rarea = LMA_LEAF * Rmass / 1000) %>%
  mutate(Rarea_DISC = LMA * Rmass / 1000) %>%
  mutate(sp_site_strata = paste(sp,site,strata, sep="_")) %>%
  mutate(Narea = LMA_LEAF * N_PCT / 1000) %>%
  mutate(Parea = LMA_LEAF * P_PCT / 1000) %>%
  mutate(LD = LMA / LT / 1000) %>%
  mutate(LAMTUF = LAMTUF / 1000, MDRBTUF = MDRBTUF / 1000,
      VEINTUF = VEINTUF / 1000) %>%
  filter(Rarea > 0) 

d3 <- left_join(d2, fiber2, by = c("sp" = "sp", 
                                   "site" = "site",
                                   "strata" = "position")) %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET"))

# removing strange data
d4 <- d3 %>%
  filter(sp != "PHRC") %>% # 3 data
  filter(sp != "MARL") %>% # Rarea of sun leaves < Rarea of shade leaves
  filter(sp != "TRAA") %>% # LL measurement error
  filter(sp != "ABUP") %>% # LL measurement error
  filter(sp != "VIR1") # Rdark is too low

d5 <- d4 %>%
  mutate(cell_mass = ADF - Lig) %>%
  mutate(cell_area = cell_mass / 100 * LMA) %>%
  mutate(cell_vol = cell_area / LT / 100 / 100 / 0.1) # g/cm3 

d6 <- d5 %>%
  filter(!is.na(LD)) %>%
  filter(!is.na(LT))

write.csv(d6, "./data/PA_data.csv", row.names = FALSE)
write.csv(d5, "./data/PA_data_more.csv", row.names = FALSE)

# Onoda et al. 2017

onoda_url <- "https://nph.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fnph.14496&file=nph14496-sup-0003-TableS3.xlsx"
tmp <- tempdir()
xlsx_file <- file.path(tmp, "onoda_et_al.xlsx")
xlsx_dir <- file.path(tmp)
download.file(onoda_url, xlsx_file)

# Panama taxa

d <- read_csv("./data-raw/LFTRAITS.csv")

d2 <- d %>%
  dplyr::select(sp = "SP4$", #"SITE$", "STRATA$", 
                genus = "GENUS$", 
                genus1 = "GENUS1$", 
                species = "SPECIES$",
                species1 = "SPECIES1$"
                ) %>%
  mutate(name = str_c(genus, "_", species)) %>%
  mutate(name1 = str_c(genus1, "_", species1)) %>%
  unique %>%
  mutate(name = ifelse(is.na(name), name1, name)) %>%
  dplyr::select(sp, genus, species, name)

habit <- read_csv("./data-raw/Osnas2018_S1.csv") %>%
 # dplyr::select(Genus, Species, LeafHabit, Site, Stratum)
  dplyr::select(genus = Genus, species = Species, LeafHabit) %>%
  mutate(name = str_c(genus, "_", species)) %>% 
  select(name, LeafHabit) %>%
  unique

d3 <- full_join(d2, habit, by = "name")

taxa <- d3 %>%
  filter(!is.na(sp)) %>%
  filter(!is.na(genus)) %>%
  filter(!is.na(LeafHabit)) 

write_csv(taxa, "./data/PA_LH.csv")
