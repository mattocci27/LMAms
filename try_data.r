# Get traits that we are interested in, and standardize their units.
# Note that some only traits have either one of mass or area basis values.

library(tidyverse)
library(RSQLite)

if (file.exists("./data/try.sqlite3")) {
  db <- src_sqlite("./data/try.sqlite3", create = TRUE)
} else stop("no sqlite data")

trait <- db %>%
  tbl("try")# %>%
  #collect

trait2 <- trait %>%
  filter(TraitName == "Photosynthesis per leaf area" |
         TraitName == "Photosynthesis per leaf dry mass" |
         TraitName == "Leaf respiration per area" |
         TraitName == "Leaf respiration per dry mass" |
         TraitName == "Leaf lifespan" |
         TraitName == "Leaf thickness" |
         TraitName == "Leaf density" |
         TraitName == "Leaf lamina thickness" |
         TraitName == "Leaf lamina density" |
         TraitName == "Leaf specific area (SLA)" |
         TraitName == "Specific leaf area (SLA) of leaf lamina"|
         TraitName == "Leaf nitrogen (N) content per dry mass" |
         TraitName == "Leaf nitrogen (N) content per area" |
         TraitName == "Leaf phosphorus (P) content per dry mass" |
         TraitName == "Leaf phosphorus (P) content per area"
         ) %>%
  dplyr::select(TraitName, ObservationID,
                OrigValueStr, StdValue,
                OrigUnitStr, Unit_1_UnitName) %>%
  mutate(val = OrigValueStr %>% as.numeric) %>%
  mutate(val = ifelse(is.na(val), 
                      StdValue, 
                      val))
 
  # %>%
 # mutate(trait = TraitName) # %>%
#  mutate(trait = ifelse(trait == "Leaf specific area (SLA)",
#                        "SLA", trait)) %>%
#  mutate(trait = ifelse(trait == "Specific leaf area (SLA) of leaf lamina",
#                        "SLA2", trait)) %>%
#  mutate(trait = ifelse(trait == "Leaf density",
#                        "LD", trait)) %>%
#  mutate(trait = ifelse(trait == "Leaf thickness",
#                        "LT", trait)) %>%
#  mutate(trait = ifelse(trait == "Leaf lamina density",
#                        "LD", trait)) %>%
#  mutate(trait = ifelse(trait == "Leaf lamina thickness",
#                        "LT", trait)) %>%
#  mutate(trait = ifelse(trait == "Leaf lifespan",
#                        "LL", trait)) 
#trait3 <- trait2 %>%
#  mutate(trait = ifelse(trait == "Leaf nitrogen (N) content per dry mass",
#                        "Nmass", trait)) 
#trait3 %>%
#  mutate(trait = ifelse(trait == "Leaf nitrogen (N) content per area",
 #                       "Narea", trait))# %>%
 # mutate(trait = ifelse(trait == "Leaf phosphorus (P) content per dry mass",
 #                       "Pmass", trait)) %>%
 # mutate(trait = ifelse(trait == "Leaf phosphorus (P) content per area",
 #                       "Parea", trait)) %>%
 # mutate(trait = ifelse(trait == "Photosynthesis per leaf dry mass",
 #                       "Amass", trait)) %>%
 # mutate(trait = ifelse(trait == "Photosynthesis per leaf area",
 #                       "Aarea", trait)) %>%
 # mutate(trait = ifelse(trait == "Leaf respiration per dry mass",
 #                       "Rmass", trait)) %>%
 # mutate(trait = ifelse(trait == "Leaf respiration per area",
 #                       "Rarea", trait)) 
#

## Aarea
Aarea <- trait2 %>%
  collect %>%
  filter(TraitName == "Photosynthesis per leaf area") 

Aarea %>% select(OrigUnitStr) %>% collect %>% unlist %>% unique
Aarea %>% select(Unit_1_UnitName) %>% collect %>%unique

## Amass
Amass <- trait2 %>%
  collect %>%
  filter(TraitName == "Photosynthesis per leaf dry mass")

Amass %>% select(OrigUnitStr) %>% collect %>% unlist %>% unique
Amass %>% select(Unit_1_UnitName) %>% collect %>%unique

Amass <- Amass %>% 
  mutate(val = ifelse(str_detect(OrigUnitStr, "nano|nmol"), val / 1000, val)) %>%
  mutate(val = ifelse(str_detect(OrigUnitStr, "mmol"), val * 1000, val))

## Rarea
Rarea <- trait2 %>%
  filter(TraitName == "Leaf respiration per area")

Rarea %>% select(OrigUnitStr) %>% collect %>% unlist %>% unique
Rarea %>% select(Unit_1_UnitName) %>% collect %>%unique

Rarea <- Rarea %>% 
  collect %>%
  mutate(val = ifelse(str_detect(OrigUnitStr, "mmol"), val * 1000, val))

## Rmass
Rmass <- trait2 %>%
  collect %>%
  filter(TraitName == "Leaf respiration per dry mass")

Rmass %>% select(OrigUnitStr) %>% collect %>% unlist %>% unique
Rmass %>% select(Unit_1_UnitName) %>% collect %>%unique

Rmass <- Rmass %>% 
  mutate(val = ifelse(str_detect(OrigUnitStr, "mmol"), val * 1000, val)) %>%
  mutate(val = ifelse(str_detect(OrigUnitStr, "nmol|nano"), val / 1000, val))

## LL
LL <- trait2 %>%
  collect %>%
  filter(TraitName == "Leaf lifespan")

LL %>% select(OrigUnitStr) %>% collect %>% unlist %>% unique
LL %>% select(Unit_1_UnitName) %>% collect %>%unique

LL <- LL %>% 
  mutate(val = ifelse(str_detect(OrigUnitStr, "yr|years"), val * 12, val)) %>%
  mutate(val = ifelse(str_detect(OrigUnitStr, "day"), val / 30, val)) 


## SLA
SLA <- trait2 %>%
  filter(TraitName == "Leaf specific area (SLA)" |
         TraitName == "Specific leaf area (SLA) of leaf lamina") %>%
  collect

SLA %>% select(OrigUnitStr) %>% collect %>% unlist %>% unique
SLA %>% select(Unit_1_UnitName) %>% collect %>%unique

SLA %>%
  mutate(val = ifelse(OrigUnitStr == "cm2/g", val * 100/1000, val)) %>%
  mutate(val = ifelse(OrigUnitStr == "mg/dm2", val * 100/1000, val))# %>%

  filter(OrigUnitStr == "mg/cm2")



## LT
LT <- trait2 %>%
  filter(TraitName == "Leaf thickness" |
         TraitName == "Leaf lamina thickness") %>%
  collect

## LD
LD <- trait2 %>%
  filter(TraitName == "Leaf density" |
         TraitName == "Leaf lamina density") %>%
  collect

tmp <- Aarea %>%
  dplyr::select(ObservationID, Aarea = val) %>%
  full_join(., Amass %>% 
              dplyr::select(ObservationID, Amass = val),
             by = "ObservationID"
             ) %>%
  full_join(., Rarea %>% 
              dplyr::select(ObservationID, Rarea = val),
             by = "ObservationID"
             ) %>%
  full_join(., Rmass %>% 
              dplyr::select(ObservationID, Rmass = val),
             by = "ObservationID"
             ) %>%
  full_join(., LL %>% 
              dplyr::select(ObservationID, LL = val),
             by = "ObservationID"
             ) %>%
  full_join(., SLA %>% 
              dplyr::select(ObservationID, SLA = val),
             by = "ObservationID"
             ) %>%
  full_join(., LD %>% 
              dplyr::select(ObservationID, LD = val),
             by = "ObservationID"
             ) %>%
  full_join(., LT %>% 
              dplyr::select(ObservationID, LT = val),
             by = "ObservationID"
             )


tmp %>% 
  filter(!is.na(Rarea) | !is.na(Rmass)) %>%
  filter(!is.na(Aarea) | !is.na(Amass)) %>%
  filter(!is.na(LL)) %>%
  filter(!is.na(SLA)) %>%
  filter(!is.na(LD) | !is.na(LT)) %>%
  unique


  
  Aarea %>%
  dplyr::select(TraitName, Aarea = val) %>%



#moge <- bind_rows(Aarea, Rarea, Amass, Rmass, LL) %>%
#  group_by(TraitName, ObservationID) %>%
#  summarize(val = mean(val, na.rm =T)) %>%
#  spread(TraitName, val)
#
#moge2 <- moge %>%
#  mutate(Aarea = ifelse(is.na(`Photosynthesis per leaf area`),
#                       #`Photosynthesis per leaf dry mass`,
#                       `Photosynthesis per leaf area`,
#                       `Photosynthesis per leaf area` )) %>%
#  mutate(Rarea = ifelse(is.na(`Leaf respiration per area`),
#                       #`Leaf respiration per dry mass`,
#                       `Leaf respiration per area`,
#                       `Leaf respiration per area` )) %>%
#  dplyr::select(`Leaf lifespan`, Aarea, Rarea)
#
#na.omit(moge2)
