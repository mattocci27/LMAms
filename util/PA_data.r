rm(list = ls()) # This clears everything from memory.
set.seed(0615)
setwd("~/Dropbox/LES/")
library(dplyr)
d <- read.csv("LFTRAITS.csv")
d$SP4. <- as.character(d$SP4.)
d$SITE. <- as.character(d$SITE.)
d$STRATA. <- as.character(d$STRATA.)

d[d$SITE.=="FTS",]$SITE. <-  "PNSL"
d$STRATA. <- ifelse(d$STRATA. == "CANOPY","CAN","UNDER")

# there two types of SLA. In our analysis, both mass and area-normalization are
# based on SLA_LEAF
d2 <- data_frame(sp = d$SP4., site = d$SITE., strata = d$STRATA.,
  SLA.leaf = d$SLA_LEAF,
  SLA.disc = d$SLA_DISC,
# LFTHICK = d$LFTHICK,
  LL = d$LIFETIME,
  Amass =  d$AMAXMASS,
  Rmass = d$RESPMASS,
  Amax = d$AMAX) %>%
  mutate(LMA.leaf = 1/SLA.leaf * 10000) %>%
  mutate(LMA = 1/SLA.disc * 10000) %>%
  mutate(LL = LL * 12/365) %>%
  mutate(Aarea = LMA.leaf * Amass / 1000) %>%
  mutate(Aarea.disc = LMA * Amass / 1000) %>%
  mutate(Rarea = LMA.leaf * Rmass / 1000) %>%
  mutate(Rarea.disc = LMA * Rmass / 1000) %>%
  mutate(sp.site.strata = paste(sp,site,strata, sep=".")) %>%
  filter(Rarea > 0) %>%
  na.omit()

d2$sp <- as.factor(d2$sp)
d2$site <- as.factor(d2$site)
d2$strata <- as.factor(d2$strata)

# removing strange data
data.obs <- d2 %>%
  filter(sp != "PHRC") %>% # 3 data
  filter(sp != "MARL") #%>% # Rarea of sun leaves < Rarea of shade leaves

# data
# PA order
# PNSL - CAN
# PNSL - UNDER
# PNM - CAN
# PNM - UNDER

data1 <- data.obs %>%
  filter(site == "PNSL" & strata == "CAN")
data2 <- data.obs %>%
  filter(site == "PNSL" & strata == "UNDER")
data3 <- data.obs %>%
  filter(site == "PNM" & strata == "CAN")
data4 <- data.obs %>%
  filter(site == "PNM" & strata == "UNDER")

data <- rbind(data1, data2, data3, data4)

LMA.all.data <- list()
for (i in 1:99){
  data_temp <- data %>%
    select(sp.site.strata, LMA, LL, Aarea, Rarea)
  
  data_temp2 <- data.frame(sp.site.strata = sample(data_temp$sp.site.strata),
             LMA = sample(data_temp$LMA),
             LL = sample(data_temp$LL),
             Aarea = data_temp$Aarea,
             Rarea = data_temp$Rarea
             ) 
  sp_site_st <-strsplit(as.character(data_temp2$sp.site.strata), "\\.") %>%
    sapply(.,"[",1:3) %>% 
    t %>% 
    as.data.frame
  names(sp_site_st) <- c("sp", "site", "strata")

  LMA.all.data[[i]] <- 
    data.frame(sp_site_st, data_temp2 %>% select(-sp.site.strata))
} 

LMA.all.data2 <- list()
for (i in 1:99){
    temp <- data.frame(data[,1:3],
             LMA = sample(data$LMA),
             LL = sample(data$LL),
             Aarea = sample(data$Aarea),
             Rarea = sample(data$Rarea)
             ) 
    while (min(temp$Aarea - temp$Rarea) < 0){
    temp <- data.frame(data[,1:3],
             LMA = sample(data$LMA),
             LL = sample(data$LL),
             Aarea = sample(data$Aarea),
             Rarea = sample(data$Rarea)
             ) 
    }
  temp$A_R <- temp$A - temp$R
  LMA.all.data2[[i]] <- temp
} 

#sapply(LMA.all.data2, "[[", 8) %>%
#  apply(., 1, min) %>% min

LMA.ra.data <- NULL
for (i in 1:99) LMA.ra.data <-
  cbind(LMA.ra.data, sample(data.obs$LMA, nrow(data.obs)))

LMA.rw.data <- NULL
for (i in 1:99){
  data1 <- data.obs %>%
    filter(site == "PNSL" & strata == "CAN") %>%
    mutate(LMA.rw = sample(LMA))
  data2 <- data.obs %>%
    filter(site == "PNSL" & strata == "UNDER") %>%
    mutate(LMA.rw = sample(LMA))
  data3 <- data.obs %>%
    filter(site == "PNM" & strata == "CAN") %>%
    mutate(LMA.rw = sample(LMA))
  data4 <- data.obs %>%
    filter(site == "PNM" & strata == "UNDER") %>%
    mutate(LMA.rw = sample(LMA))
  data.n <- rbind(data1, data2, data3, data4)
  LMA.rw.data <- cbind(LMA.rw.data, data.n$LMA.rw)
}

# shuffle within strata but across sites
# PA order
# PNSL - CAN
# PNSL - UNDER
# PNM - CAN
# PNM - UNDER

LMA.rs.data <- NULL
for (i in 1:99) {
  data1 <- data.obs %>%
    filter(strata == "CAN") %>%
    mutate(LMA.rs = sample(LMA))

  data2 <- data.obs %>%
    filter(strata == "UNDER") %>%
    mutate(LMA.rs = sample(LMA))

  data.n <- rbind(data1, data2) %>%
    arrange(strata) %>%
    arrange(desc(site))
  LMA.rs.data <- cbind(LMA.rs.data, data.n$LMA.rs)
}

# shuffle within site but across strata
# PNSL - CAN
# PNSL - UNDER
# PNM - CAN
# PNM - UNDER

LMA.rc.data <- NULL
for (i in 1:99) {
  data1 <- data.obs %>%
    filter(site == "PNSL") %>%
    mutate(LMA.rc = sample(LMA))

  data2 <- data.obs %>%
    filter(site == "PNM") %>%
    mutate(LMA.rc = sample(LMA))

  data.n <- rbind(data1, data2) %>%
    arrange(strata) %>%
    arrange(desc(site))
  LMA.rc.data <- cbind(LMA.rc.data, data.n$LMA.rc)
}

data <- as.data.frame(data)
save.image("PA_rand_data_99.RData")

