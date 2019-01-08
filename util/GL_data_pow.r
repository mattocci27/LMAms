setwd("~/Dropbox/MS/LES_MS/LMApsModel/data/")
# setwd("~/Dropbox/MS/LES_MS/LMApsModel/md/")
# render("GL_check.rmd", "bookdown::pdf_book")
# use null result 1
null_n <- 3

# GL data -------------------------------------------------------------------
load("GL_model1_obs_2017-06-19_.RData")

s1 <- fit.summary
GL <- data.obs
P_vec <- paste("p[", 1:nrow(GL), "]" ,sep = "")

GL <- GL %>%
  mutate(DE = ifelse(GL$DE == "", "U", as.character(DE))) %>%
  mutate(LMAp1 =  fit.summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAs1 = LMA - LMAp1)


load("GL_model1r4_obs_2017-06-19_.RData")
s4 <- fit.summary

GL <- GL %>%
  mutate(LMAp4 =  fit.summary[P_vec, "mean"] * LMA) %>%
  mutate(LMAs4 = LMA - LMAp4)

head(GL)

load("GL_model1_LMA_2017-06-19_.RData")
s1LMA <- fit.summary.list[[null_n]]

temp <- LMA.ra.data[,null_n]
GL <- GL %>%
  mutate(LMA_ra = temp) %>%
  mutate(LMAp1_ra =  s1LMA[P_vec, "mean"] * LMA_ra) %>%
  mutate(LMAs1_ra = LMA_ra - LMAp1_ra)

head(GL)

load("GL_model1_All_2017-06-19_.RData")

# sp order
s1ALL <- fit.summary.list[[null_n]]

# sp order
temp <- LMA.ra2.data[[null_n]]
  #arrange(LMA)

GL <- GL %>%
  #arrange(LMA) %>%
  mutate(LMA_all = temp[,"LMA"]) %>%
  mutate(LMAp1_all =  s1ALL[P_vec, "mean"] * LMA_all) %>%
  mutate(LMAs1_all = LMA_all - LMAp1_all) %>%
  mutate(LL_all = temp[, "LL"]) %>%
  mutate(Rarea_all = temp[, "Rarea"]) %>%
  mutate(Aarea_all = temp[, "Aarea"])



load("GL_model1r4_All_2017-06-19_.RData")
s4ALL <- fit.summary.list[[null_n]]

temp <- LMA.ra2.data[[null_n]]

GL <- GL %>%
  mutate(LMAp4_all =  s4ALL[P_vec, "mean"] * LMA_all) %>%
  mutate(LMAs4_all = LMA_all - LMAp4_all)


load("GL_model1r4_LMA_2017-06-19_.RData")
s4LMA <- fit.summary.list[[null_n]]

GL <- GL %>%
  mutate(LMAp4_ra =  s1LMA[P_vec, "mean"] * LMA_ra) %>%
  mutate(LMAs4_ra = LMA_ra - LMAp4_ra)

dd <- data.frame(Narea = 10^d$log.Narea,
        Parea = 10^d$log.Parea,
        sp = d$Species) %>%
        group_by(sp) %>%
        summarize(Narea = mean(Narea, na.omit = T),
            Parea = mean(Parea, na.omit = T))

GL <- left_join(GL, dd, by = "sp")

write.csv(GL, "~/Dropbox/MS/LES_MS/LMApsModel/data/GL20170911.csv", row.names = FALSE)


