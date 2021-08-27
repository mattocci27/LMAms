library(tidyverse)

# Loo table ================================================================

GLtab <- read_csv("./data/GL_elpd.csv")

GLtab2 <- GLtab %>%
  mutate(LMA = ifelse(LMA == "GL_LMAms",
         "LMAp and LMAs", "LMA")) %>%
  arrange(looic) %>%
  mutate(looic = round(looic, 1)) %>%
  mutate(looic = as.character(looic)) %>%
  mutate(N = as.character(N)) %>%
  mutate(site = "GL")# %>%
  #bind_rows(data.frame(LMA = "**GLOPNET**", looic = "", N = "", site = ""),
  #          .)

PAtab <- read_csv("./data/PA_elpd.csv")

PAtab2 <- PAtab %>%
  mutate(LMA = ifelse(LMA == "PA_LMAms0",
         "LMAp + LMAs", LMA)) %>%
  mutate(LMA = ifelse(LMA == "PA_LMAms_L0",
         "LMAp + LMAs + light", LMA)) %>%
  mutate(LMA = ifelse(LMA == "PA_LMA_L",
         "LMA + light", LMA)) %>%
  mutate(LMA = ifelse(LMA == "PA_LD_L",
         "LMAp + LMAs/LT + light", LMA)) %>%
  mutate(LMA = ifelse(LMA == "PA_LMA",
         "LMA", LMA)) %>%
  arrange(looic) %>%
  mutate(looic = round(looic, 1)) %>%
  mutate(looic = as.character(looic)) %>%
  mutate(N = as.character(N)) %>%
  mutate(site = "Panama")# %>%
  #bind_rows(data.frame(LMA = "**Panama**", looic = "", N = "", site = ""),
  #          .)

tab3 <- bind_rows(GLtab2, PAtab2) %>%
  rename(Model = LMA) %>%
  rename(LOOIC = looic) #%>%
  #dplyr::select(-site)

#colnames(tab3) <- c("",
#                    "LOOIC",
#                    "N")

write_csv(tab3, "./data/LOO.csv")


# para table ===============================================================
s_GL <- read_csv("./data/GLpara.csv")

GLtab <- s_GL %>%
  filter(para == "amas[1]" |
         para == "amas[2]" |
         para == "z[4]" |
         para == "z[5]" |
         para == "z[6]" |
         para == "z[7]") %>%
  mutate(tmp = c(2, 1, 3, 5, 4, 6)) %>%
  arrange(tmp) %>%
  mutate(mid = round(`X50.`, 3)) %>%
  mutate(low = round(`X2.5.`, 3)) %>%
  mutate(up = round(`X97.5.`, 3)) %>%
  mutate(est = paste0(mid, " [", low, ", ", up, "]")) %>%
  mutate(para = c("Effect of LMAp on *A*~area~ ($\\alpha_p$)",
                  "Effect of LMAs on *A*~area~ ($\\alpha_s$)",
                  "Effect of LMAp on LL ($\\beta_p$)",
                  "Effect of LMAs on LL ($\\beta_s$)",
                  "Effect of LMAp on *R*~area~ ($\\gamma_p$)",
                  "Effect of LMAs on *R*~area~ ($\\gamma_s$)"
                  ))  %>%
  dplyr::select(para, GLOPNET = est, sig1 = sig)


s_PA <- read_csv("./data/PApara.csv")

PAtab <- s_PA %>%
  filter(para == "am" |
         para == "as" |
         para == "z[8]" |
         para == "z[4]" |
         para == "z[5]" |
         para == "z[6]" |
         para == "z[7]") %>%
  mutate(tmp = c(3, 5, 4, 6, 7, 1, 2)) %>%
  arrange(tmp) %>%
  mutate(mid = round(`X50.`, 3)) %>%
  mutate(low = round(`X2.5.`, 3)) %>%
  mutate(up = round(`X97.5.`, 3)) %>%
  mutate(est = paste0(mid, " [", low, ", ", up, "]")) %>%
  mutate(para = c("Effect of LMAp on *A*~area~ ($\\alpha_p$)",
                  "Effect of LMAs on *A*~area~ ($\\alpha_s$)",
                  "Effect of LMAp on LL ($\\beta_p$)",
                  "Effect of LMAs on LL ($\\beta_s$)",
                  "Effect of LMAp on *R*~area~ ($\\gamma_p$)",
                  "Effect of LMAs on *R*~area~ ($\\gamma_s$)",
                  "Effect of light on LL ($\\theta$)"
                  ))  %>%
  dplyr::select(para, Panama = est, sig2 = sig)


GLPAtab <- full_join(GLtab, PAtab, by = "para") %>%
  dplyr::rename(Parameters = para) %>%
  mutate(GLOPNET = ifelse(sig1 == "sig",
                        paste0("**", GLOPNET, "**"),
                        GLOPNET)) %>%
  mutate(Panama = ifelse(sig2 == "sig",
                        paste0("**", Panama, "**"),
                        Panama)) %>%
  dplyr::select(Parameters, GLOPNET, Panama)

GLPAtab[is.na(GLPAtab)] <- "-"

write_csv(GLPAtab, "./data/para_tbl.csv")
