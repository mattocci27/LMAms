#---------------------------------------------------------------------
library(dplyr)
P_vec <- paste("p[", 1:nrow(data), "]", sep = "")
Mu_vec <- paste("mu[", 1:nrow(data), ",2]", sep = "")


PA <- data %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, strata, sep = "_")) %>%
  mutate(site_strata = paste(site2, strata, sep = "_")) %>%
  mutate(LMAp_opt = LMA * fit.summary[P_vec, "mean"]) %>%
  mutate(LMAs_opt = LMA - LMAp_opt) %>%
  mutate(preLL_opt = exp(fit.summary[Mu_vec, "mean"])) %>%
  mutate(p = LMAp_opt/LMA)


# fiber data -------------------------------------------------------------------
fiber <- read.csv("~/Dropbox/LES/fiber_analysis.csv")
trait <- read.csv("~/Dropbox/LES/LFTRAITS.csv")

trait2 <- trait %>%
  mutate(strata = ifelse(STRATA. == "CANOPY", "CAN", "UNDER")) %>%
  mutate(site = ifelse(trait$SITE. == "PNM", "PNM", "PNLS")) %>%
  mutate(site2 = ifelse(trait$SITE. == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(SP4., site2, strata, sep = "_")) %>%
  mutate(LMA_LEAF = 1/SLA_LEAF * 10000) %>%
　　mutate(LMA = 1/SLA_DISC * 10000) %>%
　　mutate(Narea = LMA_LEAF * N_PCT / 1000) %>%
  mutate(Parea = LMA_LEAF * P_PCT / 1000) %>%
  mutate(LD = LMA / LFTHICK / 1000) %>%
  mutate(LAMTUF = LAMTUF / 1000, MDRBTUF = MDRBTUF / 1000,
      VEINTUF = VEINTUF / 1000)

fiber2 <- fiber %>%
  mutate(site2 = ifelse(site == "PNM", "DRY", "WET")) %>%
  mutate(sp_site_strata = paste(sp, site2, position, sep = "_"))

fdata <- full_join(trait2, fiber2, by = "sp_site_strata") %>%
  select(sp_site_strata, X.ADF, X.NPE, X.Lignin,
     LFTHICK, LD, Narea, Parea, LAMTUF, MDRBTUF, VEINTUF) %>%
  rename(ADF = X.ADF, NPE = X.NPE, Lig = X.Lignin, LT = LFTHICK)

PA2 <- left_join(PA, fdata, by = "sp_site_strata") %>%
  mutate(cell_mass = ADF - Lig) %>%
  mutate(cell_area = cell_mass / 100 * LMA) %>%
  mutate(cell_vol = cell_area / LT / 100 / 100 / 0.1) # g/cm3




  Lim.func <- function(trait){
    mid_len <- (log10(max(trait, na.rm = T)) - log10(min(trait, na.rm = T))) /2
    max_lim <- log10(max(trait, na.rm = T)) + mid_len * 0.25
    min_lim <- log10(min(trait, na.rm = T)) - mid_len * 0.25
    return(10^(c(min_lim, max_lim)))
  }

  PA.each <- function(x, y, data, rand = c("ra", "rc"), log = log, y_text = NULL, x_text = NULL, subscript = NULL){
    sun.wet <- data %>% filter(strata == "CAN" & site == "PNSL")
    sun.dry <- data %>% filter(strata == "CAN" & site == "PNM")
    shade.wet <- data %>% filter(strata == "UNDER" & site == "PNSL")
    shade.dry <- data %>% filter(strata == "UNDER" & site == "PNM")

    rand.x <- paste(x, rand, sep = "_")

    plot(data[, x], data[, y], log = log, axes = F, type = "n",
      xlim = Lim.func(data[, x]), ylim = Lim.func(data[, y]))
    mtext(subscript, side = 3, line = -1, adj = 0, cex = 0.65)
    box()

    # points(data[, rand.x], data[, y], col="gray", pch=19, cex=0.8)

    points(sun.wet[, x], sun.wet[, y], col = "dodgerblue3",
      pch = 21, bg = "white", lwd = 1.2)

    points(sun.dry[, x], sun.dry[, y], col = "burlywood4",
      pch = 21, bg = "white", lwd = 1.2)

    points(shade.wet[, x], shade.wet[, y], bg = "dodgerblue3", col = "black",
      pch = 21, lwd = 0.8)

    points(shade.dry[, x], shade.dry[, y], bg = "burlywood4", col = "black",
      pch = 21, lwd = 0.8)


    if(is.null(y_text) != TRUE){
      axis(2, tick = FALSE,line = -0.8, cex.axis = 0.9)
      axis(2, tcl = 0.2, labels = FALSE)
      mtext(y_text, side = 2, line = 1.2, cex = 0.8)
    }

    if(is.null(x_text) != TRUE){
      axis(1, tick = FALSE, line = -0.8, cex.axis = 0.9)
      axis(1, tcl = 0.2, labels = FALSE, cex = 0.8)
      mtext(x_text, side = 1, line = 1.5, cex = 0.8)
    }
  }


  # fig.2 -----------------------------------------------------------------------
  postscript("~/Dropbox/MS/LES_MS/fig/moge.eps", width = 4.33, height = 4.33)
  par(mfrow = c(3, 3))
  par(mar = c(0, 0, 0, 0))
  par(oma = c(3, 3, 2, 2))

  PA.each(y = "Aarea", x = "LMA", rand = "rc", data = PA2, log = "xy",
    subscript = " (a)",
    y_text = expression(paste(italic(A)[area]," (nmol ",g^-1,"",s^-1,")")))


  par(xpd = NA)
  legend(15, 70, c("Sun-Wet", "Sun-Dry", "Shade-Wet", "Shade-Dry", "Null"),
           pch = c(21, 21, 21, 21, 21),
           col = c("dodgerblue3", "burlywood4", "black", "black", "gray"),
           pt.bg = c("white", "white", "dodgerblue3", "burlywood4", "gray"),
           bty = "n",
           # lwd = 1,
           cex = 0.6,
           pt.cex = 1,
           pt.lwd = 0.8,
           ncol = 5)
  par(xpd = TRUE)

  PA.each(y = "Aarea", x = "LMAp_opt", rand = "rc",
    data = PA2, log = "xy", subscript = " (b)")

  PA.each(y = "Aarea", x = "LMAs_opt", rand = "rc",
    data = PA2, log = "xy", subscript = " (c)")

  PA.each(y = "Rarea", x = "LMA", rand = "rc", data = PA2, log = "xy",
    subscript = " (d)",
    y_text = expression(paste(italic(R)[area]," (nmol ",g^-1,"",s^-1,")")))

  PA.each(y = "Rarea", x = "LMAp_opt", rand = "rc",
    data = PA2, log = "xy", subscript = " (e)")

  PA.each(y = "Rarea", x = "LMAs_opt", rand = "rc",
    data = PA2, log = "xy", subscript = " (f)",)

  PA.each(y = "LL", x = "LMA", rand = "rc", data = PA2, log = "xy",
    subscript = " (g)",
    y_text = expression(paste("LL (months)")),
    x_text = expression(paste("LMA (g ",m^-2,")")))

  PA.each(y = "LL", x = "LMAp_opt", rand = "rc",
    data = PA2, log = "xy",
    subscript = " (h)",
    x_text = expression(paste("LMAp (g ",m^-2,")")))

  PA.each(y = "LL", x = "LMAs_opt", rand = "rc",
    data = PA2, log = "xy",
    subscript = " (i)",
    x_text = expression(paste("LMAs (g ",m^-2,")")))

  dev.off()

  # fig.sup1 -

####------------------------

my.cor <- function(x,y,obs=T){

  res.test <- cor.test(log10(x),log10(y))$p.value
  if (res.test < 0.001) aa <- "***" else if (res.test < 0.01) aa <- "**" else if (res.test < 0.05) aa <- "*" else aa <-""

  res.cor <- round(cor(log10(x),log10(y)),2)

  if(obs==T) substitute(paste(italic(r[obs])," = ", m,aa),list(m=res.cor,aa=aa)) else substitute(paste(italic(r[rand])," = ", m,aa),list(m=res.cor,aa=aa))
}

CAN_PNSL <- PA2 %>% filter(strata == "CAN") %>% filter(site == "PNSL")
CAN_PNM <- PA2 %>% filter(strata == "CAN") %>% filter(site == "PNM")
UNDER_PNSL <- PA2 %>% filter(strata == "UNDER") %>% filter(site == "PNSL")
UNDER_PNM <- PA2 %>% filter(strata == "UNDER") %>% filter(site == "PNM")


# fig LL for main text: pot vs opt --------------------------------------
PA_fig <- function(x, y){
  xx <- paste(x, "_ra", sep = "")
  min1 <- min(c(PA2[,y], PA2[,x]))
  max1 <- max(c(PA2[,y], PA2[,x]))

  # plot(PA2[,x], PA2[,y], log = "xy", type = "n", axes = F,
  #   xlim = c(min1*0.5, max1*2),
  #   ylim = c(min1*0.5, max1*2))

  plot(PA2[,x], PA2[,y], log = "xy", type = "n", axes = F,
    xlim = c(0.8, 180),
    ylim = c(0.8, 180))

  box()
  # points(PA2[,xx], PA2[,y], col="gray", pch=19, cex=0.8)
  points(CAN_PNSL[,x], CAN_PNSL[,y], col = "dodgerblue3", pch = 21, bg = "white", lwd = 1.2)
  points(CAN_PNM[,x], CAN_PNM[,y], col = "burlywood4", pch = 21, bg = "white", lwd = 1.2)
  points(UNDER_PNSL[,x], UNDER_PNSL[,y], col = "black", pch = 21, bg = "dodgerblue3", lwd = 0.8)
  points(UNDER_PNM[,x], UNDER_PNM[,y], col = "black", pch = 21, bg = "burlywood4", lwd = 0.8)
}

preLL<-function(){
     par(mfrow=c(1,1))
     par(mar=c(0,0,0,0))
     par(oma=c(3,3,2,2))


     PA_fig("preLL_opt","LL")


     text(50,2, my.cor(PA2$LL,PA2$preLL_opt),cex=0.65)
    #  text(50,1.2, my.cor(PA2$LL,PA2$preLL2_2.ra,obs=F),cex=0.65)
    #  text(50,1.2, expression(paste(italic(r[rand])," = 0.64***")),cex=0.65)


     axis(2,tcl=0.2, labels = FALSE)
     axis(1,tick=FALSE,line=-0.8)
     axis(1,tcl=0.2, labels = FALSE)
     mtext("Predicted LL (months)", side=1, line=1.2,cex=0.8)
     mtext("Optimal LL model", side=3, line = 0.2, font=2)
     text(90,160,"(b)")
     abline(a=0,b=1,lty=2)

}


pdf("~/Dropbox/MS/LES_MS/fig/mogeLL.pdf",width=2.66,height=2.66)
preLL()
dev.off()

##
