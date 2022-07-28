#!/usr/bin/Rscript
library("mgcv",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("dplyr",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("ggplot2",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("tzdb",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2/")
library("vroom",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("emmeans",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("scales",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("survey",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("parallel",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("splines")
library("lsr",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
#http://asdfree.com/national-study-on-drug-use-and-health-nsduh.html
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.Rdata")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0X_splinefuncs.R")
NSDUH20022019_reduceage<-data.frame(NSDUH20022019_reduceage)

fullnsduh_design <- svydesign(id = ~ VEREP , strata = ~ VESTR , data = NSDUH20022019_reduceage , weights = ~ ANALWC18 , nest = TRUE)

risktable<-svytable(~all_RiskDanger+all_RiskTest,fullnsduh_design)
risktableprop<-as.data.frame.matrix(prop.table(risktable)*100)
risktableprop<-as.table(prop.table(risktable)*100)
lsr::cramersV(risktable)

hist(NSDUH20022019_reduceage$riskpropensity)

lunaize<-function (p, ajust = 0.05) 
{
    p$labels$y <- paste0(p$labels$y, "\n")
    p$labels$x <- paste0("\n", p$labels$x)
    p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 20), axis.text.y = element_text(hjust = ajust, 
            color = "black"), axis.text.x = element_text(vjust = ajust, 
            color = "black"))
}

ggfluc <- function(tab) {
  if (is.table(tab)) 
    tab <- as.data.frame(t(tab)) 
  tab <- as.data.frame(tab) 
  oldnames <- names(tab) 
  names(tab) <- c("x", "y", "result") 
  tab <- transform(tab, x = as.factor(x), y = as.factor(y), freq = result) 
  ceiling = max(tab$freq); floor = 0 
  tab <- transform(tab, freq = sqrt(pmin(freq, ceiling)/ceiling), 
                   border = ifelse(is.na(freq), "grey90", ifelse(freq > ceiling, "grey30", "grey50"))) 
  tab[is.na(tab$freq), "freq"] <- 1 
  tab <- subset(tab, freq * ceiling >= floor) 
  nx <- length(levels(tab$x)) 
  ny <- length(levels(tab$y)) 
  p <- ggplot(tab, aes_string(x = "x", y = "y", height = "freq", width = "freq", fill = "result")) + 
    geom_tile(colour = "white")
  p
}

ggriskbyrisk<-ggfluc(risktableprop)+scale_fill_gradient(low = "#374E55FF", high = "#00A1D5FF")
ggriskbyrisk<-lunaize(ggriskbyrisk)+xlab("")+ylab("")+guides(color=guide_colorbar(title="%"))+theme(text=element_text(size=32))

NSDUH20022019_reduceage_add<-NSDUH20022019_reduceage[NSDUH20022019_reduceage$Ageyears %in% c(15,16,17,18),]
fullnsduh_designadd <- svydesign(id = ~ VEREP , strata = ~ VESTR , data = NSDUH20022019_reduceage_add , weights = ~ ANALWC18 , nest = TRUE)

risktableadd<-svytable(~all_RiskDanger+all_RiskTest,fullnsduh_designadd)
lsr::cramersV(risktableadd)

