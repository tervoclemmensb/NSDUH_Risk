#!/usr/bin/Rscript
library("mgcv",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("dplyr",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("ggplot2",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("tzdb",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2/")
library("vroom",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("emmeans",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")

#####create companion text file for naming levels of variables
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.Rdata")

IRSEX=data.frame(var="IRSEX",levels=c(1,2),labels=c("M","F"))
NEWRACE2=data.frame(var="NEWRACE2",levels=c(1,2,3,4,5,6,7),labels=c("NH W","NH B|AA","NH NA|AKN","NH HI|PI","NH AS","NH MR","HIS"))
INCOME=data.frame(var="INCOME",levels=c(1,2,3,4),labels=c("<20K","20-40K","50-75K",">75K"))
COUTYP2=data.frame(var="COUTYP2",levels=c(1,2,3),labels=c("Metro L","Metro S","Nonmetro"))
#SEXIDENT=data.frame(var="SEXIDENT",levels=c(1,2,3),labels=c("H","L/G","B")) only available in 2019 data####
HEALTH=data.frame(var="HEALTH",levels=c(1,2,3,4,5),labels=c("Excellent","V. Good","Good","Fair","Poor"))
HEALTH2_IMP=data.frame(var="HEALTH2_IMP",levels=c(1,2,3,4),labels=c("Excellent","V. Good","Good","Fair/Poor"))
regiliousity=data.frame(var="regiliousity",levels=seq(1:8),labels=sprintf("Relig.: %s",seq(1:8)))

allvarlabels<-rbind(IRSEX,NEWRACE2,INCOME,COUTYP2,HEALTH,HEALTH2_IMP,regiliousity)

write.csv(allvarlabels,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/studyvarlevellabels.txt")


