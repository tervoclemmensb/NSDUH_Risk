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
#http://asdfree.com/national-study-on-drug-use-and-health-nsduh.html
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.Rdata")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0X_splinefuncs.R")
NSDUH20022019_reduceage<-data.frame(NSDUH20022019_reduceage)
varlabels<-read.csv(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/studyvarlevellabels.txt")
varlabels$comparevar<-varlabels$var
varlabels$compare<-varlabels$levels
varlabels_merge<-varlabels[,c("comparevar","compare","labels")]

varlabel_relig<-data.frame(comparevar="religionbin",compare=c("Low","Moderate","High"),labels=c("Low","Moderate","High"))
varlabel_dep<-data.frame(comparevar="MDEYR_binbyage",compare=c(0,1),labels=c("No DEP","DEP"))

varlabels_merge<-rbind(varlabels_merge,varlabel_relig,varlabel_dep)


######################
frm<-as.formula("riskpropensity~s(predvar)+s(YEARfactor,bs='re')")
####age at peaks main effects#########
NSDUH20022019_reduceage$YEARfactor<-as.factor(NSDUH20022019_reduceage$YEAR)
 

#####Years#######
years_ageatpeak<-bamandsvyfitandpeakbysplit_sf(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",df=NSDUH20022019_reduceage)
save(years_ageatpeak,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/agefitpeaks/riskpeaksbyyear.Rdata")
###SEX########
sex_ageatpeak<-bamandsvyfitandpeakbysplit_sf(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="IRSEX",df=NSDUH20022019_reduceage)
sex_ageatpeak<-merge(sex_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
sex_fullmodelfit<-bamfitsvynestedbycomparevarlevel_sf(frm,predvarname="Ageyears",comparevar="IRSEX",df=NSDUH20022019_reduceage)
sex_fullmodelfit<-merge(sex_fullmodelfit,varlabels_merge,by=c("comparevar","compare"))
####RACE#########
NSDUH20022019_reduceage_1_2_5_6_7race<-NSDUH20022019_reduceage[NSDUH20022019_reduceage$NEWRACE2 %in% c("1","2","5","6","7"),]
race_ageatpeak<-bamandsvyfitandpeakbysplit_sf(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="NEWRACE2",df=NSDUH20022019_reduceage_1_2_5_6_7race)
race_ageatpeak<-merge(race_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
race_fullmodelfit<-bamfitsvynestedbycomparevarlevel_sf(frm,predvarname="Ageyears",comparevar="NEWRACE2",df=NSDUH20022019_reduceage_1_2_5_6_7race)
race_fullmodelfit<-merge(race_fullmodelfit,varlabels_merge,by=c("comparevar","compare"))
#####income#######
income_ageatpeak<-bamandsvyfitandpeakbysplit_sf(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="INCOME",df=NSDUH20022019_reduceage)
income_ageatpeak<-merge(income_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
income_fullmodelfit<-bamfitsvynestedbycomparevarlevel_sf(frm,predvarname="Ageyears",comparevar="INCOME",df=NSDUH20022019_reduceage)
income_fullmodelfit<-merge(income_fullmodelfit,varlabels_merge,by=c("comparevar","compare"))
###COUTYP2########
NSDUH20022019_reduceage_couty<-NSDUH20022019_reduceage[!(NSDUH20022019_reduceage$YEAR %in% c(2015,2016,2017,2018,2019)),]
COUTY_ageatpeak<-bamandsvyfitandpeakbysplit_sf(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="COUTYP2",df=NSDUH20022019_reduceage_couty)
COUTY_ageatpeak<-merge(COUTY_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
COUTY_fullmodelfit<-bamfitsvynestedbycomparevarlevel_sf(frm,predvarname="Ageyears",comparevar="COUTYP2",df=NSDUH20022019_reduceage_couty)
COUTY_fullmodelfit<-merge(COUTY_fullmodelfit,varlabels_merge,by=c("comparevar","compare"))
###regiliousity######
cumsum(table(NSDUH20022019_reduceage$regiliousity))/nrow(NSDUH20022019_reduceage)
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(1,2,3,4)]<-"Low"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(5,6)]<-"Moderate"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(7,8)]<-"High"

religion_ageatpeak<-bamandsvyfitandpeakbysplit_sf(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="religionbin",df=NSDUH20022019_reduceage)
religion_ageatpeak<-merge(religion_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
religion_fullmodelfit<-bamfitsvynestedbycomparevarlevel_sf(frm,predvarname="Ageyears",comparevar="religionbin",df=NSDUH20022019_reduceage)
religion_fullmodelfit<-merge(religion_fullmodelfit,varlabels_merge,by=c("comparevar","compare"))

#######DEP###
NSDUH20022019_reduceage_DEP<-NSDUH20022019_reduceage[!is.na(NSDUH20022019_reduceage$MDEYR_binbyage),]

dep_ageatpeak<-bamandsvyfitandpeakbysplit_sf(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="MDEYR_binbyage",df=NSDUH20022019_reduceage_DEP)
dep_ageatpeak<-merge(dep_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
dep_fullmodelfit<-bamfitsvynestedbycomparevarlevel_sf(frm,predvarname="Ageyears",comparevar="MDEYR_binbyage",df=NSDUH20022019_reduceage_DEP)
dep_fullmodelfit<-merge(dep_fullmodelfit,varlabels_merge,by=c("comparevar","compare"))

#####bind all peaks########

#allagepeaks<-plyr::rbind.fill(sex_ageatpeak,race_ageatpeak) %>% plyr::rbind.fill(.,income_ageatpeak) %>% plyr::rbind.fill(.,COUTY_ageatpeak) %>%
#plyr::rbind.fill(.,religion_ageatpeak) %>% plyr::rbind.fill(.,health_ageatpeak) %>% plyr::rbind.fill(.,dep_ageatpeak)

allagepeaks<-plyr::rbind.fill(sex_ageatpeak,race_ageatpeak) %>% plyr::rbind.fill(.,income_ageatpeak) %>% plyr::rbind.fill(.,COUTY_ageatpeak) %>%
plyr::rbind.fill(.,religion_ageatpeak) %>% plyr::rbind.fill(.,dep_ageatpeak)

save(allagepeaks,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/agefitpeaks/allpeaks.Rdata")


#####bind all fits########

#allfits<-plyr::rbind.fill(sex_fullmodelfit,race_fullmodelfit) %>% plyr::rbind.fill(.,income_fullmodelfit) %>% plyr::rbind.fill(.,COUTY_fullmodelfit) %>% plyr::rbind.fill(.,religion_fullmodelfit) %>% plyr::rbind.fill(.,health_fullmodelfit) %>% plyr::rbind.fill(.,dep_fullmodelfit)

allfits<-plyr::rbind.fill(sex_fullmodelfit,race_fullmodelfit) %>% plyr::rbind.fill(.,income_fullmodelfit) %>% plyr::rbind.fill(.,COUTY_fullmodelfit) %>% plyr::rbind.fill(.,religion_fullmodelfit) %>% plyr::rbind.fill(.,dep_fullmodelfit)

save(allfits,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/agefitpeaks/allfits.Rdata")


