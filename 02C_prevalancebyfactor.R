library("mgcv",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("dplyr",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("ggplot2",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("tzdb",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2/")
library("vroom",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("emmeans",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("scales",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("survey",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("sjstats",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("pscl",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("parallel",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("jtools",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("remotes",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("splines")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0x_lunaize.R")
#http://asdfree.c om/national-study-on-drug-use-and-health-nsduh.html
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.Rdata")
NSDUH20022019_reduceage<-data.frame(NSDUH20022019_reduceage)
varlabels<-read.csv(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/studyvarlevellabels.txt")
varlabels$comparevar<-varlabels$var
varlabels$compare<-varlabels$levels
varlabels_merge<-varlabels[,c("comparevar","compare","labels")]
#######################
NSDUH20022019_reduceage$agebin<-15
NSDUH20022019_reduceage$agebin[NSDUH20022019_reduceage$Ageyears > 18 & NSDUH20022019_reduceage$Ageyears <= 25]<-21.5
NSDUH20022019_reduceage$agebin[NSDUH20022019_reduceage$Ageyears> 25 ]<-median(NSDUH20022019_reduceage$Ageyears[NSDUH20022019_reduceage$Ageyears> 25])
NSDUH20022019_reduceage$cannreg<-dplyr::if_else(NSDUH20022019_reduceage$IRMJFY> 240,1,0)
NSDUH20022019_reduceage$cannexp<-dplyr::if_else(NSDUH20022019_reduceage$IRMJFY> 0,1,0)
NSDUH20022019_reduceage$alcreg<-dplyr::if_else(NSDUH20022019_reduceage$IRALCFY> 240,1,0)
NSDUH20022019_reduceage$pastyearbook<-dplyr::if_else(NSDUH20022019_reduceage$NOBOOKY2 > 0,1,0)
NSDUH20022019_reduceage$YESTSMJ_IMP<-NSDUH20022019_reduceage$YESTSMJ
NSDUH20022019_reduceage$YESTSMJ_IMP[NSDUH20022019_reduceage$YESTSMJ_IMP==4]<-3
####check data#####

summary(NSDUH20022019_reduceage[,c("Ageyears","agebin","riskpropensity","NEWRACE2","INCOME","COUTYP2","HEALTH2_IMP","regiliousity")])


NSDUH20022019_reduceage[,c("NEWRACE2","INCOME","COUTYP2","HEALTH2_IMP","YESTSMJ_IMP")]<-lapply(NSDUH20022019_reduceage[,c("NEWRACE2","INCOME","COUTYP2","HEALTH2_IMP","YESTSMJ_IMP")],function(x){as.factor(as.character(x))})

summary(NSDUH20022019_reduceage[,c("Ageyears","agebin","riskpropensity","NEWRACE2","INCOME","COUTYP2","HEALTH2_IMP","regiliousity","YESTSMJ_IMP")])
####################prev######

prevalance_byfactor<-function(outcome,comparevar,agevar,df){
nyears<-length(unique(df$YEAR))
df$splitsweight<-df[,sprintf("ANALWC%s",nyears)]
allagesmods<-lapply(unique(df[,agevar]),function(ai){
acrosscompare<-lapply(unique(df[,comparevar][!is.na(df[,comparevar])]),function(cli){
aidf<-df[df[,agevar]==ai & df[,comparevar]==cli,]
aidf$outcome<-aidf[,outcome]
aidf<-aidf[!is.na(aidf$VEREP),]
print(ai)
print(cli)
print(dim(aidf))
options(survey.lonely.psu="remove")
fulldesign<-svydesign(id = ~ VEREP , strata = ~ VESTR , data = aidf , weights = ~ splitsweight , nest = TRUE)

prop<-data.frame(svymean(~outcome,fulldesign,na.rm=TRUE))
returndata<-data.frame(comparevar=comparevar,prev=prop$mean,se=prop$outcome)
returndata[,comparevar]<-cli
returndata$comparelevel<-cli
returndata[,agevar]<-ai

return(returndata)
})
acrosscomparemat<-do.call(rbind,acrosscompare)
return(acrosscomparemat)
})
allmods<-do.call(rbind,allagesmods)
return(allmods)
}

############
prevbysex<-prevalance_byfactor("cannreg","IRSEX","agebin",NSDUH20022019_reduceage)
prevbyincome<-prevalance_byfactor("cannreg","INCOME","agebin",NSDUH20022019_reduceage)
prevbyrace<-prevalance_byfactor("cannreg","NEWRACE2","agebin",NSDUH20022019_reduceage)

############
NSDUH20022019_reduceage_couty<-NSDUH20022019_reduceage[!(NSDUH20022019_reduceage$YEAR %in% c(2015,2016,2017,2018,2019)),]
prevbycouty<-prevalance_byfactor("cannreg","COUTYP2","agebin",NSDUH20022019_reduceage_couty)
#########
NSDUH20022019_reduceage_HEALTH2_imp<-NSDUH20022019_reduceage[!(NSDUH20022019_reduceage$YEAR %in% c(2002,2003)),]
prevybyhealth<-prevalance_byfactor("cannreg","HEALTH2_IMP","agebin",NSDUH20022019_reduceage_HEALTH2_imp)

############
cumsum(table(NSDUH20022019_reduceage$regiliousity))/nrow(NSDUH20022019_reduceage)
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(1,2,3,4)]<-"Low"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(5,6)]<-"Moderate"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(7,8)]<-"High"
prevbyreligion<-prevalance_byfactor("cannreg","religionbin","agebin",NSDUH20022019_reduceage)

############
NSDUH20022019_reduceage_DEP<-NSDUH20022019_reduceage[!is.na(NSDUH20022019_reduceage$MDEYR_binbyage),]
prevbydep<-prevalance_byfactor("cannreg","MDEYR_binbyage","agebin",NSDUH20022019_reduceage_DEP)

#####all prev cann reg  by age##########

allprev<-plyr::rbind.fill(prevbysex,prevbyincome) %>% plyr::rbind.fill(.,prevbyrace) %>% plyr::rbind.fill(.,prevbycouty) %>% plyr::rbind.fill(.,prevybyhealth) %>% plyr::rbind.fill(.,prevbyreligion) %>% plyr::rbind.fill(.,prevbydep)

save(allprev,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/Demo/allprev.cannreg.Rdata")





