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
library("ggExtra",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0x_lunaize.R")

load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.Rdata")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0X_splinefuncs.R")
NSDUH20022019_reduceage<-data.frame(NSDUH20022019_reduceage)

#####function########
ageatpeaknomodel<-function(riskvar,agevar,splitvar,comparevar=NULL,df,cores=20,returnpreds=FALSE){
df$agevar<-df[,agevar]
df$riskvar<-df[,riskvar]
if(is.null(comparevar)){
splitmods<-parallel::mclapply(unique(df[,splitvar]),mc.cores=cores,function(split){
print(split)
dfsplit<-df[df[,splitvar]==split,]
print(nrow(dfsplit))
splitnsduh_design <- svydesign(id = ~ VEREP , strata = ~ VESTR , data = dfsplit , weights = ~ ANALWC1 , nest = TRUE)
svyriskbyyear<-svyby(~ riskvar, by = ~ agevar, splitnsduh_design, svymean, na.rm=T)

svyriskbyyear$agepeak<-svyriskbyyear$agevar[which(svyriskbyyear$riskvar==max(svyriskbyyear$riskvar))]
svyriskbyyear$split<-split

return(svyriskbyyear)
})
allsplitmods<-do.call(rbind,splitmods)
outmat<-allsplitmods

}else{
df<-df[!is.na(df[,comparevar]),]
comparebysplitmat<-expand.grid(splitvar=unique(df[,splitvar]),comparevar=unique(df[,comparevar])[!is.na(unique(df[,comparevar]))])

splitmodsbycompare<-parallel::mclapply(1:nrow(comparebysplitmat),mc.cores=cores,function(cbsi){
print(cbsi)
cbsi_df<-df[df[,splitvar]==comparebysplitmat$splitvar[cbsi] & df[,comparevar]==comparebysplitmat$comparevar[cbsi] , ]
baseratepersplit<-nrow(cbsi_df)/length(which(df[,splitvar]==comparebysplitmat$splitvar[cbsi]))

splitnsduh_design <- svydesign(id = ~ VEREP , strata = ~ VESTR , data = cbsi_df , weights = ~ ANALWC1 , nest = TRUE)
svyriskbyyear_bycompsplit<-svyby(~ riskvar, by = ~ agevar, splitnsduh_design, svymean, na.rm=T)

svyriskbyyear_bycompsplit$agepeak<-svyriskbyyear_bycompsplit$agevar[which(svyriskbyyear_bycompsplit$riskvar==max(svyriskbyyear_bycompsplit$riskvar))]
svyriskbyyear_bycompsplit$split<-comparebysplitmat$splitvar[cbsi]
svyriskbyyear_bycompsplit$baserate<-baseratepersplit
svyriskbyyear_bycompsplit$compare<-comparebysplitmat$comparevar[cbsi]
return(svyriskbyyear_bycompsplit)

})
allsplitmodsbycompare<-do.call(rbind,splitmodsbycompare)
outmat<-allsplitmodsbycompare
}
outmat$riskvar<-riskvar
outmat$agevarname<-agevar
outmat$splitvar<-splitvar
outmat$comparevar<-comparevar

return(outmat)
}

################

varlabels<-read.csv(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/studyvarlevellabels.txt")
varlabels$comparevar<-varlabels$var
varlabels$compare<-varlabels$levels
varlabels_merge<-varlabels[,c("comparevar","compare","labels")]

varlabel_relig<-data.frame(comparevar="religionbin",compare=c("Low","Moderate","High"),labels=c("Low","Moderate","High"))
varlabel_dep<-data.frame(comparevar="MDEYR_binbyage",compare=c(0,1),labels=c("No DEP","DEP"))

varlabels_merge<-rbind(varlabels_merge,varlabel_relig,varlabel_dep)


######################
frm<-as.formula("riskpropensity~s(predvar)+s(YEARfactor,bs='re')")
NSDUH20022019_reduceage$YEARfactor<-as.factor(NSDUH20022019_reduceage$YEAR)

#####Years#######
years_ageatpeak<-ageatpeaknomodel(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",df=NSDUH20022019_reduceage)
save(years_ageatpeak,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/agemeanpeak/riskpeaksbyyear.Rdata")
###SEX########
sex_ageatpeak<-ageatpeaknomodel(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="IRSEX",df=NSDUH20022019_reduceage)
sex_ageatpeak<-merge(sex_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
####RACE#########
NSDUH20022019_reduceage_1_2_5_6_7race<-NSDUH20022019_reduceage[NSDUH20022019_reduceage$NEWRACE2 %in% c("1","2","5","6","7"),]
race_ageatpeak<-ageatpeaknomodel(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="NEWRACE2",df=NSDUH20022019_reduceage_1_2_5_6_7race)
race_ageatpeak<-merge(race_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
#####income#######
income_ageatpeak<-ageatpeaknomodel(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="INCOME",df=NSDUH20022019_reduceage)
income_ageatpeak<-merge(income_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
###COUTYP2########
NSDUH20022019_reduceage_couty<-NSDUH20022019_reduceage[!(NSDUH20022019_reduceage$YEAR %in% c(2015,2016,2017,2018,2019)),]
COUTY_ageatpeak<-ageatpeaknomodel(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="COUTYP2",df=NSDUH20022019_reduceage_couty)
COUTY_ageatpeak<-merge(COUTY_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
###regiliousity######
cumsum(table(NSDUH20022019_reduceage$regiliousity))/nrow(NSDUH20022019_reduceage)
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(1,2,3,4)]<-"Low"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(5,6)]<-"Moderate"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(7,8)]<-"High"

religion_ageatpeak<-ageatpeaknomodel(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="religionbin",df=NSDUH20022019_reduceage)
religion_ageatpeak<-merge(religion_ageatpeak,varlabels_merge,by=c("comparevar","compare"))
#######DEP###
NSDUH20022019_reduceage_DEP<-NSDUH20022019_reduceage[!is.na(NSDUH20022019_reduceage$MDEYR_binbyage),]
dep_ageatpeak<-ageatpeaknomodel(agevar="Ageyears",riskvar="riskpropensity",splitvar="YEARfactor",comparevar="MDEYR_binbyage",df=NSDUH20022019_reduceage_DEP)
dep_ageatpeak<-merge(dep_ageatpeak,varlabels_merge,by=c("comparevar","compare"))


allagepeaks<-plyr::rbind.fill(sex_ageatpeak,race_ageatpeak) %>% plyr::rbind.fill(.,income_ageatpeak) %>% plyr::rbind.fill(.,COUTY_ageatpeak) %>%
plyr::rbind.fill(.,religion_ageatpeak) %>% plyr::rbind.fill(.,dep_ageatpeak)

save(allagepeaks,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/agemeanpeak/allpeaks.Rdata")

####################################
####################################

load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/agemeanpeak/allpeaks.Rdata")
allagepeaksmeans<-allagepeaks
allagepeaksmeans$peaktype<-"means"
allagepeaksmeans$peakfrommean<-allagepeaksmeans$agepeak

allagepeaksmeans_reduce<-allagepeaksmeans %>% group_by(comparevar,compare,split) %>% summarize(peakfrommean=mean(peakfrommean))

load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/agefitpeaks/allpeaks.Rdata")
allagepeaksplines<-allagepeaks
allagepeaksplines<-allagepeaksplines[allagepeaksplines$type=="svy",]
allagepeaksplines$peaktype<-"spline"
allagepeaksplines$peakfromspline<-allagepeaksplines$ageatpeak

mergevars<-c("comparevar","compare","split")

meansandsplines<-merge(allagepeaksmeans_reduce[,c(mergevars,"peakfrommean")],allagepeaksplines[,c(mergevars,"peakfromspline")],by=c("comparevar","compare","split"))

splinewithmean<-ggplot(meansandsplines,aes(x=peakfrommean,y=peakfromspline))+geom_point(alpha=.33)+geom_smooth(method="lm",color = "black")
splinewithmean<-lunaize(splinewithmean)+xlab("Peak from Mean Estimate")+ylab("Peak from Lifespan Spline Estimate")
ggsplinewithmean<-ggExtra::ggMarginal(splinewithmean, type = "histogram")
ggsave(ggsplinewithmean,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/splinewithmean.pdf",height=6,width=6)

median(meansandsplines$peakfromspline)
median(meansandsplines$peakfrommean)

meansandsplines %>% group_by(comparevar,compare) %>% summarize(medianmean=median(peakfrommean),medianspline=median(peakfromspline))

length(which(meansandsplines$peakfrommean >=15 & meansandsplines$peakfrommean <= 18))/nrow(meansandsplines)
length(which(meansandsplines$peakfromspline >=15 & meansandsplines$peakfromspline <= 18))/nrow(meansandsplines)


