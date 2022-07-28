#!/usr/bin/Rscript
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
library("pROC",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("DescTools",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("psych",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("splines")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0x_lunaize.R")
#http://asdfree.c om/national-study-on-drug-use-and-health-nsduh.html
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.2021112.Rdata")
NSDUH20022019_reduceage<-data.frame(NSDUH20022019_reduceage)
varlabels<-read.csv(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/studyvarlevellabels.txt")
varlabels$comparevar<-varlabels$var
varlabels$compare<-varlabels$levels
varlabels_merge<-varlabels[,c("comparevar","compare","labels")]
##############################################################
#############
######generate models for Figure D
######cohen D with cov models#######
byagepredict_comparevarbysplitvarCD_cov<-function(frmwithrisk,groupvar,agevar,comparevar,splitvar,df,mc.cores=10){
outcome<-as.character(frmwithrisk)[2]
nsplits<-length(unique(df[,splitvar]))
print(sprintf("ANALWC%s",nsplits-1))
df$splitsweight<-df[,sprintf("ANALWC%s",nsplits-1)]

acrossyears<-mclapply(unique(df[,splitvar]),mc.cores=mc.cores,function(sli){
print(sprintf("splitvar level: %s",sli))
allcomparelevels<-lapply(unique(df[,comparevar][!is.na(df[,comparevar])]),function(cli){
print(cli)
allagesmods<-lapply(unique(df[,agevar]),function(ai){
print(ai)
aidf<-df[df[,agevar]==ai & df[,comparevar]==cli & df[,splitvar]!=sli,]
aidf<-aidf[!is.na(aidf[,outcome]),]
aidf$outcome<-aidf[,outcome]
aidftest<-df[df[,agevar]==ai & df[,comparevar]==cli & df[,splitvar]==sli,]
aidftest<-aidftest[!is.na(aidftest[,outcome]),]
aidftest$outcome<-aidftest[,outcome]

propoutcomeai<-sum(aidf[,outcome],na.rm=TRUE)/nrow(aidf)
print(sprintf("%s obs for %s comparevar level at %s agebin, %s cases in outcome: %s per, at %s split",nrow(aidf),cli,ai,sum(aidf[,groupvar],na.rm=TRUE),propoutcomeai,sli))

varsinmodelcheck<-labels(terms(frmwithrisk))[!grepl("ns[(]",labels(terms(frmwithrisk)))]
allnacols<-names(which(colSums(is.na(aidf[,varsinmodelcheck])) == nrow(aidf[,varsinmodelcheck])))
allnacolstest<-names(which(colSums(is.na(aidftest[,varsinmodelcheck])) == nrow(aidftest[,varsinmodelcheck])))
allnacols<-c(allnacols,allnacolstest)

testterms<-unique(attr(terms.formula(frmwithrisk),"term.labels"))#
testterms<-testterms[!grepl("ns[(]",testterms)]
testterms<-testterms[!(testterms %in% allnacols)]

aidf$outcome<-aidf[,outcome]
aidfcomplete<-aidf[complete.cases(aidf[,c("outcome","Ageyears",testterms,"VEREP","VESTR","splitsweight")]),] #
print(sprintf("%s complete rows",nrow(aidfcomplete)))#

airsqs<-data.frame(type=sprintf("CVby%s",splitvar))
if (length(allnacols)!=0){
print(sprintf("%s all NA for this year, compare, age bin, removing from models",allnacols))
frmwithriskup<-as.formula(sprintf("%s~%s",outcome,as.character(paste0(labels(terms(frmwithrisk))[!(labels(terms(frmwithrisk)) %in% allnacols)],collapse="+"))))
airsqs$nacols<-as.character(paste(allnacols,collapse="+"))
}else{
frmwithriskup<-frmwithrisk
airsqs$nacols<-NA
}
frmwithriskup<-as.formula(sprintf("%s~%s",outcome,gsub(groupvar,"groupvar",frmwithriskup)[3]))
aidfcomplete$groupvar<-aidfcomplete[,groupvar]

options(survey.lonely.psu="remove")
aidfcomplete$riskpropensity<-scale(aidfcomplete$riskpropensity)

fulldesign<-svydesign(id = ~ VEREP , strata = ~ VESTR , data = aidfcomplete , weights = ~ splitsweight , nest = TRUE)
print("design made")
print(dim(fulldesign$variables))

###Full model#############full model excludes year SLI, includes all other years
print("primary model made")

fullmod<-svyglm(frmwithriskup,fulldesign)

airsqs$cohendfromlm<-unlist(emmeans::emmeans(fullmod,~groupvar) %>% summary %>% dplyr::summarize(diff(emmean)))
paircompare<-data.frame(pairs(emmeans::emmeans(fullmod,~groupvar)))
airsqs$cohendfromlm_SE<-paircompare$SE
airsqs$cohendfromlm_pval<-paircompare$p.value

airsqs[,agevar]<-ai
airsqs[,comparevar]<-cli
airsqs[,splitvar]<-sli

####Test model###test model includes only SLI (i.e. Figure D)

aidftest$outcome<-aidftest[,outcome]
aidftestcomplete<-aidftest[complete.cases(aidftest[,c("outcome","Ageyears",testterms,"VEREP","VESTR")]),]

aidftestcomplete$groupvar<-aidftestcomplete[,groupvar]
aidftestcomplete$riskpropensity<-scale(aidftestcomplete$riskpropensity)
testdesign<-svydesign(id = ~ VEREP , strata = ~ VESTR , data = aidftestcomplete , weights = ~ ANALWC1 , nest = TRUE)

testfullmod<-svyglm(frmwithriskup,testdesign)

airsqs$cohendtestfromlm<-unlist(emmeans::emmeans(testfullmod,~groupvar) %>% summary %>% dplyr::summarize(diff(emmean)))
testpaircompare<-data.frame(pairs(emmeans::emmeans(testfullmod,~groupvar)))
airsqs$cohendtestfromlm_SE<-testpaircompare$SE
airsqs$cohendtestfromlm_pval<-testpaircompare$p.value

testnocovmod<-svyglm(riskpropensity~groupvar,testdesign)

airsqs$cohendtestfromlm_unadj<-unlist(emmeans::emmeans(testnocovmod,~groupvar) %>% summary %>% dplyr::summarize(diff(emmean)))
testpaircompare_unadj<-data.frame(pairs(emmeans::emmeans(testnocovmod,~groupvar)))
airsqs$cohendtestfromlm_unadj_pval<-testpaircompare_unadj$p.value

print(sprintf("test has %s obs for %s comparevar level at %s agebin, %s cases in outcome  at %s split",nrow(aidftestcomplete),cli,ai,sum(aidftestcomplete[,groupvar],na.rm=TRUE),sli))

airsqs$ntrain<-nrow(aidfcomplete)
airsqs$ntrain_cases<-sum(aidfcomplete[,groupvar],na.rm=TRUE)

airsqs$ntest<-nrow(aidftestcomplete)
airsqs$ntest_cases<-sum(aidftestcomplete[,groupvar],na.rm=TRUE)
airsqs$mtype<-"cov"
return(airsqs)
})
allagesmat<-do.call(rbind,allagesmods)
})
allcomparelevelsmat<-do.call(rbind,allcomparelevels)
return(allcomparelevelsmat)
})
yearsmat<-do.call(rbind,acrossyears)
return(yearsmat)
}

#######
#####
NSDUH20022019_reduceage$agebin[NSDUH20022019_reduceage$Ageyears %in% seq(15,18)]<-"add"
NSDUH20022019_reduceage <- NSDUH20022019_reduceage %>% mutate(canngroup=case_when(IRMJFY==0 ~ "non-user",IRMJFY<52 ~"lessthanreg", IRMJFY>= 52 ~"reg"))
NSDUH20022019_reduceage <- NSDUH20022019_reduceage %>% mutate(alcgroup=case_when(IRALCFY==0 ~ "non-user",IRALCFY<52 ~"lessthanreg", IRALCFY>= 52 ~"reg"))
NSDUH20022019_reduceage$cannreg<-dplyr::if_else(NSDUH20022019_reduceage$IRMJFY>= 52 & NSDUH20022019_reduceage$IRMJRC_bin==1,1,0)
NSDUH20022019_reduceage$alcreg<-dplyr::if_else(NSDUH20022019_reduceage$IRALCFY>=52 & NSDUH20022019_reduceage$IRALCRC_bin==1,1,0)
cumsum(table(NSDUH20022019_reduceage$regiliousity))/nrow(NSDUH20022019_reduceage)
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(1,2,3,4)]<-"Low"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(5,6)]<-"Moderate"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(7,8)]<-"High"

###reudce to adolescents only#####

NSDUH20022019_reduceage<-NSDUH20022019_reduceage[!is.na(NSDUH20022019_reduceage$agebin) & NSDUH20022019_reduceage$agebin=="add",]
NSDUH20022019_reduceage<-NSDUH20022019_reduceage[!is.na(NSDUH20022019_reduceage$YEAR),]

###save full for subsequent alcohol models
full_NSDUH20022019_reduceage<-NSDUH20022019_reduceage 

####reduce primary for cannreg versus non user
NSDUH20022019_reduceage<-full_NSDUH20022019_reduceage[full_NSDUH20022019_reduceage$canngroup!="lessthanreg",]
###remove reg users (based on yearly use days)  who deny past month use
NSDUH20022019_reduceage<-NSDUH20022019_reduceage[!(NSDUH20022019_reduceage$canngroup=="reg" & NSDUH20022019_reduceage$cannreg==0),]
##clean up data with missing year values
NSDUH20022019_reduceage<-NSDUH20022019_reduceage[!is.na(NSDUH20022019_reduceage$YEAR),]
####check data#####


NSDUH20022019_reduceage[,c("NEWRACE2","INCOME","COUTYP2","MDEYR_binbyage","IRSEX","religionbin")]<-lapply(NSDUH20022019_reduceage[,c("NEWRACE2","INCOME","COUTYP2","MDEYR_binbyage","IRSEX","religionbin")],function(x){as.factor(as.character(x))})

summary(NSDUH20022019_reduceage[,c("Ageyears","agebin","riskpropensity","NEWRACE2","INCOME","COUTYP2","regiliousity","MDEYR_binbyage","IRSEX","religionbin","canngroup","cannreg","YEAR")])

##############
########CANN REG############
##################

####by year######
byyearmodel<-as.formula("riskpropensity~IRSEX+cannreg+NEWRACE2+INCOME+COUTYP2+religionbin+MDEYR_binbyage")
NSDUH20022019_reduceage$dummy<-1 ###dummy var prompts byagepredict_comparevarbysplitvarCD_cov to run across the full sample
canregbyyear<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=byyearmodel,groupvar="cannreg",agevar="agebin",comparevar="dummy",splitvar="YEAR",df=NSDUH20022019_reduceage,mc.cores=20)
save(canregbyyear,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/canregbyyearbyage_20211030.Rdata")

###by strata#######
###sex###################
###can reg########

irsexmodel<-as.formula("riskpropensity~cannreg+NEWRACE2+INCOME+COUTYP2+religionbin+MDEYR_binbyage")
cannregbysex<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=irsexmodel,groupvar="cannreg",agevar="agebin",comparevar="IRSEX",splitvar="YEAR",df=NSDUH20022019_reduceage,mc.cores=20)

save(cannregbysex,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbysexbyage_20211030.Rdata")

###income################
###can reg#######

incomemodel<-as.formula("riskpropensity~IRSEX+cannreg+NEWRACE2+COUTYP2+religionbin+MDEYR_binbyage")
cannregbyincome<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=incomemodel,groupvar="cannreg",agevar="agebin",comparevar="INCOME",splitvar="YEAR",df=NSDUH20022019_reduceage)
save(cannregbyincome,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbyincomebyage_20211030.Rdata")


###RACE##############
###can reg########
NSDUH20022019_reduceage_1_2_5_6_7race<-NSDUH20022019_reduceage[NSDUH20022019_reduceage$NEWRACE2 %in% c("1","2","5","6","7"),]
racemodel<-as.formula("riskpropensity~IRSEX+cannreg+INCOME+COUTYP2+religionbin+MDEYR_binbyage")
cannregbyrace<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=racemodel,groupvar="cannreg",agevar="agebin",comparevar="NEWRACE2",splitvar="YEAR",df=NSDUH20022019_reduceage_1_2_5_6_7race,mc.cores=20)
save(cannregbyrace,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbyracebyage_20211030.Rdata")

#####COUTYP2#############
NSDUH20022019_reduceage_couty<-NSDUH20022019_reduceage[!(NSDUH20022019_reduceage$YEAR %in% c(2015,2016,2017,2018,2019)),]

coutymodel<-as.formula("riskpropensity~IRSEX+cannreg+INCOME+NEWRACE2+religionbin+MDEYR_binbyage")
cannregbyrcouty<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=coutymodel,groupvar="cannreg",agevar="agebin",comparevar="COUTYP2",splitvar="YEAR",df=NSDUH20022019_reduceage_couty)
save(cannregbyrcouty,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbycoutybyage_20211030.Rdata")

###regiliousity#####

religmodel<-as.formula("riskpropensity~IRSEX+cannreg+INCOME+NEWRACE2+COUTYP2+MDEYR_binbyage")
cannregbyrelig<-byagepredict_comparevarbysplitvarCD_cov(frmwith=religmodel,groupvar="cannreg",agevar="agebin",comparevar="religionbin",splitvar="YEAR",df=NSDUH20022019_reduceage)
save(cannregbyrelig,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbyreligbyage_20211030.Rdata")

#####dep#############

NSDUH20022019_reduceage_DEP<-NSDUH20022019_reduceage[!is.na(NSDUH20022019_reduceage$MDEYR_binbyage),]
depmodel<-as.formula("riskpropensity~IRSEX+cannreg+INCOME+NEWRACE2+COUTYP2+religionbin")
cannregbydep<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=depmodel,groupvar="cannreg",agevar="agebin",comparevar="MDEYR_binbyage",splitvar="YEAR",df=NSDUH20022019_reduceage_DEP)
save(cannregbydep,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbydepbyage_20211030.Rdata")

##############
########ALC REG############
##################
####reduce primary for cannreg versus non user
NSDUH20022019_reduceage<-full_NSDUH20022019_reduceage[full_NSDUH20022019_reduceage$alcgroup!="lessthanreg",]
###remove reg users (based on yearly use days)  who deny past month use
NSDUH20022019_reduceage<-NSDUH20022019_reduceage[!(NSDUH20022019_reduceage$alcgroup=="reg" & NSDUH20022019_reduceage$alcreg==0),]
##clean up data with missing year values
NSDUH20022019_reduceage<-NSDUH20022019_reduceage[!is.na(NSDUH20022019_reduceage$YEAR),]

####check data#####

NSDUH20022019_reduceage[,c("NEWRACE2","INCOME","COUTYP2","MDEYR_binbyage","IRSEX","religionbin")]<-lapply(NSDUH20022019_reduceage[,c("NEWRACE2","INCOME","COUTYP2","MDEYR_binbyage","IRSEX","religionbin")],function(x){as.factor(as.character(x))})

summary(NSDUH20022019_reduceage[,c("Ageyears","agebin","riskpropensity","NEWRACE2","INCOME","COUTYP2","regiliousity","MDEYR_binbyage","IRSEX","religionbin","canngroup","cannreg","YEAR")])

####by year######

byyearmodel<-as.formula("riskpropensity~IRSEX+alcreg+NEWRACE2+INCOME+COUTYP2+religionbin+MDEYR_binbyage")
NSDUH20022019_reduceage$dummy<-1
alcregbyyear<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=byyearmodel,groupvar="alcreg",agevar="agebin",comparevar="dummy",splitvar="YEAR",df=NSDUH20022019_reduceage,mc.cores=20)
save(alcregbyyear,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbyyearbyage_20211030.Rdata")


###by strata#######
###sex###################
###alc reg########

irsexmodel<-as.formula("riskpropensity~alcreg+NEWRACE2+INCOME+COUTYP2+religionbin+MDEYR_binbyage")
alcregbysex<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=irsexmodel,groupvar="alcreg",agevar="agebin",comparevar="IRSEX",splitvar="YEAR",df=NSDUH20022019_reduceage,mc.cores=20)

save(alcregbysex,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbysexbyage_20211030.Rdata")

###income################
###can reg#######
incomemodel<-as.formula("riskpropensity~IRSEX+alcreg+NEWRACE2+COUTYP2+religionbin+MDEYR_binbyage")
alcregbyincome<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=incomemodel,groupvar="alcreg",agevar="agebin",comparevar="INCOME",splitvar="YEAR",df=NSDUH20022019_reduceage)
save(alcregbyincome,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbyincomebyage_20211030.Rdata")

###RACE##############
###can reg########

NSDUH20022019_reduceage_1_2_5_6_7race<-NSDUH20022019_reduceage[NSDUH20022019_reduceage$NEWRACE2 %in% c("1","2","5","6","7"),]
racemodel<-as.formula("riskpropensity~IRSEX+alcreg+INCOME+COUTYP2+religionbin+MDEYR_binbyage")
alcregbyrace<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=racemodel,groupvar="alcreg",agevar="agebin",comparevar="NEWRACE2",splitvar="YEAR",df=NSDUH20022019_reduceage_1_2_5_6_7race,mc.cores=20)
save(alcregbyrace,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbyracebyage_20211030.Rdata")

#####COUTYP2#############
NSDUH20022019_reduceage_couty<-NSDUH20022019_reduceage[!(NSDUH20022019_reduceage$YEAR %in% c(2015,2016,2017,2018,2019)),]
coutymodel<-as.formula("riskpropensity~IRSEX+alcreg+INCOME+NEWRACE2+religionbin+MDEYR_binbyage")
alcregbyrcouty<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=coutymodel,groupvar="alcreg",agevar="agebin",comparevar="COUTYP2",splitvar="YEAR",df=NSDUH20022019_reduceage_couty)
save(alcregbyrcouty,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbycoutybyage_20211030.Rdata")

###regiliousity#####
religmodel<-as.formula("riskpropensity~IRSEX+alcreg+INCOME+NEWRACE2+COUTYP2+MDEYR_binbyage")
alcregbyrelig<-byagepredict_comparevarbysplitvarCD_cov(frmwith=religmodel,groupvar="alcreg",agevar="agebin",comparevar="religionbin",splitvar="YEAR",df=NSDUH20022019_reduceage)
save(alcregbyrelig,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbyreligbyage_20211030.Rdata")

#####dep#############

NSDUH20022019_reduceage_DEP<-NSDUH20022019_reduceage[!is.na(NSDUH20022019_reduceage$MDEYR_binbyage),]
depmodel<-as.formula("riskpropensity~IRSEX+alcreg+INCOME+NEWRACE2+COUTYP2+religionbin")
alcregbydep<-byagepredict_comparevarbysplitvarCD_cov(frmwithrisk=depmodel,groupvar="alcreg",agevar="agebin",comparevar="MDEYR_binbyage",splitvar="YEAR",df=NSDUH20022019_reduceage_DEP)
save(alcregbydep,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbydepbyage_20211030.Rdata")














