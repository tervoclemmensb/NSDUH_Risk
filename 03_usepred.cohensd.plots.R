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
library("splines")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0x_lunaize.R")

varlabels<-data.frame(var=c("IRSEX","riskpropensity","NEWRACE2","INCOME","COUTYP2","HEALTH2_IMP","regiliousity","full","MDEYR_binbyage"),
label=c("sex","risk-taking","race/ethnicity","income","county population","physical health","religious","full","depression"))

agelabels<-data.frame(agebin=c(21.5,42.0,15.0),agecat=c("young adulthood","adulthood","adolescence"))

###by strata###
##strata labels###
varlabels<-read.csv(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/studyvarlevellabels.txt")
varlabels$comparevar<-varlabels$var
varlabels$compare<-varlabels$levels
varlabels_merge<-varlabels[,c("comparevar","compare","labels")]

varlabel_relig<-data.frame(comparevar="religionbin",compare=c("Low","Moderate","High"),labels=c("Low","Moderate","High"))
varlabel_dep<-data.frame(comparevar="MDEYR_binbyage",compare=c(0,1),labels=c("No DEP","DEP"))

varlabels_merge<-rbind(varlabels_merge,varlabel_relig,varlabel_dep)
########can######################
######load############
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbysexbyage_20211030.Rdata")
cannregbysex$comparevar<-"IRSEX"
cannregbysex$compare<-cannregbysex$IRSEX
###income################
###can reg#######
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbyincomebyage_20211030.Rdata")
cannregbyincome$comparevar<-"INCOME"
cannregbyincome$compare<-cannregbyincome$INCOME
###RACE##############
###can reg########
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbyracebyage_20211030.Rdata")
cannregbyrace$comparevar<-"NEWRACE2"
cannregbyrace$compare<-cannregbyrace$NEWRACE2
#######COUTY############
###can reg#########
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbycoutybyage_20211030.Rdata")
cannregbyrcouty$comparevar<-"COUTYP2"
cannregbyrcouty$compare<-cannregbyrcouty$COUTYP2
#####RELIG################
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbyreligbyage_20211030.Rdata")
cannregbyrelig$comparevar<-"religionbin"
cannregbyrelig$compare<-cannregbyrelig$religionbin
######DEP#############3
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/cannregbydepbyage_20211030.Rdata")
cannregbydep$comparevar<-"MDEYR_binbyage"
cannregbydep$compare<-cannregbydep$MDEYR_binbyage
####merge all strata####
allstrata<-plyr::rbind.fill(cannregbysex,cannregbyincome) %>% plyr::rbind.fill(.,cannregbyrace) %>% plyr::rbind.fill(.,cannregbyrcouty)  %>% plyr::rbind.fill(.,cannregbyrelig) %>% plyr::rbind.fill(.,cannregbydep)
allstratawithlabels<-merge(allstrata,varlabels_merge,by=c("comparevar","compare"))

#allstratarange<-allstratawithlabels %>% dplyr::group_by(agebin,labels,comparevar,compare) %>% dplyr::summarize(median=median(cohendfromlm,na.rm=TRUE),hi=quantile(cohendfromlm,na.rm=TRUE,probs=c(.975)),lo=quantile(cohendfromlm,na.rm=TRUE,probs=c(.025)),IQRlow=quantile(cohendfromlm,na.rm=TRUE,probs=c(.25)),IQRhigh=quantile(cohendfromlm,na.rm=TRUE,probs=c(.75)),min=min(cohendfromlm,na.rm=TRUE),max=max(cohendfromlm,na.rm=TRUE)) ###display whole boxplot 

allstratawithlabels$label_fl<-factor(allstratawithlabels$labels,levels=c("F","M","<20K","20-40K","50-75K",">75K","HIS","NH AS","NH B|AA","NH MR","NH W","Nonmetro","Metro S","Metro L","Fair/Poor","Good","V. Good","Excellent","No DEP","DEP","Low","Moderate","High"))

allstratawithlabels$su<-"cann"
allstratawithlabels$correctedsig<-dplyr::if_else(sign(allstratawithlabels$cohendtestfromlm)==1 & allstratawithlabels$cohendtestfromlm_pval<.05/length(unique(allstratawithlabels$label_fl)),1,0)

bystratarsqfig<-ggplot()+
geom_hline(yintercept=0,linetype="dashed")+
geom_boxplot(data=allstratawithlabels,aes(x=label_fl,y=cohendtestfromlm),outlier.shape=NA,colour="#374e55")+
geom_jitter(data=allstratawithlabels,aes(x=label_fl,y=cohendtestfromlm,shape=as.factor(correctedsig)),width=0.25,alpha=.33,height = 0,colour="#374e55")+scale_shape_manual(values=c(17,16))

bystratarsqfig<-lunaize(bystratarsqfig)+theme(legend.position="top",legend.title=element_blank())+xlab("")+ylab("Cannabis Prediction from Risk-Taking\n(Partial R-sq)")+
theme(axis.text.x= element_text(size = 14,angle=90))+theme(legend.text=element_text(size=10))

###alc###############

###########load###########################3
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbysexbyage_20211030.Rdata")
alcregbysex$comparevar<-"IRSEX"
alcregbysex$compare<-alcregbysex$IRSEX
###income################
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbyincomebyage_20211030.Rdata")
alcregbyincome$comparevar<-"INCOME"
alcregbyincome$compare<-alcregbyincome$INCOME
###RACE##############
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbyracebyage_20211030.Rdata")
alcregbyrace$comparevar<-"NEWRACE2"
alcregbyrace$compare<-alcregbyrace$NEWRACE2
#######COUTY############
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbycoutybyage_20211030.Rdata")
alcregbyrcouty$comparevar<-"COUTYP2"
alcregbyrcouty$compare<-alcregbyrcouty$COUTYP2
#####RELIG################
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbyreligbyage_20211030.Rdata")
alcregbyrelig$comparevar<-"religionbin"
alcregbyrelig$compare<-alcregbyrelig$religionbin
######DEP#############3
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/cohend/alcregbydepbyage_20211030.Rdata")
alcregbydep$comparevar<-"MDEYR_binbyage"
alcregbydep$compare<-alcregbydep$MDEYR_binbyage
####merge all strata####
allstrataalc<-plyr::rbind.fill(alcregbysex,alcregbyincome) %>% plyr::rbind.fill(.,alcregbyrace) %>% plyr::rbind.fill(.,alcregbyrcouty)  %>% plyr::rbind.fill(.,alcregbyrelig) %>% plyr::rbind.fill(.,alcregbydep)
allstratawithlabelsalc<-merge(allstrataalc,varlabels_merge,by=c("comparevar","compare"))

allstratawithlabelsalc$label_fl<-factor(allstratawithlabelsalc$labels,levels=c("F","M","<20K","20-40K","50-75K",">75K","HIS","NH AS","NH B|AA","NH MR","NH W","Nonmetro","Metro S","Metro L","Fair/Poor","Good","V. Good","Excellent","No DEP","DEP","Low","Moderate","High"))

allstratawithlabelsalc$su<-"alc"

allstratawithlabelsalc$correctedsig<-dplyr::if_else(sign(allstratawithlabelsalc$cohendtestfromlm)==1 & allstratawithlabelsalc$cohendtestfromlm_pval<.05/length(unique(allstratawithlabelsalc$label_fl)),1,0)


############################
allstratabothsubstances<-dplyr::bind_rows(allstratawithlabelsalc,allstratawithlabels)

allstratabothsubstances$label_fl<-factor(allstratabothsubstances$labels,levels=c("F","M","HIS","NH AS","NH B|AA","NH MR","NH W","<20K","20-40K","50-75K",">75K","Nonmetro","Metro S","Metro L","Low","Moderate","High","No DEP","DEP"))

bystratarsqfigalcadd<-ggplot()+
geom_hline(yintercept=0,linetype="dashed")+
geom_boxplot(data=allstratabothsubstances,aes(x=label_fl,y=cohendtestfromlm,colour=su),outlier.shape=NA)+
geom_point(data=allstratabothsubstances,aes(x=label_fl,y=cohendtestfromlm,shape=as.factor(correctedsig),colour=su,group=su),position=position_jitterdodge(jitter.width=.25,dodge.width=0.75),alpha=.33)+
scale_shape_manual(values=c(17,16))+
scale_colour_manual(values=c("#374e55","#00A1D5FF"))

bystratarsqfigalcadd<-lunaize(bystratarsqfigalcadd)+xlab("")+ylab("Risk-Taking Differences\nRegular Substance Users vs. Non-Users\n (Cohen's d)")+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+theme(text=element_text(size=32))+theme(legend.position=c(14,1.5),legend.title=element_blank())

save(bystratarsqfigalcadd,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/cohensd.add.alccan.Rdata")
ggsave(bystratarsqfigalcadd,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/cohensd.add.alccan.pdf",height=8,width=13)

#################################

allstratabothsubstances$correctedsig_unadjust<-dplyr::if_else(allstratabothsubstances$cohendtestfromlm_unadj_pval<.05/length(unique(allstratabothsubstances$label_fl)),1,0)

allstratabothsubstances %>% group_by(su) %>% summarize(percentsigcorrect=sum(correctedsig)/n(),percentsigcorrect_unadjust=sum(correctedsig_unadjust)/n())




