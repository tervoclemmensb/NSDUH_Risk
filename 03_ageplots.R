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
library("ggsci",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("cowplot",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("ggprism",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0x_lunaize.R")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0X_splinefuncs.R")

load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.Rdata")
NSDUH20022019_reduceage<-data.frame(NSDUH20022019_reduceage)
#####mean by age compare func#####
NSDUHmeanbyagecompare<-function(df=NSDUH20022019_reduceage,comparevar,agevar,splitvar="YEAR",cores=20){
df<-df[complete.cases(df[,c(comparevar,agevar,splitvar)]),]
if (comparevar=="NEWRACE2"){
df<-df[df$NEWRACE2 %in% c(1,2,5,6,7),] ##very low base rate groups (less than 2% of all respondents) are removed for precision across all analyses####
}
bycompare<-lapply(unique(df[,comparevar]),function(cli){
byage<-lapply(unique(df[,agevar]),function(ai){
byyear<-mclapply(unique(df[,splitvar]),mc.cores=cores,function(sli){
print(ai)
###individual estimates######
aidf<-df[df[,comparevar]==cli & df[,agevar]==ai & df[,splitvar]==sli,]
aidesign<-svydesign(id = ~ VEREP , strata = ~ VESTR , data = aidf , weights = ~ ANALWC1 , nest = TRUE)
options(survey.lonely.psu="remove")
aimean<-svymean(~riskpropensity,aidesign,na.rm=TRUE)
aiCI<-confint(aimean,level = 0.99)
out<-data.frame(mean=aimean[1],hiCI=aiCI[1],loCI=aiCI[2])
out$comparevar<-comparevar
out$compare<-cli
###difference testing#######
if (length(unique(df[,comparevar]))>1){
aidfalllevels<-df[df[,agevar]==ai & df[,splitvar]==sli,]
aidfalllevels$comparevar<-aidfalllevels[,comparevar]
aidfalllevelsdesign<-svydesign(id = ~ VEREP , strata = ~ VESTR , data = aidfalllevels , weights = ~ ANALWC1 , nest = TRUE)
options(survey.lonely.psu="remove")
modelalllevels<-svyglm(riskpropensity~comparevar,aidfalllevelsdesign)
alllevelstest<-regTermTest(modelalllevels, ~comparevar)
out$Ftestcompare<-alllevelstest$Ftest
out$pvalcompare<-alllevelstest$p
}
out[,agevar]<-ai
out[,splitvar]<-sli
return(out)
})
byyearmat<-do.call(rbind,byyear)
})
agemat<-do.call(rbind,byage)
return(agemat)
})
bycomparemat<-do.call(rbind,bycompare)
}

#############################3
#### run differences by strata ### Figure C
varlabels<-data.frame(var=c("IRSEX","riskpropensity","NEWRACE2","INCOME","COUTYP2","HEALTH2_IMP","regiliousity","full","MDEYR_binbyage"),
label=c("sex","risk-taking","race/ethnicity","income","county population","physical health","religious","full","depression"))

NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(1,2,3,4)]<-"Low"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(5,6)]<-"Moderate"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(7,8)]<-"High"
NSDUH20022019_reduceageaddonly<-NSDUH20022019_reduceage[NSDUH20022019_reduceage$Ageyears %in% seq(15,18),]
NSDUH20022019_reduceageaddonly$agebin<-"addbin"

comparevars<-c("IRSEX","NEWRACE2","INCOME","COUTYP2","religionbin","MDEYR_binbyage")
agebycomparevarall<-lapply(comparevars,function(cv){
print(cv)
cvbyagemean<-NSDUHmeanbyagecompare(df=NSDUH20022019_reduceageaddonly,comparevar=cv,agevar="agebin")
return(cvbyagemean)
})
comparevarsdf<-do.call(rbind,agebycomparevarall)
#####dummy bin for just year comparison#########
NSDUH20022019_reduceageaddonly$comparebin<-"comparebin"
byyearmean<-NSDUHmeanbyagecompare(df=NSDUH20022019_reduceageaddonly,comparevar="comparebin",agevar="agebin")

###by strata###
##strata labels###
varlabels<-read.csv(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/studyvarlevellabels.txt")
varlabels$comparevar<-varlabels$var
varlabels$compare<-varlabels$levels
varlabels_merge<-varlabels[,c("comparevar","compare","labels")]

varlabel_relig<-data.frame(comparevar="religionbin",compare=c("Low","Moderate","High"),labels=c("Low","Moderate","High"))
varlabel_dep<-data.frame(comparevar="MDEYR_binbyage",compare=c(0,1),labels=c("No DEP","DEP"))

varlabels_merge<-rbind(varlabels_merge,varlabel_relig,varlabel_dep)

########
comparevarsdfwithvarlabel<-merge(comparevarsdf,varlabels_merge,by=c("comparevar","compare"))

###############
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/agefitpeaks/allpeaks.Rdata")
#allagepeaks
surveypeaks<-allagepeaks[allagepeaks$type=="svy",] ### limit to those from survey package (ignore BAM fits used for first stage estimate of smoothing)
surveypeaks$comparevar<-as.factor(surveypeaks$comparevar)

load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/agefitpeaks/allfits.Rdata")
#allfits
surveyfits<-allfits[allfits$type=="svy",] ### limit to those from survey package (ignore BAM fits used for first stage estimate of smoothing)

####peaks by year#####

load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/agefitpeaks/riskpeaksbyyear.Rdata")

#####all plots#######

surveypeaks$label_comparvar<-as.factor(paste(surveypeaks$comparevar,surveypeaks$labels,sep="_"))
surveypeaks$order_label_comparevar<-as.numeric(surveypeaks$label_comparvar)

labelbyseq<-surveypeaks %>% group_by(label_comparvar,labels) %>% summarize(order=unique(order_label_comparevar))

surveypeaks$labelsf<-factor(surveypeaks$labels,levels=c("F","M","HIS","NH AS","NH B|AA","NH MR","NH W","<20K","20-40K","50-75K",">75K","Nonmetro","Metro S","Metro L","Low","Moderate","High","No DEP","DEP"))

ggagepeakplot<-ggplot()+geom_hline(yintercept=range(years_ageatpeak$ageatpeak)[1],linetype="dashed",colour="black",alpha=.5)+
geom_hline(yintercept=range(years_ageatpeak$ageatpeak)[2],linetype="dashed",colour="black",alpha=.5)+
geom_boxplot(data=surveypeaks,aes(y=as.numeric(ageatpeak),x=as.factor(1),colour=as.factor(labelsf)),outlier.shape=NA,colour="#374e55")+
geom_jitter(data=surveypeaks,aes(y=as.numeric(ageatpeak),x=as.factor(1),colour=as.factor(labelsf)),width=0.25,alpha=.33,height = 0)+
coord_flip()+facet_grid(rows=vars(labelsf))+
scale_y_continuous(limits=c(12,25),breaks=seq(12,25,1))+theme(axis.text.y=element_blank())+xlab("")+ylab("Age (y)")+theme(legend.position="none")+scale_colour_manual(values=c(rep("#374e55",length(unique(surveypeaks$labels)))))+theme(text = element_text(size = 20))+theme(strip.background = element_blank(),strip.text.x = element_blank(),axis.text.x=element_blank())

ggagepeakplot<-lunaize_geomrasteryticktowhite(ggagepeakplot)+theme(strip.background=element_blank(),axis.text.y=element_blank(),strip.text.y.right=element_text(angle = 0),legend.position="none")+theme(text=element_text(size=32))

save(ggagepeakplot,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/byagepeaks.Rdata")
ggsave(ggagepeakplot,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/byagepeaks.pdf",height=8,width=10)

###percent and medians within 15-18-years-old###
length(which(surveypeaks$ageatpeak >=15 & surveypeaks$ageatpeak <=18))/nrow(surveypeaks)
medianbylabel<-surveypeaks %>% group_by(labelsf) %>% summarize(medianageatpeak=median(ageatpeak))
length(which(medianbylabel$medianageatpeak >=15 & medianbylabel$medianageatpeak <=18))/nrow(medianbylabel)
###########

####all fits#######
##ful fit###
##dummy duplicate for full fit and select first dummy (code implementation expects a comparison variable)
NSDUH20022019_reduceage$dummcompare<-1
NSDUH20022019_reduceage2<-NSDUH20022019_reduceage
NSDUH20022019_reduceage2$dummcompare<-2
allNSDUHdum<-rbind(NSDUH20022019_reduceage2,NSDUH20022019_reduceage)
allNSDUHdum$YEARfactor<-as.factor(allNSDUHdum$YEAR)

frm<-as.formula("riskpropensity~s(predvar)+s(YEARfactor,bs='re')")
fullfit<-bamfitsvynestedbycomparevarlevel_sf(frm,predvarname="Ageyears",comparevar="dummcompare",df=allNSDUHdum)
fullfitkeep<-fullfit[fullfit$compare==1 & fullfit$type=="svy",]

########
surveyfits$label_comparvar<-as.factor(paste(surveyfits$comparevar,surveyfits$labels,sep="_"))
surveyfits$order_label_comparevar<-as.numeric(surveyfits$label_comparvar)

labelbyseqfit<-surveyfits %>% group_by(label_comparvar,labels) %>% summarize(order=unique(order_label_comparevar))

surveyfits$labelsf<-factor(surveyfits$labels,levels=labelbyseqfit$labels)

baserates<-surveyfits %>% group_by(label_comparvar) %>% summarize(meanbaserate=mean(baseratecli))

surveyfitstest<-surveyfits[surveyfits$comparevar!="HEALTH2_IMP",]###ignore HEALTH

insert_minor <- function(major_labs, n_minor) {labs <- 
                              c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
                              labs[1:(length(labs)-n_minor)]}

breaks=c(12,20,30,40,50,60)
breaksandlabels<-data.frame(ageyears=unique(NSDUH20022019_reduceage$Ageyears))
breaksandlabels$labelfull<-as.character(breaksandlabels$ageyears)
breaksandlabels$labelfull[!(breaksandlabels$ageyears %in% breaks)]<-""

ggallfits<-ggplot(surveyfitstest,aes(x=Ageyears,y=emmean,colour=as.factor(labelsf)))+geom_line(size=1,alpha=.25)+scale_x_continuous(limits=c(12,65),breaks=breaks)+scale_colour_manual(values=c(rep("#374e55",length(unique(surveyfitstest$labelsf)))))+ylab("Self-Reported Risk-Taking")+geom_line(data=fullfitkeep,aes(x=Ageyears,y=emmean),colour="#374e55",size=2)+
geom_segment(data=breaksandlabels,aes(x =ageyears, y = 1, xend = ageyears, yend = 2.45),colour="grey33")+coord_cartesian(ylim = c(2.5, 5))

ggallfits<-lunaize(ggallfits,ajust=0)+theme(legend.position="none",legend.title=element_blank())+ylab("Self-Reported Risk-Taking")+xlab("Age (y)")+theme(strip.text.y=element_blank())+theme(text=element_text(size=32))

ggsave(ggallfits,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/fullageplotsRT.pdf",height=8,width=10)
save(ggallfits,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/fullageplotsRT.Rdata")

######magnitude at adolescent

comparevarsdfwithvarlabel$labelsf<-factor(comparevarsdfwithvarlabel$labels,levels=c("F","M","HIS","NH AS","NH B|AA","NH MR","NH W","<20K","20-40K","50-75K",">75K","Nonmetro","Metro S","Metro L","Low","Moderate","High","No DEP","DEP"))
comparevarsdfwithvarlabel<-comparevarsdfwithvarlabel[!is.na(comparevarsdfwithvarlabel$labelsf),]

ggvalatpeakplot<-ggplot()+
geom_hline(yintercept=range(byyearmean$mean)[1],linetype="dashed",colour="black",alpha=.5)+geom_hline(yintercept=range(byyearmean$mean)[2],linetype="dashed",colour="black",alpha=.5)+
geom_boxplot(data=comparevarsdfwithvarlabel,aes(x=labelsf,y=mean),outlier.shape=NA,colour="#374e55")+
geom_jitter(data=comparevarsdfwithvarlabel,aes(x=labelsf,y=mean),width=0.25,alpha=.33,height = 0,colour="#374e55")

ggvalatpeakplot<-lunaize(ggvalatpeakplot)+theme(text=element_text(size=32))+xlab("")+ylab("Self-Reported Risk-Taking")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(ggvalatpeakplot,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/RTatpeak.pdf",height=8,width=10)
save(ggvalatpeakplot,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/RTatpeak.Rdata")
###significance by comparvar#####
pvaloutbyyear<-comparevarsdfwithvarlabel %>% group_by(comparevar,YEAR) %>% summarize(np=length(unique(pvalcompare)),p=unique(pvalcompare),npnonun=length(pvalcompare),)
pvaloutbyyear$correctp<-dplyr::if_else((pvaloutbyyear$p >=.05/length(unique(pvaloutbyyear$comparevar))),1,0)



