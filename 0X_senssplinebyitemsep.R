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
library("MASS",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("glm.predict",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0x_lunaize.R")
library("svyVGAM",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("patchwork",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")


load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.Rdata")
NSDUH20022019_reduceage<-data.frame(NSDUH20022019_reduceage)

####
agemeanbyyear<-function(riskvar,agevar,splitvar,df,cores=20){
df$agevar<-df[,agevar]
df$riskvar<-df[,riskvar]
splitmods<-parallel::mclapply(unique(df[,splitvar]),mc.cores=cores,function(split){
print(split)
dfsplit<-df[df[,splitvar]==split,]
print(nrow(dfsplit))
splitnsduh_design <- svydesign(id = ~ VEREP , strata = ~ VESTR , data = dfsplit , weights = ~ ANALWC1 , nest = TRUE)
svyriskbyyear<-svyby(~ riskvar, by = ~ agevar, splitnsduh_design, svymean, na.rm=T)

#svyriskbyyear$agepeak<-svyriskbyyear$agevar[which(svyriskbyyear$riskvar==max(svyriskbyyear$riskvar))]
svyriskbyyear$split<-split

return(svyriskbyyear)
})
splitmodsdf<-do.call(rbind,splitmods)
return(splitmodsdf)
}

splinesallyears<-function(riskvar,agevar,df){
df$agevar<-df[,agevar]
frm<-as.formula(sprintf("%s~s(agevar)",riskvar))

mod<-mgcv::bam(frm,data=df)
edfagevar<-summary(mod)$s.table[row.names(summary(mod)$s.table)=="s(agevar)",1]
######NSDUH design#####
fullnsduh_design <- svydesign(id = ~ VEREP , strata = ~ VESTR , data =df, weights = ~ANALWC18, nest = TRUE)
###svyglm#######
svyfrm<-as.formula(sprintf("%s~ns(agevar,%s)",riskvar,round(edfagevar)))
svyglmmod<-svyglm(svyfrm,fullnsduh_design)

svyglmpred<-data.frame(summary(emmeans(svyglmmod, ~ agevar,type="response", at=list(agevar=seq(min(df$agevar,na.rm=TRUE),max(df$agevar,na.rm=TRUE),.1)))))
return(svyglmpred)
}


#########means for sens plot######

dangermean<-agemeanbyyear("all_RiskDanger","Ageyears","YEAR",NSDUH20022019_reduceage)
dangermean$type<-"Danger"
testmean<-agemeanbyyear("all_RiskTest","Ageyears","YEAR",NSDUH20022019_reduceage)
testmean$type<-"Test"
fullriskmean<-agemeanbyyear("riskpropensity","Ageyears","YEAR",NSDUH20022019_reduceage)
fullriskmean$type<-"Sum"

allriskmeans<-plyr::rbind.fill(dangermean,testmean) %>% plyr::rbind.fill(.,fullriskmean)
allriskmeans<-allriskmeans %>% group_by(type,split) %>% mutate(scaledrisk=rescale(riskvar))

allriskmeans$typef<-factor(allriskmeans$type,levels=c("Danger","Test","Sum"))

#ggallrisknonscaled<-ggplot(allriskmeans,aes(x=agevar,y=riskvar,colour=typef))+geom_line(aes(group=interaction(typef, split)),alpha=.1)+geom_point(alpha=.33,size=.5)+ggsci::scale_color_jama()

#ggallrisknonscaled<-lunaize(ggallrisknonscaled)+ylab("Self-Reported Risk-Taking")+xlab("Age (y)")+theme(legend.title=element_blank(),legend.position="none",text=element_text(size=32))

####spline fits##########

dangerspline<-splinesallyears("all_RiskDanger","Ageyears",NSDUH20022019_reduceage)
dangerspline$type<-"Danger"
testspline<-splinesallyears("all_RiskTest","Ageyears",NSDUH20022019_reduceage)
testspline$type<-"Test"
fullriskspline<-splinesallyears("riskpropensity","Ageyears",NSDUH20022019_reduceage)
fullriskspline$type<-"Sum"

allrisksplines<-plyr::rbind.fill(dangerspline,testspline) %>% plyr::rbind.fill(.,fullriskspline)
allrisksplines<-allrisksplines %>% group_by(type) %>% mutate(scaledrisk=rescale(emmean))

allrisksplines$typef<-factor(allrisksplines$type,levels=c("Danger","Test","Sum"))


ggallrisknonscaled<-ggplot(allriskmeans,aes(x=agevar,y=riskvar,colour=typef))+geom_line(aes(group=interaction(typef, split)),alpha=.1)+geom_point(alpha=.33,size=.5)+geom_line(data=allrisksplines,aes(x=agevar,y=emmean),alpha=.77,size=1)+ggsci::scale_color_jama()

ggallrisknonscaled<-lunaize(ggallrisknonscaled)+ylab("Self-Reported Risk-Taking")+xlab("Age (y)")+theme(legend.title=element_blank(),legend.position="none",text=element_text(size=32))

###scaled###

allriskmeans_sum_scaled<-allriskmeans %>% group_by(agevar,typef) %>% summarize(medianscaledrisk=median(scaledrisk))


ggallscaled<-ggplot(allriskmeans_sum_scaled,aes(x=agevar,y=medianscaledrisk,colour=typef))+geom_point(alpha=.33,size=2)+geom_line(data=allrisksplines,aes(x=agevar,y=scaledrisk),alpha=.77,size=1)+ggsci::scale_color_jama()

ggallscaled<-lunaize(ggallscaled)+ylab("Scaled")+xlab("Age (y)")+theme(legend.title=element_blank(),legend.position="none",text=element_text(size=32))


allriskplots<-ggallrisknonscaled+ggallscaled+plot_layout(ncol=2)

ggsave(allriskplots,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/risksensitemplots.pdf",height=8,width=10)


