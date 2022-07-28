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

bamandsvyfitandpeakbysplit_sf<-function(riskvar,agevar,splitvar,comparevar=NULL,df,cores=20,returnpreds=FALSE){
df$agevar<-df[,agevar]
frm<-as.formula(sprintf("%s~s(agevar)",riskvar))

if(is.null(comparevar)){
splitmods<-parallel::mclapply(unique(df[,splitvar]),mc.cores=cores,function(split){
print(split)
dfsplit<-df[df[,splitvar]==split,]
print(nrow(dfsplit))
mod<-mgcv::bam(formula=frm,data=dfsplit)
edfagevar<-summary(mod)$s.table[row.names(summary(mod)$s.table)=="s(agevar)",1] 
splitnsduh_design <- svydesign(id = ~ VEREP , strata = ~ VESTR , data = dfsplit , weights = ~ ANALWC1 , nest = TRUE)
svyfrm<-as.formula(sprintf("%s~ns(agevar,%s)",riskvar,round(edfagevar)))
svymod<-svyglm(svyfrm,splitnsduh_design)
svypred<-data.frame(summary(emmeans(svymod, ~ agevar,type="response", at=list(agevar=seq(min(dfsplit$agevar,na.rm=TRUE),max(dfsplit$agevar,na.rm=TRUE),.1)))))
svypred$split<-split
svypred$ageatpeak<-svypred$agevar[svypred$emmean==max(svypred$emmean)]
svypred$type<-"svy"

predvar<-data.frame(summary(emmeans(mod, ~ agevar,type="response", at=list(agevar=seq(min(dfsplit$agevar,na.rm=TRUE),max(dfsplit$agevar,na.rm=TRUE),.1)))))
predvar$split<-split
predvar$ageatpeak<-predvar$agevar[predvar$emmean==max(predvar$emmean)]
predvar$type<-"bam"

allpred<-dplyr::bind_rows(svypred,predvar)

return(allpred)
})
allsplitmods<-do.call(rbind,splitmods)
outmat<-allsplitmods
if(!returnpreds){

outmat<-outmat %>% dplyr::group_by(split,type) %>%  dplyr::summarize(ageatpeak=unique(ageatpeak))

}

}else{
df<-df[!is.na(df[,comparevar]),]
comparebysplitmat<-expand.grid(splitvar=unique(df[,splitvar]),comparevar=unique(df[,comparevar])[!is.na(unique(df[,comparevar]))])

splitmodsbycompare<-parallel::mclapply(1:nrow(comparebysplitmat),mc.cores=cores,function(cbsi){
print(cbsi)
cbsi_df<-df[df[,splitvar]==comparebysplitmat$splitvar[cbsi] & df[,comparevar]==comparebysplitmat$comparevar[cbsi] , ]
baseratepersplit<-nrow(cbsi_df)/length(which(df[,splitvar]==comparebysplitmat$splitvar[cbsi]))
cbsimod<-mgcv::bam(formula=frm,data=cbsi_df)

edfagevar<-summary(cbsimod)$s.table[row.names(summary(cbsimod)$s.table)=="s(agevar)",1]
splitnsduh_design <- svydesign(id = ~ VEREP , strata = ~ VESTR , data = cbsi_df , weights = ~ ANALWC1 , nest = TRUE)
svyfrm<-as.formula(sprintf("%s~ns(agevar,%s)",riskvar,round(edfagevar)))
svymod<-svyglm(svyfrm,splitnsduh_design)
svypred<-data.frame(summary(emmeans(svymod, ~ agevar,type="response", at=list(agevar=seq(min(cbsi_df$agevar,na.rm=TRUE),max(cbsi_df$agevar,na.rm=TRUE),.1)))))
svypred$split<-comparebysplitmat$splitvar[cbsi]
svypred$ageatpeak<-svypred$agevar[svypred$emmean==max(svypred$emmean)]
svypred$compare<-comparebysplitmat$comparevar[cbsi]
svypred$type<-"svy"

predvar<-data.frame(summary(emmeans(cbsimod, ~ agevar,type="response", at=list(agevar=seq(min(cbsi_df$agevar,na.rm=TRUE),max(cbsi_df$agevar,na.rm=TRUE),.1)))))
predvar$split<-comparebysplitmat$splitvar[cbsi]
predvar$compare<-comparebysplitmat$comparevar[cbsi]
predvar$ageatpeak<-predvar$agevar[predvar$emmean==max(predvar$emmean)]
predvar$type<-"bam"

allpred<-dplyr::bind_rows(svypred,predvar)
allpred$baserate<-baseratepersplit
allpred$edfagevar

return(allpred)

})
allsplitmodsbycompare<-do.call(rbind,splitmodsbycompare)
outmat<-allsplitmodsbycompare
if(!returnpreds){

outmat<-outmat %>% dplyr::group_by(split,type,compare) %>%  dplyr::summarize(ageatpeak=unique(ageatpeak),baserate=unique(baserate))
}

}
outmat$riskvar<-riskvar
outmat$agevarname<-agevar
outmat$splitvar<-splitvar
outmat$comparevar<-comparevar
outmat$scripttype<-"sf"

return(outmat)
}

##########

bamfitsvynestedbycomparevarlevel_sf<-function(frm,predvarname,comparevar,df){
df$predvar<-df[,predvarname]
df<-df[!is.na(df[,comparevar]),]
df<-df[!is.na(df[,predvarname]),]
df<-df[!is.na(df[,as.character(frm)[2]]),]
df$weights<-df[,sprintf("ANALWC%s",length(unique(df$YEAR)))]
comparevarsplitmods<-lapply(unique(df[,comparevar][!is.na(df[,comparevar])]),function(cli){
print(cli)

dfcli<-df[df[,comparevar]==cli,]
dfcli<-dfcli[!is.na(dfcli[,comparevar]),]
dfcli<-dfcli[!is.na(dfcli[,predvarname]),]
dfcli<-dfcli[!is.na(dfcli[,as.character(frm)[2]]),]
print(nrow(dfcli)/nrow(df))

cbsimod<-mgcv::bam(formula=frm,discrete=TRUE,nthreads=15,data=dfcli)

cbsipredvar<-data.frame(summary(emmeans(cbsimod, ~ predvar,type="response",at=list(predvar=seq(min(dfcli$predvar,na.rm=TRUE),max(dfcli$predvar,na.rm=TRUE),.1)))))
cbsipredvar$normedemmean<-scales::rescale(cbsipredvar$emmean,to=c(0,1))
names(cbsipredvar)[names(cbsipredvar)=="predvar"]<-predvarname
cbsipredvar[,comparevar]<-cli
cbsipredvar$type<-"bam"

edfagevar<-summary(cbsimod)$s.table[row.names(summary(cbsimod)$s.table)=="s(predvar)",1]
splitnsduh_design <- svydesign(id = ~ VEREP , strata = ~ VESTR , data = dfcli , weights = ~ weights , nest = TRUE)

svyfrm<-as.formula(sprintf("%s~ns(predvar,%s)",as.character(frm)[2],round(edfagevar)))
boolFalse<-F
edf<-round(edfagevar)
while(boolFalse==F){
print(edf)
    svyfrm<-as.formula(sprintf("%s~ns(predvar,%s)",as.character(frm)[2],edf))
    svymod<-tryCatch(svyglm(svyfrm,splitnsduh_design),error=identity)
    if(is(svymod,"error")){
    edf<-edf-1
    print("reducing edf")
    }
    if(!is(svymod,"error")){
    boolFalse=T
    }
}
    
svypred<-data.frame(summary(emmeans(svymod, ~ predvar,type="response", at=list(predvar=seq(min(dfcli$predvar,na.rm=TRUE),max(dfcli$predvar,na.rm=TRUE),.1)))))
names(svypred)[names(svypred)=="predvar"]<-predvarname
svypred[,comparevar]<-cli
svypred$type<-"svy"

allpred<-plyr::rbind.fill(cbsipredvar,svypred)
allpred$baseratecli<-nrow(dfcli)/nrow(df)

return(allpred)

})

allmods<-do.call(rbind,comparevarsplitmods)
allmods$comparevar<-comparevar
allmods$compare<-allmods[,comparevar]
allmods$scripttype<-"sf"
return(allmods)
}
