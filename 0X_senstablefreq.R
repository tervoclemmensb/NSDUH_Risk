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

#load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.Rdata")
load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.2021112.Rdata")
source("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Scripts/0X_splinefuncs.R")
NSDUH20022019_reduceage<-data.frame(NSDUH20022019_reduceage)

varlabels<-read.csv(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/studyvarlevellabels.txt")
varlabels$comparevar<-varlabels$var
varlabels$compare<-varlabels$levels
varlabels_merge<-varlabels[,c("comparevar","compare","labels")]

varlabel_relig<-data.frame(comparevar="religionbin",compare=c("Low","Moderate","High"),labels=c("Low","Moderate","High"))
varlabel_dep<-data.frame(comparevar="MDEYR_binbyage",compare=c(0,1),labels=c("No DEP","DEP"))

varlabels_merge<-rbind(varlabels_merge,varlabel_relig,varlabel_dep)

#####transformation#####
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(1,2,3,4)]<-"Low"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(5,6)]<-"Moderate"
NSDUH20022019_reduceage$religionbin[NSDUH20022019_reduceage$regiliousity %in% c(7,8)]<-"High"

freqdataout<-function(comparvar,data){
	
	data$comparvar<-data[,comparvar]
	data<-data[complete.cases(data$comparvar),]
	dataout<-data.frame(table(data$comparvar))
	dataout$Yearswithdata[1]<-paste(sort(unique(data$YEAR)),collapse=" ")
	dataout$comparevar<-comparvar
	names(dataout)[names(dataout)=="Var1"]<-"compare"
	return(dataout)
}

fullfreqout<-lapply(c("Ageyears","IRSEX","INCOME","riskpropensity","COUTYP2","MDEYR_binbyage","religionbin","NEWRACE2"),function(thiscv){
	print(thiscv)	
	thisfreq<-freqdataout(thiscv,data=NSDUH20022019_reduceage)
	return(thisfreq)
})

fullfreqoutdf<-do.call(bind_rows,fullfreqout)
fullfreqoutdfwithlabels<-merge(fullfreqoutdf,varlabels_merge,by=c("comparevar","compare"),all.x=TRUE)
fullfreqoutdfwithlabels$compare<-as.character(fullfreqoutdfwithlabels$compare)

fullfreqoutdfwithlabels<-fullfreqoutdfwithlabels[order(fullfreqoutdfwithlabels$comparevar),]
fullfreqoutdfwithlabels$NSDUHvar<-fullfreqoutdfwithlabels$comparevar
fullfreqoutdfwithlabels<-fullfreqoutdfwithlabels[,c("comparevar","labels","compare","Freq","NSDUHvar","Yearswithdata")]
fullfreqoutdfwithlabels$labels[fullfreqoutdfwithlabels$comparevar=="Ageyears"]<-as.character(fullfreqoutdfwithlabels$compare[fullfreqoutdfwithlabels$comparevar=="Ageyears"])
fullfreqoutdfwithlabels$compare[fullfreqoutdfwithlabels$comparevar=="Ageyears"]<-as.character(seq(1,length(fullfreqoutdfwithlabels$compare[fullfreqoutdfwithlabels$comparevar=="Ageyears"])))

write.csv(fullfreqoutdfwithlabels,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/freqandvars.csv",row.names=FALSE)


##########use criteria#####

NSDUH20022019_reduceage$agebin[NSDUH20022019_reduceage$Ageyears %in% seq(15,18)]<-"add"
NSDUH20022019_reduceage <- NSDUH20022019_reduceage %>% mutate(canngroup=case_when(IRMJFY==0 ~ "non-user",IRMJFY<52 ~"lessthanreg", IRMJFY>= 52 ~"reg"))
NSDUH20022019_reduceage <- NSDUH20022019_reduceage %>% mutate(alcgroup=case_when(IRALCFY==0 ~ "non-user",IRALCFY<52 ~"lessthanreg", IRALCFY>= 52 ~"reg"))
NSDUH20022019_reduceage$cannreg<-dplyr::if_else(NSDUH20022019_reduceage$IRMJFY>= 52 & NSDUH20022019_reduceage$IRMJRC_bin==1,1,0)
NSDUH20022019_reduceage$alcreg<-dplyr::if_else(NSDUH20022019_reduceage$IRALCFY>=52 & NSDUH20022019_reduceage$IRALCRC_bin==1,1,0)

NSDUH20022019_reduceageaddonly<-NSDUH20022019_reduceage[!is.na(NSDUH20022019_reduceage$agebin) & NSDUH20022019_reduceage$agebin=="add",]
NSDUH20022019_reduceageaddonly<-NSDUH20022019_reduceageaddonly[!is.na(NSDUH20022019_reduceageaddonly$YEAR),]

full_NSDUH20022019_reduceage<-NSDUH20022019_reduceageaddonly

####reduce primary for cannreg versus non user
full_NSDUH20022019_reduceage$canngroup[full_NSDUH20022019_reduceage$canngroup=="reg" & full_NSDUH20022019_reduceage$cannreg==0]<-"lessthanreg"
full_NSDUH20022019_reduceage$alcgroup[full_NSDUH20022019_reduceage$alcgroup=="reg" & full_NSDUH20022019_reduceage$alcreg==0]<-"lessthanreg"

table(full_NSDUH20022019_reduceage$canngroup)
table(full_NSDUH20022019_reduceage$alcgroup)


subusefreqout<-lapply(c("canngroup","alcgroup"),function(thiscv){
        print(thiscv)
        thisfreq<-freqdataout(thiscv,data=full_NSDUH20022019_reduceage)
        return(thisfreq)
})

subusefreqoutdf<-do.call(bind_rows,subusefreqout)

write.csv(subusefreqoutdf,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/subfreqtable.csv",row.names=FALSE)



