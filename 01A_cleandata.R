#https://pdas.samhsa.gov/#/survey/NSDUH-2019-DS0001
library("mgcv",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("dplyr",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("ggplot2",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("tzdb",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2/")
library("vroom",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
#####
clean_nsduh<-function(x,badcodes=c(-9,85,89,94,97,98,99),settozero=NULL){
nbadcodes<-length(which(x %in% badcodes))
print(sprintf("%s entries with badcodes/missing data",nbadcodes))
print(sprintf("based on badcodes %s",badcodes))
xout<-x
xout[xout %in% badcodes]<-NA
xout[xout %in% settozero]<-0
return(xout)
}


####load reduced data########
#load("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reduce_20211025.Rdata")
load("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reduce_20211103.Rdata")
NSDUH20022019_reducenames<-names(NSDUH20022019_reduce)
NSDUH20022019_reduce<-data.frame(sapply(NSDUH20022019_reduce, haven::zap_labels))
names(NSDUH20022019_reduce)<-NSDUH20022019_reducenames
NSDUH20022019_reduce[,names(NSDUH20022019_reduce)]<-lapply(NSDUH20022019_reduce[,names(NSDUH20022019_reduce)],function(x){as.numeric(x)})

usevars<-c("YEAR","AGE2","IRSEX","NEWRACE2","INCOME","COUTYP2","SNRLGIMP","SNRLDCSN","YERLGIMP","YERLDCSN","MRDAYPYR","ALCYRTOT","ANALWT_C","RSKYFQDGR","RKFQDNGR","RSKYFQTES","RKFQRSKY","RKFQPBLT","RKFQDBLT","HEALTH2_IMP")

cleanvars<-c("RKFQDNGR","RSKYFQDGR","COUTYP2","SNRLGIMP","SNRLDCSN","YERLGIMP","YERLDCSN","RSKYFQTES","RKFQRSKY","RKFQPBLT","RKFQDBLT","HEALTH2","BOOKED","NOBOOKY2","YESTSMJ","YESTSALC","TXYRILL","TXYRALC","SPD_UADJ","YMDEYR","AMDEYR") #non imputed demo vars to be cleaned

NSDUH20022019_reduce[,cleanvars]<-lapply(NSDUH20022019_reduce[,cleanvars],function(x){clean_nsduh(x)})
#NSDUH20022019_reduce$MRDAYPYR<-clean_nsduh(NSDUH20022019_reduce$MRDAYPYR,badcodes=c(985,989,991,994,997,998,999),settozero=993)
#NSDUH20022019_reduce$ALCYRTOT<-clean_nsduh(NSDUH20022019_reduce$ALCYRTOT,badcodes=c(985,989,991,994,997,998,999),settozero=993)
NSDUH20022019_reduce$HEALTH2_IMP<-NSDUH20022019_reduce$HEALTH
NSDUH20022019_reduce$HEALTH2_IMP[NSDUH20022019_reduce$HEALTH %in% c(4,5)]<-4
NSDUH20022019_reduce$IRALCFY<-clean_nsduh(NSDUH20022019_reduce$IRALCFY,settozero=c(991,993)) ####ALC Frequency
NSDUH20022019_reduce$IRMJFY<-clean_nsduh(NSDUH20022019_reduce$IRMJFY,settozero=c(991,993)) ####MJ Frequency
NSDUH20022019_reduce$MJAGE<-clean_nsduh(NSDUH20022019_reduce$MJAGE,badcodes=c(985,994,997,998),settozero=c(991))
NSDUH20022019_reduce$ALCTRY<-clean_nsduh(NSDUH20022019_reduce$ALCTRY,badcodes=c(985,994,997,998),settozero=c(991))
NSDUH20022019_reduce$NOBOOKY2<-clean_nsduh(NSDUH20022019_reduce$NOBOOKY2,badcodes=c(985,994,997,998),settozero=c(999))


NSDUH20022019_reduce$TXYRILLORALC<-0
NSDUH20022019_reduce$TXYRILLORALC[which(rowSums(NSDUH20022019_reduce[,c("TXYRILL","TXYRALC")],na.rm=TRUE) > 0)]<-1
NSDUH20022019_reduce$YMDEYR_bin<-NSDUH20022019_reduce$YMDEYR
NSDUH20022019_reduce$YMDEYR_bin[NSDUH20022019_reduce$YMDEYR==2]<-0

NSDUH20022019_reduce$AMDEYR_bin<-NSDUH20022019_reduce$AMDEYR
NSDUH20022019_reduce$AMDEYR_bin[NSDUH20022019_reduce$AMDEYR==2]<-0

NSDUH20022019_reduce$MDEYR_binbyage<-base::rowSums(NSDUH20022019_reduce[,c("AMDEYR","YMDEYR")],na.rm=TRUE)
NSDUH20022019_reduce$MDEYR_binbyage[NSDUH20022019_reduce$MDEYR_binbyage==0]<-NA ##set NA to zeros (rowSums NA + NA outputs 0)
NSDUH20022019_reduce$MDEYR_binbyage[NSDUH20022019_reduce$MDEYR_binbyage==2]<-0 ###2 is no depression


NSDUH20022019_reduce$RKFQPBLT<-clean_nsduh(NSDUH20022019_reduce$RKFQPBLT,badcodes=c(5,85,94,97,98))
NSDUH20022019_reduce$RKFQDBLT<-clean_nsduh(NSDUH20022019_reduce$RKFQDBLT,badcodes=c(5,85,94,97,98))

#NSDUH20022019_reduce$DEPNDALC
#NSDUH20022019_reduce$ABUSEALC
NSDUH20022019_reduce$ALC_AORD<-dplyr::if_else(rowSums(NSDUH20022019_reduce[,c("DEPNDALC","ABUSEALC")])>0,1,0)
NSDUH20022019_reduce$MRJ_AORD<-dplyr::if_else(rowSums(NSDUH20022019_reduce[,c("DEPNDMRJ","ABUSEMRJ")])>0,1,0)

NSDUH20022019_reduce$RSKMJOCC<-clean_nsduh(NSDUH20022019_reduce$RSKMJOCC)
NSDUH20022019_reduce$RSKMJREG<-clean_nsduh(NSDUH20022019_reduce$RSKMJREG)
#RSKMJOCC
#RSKMJREG

NSDUH20022019_reduce$IRMJRC_bin<-dplyr::if_else(NSDUH20022019_reduce$IRMJRC==1,1,0)
NSDUH20022019_reduce$IRALCRC_bin<-dplyr::if_else(NSDUH20022019_reduce$IRALCRC==1,1,0)

convertage<-data.frame(AGE2=seq(1:17),Ageyears=c(12,13,14,15,16,17,18,19,20,21,22.5,24.5,27.5,32,42,57,65))
NSDUH20022019_reduceage<-merge(NSDUH20022019_reduce,convertage,by="AGE2")

###combine vars############

###religious

NSDUH20022019_reduceage$all_GIMP<-base::rowSums(NSDUH20022019_reduceage[,c("SNRLGIMP","YERLGIMP")],na.rm=TRUE) ###combine youth and adult regligous beliefs are important
NSDUH20022019_reduceage$all_GIMP[NSDUH20022019_reduceage$all_GIMP==0]<-NA ##set NA to zeros (rowSums NA + NA outputs 0)

NSDUH20022019_reduceage$all_DCSN<-base::rowSums(NSDUH20022019_reduceage[,c("SNRLDCSN","YERLDCSN")],na.rm=TRUE)	

NSDUH20022019_reduceage$all_DCSN[NSDUH20022019_reduceage$all_DCSN==0]<-NA ##set NA to zeros (rowSums NA + NA outputs 0)


NSDUH20022019_reduceage$regiliousity<-base::rowSums(NSDUH20022019_reduceage[,c("all_GIMP","all_DCSN")],na.rm=TRUE)

NSDUH20022019_reduceage$regiliousity[NSDUH20022019_reduceage$regiliousity==0]<-NA ##set NA to zeros (rowSums NA + NA outputs 0)

###
library("survey",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
fullnsduh_design <- svydesign(id = ~ VEREP , strata = ~ VESTR , data = NSDUH20022019_reduceage , weights = ~ ANALWC18 , nest = TRUE)
religtable<-svytable(~all_GIMP+all_DCSN,fullnsduh_design)
lsr::cramersV(religtable)
####Risk

NSDUH20022019_reduceage$all_RiskDanger<-base::rowSums(NSDUH20022019_reduceage[,c("RSKYFQDGR","RKFQDNGR")],na.rm=TRUE) ###combine youth and adult
NSDUH20022019_reduceage$all_RiskDanger[NSDUH20022019_reduceage$all_RiskDanger==0]<-NA ##set NA to zeros (rowSums NA + NA outputs 0)


NSDUH20022019_reduceage$all_RiskTest<-base::rowSums(NSDUH20022019_reduceage[,c("RSKYFQTES","RKFQRSKY")],na.rm=TRUE) ###combine youth and adult
NSDUH20022019_reduceage$all_RiskTest[NSDUH20022019_reduceage$all_RiskTest==0]<-NA ##set NA to zeros (rowSums NA + NA outputs 0)


NSDUH20022019_reduceage$riskpropensity<-base::rowSums(NSDUH20022019_reduceage[,c("all_RiskDanger","all_RiskTest")],na.rm=TRUE)

NSDUH20022019_reduceage$riskpropensity[NSDUH20022019_reduceage$riskpropensity==0]<-NA ##set NA to zeros (rowSums NA + NA outputs 0)
NSDUH20022019_reduceage$BLT<-dplyr::if_else(NSDUH20022019_reduceage$RKFQPBLT==1,1,0)


NSDUH20022019_reduceage_num<-sapply(NSDUH20022019_reduceage, haven::zap_labels)
NSDUH20022019_reduceage<-NSDUH20022019_reduceage_num

save(NSDUH20022019_reduceage,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reducevarcomputed.2021112.Rdata")




