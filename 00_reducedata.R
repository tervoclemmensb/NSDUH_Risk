library("mgcv",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("dplyr",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("ggplot2",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("tzdb",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2/")
library("vroom",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("haven",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")

#####p 443 for AUD
#####p 446 for CUD

NSDUH20022019<-haven::read_sav("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019.sav")

#NSDUH20022019<-vroom("/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019_Tab.tsv") ##load
NSDUH20022019<-data.frame(NSDUH20022019) ##convert to dataframe
usevars<-c("YEAR","AGE2","IRSEX","NEWRACE2","INCOME","COUTYP2","SNRLGIMP","SNRLDCSN","YERLGIMP","YERLDCSN","MRDAYPYR","MJYRTOT","IRMJFY","DEPNDMRJ","ABUSEMRJ","MJAGE","IRALCFY","ALCYRTOT","ALCTRY","DEPNDALC","ABUSEALC","ANALWT_C","RSKYFQDGR","RKFQDNGR","RSKYFQTES","RKFQRSKY","RKFQPBLT","RKFQDBLT","HEALTH2","ANALWT_C","VESTR","VEREP","BOOKED","NOBOOKY2","NOBOOKY2","YESTSMJ","YESTSALC","TXYRALC","TXYRILL","IMPWEEKS","SPD_USCR","SPD_UADJ","YMDEYR","AMDEYR","IRMJRC","IRALCRC")
CUDvars<-c("MRJKPLMT","MRJCUTDN","MRJCUTEV","MRJLOTTM","MRJGTOVR","MRJSERPB","MRJFMFPB","MRJFMCTD","MRJLSACT","MRJPDANG","MRJEMOPB","MRJEMCTD","MRJPHLPB","MRJPHCTD","MRJNDMOR","MRJLSEFX") 
mjattitudes<-c("RSKMJOCC","RSKMJREG")
mjavailable<-c("DIFGETMRJ")
usevars<-c(usevars,CUDvars,mjavailable,mjattitudes)
usevars<-c(usevars,grep("ANALWC",names(NSDUH20022019),value=TRUE))

##define variables we want to keep
usevars[!(usevars %in% names(NSDUH20022019))] ###check that all keep variables exist in data
NSDUH20022019_reduce<-NSDUH20022019[,usevars] ### reduce data to only those variables we want to keep

save(NSDUH20022019_reduce,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Data/NSDUH_2002_2019RISKtaking_reduce_20211103.Rdata") ##save


