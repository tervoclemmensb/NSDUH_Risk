library("ggplot2",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")
library("patchwork",lib="/local_mount/space/arya/1/users/Rpackages_4.0.2")


#####plots load##########

load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/byagepeaks.Rdata")

load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/fullageplotsRT.Rdata")

load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/RTatpeak.Rdata")

load(file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/cohensd.add.alccan.Rdata")

#####

allplots<-(ggallfits+ggagepeakplot)/(ggvalatpeakplot+bystratarsqfigalcadd)


ggsave(allplots,file="/local_mount/space/arya/1/users/Brenden/Projects/NSDUH/Figures/fullpanelfigure.20220711.pdf",height=16,width=22)


