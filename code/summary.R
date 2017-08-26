rm(list=ls())
mainDir<-getwd()
dir.create(file.path(mainDir, "summary_test"), showWarnings = FALSE)
source("code/my_summary.R")

# change your data input here
data<-read.table("experiment_dataset.csv",header=TRUE,sep=",")

#for all
my_summary(data=data)
#by drug type
for (i in 0:1){
  subdata<-subset(data,data$Drug_Type==i)
  subdata$Drug_Type<-NULL
  my_summary(data=subdata,file=paste0("summary/drug_summary",i,".csv"))
}

#by Insurance type

for (i in 0:2){
  subdata<-subset(data,data$Insurance_Type==i)
  subdata$Insurance_Type<-NULL
  my_summary(data=subdata,file=paste0("summary/insurance_summary",i,".csv"))
}

#by both
for (i in 0:1){
  for (j in 0:2){
    subdata<-subset(data,data$Drug_Type==i&data$Insurance_Type==j)
    subdata$Insurance_Type<-NULL
    subdata$Drug_Type<-NULL
    my_summary(data=subdata,file=paste0("summary/ins",j,"_drug",i,"_summary.csv"))
  }
}
subdata<-NULL

