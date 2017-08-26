library(boot)
# calculate probability and ratio

rm(list=ls())
############################# read in data ########################
raw_data<-read.table(file="experiment_dataset.csv",header=TRUE,sep=",")

sdata<-as.data.frame(lapply(raw_data,as.factor))

data<-sdata[complete.cases(sdata[,-c(4,9)]),-c(4)]
### create new variable
data$treatment<-as.factor(apply(data[,c("Drug_Type","Insurance_Type")],1,function(x){
  paste0(ifelse(x[1]==1,"H",ifelse(x[1]==0,"L","NA")),
         ifelse(x[2]==0,"N",ifelse(x[2]==1,"T",
                                   ifelse(x[2]==2,"I","NA"))),collapse="_")
}))


### specify the formula
"%w/o%" <- function(x,y) x[!x %in% y]
all_formula<-as.formula(paste("Bucarin_consumption~","Insurance_Type*Drug_Type+",
                              paste((names(data) %w/o% c("Drug_Type","Insurance_Type","Bucarin_consumption",
                                                         "treatment","hypothetical_insurance_status")) ,collapse="+")))

### subset data 
save_data<-data

sdata<-subset(data,data$hypothetical_insurance_status==data$Insurance_Type)
data<-subset(sdata,!(sdata$Insurance_Type==0&as.integer(sdata$Spending_power)<=8 &sdata$Bucarin_consumption==1))

# xdata<-subset(raw_data,raw_data$Insurance_Type!=0)
# ydata<-subset(raw_data,raw_data$Insurance_Type!=1)
# zdata<-subset(raw_data,raw_data$Insurance_Type!=2)

xdata<-subset(data,data$Insurance_Type!=0)
ydata<-subset(data,data$Insurance_Type!=1)
zdata<-subset(data,data$Insurance_Type!=2)
lfit<-glm(formula = all_formula,family = binomial(link="logit"),data)


summary(lfit)


### function to bootstrap
CI<-function(formula,data,indices,select,new){
  d <- data[indices,] # allows boot to select sample 
  dnew<- new[indices,]
  lfit<-glm(formula = all_formula,family = binomial(link="logit"),d)
  ### mX
  mX <- mean(predict(lfit,newdata=dnew,type="link"))
  prob <- plogis(mX,location=0,scale=1)
  fit_mean<-mean(prob)
  return(fit_mean)
}

N<-nrow(data)

### record result 
filename<-"bootstrap_mX_less.txt"
file.remove(filename)
for (x in levels(droplevels(data$treatment))){
  new<-data
  y<-strsplit(x,split = "")[[1]]
  new$Drug_Type<-as.factor(ifelse("H" %in% y,1,0))
  new$Insurance_Type<-as.factor(ifelse("N" %in% y,0,ifelse("T" %in% y,1,2)))
  #!!! 10000 takes 40 min
  results <- boot(data=data, statistic=CI, 
                  R=1000, formula=all_formula,select=x,new=new)
  ci<-boot.ci(results, type="bca")
  write(paste0(c(x,ci$t0,"CI",ci$bca[4],ci$bca[5]), collapse=" "),file=filename,append=T)
}



