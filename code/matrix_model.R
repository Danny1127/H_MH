rm(list=ls())
############################# read in data ########################
raw_data<-read.table(file="experiment_dataset.csv",header=TRUE,sep=",")

xdata<-as.data.frame(lapply(raw_data,as.factor))

data<-xdata[complete.cases(xdata[,-c(4,9,10)]),-c(4)]
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
                                                         "treatment","hypothetical_insurance_status","acutual_insurance_status")) ,collapse="+")))
Xstar<- model.matrix(all_formula,data=data)

attr(Xstar,"contrasts")
lfit<-glm(formula = all_formula,family = binomial(link="logit"),data)
beta_hat<-lfit$coefficients
xbar<-apply(Xstar,2,mean)

V_beta_hat<-vcov(lfit)

sqrt(diag(V_beta_hat))



