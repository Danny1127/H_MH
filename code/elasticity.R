data<-experiment_dataset

## method 1, use proportional demand
tab<-table(data$Bucarin_consumption, data$Insurance_Type)
# no insurance
q0<-tab['1','0']/sum(tab[,'0'])
# traditional insurance
q1<-tab['1','1']/sum(tab[,'1'])
#elasticity, price change -100%=-1
elasticity1<--(q1-q0)/q0
cat("initial point elasticity=",elasticity1,"\n")
## method 2, use log regression, consumption either 1 or 80K
data<-subset(data,data$Insurance_Type<2)
data$cons<-ifelse(data$Bucarin_consumption==1,80000,1)
lmfit<-lm(formula=log(data$cons)~data$Insurance_Type)
cat("log regression elasticity=",exp(lmfit$coefficients[2]),"\n")

## method 3, use mid point
elasticity2<-(q1-q0)/(q1+q0)/((0-80000)/(80000+0))
cat("mid point elasticity=",elasticity2,"\n")
