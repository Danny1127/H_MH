# calculate probability and ratio

rm(list=ls())

raw_data<-read.table(file="experiment_dataset.csv",header=TRUE,sep=",")

xdata<-as.data.frame(lapply(raw_data,as.factor))
data<-xdata[complete.cases(xdata[,-c(3,4,9,10)]),-c(3,4,9,10)]
"%w/o%" <- function(x,y) x[!x %in% y]
all_formula<-as.formula(paste("Bucarin_consumption~","Insurance_Type*Drug_Type+",paste((names(data) %w/o% c("Drug_Type","Insurance_Type","Bucarin_consumption")) ,collapse="+")))
all<-list()

# do for all data
lfit<-glm(formula = all_formula,family = binomial(link="logit"),data)
print(summary(lfit))
results<-draw_CI(lfit,data=data)
sink("mean_se.txt")
for(i in results){
  for(j in i){
    cat(j," ")
  }
  cat("\n")
}
sink()
print(as.matrix(results))
data$treatment<-as.factor(apply(data[,c("Drug_Type","Insurance_Type")],1,function(x){
  paste0(ifelse(x[1]==1,"H",ifelse(x[1]==0,"L","NA")),
         ifelse(x[2]==0,"N",ifelse(x[2]==1,"T",
                                   ifelse(x[2]==2,"I","NA"))),collapse="_")
}))
by(data=as.integer(data$Bucarin_consumption),data$treatment,sd)
