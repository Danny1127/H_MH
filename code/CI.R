rm(list=ls())
library(ggplot2)


data<-read.table(file="experiment_dataset.csv", header=TRUE,sep=",")

mean6<-aggregate(Bucarin_consumption~Drug_Type+Insurance_Type,data=data,mean)
mean<-as.matrix(mean6[,3])
rownames(mean)<-apply(mean6[,-3],1,function(x) paste0(x,collapse=""))
#mean6<-as.matrix(mean6[,3])

# create a function to add CI
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

sum <- aggregate(Bucarin_consumption~Drug_Type+Insurance_Type,data=data,sum)
N<-sum[,3]/mean

#appstd <- as.matrix(sqrt(mean*(1-mean))/N) * 1.96 

std6<-aggregate(Bucarin_consumption~Drug_Type+Insurance_Type,data=data, sd)
std<-as.matrix(std6[,3]) * 1.96
rownames(std)<-apply(std6[,-3],1,function(x) paste0(x,collapse=""))

zbarplot <- barplot(mean , beside=T,legend.text=T,col=heat.colors(nrow(std)) , ylim=c(0,1) , ylab="Probability %")
error.bar(zbarplot,mean, std)

data$treatment<-apply(data[,c("Drug_Type","Insurance_Type")],1,function(x){
  paste0(ifelse(x[1]==1,"H",ifelse(x[1]==0,"L","NA")),
                  ifelse(x[2]==0,"N",ifelse(x[2]==1,"T",
                  ifelse(x[2]==2,"I","NA"))),collapse="_")
  })
data$rich<-ifelse(data$Spending_power>=8,1,0)

# not working yet 
# ggplot(data=data, aes(x=treatment, y=Bucarin_consumption,fill=rich)) +
#   geom_bar(stat="identity") +
#   guides(fill=FALSE) +
#   xlab("Treatment") + ylab("Consumption of Bucarin") +
#   ggtitle("Consumption likelihood comparison") + 
#   geom_errorbar(aes(ymin=Bucarin_consumption-std,ymax=Bucarin_consumption+std)) 
#   

