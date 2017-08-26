
# Manipulation Check (MC)
rm(list=ls())
experiment_dataset<-read.csv(file="data-final.csv")
data_subset_MC <- subset(experiment_dataset, experiment_dataset$Insurance_Type == experiment_dataset$hypothetical_insurance_status)
#532 out of 610 survived the manipulation check

#library(party)
#tree_MC <- ctree(Bucarin_consumption ~ Drug_Type+Insurance_Type, data = data_subset_MC)
check<-c("Drug_Type","Insurance_Type")
sink("ttest.txt")
for (i in 0:2){
  sub<-subset(data_subset_MC,Insurance_Type!=i)
  cat(i)
  by(sub[,c("Bucarin_consumption","Insurance_Type")],sub$Drug_Type,function(x){
   print(t.test(x[,1]~x[,2])) })
}
sink()
#glm(formula = Bucarin_consumption ~ Insurance_Type * Drug_Type, weights =  )
