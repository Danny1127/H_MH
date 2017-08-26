# Impossibility Check (IC)
data_subset_IC <- experiment_dataset[!(experiment_dataset$acutual_insurance_status == 6 &
                                         experiment_dataset$Spending_power <= 8 &
                                         experiment_dataset$Bucarin_consumption == 1),]
#561 out of 610 survived the impossibility check

IC_fit1 <- glm(data_subset_IC$Bucarin_consumption ~ data_subset_IC$Insurance_Type, family = binomial(link = "logit"))
summary(IC_fit1)

IC_fit2 <- glm(data_subset_IC$Bucarin_consumption ~ data_subset_IC$Insurance_Type*data_subset_IC$Drug_Type, family = binomial(link = "logit"))
summary(IC_fit2)

IC_fit3 <- glm(data_subset_IC$Bucarin_consumption ~ data_subset_IC$Insurance_Type*data_subset_IC$Drug_Type + data_subset_IC$Education
               + data_subset_IC$Hispanic_indicator + data_subset_IC$Race + data_subset_IC$Sex + data_subset_IC$before_tax_income
               + data_subset_IC$Spending_power + data_subset_IC$acutual_insurance_status, family = binomial(link = "logit"))
summary(IC_fit3)

IC_sub1 <- subset(data_subset_IC, data_subset_IC$Insurance_Type == 1 | data_subset_IC$Insurance_Type == 2)
IC_sub1$Insurance_Type[IC_sub1$Insurance_Type == 1] <- 0
IC_sub1$Insurance_Type[IC_sub1$Insurance_Type == 2] <- 1

IC_fit4 <- glm(IC_sub1$Bucarin_consumption ~ IC_sub1$Insurance_Type, family = binomial(link = "logit"))
summary(IC_fit4)

IC_fit5 <- glm(IC_sub1$Bucarin_consumption ~ IC_sub1$Insurance_Type * IC_sub1$Drug_Type, family = binomial(link = "logit"))
summary(IC_fit5)

IC_fit6 <- glm(IC_sub1$Bucarin_consumption ~ IC_sub1$Insurance_Type * IC_sub1$Drug_Type + IC_sub1$Education
               + IC_sub1$Hispanic_indicator + IC_sub1$Race + IC_sub1$Sex + IC_sub1$before_tax_income
               +IC_sub1$Spending_power + IC_sub1$acutual_insurance_status, family = binomial(link = "logit"))
summary(IC_fit6)

IC_sub2 <- data_subset_IC[!(data_subset_IC$Insurance_Type == 2),]
# subset the Manipulation Check data where insurance_type=0 for no insurance; insurance type=1 for traditional insurance
# This is testing the effects of switching from no insurance to traditional insurance.

IC_fit7 <- glm(IC_sub2$Bucarin_consumption ~ IC_sub2$Insurance_Type, family = binomial(link = "logit"))
summary(IC_fit7)

IC_fit8 <- glm(IC_sub2$Bucarin_consumption ~ IC_sub2$Insurance_Type * IC_sub2$Drug_Type, family = binomial(link = "logit"))
summary(IC_fit8)

IC_fit9 <- glm(IC_sub2$Bucarin_consumption ~ IC_sub2$Insurance_Type * IC_sub2$Drug_Type + IC_sub2$Education
               + IC_sub2$Hispanic_indicator + IC_sub2$Race + IC_sub2$Sex + IC_sub2$before_tax_income
               +IC_sub2$Spending_power + IC_sub2$acutual_insurance_status, family = binomial(link = "logit"))
summary(IC_fit9)
