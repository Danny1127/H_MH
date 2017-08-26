# Manipulation Check (MC)

data_subset_MC <- subset(experiment_dataset, experiment_dataset$Insurance_Type == experiment_dataset$hypothetical_insurance_status)
#532 out of 610 survived the manipulation check


MC_fit1 <- glm(data_subset_MC$Bucarin_consumption ~ data_subset_MC$Insurance_Type, family = binomial(link = "logit"))
summary(MC_fit1)

MC_fit2 <- glm(data_subset_MC$Bucarin_consumption ~ data_subset_MC$Drug_Type*data_subset_MC$Insurance_Type,
               family = binomial(link = "logit"))
summary(MC_fit2)

MC_fit3 <- glm(data_subset_MC$Bucarin_consumption ~ data_subset_MC$Drug_Type*data_subset_MC$Insurance_Type + data_subset_MC$Education
               + data_subset_MC$Hispanic_indicator + data_subset_MC$Race + data_subset_MC$Sex + data_subset_MC$before_tax_income
               + data_subset_MC$Spending_power + data_subset_MC$acutual_insurance_status, family = binomial(link = "logit") )
summary(MC_fit3)

MC_sub1 <- subset(data_subset_MC, data_subset_MC$Insurance_Type == 1 | data_subset_MC$Insurance_Type == 2)
MC_sub1$Insurance_Type[MC_sub1$Insurance_Type == 1] <- 0
MC_sub1$Insurance_Type[MC_sub1$Insurance_Type == 2] <- 1
# subset the manipulation check data where insurance type = 0 for traditional insurance; insurance type = 1
# for indemnity insurance. This is estimating the effects of switching from traditional insurance to indeminity insurance.

MC_fit4 <- glm(MC_sub1$Bucarin_consumption ~ MC_sub1$Insurance_Type, family = binomial(link = "logit"))
summary(MC_fit4)

MC_fit5 <- glm(MC_sub1$Bucarin_consumption ~ MC_sub1$Insurance_Type * MC_sub1$Drug_Type, family = binomial(link = "logit"))
summary(MC_fit5)

MC_fit6 <- glm(MC_sub1$Bucarin_consumption ~ MC_sub1$Insurance_Type * MC_sub1$Drug_Type + MC_sub1$Education
               + MC_sub1$Hispanic_indicator + MC_sub1$Race + MC_sub1$Sex + MC_sub1$before_tax_income
               +MC_sub1$Spending_power + MC_sub1$acutual_insurance_status, family = binomial(link = "logit"))
summary(MC_fit6)

MC_sub2 <- data_subset_MC[!(data_subset_MC$Insurance_Type == 2),]
# subset the Manipulation Check data where insurance_type=0 for no insurance; insurance type=1 for traditional insurance
# This is testing the effects of switching from no insurance to traditional insurance.

MC_fit7 <- glm(MC_sub2$Bucarin_consumption ~ MC_sub2$Insurance_Type, family = binomial(link = "logit"))
summary(MC_fit7)

MC_fit8 <- glm(MC_sub2$Bucarin_consumption ~ MC_sub2$Insurance_Type * MC_sub2$Drug_Type, family = binomial(link = "logit"))
summary(MC_fit8)

MC_fit9 <- glm(MC_sub2$Bucarin_consumption ~ MC_sub2$Insurance_Type * MC_sub2$Drug_Type + MC_sub2$Education
               + MC_sub2$Hispanic_indicator + MC_sub2$Race + MC_sub2$Sex + MC_sub2$before_tax_income
               +MC_sub2$Spending_power + MC_sub2$acutual_insurance_status, family = binomial(link = "logit"))
summary(MC_fit9)
