experiment_dataset <- read.csv(file.choose())
attach(experiment_dataset)

experiment_dataset$Bucarin_consumption[experiment_dataset$Bucarin_consumption > 1] <- 0
# Change the value of outcome variable: 0 -- No consumption; 1 -- consumption

colnames(experiment_dataset)[colnames(experiment_dataset) == "Maximal_purchase"] <- "Spending_power"
# Change variable name: from "maximal_purchase" to "spending_power"

experiment_dataset$hypothetical_insurance_status[experiment_dataset$hypothetical_insurance_status == 3] <- 2
experiment_dataset$hypothetical_insurance_status[experiment_dataset$hypothetical_insurance_status == 4] <- 0
# Change the values for hypothetical_insurance_status: 0: no insurance; 1: traditional insurance; 2: indemnity insurance

race_level <- levels(experiment_dataset$Race)
levels(experiment_dataset$Race) <- c(race_level, 7)
experiment_dataset$Race[experiment_dataset$Race == "1,3"|experiment_dataset$Race == "1,2" | experiment_dataset$Race == "1,2,3"
                        | experiment_dataset$Race == "1,2,4,5" | experiment_dataset$Race == "1,4" | experiment_dataset$Race == "1,4,5"
                        | experiment_dataset$Race == "1,6" | experiment_dataset$Race == "2,3"] <- "7"
describe(experiment_dataset$Race)



fit1 <-glm(Bucarin_consumption ~ Insurance_Type, family = binomial(link = "logit"))
summary(fit1)
# The effects of insurance type on the likelihood of consuming Bucarin

fit2 <- glm(Bucarin_consumption ~ Drug_Type*Insurance_Type, family = binomial(link = "logit"))
summary(fit2)
# Putting the two treatments in one logistic regression

fit3 <- glm(Bucarin_consumption ~ Drug_Type*Insurance_Type + Education + Hispanic_indicator + Race + Sex
            + before_tax_income + Spending_power + acutual_insurance_status, family = binomial(link = "logit"))
summary(fit3)
# Regression with two treament plus all of the demographic variables

# Subsetting data with insurance types including no insurance and traditional insurance.
data_subset_1 <- subset(experiment_dataset, Insurance_Type <= 1)
colnames(data_subset_1)[colnames(data_subset_1) == 'Insurance_Type'] <- "TI_Type"
# In this subset set, I am testing the effects of traditional insurance type

fit4 <-glm(data_subset_1$Bucarin_consumption ~ data_subset_1$TI_Type, family = binomial(link = "logit"))
summary(fit4)
# The effects of switching from no insurance to traditional insurance

fit5 <-glm(data_subset_1$Bucarin_consumption ~ data_subset_1$Drug_Type*data_subset_1$TI_Type, 
           family = binomial(link = "logit"))
summary(fit5)
# Regression with two treatments (two drug types; two insurance types: no insurance v. traditional insurance)

fit6 <-glm(data_subset_1$Bucarin_consumption ~ data_subset_1$Drug_Type*data_subset_1$TI_Type + data_subset_1$Education
           + data_subset_1$Hispanic_indicator + data_subset_1$Race + data_subset_1$Sex + data_subset_1$before_tax_income + data_subset_1$Spending_power
           + data_subset_1$acutual_insurance_status, 
           family = binomial(link = "logit"))
summary(fit6)
# Regression with two treatments and demographics (insurance types: no insurance v. traditional insurance)

# subsetting the data set with insurance types including traditional insurance and idemnity insurance
data_subset_2 <- subset(experiment_dataset, Insurance_Type >= 1)
data_subset_2$Insurance_Type <- data_subset_2$Insurance_Type - 1
colnames(data_subset_2)[colnames(data_subset_2) == 'Insurance_Type'] <- "II_Type"
# II_type: new insurance type in this subset. II_type = 0 means traditional insurance; II_type = 1 means idemnity insurance

fit7 <- glm(data_subset_2$Bucarin_consumption ~ data_subset_2$II_Type, family = binomial(link = "logit"))
summary(fit7)
# The effects of switching from traditional insurance type to idemnity insurance. There does not suggest statistic significance.


fit8 <- glm(data_subset_2$Bucarin_consumption ~ data_subset_2$Drug_Type*data_subset_2$II_Type, 
           family = binomial(link = "logit"))
summary(fit8)

fit9 <-glm(data_subset_2$Bucarin_consumption ~ data_subset_2$Drug_Type*data_subset_2$II_Type + data_subset_2$Education
           + data_subset_2$Hispanic_indicator + data_subset_2$Race + data_subset_2$Sex + data_subset_2$before_tax_income + data_subset_2$Spending_power
           + data_subset_2$acutual_insurance_status, 
           family = binomial(link = "logit"))
summary(fit9)

#Descriptive statistics, use Package (Hmisc) and then describe(name of the dataset), all of the 
#descrive statistics information will show up
library(Hmisc)
describe(experiment_dataset)


# power analysis

df_t <- data.frame(table(experiment_dataset$Bucarin_consumption[experiment_dataset$Drug_Type == 0 & 
                                                                  experiment_dataset$Insurance_Type == 0]))
df_t
Prob_Low_NI <- df_t[2,2] / (df_t[1,2] + df_t[2,2])
Prob_Low_NI # 0.2

# Power analysis for insurance effects in low value group
library(pwr)
pwr.2p.test(h = 0.1, n = NULL, sig.level = 0.05, power = 0.8)
power.prop.test(n = NULL, p1 = 0.2, p2 = 0.2*1.2, sig.level = 0.05, power = 0.5) # Effects size 0.2
power.prop.test(n = NULL, p1 = 0.2, p2 = 0.2*1.15, sig.level = 0.05, power = 0.5) # Effects size 0.15

df_t <- data.frame(table(experiment_dataset$Bucarin_consumption[experiment_dataset$Drug_Type == 0 & 
                                                                  experiment_dataset$Insurance_Type == 1]))
df_t
Prob_Low_TI <- df_t[2,2] / (df_t[1,2] + df_t[2,2])
Prob_Low_TI # 0.4712

power.prop.test(n = NULL, p1 = 0.4712, p2 = 0.4712*1.1, sig.level = 0.05, power = 0.5) #Effect size 0.1
# Under low-value drug: the effects of indeminity insurance
power.prop.test(n = NULL, p1 = 0.4712, p2 = 0.4712*1.05, sig.level = 0.05, power = 0.5)# Effect size 0.05

df_t <- data.frame(table(experiment_dataset$Bucarin_consumption[experiment_dataset$Drug_Type == 1 & 
                                                                  experiment_dataset$Insurance_Type == 0]))
df_t
Prob_High_NI <- df_t[2,2] / (df_t[1,2] + df_t[2,2])
Prob_High_NI # 0.2255
power.prop.test(n = NULL, p1 = Prob_High_NI, p2 = Prob_High_NI*1.2, sig.level = 0.05, power = 0.5)
power.prop.test(n = NULL, p1 = Prob_High_NI, p2 = Prob_High_NI*1.15, sig.level = 0.05, power = 0.5)

df_t <- data.frame(table(experiment_dataset$Bucarin_consumption[experiment_dataset$Drug_Type == 1 & 
                                                                  experiment_dataset$Insurance_Type == 1]))
df_t
Prob_High_TI <- df_t[2,2] / (df_t[1,2] + df_t[2,2])
Prob_High_TI # 0.7157

power.prop.test(n = NULL, p1 = Prob_High_TI, p2 = Prob_High_TI*1.1, sig.level = 0.05, power = 0.5)
power.prop.test(n = NULL, p1 = Prob_High_TI, p2 = Prob_High_TI*1.05, sig.level = 0.05, power = 0.5)

df_t <- data.frame(table(experiment_dataset$Bucarin_consumption[experiment_dataset$Drug_Type == 0]))
df_t
Prob_Low <- df_t[2,2] / (df_t[1,2] + df_t[2,2])
Prob_Low # 0.3684

power.prop.test(n = NULL, p1 = Prob_Low, p2 = Prob_Low * 1.2, sig.level = 0.05, power = 0.8)
power.prop.test(n = NULL, p1 = Prob_Low, p2 = Prob_Low * 1.15, sig.level = 0.05, power = 0.8)
