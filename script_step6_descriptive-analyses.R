

# Libraries ---------------------------------------------------------------
# 
# install.packages("TraMineR")
# install.packages("TraMineRextras")

library(tidyverse)
library(TraMineR)
library(TraMineRextras)
library(RColorBrewer)



# Clear everything  -------------------------------------------------------

rm(list = ls())
load( file="data_step3-out-variables-all-setup.Rdata")

subset <- data %>% select(mergeid, starts_with("joint.income.wealth.poverty"), eduyears, highest_lifetime_ISCO_88_recoded, gender.rcd, cohort, starts_with("valid.information")) %>% 
  mutate(across(everything(), 
                ~if_else(is.na(.), "Missing", as.character(.))))

names(subset)
 
options(digits=3)

x1 <- prop.table(table(subset$cohort))*100
x1
write.csv(x1, file="table_sampledesc_cohort.csv")

x1 <- prop.table(table(subset$gender.rcd))*100
x1

x1 <- mean(subset$eduyears, na.rm=TRUE)
x1
x2 <- sd(subset$eduyears)
x2
length(which(is.na(subset$eduyears)))

x1 <- prop.table(table(subset$joint.income.wealth.poverty.bn.w2))*100
x2 <- prop.table(table(subset$joint.income.wealth.poverty.bn.w4))*100
x3 <- prop.table(table(subset$joint.income.wealth.poverty.bn.w5))*100
x4 <- prop.table(table(subset$joint.income.wealth.poverty.bn.w6))*100
x5 <- prop.table(table(subset$joint.income.wealth.poverty.bn.w7))*100
x6 <- prop.table(table(subset$joint.income.wealth.poverty.bn.w8))*100
x7 <- prop.table(table(subset$joint.income.wealth.poverty.bn.w9))*100
cons <- rbind(x1,x2,x3,x4,x5,x6,x7)
print(cons)
write.csv(cons, file="table_sampledesc_JIWP.csv")

x2 <- prop.table(table(subset$highest_lifetime_ISCO_88_recoded))*100
x2
