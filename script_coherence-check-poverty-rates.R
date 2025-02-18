rm(list=ls())
load(file="data_step3-out-variables-all-setup.Rdata")

summary(data$h6ittot)
prop.table(table(data$poverty.bn.w6))

           