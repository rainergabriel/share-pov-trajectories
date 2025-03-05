rm(list=ls())
load(file="data_step3-out-variables-all-setup.Rdata")

prop.table(table(data$poverty.bn.w6))
