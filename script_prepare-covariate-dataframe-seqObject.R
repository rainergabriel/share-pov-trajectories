


library(tidyverse)

rm(list=ls())
load(file="data_step3-out-variables-all-setup.Rdata")
load(file="data_step4-out_pov-STS.Rdata")

ids <- rownames(pov.seq)

# create a dataframe with covariates that i can use to order the plots in the next analyses part
cov.container <- as_tibble(ids)
names(cov.container) <- "mergeid"

covs.from.data <- data %>% select(
  mergeid,
  gender.rcd, 
  is.swiss, 
  edu.rcd
)


cov.data <- left_join(cov.container, covs.from.data, by="mergeid")


# saving ------------------------------------------------------------------

save(cov.data, file="data_covariates-sts.Rdata")
