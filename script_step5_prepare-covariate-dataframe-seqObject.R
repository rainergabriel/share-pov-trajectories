


library(tidyverse)
library(dplyr)

rm(list = ls())
load(file="data_step4-out_pov-STS.Rdata")
load(file="data_step4-out_trajectories-STS.Rdata")
load(file="data_covariates-sts.Rdata")
load(file="data_step3-out-variables-all-setup.Rdata")





# for all trajectories ----------------------------------------------------


ids <- rownames(tra.seq)

# create a dataframe with covariates that i can use to order the plots in the next analyses part
cov.container <- as_tibble(ids)
names(cov.container) <- "mergeid"

covs.from.data <- data %>% select(
  mergeid,
  gender.rcd, 
  is.swiss, 
  edu.rcd
)

cov.data.tra <- left_join(cov.container, covs.from.data, by="mergeid")


# saving ------------------------------------------------------------------

save(cov.data.tra, file="data_covariates-tra-sts.Rdata")


# just for the poor  ------------------------------------------------------

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

cov.data.pov <- left_join(cov.container, covs.from.data, by="mergeid")


# saving ------------------------------------------------------------------

save(cov.data.pov, file="data_covariates-pov-sts.Rdata")

