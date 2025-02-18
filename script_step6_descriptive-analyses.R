

# Libraries ---------------------------------------------------------------
# 
# install.packages("TraMineR")
# install.packages("TraMineRextras")

library(tidyverse)
library(TraMineR)
library(TraMineRextras)


# Clear everything  -------------------------------------------------------

rm(list = ls())
load(file="data_step4-out_pov-STS.Rdata")
load(file="data_step4-out_trajectories-STS.Rdata")
load(file="data_covariates-pov-sts.Rdata")
load( file="data_covariates-tra-sts.Rdata")


# descriptive analyses all trajectories -----------------------------------

seqIplot(tra.seq)
seqiplot(tra.seq)
seqdplot(tra.seq)

# grouped plots 

seqdplot(tra.seq, group= cov.data.tra$gender.rcd)
  seqdplot(tra.seq, group= cov.data.tra$is.swiss)
  seqdplot(tra.seq, group= cov.data.tra$edu.rcd)
  
