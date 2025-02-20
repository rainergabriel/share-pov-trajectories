

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
load(file="data_step4-out_pov-STS.Rdata")
load(file="data_step4-out_trajectories-STS.Rdata")
load(file="data_covariates-pov-sts.Rdata")
load( file="data_covariates-tra-sts.Rdata")


# descriptive analyses all trajectories -----------------------------------


summary(tra.seq)
cpal(tra.seq) <- c(brewer.pal(3, name="Reds"),
                          "green",
                          "darkgreen", 
                          "magenta", 
                          brewer.pal(4, name="Blues"), 
                          "white")


cpal(tra.seq) <- c("#A50026", "#D73027", "#F46D43", "#006D2C", "#FFFFFF", "#F0F0F0")

seqIplot(tra.seq)
seqiplot(tra.seq)
seqdplot(tra.seq)

# grouped plots 

seqdplot(tra.seq, group= cov.data.tra$gender.rcd)
  seqdplot(tra.seq, group= cov.data.tra$is.swiss)
  seqdplot(tra.seq, group= cov.data.tra$edu.rcd)
  

# pov ---------------------------------------------------------------------


  
  seqIplot(pov.seq, sort="from.end")
  seqiplot(pov.seq)
  seqdplot(pov.seq)
  