


# Libraries ---------------------------------------------------------------

library(tidyverse)


# Clear everything  -------------------------------------------------------

rm(list=ls())

# Paths -------------------------------------------------------------------

#path to local folder where I run the analyses 
path <- "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Poverty Trajectories SHARE/"

# Load the data  ----------------------------------------------------------

load(file=paste0(path, "WORKING-DATA/data_step1_harmonized-SHARE.Rdata"))

# Subset  -----------------------------------------------------------------

levels(as.factor(data$country))
data <- data %>% filter(country=="20")


# Save --------------------------------------------------------------------


save(data, file=paste0(path, "WORKING-DATA/data_step2-out_harmonized-Swiss.Rdata"))

