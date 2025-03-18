


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

# based on country: aka just take Swiss respondents
levels(as.factor(data$country))
data <- data %>% filter(country=="20")

# based on age
2020-65 #take only people that aren't any younger than born 1955, because with this, there is a chance that they've at least
# been observed twice in the dataset, then they were at least 65 in the before last wave 
data <- data %>% filter(rabyear<=1955) 

# Save --------------------------------------------------------------------


save(data, file=paste0(path, "WORKING-DATA/data_step2-out_harmonized-Swiss.Rdata"))

