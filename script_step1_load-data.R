

# Libraries ---------------------------------------------------------------

library(tidyverse)

# Clear everything  -------------------------------------------------------

rm(list=ls())

# Paths -------------------------------------------------------------------

#path to local folder where I run the analyses 
path <- "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Poverty Trajectories SHARE/"

# Load the data  ----------------------------------------------------------

load(file=paste0(path, "GH_SHARE_g_rel9-0-0_ALL_datasets_R/GH_SHARE_g.rdata"))

#rename and remove the incomprehensibly named one
data <- H_SHARE_g 
rm(H_SHARE_g)

# Saving ------------------------------------------------------------------

save(data, file=paste0(path, "WORKING-DATA/data_step1_harmonized-SHARE.Rdata"))
