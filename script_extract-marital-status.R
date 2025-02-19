# Load required libraries
library(tidyverse)
library(haven) 
library(purrr)

# Clear the environment
rm(list = ls())

# Define the directory path
directory_path <- "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Poverty Trajectories SHARE/SHARE-Raw-Data/"

# Load all imputed CSV files into a list of data frames
file_list <- list.files(
  path = directory_path,
  pattern = "_gv_imputations\\.sav$",
  full.names = TRUE,
  recursive = TRUE
)

# Dynamically load each .sav file into the environment
for (file in file_list) {
  dataset_name <- make.names(gsub("\\.sav$", "", basename(file)))
  dataset <- haven::read_sav(file) %>% filter(country == 20)  # Subset only Switzerland
  assign(dataset_name, dataset)
  cat("Loaded:", dataset_name, "\n")
}

# ------------------------------
# Extract and Aggregate "mstat"
# ------------------------------

# List all dataset names in the environment, EXCLUDING wave 1 (w1)
dataset_names <- ls(pattern = "sharew[2-9]_.*_gv_imputations")  # Only waves 2-9

# Function to extract and recode "mstat"
process_mstat <- function(dataset_name) {
  dataset <- get(dataset_name)  # Retrieve dataset from environment
  
  if (!"mstat" %in% names(dataset)) {
    message("Variable 'mstat' not found in dataset: ", dataset_name)
    return(NULL)
  }
  
  # Extract wave suffix (e.g., "w2" from "sharew2...")
  wave_suffix <- gsub(".*(w\\d+).*", "\\1", dataset_name)
  new_var_name <- paste0(wave_suffix, ".married.bn")  # e.g., "w2.married.bn"
  
  dataset %>%
    select(mergeid, mstat) %>%
    mutate(!!new_var_name := ifelse(mstat %in% c(1, 2, 3), 1, 0)) %>%  # Recode into binary indicator
    select(-mstat)  # Remove original column
}

# Apply function to each dataset
data_list <- map(dataset_names, process_mstat)

# Remove NULL entries (in case a dataset was missing "mstat")
data_list <- compact(data_list)

# Merge all datasets using mergeid
mstat_data <- reduce(data_list, left_join, by = "mergeid")

# Print first few rows as a check
print(head(mstat_data))

ids <- unique(mstat_data$mergeid)
length(ids)

mstat_data <- mstat_data[ids,]

# Save dataset as .Rdata
save(mstat_data, file = "data_wave-specific-married.bn.Rdata")