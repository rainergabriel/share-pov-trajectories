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
  assign(dataset_name, haven::read_sav(file))
  cat("Loaded:", dataset_name, "\n")
}

# ------------------------------
# Extract and Aggregate "hnetw"
# ------------------------------

# List all dataset names in the environment, EXCLUDING wave 1 (w1)
dataset_names <- ls(pattern = "sharew[2-9]_.*_gv_imputations")  # Only waves 2-9

# Function to process and extract "hnetw" and compute mean of imputations
process_dataset_hnetw <- function(dataset_name) {
  dataset <- get(dataset_name)  # Retrieve dataset from environment
  
  if (!"hnetw" %in% names(dataset)) {
    message("Variable 'hnetw' not found in dataset: ", dataset_name)
    return(NULL)
  }
  
  # Extract wave suffix (e.g., "w2" from "sharew2...")
  wave_suffix <- gsub(".*(w\\d+).*$", "\\1", dataset_name)
  new_var_name <- paste0(wave_suffix, ".hnetw")  # e.g., "w2.hnetw"
  
  dataset %>%
    group_by(mergeid) %>%
    summarise(!!new_var_name := mean(hnetw, na.rm = TRUE), .groups = "drop")  # Compute mean of imputations
}

# Apply function to each dataset
data_list_hnetw <- map(dataset_names, process_dataset_hnetw)

# Remove NULL entries (in case a dataset was missing "hnetw")
data_list_hnetw <- compact(data_list_hnetw)

# Merge all datasets using mergeid
hnetw_data <- reduce(data_list_hnetw, left_join, by = "mergeid")

# Print first few rows as a check
print(head(hnetw_data))

# Save dataset as .Rdata
save(hnetw_data, file = "data_wave-specific-hnetw.Rdata")
