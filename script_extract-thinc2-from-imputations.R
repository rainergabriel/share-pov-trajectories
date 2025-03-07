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
# Extract and Aggregate "thinc2"
# ------------------------------

# List all dataset names in the environment, EXCLUDING wave 1 (w1)
dataset_names <- ls(pattern = "sharew[2-9]_.*_gv_imputations")  # Only waves 2-9

# Function to process and extract "thinc2" and compute mean of imputations
process_dataset <- function(dataset_name) {
  dataset <- get(dataset_name)  # Retrieve dataset from environment
  
  if (!"thinc2" %in% names(dataset)) {
    message("Variable 'thinc2' not found in dataset: ", dataset_name)
    return(NULL)
  }
  
  # Extract wave suffix (e.g., "w2" from "sharew2...")
  wave_suffix <- gsub(".*(w\\d+).*", "\\1", dataset_name)
  new_var_name <- paste0(wave_suffix, ".thinc2")  # e.g., "w2.thinc2"
  
  dataset %>%
    group_by(mergeid) %>%
    summarise(!!new_var_name := mean(thinc2, na.rm = TRUE), .groups = "drop")  # Compute mean of imputations
}

# Apply function to each dataset
data_list <- map(dataset_names, process_dataset)

# Remove NULL entries (in case a dataset was missing "thinc2")
data_list <- compact(data_list)

# Merge all datasets using mergeid
thinc2_data <- reduce(data_list, left_join, by = "mergeid")



# Print first few rows as a check
print(head(thinc2_data))


# Save dataset as .Rdata
save(thinc2_data, file = "data_wave-specific-thinc2.Rdata")

