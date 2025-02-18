library(tidyverse)
library(haven)

rm(list = ls())

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

# Function to process dataset and extract "thinc2"
process_dataset <- function(dataset, dataset_name) {
  if (!"thinc2" %in% names(dataset)) {
    message("Variable 'thinc2' not found in dataset: ", dataset_name)
    return(NULL)
  }
  
  # Extract wave suffix
  wave_suffix <- gsub(".*(w\\d+).*", "\\1", dataset_name)
  new_var_name <- paste0("thinc2_", wave_suffix)
  
  dataset <- dataset %>%
    select(mergeid, thinc2) %>%
    rename(!!new_var_name := thinc2)
  
  return(dataset)
}

# List of imputed datasets
dataset_names <- c(
  "sharew1_rel7.1.0_gv_imputations", "sharew2_rel7.1.0_gv_imputations", "sharew4_rel7.1.0_gv_imputations", 
  "sharew5_rel7.1.0_gv_imputations", "sharew6_rel7.1.0_gv_imputations", "sharew7_rel7.1.0_gv_imputations", 
  "sharew8_rel9.0.0_gv_imputations", "sharew9_rel9.0.0_gv_imputations"
)

# Extract "thinc2" for each dataset
data_list <- map(dataset_names, function(dataset_name) {
  if (exists(dataset_name)) {
    process_dataset(get(dataset_name), dataset_name)
  } else {
    message("Dataset not found: ", dataset_name)
    return(NULL)
  }
})

data_list <- compact(data_list) # Remove NULLs

# Merge all datasets by "mergeid"
thinc2_data <- reduce(data_list, ~ right_join(.x, .y, by = "mergeid"))

# Convert to tibble
thinc2_data <- as_tibble(thinc2_data)

# Save dataset
save(thinc2_data, file = "data_thinc2.Rdata")
