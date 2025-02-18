


library(tidyverse) 

rm(list=ls())

directory_path <- "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Poverty Trajectories SHARE/SHARE-Raw-Data/"

# Load all CSV files into a list of data frames
file_list <- list.files(
  path = directory_path,
  pattern = "_ch.*\\.sav$",
  full.names = TRUE,
  recursive = TRUE
)

# Dynamically load each .sav file into the environment
for (file in file_list) {
  # Generate a valid variable name based on the file name
  dataset_name <- make.names(gsub("\\.sav$", "", basename(file)))
  
  # Load the .sav file into a data frame and assign it to the variable
  assign(dataset_name, haven::read_sav(file))
  
  # Print confirmation
  cat("Loaded:", dataset_name, "\n")
}

#remove imputation files in which i'm not interested 
all_objects <- ls()
# Step 2: Filter object names that contain "_gv_"
gv_objects <- grep("_gv_", all_objects, value = TRUE)
rm(list=gv_objects)


# extract values ----------------------------------------------------------

process_dataset <- function(dataset) {
  # Identify variables related to children's living situation
  child_vars <- paste0("ch007_", 1:5)
  
  # Check which variables exist in the dataset
  existing_vars <- intersect(child_vars, names(dataset))
  
  # Debug: Show identified variables
  cat("Processing dataset...\n")
  cat("Found the following child variables:", paste(existing_vars, collapse = ", "), "\n")
  
  if (length(existing_vars) == 0) {
    message("No child-related variables found in dataset.")
    return(dataset)  # Return the dataset unmodified
  }
  
  # Create dummy variables for value 1 (In the same household)
  for (var in existing_vars) {
    dummy_var <- paste0(var, ".inhh")
    
    # Debug: Print raw values of the variable
    cat("Raw values of", var, ":\n")
    print(head(dataset[[var]]))
    
    # Convert to numeric
    dataset[[var]] <- as.numeric(as.character(dataset[[var]]))
    
    # Debug: Check conversion results
    cat("Converted values of", var, "to numeric:\n")
    print(head(dataset[[var]]))
    
    # Create dummy: 1 if the value is 1, otherwise 0
    dataset[[dummy_var]] <- ifelse(dataset[[var]] == 1, 1, 0)
    
    # Debug: Check the dummy variable creation
    cat("Dummy variable", dummy_var, "values:\n")
    print(head(dataset[[dummy_var]]))
    
    # Debug: Summarize the dummy variable
    cat("Summary of", dummy_var, ":\n")
    print(table(dataset[[dummy_var]]))
  }
  
  # Calculate total number of children living in the household
  inhh_vars <- paste0(existing_vars, ".inhh")
  dataset$number.children.living.inHH <- rowSums(dataset[inhh_vars], na.rm = TRUE)
  
  # Debug: Show first few values of the new total column
  cat("First few values of total number.children.living.inHH:\n")
  print(head(dataset$number.children.living.inHH))
  
  return(dataset)
}

# List of datasets
dataset_names <- c(
  "sharew1_rel7.1.0_ch", "sharew2_rel7.1.0_ch", "sharew4_rel7.1.0_ch", 
  "sharew5_rel7.1.0_ch", "sharew6_rel7.1.0_ch", "sharew7_rel7.1.0_ch", 
  "sharew8_rel9.0.0_ch", "sharew9_rel9.0.0_ch"
)

# Apply the processing function to each dataset
for (dataset_name in dataset_names) {
  cat("\nProcessing dataset:", dataset_name, "\n")
  
  # Retrieve the dataset
  dataset <- get(dataset_name)
  
  # Process it
  processed_dataset <- process_dataset(dataset)
  
  # Save the processed dataset back to the environment
  assign(dataset_name, processed_dataset)
  
  cat("Successfully processed dataset:", dataset_name, "\n")
}


# check -------------------------------------------------------------------

table(sharew2_rel7.1.0_ch$number.children.living.inHH)


# List of datasets
dataset_names <- c(
  "sharew1_rel7.1.0_ch", "sharew2_rel7.1.0_ch", "sharew4_rel7.1.0_ch", 
  "sharew5_rel7.1.0_ch", "sharew6_rel7.1.0_ch", "sharew7_rel7.1.0_ch", 
  "sharew8_rel9.0.0_ch", "sharew9_rel9.0.0_ch"
)

sharew1_rel7.1.0_ch$number.children.living.inHH.w1
# Initialize an empty list to store dataframes for merging later
merge_list <- list()

# Process each dataset
for (dataset_name in dataset_names) {
  cat("\nProcessing dataset:", dataset_name, "\n")
  
  # Retrieve the dataset
  dataset <- get(dataset_name)
  
  # Extract the wave number from the dataset name (e.g., "w1" from "sharew1_rel7.1.0_ch")
  wave_suffix <- gsub(".*(w\\d+).*", "\\1", dataset_name)
  
  # Process the dataset to calculate the number of children living in the household
  processed_dataset <- process_dataset(dataset)
  
  # Add or rename the variable with the wave suffix
  new_var_name <- paste0("number.children.living.inHH.", wave_suffix)
  processed_dataset[[new_var_name]] <- processed_dataset$number.children.living.inHH
  
  # Add the dataset back to the environment with the new variable
  assign(dataset_name, processed_dataset)
  
  # Extract the mergeid and the new variable for the combined dataframe
  merge_list[[dataset_name]] <- processed_dataset[, c("mergeid", new_var_name)]
  
  cat("Successfully processed and updated dataset:", dataset_name, "\n")
}

dataset_names <- c(
  "sharew1_rel7.1.0_ch", "sharew2_rel7.1.0_ch", "sharew4_rel7.1.0_ch", 
  "sharew5_rel7.1.0_ch", "sharew6_rel7.1.0_ch", "sharew7_rel7.1.0_ch", 
  "sharew8_rel9.0.0_ch", "sharew9_rel9.0.0_ch"
)

# Extract the relevant variables from each dataset
data_list <- map(dataset_names, function(dataset_name) {
  # Retrieve the dataset
  dataset <- get(dataset_name)
  
  # Extract wave suffix (e.g., "w1")
  wave_suffix <- gsub(".*(w\\d+).*", "\\1", dataset_name)
  new_var_name <- paste0("number.children.living.inHH.", wave_suffix)
  
  # Select only mergeid and the relevant variable
  dataset %>%
    select(mergeid, !!sym(new_var_name))
})

# Combine all datasets using right_join iteratively
number.children.living.inHH <- reduce(data_list, ~ right_join(.x, .y, by = "mergeid"))

# View the final dataframe
head(number.children.living.inHH)

number.children.living.inHH <- as_tibble(number.children.living.inHH)

# save  -------------------------------------------------------------------

save(number.children.living.inHH, file="data_number.children.inHH.Rdata")
