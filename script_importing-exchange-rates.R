library(tidyverse) 

rm(list=ls())

directory_path <- "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Poverty Trajectories SHARE/SHARE-Raw-Data/"

# Load all CSV files into a list of data frames
file_list <- list.files(
  path = directory_path,
  pattern = "imputation.*\\.sav$",
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

sharew1_rel7.1.0_gv_imputations$nomx2003
sharew1_rel7.1.0_gv_imputations$country==20



# extracting values -------------------------------------------------------

# Initialize a unified data frame to store results
unified_table <- data.frame(
  year = integer(),
  nomx_value = numeric(),
  stringsAsFactors = FALSE
)

# Define the regular expression to match "nomx" variables with years (e.g., nomx2004)
nomx_pattern <- "^nomx(200[4-9]|201[0-9]|202[0-2])$"

# Loop through all datasets
for (file in file_list) {
  # Generate the dataset name based on the file name
  dataset_name <- make.names(gsub("\\.sav$", "", basename(file)))
  
  # Check if the dataset exists in the environment
  if (exists(dataset_name)) {
    # Access the dataset
    dataset <- get(dataset_name)
    
    # Filter for the first observation where country == 20
    swiss_data <- dataset[dataset$country == 20, ]
    
    if (nrow(swiss_data) > 0) {
      # Use only the first matching observation
      swiss_data <- swiss_data[1, , drop = FALSE]
      
      # Find variables matching the "nomxYYYY" pattern
      matching_vars <- names(dataset)[grepl(nomx_pattern, names(dataset))]
      
      if (length(matching_vars) > 0) {
        for (var in matching_vars) {
          # Extract the year from the variable name
          year <- as.numeric(gsub("nomx", "", var))
          
          # Extract the value (only one, as we are using the first observation)
          value <- swiss_data[[var]]
          
          # Add the year and value to the unified table
          unified_table <- rbind(
            unified_table,
            data.frame(
              year = year,
              nomx_value = value
            )
          )
        }
      } else {
        cat("No matching 'nomxYYYY' variables found in", dataset_name, "\n")
      }
    } else {
      cat("No observations for country == 20 in", dataset_name, "\n")
    }
  }
}

# Display the unified table
print(unified_table)

chf.eur.exrates <- as_tibble(unified_table)

save(chf.eur.exrates, file="data_chfexrates.Rdata")
