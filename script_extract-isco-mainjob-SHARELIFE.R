
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(tidyr)

# clear everything --------------------------------------------------------

rm(list=ls())




#path to local folder where I run the analyses
path <- "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Poverty Trajectories SHARE/"


# load data from the g2aging file which contains people from switz --------

load(file = paste0(path, "WORKING-DATA/data_step2-out_harmonized-Swiss.Rdata"))
library(dplyr)
library(tidyr)

# Define the recoding function based on OECD classification
recode_ISCO88 <- function(isco) {
  case_when(
    isco %in% c(1, 2, 3) ~ "high",
    isco %in% c(4, 5, 6, 7, 8) ~ "medium",
    isco %in% c(9) ~ "low",
    TRUE ~ NA_character_
  )
}

# Extract and recode ISCO-88 variables
isco_vars <- paste0("r", c(2, 4:9), "jisco")
existing_vars <- intersect(isco_vars, names(data))

isco_data <- data %>%
  select(mergeid, all_of(existing_vars)) %>%
  pivot_longer(cols = existing_vars, names_to = "wave", values_to = "ISCO_88") %>%
  mutate(
    wave = as.integer(gsub("r(\\d+)jisco", "\\1", wave)),
    ISCO_88_recoded = recode_ISCO88(ISCO_88)
  )

# Keep original and recoded versions
isco_wide <- isco_data %>%
  pivot_wider(names_from = wave, values_from = c(ISCO_88, ISCO_88_recoded), names_sep = "_")

# Determine highest lifetime ISCO-88 classification
isco_wide <- isco_wide %>%
  rowwise() %>%
  mutate(highest_lifetime_ISCO_88_recoded = max(c_across(matches("ISCO_88_recoded_\\d+")), na.rm = TRUE)) %>%
  ungroup()


# correcting for those who never worked  ----------------------------------


# Extract work variables
work_vars <- paste0("r", c(2, 4:9), "work")
existing_work_vars <- intersect(work_vars, names(data))

work_data <- data %>%
  select(mergeid, all_of(existing_work_vars)) %>%
  pivot_longer(cols = existing_work_vars, names_to = "wave", values_to = "work_status") %>%
  mutate(wave = as.integer(gsub("r(\\d+)work", "\\1", wave)))

work_wide <- work_data %>%
  pivot_wider(names_from = wave, values_from = work_status, names_prefix = "work_status_")

# Create 'never.worked' variable
work_wide <- work_wide %>%
  mutate(never_worked = ifelse(rowSums(across(starts_with("work_status_")) == 0, na.rm = TRUE) == length(existing_work_vars), 1, 0))

# Assign "low" ISCO-88 if highest_lifetime_ISCO_88_recoded is NA and never worked
isco_wide <- isco_wide %>%
  left_join(select(work_wide, mergeid, never_worked), by = "mergeid") %>%
  mutate(highest_lifetime_ISCO_88_recoded = ifelse(is.na(highest_lifetime_ISCO_88_recoded) & never_worked == 1, "low", highest_lifetime_ISCO_88_recoded))

length(which(is.na(isco_wide$highest_lifetime_ISCO_88_recoded)))

save(isco_wide, file="data_isco-job-codings.Rdata")
