
# Libraries ---------------------------------------------------------------

library(tidyverse)


# Clear everything  -------------------------------------------------------

rm(list = ls())

# Paths -------------------------------------------------------------------

#path to local folder where I run the analyses
path <- "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Poverty Trajectories SHARE/"

path.to.othershare <- "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Armut 13. AHV /Shared-Eliane/Data/RDATA/"


# Load the main data  ----------------------------------------------------------

load(file = paste0(path, "WORKING-DATA/data_step2-out_harmonized-Swiss.Rdata"))
load(file = paste0(path.to.othershare, "data_step2-out__wave9.imp.Rdata"))



#calculate the ages of interviewed individuals in each wave
data <- data %>%
  mutate(
    age.w1 = r1iwy - rabyear,
    age.w2 = r2iwy - rabyear,
    age.w4 = r4iwy - rabyear,
    age.w5 = r5iwy - rabyear,
    age.w6 = r6iwy - rabyear,
    age.w8 = r8iwy - rabyear,
    age.w9 = r9iwy - rabyear
  )




# calculate the equivalizing factor for each wave
data$hh1hhres # number of people in the household
# Calculate initial weighting factor
data$household.equiv.weighting.factor.w1 <- case_when(
  data$hh1hhres == 1 ~ 1,
  data$hh1hhres == 2 ~ 1 + (1 * 0.5),
  data$hh1hhres == 3 ~ 1 + (2 * 0.5),
  data$hh1hhres == 4 ~ 1 + (3 * 0.5),
  data$hh1hhres == 5 ~ 1 + (4 * 0.5),
  data$hh1hhres == 6 ~ 1 + (5 * 0.5),
  data$hh1hhres == 7 ~ 1 + (6 * 0.5),
  TRUE ~ NA_real_
)

#reminder: here i use the information on number of people in the household from the harmonized file, but the information on whether children live in the household that i gathered from each regular wave
# source("script_extracting-child-habitation-info-panel.R")
load(file = "data_number.children.inHH.Rdata")
#merge number of children
data <- left_join(data, number.children.living.inHH, by = "mergeid")


# Adjust for households with children
adjustments <- list(
  list(
    hh = 2,
    children = 1,
    weight = 1.3
  ),
  list(
    hh = 3,
    children = 1,
    weight = 1 + 0.5 + 0.3
  ),
  list(
    hh = 3,
    children = 2,
    weight = 1 + (2 * 0.3)
  ),
  list(
    hh = 4,
    children = 1,
    weight = 1 + (2 * 0.5) + 0.3
  ),
  list(
    hh = 4,
    children = 2,
    weight = 1 + 0.5 + (2 * 0.3)
  ),
  list(
    hh = 4,
    children = 3,
    weight = 1 + (3 * 0.3)
  ),
  list(
    hh = 5,
    children = 1,
    weight = 1 + (3 * 0.5) + 0.3
  ),
  list(
    hh = 5,
    children = 2,
    weight = 1 + (2 * 0.5) + (2 * 0.3)
  ),
  list(
    hh = 5,
    children = 3,
    weight = 1 + 0.5 + (3 * 0.3)
  ),
  list(
    hh = 6,
    children = 1,
    weight = 1 + (4 * 0.5) + 0.3
  ),
  list(
    hh = 6,
    children = 2,
    weight = 1 + (3 * 0.5) + (2 * 0.3)
  ),
  list(
    hh = 6,
    children = 3,
    weight = 1 + (2 * 0.5) + (3 * 0.3)
  ),
  list(
    hh = 7,
    children = 1,
    weight = 1 + (5 * 0.5) + 0.3
  ),
  list(
    hh = 7,
    children = 2,
    weight = 1 + (4 * 0.5) + (2 * 0.3)
  ),
  list(
    hh = 7,
    children = 3,
    weight = 1 + (3 * 0.5) + (3 * 0.3)
  )
)

for (adj in adjustments) {
  filter <- which(data$hh1hhres == adj$hh &
                    data$number.children.living.inHH.w1 == adj$children)
  data$household.equiv.weighting.factor.w1[filter] <- adj$weight
}

# To replicate for other waves
waves <- list(
  w2 = list(
    hh_res = "hh2hhres",
    children = "number.children.living.inHH.w2",
    weight = "household.equiv.weighting.factor.w2"
  ),
  w4 = list(
    hh_res = "hh4hhres",
    children = "number.children.living.inHH.w4",
    weight = "household.equiv.weighting.factor.w4"
  ),
  w5 = list(
    hh_res = "hh5hhres",
    children = "number.children.living.inHH.w5",
    weight = "household.equiv.weighting.factor.w5"
  ),
  w6 = list(
    hh_res = "hh6hhres",
    children = "number.children.living.inHH.w6",
    weight = "household.equiv.weighting.factor.w6"
  ),
  w7 = list(
    hh_res = "hh7hhres",
    children = "number.children.living.inHH.w7",
    weight = "household.equiv.weighting.factor.w7"
  ),
  w8 = list(
    hh_res = "hh8hhres",
    children = "number.children.living.inHH.w8",
    weight = "household.equiv.weighting.factor.w8"
  ),
  w9 = list(
    hh_res = "hh9hhres",
    children = "number.children.living.inHH.w9",
    weight = "household.equiv.weighting.factor.w9"
  )
)

for (wave in waves) {
  hh_res_col <- wave$hh_res
  children_col <- wave$children
  weight_col <- wave$weight
  
  # Initial weighting factors for the wave
  data[[weight_col]] <- case_when(
    data[[hh_res_col]] == 1 ~ 1,
    data[[hh_res_col]] == 2 ~ 1 + (1 * 0.5),
    data[[hh_res_col]] == 3 ~ 1 + (2 * 0.5),
    data[[hh_res_col]] == 4 ~ 1 + (3 * 0.5),
    data[[hh_res_col]] == 5 ~ 1 + (4 * 0.5),
    data[[hh_res_col]] == 6 ~ 1 + (5 * 0.5),
    data[[hh_res_col]] == 7 ~ 1 + (6 * 0.5),
    TRUE ~ NA_real_
  )
  
  # Adjustments for children
  for (adj in adjustments) {
    filter <- which(data[[hh_res_col]] == adj$hh &
                      data[[children_col]] == adj$children)
    data[[weight_col]][filter] <- adj$weight
  }
}


rm(number.children.living.inHH)



# calculate the equivalized income for each wave

# here we use the exchange rate gathered from the imputation files in each wave to transform the EUR values in the file to CHF
# source("data_chfexrates.Rdata", echo=TRUE)

load(file = "data_chfexrates.Rdata") # see the script "importing exchange rates" where I pull this from all GV files in each wave
#add a variable for the exchange rate

# List of variables to process
rXiwy_vars <- c("r1iwy",
                "r2iwy",
                "r4iwy",
                "r5iwy",
                "r6iwy",
                "r7iwy",
                "r8iwy",
                "r9iwy")

# Apply the matching logic in a tidy way
data <- data %>%
  mutate(across(all_of(rXiwy_vars), .fns = ~ {
    find <- match(as.character(.), as.character(chf.eur.exrates$year))
    as.numeric(chf.eur.exrates$nomx_value[find])
  }, .names = "chf.eur.exrate_{col}"))


# Load the dataset with wave-specific thinc2 income variables (thinc2 being the one-shot question for total household income)
load(file = "data_wave-specific-thinc2.Rdata") 

# Filter `thinc2_data` to only keep `mergeid` values that exist in `data`
thinc2_filtered <- thinc2_data %>%
  filter(mergeid %in% data$mergeid)

# Merge only the relevant `thinc2` variables with `data`
data <- data %>%
  left_join(thinc2_filtered, by = "mergeid")


# Load the dataset with wave-specific hnetw variables (hnet being the total net household wealth from the GV imputation in SHARE)
load(file = "data_wave-specific-hnetw.Rdata") 

# Filter `hnetw_data` to only keep `mergeid` values that exist in `data`
hnetw_filtered <- hnetw_data %>%
  filter(mergeid %in% data$mergeid)

# Merge only the relevant `hnetw` variables with `data`
data <- data %>%
  left_join(hnetw_filtered, by = "mergeid")


rm(hnetw_data, hnetw_filtered)


### transform wealth into CHF

# Apply transformations for each wave (w2 to w9)
data <- data %>%
  mutate(
    # Wave 2
    household.wealth.eur.w2 = w2.hnetw,
    household.wealth.chf.w2 = if_else(
      is.na(w2.hnetw) | is.na(chf.eur.exrate_r2iwy),
      NA_real_,
      w2.hnetw * chf.eur.exrate_r2iwy
    ),
    
    # Wave 4
    household.wealth.eur.w4 = w4.hnetw,
    household.wealth.chf.w4 = if_else(
      is.na(w4.hnetw) | is.na(chf.eur.exrate_r4iwy),
      NA_real_,
      w4.hnetw * chf.eur.exrate_r4iwy
    ),
    
    # Wave 5
    household.wealth.eur.w5 = w5.hnetw,
    household.wealth.chf.w5 = if_else(
      is.na(w5.hnetw) | is.na(chf.eur.exrate_r5iwy),
      NA_real_,
      w5.hnetw * chf.eur.exrate_r5iwy
    ),
    
    # Wave 6
    household.wealth.eur.w6 = w6.hnetw,
    household.wealth.chf.w6 = if_else(
      is.na(w6.hnetw) | is.na(chf.eur.exrate_r6iwy),
      NA_real_,
      w6.hnetw * chf.eur.exrate_r6iwy
    ),
    
    # Wave 7
    household.wealth.eur.w7 = w7.hnetw,
    household.wealth.chf.w7 = if_else(
      is.na(w7.hnetw) | is.na(chf.eur.exrate_r7iwy),
      NA_real_,
      w7.hnetw * chf.eur.exrate_r7iwy
    ),
    
    # Wave 8
    household.wealth.eur.w8 = w8.hnetw,
    household.wealth.chf.w8 = if_else(
      is.na(w8.hnetw) | is.na(chf.eur.exrate_r8iwy),
      NA_real_,
      w8.hnetw * chf.eur.exrate_r8iwy
    ),
    
    # Wave 9
    household.wealth.eur.w9 = w9.hnetw,
    household.wealth.chf.w9 = if_else(
      is.na(w9.hnetw) | is.na(chf.eur.exrate_r9iwy),
      NA_real_,
      w9.hnetw * chf.eur.exrate_r9iwy
    )
  )

# Display summaries
summary(data$household.wealth.chf.w2)
summary(data$household.wealth.chf.w3)
summary(data$household.wealth.chf.w4)
summary(data$household.wealth.chf.w5)
summary(data$household.wealth.chf.w6)
summary(data$household.wealth.chf.w7)
summary(data$household.wealth.chf.w8)
summary(data$household.wealth.chf.w9)


# in the next step we transform household income into chf, adjust to monthly, and divide by equivalizing factor

# Apply transformations for each wave (w2 to w9)
data <- data %>%
  mutate(
    # Wave 2
    yearly.thinc.eur.w2 = w2.thinc2,
    yearly.thinc.chf.w2 = if_else(
      is.na(w2.thinc2) | is.na(chf.eur.exrate_r2iwy),
      NA_real_,
      w2.thinc2 * chf.eur.exrate_r2iwy
    ),
    monthly.thinc.chf.w2 = yearly.thinc.chf.w2 / 12,
    equiv.hhinc.w2 = monthly.thinc.chf.w2 / household.equiv.weighting.factor.w2,
    
    # Wave 4
    yearly.thinc.eur.w4 = w4.thinc2,
    yearly.thinc.chf.w4 = if_else(
      is.na(w4.thinc2) | is.na(chf.eur.exrate_r4iwy),
      NA_real_,
      w4.thinc2 * chf.eur.exrate_r4iwy
    ),
    monthly.thinc.chf.w4 = yearly.thinc.chf.w4 / 12,
    equiv.hhinc.w4 = monthly.thinc.chf.w4 / household.equiv.weighting.factor.w4,
    
    # Wave 5
    yearly.thinc.eur.w5 = w5.thinc2,
    yearly.thinc.chf.w5 = if_else(
      is.na(w5.thinc2) | is.na(chf.eur.exrate_r5iwy),
      NA_real_,
      w5.thinc2 * chf.eur.exrate_r5iwy
    ),
    monthly.thinc.chf.w5 = yearly.thinc.chf.w5 / 12,
    equiv.hhinc.w5 = monthly.thinc.chf.w5 / household.equiv.weighting.factor.w5,
    
    # Wave 6
    yearly.thinc.eur.w6 = w6.thinc2,
    yearly.thinc.chf.w6 = if_else(
      is.na(w6.thinc2) | is.na(chf.eur.exrate_r6iwy),
      NA_real_,
      w6.thinc2 * chf.eur.exrate_r6iwy
    ),
    monthly.thinc.chf.w6 = yearly.thinc.chf.w6 / 12,
    equiv.hhinc.w6 = monthly.thinc.chf.w6 / household.equiv.weighting.factor.w6,
    
    # Wave 7
    yearly.thinc.eur.w7 = w7.thinc2,
    yearly.thinc.chf.w7 = if_else(
      is.na(w7.thinc2) | is.na(chf.eur.exrate_r7iwy),
      NA_real_,
      w7.thinc2 * chf.eur.exrate_r7iwy
    ),
    monthly.thinc.chf.w7 = yearly.thinc.chf.w7 / 12,
    equiv.hhinc.w7 = monthly.thinc.chf.w7 / household.equiv.weighting.factor.w7,
    
    # Wave 8
    yearly.thinc.eur.w8 = w8.thinc2,
    yearly.thinc.chf.w8 = if_else(
      is.na(w8.thinc2) | is.na(chf.eur.exrate_r8iwy),
      NA_real_,
      w8.thinc2 * chf.eur.exrate_r8iwy
    ),
    monthly.thinc.chf.w8 = yearly.thinc.chf.w8 / 12,
    equiv.hhinc.w8 = monthly.thinc.chf.w8 / household.equiv.weighting.factor.w8,
    
    # Wave 9
    yearly.thinc.eur.w9 = w9.thinc2,
    yearly.thinc.chf.w9 = if_else(
      is.na(w9.thinc2) | is.na(chf.eur.exrate_r9iwy),
      NA_real_,
      w9.thinc2 * chf.eur.exrate_r9iwy
    ),
    monthly.thinc.chf.w9 = yearly.thinc.chf.w9 / 12,
    equiv.hhinc.w9 = monthly.thinc.chf.w9 / household.equiv.weighting.factor.w9
  )

# Display summaries
summary(data$equiv.hhinc.w2)
summary(data$equiv.hhinc.w4)
summary(data$equiv.hhinc.w5)
summary(data$equiv.hhinc.w6)
summary(data$equiv.hhinc.w7)
summary(data$equiv.hhinc.w8)
summary(data$equiv.hhinc.w9)


# at this point we have equivalized monthly household incomes for each wave for each respondent

# now we transform the equivalized household income to poverty indicators 
# note, since the variable which i've now used as hhincome is not available for w1 and is not comparable, i do the sequences only for w2-9 ( skipping 3 because it is missing)

#here we have to load the retrospective table with the poverty thresholds that i got from BFS 
BFS_Poverty_Thresholds <- read_csv("~/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Poverty Trajectories SHARE/secondary data /BFS-Poverty_Thresholds_Einzelperson.csv")

#here i have the problem that the BFS series only goes back to 2007. I am therefore going to assume that the pvoerty threshold for 2006 was identical as 2007. 
BFS_Poverty_Thresholds <- BFS_Poverty_Thresholds %>%
  bind_rows(slice(BFS_Poverty_Thresholds, 16)) %>% # Add the 16th row to the data frame
  mutate(Year = as.character(Year)) %>%           # Convert the "Year" column to character
  mutate(Year = replace(Year, n(), "2006"))       # Replace the "Year" value in the last row with "2006"


# Create a named vector from BFS_Poverty_Thresholds for mapping
poverty_threshold_lookup <- setNames(BFS_Poverty_Thresholds$poverty.treshold, BFS_Poverty_Thresholds$Year)

# Loop through waves, skipping wave 3
waves <- c(1, 2, 4, 5, 6, 7, 8, 9)

# Iterate through each wave and create poverty threshold variables
for (wave in waves) {
  # Construct variable names
  year_var <- paste0("r", wave, "iwy")  # Interview year variable
  poverty_var <- paste0("poverty.threshold.w", wave)  # Poverty threshold variable
  
  # Ensure year variable exists in data
  if (!year_var %in% colnames(data)) {
    stop(paste("Year variable", year_var, "not found in dataset"))
  }
  
  # Map poverty threshold values based on year
  data[[poverty_var]] <- poverty_threshold_lookup[as.character(data[[year_var]])]
}

# now we create wave-specific poverty indicators 
data <- data %>%
  mutate(
    poverty.bn.w2 = ifelse(equiv.hhinc.w2 < poverty.threshold.w2, 1, 0),
    poverty.bn.w4 = ifelse(equiv.hhinc.w4 < poverty.threshold.w4, 1, 0),
    poverty.bn.w5 = ifelse(equiv.hhinc.w5 < poverty.threshold.w5, 1, 0),
    poverty.bn.w6 = ifelse(equiv.hhinc.w6 < poverty.threshold.w6, 1, 0),
    poverty.bn.w7 = ifelse(equiv.hhinc.w7 < poverty.threshold.w7, 1, 0),
    poverty.bn.w8 = ifelse(equiv.hhinc.w8 < poverty.threshold.w8, 1, 0),
    poverty.bn.w9 = ifelse(equiv.hhinc.w9 < poverty.threshold.w9, 1, 0)
  )


prop.table(table(data$poverty.bn.w6))
# 
# 
# # Extend BFS_Poverty_Thresholds to include wealth poverty threshold
# BFS_Poverty_Thresholds <- BFS_Poverty_Thresholds %>%
#   mutate(wealth.poverty.threshold = 12 * poverty.treshold)  # New variable

# Extend BFS_Poverty_Thresholds to include wealth poverty threshold
BFS_Poverty_Thresholds <- cbind(BFS_Poverty_Thresholds, rep(30000, times=17))
names(BFS_Poverty_Thresholds) <- c("Year", "poverty.treshold", "wealth.poverty.threshold")
BFS_Poverty_Thresholds

# Create lookup vector for wealth poverty thresholds
wealth_poverty_threshold_lookup <- setNames(BFS_Poverty_Thresholds$wealth.poverty.threshold, as.character(BFS_Poverty_Thresholds$Year))

# Loop through waves, skipping wave 3
waves <- c(1, 2, 4, 5, 6, 7, 8, 9)

# Iterate through each wave and create wealth poverty threshold variables
for (wave in waves) {
  year_var <- paste0("r", wave, "iwy")  # Interview year variable
  wealth_poverty_var <- paste0("wealth.poverty.threshold.w", wave)  # Wealth poverty threshold variable
  
  if (!year_var %in% colnames(data)) {
    warning(paste("Year variable", year_var, "not found in dataset"))
    next
  }
  
  # Assign wealth poverty threshold
  data[[wealth_poverty_var]] <- wealth_poverty_threshold_lookup[as.character(data[[year_var]])]
}

# Create wave-specific wealth poverty indicators
data <- data %>%
  mutate(
    wealth.poverty.bn.w2 = ifelse(household.wealth.chf.w2 < wealth.poverty.threshold.w2, 1, 0),
    wealth.poverty.bn.w4 = ifelse(household.wealth.chf.w4 < wealth.poverty.threshold.w4, 1, 0),
    wealth.poverty.bn.w5 = ifelse(household.wealth.chf.w5 < wealth.poverty.threshold.w5, 1, 0),
    wealth.poverty.bn.w6 = ifelse(household.wealth.chf.w6 < wealth.poverty.threshold.w6, 1, 0),
    wealth.poverty.bn.w7 = ifelse(household.wealth.chf.w7 < wealth.poverty.threshold.w7, 1, 0),
    wealth.poverty.bn.w8 = ifelse(household.wealth.chf.w8 < wealth.poverty.threshold.w8, 1, 0),
    wealth.poverty.bn.w9 = ifelse(household.wealth.chf.w9 < wealth.poverty.threshold.w9, 1, 0)
  )


#create a joint-income wealth poverty indicator

data <- data %>%
  mutate(
    joint.income.wealth.poverty.bn.w2 = case_when(
      poverty.bn.w2 == 1 & wealth.poverty.bn.w2 == 1 ~ "twice.poor",
      poverty.bn.w2 == 0 & wealth.poverty.bn.w2 == 1 ~ "not.poor.but.nowealth",
      poverty.bn.w2 == 1 & wealth.poverty.bn.w2 == 0 ~ "income.poor.but.wealth",
      poverty.bn.w2 == 0 & wealth.poverty.bn.w2 == 0 ~ "not.poor",
      TRUE ~ NA_character_
    ),
    joint.income.wealth.poverty.bn.w4 = case_when(
      poverty.bn.w4 == 1 & wealth.poverty.bn.w4 == 1 ~ "twice.poor",
      poverty.bn.w4 == 0 & wealth.poverty.bn.w4 == 1 ~ "not.poor.but.nowealth",
      poverty.bn.w4 == 1 & wealth.poverty.bn.w4 == 0 ~ "income.poor.but.wealth",
      poverty.bn.w4 == 0 & wealth.poverty.bn.w4 == 0 ~ "not.poor",
      TRUE ~ NA_character_
    ),
    joint.income.wealth.poverty.bn.w5 = case_when(
      poverty.bn.w5 == 1 & wealth.poverty.bn.w5 == 1 ~ "twice.poor",
      poverty.bn.w5 == 0 & wealth.poverty.bn.w5 == 1 ~ "not.poor.but.nowealth",
      poverty.bn.w5 == 1 & wealth.poverty.bn.w5 == 0 ~ "income.poor.but.wealth",
      poverty.bn.w5 == 0 & wealth.poverty.bn.w5 == 0 ~ "not.poor",
      TRUE ~ NA_character_
    ),
    joint.income.wealth.poverty.bn.w6 = case_when(
      poverty.bn.w6 == 1 & wealth.poverty.bn.w6 == 1 ~ "twice.poor",
      poverty.bn.w6 == 0 & wealth.poverty.bn.w6 == 1 ~ "not.poor.but.nowealth",
      poverty.bn.w6 == 1 & wealth.poverty.bn.w6 == 0 ~ "income.poor.but.wealth",
      poverty.bn.w6 == 0 & wealth.poverty.bn.w6 == 0 ~ "not.poor",
      TRUE ~ NA_character_
    ),
    joint.income.wealth.poverty.bn.w7 = case_when(
      poverty.bn.w7 == 1 & wealth.poverty.bn.w7 == 1 ~ "twice.poor",
      poverty.bn.w7 == 0 & wealth.poverty.bn.w7 == 1 ~ "not.poor.but.nowealth",
      poverty.bn.w7 == 1 & wealth.poverty.bn.w7 == 0 ~ "income.poor.but.wealth",
      poverty.bn.w7 == 0 & wealth.poverty.bn.w7 == 0 ~ "not.poor",
      TRUE ~ NA_character_
    ),
    joint.income.wealth.poverty.bn.w8 = case_when(
      poverty.bn.w8 == 1 & wealth.poverty.bn.w8 == 1 ~ "twice.poor",
      poverty.bn.w8 == 0 & wealth.poverty.bn.w8 == 1 ~ "not.poor.but.nowealth",
      poverty.bn.w8 == 1 & wealth.poverty.bn.w8 == 0 ~ "income.poor.but.wealth",
      poverty.bn.w8 == 0 & wealth.poverty.bn.w8 == 0 ~ "not.poor",
      TRUE ~ NA_character_
    ),
    joint.income.wealth.poverty.bn.w9 = case_when(
      poverty.bn.w9 == 1 & wealth.poverty.bn.w9 == 1 ~ "twice.poor",
      poverty.bn.w9 == 0 & wealth.poverty.bn.w9 == 1 ~ "not.poor.but.nowealth",
      poverty.bn.w9 == 1 & wealth.poverty.bn.w9 == 0 ~ "income.poor.but.wealth",
      poverty.bn.w9 == 0 & wealth.poverty.bn.w9 == 0 ~ "not.poor",
      TRUE ~ NA_character_
    )
  )

table(data$joint.income.wealth.poverty.bn.w6)
prop.table(table(data$joint.income.wealth.poverty.bn.w6))


#creating a binary variable for participation in wave effects 

# Create binary variables indicating valid information for each wave
# Define the waves included in the dataset
# Define the waves included in the dataset
waves <- c(2, 4, 5, 6, 7, 8, 9)

# Create binary variables indicating valid information for each wave
data <- data %>%
  mutate(across(
    all_of(paste0("joint.income.wealth.poverty.bn.w", waves)),
    ~ as.integer(!is.na(.)),
    .names = "valid.information.w{col}"
  ))


data$valid.information.wjoint.income.wealth.poverty.bn.w2

# setting up the covariates (from the g2a file)

#education 
levels(as.factor(data$raedisced))
data <- data %>% 
  mutate( edu.rcd =  as.factor(case_when(
    raedisced < 3  ~ "Low education",
    raedisced >= 3 & raedisced < 5 ~ "Apprenticeship", 
    raedisced > 4  ~ "Tertiary education"
  ))) 

summary(data$edu.rcd)
length(which(is.na(data$edu.rcd)))

#education years 
data <- data %>% mutate(eduyears = raedyrs)
data$eduyears

#gender
data <- data %>% 
  mutate( gender.rcd =  as.factor(case_when(
    ragender == 1  ~ "Male",
    ragender == 2  ~ "Female"))) 
table(data$gender.rcd)

#citizenship
table(data$racitizen)
data <- data %>% 
  mutate( is.swiss =  as.factor(case_when(
    racitizen == 0 ~ "Foreigner",
    racitizen == 1  ~ "Swiss"))) 
table(data$is.swiss)


# load isco work codings

load(file="data_isco-job-codings.Rdata")
isco_wide <- isco_wide %>% select(mergeid,highest_lifetime_ISCO_88_recoded ) 

data <- data %>%
  left_join(isco_wide, by = "mergeid")

data$highest_lifetime_ISCO_88_recoded <- relevel(as.factor(data$highest_lifetime_ISCO_88_recoded), ref="medium")

# calculate cohort 

table(data$rabyear)

data <- data %>%
  mutate(cohort = case_when(
    rabyear >= 1910 & rabyear < 1920 ~ "1910-1919",
    rabyear >= 1920 & rabyear < 1930 ~ "1920-1929",
    rabyear >= 1930 & rabyear < 1940 ~ "1930-1939",
    rabyear >= 1940 & rabyear < 1950 ~ "1940-1949",
    rabyear >= 1950 & rabyear < 1960 ~ "1950-1959",
    rabyear >= 1960 & rabyear < 1970 ~ "1960-1969",
    rabyear >= 1970 & rabyear < 1980 ~ "1970-1979",
    TRUE ~ NA_character_  # Assign NA if outside the range
  ))

load(file="data_experience-poverty-transition.Rdata")

data <- data %>%  left_join(indicator, by="mergeid")
data$experience.twice.poor.transition.bn
data$experience.income_poor_but_wealth.transition.bn



load(file="data_poverty.trajectories.clusters.Rdata")
poverty.trajectories.clusters
data <- data %>%  left_join(poverty.trajectories.clusters, by="mergeid")
data$poverty.trajectories.clusters

clusters <- levels(as.factor(data$poverty.trajectories.clusters))
library(dplyr)

# Define a function to clean and format names safely without regex
clean_name <- function(name) {
  name <- gsub(" ", "_", name, fixed = TRUE)
  name <- gsub("/", "_", name, fixed = TRUE)
  name <- gsub("-", "_", name, fixed = TRUE)
  return(name)
}

# Ensure poverty.trajectories.clusters is a factor
if (!is.factor(data$poverty.trajectories.clusters)) {
  data <- data %>% mutate(poverty.trajectories.clusters = as.factor(poverty.trajectories.clusters))
}

# Extract unique levels from the factor variable
cluster_levels <- levels(data$poverty.trajectories.clusters)

# Create binary indicators and store them as new columns in a dataframe
binary_df <- as.data.frame(sapply(cluster_levels, function(cluster) {
  as.integer(data$poverty.trajectories.clusters == cluster)
}))

# Rename the columns appropriately
colnames(binary_df) <- paste0("POV.CLUST.", sapply(cluster_levels, clean_name))

# Bind new binary variables to the original dataframe
data <- bind_cols(data, binary_df)

data$POV.CLUST.Mainly_economically_vulnerable



# save --------------------------------------------------------------------

save(data, file="data_step3-out-variables-all-setup.Rdata")

