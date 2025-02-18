
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

# in the next step we transform into chf, adjust to monthly, and divide by equivalizing factor
glimpse(data$hh1itot)
glimpse(data$household.equiv.weighting.factor.w1)
glimpse(data$hh9)

#setting up w1 where the variables are slightly different
data <- data %>%
  mutate(
    yearly.thinc.eur.w1 = hh1itot,
    yearly.thinc.chf.w1 = if_else(
      is.na(hh1itot) | is.na(chf.eur.exrate_r1iwy),
      NA,
      hh1itot / chf.eur.exrate_r1iwy
    ),
    monthly.thinc.chf.w1 = if_else(is.na(yearly.thinc.chf.w1), NA, yearly.thinc.chf.w1 /
                                     12),
    equiv.hhinc.w1 = monthly.thinc.chf.w1 / household.equiv.weighting.factor.w1
  )
summary(data$equiv.hhinc.w1)


# then setting up waves 4-9
# note: i stumbled across a problem where the total houhosel income (hhWittot) is missing for w6-w9 for no reason (it exists in the regular waves)
# therefore i use the other one-shot household income measure hhWitothhinc
glimpse(data$hh2ittot)


# For w2
data <- data %>%
  mutate(
    yearly.thinc.eur.w2 = hh2itothhinc,
    yearly.thinc.chf.w2 = if_else(
      is.na(hh2itothhinc) | is.na(chf.eur.exrate_r2iwy),
      NA_real_,
      hh2itothhinc / chf.eur.exrate_r2iwy
    ),
    monthly.thinc.chf.w2 = if_else(is.na(yearly.thinc.chf.w2), NA_real_, yearly.thinc.chf.w2 / 12),
    equiv.hhinc.w2 = monthly.thinc.chf.w2 / household.equiv.weighting.factor.w2
  )
summary(data$equiv.hhinc.w4)


# For w4
data <- data %>%
  mutate(
    yearly.thinc.eur.w4 = hh4itothhinc,
    yearly.thinc.chf.w4 = if_else(
      is.na(hh4itothhinc) | is.na(chf.eur.exrate_r4iwy),
      NA_real_,
      hh4itothhinc / chf.eur.exrate_r4iwy
    ),
    monthly.thinc.chf.w4 = if_else(is.na(yearly.thinc.chf.w4), NA_real_, yearly.thinc.chf.w4 / 12),
    equiv.hhinc.w4 = monthly.thinc.chf.w4 / household.equiv.weighting.factor.w4
  )
summary(data$equiv.hhinc.w4)

# For w5
data <- data %>%
  mutate(
    yearly.thinc.eur.w5 = hh5itothhinc,
    yearly.thinc.chf.w5 = if_else(
      is.na(hh5itothhinc) | is.na(chf.eur.exrate_r5iwy),
      NA_real_,
      hh5itothhinc / chf.eur.exrate_r5iwy
    ),
    monthly.thinc.chf.w5 = if_else(is.na(yearly.thinc.chf.w5), NA_real_, yearly.thinc.chf.w5 / 12),
    equiv.hhinc.w5 = monthly.thinc.chf.w5 / household.equiv.weighting.factor.w5
  )
summary(data$equiv.hhinc.w5)

# For w6
data <- data %>%
  mutate(
    yearly.thinc.eur.w6 = hh6itothhinc,
    yearly.thinc.chf.w6 = if_else(
      is.na(hh6itothhinc) | is.na(chf.eur.exrate_r6iwy),
      NA_real_,
      hh6itothhinc / chf.eur.exrate_r6iwy
    ),
    monthly.thinc.chf.w6 = if_else(is.na(yearly.thinc.chf.w6), NA_real_, yearly.thinc.chf.w6 / 12),
    equiv.hhinc.w6 = monthly.thinc.chf.w6 / household.equiv.weighting.factor.w6
  )
summary(data$equiv.hhinc.w6)

# For w7
data <- data %>%
  mutate(
    yearly.thinc.eur.w7 = hh7itothhinc,
    yearly.thinc.chf.w7 = if_else(
      is.na(hh7itothhinc) | is.na(chf.eur.exrate_r7iwy),
      NA_real_,
      hh7itothhinc / chf.eur.exrate_r7iwy
    ),
    monthly.thinc.chf.w7 = if_else(is.na(yearly.thinc.chf.w7), NA_real_, yearly.thinc.chf.w7 / 12),
    equiv.hhinc.w7 = monthly.thinc.chf.w7 / household.equiv.weighting.factor.w7
  )
summary(data$equiv.hhinc.w7)

# For w8
data <- data %>%
  mutate(
    yearly.thinc.eur.w8 = hh8itothhinc,
    yearly.thinc.chf.w8 = if_else(
      is.na(hh8itothhinc) | is.na(chf.eur.exrate_r8iwy),
      NA_real_,
      hh8itothhinc / chf.eur.exrate_r8iwy
    ),
    monthly.thinc.chf.w8 = if_else(is.na(yearly.thinc.chf.w8), NA_real_, yearly.thinc.chf.w8 / 12),
    equiv.hhinc.w8 = monthly.thinc.chf.w8 / household.equiv.weighting.factor.w8
  )
summary(data$equiv.hhinc.w8)

# For w9
data <- data %>%
  mutate(
    yearly.thinc.eur.w9 = hh9itothhinc,
    yearly.thinc.chf.w9 = if_else(
      is.na(hh9itothhinc) | is.na(chf.eur.exrate_r9iwy),
      NA_real_,
      hh9itothhinc / chf.eur.exrate_r9iwy
    ),
    monthly.thinc.chf.w9 = if_else(is.na(yearly.thinc.chf.w9), NA_real_, yearly.thinc.chf.w9 / 12),
    equiv.hhinc.w9 = monthly.thinc.chf.w9 / household.equiv.weighting.factor.w9
  )
summary(data$equiv.hhinc.w9)

# at this point we have equivalized household incomes for each wave 

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


# save --------------------------------------------------------------------

save(data, file="data_step3-out-variables-all-setup.Rdata")

