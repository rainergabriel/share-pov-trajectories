

# Libraries ---------------------------------------------------------------
# 
# install.packages("TraMineR")
# install.packages("TraMineRextras")

library(tidyverse)
library(TraMineR)
library(TraMineRextras)
library(WeightedCluster)
library(RColorBrewer)
library(stargazer)



# Load required libraries
library(tidyverse)





# Clear everything  -------------------------------------------------------

rm(list = ls())
load(file="data_step4-out_pov-STS.Rdata")
load(file="data_step4-out_trajectories-STS.Rdata")
load(file="data_covariates-pov-sts.Rdata")
load( file="data_covariates-tra-sts.Rdata")
load(file="data_step3-out-variables-all-setup.Rdata")






# manually check sequences for transitions --------------------------------





load(file="data_step3-out-variables-all-setup.Rdata")

# creating variables as a basis to create a TSE frame ---------------------


#calculate an 
data <- data %>%
  mutate(
    age.w1 = r1iwy - rabyear,
    age.w2 = r2iwy - rabyear,
    age.w4 = r4iwy - rabyear,
    age.w5 = r5iwy - rabyear,
    age.w6 = r6iwy - rabyear,
    age.w8 = r8iwy - rabyear,
    age.w9 = r9iwy - rabyear, 
    age.end.observation = 2022 - rabyear
  )

table(data$age.end.observation)

# Transform data to long format


# Transform age variables to long format
age_data <- data %>%
  dplyr::select(mergeid, starts_with("age.w")) %>%
  pivot_longer(
    cols = -mergeid,
    names_to = "wave",
    names_prefix = "age.w",
    values_to = "age"
  ) %>%
  mutate(wave = as.integer(wave))

# Transform poverty variables to long format
poverty_data <- data %>%
  select(mergeid, starts_with("joint.income.wealth.poverty.bn.w")) %>%
  pivot_longer(
    cols = -mergeid,
    names_to = "wave",
    names_prefix = "joint.income.wealth.poverty.bn.w",
    values_to = "poverty"
  ) %>%
  mutate(wave = as.integer(wave))

# Merge age and poverty data

tse_data <- left_join(age_data, poverty_data, by = c("mergeid", "wave"))

tse_data <- tse_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "missing", .)))

levels(as.factor(tse_data$poverty))
length(levels(as.factor(tse_data$poverty)))
# transition matrix

events <- levels(as.factor(tse_data$poverty))
events
dm <- matrix(TRUE, 5,5, dimnames=list(events, events))
dm
dm[1, ] <- c(F, T, T, T, T)
dm[2, ] <- c(T, F, T, T, T)
dm[3, ] <- c(T, T, F, T, T)
dm[4, ] <- c(T, T, T, F, T)
dm[5, ] <- c(T, T, T, T, F)
print(dm)
stm2 <- seqe2stm(events, dropMatrix=dm)


tse_data <- tse_data %>% filter(age!="missing")

tse_data$age <- as.numeric(tse_data$age)

tse_data <- as.data.frame(tse_data)

sts <- TSE_to_STS(seqdata = tse_data, id = "mergeid", timestamp = "age", event = "poverty", stm = stm2, tmin = 30, tmax=100)

head(sts)

levels(as.factor(sts$a70))

process_observations <- function(sts, data, mergeid_col = "mergeid", age_end_col = "age.end.observation") {
  # Ensure row names in sts match the identifiers in `data`
  if (!all(row.names(sts) %in% data[[mergeid_col]])) {
    stop("Row names in sts must match the identifiers in data.")
  }
  
  # Loop through each row (individual) in sts
  for (id in row.names(sts)) {
    # Find the corresponding observation end age for the individual
    obs_end_age <- data[data[[mergeid_col]] == id, age_end_col]
    
    # Check if observation end age is valid
    if (length(obs_end_age) != 1 || is.na(obs_end_age)) {
      stop(paste("Invalid or missing observation end age for ID:", id))
    }
    
    # Identify columns beyond the observation end age
    age_columns <- as.numeric(gsub("a", "", colnames(sts))) # Extract age numbers from column names
    columns_to_replace <- which(age_columns > obs_end_age)
    
    # Replace values in those columns with "obs.end"
    if (length(columns_to_replace) > 0) {
      sts[id, columns_to_replace] <- "not.observed"
    }
  }
  
  return(sts)
}

sts <- process_observations(sts, data)
print(sts)
head(sts)

sts[sts == "None"] <- "not.observed"

levels(as.factor(sts$a70))




# Step 1: Convert Wide Data to Long Format
long_data <- sts %>%
  rownames_to_column(var = "mergeid") %>%  # Convert row names to a column
  pivot_longer(cols = starts_with("a"),   # Select all state columns
               names_to = "age", 
               values_to = "state") %>%
  mutate(age = as.numeric(str_replace(age, "a", "")))  # Extract numeric age (e.g., 'a70' → 70)






# Step 2: Keep only relevant financial states
valid_states <- c("not.poor", "not.poor.but.nowealth", "income.poor.but.wealth", "twice.poor")

long_data <- long_data %>%
  filter(state %in% valid_states) %>%
  mutate(state = factor(state, levels = valid_states, ordered = TRUE))  # Set state as an ordered factor

# Step 3: Create Previous State Variable (Lagged)
long_data <- long_data %>%
  arrange(mergeid, age) %>%
  group_by(mergeid) %>%
  mutate(state_previous = lag(state),  # Get previous state
         age_previous = lag(age)) %>%  # Get previous age
  ungroup()

# Step 4: Filter out transitions involving missing/not.observed states
long_data <- long_data %>%
  filter(!is.na(state_previous))  # Ensure only valid previous states


# subset ages outside of the ages of interest

long_data <- long_data %>% subset(age >= 65 & age <= 80)
summary(long_data$age)

names(long_data)
levels(as.factor(long_data$state))
levels(as.factor(long_data$state_previous))



# indicators --------------------------------------------------------------


transition.data <- long_data %>%
  mutate(
    experience.not_poor.transition.bn = ifelse(state == "not.poor" & state_previous != "not.poor", 1, 0),
    experience.not_poor_but_nowealth.transition.bn = ifelse(state == "not.poor.but.nowealth" & state_previous != "not.poor.but.nowealth", 1, 0),
    experience.income_poor_but_wealth.transition.bn = ifelse(state == "income.poor.but.wealth" & state_previous != "income.poor.but.wealth", 1, 0),
    experience.twice_poor.transition.bn = ifelse(state == "twice.poor" & state_previous != "twice.poor", 1, 0)
  )

transition.data <- left_join(transition.data, data, by="mergeid")


head(transition.data)
# save --------------------------------------------------------------------

save(transition.data, file="data_transition-data.Rdata")

