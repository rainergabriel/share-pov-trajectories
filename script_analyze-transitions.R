

# Libraries ---------------------------------------------------------------
# 
# install.packages("TraMineR")
# install.packages("TraMineRextras")

library(tidyverse)
library(TraMineR)
library(TraMineRextras)
library(WeightedCluster)
library(RColorBrewer)




# Clear everything  -------------------------------------------------------

rm(list = ls())
load(file="data_step4-out_pov-STS.Rdata")
load(file="data_step4-out_trajectories-STS.Rdata")
load(file="data_covariates-pov-sts.Rdata")
load( file="data_covariates-tra-sts.Rdata")
load(file="data_step3-out-variables-all-setup.Rdata")


# Transition matrix -------------------------------------------------------

focus <- alphabet(pov.seq)[1:4]

trans.matrix <- seqtrate(tra.seq, sel.states=focus)

table <- print(trans.matrix)

table

write.csv(table, file="table_transition-matrix.csv")

# event sequence ----------------------------------------------------------

# Create event sequence object
seqe_data <- seqecreate(tra.seq)

t <- as_tibble(seqe_data)

summary(seqe_data)

# Extract all sequences containing "non-poor → twice poor"
specific_transitions <- seqefsub(seqe_data, c("Non-poor>Twice poor"))

specific_transitions$eseq

# Print sequences that contain the transition
print(specific_transitions)

seqIplot(tra.seq, group = (previous_state == "non-poor" & final_state == "twice poor"))




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
  select(mergeid, starts_with("age.w")) %>%
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

detect_twice_poor_transition <- function(sts) {
  age_vars <- grep("^a[3-9][0-9]$|^a100$", names(sts), value = TRUE)  # Select age variables (a30 to a100)
  sts$experience.twice.poor.transition.bn <- 0  # Initialize new variable with 0
  
  for (i in seq_along(age_vars)[-1]) {  # Start from second age variable to check transitions
    prev_age <- age_vars[i - 1]
    curr_age <- age_vars[i]
    
    # Identify individuals transitioning into "twice.poor" from specified states
    transition_occurs <- sts[[curr_age]] == "twice.poor" & 
      sts[[prev_age]] %in% c("income.poor.but.wealth", "not.poor.but.nowealth", "not.poor")
    
    # Assign 1 if any transition to "twice.poor" is observed for an individual
    sts$experience.twice.poor.transition.bn <- sts$experience.twice.poor.transition.bn | transition_occurs
  }
  
  sts$experience.twice.poor.transition.bn <- as.integer(sts$experience.twice.poor.transition.bn)  # Convert logical to integer
  return(sts)
}

sts <- detect_twice_poor_transition(sts)

indicator <- cbind(rownames(sts), sts$experience.twice.poor.transition.bn)
indicator
indicator <- as_tibble(indicator)
names(indicator) <- c("mergeid", "experience.twice.poor.transition.bn")

data <- data %>%  left_join(indicator, by="mergeid")
data$experience.twice.poor.transition.bn

data$gender.rcd
glm(experience.twice.poor.transition.bn ~ gender.rcd, family= binomial, data=data )

