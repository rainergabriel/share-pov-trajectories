# Libraries ---------------------------------------------------------------
# 
# install.packages("TraMineR")
# install.packages("TraMineRextras")

library(tidyverse)
library(TraMineR)
library(TraMineRextras)


# Clear everything  -------------------------------------------------------

rm(list = ls())
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
    age.w7 = r7iwy - rabyear,
    age.w8 = r8iwy - rabyear,
    age.w9 = r9iwy - rabyear, 
    age.end.observation = 2022 - rabyear
  )

table(data$age.end.observation)

tse_data <- data %>%
  pivot_longer(
    cols = c("joint.income.wealth.poverty.bn.wpoverty.bn.w2",
             "joint.income.wealth.poverty.bn.wpoverty.bn.w4",
             "joint.income.wealth.poverty.bn.wpoverty.bn.w5",
             "joint.income.wealth.poverty.bn.wpoverty.bn.w6",
             "joint.income.wealth.poverty.bn.wpoverty.bn.w7",
             "joint.income.wealth.poverty.bn.wpoverty.bn.w8",
             "joint.income.wealth.poverty.bn.wpoverty.bn.w9",
             "age.w2", "age.w4", "age.w5", "age.w6", "age.w7", "age.w8", "age.w9"),
    names_to = c(".value", "wave"),
    names_pattern = "(.*)\\.w([2-9])"
  ) %>%
  mutate(
    wave = as.integer(wave) # Convert wave to numeric for proper ordering
  ) %>%
  select(mergeid, wave, poverty = joint.income.wealth.poverty.bn.wpoverty.bn, age) # Keep ID, wave, and relevant variables


# Preview the resulting TSE-format data
tse_data

tse_data <- tse_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "missing", .)))


# transition matrix
events <- levels(as.factor(tse_data$poverty))
events
dm <- matrix(TRUE, 3,3, dimnames=list(events, events))
dm
dm[1, ] <- c(F, T, T)
dm[2, ] <- c(T, F, T)
dm[3, ] <- c(T, T, F)
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


head(sts)

alphabet=c("0", "1", "missing", "not.observed")
states=c("Not poor", "Poor", "Missing Info", "Not observed")

# pov.seq <- seqdef(sts[,21:71], informat="STS", states = states, alphabet = alphabet, start= 50 ) # this imports sequence from 50
tra.seq <- seqdef(sts[,36:70], informat="STS", states = states, alphabet = alphabet, start= 50  ) 



# pov.seq <- seqdef(sts[,21:71], informat="STS", states = states, alphabet = alphabet, start= 50 ) # this imports sequence from 50
tra.seq <- seqdef(sts[,36:51], informat="STS", states = states, alphabet = alphabet, start= 50  ) 

seqdplot(tra.seq)

# subsetting useless observations -----------------------------------------

save(tra.seq, file="data_step4-out_trajectories-STS.Rdata")

# subsetting --------------------------------------------------------------

pov.seq <- tra.seq 

results <- seqistatd(pov.seq)
results <- as.data.frame(results)
names(results)


# filter <- which(results$`Not observed`==20
#                 )
# length(filter)
# pov.seq <- pov.seq[-filter,]


results <- seqistatd(pov.seq)
results <- as.data.frame(results)
names(results)

filter <- which(results$Poor==0)
length(filter)
pov.seq <- pov.seq[-filter,]


save(pov.seq, file="data_step4-out_pov-STS.Rdata")
