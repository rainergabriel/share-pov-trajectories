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
    age.w8 = r8iwy - rabyear,
    age.w9 = r9iwy - rabyear, 
    age.end.observation = 2022 - rabyear
  )

table(data$age.end.observation)

# Transform data to long format

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

head(sts)

events
alphabet=c( "twice.poor"  , "income.poor.but.wealth" ,"not.poor.but.nowealth"  ,"not.poor" ,  "not.observed", "missing")
states=c( "twice.poor"  , "income.poor.but.wealth" ,"not.poor.but.nowealth"  ,"not.poor" ,  "not.observed", "missing")
labels= c("Twice poor", "Protected poor", "Economically vulnerable",  "Non-poor",      "Not observed", "Missing")
         
      
       


# tra.seq <- seqdef(sts[,36:70], informat="STS", states = states, alphabet = alphabet, start= 50  ) 
tra.seq <- seqdef(sts[,36:51], informat="STS", states = states, alphabet = alphabet, start= 50 , labels=labels ) 

summary(tra.seq)

seqdplot(tra.seq)

# subsetting useless observations -----------------------------------------

save(tra.seq, file="data_step4-out_trajectories-STS.Rdata")

# subsetting --------------------------------------------------------------

pov.seq <- tra.seq 

results <- seqistatd(pov.seq)
results <- as.data.frame(results)
names(results)

#remove those who are always missing
filter <- which(results$missing==16)
length(filter)
pov.seq <- pov.seq[-filter,]

#remove those who are never observed
filter <- which(results$not.observed==16)
length(filter)
pov.seq <- pov.seq[-filter,]

#remove those who are never poor
filter <- which(results$not.poor==16)
length(filter)
pov.seq <- pov.seq[-filter,]

filter <- which(results$not.poor==0 
                & results$income.poor.but.wealth == 0 
                & results$not.poor.but.nowealth == 0 
                & results$twice.poor == 0 
                  )
length(filter)
pov.seq <- pov.seq[-filter,]

save(pov.seq, file="data_step4-out_pov-STS.Rdata")
