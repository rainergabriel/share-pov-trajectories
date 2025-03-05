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

sts[sts == "None"] <- NA
sts[sts == "missing"] <- NA
sts[sts == "not.observed"] <- NA

head(sts)

events
alphabet=c( "twice.poor"  , "income.poor.but.wealth" ,"not.poor.but.nowealth"  ,"not.poor" )
states=c( "twice.poor"  , "income.poor.but.wealth" ,"not.poor.but.nowealth"  ,"not.poor" )
labels= c("Twice poor", "Protected poor", "Economically vulnerable",  "Non-poor")





# tra.seq <- seqdef(sts[,36:70], informat="STS", states = states, alphabet = alphabet, start= 50  ) 
tra.seq <- seqdef(sts[,36:51], informat="STS", states = states, alphabet = alphabet, start= 50 , labels=labels ) 

summary(tra.seq)

# all sequences -----------------------------------------------------------


measures <- seqindic(tra.seq, indic=c("turb","entr"))
ids <- rownames(measures)
measures <- cbind(ids, measures)
measures <- as_tibble(measures)
names(measures) <- c("mergeid", "entropy", "turbulence")

data <- left_join(data, measures, by="mergeid" )
head(data$turbulence)



# regression models  ------------------------------------------------------



# Modellieren Part 1 -------------------------------------------------------------


# --- entropy ---
m1_vuln <- lm(entropy ~ eduyears,  data = data)
m2_vuln <- glm(entropy ~ highest_lifetime_ISCO_88_recoded,  data = data)
m3_vuln <- glm(entropy ~ eduyears + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9,  data = data)
m4_vuln <- glm(entropy ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9,  data = data)


# Stargazer Table
stargazer(
  m1_vuln, m2_vuln, m3_vuln, m4_vuln,
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Entropy Index", 
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# --- entropy ---
m1_vuln <- lm(entropy ~ eduyears,  data = data)
m2_vuln <- glm(entropy ~ highest_lifetime_ISCO_88_recoded,  data = data)
m3_vuln <- glm(entropy ~ eduyears + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9,  data = data)
m4_vuln <- glm(entropy ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9,  data = data)


# Stargazer Table
stargazer(
  m1_vuln, m2_vuln, m3_vuln, m4_vuln,
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Entropy Index", 
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

library(stargazer)

# --- entropy ---

median(data$entropy, na.rm=TRUE)

sd(data$entropy, na.rm=TRUE)

max(data$entropy, na.rm=TRUE)


m1_vuln <- glm(entropy ~ eduyears,  data = data)
m2_vuln <- glm(entropy ~ highest_lifetime_ISCO_88_recoded,  data = data)
m3_vuln <- glm(entropy ~ eduyears + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9,  data = data)
m4_vuln <- glm(entropy ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9,  data = data)



stargazer(
  m1_vuln, m2_vuln, m3_vuln, m4_vuln,
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly economically vulnerable", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# --- turbulence ---
m1_npno <- glm(turbulence ~ eduyears,  data = data)
m2_npno <- glm(turbulence ~ highest_lifetime_ISCO_88_recoded,  data = data)
m3_npno <- glm(turbulence ~ eduyears + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9,  data = data)
m4_npno <- glm(turbulence ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9,  data = data)



stargazer(
  m1_npno, m2_npno, m3_npno, m4_npno,
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  # dep.var.labels = "Mainly non-poor then not observed", 
  # dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)



