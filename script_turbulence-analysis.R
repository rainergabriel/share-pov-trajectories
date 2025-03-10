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

stargazer(
  m1_vuln, m2_vuln, m3_vuln, m4_vuln,
  type = "html", 
  out = "stargazer_entropy-FINAL.html",   
  report = "vc*",  
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Entropy Index", 
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



stargazer(
  m1_npno, m2_npno, m3_npno, m4_npno,
  type = "html", 
  out = "stargazer_turbulence-FINAL.html",    report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Turbulence",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)


# display the results in a graph  -----------------------------------------


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

stargazer(
  m1_vuln, m2_vuln, m3_vuln, m4_vuln,
  type = "html", 
  out = "stargazer_entropy-FINAL.html",   
  report = "vc*",  
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Entropy Index", 
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



stargazer(
  m1_npno, m2_npno, m3_npno, m4_npno,
  type = "html", 
  out = "stargazer_turbulence-FINAL.html",    report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Turbulence",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)



# displaying the results  -------------------------------------------------
# Predicted Values for Linear Models ----------------------------------------
# Load required libraries
library(ggplot2)
library(dplyr)
library(broom)
library(kableExtra)

# Function to calculate predicted values with confidence intervals
calculate_predicted_values <- function() {
  # Specify the models and variables of interest
  models_info <- list(
    list(
      model = m3_vuln, 
      outcome = "Entropy", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m4_vuln, 
      outcome = "Entropy", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m3_npno, 
      outcome = "Turbulence", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m4_npno, 
      outcome = "Turbulence", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    )
  )
  
  # Calculate predicted values and significance
  results <- lapply(models_info, function(model_info) {
    # Get model and variable name
    model <- model_info$model
    var_name <- model_info$variable
    display_name <- model_info$display_name
    
    # Get model frame
    pred_data <- model.frame(model)
    
    # Conduct statistical test for the variable
    model_summary <- tidy(model)
    
    # For categorical variables, we need representative values
    if (is.factor(pred_data[[var_name]])) {
      # Get all terms related to this variable
      var_terms <- model_summary %>% 
        filter(grepl(paste0("^", var_name), term))
      
      # Check if any of these terms are significant
      is_significant <- any(var_terms$p.value < 0.05)
      
      # Create new data frame with the same structure as the original
      # This ensures variable types are preserved
      new_data <- pred_data[rep(1, length(levels(pred_data[[var_name]]))), ]
      
      # Set categorical variable levels
      new_data[[var_name]] <- factor(levels(pred_data[[var_name]]), levels = levels(pred_data[[var_name]]))
      
      # Get predictions with confidence intervals
      preds <- predict(model, newdata = new_data, se.fit = TRUE)
      
      # Calculate confidence intervals
      conf_int_lower <- preds$fit - 1.96 * preds$se.fit
      conf_int_upper <- preds$fit + 1.96 * preds$se.fit
      
      # Rename the levels for display
      level_names <- levels(pred_data[[var_name]])
      display_levels <- level_names
      
      # Rename specific levels if they match
      for (i in 1:length(level_names)) {
        if (level_names[i] == "high") display_levels[i] <- "Highly skilled"
        if (level_names[i] == "low") display_levels[i] <- "Low skill"
        if (level_names[i] == "medium") display_levels[i] <- "Medium"
      }
      
      # Create a data frame with the results
      result_df <- data.frame(
        Outcome = rep(model_info$outcome, length(level_names)),
        Variable = rep(display_name, length(level_names)),
        Level = display_levels,
        PredictedValue = preds$fit,
        CI_Lower = conf_int_lower,
        CI_Upper = conf_int_upper,
        Significant = rep(is_significant, length(level_names))
      )
      
      return(result_df)
    } else {
      # For continuous variables, create a sequence of values
      var_sig <- model_summary %>% 
        filter(term == var_name) %>% 
        mutate(significant = p.value < 0.05)
      
      # If no matching term was found, set significance to FALSE
      if(nrow(var_sig) == 0) {
        is_significant <- FALSE
      } else {
        is_significant <- var_sig$significant
      }
      
      # Create representative values (5 points from min to max)
      min_val <- min(pred_data[[var_name]], na.rm = TRUE)
      max_val <- max(pred_data[[var_name]], na.rm = TRUE)
      seq_vals <- seq(min_val, max_val, length.out = 5)
      
      # Create new data frame with the same structure as the original
      # This ensures variable types are preserved
      new_data <- pred_data[rep(1, length(seq_vals)), ]
      
      # Set the variable of interest
      new_data[[var_name]] <- seq_vals
      
      # Get predictions with confidence intervals
      preds <- predict(model, newdata = new_data, se.fit = TRUE)
      
      # Calculate confidence intervals
      conf_int_lower <- preds$fit - 1.96 * preds$se.fit
      conf_int_upper <- preds$fit + 1.96 * preds$se.fit
      
      # Create a data frame with the results
      result_df <- data.frame(
        Outcome = rep(model_info$outcome, length(seq_vals)),
        Variable = rep(display_name, length(seq_vals)),
        Level = as.character(round(seq_vals, 1)),
        PredictedValue = preds$fit,
        CI_Lower = conf_int_lower,
        CI_Upper = conf_int_upper,
        Significant = rep(is_significant, length(seq_vals))
      )
      
      return(result_df)
    }
  })
  
  # Combine results
  do.call(rbind, results)
}

# Calculate predicted values
pred_results <- calculate_predicted_values()

# Create the visualization for categorical predictors
categorical_results <- pred_results %>%
  filter(!grepl("^\\d", Level))

# Create the plot for categorical predictors
ggplot(categorical_results, aes(
  x = Level, 
  y = PredictedValue, 
  fill = Significant,
  color = Significant
)) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.9), 
    aes(alpha = Significant)
  ) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper, color = Significant),
    position = position_dodge(width = 0.9),
    width = 0.25
  ) +
  scale_fill_manual(
    values = c("TRUE" = "black", "FALSE" = "gray80"),
    guide = "none"
  ) +
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "gray80"),
    guide = "none"
  ) +
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.5),
    guide = "none"
  ) +
  labs(
    title = "Predicted Values by Outcome Measure and Variable",
    subtitle = "Black indicates statistically significant effects (p < 0.05)",
    x = "Level",
    y = "Predicted Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  facet_wrap(~ Outcome + Variable, ncol = 2, scales = "free_y")

# Save the plot
ggsave("predicted_values_categorical.png", width = 12, height = 8)

# Create visualization for continuous predictors (education years)
continuous_results <- pred_results %>%
  filter(grepl("^\\d", Level))
# Create visualization for continuous predictors (education years)
continuous_results <- pred_results %>%
  filter(grepl("^\\d", Level))

# Get p-values for adding to subtitle
education_sig_entropy <- any(tidy(m3_vuln) %>% filter(term == "eduyears") %>% pull(p.value) < 0.05)
education_sig_turbulence <- any(tidy(m3_npno) %>% filter(term == "eduyears") %>% pull(p.value) < 0.05)

# Create subtitle with significance information
education_subtitle <- paste0(
  "Education effect on Entropy: ", ifelse(education_sig_entropy, "significant", "not significant"),
  " | Education effect on Turbulence: ", ifelse(education_sig_turbulence, "significant", "not significant"),
  " (p < 0.05)"
)

# Create the plot for continuous predictors
ggplot(continuous_results, aes(
  x = as.numeric(Level), 
  y = PredictedValue, 
  color = Outcome,
  group = Outcome
)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper, fill = Outcome), alpha = 0.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("Entropy" = "black", "Turbulence" = "darkblue")
  ) +
  scale_fill_manual(
    values = c("Entropy" = "black", "Turbulence" = "darkblue")
  ) +
  labs(
    title = "Predicted Values by Years of Education",
    subtitle = education_subtitle,
    x = "Years of Education",
    y = "Predicted Value"
  ) +
  theme_minimal() +
  facet_wrap(~ Outcome, ncol = 2, scales = "free_y")

# Save the plot
ggsave("predicted_values_education.png", width = 10, height = 6)

# Print out the exact values with significance
print(pred_results)

# Create a formatted table of the results
library(kableExtra)
pred_results %>%
  arrange(Outcome, Variable, Level) %>%
  mutate(
    PredictedValue = round(PredictedValue, 2),
    CI = paste0("[", round(CI_Lower, 2), ", ", round(CI_Upper, 2), "]"),
    PredictedValue = ifelse(Significant, paste0("**", PredictedValue, "**"), as.character(PredictedValue))
  ) %>%
  select(Outcome, Variable, Level, PredictedValue, CI) %>%
  kable(
    format = "html", 
    caption = "Predicted Values by Outcome Measure and Variable",
    col.names = c("Outcome", "Predictor Variable", "Level", "Predicted Value", "95% Confidence Interval")
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  footnote(general = "** = p < 0.05", general_title = "Note: ") %>%
  save_kable("predicted_values_table.html")
