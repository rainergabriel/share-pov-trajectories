

# install.packages("gt", dependencies = TRUE, reinstall = TRUE)
# install.packages("rlang", dependencies = TRUE, type = "source")
# install.packages("kableExtra")

library(stargazer)
library(car)

library(broom)
library(dplyr)

library(gt)  # For nice tables (or use kableExtra)


library(kableExtra)

# load data  --------------------------------------------------------------


rm(list=ls())
load(file="data_step3-out-variables-all-setup.Rdata")

# Modellieren Part 1 -------------------------------------------------------------


# --- POV.CLUST.Mainly_economically_vulnerable ---
m1_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ eduyears, family = "binomial", data = data)
m2_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ eduyears + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_vuln <- glm(POV.CLUST.Mainly_economically_vulnerable ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

# Compute Odds Ratios
coef_m1_vuln <- exp(coef(m1_vuln))
coef_m2_vuln <- exp(coef(m2_vuln))
coef_m3_vuln <- exp(coef(m3_vuln))
coef_m4_vuln <- exp(coef(m4_vuln))

# Stargazer Table
stargazer(
  m1_vuln, m2_vuln, m3_vuln, m4_vuln,
  coef = list(coef_m1_vuln, coef_m2_vuln, coef_m3_vuln, coef_m4_vuln),
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


stargazer(
  m1_vuln, m2_vuln, m3_vuln, m4_vuln,
  coef = list(coef_m1_vuln, coef_m2_vuln, coef_m3_vuln, coef_m4_vuln),
  type = "html", 
  out = "stargazer_cluster-membership_FINAL_econ-vulnerable.html",
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly economically vulnerable", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)


# --- POV.CLUST.Mainly_missing___not_observed ---
m1_miss <- glm(POV.CLUST.Mainly_missing___not_observed ~ eduyears, family = "binomial", data = data)
m2_miss <- glm(POV.CLUST.Mainly_missing___not_observed ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_miss <- glm(POV.CLUST.Mainly_missing___not_observed ~ eduyears + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_miss <- glm(POV.CLUST.Mainly_missing___not_observed ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                 valid.information.wjoint.income.wealth.poverty.bn.w2 +
                 valid.information.wjoint.income.wealth.poverty.bn.w4 +
                 valid.information.wjoint.income.wealth.poverty.bn.w5 +
                 valid.information.wjoint.income.wealth.poverty.bn.w6 +
                 valid.information.wjoint.income.wealth.poverty.bn.w7 +
                 valid.information.wjoint.income.wealth.poverty.bn.w8 +
                 valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

# Compute Odds Ratios
coef_m1_miss <- exp(coef(m1_miss))
coef_m2_miss <- exp(coef(m2_miss))
coef_m3_miss <- exp(coef(m3_miss))
coef_m4_miss <- exp(coef(m4_miss))

# Stargazer Table
stargazer(
  m1_miss, m2_miss, m3_miss, m4_miss,
  coef = list(coef_m1_miss, coef_m2_miss, coef_m3_miss, coef_m4_miss),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly missing / not observed", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

stargazer(
  m1_miss, m2_miss, m3_miss, m4_miss,
  coef = list(coef_m1_miss, coef_m2_miss, coef_m3_miss, coef_m4_miss),
  type = "html", 
  out = "stargazer_cluster_FINAL_membership_missing.html",
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Variations of missing / not observed", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

# --- POV.CLUST.Mainly_Non_poor ---
m1_mtp <- glm(POV.CLUST.Mainly_Non_poor ~ eduyears, family = "binomial", data = data)
m2_mtp <- glm(POV.CLUST.Mainly_Non_poor ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_mtp <- glm(POV.CLUST.Mainly_Non_poor ~ eduyears + cohort + gender.rcd +
                valid.information.wjoint.income.wealth.poverty.bn.w2 +
                valid.information.wjoint.income.wealth.poverty.bn.w4 +
                valid.information.wjoint.income.wealth.poverty.bn.w5 +
                valid.information.wjoint.income.wealth.poverty.bn.w6 +
                valid.information.wjoint.income.wealth.poverty.bn.w7 +
                valid.information.wjoint.income.wealth.poverty.bn.w8 +
                valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_mtp <- glm(POV.CLUST.Mainly_Non_poor ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
                valid.information.wjoint.income.wealth.poverty.bn.w2 +
                valid.information.wjoint.income.wealth.poverty.bn.w4 +
                valid.information.wjoint.income.wealth.poverty.bn.w5 +
                valid.information.wjoint.income.wealth.poverty.bn.w6 +
                valid.information.wjoint.income.wealth.poverty.bn.w7 +
                valid.information.wjoint.income.wealth.poverty.bn.w8 +
                valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

coef_m1_mtp <- exp(coef(m1_mtp))
coef_m2_mtp <- exp(coef(m2_mtp))
coef_m3_mtp <- exp(coef(m3_mtp))
coef_m4_mtp <- exp(coef(m4_mtp))

stargazer(
  m1_mtp, m2_mtp, m3_mtp, m4_mtp,
  coef = list(coef_m1_mtp, coef_m2_mtp, coef_m3_mtp, coef_m4_mtp),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly missing to non-poor", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

stargazer(
  m1_mtp, m2_mtp, m3_mtp, m4_mtp,
  coef = list(coef_m1_mtp, coef_m2_mtp, coef_m3_mtp, coef_m4_mtp),
  type = "html", 
  out = "stargazer_cluster-membership_FINAL_non-poor.html",  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly non-poor", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

library(stargazer)


# --- POV.CLUST.Mainly_economically_vulnerable ---
# Models and stargazer table already included

# --- POV.CLUST.Mainly_non_poor_then_not_observed ---
# Models and stargazer table already included

# --- POV.CLUST.Mainly_protected_poor ---
m1_pp <- glm(POV.CLUST.Mainly_protected_poor ~ eduyears, family = "binomial", data = data)
m2_pp <- glm(POV.CLUST.Mainly_protected_poor ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_pp <- glm(POV.CLUST.Mainly_protected_poor ~ eduyears + cohort + gender.rcd +
               valid.information.wjoint.income.wealth.poverty.bn.w2 +
               valid.information.wjoint.income.wealth.poverty.bn.w4 +
               valid.information.wjoint.income.wealth.poverty.bn.w5 +
               valid.information.wjoint.income.wealth.poverty.bn.w6 +
               valid.information.wjoint.income.wealth.poverty.bn.w7 +
               valid.information.wjoint.income.wealth.poverty.bn.w8 +
               valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)
m4_pp <- glm(POV.CLUST.Mainly_protected_poor ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
               valid.information.wjoint.income.wealth.poverty.bn.w2 +
               valid.information.wjoint.income.wealth.poverty.bn.w4 +
               valid.information.wjoint.income.wealth.poverty.bn.w5 +
               valid.information.wjoint.income.wealth.poverty.bn.w6 +
               valid.information.wjoint.income.wealth.poverty.bn.w7 +
               valid.information.wjoint.income.wealth.poverty.bn.w8 +
               valid.information.wjoint.income.wealth.poverty.bn.w9, family = "binomial", data = data)

coef_m1_pp <- exp(coef(m1_pp))
coef_m2_pp <- exp(coef(m2_pp))
coef_m3_pp <- exp(coef(m3_pp))
coef_m4_pp <- exp(coef(m4_pp))

stargazer(
  m1_pp, m2_pp, m3_pp, m4_pp,
  coef = list(coef_m1_pp, coef_m2_pp, coef_m3_pp, coef_m4_pp),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly protected poor", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

stargazer(
  m1_pp, m2_pp, m3_pp, m4_pp,
  coef = list(coef_m1_pp, coef_m2_pp, coef_m3_pp, coef_m4_pp),
  type = "html", 
  out = "stargazer_cluster-membership_FINAL_protected-poor.html",  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Mainly protected poor", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)


# create plot -------------------------------------------------------------

# Predicted Probabilities ----------------------------------------
# Load required libraries
library(margins)
library(ggplot2)
library(dplyr)
library(broom)

# Function to calculate predicted probabilities with significance
calculate_predicted_probs <- function() {
  # Specify the models and variables of interest
  models_info <- list(
    list(
      model = m3_vuln, 
      trajectory = "Mainly economically vulnerable", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m4_vuln, 
      trajectory = "Mainly economically vulnerable", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m3_miss, 
      trajectory = "Mainly missing / not observed", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m4_miss, 
      trajectory = "Mainly missing / not observed", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m3_mtp, 
      trajectory = "Mainly Non-poor", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m4_mtp, 
      trajectory = "Mainly Non-poor", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m3_pp, 
      trajectory = "Mainly protected poor", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m4_pp, 
      trajectory = "Mainly protected poor", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    )
  )
  
  # Calculate predicted probabilities and significance
  results <- lapply(models_info, function(model_info) {
    # Get model and variable name
    model <- model_info$model
    var_name <- model_info$variable
    display_name <- model_info$display_name
    
    # Get model frame
    pred_data <- model.frame(model)
    
    # Conduct statistical test
    model_summary <- tidy(model)
    
    # For categorical variables, we need to check all levels
    if (is.factor(pred_data[[var_name]])) {
      # Get all terms related to this variable
      var_terms <- model_summary %>% 
        filter(grepl(paste0("^", var_name), term))
      
      # Check if any of these terms are significant
      is_significant <- any(var_terms$p.value < 0.05)
      
      # Predict for each level of the categorical variable
      pred_probs <- tapply(predict(model, type = "response"), 
                           pred_data[[var_name]], 
                           mean)
      
      # Rename the levels
      level_names <- names(pred_probs)
      display_levels <- level_names
      
      # Rename specific levels if they match
      for (i in 1:length(level_names)) {
        if (level_names[i] == "high") display_levels[i] <- "Highly skilled"
        if (level_names[i] == "low") display_levels[i] <- "Low skill"
        if (level_names[i] == "medium") display_levels[i] <- "Medium"
      }
      
      # Create a data frame with one row per level
      result_df <- data.frame(
        Trajectory = rep(model_info$trajectory, length(pred_probs)),
        Variable = rep(display_name, length(pred_probs)),
        Level = display_levels,
        PredictedProbability = as.numeric(pred_probs),
        Significant = rep(is_significant, length(pred_probs))
      )
      
      return(result_df)
    } else {
      # For continuous variables
      var_sig <- model_summary %>% 
        filter(term == var_name) %>% 
        mutate(significant = p.value < 0.05)
      
      # If no matching term was found, set significance to FALSE
      if(nrow(var_sig) == 0) {
        is_significant <- FALSE
      } else {
        is_significant <- var_sig$significant
      }
      
      # Return a single row for continuous variables
      return(data.frame(
        Trajectory = model_info$trajectory,
        Variable = display_name,
        Level = "Mean",
        PredictedProbability = mean(predict(model, type = "response")),
        Significant = is_significant
      ))
    }
  })
  
  # Combine results
  do.call(rbind, results)
}

# Calculate predicted probabilities
prob_results <- calculate_predicted_probs()

# Create the plot
ggplot(prob_results, aes(
  x = Level, 
  y = PredictedProbability, 
  fill = Significant,
  color = Significant
)) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.9), 
    aes(alpha = Significant)
  ) +
  geom_point(
    position = position_dodge(width = 0.9), 
    size = 3,
    aes(color = Significant)
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
    title = "Predicted Probabilities by Transition Experience (Transitioning into a Given State) and Variable",
    subtitle = "Black indicates statistically significant effects (p < 0.05)",
    x = "Level",
    y = "Predicted Probability"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  facet_wrap(~ Trajectory + Variable, ncol = 2, scales = "free_x")

# Print out the exact values with significance
print(prob_results)
