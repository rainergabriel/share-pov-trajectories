# Load necessary libraries
library(stargazer)
library(car)
library(broom)
library(dplyr)
library(gt)
library(kableExtra)

# Load data
rm(list=ls())
load(file="data_transition-data.Rdata")

data <- transition.data

names(data %>% select(starts_with("experience")))

# --- experience.twice_poor.transition.bn ---
m1_twp <- glm(experience.twice_poor.transition.bn ~ eduyears, family = "binomial", data = data)
m2_twp <- glm(experience.twice_poor.transition.bn ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_twp <- glm(experience.twice_poor.transition.bn ~ eduyears + age + cohort + gender.rcd , family = "binomial", data = data)
m4_twp <- glm(experience.twice_poor.transition.bn ~ highest_lifetime_ISCO_88_recoded + age + cohort + gender.rcd , family = "binomial", data = data)

coef_m1_twp <- exp(coef(m1_twp))
coef_m2_twp <- exp(coef(m2_twp))
coef_m3_twp <- exp(coef(m3_twp))
coef_m4_twp <- exp(coef(m4_twp))

stargazer(
  m1_twp, m2_twp, m3_twp, m4_twp,
  coef = list(coef_m1_twp, coef_m2_twp, coef_m3_twp, coef_m4_twp),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Twice Poor", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Education", "High-skill job", "Low-skill job")
)

# --- experience.income_poor_but_wealth.transition.bn ---
m1_ipw <- glm(experience.income_poor_but_wealth.transition.bn ~ eduyears, family = "binomial", data = data)
m2_ipw <- glm(experience.income_poor_but_wealth.transition.bn ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_ipw <- glm(experience.income_poor_but_wealth.transition.bn ~ eduyears + cohort + gender.rcd +age, family = "binomial", data = data)
m4_ipw <- glm(experience.income_poor_but_wealth.transition.bn ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +age, family = "binomial", data = data)

coef_m1_ipw <- exp(coef(m1_ipw))
coef_m2_ipw <- exp(coef(m2_ipw))
coef_m3_ipw <- exp(coef(m3_ipw))
coef_m4_ipw <- exp(coef(m4_ipw))

stargazer(
  m1_ipw, m2_ipw, m3_ipw, m4_ipw,
  coef = list(coef_m1_ipw, coef_m2_ipw, coef_m3_ipw, coef_m4_ipw),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Income Poor but Wealthy", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Education", "High-skill job", "Low-skill job")
)

# --- experience.not_poor_but_nowealth.transition.bn ---
m1_npbnw <- glm(experience.not_poor_but_nowealth.transition.bn ~ eduyears, family = "binomial", data = data)
m2_npbnw <- glm(experience.not_poor_but_nowealth.transition.bn ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_npbnw <- glm(experience.not_poor_but_nowealth.transition.bn ~ eduyears + cohort + gender.rcd +age, family = "binomial", data = data)
m4_npbnw <- glm(experience.not_poor_but_nowealth.transition.bn ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +age, family = "binomial", data = data)

coef_m1_npbnw <- exp(coef(m1_npbnw))
coef_m2_npbnw <- exp(coef(m2_npbnw))
coef_m3_npbnw <- exp(coef(m3_npbnw))
coef_m4_npbnw <- exp(coef(m4_npbnw))

stargazer(
  m1_npbnw, m2_npbnw, m3_npbnw, m4_npbnw,
  coef = list(coef_m1_npbnw, coef_m2_npbnw, coef_m3_npbnw, coef_m4_npbnw),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Not Poor but No Wealth", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Education", "High-skill job", "Low-skill job")
)

# --- experience.not_poor.transition.bn ---
m1_np <- glm(experience.not_poor.transition.bn ~ eduyears, family = "binomial", data = data)
m2_np <- glm(experience.not_poor.transition.bn ~ highest_lifetime_ISCO_88_recoded, family = "binomial", data = data)
m3_np <- glm(experience.not_poor.transition.bn ~ eduyears + cohort + gender.rcd +age, 
             family = "binomial", data = data)
m4_np <- glm(experience.not_poor.transition.bn ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd +
               valid.information.wjoint.income.wealth.poverty.bn.w2 +age, 
             family = "binomial", data = data)

coef_m1_np <- exp(coef(m1_np))
coef_m2_np <- exp(coef(m2_np))
coef_m3_np <- exp(coef(m3_np))
coef_m4_np <- exp(coef(m4_np))

stargazer(
  m1_np, m2_np, m3_np, m4_np,
  coef = list(coef_m1_np, coef_m2_np, coef_m3_np, coef_m4_np),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.labels = "Not Poor", 
  dep.var.caption = "Poverty trajectory type",
  covariate.labels = c("Education", "High-skill job", "Low-skill job")
)




# consolidated models  ----------------------------------------------------




# Consolidated Table for Poverty Transition Models --------------------------------

library(stargazer)
# Consolidated Table for Poverty Transition Models --------------------------------

library(stargazer)

# Shorten model names for stargazer compatibility
m1_1 <- m1_twp; m2_1 <- m2_twp; m3_1 <- m3_twp; m4_1 <- m4_twp;
m1_2 <- m1_ipw; m2_2 <- m2_ipw; m3_2 <- m3_ipw; m4_2 <- m4_ipw;
m1_3 <- m1_npbnw; m2_3 <- m2_npbnw; m3_3 <- m3_npbnw; m4_3 <- m4_npbnw;
m1_4 <- m1_np; m2_4 <- m2_np; m3_4 <- m3_np; m4_4 <- m4_np;

# Convert coefficients to odds ratios (exp(coef))
coef_m1_1 <- exp(coef(m1_1)); coef_m2_1 <- exp(coef(m2_1)); coef_m3_1 <- exp(coef(m3_1)); coef_m4_1 <- exp(coef(m4_1))
coef_m1_2 <- exp(coef(m1_2)); coef_m2_2 <- exp(coef(m2_2)); coef_m3_2 <- exp(coef(m3_2)); coef_m4_2 <- exp(coef(m4_2))
coef_m1_3 <- exp(coef(m1_3)); coef_m2_3 <- exp(coef(m2_3)); coef_m3_3 <- exp(coef(m3_3)); coef_m4_3 <- exp(coef(m4_3))
coef_m1_4 <- exp(coef(m1_4)); coef_m2_4 <- exp(coef(m2_4)); coef_m3_4 <- exp(coef(m3_4)); coef_m4_4 <- exp(coef(m4_4))

# --- Consolidated Table 1: Twice Poor & Income Poor but Wealthy ---
stargazer(
  m1_1, m2_1, m3_1, m4_1,
  m1_2, m2_2, m3_2, m4_2,
  coef = list(coef_m1_1, coef_m2_1, coef_m3_1, coef_m4_1,
              coef_m1_2, coef_m2_2, coef_m3_2, coef_m4_2),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  # dep.var.caption = "Poverty Trajectory Type - Twice Poor & Income Poor but Wealthy",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)


stargazer(
  m1_1, m2_1, m3_1, m4_1,
  m1_2, m2_2, m3_2, m4_2,
  coef = list(coef_m1_1, coef_m2_1, coef_m3_1, coef_m4_1,
              coef_m1_2, coef_m2_2, coef_m3_2, coef_m4_2),
  type = "html", 
  out = "stargazer_table_transitions_1.html",
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  # dep.var.caption = "Poverty Trajectory Type - Twice Poor & Income Poor but Wealthy",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)



# --- Consolidated Table 2: Not Poor but No Wealth & Transition to Not Poor ---
stargazer(
  m1_3, m2_3, m3_3, m4_3,
  m1_4, m2_4, m3_4, m4_4,
  coef = list(coef_m1_3, coef_m2_3, coef_m3_3, coef_m4_3,
              coef_m1_4, coef_m2_4, coef_m3_4, coef_m4_4),
  type = "text", 
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  # dep.var.caption = "Poverty Trajectory Type - Not Poor but No Wealth & Transition to Not Poor",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)

stargazer(
  m1_3, m2_3, m3_3, m4_3,
  m1_4, m2_4, m3_4, m4_4,
  coef = list(coef_m1_3, coef_m2_3, coef_m3_3, coef_m4_3,
              coef_m1_4, coef_m2_4, coef_m3_4, coef_m4_4),
  type = "html", 
  out = "stargazer_table_transitions_2.html",
  report = "vc*",
  omit = c("valid*", "cohort*", "Constant", "gender*"),
  single.row = TRUE,
  p.auto = FALSE,
  digits = 2,
  dep.var.caption = "Poverty Trajectory Type - Not Poor but No Wealth & Transition to Not Poor",
  covariate.labels = c("Years of education", "Highly skilled occupation (ref. medium)", "Low skilled occupation")
)




# Average Marginal effect displays ----------------------------------------
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
      model = m3_twp, 
      trajectory = "Twice Poor", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m4_twp, 
      trajectory = "Twice Poor", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m3_ipw, 
      trajectory = "Income Poor but Wealthy", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m4_ipw, 
      trajectory = "Income Poor but Wealthy", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m3_npbnw, 
      trajectory = "Not Poor but No Wealth", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m4_npbnw, 
      trajectory = "Not Poor but No Wealth", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m3_np, 
      trajectory = "Not Poor", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m4_np, 
      trajectory = "Not Poor", 
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
    title = "Predicted Probabilities by Trajectory and Variable",
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