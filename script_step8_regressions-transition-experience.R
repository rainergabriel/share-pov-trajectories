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


# Load required libraries
library(ggplot2)
library(dplyr)
library(broom)
library(tidyr)
library(patchwork)

# Function to calculate predicted probabilities with proper significance
calculate_predicted_probs <- function() {
  # Part 1: Education continuous curves
  # Specify the education models
  edu_models_info <- list(
    list(
      model = m3_twp, 
      trajectory = "Twice Poor", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m3_ipw, 
      trajectory = "Income Poor but Wealthy", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m3_npbnw, 
      trajectory = "Not Poor but No Wealth", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m3_np, 
      trajectory = "Not Poor", 
      variable = "eduyears",
      display_name = "Years of education"
    )
  )
  
  # Calculate predictions for education models
  edu_results <- lapply(edu_models_info, function(model_info) {
    # Get model information
    model <- model_info$model
    var_name <- model_info$variable
    
    # Get model summary for p-value
    model_summary <- tidy(model)
    p_value <- model_summary %>% filter(term == var_name) %>% pull(p.value)
    is_significant <- !is.null(p_value) && p_value < 0.05
    
    # Get data range
    pred_data <- model.frame(model)
    min_edu <- floor(min(pred_data[[var_name]]))
    max_edu <- ceiling(max(pred_data[[var_name]]))
    
    # Create a sequence of education years across the range
    edu_seq <- seq(from = min_edu, to = max_edu, by = 1)
    
    # Create prediction dataframes at each education level
    result_list <- list()
    
    for (edu in edu_seq) {
      # Create prediction dataset
      new_data <- pred_data
      new_data[[var_name]] <- rep(edu, nrow(new_data))
      
      # Get predictions for all rows
      preds <- predict(model, newdata = new_data, type = "response")
      
      # Calculate mean
      mean_pred <- mean(preds)
      
      result_list[[length(result_list) + 1]] <- data.frame(
        Trajectory = model_info$trajectory,
        Variable = model_info$display_name,
        Years = edu,
        PredictedProbability = mean_pred,
        Significant = is_significant
      )
    }
    
    do.call(rbind, result_list)
  })
  
  # Combine all education results
  education_df <- do.call(rbind, edu_results)
  
  # Part 2: Categorical ISCO predictions
  # Specify the ISCO models
  isco_models_info <- list(
    list(
      model = m4_twp, 
      trajectory = "Twice Poor", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m4_ipw, 
      trajectory = "Income Poor but Wealthy", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m4_npbnw, 
      trajectory = "Not Poor but No Wealth", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m4_np, 
      trajectory = "Not Poor", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    )
  )
  
  # Calculate predictions for ISCO models
  isco_results <- lapply(isco_models_info, function(model_info) {
    # Get model and variable name
    model <- model_info$model
    var_name <- model_info$variable
    
    # Get model summary for p-values
    model_summary <- tidy(model)
    
    # Get model frame and data
    pred_data <- model.frame(model)
    
    # Get the terms for each level of the categorical variable
    high_term <- paste0(var_name, "high")
    low_term <- paste0(var_name, "low")
    
    # Get p-values for each level
    p_high <- model_summary %>% filter(term == high_term) %>% pull(p.value)
    p_low <- model_summary %>% filter(term == low_term) %>% pull(p.value)
    
    # Check significance for each level
    sig_high <- !is.null(p_high) && p_high < 0.05
    sig_low <- !is.null(p_low) && p_low < 0.05
    
    # Create new data for predictions with specific factor levels
    new_data_list <- list()
    levels <- c("high", "medium", "low")
    
    for (level in levels) {
      new_data <- pred_data
      new_data[[var_name]] <- factor(rep(level, nrow(new_data)), levels = levels)
      new_data_list[[level]] <- new_data
    }
    
    # Calculate predictions for each level
    result_df <- data.frame()
    
    for (level in levels) {
      # Get predictions for all observations
      preds <- predict(model, newdata = new_data_list[[level]], type = "response")
      
      # Calculate mean
      mean_pred <- mean(preds)
      
      # Determine significance
      is_significant <- FALSE
      if (level == "high") is_significant <- sig_high
      if (level == "low") is_significant <- sig_low
      # medium is reference, so never marked significant
      
      # Create display name
      display_level <- level
      if (level == "high") display_level <- "Highly skilled"
      if (level == "medium") display_level <- "Medium"
      if (level == "low") display_level <- "Low skill"
      
      # Add to results
      result_df <- rbind(result_df, data.frame(
        Trajectory = model_info$trajectory,
        Variable = model_info$display_name,
        Level = display_level,
        PredictedProbability = mean_pred,
        Significant = is_significant
      ))
    }
    
    result_df
  })
  
  # Combine all ISCO results
  isco_df <- do.call(rbind, isco_results)
  
  # Return both datasets
  list(education_df = education_df, isco_df = isco_df)
}

# Calculate predicted probabilities
results <- calculate_predicted_probs()
edu_results <- results$education_df
isco_results <- results$isco_df

# Create the combined plot
# 1. Education curve plot
edu_plot <- ggplot(edu_results, aes(
  x = Years, 
  y = PredictedProbability,
  color = Significant
)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "gray50"),
    guide = "none"
  ) +
  labs(
    x = "Years of Education",
    y = "Predicted Probability"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ Trajectory, ncol = 2)

# 2. ISCO bar plot with value labels
isco_plot <- ggplot(isco_results, aes(
  x = Level, 
  y = PredictedProbability, 
  fill = Significant
)) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.9),
    width = 0.7
  ) +
  geom_text(
    aes(label = scales::percent(PredictedProbability, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c("TRUE" = "black", "FALSE" = "gray80"),
    guide = "none"
  ) +
  labs(
    x = "",
    y = "Predicted Probability"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ Trajectory, ncol = 2)

# Combine plots using patchwork
combined_plot <- edu_plot / isco_plot +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Predicted Probabilities by Poverty Transition Type and Socioeconomic Variables",
    subtitle = "Black indicates statistically significant effects (p < 0.05)",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

# Display the combined plot
combined_plot
