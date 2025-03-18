

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
  dep.var.labels = "Mainly non-poor", 
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
      model = m3_vuln, 
      trajectory = "Mainly economically vulnerable", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m3_miss, 
      trajectory = "Mainly missing / not observed", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m3_mtp, 
      trajectory = "Mainly Non-poor", 
      variable = "eduyears",
      display_name = "Years of education"
    ),
    list(
      model = m3_pp, 
      trajectory = "Mainly protected poor", 
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
      model = m4_vuln, 
      trajectory = "Mainly economically vulnerable", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m4_miss, 
      trajectory = "Mainly missing / not observed", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m4_mtp, 
      trajectory = "Mainly Non-poor", 
      variable = "highest_lifetime_ISCO_88_recoded",
      display_name = "Highest lifetime ISCO-88"
    ),
    list(
      model = m4_pp, 
      trajectory = "Mainly protected poor", 
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
    title = "Predicted Probabilities by Poverty Trajectory Type and Socioeconomic Variables",
    subtitle = "Black indicates statistically significant effects (p < 0.05)",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

# Display the combined plot
combined_plot
