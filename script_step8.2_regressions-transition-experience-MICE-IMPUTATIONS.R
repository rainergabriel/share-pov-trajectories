# Load necessary libraries
library(stargazer)
library(car)
library(broom)
library(dplyr)
library(mice)
library(tidyr)
library(kableExtra)

# Load data (using CSV since we know this works)
data <- read.csv("transition_data_subset.csv", stringsAsFactors = TRUE)

# Check structure of data
str(data)

# Check for missing values in each column
missing_counts <- colSums(is.na(data))
print("Missing values by column:")
print(missing_counts)

# Data preparation - ensure variables are of correct type
# Make sure outcome variables are 0/1 numeric variables for binary logit models
if(is.factor(data$experience.twice_poor.transition.bn)) {
  data$experience.twice_poor.transition.bn <- as.numeric(as.character(data$experience.twice_poor.transition.bn))
}
if(is.factor(data$experience.income_poor_but_wealth.transition.bn)) {
  data$experience.income_poor_but_wealth.transition.bn <- as.numeric(as.character(data$experience.income_poor_but_wealth.transition.bn))
}
if(is.factor(data$experience.not_poor_but_nowealth.transition.bn)) {
  data$experience.not_poor_but_nowealth.transition.bn <- as.numeric(as.character(data$experience.not_poor_but_nowealth.transition.bn))
}
if(is.factor(data$experience.not_poor.transition.bn)) {
  data$experience.not_poor.transition.bn <- as.numeric(as.character(data$experience.not_poor.transition.bn))
}

# Convert highest_lifetime_ISCO_88_recoded to factor if it isn't already
if(!is.factor(data$highest_lifetime_ISCO_88_recoded)) {
  data$highest_lifetime_ISCO_88_recoded <- as.factor(data$highest_lifetime_ISCO_88_recoded)
}

# 1. MULTIPLE IMPUTATION WITH IMPROVED METHODS
# Let's use a combination of methods rather than just random forest

# Include all variables for imputation
all_vars <- c(
  # Outcome variables
  "experience.twice_poor.transition.bn", 
  "experience.income_poor_but_wealth.transition.bn",
  "experience.not_poor_but_nowealth.transition.bn", 
  "experience.not_poor.transition.bn",
  
  # Predictor variables
  "eduyears", 
  "highest_lifetime_ISCO_88_recoded", 
  "age", 
  "cohort",
  "gender.rcd",
  "valid.information.wjoint.income.wealth.poverty.bn.w2",
  
  # Additional variables that might help with imputation
  "POV.CLUST.Mainly_Non_poor",
  "POV.CLUST.Mainly_protected_poor"
)

# Subset data to only include variables needed for imputation
impute_data <- data[, all_vars]

# Check variable types to choose appropriate imputation methods
var_types <- sapply(impute_data, class)
print("Variable types:")
print(var_types)

# Create predictor matrix for imputation (important for categorical variables)
init_imp <- mice(impute_data, maxit = 0)
pred_matrix <- init_imp$predictorMatrix

# Customize imputation methods based on variable types
imputation_methods <- vector("character", length(all_vars))
names(imputation_methods) <- all_vars

# Default method assignments based on variable types
for (i in seq_along(all_vars)) {
  var_name <- all_vars[i]
  
  if (is.character(impute_data[[var_name]]) || is.factor(impute_data[[var_name]])) {
    if (length(unique(impute_data[[var_name]])) == 2) {
      imputation_methods[i] <- "logreg"  # Binary categorical
    } else {
      imputation_methods[i] <- "polyreg"  # Multi-category
    }
  } else if (is.integer(impute_data[[var_name]]) || is.numeric(impute_data[[var_name]])) {
    if (length(unique(impute_data[[var_name]])) == 2) {
      imputation_methods[i] <- "logreg"  # Binary numeric
    } else {
      imputation_methods[i] <- "pmm"  # Continuous numeric
    }
  } else {
    imputation_methods[i] <- "pmm"  # Default to pmm
  }
}

# Specific overrides for certain variables
# For binary outcome variables, use logistic regression
binary_outcomes <- c(
  "experience.twice_poor.transition.bn", 
  "experience.income_poor_but_wealth.transition.bn",
  "experience.not_poor_but_nowealth.transition.bn", 
  "experience.not_poor.transition.bn"
)
imputation_methods[binary_outcomes] <- "logreg"

# Print imputation methods
cat("\nImputation methods for each variable:\n")
print(imputation_methods)

# Perform multiple imputation with mixed methods
# Increase iterations and number of imputed datasets for better results
set.seed(123) # For reproducibility
imp <- mice(impute_data, 
            m = 10,  # Increased from 5 to 10 datasets
            method = imputation_methods,
            predictorMatrix = pred_matrix,
            printFlag = TRUE,
            maxit = 20)  # Increased from 10 to 20 iterations

# Print imputation summary
print(imp)

# 2. IMPROVED FUNCTION TO RUN MODELS AND POOL RESULTS
run_pooled_model <- function(formula, imp_data) {
  tryCatch({
    # Capture the formula as a string for diagnostics
    formula_str <- deparse(formula)
    cat("Attempting model:", formula_str, "\n")
    
    # Run the model on each imputed dataset with extra robustness
    # Use a control object to improve convergence chances
    glm_control <- list(maxit = 100, trace = FALSE)
    models <- with(imp_data, glm(formula, family = binomial(link = "logit"), 
                                 control = glm_control))
    
    # Check if all models converged
    convergence_status <- sapply(models$analyses, function(m) {
      return(!is.null(m) && m$converged)
    })
    
    if (!all(convergence_status)) {
      warning(paste("Some models did not converge for formula:", formula_str))
      
      # Try again with a different convergence algorithm if some failed
      non_converged_indices <- which(!convergence_status)
      
      for (i in non_converged_indices) {
        cat("Retrying model", i, "with different settings\n")
        # Try with more iterations and different algorithm
        alt_control <- list(maxit = 200, trace = FALSE, algorithm = "fisher")
        models$analyses[[i]] <- try(
          glm(formula, family = binomial(link = "logit"), 
              control = alt_control, 
              data = complete(imp_data, i)),
          silent = TRUE
        )
      }
      
      # Check convergence again
      convergence_status <- sapply(models$analyses, function(m) {
        return(!is.null(m) && !inherits(m, "try-error") && m$converged)
      })
      
      if (!all(convergence_status)) {
        warning(paste("Some models still did not converge even with alternative settings"))
      }
    }
    
    # Only pool models that converged
    if (!all(convergence_status)) {
      cat("Note: Only pooling converged models\n")
      models$analyses <- models$analyses[convergence_status]
      
      # If none converged, return NULL
      if (length(models$analyses) == 0) {
        cat("ERROR: No models converged for formula:", formula_str, "\n")
        return(NULL)
      }
    }
    
    # Pool the results
    pooled_model <- pool(models)
    
    # Extract summary statistics
    summary_stats <- summary(pooled_model)
    
    # Calculate odds ratios and confidence intervals
    summary_stats$OR <- exp(summary_stats$estimate)
    summary_stats$CI.low <- exp(summary_stats$estimate - 1.96 * summary_stats$std.error)
    summary_stats$CI.high <- exp(summary_stats$estimate + 1.96 * summary_stats$std.error)
    
    # Add significance stars
    summary_stats$stars <- ""
    summary_stats$stars[summary_stats$p.value < 0.05] <- "*"
    summary_stats$stars[summary_stats$p.value < 0.01] <- "**"
    summary_stats$stars[summary_stats$p.value < 0.001] <- "***"
    
    # Calculate average AIC and BIC across imputed datasets
    aic_values <- sapply(models$analyses, function(m) {
      if (!is.null(m) && !inherits(m, "try-error") && m$converged) {
        return(AIC(m))
      } else {
        return(NA)
      }
    })
    bic_values <- sapply(models$analyses, function(m) {
      if (!is.null(m) && !inherits(m, "try-error") && m$converged) {
        return(BIC(m))
      } else {
        return(NA)
      }
    })
    
    avg_aic <- mean(aic_values, na.rm = TRUE)
    avg_bic <- mean(bic_values, na.rm = TRUE)
    
    # Combine results
    results <- list(
      summary = summary_stats,
      AIC = avg_aic,
      BIC = avg_bic,
      pooled = pooled_model,
      models = models,
      converged = convergence_status
    )
    
    cat("Model successfully pooled\n")
    return(results)
  }, error = function(e) {
    # Convert formula to string before printing
    formula_str <- deparse(formula)
    cat("Error in model:", formula_str, "\n")
    cat("Error message:", e$message, "\n")
    return(NULL)
  })
}

# Try simpler models first for diagnostic purposes
cat("\n\nTrying simplified models first for diagnostics...\n")

# --- Simplified Twice Poor models ---
cat("\nSimplified models for Twice Poor outcome:\n")
twp_simple1 <- run_pooled_model(experience.twice_poor.transition.bn ~ 1, imp)
cat("Intercept-only model status:", ifelse(is.null(twp_simple1), "Failed", "Success"), "\n")

twp_simple2 <- run_pooled_model(experience.twice_poor.transition.bn ~ eduyears, imp)
cat("Single predictor model status:", ifelse(is.null(twp_simple2), "Failed", "Success"), "\n")

# --- Now run full models with better error handling ---
cat("\n\nRunning full models for all outcome variables with improved diagnostics:\n")

# Function to run a model with better error handling and diagnostics
safe_run_model <- function(formula, name, number) {
  cat("\nRunning", name, "model", number, "...\n")
  result <- run_pooled_model(formula, imp)
  cat(name, "model", number, "status:", ifelse(is.null(result), "Failed", "Success"), "\n")
  if (!is.null(result)) {
    # Print model summary statistics for diagnostics
    cat("  Coefficients:", nrow(result$summary), "\n")
    cat("  AIC:", result$AIC, "\n")
  }
  return(result)
}

# --- Twice Poor ---
cat("\nModels for Twice Poor outcome:\n")
twp_m1 <- safe_run_model(experience.twice_poor.transition.bn ~ eduyears, "Twice Poor", 1)
twp_m2 <- safe_run_model(experience.twice_poor.transition.bn ~ highest_lifetime_ISCO_88_recoded, "Twice Poor", 2)
twp_m3 <- safe_run_model(experience.twice_poor.transition.bn ~ eduyears + age + cohort + gender.rcd, "Twice Poor", 3)
twp_m4 <- safe_run_model(experience.twice_poor.transition.bn ~ highest_lifetime_ISCO_88_recoded + age + cohort + gender.rcd, "Twice Poor", 4)

# --- Income Poor but Wealthy ---
cat("\nModels for Income Poor but Wealthy outcome:\n")
ipw_m1 <- safe_run_model(experience.income_poor_but_wealth.transition.bn ~ eduyears, "Income Poor but Wealthy", 1)
ipw_m2 <- safe_run_model(experience.income_poor_but_wealth.transition.bn ~ highest_lifetime_ISCO_88_recoded, "Income Poor but Wealthy", 2)
ipw_m3 <- safe_run_model(experience.income_poor_but_wealth.transition.bn ~ eduyears + cohort + gender.rcd + age, "Income Poor but Wealthy", 3) 
ipw_m4 <- safe_run_model(experience.income_poor_but_wealth.transition.bn ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd + age, "Income Poor but Wealthy", 4)

# --- Not Poor but No Wealth ---
cat("\nModels for Not Poor but No Wealth outcome:\n")
npbnw_m1 <- safe_run_model(experience.not_poor_but_nowealth.transition.bn ~ eduyears, "Not Poor but No Wealth", 1)
npbnw_m2 <- safe_run_model(experience.not_poor_but_nowealth.transition.bn ~ highest_lifetime_ISCO_88_recoded, "Not Poor but No Wealth", 2)
npbnw_m3 <- safe_run_model(experience.not_poor_but_nowealth.transition.bn ~ eduyears + cohort + gender.rcd + age, "Not Poor but No Wealth", 3)
npbnw_m4 <- safe_run_model(experience.not_poor_but_nowealth.transition.bn ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd + age, "Not Poor but No Wealth", 4)

# --- Not Poor ---
cat("\nModels for Not Poor outcome:\n")
np_m1 <- safe_run_model(experience.not_poor.transition.bn ~ eduyears, "Not Poor", 1)
np_m2 <- safe_run_model(experience.not_poor.transition.bn ~ highest_lifetime_ISCO_88_recoded, "Not Poor", 2)
np_m3 <- safe_run_model(experience.not_poor.transition.bn ~ eduyears + cohort + gender.rcd + age, "Not Poor", 3)

# For the 4th model, check if the validation variable exists before using it
if ("valid.information.wjoint.income.wealth.poverty.bn.w2" %in% names(data)) {
  np_m4 <- safe_run_model(experience.not_poor.transition.bn ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd + valid.information.wjoint.income.wealth.poverty.bn.w2 + age, "Not Poor", 4)
} else {
  # Use a simplified 4th model if the validation variable doesn't exist
  np_m4 <- safe_run_model(experience.not_poor.transition.bn ~ highest_lifetime_ISCO_88_recoded + cohort + gender.rcd + age, "Not Poor", 4)
}

# Summarize model success
cat("\n\n=== MODEL SUMMARY ===\n")
model_list <- list(
  "Twice Poor - Model 1" = twp_m1,
  "Twice Poor - Model 2" = twp_m2,
  "Twice Poor - Model 3" = twp_m3,
  "Twice Poor - Model 4" = twp_m4,
  "Income Poor but Wealthy - Model 1" = ipw_m1,
  "Income Poor but Wealthy - Model 2" = ipw_m2,
  "Income Poor but Wealthy - Model 3" = ipw_m3,
  "Income Poor but Wealthy - Model 4" = ipw_m4,
  "Not Poor but No Wealth - Model 1" = npbnw_m1,
  "Not Poor but No Wealth - Model 2" = npbnw_m2,
  "Not Poor but No Wealth - Model 3" = npbnw_m3,
  "Not Poor but No Wealth - Model 4" = npbnw_m4,
  "Not Poor - Model 1" = np_m1,
  "Not Poor - Model 2" = np_m2,
  "Not Poor - Model 3" = np_m3,
  "Not Poor - Model 4" = np_m4
)

success_count <- sum(!sapply(model_list, is.null))
cat("Successfully ran", success_count, "out of", length(model_list), "models\n\n")

# 4. FUNCTION TO CREATE STARGAZER-LIKE TABLES FOR POOLED RESULTS
create_pooled_table <- function(models, covariate_labels = NULL, dep_var_label, output_type = "text", file_name = NULL) {
  # Filter out NULL models
  valid_models <- models[!sapply(models, is.null)]
  n_valid_models <- length(valid_models)
  
  if (n_valid_models == 0) {
    cat("No valid models available for", dep_var_label, "- skipping table creation\n")
    return(NULL)
  }
  
  # Keep track of which models were valid
  valid_indices <- which(!sapply(models, is.null))
  
  # Format coefficients for each valid model
  formatted_coefs <- list()
  for (i in seq_along(valid_models)) {
    model_summary <- valid_models[[i]]$summary
    
    # Add stars to odds ratios
    formatted_coef <- paste0(sprintf("%.2f", model_summary$OR), model_summary$stars)
    names(formatted_coef) <- rownames(model_summary)
    
    formatted_coefs[[i]] <- formatted_coef
  }
  
  # Create a data frame for the output table
  table_data <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  
  # Add model columns
  for (i in 1:n_valid_models) {
    col_name <- paste0("Model", valid_indices[i])
    table_data[[col_name]] <- character()
  }
  
  # Get all variable names across models
  all_vars <- unique(unlist(lapply(valid_models, function(m) rownames(m$summary))))
  
  # Fill in coefficient values
  for (var in all_vars) {
    if (var != "(Intercept)") {  # Skip intercept
      row_data <- c(var)
      
      for (i in 1:n_valid_models) {
        model_summary <- valid_models[[i]]$summary
        if (var %in% rownames(model_summary)) {
          idx <- which(rownames(model_summary) == var)
          row_data <- c(row_data, formatted_coefs[[i]][idx])
        } else {
          row_data <- c(row_data, "")
        }
      }
      
      table_data[nrow(table_data) + 1, ] <- row_data
    }
  }
  
  # Add AIC and BIC rows
  aic_row <- c("AIC")
  bic_row <- c("BIC")
  
  for (i in 1:n_valid_models) {
    aic_row <- c(aic_row, sprintf("%.2f", valid_models[[i]]$AIC))
    bic_row <- c(bic_row, sprintf("%.2f", valid_models[[i]]$BIC))
  }
  
  table_data[nrow(table_data) + 1, ] <- aic_row
  table_data[nrow(table_data) + 1, ] <- bic_row
  
  # Rename variables if covariate_labels provided
  if (!is.null(covariate_labels)) {
    for (i in 1:nrow(table_data)) {
      var_name <- table_data$Variable[i]
      if (var_name %in% names(covariate_labels)) {
        table_data$Variable[i] <- covariate_labels[[var_name]]
      } else if (startsWith(var_name, "highest_lifetime_ISCO_88_recoded")) {
        if (var_name == "highest_lifetime_ISCO_88_recoded1") {
          table_data$Variable[i] <- "Highly skilled occupation (ref. medium)"
        } else if (var_name == "highest_lifetime_ISCO_88_recoded3") {
          table_data$Variable[i] <- "Low skilled occupation"
        }
      } else if (startsWith(var_name, "cohort")) {
        table_data$Variable[i] <- paste0("Cohort: ", sub("^cohort", "", var_name))
      } else if (startsWith(var_name, "gender.rcd")) {
        table_data$Variable[i] <- paste0("Gender: ", sub("^gender.rcd", "", var_name))
      }
    }
  }
  
  # Create header
  header <- paste0("Dependent Variable: ", dep_var_label, " (Pooled Results after Multiple Imputation)")
  
  # Print the table
  if (output_type == "text") {
    cat("\n", header, "\n")
    cat(paste(rep("=", nchar(header)), collapse = ""), "\n")
    
    # Print model numbers
    cat(paste0("Variable", paste(rep(" ", max(nchar(table_data$Variable)) - nchar("Variable")), collapse = "")))
    for (i in 1:n_valid_models) {
      cat(paste0("   Model ", valid_indices[i]))
    }
    cat("\n")
    
    # Print separator
    cat(paste(rep("-", max(nchar(table_data$Variable)) + 8*n_valid_models), collapse = ""), "\n")
    
    # Print rows
    for (i in 1:nrow(table_data)) {
      cat(paste0(table_data$Variable[i], paste(rep(" ", max(nchar(table_data$Variable)) - nchar(table_data$Variable[i])), collapse = "")))
      for (j in 2:(n_valid_models+1)) {
        cat(paste0("   ", table_data[i, j], paste(rep(" ", 8 - nchar(table_data[i, j])), collapse = "")))
      }
      cat("\n")
    }
    
    cat(paste(rep("=", max(nchar(table_data$Variable)) + 8*n_valid_models), collapse = ""), "\n")
  }
  
  # Write to file if specified
  if (!is.null(file_name)) {
    if (output_type == "text") {
      # Create text file
      sink(file_name)
      cat("\n", header, "\n")
      cat(paste(rep("=", nchar(header)), collapse = ""), "\n")
      
      # Print model numbers
      cat(paste0("Variable", paste(rep(" ", max(nchar(table_data$Variable)) - nchar("Variable")), collapse = "")))
      for (i in 1:n_valid_models) {
        cat(paste0("   Model ", valid_indices[i]))
      }
      cat("\n")
      
      # Print separator
      cat(paste(rep("-", max(nchar(table_data$Variable)) + 8*n_valid_models), collapse = ""), "\n")
      
      # Print rows
      for (i in 1:nrow(table_data)) {
        cat(paste0(table_data$Variable[i], paste(rep(" ", max(nchar(table_data$Variable)) - nchar(table_data$Variable[i])), collapse = "")))
        for (j in 2:(n_valid_models+1)) {
          cat(paste0("   ", table_data[i, j], paste(rep(" ", 8 - nchar(table_data[i, j])), collapse = "")))
        }
        cat("\n")
      }
      
      cat(paste(rep("=", max(nchar(table_data$Variable)) + 8*n_valid_models), collapse = ""), "\n")
      sink()
    } else if (output_type == "html") {
      # Create HTML table using kableExtra
      library(knitr)
      
      html_table <- kable(table_data, format = "html", caption = header) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
      
      write(html_table, file = file_name)
    }
  }
  
  return(table_data)
}

# 5. DEFINE VARIABLE LABELS
covariate_labels <- list(
  "eduyears" = "Years of education",
  "age" = "Age",
  "valid.information.wjoint.income.wealth.poverty.bn.w2" = "Valid information",
  "POV.CLUST.Mainly_Non_poor" = "Mainly Non-poor Cluster",
  "POV.CLUST.Mainly_protected_poor" = "Mainly Protected Poor Cluster"
)

# 6. CREATE INDIVIDUAL TABLES FOR EACH DEPENDENT VARIABLE

# Twice Poor
twp_table <- create_pooled_table(
  list(twp_m1, twp_m2, twp_m3, twp_m4),
  covariate_labels,
  "Twice Poor",
  "text",
  "pooled_results_twice_poor.txt"
)

# HTML version
twp_table_html <- create_pooled_table(
  list(twp_m1, twp_m2, twp_m3, twp_m4),
  covariate_labels,
  "Twice Poor",
  "html",
  "pooled_results_twice_poor.html"
)

# Income Poor but Wealthy
ipw_table <- create_pooled_table(
  list(ipw_m1, ipw_m2, ipw_m3, ipw_m4),
  covariate_labels,
  "Income Poor but Wealthy",
  "text",
  "pooled_results_income_poor_wealthy.txt"
)

# HTML version
ipw_table_html <- create_pooled_table(
  list(ipw_m1, ipw_m2, ipw_m3, ipw_m4),
  covariate_labels,
  "Income Poor but Wealthy",
  "html",
  "pooled_results_income_poor_wealthy.html"
)

# Not Poor but No Wealth
npbnw_table <- create_pooled_table(
  list(npbnw_m1, npbnw_m2, npbnw_m3, npbnw_m4),
  covariate_labels,
  "Not Poor but No Wealth",
  "text",
  "pooled_results_not_poor_no_wealth.txt"
)

# HTML version
npbnw_table_html <- create_pooled_table(
  list(npbnw_m1, npbnw_m2, npbnw_m3, npbnw_m4),
  covariate_labels,
  "Not Poor but No Wealth",
  "html",
  "pooled_results_not_poor_no_wealth.html"
)

# Not Poor
np_table <- create_pooled_table(
  list(np_m1, np_m2, np_m3, np_m4),
  covariate_labels,
  "Not Poor",
  "text",
  "pooled_results_not_poor.txt"
)

# HTML version
np_table_html <- create_pooled_table(
  list(np_m1, np_m2, np_m3, np_m4),
  covariate_labels,
  "Not Poor",
  "html",
  "pooled_results_not_poor.html"
)

# 7. CREATE CONSOLIDATED TABLES WITH MORE FLEXIBLE HANDLING
# Function to create consolidated table with two outcome sets
create_consolidated_table <- function(models1, models2, title1, title2, covariate_labels = NULL, file_name = NULL) {
  # Filter out NULL models
  valid_models1 <- models1[!sapply(models1, is.null)]
  valid_models2 <- models2[!sapply(models2, is.null)]
  
  n_valid_models1 <- length(valid_models1)
  n_valid_models2 <- length(valid_models2)
  
  # Check if we have any valid models
  if (n_valid_models1 == 0 || n_valid_models2 == 0) {
    cat("Cannot create consolidated table - insufficient valid models\n")
    return(NULL)
  }
  
  # Keep track of which models were valid
  valid_indices1 <- which(!sapply(models1, is.null))
  valid_indices2 <- which(!sapply(models2, is.null))
  
  # Format coefficients for each valid model
  formatted_coefs1 <- list()
  formatted_coefs2 <- list()
  
  for (i in seq_along(valid_models1)) {
    model_summary <- valid_models1[[i]]$summary
    formatted_coef <- paste0(sprintf("%.2f", model_summary$OR), model_summary$stars)
    names(formatted_coef) <- rownames(model_summary)
    formatted_coefs1[[i]] <- formatted_coef
  }
  
  for (i in seq_along(valid_models2)) {
    model_summary <- valid_models2[[i]]$summary
    formatted_coef <- paste0(sprintf("%.2f", model_summary$OR), model_summary$stars)
    names(formatted_coef) <- rownames(model_summary)
    formatted_coefs2[[i]] <- formatted_coef
  }
  
  # Get all variables across all models
  all_vars <- unique(c(
    unlist(lapply(valid_models1, function(m) rownames(m$summary))),
    unlist(lapply(valid_models2, function(m) rownames(m$summary)))
  ))
  all_vars <- all_vars[all_vars != "(Intercept)"]  # Skip intercept
  
  # Create table structure for output
  table_data <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  
  # Add columns for each valid model
  for (i in seq_along(valid_indices1)) {
    col_name1 <- paste0(title1, "_Model", valid_indices1[i])
    table_data[[col_name1]] <- character()
  }
  
  for (i in seq_along(valid_indices2)) {
    col_name2 <- paste0(title2, "_Model", valid_indices2[i])
    table_data[[col_name2]] <- character()
  }
  
  # Fill in coefficient values
  for (var in all_vars) {
    row_data <- c(var)
    
    # Add coefficients for first outcome models
    for (i in seq_along(valid_models1)) {
      model_summary <- valid_models1[[i]]$summary
      if (var %in% rownames(model_summary)) {
        idx <- which(rownames(model_summary) == var)
        row_data <- c(row_data, formatted_coefs1[[i]][idx])
      } else {
        row_data <- c(row_data, "")
      }
    }
    
    # Add coefficients for second outcome models
    for (i in seq_along(valid_models2)) {
      model_summary <- valid_models2[[i]]$summary
      if (var %in% rownames(model_summary)) {
        idx <- which(rownames(model_summary) == var)
        row_data <- c(row_data, formatted_coefs2[[i]][idx])
      } else {
        row_data <- c(row_data, "")
      }
    }
    
    table_data[nrow(table_data) + 1, ] <- row_data
  }
  
  # Add AIC and BIC rows
  aic_row <- c("AIC")
  for (i in seq_along(valid_models1)) {
    aic_row <- c(aic_row, sprintf("%.2f", valid_models1[[i]]$AIC))
  }
  for (i in seq_along(valid_models2)) {
    aic_row <- c(aic_row, sprintf("%.2f", valid_models2[[i]]$AIC))
  }
  table_data[nrow(table_data) + 1, ] <- aic_row
  
  bic_row <- c("BIC")
  for (i in seq_along(valid_models1)) {
    bic_row <- c(bic_row, sprintf("%.2f", valid_models1[[i]]$BIC))
  }
  for (i in seq_along(valid_models2)) {
    bic_row <- c(bic_row, sprintf("%.2f", valid_models2[[i]]$BIC))
  }
  table_data[nrow(table_data) + 1, ] <- bic_row
  
  # Rename variables if covariate_labels provided
  if (!is.null(covariate_labels)) {
    for (i in 1:nrow(table_data)) {
      var_name <- table_data$Variable[i]
      if (var_name %in% names(covariate_labels)) {
        table_data$Variable[i] <- covariate_labels[[var_name]]
      } else if (startsWith(var_name, "highest_lifetime_ISCO_88_recoded")) {
        if (var_name == "highest_lifetime_ISCO_88_recoded1") {
          table_data$Variable[i] <- "Highly skilled occupation (ref. medium)"
        } else if (var_name == "highest_lifetime_ISCO_88_recoded3") {
          table_data$Variable[i] <- "Low skilled occupation"
        }
      } else if (startsWith(var_name, "cohort")) {
        table_data$Variable[i] <- paste0("Cohort: ", sub("^cohort", "", var_name))
      } else if (startsWith(var_name, "gender.rcd")) {
        table_data$Variable[i] <- paste0("Gender: ", sub("^gender.rcd", "", var_name))
      }
    }
  }
  
  # Header for consolidated table
  header <- paste0("Consolidated Table: ", title1, " & ", title2, " (Pooled Results after Multiple Imputation)")
  
  # Print the table
  cat("\n", header, "\n")
  cat(paste(rep("=", nchar(header)), collapse = ""), "\n")
  
  # Print header rows
  cat(sprintf("%-25s", "Variable"))
  cat(sprintf("%-40s", paste("    ", title1)))
  cat(sprintf("%-40s", paste("    ", title2)))
  cat("\n")
  
  cat(sprintf("%-25s", ""))
  for (i in seq_along(valid_indices1)) {
    cat(sprintf("%-10s", paste("Model", valid_indices1[i])))
  }
  for (i in seq_along(valid_indices2)) {
    cat(sprintf("%-10s", paste("Model", valid_indices2[i])))
  }
  cat("\n")
  
  # Print separator
  total_width <- 25 + 10*(n_valid_models1 + n_valid_models2)
  cat(paste(rep("-", total_width), collapse = ""), "\n")
  
  # Print rows
  for (i in 1:nrow(table_data)) {
    cat(sprintf("%-25s", table_data$Variable[i]))
    for (j in 2:(n_valid_models1 + n_valid_models2 + 1)) {
      cat(sprintf("%-10s", table_data[i, j]))
    }
    cat("\n")
  }
  
  # Print separator
  cat(paste(rep("=", total_width), collapse = ""), "\n")
  
  # Write to file if specified
  if (!is.null(file_name)) {
    sink(file_name)
    
    # Header for consolidated table
    cat("\n", header, "\n")
    cat(paste(rep("=", nchar(header)), collapse = ""), "\n")
    
    # Print header rows
    cat(sprintf("%-25s", "Variable"))
    cat(sprintf("%-40s", paste("    ", title1)))
    cat(sprintf("%-40s", paste("    ", title2)))
    cat("\n")
    
    cat(sprintf("%-25s", ""))
    for (i in seq_along(valid_indices1)) {
      cat(sprintf("%-10s", paste("Model", valid_indices1[i])))
    }
    for (i in seq_along(valid_indices2)) {
      cat(sprintf("%-10s", paste("Model", valid_indices2[i])))
    }
    cat("\n")
    
    # Print separator
    cat(paste(rep("-", total_width), collapse = ""), "\n")
    
    # Print rows
    for (i in 1:nrow(table_data)) {
      cat(sprintf("%-25s", table_data$Variable[i]))
      for (j in 2:(n_valid_models1 + n_valid_models2 + 1)) {
        cat(sprintf("%-10s", table_data[i, j]))
      }
      cat("\n")
    }
    
    # Print separator
    cat(paste(rep("=", total_width), collapse = ""), "\n")
    
    sink()
  }
  
  # Create HTML version if requested
  if (!is.null(file_name) && endsWith(file_name, ".html")) {
    html_file <- file_name
    
    library(knitr)
    
    # Create custom column names for display
    col_names <- c("Variable")
    for (i in seq_along(valid_indices1)) {
      col_names <- c(col_names, paste(title1, "Model", valid_indices1[i]))
    }
    for (i in seq_along(valid_indices2)) {
      col_names <- c(col_names, paste(title2, "Model", valid_indices2[i]))
    }
    
    colnames(table_data) <- col_names
    
    html_table <- kable(table_data, format = "html", caption = header) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      add_header_above(c(" " = 1, 
                         title1 = n_valid_models1, 
                         title2 = n_valid_models2))
    
    write(html_table, file = html_file)
  }
  
  return(table_data)
}

# Consolidated Table 1: Twice Poor & Income Poor but Wealthy
consolidated1 <- create_consolidated_table(
  list(twp_m1, twp_m2, twp_m3, twp_m4),
  list(ipw_m1, ipw_m2, ipw_m3, ipw_m4),
  "Twice Poor",
  "Income Poor but Wealthy",
  covariate_labels,
  "consolidated_table1_mi.txt"
)

# Consolidated Table 2: Not Poor but No Wealth & Not Poor
consolidated2 <- create_consolidated_table(
  list(npbnw_m1, npbnw_m2, npbnw_m3, npbnw_m4),
  list(np_m1, np_m2, np_m3, np_m4),
  "Not Poor but No Wealth",
  "Not Poor",
  covariate_labels,
  "consolidated_table2_mi.txt"
)

# Create HTML versions of consolidated tables
create_consolidated_table(
  list(twp_m1, twp_m2, twp_m3, twp_m4),
  list(ipw_m1, ipw_m2, ipw_m3, ipw_m4),
  "Twice Poor",
  "Income Poor but Wealthy",
  covariate_labels,
  "consolidated_table1_mi.html"
)

create_consolidated_table(
  list(npbnw_m1, npbnw_m2, npbnw_m3, npbnw_m4),
  list(np_m1, np_m2, np_m3, np_m4),
  "Not Poor but No Wealth",
  "Not Poor",
  covariate_labels,
  "consolidated_table2_mi.html"
)

# Print completion message
cat("\nMultiple imputation analysis complete. Results saved to text and HTML files.\n")