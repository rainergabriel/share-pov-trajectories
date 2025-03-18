# Load required libraries
library(mice)
library(mitools)
library(dplyr)
library(ggplot2)

# Load data  --------------------------------------------------------------
rm(list=ls())
load(file="data_step3-out-variables-all-setup.Rdata")

# STEP 0: Basic diagnostic information
cat("================ BASIC DATASET INFORMATION ================\n")
cat("Number of rows in dataset:", nrow(data), "\n")
cat("Number of columns in dataset:", ncol(data), "\n\n")

# Check if poverty cluster variables exist
pov_vars <- grep("POV.CLUST", names(data), value = TRUE)
cat("Found poverty cluster variables:", length(pov_vars), "\n")
if(length(pov_vars) > 0) {
  print(pov_vars)
  
  # Check the first poverty cluster variable
  first_pov <- pov_vars[1]
  cat("\nSummary of first poverty cluster variable:", first_pov, "\n")
  print(summary(data[[first_pov]]))
  
  # Check if it's binary
  if(is.factor(data[[first_pov]])) {
    cat("Variable is a factor with levels:", paste(levels(data[[first_pov]]), collapse=", "), "\n")
  } else if(all(na.omit(data[[first_pov]]) %in% c(0,1))) {
    cat("Variable appears to be binary (0/1)\n")
  } else {
    cat("Variable is not binary or factor. Unique values:", paste(unique(na.omit(data[[first_pov]])), collapse=", "), "\n")
  }
}

# Check predictor variables
cat("\nEducation years variable:\n")
print(summary(data$eduyears))

cat("\nISCO variable:\n")
print(summary(data$highest_lifetime_ISCO_88_recoded))
if(is.factor(data$highest_lifetime_ISCO_88_recoded)) {
  cat("ISCO levels:", paste(levels(data$highest_lifetime_ISCO_88_recoded), collapse=", "), "\n")
} else {
  cat("ISCO unique values:", paste(unique(na.omit(data$highest_lifetime_ISCO_88_recoded)), collapse=", "), "\n")
  cat("ISCO is not a factor - this may cause issues with the models\n")
}

# Missing data information
cat("\nMissing values in key variables:\n")
key_vars <- c("eduyears", "highest_lifetime_ISCO_88_recoded", pov_vars[1])
missing_data <- sapply(data[, key_vars, drop=FALSE], function(x) sum(is.na(x)))
print(missing_data)

# STEP 1: Minimal variable preparation -------------------------------------
cat("\n================ PREPARING VARIABLES ================\n")

# Try to convert ISCO to factor if it's not already
if(!is.factor(data$highest_lifetime_ISCO_88_recoded)) {
  cat("Converting ISCO to factor\n")
  # Check if it has expected values
  isco_values <- unique(na.omit(data$highest_lifetime_ISCO_88_recoded))
  
  if(all(isco_values %in% c("high", "medium", "low"))) {
    data$highest_lifetime_ISCO_88_recoded <- factor(
      data$highest_lifetime_ISCO_88_recoded,
      levels = c("high", "medium", "low")
    )
    cat("ISCO converted to factor with levels: high, medium, low\n")
  } else {
    cat("ISCO has unexpected values, using as-is\n")
    data$highest_lifetime_ISCO_88_recoded <- factor(data$highest_lifetime_ISCO_88_recoded)
  }
}

# STEP 2: Simple multiple imputation with minimal variables ---------------
cat("\n================ RUNNING MULTIPLE IMPUTATION ================\n")

# Select only essential variables for a minimal test
needed_vars <- c(
  pov_vars[1], # Just use the first poverty variable for testing
  "eduyears", 
  "highest_lifetime_ISCO_88_recoded"
)

# Print the selected variables
cat("Selected variables for imputation:\n")
print(needed_vars)

# Subset data
test_data <- data[, needed_vars, drop=FALSE]

# Print structure of test data
cat("\nStructure of test data:\n")
str(test_data)

# Run a simplified imputation with fewer iterations
cat("\nRunning simplified imputation...\n")
imp <- tryCatch({
  # Use only 3 imputations for testing
  mice(test_data, m = 3, method = "rf", printFlag = TRUE, seed = 123, maxit = 5)
}, error = function(e) {
  cat("ERROR during imputation:", e$message, "\n")
  return(NULL)
})

if(is.null(imp)) {
  cat("Imputation failed. Stopping analysis.\n")
  stop("Imputation failed")
}

cat("Imputation completed successfully\n")

# STEP 3: Run a single basic model ----------------------------------------
cat("\n================ FITTING BASIC MODEL ================\n")

# First, check what variables are in the imputed data
cat("Variables in the first imputed dataset:\n")
first_data <- complete(imp, 1)
print(names(first_data))

# Print the structure of the first imputed dataset
cat("\nStructure of first imputed dataset:\n")
str(first_data)

# Try to fit the most basic model possible
outcome_var <- pov_vars[1]
cat("\nFitting simple model with outcome:", outcome_var, "\n")

# Run model on each imputed dataset
basic_models <- tryCatch({
  with(imp, glm(as.formula(paste(outcome_var, "~ eduyears")), family = binomial))
}, error = function(e) {
  cat("ERROR fitting model:", e$message, "\n")
  return(NULL)
})

if(is.null(basic_models)) {
  cat("Model fitting failed.\n")
} else {
  cat("Model fitting successful. Pooling results...\n")
  
  # Pool results
  pooled_model <- tryCatch({
    pool(basic_models)
  }, error = function(e) {
    cat("ERROR pooling results:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(pooled_model)) {
    cat("Pooling failed.\n")
  } else {
    cat("Pooling successful. Extracting coefficients...\n")
    
    # Print the class of the pooled model
    cat("Class of pooled model:", class(pooled_model), "\n")
    
    # Try to get summary
    model_summary <- tryCatch({
      summary(pooled_model)
    }, error = function(e) {
      cat("ERROR getting summary:", e$message, "\n")
      return(NULL)
    })
    
    if(is.null(model_summary)) {
      cat("Could not get model summary.\n")
    } else {
      cat("Model summary obtained successfully.\n")
      cat("Structure of model summary:\n")
      str(model_summary)
      
      cat("\nColumn names in summary:\n")
      print(colnames(model_summary))
      
      cat("\nRow names in summary:\n")
      print(rownames(model_summary))
      
      cat("\nModel summary results:\n")
      print(model_summary)
      
      # Calculate odds ratios
      cat("\nOdds ratios:\n")
      odds_ratios <- exp(model_summary$estimate)
      print(odds_ratios)
      
      # Create a simple HTML table
      if(length(odds_ratios) > 0) {
        cat("\nCreating HTML table of results...\n")
        
        # Calculate CIs and p-values
        lower_ci <- exp(model_summary$estimate - 1.96 * model_summary$std.error)
        upper_ci <- exp(model_summary$estimate + 1.96 * model_summary$std.error)
        p_values <- model_summary$p.value
        
        # Create table HTML
        html <- "<table border='1'>\n"
        html <- paste0(html, "<tr><th>Variable</th><th>Odds Ratio</th><th>95% CI</th><th>p-value</th></tr>\n")
        
        for(i in 1:length(odds_ratios)) {
          var_name <- rownames(model_summary)[i]
          or <- sprintf("%.2f", odds_ratios[i])
          ci <- sprintf("%.2f - %.2f", lower_ci[i], upper_ci[i])
          p <- sprintf("%.4f", p_values[i])
          stars <- ""
          if(p_values[i] < 0.05) stars <- "*"
          if(p_values[i] < 0.01) stars <- "**"
          if(p_values[i] < 0.001) stars <- "***"
          
          html <- paste0(html, "<tr><td>", var_name, "</td><td>", or, "</td><td>", ci, "</td><td>", p, stars, "</td></tr>\n")
        }
        
        html <- paste0(html, "</table>")
        
        # Save to file
        writeLines(html, "basic_model_results.html")
        cat("Results saved to basic_model_results.html\n")
      }
      
      # Create a simple plot
      if(length(odds_ratios) > 1) { # Need at least 2 rows (intercept + variable)
        cat("\nCreating plot of odds ratios...\n")
        
        # Create data frame for plotting
        plot_data <- data.frame(
          Variable = rownames(model_summary),
          OR = odds_ratios,
          Lower = lower_ci,
          Upper = upper_ci,
          Significant = p_values < 0.05
        )
        
        # Remove intercept
        plot_data <- plot_data[plot_data$Variable != "(Intercept)",]
        
        if(nrow(plot_data) > 0) {
          # Create plot
          p <- ggplot(plot_data, aes(x = Variable, y = OR, ymin = Lower, ymax = Upper, fill = Significant)) +
            geom_col() +
            geom_errorbar(width = 0.2) +
            geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
            coord_flip() +
            scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "gray")) +
            labs(
              title = paste("Odds Ratios for", outcome_var),
              subtitle = "From Multiple Imputation Analysis",
              y = "Odds Ratio",
              x = ""
            ) +
            theme_minimal()
          
          # Save plot
          ggsave("basic_model_plot.png", p, width = 8, height = 6)
          cat("Plot saved to basic_model_plot.png\n")
        } else {
          cat("No variables to plot after removing intercept.\n")
        }
      }
    }
  }
}

# STEP 4: Try alternative approaches -------------------------------------
cat("\n================ TRYING ALTERNATIVE APPROACHES ================\n")

# Approach 1: Manual modeling on each imputed dataset
cat("\nApproach 1: Manual modeling on each imputed dataset\n")

# List to store results
manual_results <- list()

for(i in 1:imp$m) {
  cat("Processing imputation", i, "\n")
  
  # Get imputed dataset
  imp_data <- complete(imp, i)
  
  # Fit model
  fit <- tryCatch({
    glm(as.formula(paste(outcome_var, "~ eduyears")), family = binomial, data = imp_data)
  }, error = function(e) {
    cat("  ERROR fitting model:", e$message, "\n")
    return(NULL)
  })
  
  if(!is.null(fit)) {
    # Extract coefficients
    coefs <- tryCatch({
      coef(fit)
    }, error = function(e) {
      cat("  ERROR extracting coefficients:", e$message, "\n")
      return(NULL)
    })
    
    if(!is.null(coefs)) {
      cat("  Coefficients:", paste(names(coefs), "=", round(coefs, 4), collapse=", "), "\n")
      manual_results[[i]] <- coefs
    }
  }
}

# Manually pool the results if we have any
if(length(manual_results) > 0) {
  cat("\nManually pooling results from", length(manual_results), "imputations\n")
  
  # Find all coefficient names
  all_coef_names <- unique(unlist(lapply(manual_results, names)))
  
  # Create matrix for each coefficient
  pooled_coefs <- list()
  
  for(coef_name in all_coef_names) {
    # Extract this coefficient from all imputations
    coef_values <- sapply(manual_results, function(x) x[coef_name])
    
    # Calculate mean
    mean_value <- mean(coef_values, na.rm = TRUE)
    
    # Calculate standard error (simple approach)
    se_value <- sd(coef_values, na.rm = TRUE) / sqrt(sum(!is.na(coef_values)))
    
    # Store results
    pooled_coefs[[coef_name]] <- c(mean_value, se_value)
  }
  
  # Create a data frame of results
  manual_df <- data.frame(
    Variable = names(pooled_coefs),
    Estimate = sapply(pooled_coefs, function(x) x[1]),
    StdError = sapply(pooled_coefs, function(x) x[2]),
    OddsRatio = sapply(pooled_coefs, function(x) exp(x[1])),
    LowerCI = sapply(pooled_coefs, function(x) exp(x[1] - 1.96 * x[2])),
    UpperCI = sapply(pooled_coefs, function(x) exp(x[1] + 1.96 * x[2]))
  )
  
  cat("\nManually pooled results:\n")
  print(manual_df)
  
  # Save as CSV
  write.csv(manual_df, "manual_pooled_results.csv", row.names = FALSE)
  cat("Manually pooled results saved to manual_pooled_results.csv\n")
}

# Final message
cat("\nAnalysis complete. Please check the console output for errors and diagnostic information.\n")