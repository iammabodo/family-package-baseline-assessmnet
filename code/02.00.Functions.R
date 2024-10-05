# Required Libraries
library(survey)   # For survey-weighted data
library(dplyr)    # For data manipulation
library(gt)       # For table formatting
library(haven)    # To read Stata .dta files
library(srvyr)    # To convert data to survey design


# Create function to calculate means, regressions, and p-values for each variable
calculate_balance <- function(y, survey_design, covariates) {
  # Calculate means
  mean_overall <- svymean(as.formula(paste("~", y)), design = survey_design)
  mean_treatment <- svymean(as.formula(paste("~", y)), design = subset(survey_design, T1 == 1))
  mean_comparison <- svymean(as.formula(paste("~", y)), design = subset(survey_design, T1 == 0))
  
  # Run regression with treatment and covariates
  regression_formula <- as.formula(paste(y, "~ factor(T1) +", paste(covariates, collapse = " + ")))  # Corrected from T to T1
  regression_result <- svyglm(regression_formula, design = survey_design)
  
  # Extract regression coefficient and p-value for treatment effect
  difference <- coef(regression_result)[2]  # Difference between treatment and control
  p_value <- summary(regression_result)$coefficients[2, 4]
  
  # Create data frame for results
  results <- data.frame(
    Indicator = y,
    Overall_Mean = coef(mean_overall),
    Treatment_Mean = coef(mean_treatment),
    Comparison_Mean = coef(mean_comparison),
    Difference = difference,
    p_value = p_value,
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    )
  )
  
  return(results)
}


# Load necessary library for ordered logistic regression
library(survey)
library(MASS)  # For svyolr function

# Function to calculate means, ordered logistic regressions, and p-values for each variable
calculate_balance_ordered <- function(y, survey_design, covariates) {
  
  # Convert dependent variable to an ordered factor in the survey design
  survey_design <- update(survey_design, y_factor = ordered(get(y)))  # Convert y to an ordered factor
  
  # Calculate means
  mean_overall <- svymean(as.formula(paste("~", y)), design = survey_design)
  mean_treatment <- svymean(as.formula(paste("~", y)), design = subset(survey_design, T1 == 1))
  mean_comparison <- svymean(as.formula(paste("~", y)), design = subset(survey_design, T1 == 0))
  
  # Run ordered logistic regression with treatment and covariates
  regression_formula <- as.formula(paste("y_factor ~ factor(T1) +", paste(covariates, collapse = " + ")))  # Use y_factor instead of y
  regression_result <- tryCatch({
    svyolr(regression_formula, design = survey_design)
  }, error = function(e) {
    warning("Regression failed due to a problem with the design.")
    return(NULL)
  })
  
  if (is.null(regression_result)) {
    # If regression failed, return NA for difference and p-value
    difference <- NA
    p_value <- NA
  } else {
    # Try to extract treatment effect if it exists
    coef_names <- names(coef(regression_result))
    
    if ("factor(T1)1" %in% coef_names) {
      difference <- coef(regression_result)["factor(T1)1"]  # Difference between treatment and control
      p_value <- summary(regression_result)$coefficients["factor(T1)1", "Pr(>|z|)"]  # P-value for treatment effect
    } else {
      difference <- NA  # Treatment effect not estimable
      p_value <- NA  # No p-value available
      warning("Treatment variable (T1) has been dropped due to rank-deficiency or other issues.")
    }
  }
  
  # Create data frame for results
  results <- data.frame(
    Indicator = y,
    Overall_Mean = coef(mean_overall),
    Treatment_Mean = coef(mean_treatment),
    Comparison_Mean = coef(mean_comparison),
    Difference = difference,
    p_value = p_value,
    stars = case_when(
      !is.na(p_value) & p_value < 0.01 ~ "***",
      !is.na(p_value) & p_value < 0.05 ~ "**",
      !is.na(p_value) & p_value < 0.1 ~ "*",
      TRUE ~ ""
    )
  )
  
  return(results)
}

