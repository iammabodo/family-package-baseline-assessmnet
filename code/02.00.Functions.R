# Required Libraries
library(survey)   # For survey-weighted data
library(dplyr)    # For data manipulation
library(gt)       # For table formatting
library(haven)    # To read Stata .dta files


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
