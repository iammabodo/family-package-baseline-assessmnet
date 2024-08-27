# Load necessary libraries
library(tidyverse)
library(survey)
library(haven)

# Calculate Nutrition Indicators
calculate_proportions_and_ttest_nut <- function(design, outcome_var, group_var) {
  Sixto23 <- c("MDDCat", "MMF", "MMFF", "MAD", "PCMADUnhealthyFds")
  # Step 1: Calculate Proportions
  proportions <- svyby(
    as.formula(paste0("~ I(", outcome_var, " == 1)")), 
    as.formula(paste0("~", group_var)), 
    design, 
    svymean
  )
  
  # Step 2: Extract Proportions
  control_proportion <- proportions %>%
    filter(!!sym(group_var) == "Control Group") %>%
    pull(paste0("I(", outcome_var, " == 1)TRUE")) * 100
  
  treatment_proportion <- proportions %>%
    filter(!!sym(group_var) == "Treatment Group") %>%
    pull(paste0("I(", outcome_var, " == 1)TRUE")) * 100
  
  # Overall Proportion
  overall_proportion <- (control_proportion + treatment_proportion) / 2
  
  # Step 3: Perform T-test
  svy_ttest <- svyttest(as.formula(paste0("I(", outcome_var, " == 1) ~ ", group_var)), design = design)
  
  # Step 4: Extract Difference and P-value
  diff_proportion <- treatment_proportion - control_proportion
  p_value <- svy_ttest$p.value
  
  # Step 5: Combine Results
  results <- tibble(
    Indicator = outcome_var,
    Overall = overall_proportion,
    `Control Group` = control_proportion,
    `Treatment Group` = treatment_proportion,
    Difference = diff_proportion,
    `P-value` = p_value
  )
  
  return(results)
}



# Calculate Livelihoods Coping Strategies Indicators


  
library(srvyr)
library(dplyr)

calculate_proportions_and_ttest_cs <- function(svy_design, outcome_var, group_var) {
  
  # Step 1: Calculate Proportions Using `srvyr`
  proportions <- svy_design %>%
    group_by(!!sym(group_var)) %>%
    summarize(
      proportion = survey_mean(!!sym(outcome_var) == 1, na.rm = TRUE) * 100,
      .groups = 'drop'
    )
  
  # Step 2: Extract Proportions
  control_proportion <- proportions %>%
    filter(!!sym(group_var) == "Control Group") %>%
    pull(proportion)
  
  treatment_proportion <- proportions %>%
    filter(!!sym(group_var) == "Treatment Group") %>%
    pull(proportion)
  
  # Step 3: Perform T-test Using `svyttest`
  svy_ttest <- svyttest(outcome_var ~  group_var, design = svy_design)
  
  # Step 4: Extract Difference and P-value
  diff_proportion <- treatment_proportion - control_proportion
  p_value <- svy_ttest$p.value
  
  # Step 5: Combine Results
  results <- tibble(
    Indicator = outcome_var,
    `Control Group` = control_proportion,
    `Treatment Group` = treatment_proportion,
    Difference = diff_proportion,
    `P-value` = p_value
  )
  
  return(results)
}









calculate_proportions_and_ttest_cs(SvyLCSFS, "MaxcopingBehaviourFS", "Treatment")
