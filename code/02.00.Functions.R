# Required Libraries
library(survey)   # For survey-weighted data
library(dplyr)    # For data manipulation
library(gt)       # For table formatting
library(haven)    # To read Stata .dta files

# Set up paths (Replace with your actual paths)
base_path <- "C:/Users/username/OneDrive - UNICEF/Cambodia Family Package"
input_data_path <- paste0(base_path, "/Baseline data/Final clean baseline data/Datasets")
output_data_path <- paste0(base_path, "/Baseline data/Final data after field work/Output")

cover_data <- read_dta("new data/cover.dta")
MADChildren <- read_dta("clean data/MADChildren.dta")  # Data for children indicators

# Merge data on household ID (hhid)
MAD_merged_data <- merge(cover_data, MADChildren, by = c("hhid", "Province", "District", "Commune", "Village", "HHID"), all.x = TRUE) %>% 
  filter(!is.na(ChildAgeMonths))  # Remove rows with missing hhid

# Create treatment indicators
MAD_merged_data <- MAD_merged_data %>%
  mutate(T1 = ifelse(IDPOOR < 3, 1, 0),          # Treatment indicator based on IDPoor classification
         T2 = ifelse(poorscore < 0, 1, 0))       # Treatment indicator based on poverty score

# Ensure that strataid is treated as a factor before creating dummy variables
MAD_merged_data$strataid <- as.factor(MAD_merged_data$strataid)

# Generate dummy variables for strataid, excluding the first level as the baseline
strata_dummies <- model.matrix(~ strataid - 1, data = MAD_merged_data)

# Renaming columns to match Stata output (strata_1, strata_2, ...)
colnames(strata_dummies) <- paste0("strata_", 1:ncol(strata_dummies))

# Add the dummy variables back to the merged dataset
MAD_merged_data <- cbind(MAD_merged_data, strata_dummies)

# Specify covariates, including poverty score and actual strata dummies present in the data
existing_strata_vars <- grep("^strata_", colnames(MAD_merged_data), value = TRUE)
covariates <- c("poorscore", existing_strata_vars)

# Set up survey design (ensure 'clusterid' and 'strataid' exist in the data)
MAD_survey_design <- svydesign(
  ids = ~clusterid,   # Village level clustering
  strata = ~strataid, # Control for strata
  data = MAD_merged_data
)


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

# Subsetting data for specific indicators (e.g., children aged 6 months and above)

##################################################################################################
MAD_survey_design_subset <- subset(MAD_survey_design, ChildAgeMonths >= 6)
MAD_survey_design_U5 <- subset(MAD_survey_design, ChildAgeMonths <=5)
MAD_survey_design_subset_cbf <- subset(MAD_survey_design, ChildAgeMonths >= 12)

#################################################################################################
# Child Indicator Calculations
MAD <- calculate_balance("MAD", MAD_survey_design_subset, covariates)
CBF <- calculate_balance("PCMADBreastfeeding", MAD_survey_design_subset_cbf, covariates)
MMF <- calculate_balance("MMF", MAD_survey_design_subset, covariates)
MADEBF <- calculate_balance("MADEBF", MAD_survey_design_U5, covariates)
MADMixMF <- calculate_balance("MADMixMF", MAD_survey_design_U5, covariates)
BFAll <- calculate_balance("PCMADBreastfeeding", MAD_survey_design, covariates)
PMADEggFlesh <- calculate_balance("PMADEggFlesh", MAD_survey_design_subset, covariates)
PCMADUnhealthyFds <- calculate_balance("PCMADUnhealthyFds", MAD_survey_design_subset, covariates)
PMADSwtBeverages <- calculate_balance("PMADSwtBeverages", MAD_survey_design_subset, covariates)

# Merge the tables and round numerical values

child_nutrition_balance_results <- bind_rows(MAD, CBF, MMF, MADEBF, MADMixMF, BFAll, PMADEggFlesh, PMADSwtBeverages, PCMADUnhealthyFds) %>% 
  # Round numerical values to 3 decimal places
  mutate_if(is.numeric, ~round(., 3)) %>% 
  select(Indicator, Overall_Mean, Treatment_Mean, Comparison_Mean, Difference)
###############################################################################################
# Format and display the balance table using gt
balance_table <- child_nutrition_balance_results %>%
  gt() %>%
  tab_header(
    title = "Balance Table"
  ) %>%
  cols_label(
    Overall_Mean = "Overall Mean",
    Treatment_Mean = "Treatment Mean",
    Comparison_Mean = "Comparison Mean",
    Difference = "Difference",
  ) %>%
  fmt_number(
    columns = c(Overall_Mean, Treatment_Mean, Comparison_Mean, Difference),
    decimals = 3
  )

write.xlsx(balance_table, "report tables/ChildNutritionBalanceTable.xlsx")

