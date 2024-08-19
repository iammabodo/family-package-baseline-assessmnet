# Load necessary libraries
library(tidyverse)
library(survey)

# Set root folder paths
base <- "C:/Users/<username>/OneDrive - UNICEF/Cambodia Family Package"
indta <- file.path(base, "Baseline data/Final clean baseline data/Datasets")
out <- file.path(base, "Baseline data/Final data after field work/Output")

# Load the cover data
cover <- read_dta(file.path(indta, "Sections", "cover.dta"))

# Merge with the main dataset (replace <<datafile>> with your actual data file)
main_data <- read_dta(file.path(indta, "Indicator data", "<<datafile>>.dta"))

# Merge the datasets on household ID (hhid)
merged_data <- cover %>%
  left_join(main_data, by = "hhid") %>%
  drop_na()  # Remove rows with NA values resulting from the merge

# Create treatment indicators
merged_data <- merged_data %>%
  mutate(
    T = if_else(IDPOOR < 3, 1, 0),  # Treatment indicator based on IDPoor classification
    T2 = if_else(poorscore < 0, 1, 0)  # Treatment indicator based on poverty score
  )

# Survey design settings (cluster at the village level, control for strata)
svy_design <- svydesign(
  id = ~clusterid, 
  strata = ~strataid, 
  data = merged_data
)

# Create strata dummies
strata_dummies <- model.matrix(~ factor(strataid) - 1, data = merged_data)
colnames(strata_dummies) <- paste0("strata_", seq_along(unique(merged_data$strataid)))

# Add the strata dummies to the main data
merged_data <- bind_cols(merged_data, as_tibble(strata_dummies))

# List of covariates for baseline balance
covariates <- c("poorscore", paste0("strata_", 2:7))

# Output to verify the structure of the prepared dataset
glimpse(merged_data)

# Save the prepared data (optional)
write_csv(merged_data, file.path(out, "prepared_data.csv"))


# Function for creating balance tables


# Assume `merged_data` is your dataset with a treatment indicator, covariates, and survey design information.
# Set up survey design object
svy_design <- svydesign(
  id = ~clusterid,            # Cluster ID
  strata = ~strataid,         # Strata ID
  weights = ~weight,          # Sampling weights
  data = merged_data,
  nest = TRUE
)

# Function to create balance table for one covariate in survey data
create_survey_balance_table <- function(svy_design, covariate, treatment) {
  
  if (is.numeric(svy_design$variables[[covariate]])) {
    # Continuous variable balance
    summary_df <- svyby(~ get(covariate), ~ get(treatment), svy_design, svymean) %>%
      rename(
        control_mean = `get(covariate)[get(treatment)==0]`,
        treatment_mean = `get(covariate)[get(treatment)==1]`
      ) %>%
      mutate(
        diff = treatment_mean - control_mean,
        p_value = coef(svyttest(as.formula(paste(covariate, "~", treatment)), svy_design))['p-value'],
        smd = diff / sqrt((var(get(covariate)[get(treatment)==0], na.rm = TRUE) +
                             var(get(covariate)[get(treatment)==1], na.rm = TRUE)) / 2)
      )
    
  } else if (is.factor(svy_design$variables[[covariate]]) || is.character(svy_design$variables[[covariate]])) {
    # Categorical variable balance (proportions)
    prop_df <- svytable(~ get(covariate) + get(treatment), svy_design)
    prop_df <- prop_df / margin.table(prop_df, 2)  # Proportions
    
    summary_df <- as.data.frame(prop_df) %>%
      filter(get(covariate) == 1) %>%
      select(-get(covariate)) %>%
      spread(get(treatment), Freq) %>%
      mutate(
        diff = `1` - `0`,
        p_value = prop.test(x = c(prop_df[2,2], prop_df[2,1]),
                            n = c(sum(svy_design$variables[[treatment]] == 1),
                                  sum(svy_design$variables[[treatment]] == 0)),
                            correct = TRUE)$p.value,
        smd = diff / sqrt((`0` * (1 - `0`) + `1` * (1 - `1`)) / 2)
      ) %>%
      rename(
        treatment_prop = `1`,
        control_prop = `0`
      )
  }
  
  summary_df %>%
    mutate(covariate = covariate) %>%
    select(covariate, control_mean = control_prop, treatment_mean = treatment_prop, diff, p_value, smd)
}

# List of covariates to check for balance
covariates <- c("poorscore", "strata_2", "strata_3", "strata_4", "strata_5", "strata_6", "strata_7")


  





































