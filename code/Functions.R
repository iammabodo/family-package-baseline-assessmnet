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
