
# source necessary files

source("code/01.2.MDDDataCleaning.R")
source("code/01.1.FoodSecurityDataCleaning.R")
source("code/01.3.MADChildrenDataCleaning.R")
source("code/02.00.Functions.R")

# Cover data and MADChildren data
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


# Subsetting data for specific indicators (e.g., children aged 6 months and above)

##################################################################################################
MAD_survey_design_subset <- subset(MAD_survey_design, ChildAgeMonths >= 6)
MAD_survey_design_U5 <- subset(MAD_survey_design, ChildAgeMonths <=5)
MAD_survey_design_subset_cbf <- subset(MAD_survey_design, ChildAgeMonths >= 12)

#################################################################################################
# Child Indicator Calculations
MAD <- calculate_balance("MAD", MAD_survey_design_subset, covariates) # Minimum Acceptable Diet (MAD) for children aged 6-23 months
CBF <- calculate_balance("PCMADBreastfeeding", MAD_survey_design_subset_cbf, covariates) # Continued breastfeeding for children between 12-23 months
MMF <- calculate_balance("MMF", MAD_survey_design_subset, covariates) # Minimum Meal Frequency (MMF) for children aged 6-23 months
MADEBF <- calculate_balance("MADEBF", MAD_survey_design_U5, covariates) # Exclusive Breastfeeding for children under 5 months
MADMixMF <- calculate_balance("MADMixMF", MAD_survey_design_U5, covariates) # Mixed Milk Feeding for children under 5 months
BFAll <- calculate_balance("PCMADBreastfeeding", MAD_survey_design, covariates) # Continued Breastfeeding for all children aged 0-23 months
PMADEggFlesh <- calculate_balance("PMADEggFlesh", MAD_survey_design_subset, covariates) # Egg and Flesh consumption for children aged 6-23 months
PCMADUnhealthyFds <- calculate_balance("PCMADUnhealthyFds", MAD_survey_design_subset, covariates) # Unhealthy food consumption for children aged 6-23 months
PMADSwtBeverages <- calculate_balance("PMADSwtBeverages", MAD_survey_design_subset, covariates) # Sweetened beverage consumption for children aged 6-23 months

# Merge the tables and round numerical values

child_nutrition_balance_results <- bind_rows(MAD, CBF, MMF, MADEBF, MADMixMF, BFAll, PMADEggFlesh, PMADSwtBeverages, PCMADUnhealthyFds) %>% 
  # Round numerical values to 3 decimal places
  mutate_if(is.numeric, ~round(., 3)) %>% 
  select(Indicator, Overall_Mean, Treatment_Mean, Comparison_Mean, Difference) # Removing the p-value and stars columns since non of the indicators are significant

###############################################################################################
# Format and display the balance table using gt
Child_nutrition_balance_table <- child_nutrition_balance_results %>%
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


###############################################################################################

# Adult Nutrition Indicators

# Read the adult nutrition data

AdultNutrition <- read_dta("clean data/DietQuality.dta") %>% 
  select(-c(Province, District, Commune, Village))  # Remove location columns

# Merge the adult nutrition data with the cover data

Adult_merged_data <- left_join(cover_data, AdultNutrition, by = c("hhid", "HHID", "IDPOOR")) %>% 
  mutate(T1 = ifelse(IDPOOR < 3, 1, 0),          # Treatment indicator based on IDPoor classification
         T2 = ifelse(poorscore < 0, 1, 0)) %>% 
  #mutate_at(vars(starts_with("strata")), as.factor)  # Ensure strata variables are treated as factors
  mutate(strataid = as.factor(strataid))  # Ensure strataid is treated as a factor



# Generate dummy variables for strataid, excluding the first level as the baseline
strata_dummies_adult_nutrition <- model.matrix(~ strataid - 1, data = Adult_merged_data)

# Renaming columns to match Stata output (strata_1, strata_2, ...)
colnames(strata_dummies_adult_nutrition) <- paste0("strata_", 1:ncol(strata_dummies_adult_nutrition))

# Add the dummy variables back to the merged dataset
Adult_merged_data <- cbind(Adult_merged_data, strata_dummies_adult_nutrition)

# Specify covariates, including poverty score and actual strata dummies present in the data
existing_strata_vars_adult_nutrition <- grep("^strata_", colnames(Adult_merged_data), value = TRUE)

covariates_adult_nut <- c("poorscore", existing_strata_vars_adult_nutrition)


# Set up survey design (ensure 'clusterid' and 'strataid' exist in the data)

Adult_survey_design <- svydesign(
  ids = ~clusterid,   # Village level clustering
  strata = ~strataid, # Control for strata
  data = Adult_merged_data
)


# Subsetting data for specific indicators (e.g., adults aged 18-59 years)

# Convert the gender variable to numeric

Adult_merged_data$MDDGender <- as.numeric(Adult_merged_data$MDDGender)

Adult_survey_design_women <- subset(Adult_survey_design, MDDAge >= 15 & MDDAge <= 49 & MDDGender == 2)


MDDWomen <- calculate_balance("MDDCategory", Adult_survey_design_women, covariates_adult_nut) # Minimum Dietary Diversity (MDD) for women of reproductive age
MDDScore <- calculate_balance("MDDScore", Adult_survey_design, covariates_adult_nut) # MDD Score for all adults aged 18-59 years
MDDSweetBeverages <- calculate_balance("MDDSweetBeverages", Adult_survey_design, covariates_adult_nut) # Sweetened beverage consumption for all adults aged 18-59 years
MDDUnhealthyFoods <- calculate_balance("MDDUnhealthyFoods", Adult_survey_design, covariates_adult_nut) # Unhealthy food consumption for all adults aged 18-59 years
MDDStaples <- calculate_balance("MDDStaples", Adult_survey_design, covariates_adult_nut) # Staple food consumption for all adults aged 18-59 years
MDDPulses <- calculate_balance("MDDPulses", Adult_survey_design, covariates_adult_nut) # Pulses consumption for all adults aged 18-59 years
MDDNutsSeeds <- calculate_balance("MDDNutsSeeds", Adult_survey_design, covariates_adult_nut) # Nuts and seeds consumption for all adults aged 18-59 years
MDDMilkDairy <- calculate_balance("MDDDiary", Adult_survey_design, covariates_adult_nut) # Milk and dairy consumption for all adults aged 18-59 years
MDDProtein <- calculate_balance("MDDProtein", Adult_survey_design, covariates_adult_nut) # Protein consumption for all adults aged 18-59 years
MDDEggs <- calculate_balance("MDDEggs", Adult_survey_design, covariates_adult_nut) # Egg consumption for all adults aged 18-59 years
MDDDarkGreenVeg <- calculate_balance("MDDDarkGreenVeg", Adult_survey_design, covariates_adult_nut) # Dark green vegetable consumption for all adults aged 18-59 years
MDDOtherVeg <- calculate_balance("MDDOtherVeg", Adult_survey_design, covariates_adult_nut) # Other vegetable consumption for all adults aged 18-59 years
MDDOtherVitA <- calculate_balance("MDDOtherVitA", Adult_survey_design, covariates_adult_nut) # Other Vitamin A-rich food consumption for all adults aged 18-59 years
MDDOtherFruits <- calculate_balance("MDDOtherFruits", Adult_survey_design, covariates_adult_nut) # Other fruit consumption for all adults aged 18-59 years

# Merge the tables and round numerical values

adult_nutrition_balance_results <- bind_rows(MDDWomen, MDDScore, MDDSweetBeverages, MDDUnhealthyFoods, 
                                             MDDStaples, MDDPulses, MDDNutsSeeds, MDDMilkDairy, 
                                             MDDProtein, MDDEggs, MDDDarkGreenVeg, MDDOtherVeg, MDDOtherVitA, MDDOtherFruits) %>% 
  # Round numerical values to 3 decimal places
  mutate_if(is.numeric, ~round(., 3)) %>% 
  select(Indicator, Overall_Mean, Treatment_Mean, Comparison_Mean, Difference) # Removing the p-value and stars columns since non of the indicators are significant

# Write excel file
write.xlsx(adult_nutrition_balance_results, "report tables/AdultNutritionBalanceTable.xlsx")

########################################################################################

# Livelihoods Coping Strategies Essential Needs (LCSEN) Indicators

# Read the LCSEN data
LCSEN <- read_dta("clean data/LCSEN.dta") %>% 
  dplyr::select(hhid,  MaxcopingBehaviourEN) %>% 
  pivot_wider(names_from = MaxcopingBehaviourEN, values_from = MaxcopingBehaviourEN) %>% 
  rename(StressCopingEn = `2`,
         CrisisCopingEn = `3`,
         EmergencyCopingEn = `4`,
         NoCopingEN = `1`) %>% 
  mutate(NoCopingEN = case_when(
    !is.na(NoCopingEN) ~ 1,
    TRUE ~ 0),
    StressCopingEn = case_when(
      !is.na(StressCopingEn) ~ 1,
      TRUE ~ 0),
    CrisisCopingEn = case_when(
      !is.na(CrisisCopingEn) ~ 1,
      TRUE ~ 0),
    EmergencyCopingEn = case_when(
      !is.na(EmergencyCopingEn) ~ 1,
      TRUE ~ 0))

# Merge the LCSEN data with the cover data
LCSEN_merged_data <- left_join(cover_data, LCSEN, by = c("hhid")) %>% 
  mutate(T1 = ifelse(IDPOOR < 3, 1, 0),          # Treatment indicator based on IDPoor classification
         T2 = ifelse(poorscore < 0, 1, 0)) %>% 
  #mutate_at(vars(starts_with("strata")), as.factor)  # Ensure strata variables are treated as factors
  mutate(strataid = as.factor(strataid))  # Ensure strataid is treated as a factor

# Generate dummy variables for strataid, excluding the first level as the baseline
strata_dummies_LCSEN <- model.matrix(~ strataid - 1, data = LCSEN_merged_data)

# Renaming columns to match Stata output (strata_1, strata_2, ...)
colnames(strata_dummies_LCSEN) <- paste0("strata_", 1:ncol(strata_dummies_LCSEN))

# Add the dummy variables back to the merged dataset

LCSEN_merged_data <- cbind(LCSEN_merged_data, strata_dummies_LCSEN)

# Specify covariates, including poverty score and actual strata dummies present in the data

existing_strata_vars_LCSEN <- grep("^strata_", colnames(LCSEN_merged_data), value = TRUE)

covariates_LCSEN <- c("poorscore", existing_strata_vars_LCSEN)

# Set up survey design (ensure 'clusterid' and 'strataid' exist in the data)


LCSEN_survey_design <- svydesign(
  ids = ~clusterid,   # Village level clustering
  strata = ~strataid, # Control for strata
  data = LCSEN_merged_data
)


# Indicators for LCSEN


LCSEN_Stress <- calculate_balance("StressCopingEn", LCSEN_survey_design, covariates_LCSEN) # Food consumption for LCSEN
LCSEN_Crisis <- calculate_balance("CrisisCopingEn", LCSEN_survey_design, covariates_LCSEN) # Food consumption for LCSEN
LCSEN_Emergency <- calculate_balance("EmergencyCopingEn", LCSEN_survey_design, covariates_LCSEN) # Food consumption for LCSEN
LCSEN_NoCoping <- calculate_balance("NoCopingEN", LCSEN_survey_design, covariates_LCSEN) # Food consumption for LCSEN

# Merge the tables and round numerical values

LCSEN_balance_results <- bind_rows(LCSEN_Stress, LCSEN_Crisis, LCSEN_Emergency, LCSEN_NoCoping) %>% 
  # Round numerical values to 3 decimal places
  mutate_if(is.numeric, ~round(., 3)) %>% 
  dplyr::select(Indicator, Overall_Mean, Treatment_Mean, Comparison_Mean, Difference) # Removing the p-value and stars columns since non of the indicators are significant


###############################################################################################

# Reduced coping strategies index balance table

# Read the reduced coping strategies index data

ReducedCopingStrategies <- read_dta("clean data/rCSIData.dta") %>% 
  mutate(
    rCSILessQltyCat = case_when(
      rCSILessQltyCat ==1 ~ 1,
      TRUE ~ 0
    ),
    rCSIBorrowCat = case_when(
      rCSIBorrowCat ==1 ~ 1,
      TRUE ~ 0
    ),
    rCSIMealSizeCat = case_when(
      rCSIMealSizeCat ==1 ~ 1,
      TRUE ~ 0
    ),
    rCSIMealNbCat = case_when(
      rCSIMealNbCat ==1 ~ 1,
      TRUE ~ 0
    ),
    rCSIMealAdultCat = case_when(
      rCSIMealAdultCat ==1 ~ 1,
      TRUE ~ 0))

# Merge the reduced coping strategies index data with the cover data

ReducedCopingStrategies_merged_data <- left_join(cover_data, ReducedCopingStrategies, by = c("hhid")) %>% 
  mutate(T1 = ifelse(IDPOOR < 3, 1, 0),          # Treatment indicator based on IDPoor classification
         T2 = ifelse(poorscore < 0, 1, 0)) %>% 
  #mutate_at(vars(starts_with("strata")), as.factor)  # Ensure strata variables are treated as factors
  mutate(strataid = as.factor(strataid))  # Ensure strataid is treated as a factor

# Generate dummy variables for strataid, excluding the first level as the baseline

strata_dummies_reduced_coping_strategies <- model.matrix(~ strataid - 1, 
                                                         data = ReducedCopingStrategies_merged_data)


# Renaming columns to match Stata output (strata_1, strata_2, ...)

colnames(strata_dummies_reduced_coping_strategies) <- paste0("strata_", 1:ncol(strata_dummies_reduced_coping_strategies))

# Add the dummy variables back to the merged dataset

ReducedCopingStrategies_merged_data <- cbind(ReducedCopingStrategies_merged_data, strata_dummies_reduced_coping_strategies)

# Specify covariates, including poverty score and actual strata dummies present in the data

existing_strata_vars_reduced_coping_strategies <- grep("^strata_", colnames(ReducedCopingStrategies_merged_data), value = TRUE)


covariates_reduced_coping_strategies <- c("poorscore", 
                                          existing_strata_vars_reduced_coping_strategies)

# Set up survey design (ensure 'clusterid' and 'strataid' exist in the data)

ReducedCopingStrategies_survey_design <- svydesign(
  ids = ~clusterid,   # Village level clustering
  strata = ~strataid, # Control for strata
  data = ReducedCopingStrategies_merged_data
)


# Indicators for Reduced Coping Strategies Index
 
# a. rCSI (Reduced Coping Strategies Index)
rCSI <- calculate_balance("rCSI", ReducedCopingStrategies_survey_design, covariates_reduced_coping_strategies) # Reduced Coping Strategies Index

# b. rCSILessQlty

rCSILessQltyCat <- calculate_balance("rCSILessQltyCat", 
                                     ReducedCopingStrategies_survey_design, 
                                     covariates_reduced_coping_strategies) # Reduced Coping Strategies Index

# c. rCSIBorrowCat

rCSIBorrowCat <- calculate_balance("rCSIBorrowCat", 
                                   ReducedCopingStrategies_survey_design, 
                                   covariates_reduced_coping_strategies) # Reduced Coping Strategies Index

# d. rCSIMealSizeCat

rCSIMealSizeCat <- calculate_balance("rCSIMealSizeCat", 
                                     ReducedCopingStrategies_survey_design, 
                                     covariates_reduced_coping_strategies) # Reduced Coping Strategies Index

# e. rCSIMealNbCat

rCSIMealNbCat <- calculate_balance("rCSIMealNbCat", 
                                   ReducedCopingStrategies_survey_design, 
                                   covariates_reduced_coping_strategies) # Reduced Coping Strategies Index


# f. rCSIMealAdultCat

rCSIMealAdultCat <- calculate_balance("rCSIMealAdultCat", 
                                      ReducedCopingStrategies_survey_design, 
                                      covariates_reduced_coping_strategies) # Reduced Coping Strategies Index



# Merge the tables and round numerical values

reduced_coping_strategies_balance_results <- bind_rows(rCSI, rCSILessQltyCat, rCSIBorrowCat, 
                                                       rCSIMealSizeCat, rCSIMealNbCat, rCSIMealAdultCat) %>% 
  # Round numerical values to 3 decimal places
  mutate_if(is.numeric, ~round(., 3)) %>% 
  dplyr::select(Indicator, Overall_Mean, Treatment_Mean, Comparison_Mean, Difference) # Removing the p-value and stars columns since non of the indicators are significant




