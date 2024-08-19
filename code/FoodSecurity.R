library(tidyverse)
library(haven)
library(labelled)
library(survey)
library(RM.weights)


FoodSecurity <- read_dta("data/1. UNICEF_FPBaseline_Main_V26_FINAL.dta") %>% 
  select(interview__key, interview__id, Province, District, Commune, Village, HHID,
         IDPOOR, equitycardno, householduuid, Sex, starts_with("S8_"))


# Livelihoods Copying Strategies Data

LCSEN <- FoodSecurity %>% 
  select(interview__key, interview__id, Province, District, Commune, Village, HHID,
         IDPOOR, equitycardno, householduuid, Sex, 
         # Livelihoods Copying Strategies Variables
         S8_4a:S8_4k_OTHER) %>% 
  # Rename the variables
  rename(
    LcsENStressDomAsset = S8_4a,
    LcsENStressSaving = S8_4d,
    LcsENStressBorrowCash = S8_4e,
    LcsENStressHHSeparation = S8_4i,
    LcsENCrisisProdAssets = S8_4b,
    LcsENCrisisHealth = S8_4c,
    LcsENCrisisOutSchool = S8_4g,
    LcsENEmResAssets = S8_4f,
    LcsENEmBegged = S8_4j,
    LcsENEmIllegalAct = S8_4h,
    LCSENEngaged = S8_4k__0,
    LCSENEngagedFood = S8_4k__1,
    LCSENEngagedShelter = S8_4k__2,
    LCSENEngagedEducation = S8_4k__3,
    LCSENEngagedHealth = S8_4k__4,
    LCSENEngagedNonFood = S8_4k__5,
    LCSENEngagedHygiene = S8_4k__6,
    LCSENEngagedEssentialSvcs = S8_4k__7,
    LCSENEngagedDebtRepay = S8_4k__8,
    LCSENEngagedOther = S8_4k_OTHER_EN) %>% 
  # Change labelled variables to factor variables
  mutate(
    Province = as_factor(Province),
    District = as_factor(District),
    Commune = as_factor(Commune),
    Village = as_factor(Village),
    Sex = as_factor(Sex),
    IDPOOR = as_factor(IDPOOR),
    LcsENStressDomAsset = as_factor(LcsENStressDomAsset),
    LcsENStressSaving = as_factor(LcsENStressSaving),
    LcsENStressBorrowCash = as_factor(LcsENStressBorrowCash),
    LcsENStressHHSeparation = as_factor(LcsENStressHHSeparation),
    LcsENCrisisProdAssets = as_factor(LcsENCrisisProdAssets),
    LcsENCrisisHealth = as_factor(LcsENCrisisHealth),
    LcsENCrisisOutSchool = as_factor(LcsENCrisisOutSchool),
    LcsENEmResAssets = as_factor(LcsENEmResAssets),
    LcsENEmBegged = as_factor(LcsENEmBegged),
    LcsENEmIllegalAct = as_factor(LcsENEmIllegalAct)) %>%
  #Change the variables LCSENEngaged to LCSENEngagedDebtRepay to factor variables with Yes or No labels
  mutate_at(
    vars(LCSENEngaged:LCSENEngagedDebtRepay),
    ~ factor(., levels = c(1, 0), labels = c("Yes", "No"))) %>%
  # Add variable labels
  set_variable_labels(
    LcsENStressDomAsset = "Sold household assets/goods (radio, furniture, television, jewellery etc.) to meet essential needs",
    LcsENStressSaving = "Spent savings to meet essential needs",
    LcsENStressBorrowCash = "Borrowed money to meet essential needs",
    LcsENStressHHSeparation = "seperate with household members to meet essential needs",
    LcsENCrisisProdAssets = "Sold productive assets or means of transport (sewing machine, wheelbarrow, bicycle, car, etc.) to meet essential needs",
    LcsENCrisisHealth = "Reduced expenses on health (including drugs) to meet other essential needs",
    LcsENCrisisOutSchool = "Withdrew children from school to meet essential needs",
    LcsENEmResAssets = "Mortgaged/Sold house that the household was permanently living in or sold land to meet essential needs",
    LcsENEmBegged = "Begged and/or scavenged (asked strangers for money/food) to meet essential needs",
    LcsENEmIllegalAct = "Engaged in socially degrading, high risk, or exploitive jobs, or life-threatening income activities (e.g., smuggling, theft, join armed groups, prostitution) to meet essential needs",
    LCSENEngaged = "Engaged",
    LCSENEngagedFood = "Engaged in coping strategies to buy food",
    LCSENEngagedShelter = "Engaged in coping strategies to pay for rent or access adequate shelter",
    LCSENEngagedEducation = "Engaged  in coping strategies to pay for school fees and other education expenses",
    LCSENEngagedHealth = "Engaged in coping strategies to cover health expenses",
    LCSENEngagedNonFood = "Engaged in coping strategies to buy essential non-food items (e.g., clothes, small furniture, etc.)",
    LCSENEngagedHygiene = "Engaged in coping strategies to access water or sanitation facilities",
    LCSENEngagedEssentialSvcs = "Engaged in coping strategies to access essential services (electricity, energy, waste disposal, etc.)",
    LCSENEngagedDebtRepay = "Engaged in coping strategies to pay for existing debts or loans",
    LCSENEngagedOther = "Engaged in coping strategies for other reasons") %>% 
  # Mutate the variables to specify if the household used any of the coping strategies by severity
  mutate(
    # Stress Coping Strategies
    StressCopingEn = case_when(
      LcsENStressDomAsset == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" | 
        LcsENStressDomAsset == "Yes" ~ 1,
      LcsENStressSaving == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" | 
        LcsENStressSaving == "Yes" ~ 1,
      LcsENStressBorrowCash == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" | 
        LcsENStressBorrowCash == "Yes" ~ 1,
      LcsENStressHHSeparation == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" | 
        LcsENStressHHSeparation == "Yes" ~ 1,
      TRUE ~ 0),
    # Crisis Coping Strategies
    CrisisCopingEn = case_when(
      LcsENCrisisProdAssets == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" | 
        LcsENCrisisProdAssets == "Yes" ~ 1,
      LcsENCrisisHealth == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" | 
        LcsENCrisisHealth == "Yes" ~ 1,
      LcsENCrisisOutSchool == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" | 
        LcsENCrisisOutSchool == "Yes" ~ 1,
      TRUE ~ 0),
    # Emergency Coping Strategies
    EmergencyCopingEn = case_when(
      LcsENEmResAssets == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" | 
        LcsENEmResAssets == "Yes" ~ 1,
      LcsENEmBegged == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" | 
        LcsENEmBegged == "Yes" ~ 1,
      LcsENEmIllegalAct == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" | 
        LcsENEmIllegalAct == "Yes" ~ 1,
      TRUE ~ 0)) %>%
  # Mutate maximum copind behaviour variable 
  mutate(
    MaxcopingBehaviourEN = case_when(
      EmergencyCopingEn == 1 ~ 4,
      CrisisCopingEn == 1 ~ 3,
      StressCopingEn == 1 ~ 2,
      TRUE ~ 1),
    MaxcopingBehaviourEN = factor(
      MaxcopingBehaviourEN,
      levels = c(1, 2, 3, 4),
      labels = c("Household not adopting coping strategies", 
                 "Stress coping strategies", 
                 "Crisis coping strategies", 
                 "Emergency coping strategies"))) %>%
  # Label StressCopingEn, CrisisCopingEn, and EmergencyCopingEn and chage them to factor variables
  mutate(
    StressCopingEn = factor(StressCopingEn, levels = c(1, 0), labels = c("Yes", "No")),
    CrisisCopingEn = factor(CrisisCopingEn, levels = c(1, 0), labels = c("Yes", "No")),
    EmergencyCopingEn = factor(EmergencyCopingEn, levels = c(1, 0), labels = c("Yes", "No"))) %>% 
  # Add variable labels to StressCopingEn, CrisisCopingEn, and EmergencyCopingEn
  set_variable_labels(
    StressCopingEn = "Did the household engaged in stress coping strategies",
    CrisisCopingEn = "Did the household engaged in crisis coping strategies",
    EmergencyCopingEn = "Did the household engaged in emergency coping strategies",
    MaxcopingBehaviourEN = "Summary of asset depletion") %>%
  #Create treatement variable using the IDPOOR Variable
  mutate(
    Treatment = case_when(
      IDPOOR == "POOR_1" | IDPOOR == "POOR_2" ~ "Treatment Group",
      IDPOOR == "NEAR_POOR" ~ "Control Group",
      TRUE ~ "Missing"),
    Treatment  = factor(Treatment)) %>%
  # Set variable labels for the Treatment variable
  set_variable_labels(
    Treatment = "Family Package Treatment Group or Control Group")
  

###########################################################################################################################################################################

# Livelihoods Copying Strategies Indicator Calculation

# Calculate the percentage of people using each coping strategy (by using MaxcopingBehaviourEN variable), disaggregated by treatment and control groups
LCSENMainIndicators <- LCSEN %>% 
  count(Treatment, MaxcopingBehaviourEN) %>% 
  group_by(Treatment) %>%
  mutate(percentage = n / sum(n) * 100)

LCSENFoodCons <- LCSEN %>% 
  filter(LCSENEngagedFood == "Yes") %>%
  count(Treatment, MaxcopingBehaviourEN) %>% 
  group_by(Treatment) %>%
  mutate(percentage = n / sum(n) * 100)

# Percentage of households enaged in stress coping strategies

LCSENStressCopingEn <- LCSEN %>%
  count(Treatment, StressCopingEn, Province) %>%
  group_by(Province, Treatment) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(StressCopingEn == "Yes")

# Percentage of households enaged in crisis coping strategies

LCSENCrisisCopingEn <- LCSEN %>%
  count(Treatment, CrisisCopingEn, Province) %>%
  group_by(Province, Treatment) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(CrisisCopingEn == "Yes")

# Percentage of households enaged in emergency coping strategies  
LCSENEmergencyCopingEn <- LCSEN %>%
  count(Treatment, EmergencyCopingEn, Province) %>%
  group_by(Province, Treatment) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(EmergencyCopingEn == "Yes")

# Percentage of households engaged in stress coping to access food strategies by province
LCSENFoodConsStress <- LCSEN %>%
  filter(LCSENEngagedFood == "Yes") %>%
  count(Treatment, StressCopingEn, Province) %>%
  group_by(Province, Treatment) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(StressCopingEn == "Yes")

# Percentage of households engaged in crisis coping to access food strategies by province
LCSENFoodConsCrisis <- LCSEN %>%
  filter(LCSENEngagedFood == "Yes") %>%
  count(Treatment, CrisisCopingEn, Province) %>%
  group_by(Province, Treatment) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(CrisisCopingEn == "Yes")

# Percentage of households engaged in emergency coping to access food strategies by province
LCSENFoodConsEmergency <- LCSEN %>%
  filter(LCSENEngagedFood == "Yes") %>%
  count(Treatment, EmergencyCopingEn, Province) %>%
  group_by(Province, Treatment) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(EmergencyCopingEn == "Yes")

##################################################################################################################################################################


# REDUCED COPING STRATEGIES INDICATORS CALCULATION 


rCSIData <- FoodSecurity %>% 
  select(interview__key, interview__id, Province, District, Commune, Village, HHID,
         IDPOOR, equitycardno, householduuid, Sex, S8_5:S8_9b) %>% 
  # Rename Variables
  rename(
    rCSILessQlty = S8_5,
    rCSIBorrow = S8_6a,
    rCSIMealSize = S8_7a,
    rCSIMealSizeWho = S8_7b,
    rCSIMealNb = S8_9a,
    rCSIMealNbWho = S8_9b,
    rCSIMealAdult = S8_8a,
    rCSIMealAdultWho = S8_8b) %>%
  # Change rCSIMealSizeWho,  rCSIMealAdultWho and rCSIMealNbWho to factor variables
  mutate(
    rCSIMealSizeWho = as_factor(rCSIMealSizeWho),
    rCSIMealAdultWho = as_factor(rCSIMealAdultWho),
    rCSIMealNbWho = as_factor(rCSIMealNbWho)) %>%
  # Change "Skipped" values to NA
  mutate_if(is.factor, ~ na_if(., "Skipped")) %>%
  # Set variable labels
  set_variable_labels(
    rCSILessQlty = "Relied on less preffered, less expensive food",
    rCSIBorrow = "Borrowed food or relied on help from friends or relatives",
    rCSIMealSize = "Reduced portion size of meals",
    rCSIMealSizeWho = "Who in the household reduced the size of meals",
    rCSIMealNb = "Reduced the number of meals eaten per day",
    rCSIMealNbWho = "Who in the household reduced the number of meals",
    rCSIMealAdult = "Restricted consumption by adults in order for small children to eat",
    rCSIMealAdultWho = "Which adult gender mainly reduced or restricted consumption") %>% 
  # Change labelled variables to factor variables
  mutate(
    Province = as_factor(Province),
    District = as_factor(District),
    Commune = as_factor(Commune),
    Village = as_factor(Village),
    IDPOOR = as_factor(IDPOOR),
    Sex = as_factor(Sex)) %>% 
  # Create reduced Coping strategies indicator
  mutate(rCSI = rCSILessQlty +
           (rCSIBorrow * 2) +
           rCSIMealSize +
           rCSIMealNb +
           (rCSIMealAdult * 3)) %>%
  # Create new factor variables for  rCSILessQlty, rCSIBorrow, rCSIMealSize, rCSIMealNb, and rCSIMealAdult, with Yes if the value is greater than 0 and No if the value is 0
  mutate(
    rCSILessQltyCat = case_when(
      rCSILessQlty > 0 ~ 1,
      TRUE ~ 0),
    rCSIBorrowCat = case_when(
      rCSIBorrow > 0 ~ 1,
      TRUE ~ 0),
    rCSIMealSizeCat = case_when(
      rCSIMealSize > 0 ~ 1,
      TRUE ~ 0),
    rCSIMealNbCat = case_when(
      rCSIMealNb > 0 ~ 1,
      TRUE ~ 0),
    rCSIMealAdultCat = case_when(
      rCSIMealAdult > 0 ~ 1,
      TRUE ~ 0)) %>%
  # Change these variables to factor variables, with 1 = Yes and 0 = No
  mutate_at(
    vars(rCSILessQltyCat:rCSIMealAdultCat),
    ~ factor(., levels = c(1, 0), labels = c("Yes", "No"))) %>%
  # Create treatment variable using the IDPOOR Variable
  mutate(
    Treatment = case_when(
      IDPOOR == "POOR_1" | IDPOOR == "POOR_2" ~ "Treatment Group",
      IDPOOR == "NEAR_POOR" ~ "Control Group",
      TRUE ~ "Missing"),
    Treatment  = factor(Treatment)) %>%
  # Set variable labels for the Treatment, Other factor variables and rCSI variable
  set_variable_labels(
    Treatment = "Family Package Treatment Group or Control Group",
    rCSI = "Reduced Coping Strategies Index (rCSI)",
    rCSILessQltyCat = "Did household relied on less preffered",
    rCSIBorrowCat = "Did household borrowed food or relied on help from friends or relatives",
    rCSIMealSizeCat = "Did household reduced portion size of meals",
    rCSIMealNbCat = "Did household reduced the number of meals eaten per day",
    rCSIMealAdultCat = "Did household restricted consumption by adults in order for small children to eat")

# 1. Calculate mean RCSI by treatment and control groups - This is the main reduced coping strategies index
rCSIDataIndicators <- rCSIData %>% 
  group_by(Treatment, Province) %>% 
  summarise(
    MeanRCSI = mean(rCSI, na.rm = TRUE),
    Total  = n()) %>% 
  ungroup()


# 2. Calculate the percentage of people using each coping strategy (by using rCSI variable), disaggregated by treatment and control groups
rCSILessQlty <- rCSIData %>% 
  count(Treatment, rCSILessQltyCat, Province) %>% 
  group_by(Treatment, Province) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(rCSILessQltyCat == "Yes")

rCSIBorrow <- rCSIData %>%
  count(Treatment, rCSIBorrowCat, Province) %>% 
  group_by(Treatment, Province) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(rCSIBorrowCat == "Yes")

rCSIMealSize <- rCSIData %>%
  count(Treatment, rCSIMealSizeCat, Province) %>% 
  group_by(Treatment, Province) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(rCSIMealSizeCat == "Yes")

rCSIMealNb <- rCSIData %>%
  count(Treatment, rCSIMealNbCat, Province) %>% 
  group_by(Treatment, Province) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(rCSIMealNbCat == "Yes")

rCSIMealAdult <- rCSIData %>%
  count(Treatment, rCSIMealAdultCat, Province) %>% 
  group_by(Treatment, Province) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  filter(rCSIMealAdultCat == "Yes")



##################################################################################################################################################################

# FOOD INSECURITY EXPERIENCE SCALE (FIES) INDICATORS

FIESData <- FoodSecurity %>% 
  select(interview__key, interview__id, Province, District, Commune, Village, HHID,
         IDPOOR, equitycardno, householduuid, S8_3a:S8_3h) %>%
  mutate(
    Province = as_factor(Province),
    District = as_factor(District),
    Commune = as_factor(Commune),
    Village = as_factor(Village),
    IDPOOR = as_factor(IDPOOR)) %>% 
  # Rename the variables
  rename(
    FIESWorried = S8_3a,
    FIESEatHealthy = S8_3b,
    FIESFewFoods = S8_3c,
    FIESSkipMeal = S8_3d,
    FIESAteLess = S8_3e,
    FIESRanOut = S8_3f,
    FIESHungry = S8_3g,
    FIESWholeDay = S8_3h) %>%
  # Change -99 and -96 values to NA
  mutate(across(c(FIESWorried:FIESWholeDay), ~ na_if(., -99))) %>%
  mutate(across(c(FIESWorried:FIESWholeDay), ~ na_if(., -96))) %>%
  # Drop rows with NA values
  #drop_na() %>%
  # Change labelled variables to factor variables
  # Create the Treatment Variable
  mutate(Treatment = case_when(
    IDPOOR == "POOR_1" | IDPOOR == "POOR_2" ~ "Treatment Group",
    IDPOOR == "NEAR_POOR" ~ "Control Group",
    TRUE ~ "Missing"),
    Treatment = factor(Treatment))

# Calculate the percentage of people worried about lack of food, disaggregated by treatment and control groups
FIESWorried <- FIESData %>% 
  # Change the values of FIESWorried to factor variables
  mutate(FIESWorried = as_factor(FIESWorried)) %>%
  count(Treatment, FIESWorried) %>% 
  group_by(Treatment) %>%
  mutate(percentage = n / sum(n) * 100)

# Calculate the percentage of people worried about not eating healthy and nutritious food, disaggregated by treatment and control groups
FIESEatHealthy <- FIESData %>% 
  # Change the values of FIESEatHealthy to factor variables
  mutate(FIESEatHealthy = as_factor(FIESEatHealthy)) %>%
  count(Treatment, FIESEatHealthy) %>% 
  group_by(Treatment) %>%
  mutate(percentage = n / sum(n) * 100)

# Calculate the percentage of people worried about having few kinds of food to eat, disaggregated by treatment and control groups
FIESFewFoods <- FIESData %>% 
  count(Treatment, FIESFewFoods) %>% 
  group_by(Treatment) %>%
  mutate(percentage = n / sum(n) * 100)

# Calculate the percentage of people worried about skipping meals, disaggregated by treatment and control groups
FIESSkipMeal <- FIESData %>% 
  # Change the values of FIESSkipMeal to factor variables
  mutate(FIESSkipMeal = as_factor(FIESSkipMeal)) %>%
  count(Treatment, FIESSkipMeal) %>% 
  group_by(Treatment) %>%
  mutate(percentage = n / sum(n) * 100)

# Calculate the percentage of people worried about eating less than they should, disaggregated by treatment and control groups
FIESAteLess <- FIESData %>% 
  # Change the values of FIESAteLess to factor variables
  mutate(FIESAteLess = as_factor(FIESAteLess)) %>%
  count(Treatment, FIESAteLess) %>% 
  group_by(Treatment) %>%
  mutate(percentage = n / sum(n) * 100)

# Calculate the percentage of people worried about running out of food, disaggregated by treatment and control groups
FIESRanOut <- FIESData %>% 
  # Change the values of FIESRanOut to factor variables
  mutate(FIESRanOut = as_factor(FIESRanOut)) %>%
  count(Treatment, FIESRanOut) %>% 
  group_by(Treatment) %>%
  mutate(percentage = n / sum(n) * 100)

# Calculate the percentage of people worried about being hungry but not eating, disaggregated by treatment and control groups
FIESHungry <- FIESData %>% 
  # Change the values of FIESHungry to factor variables
  mutate(FIESHungry = as_factor(FIESHungry)) %>%
  count(Treatment, FIESHungry) %>% 
  group_by(Treatment) %>%
  mutate(percentage = n / sum(n) * 100)

# Calculate the percentage of people worried about going a whole day and night without eating, disaggregated by treatment and control groups
FIESWholeDay <- FIESData %>% 
  # Change the values of FIESWholeDay to factor variables
  mutate(FIESWholeDay = as_factor(FIESWholeDay)) %>%
  count(Treatment, FIESWholeDay) %>% 
  group_by(Treatment) %>%
  mutate(percentage = n / sum(n) * 100)

##################################################################################################################################################################

# RASCH MODEL FOR ESTIMATING FOOD INSECURITY EXPERIENCE SCALE (FIES) INDICATORS



















