library(tidyverse)
library(haven)
library(labelled)
library(survey)
library(RM.weights)
library(eRm)


FoodSecurity <- read_dta("new data/sec8.dta") %>% 
  select(hhid, Province, District, Commune, Village, HHID, IDPOOR, sec8_3a:sec8_9b) %>% 
  select(-sec8_4k_OTHER)
# Load the data with disabilities


# Livelihoods Copying Strategies Data

LCSEN <- FoodSecurity %>% 
  select(hhid, Province, District, Commune, Village, HHID,
         IDPOOR,
         # Livelihoods Copying Strategies Variables
         sec8_4a:sec8_4k_OTHER_EN) %>% 
  # Rename the variables
  rename(
    LcsENStressDomAsset = sec8_4a,
    LcsENStressSaving = sec8_4d,
    LcsENStressBorrowCash = sec8_4e,
    LcsENStressHHSeparation = sec8_4i,
    LcsENCrisisProdAssets = sec8_4b,
    LcsENCrisisHealth = sec8_4c,
    LcsENCrisisOutSchool = sec8_4g,
    LcsENEmResAssets = sec8_4f,
    LcsENEmBegged = sec8_4j,
    LcsENEmIllegalAct = sec8_4h,
    LCSENEngaged = sec8_4k__0,
    LCSENEngagedFood = sec8_4k__1,
    LCSENEngagedShelter = sec8_4k__2,
    LCSENEngagedEducation = sec8_4k__3,
    LCSENEngagedHealth = sec8_4k__4,
    LCSENEngagedNonFood = sec8_4k__5,
    LCSENEngagedHygiene = sec8_4k__6,
    LCSENEngagedEssentialSvcs = sec8_4k__7,
    LCSENEngagedDebtRepay = sec8_4k__8,
    LCSENEngagedOther = sec8_4k_OTHER_EN) %>% 
  # Change labelled variables to factor variables
  mutate(
    Province = as_factor(Province),
    District = as_factor(District),
    Commune = as_factor(Commune),
    Village = as_factor(Village),
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
  

##################################################################################################################################################################


# REDUCED COPING STRATEGIES INDICATORS DATA


rCSIData <- FoodSecurity %>% 
  select(hhid, Province, District, Commune, Village, HHID,
         IDPOOR, sec8_5:sec8_9b) %>% 
  # Rename Variables
  rename(
    rCSILessQlty = sec8_5,
    rCSIBorrow = sec8_6a,
    rCSIMealSize = sec8_7a,
    rCSIMealSizeWho = sec8_7b,
    rCSIMealNb = sec8_9a,
    rCSIMealNbWho = sec8_9b,
    rCSIMealAdult = sec8_8a,
    rCSIMealAdultWho = sec8_8b) %>%
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
    IDPOOR = as_factor(IDPOOR)) %>% 
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

##################################################################################################################################################################

# FOOD INSECURITY EXPERIENCE SCALE (FIES) INDICATORS

FIESData <- FoodSecurity %>% 
  select(hhid, Province, District, Commune, Village, IDPOOR, sec8_3a:sec8_3h) %>%
  # Change the variables to factor variables
  mutate(
    Province = as_factor(Province),
    District = as_factor(District),
    Commune = as_factor(Commune),
    Village = as_factor(Village),
    IDPOOR = as_factor(IDPOOR)) %>% 
  # Rename the variables
  rename(
    FIESWorried = sec8_3a,
    FIESEatHealthy = sec8_3b,
    FIESFewFoods = sec8_3c,
    FIESSkipMeal = sec8_3d,
    FIESAteLess = sec8_3e,
    FIESRanOut = sec8_3f,
    FIESHungry = sec8_3g,
    FIESWholeDay = sec8_3h) %>%
  # Change -99 and -96 values to NA
  mutate(across(c(FIESWorried:FIESWholeDay), ~ na_if(., -99))) %>%
  mutate(across(c(FIESWorried:FIESWholeDay), ~ na_if(., -96))) %>%
  # Drop rows with NA values
  drop_na() %>%
  # Change labelled variables to factor variables
  # Create the Treatment Variable
  mutate(Treatment = case_when(
    IDPOOR == "POOR_1" | IDPOOR == "POOR_2" ~ "Treatment Group",
    IDPOOR == "NEAR_POOR" ~ "Control Group",
    TRUE ~ "Missing"),
    Treatment = factor(Treatment),
    # Calculate row scores for the FIES variables
    FIESRowScore = rowSums(select(., FIESWorried:FIESWholeDay), na.rm = TRUE)) %>% 
  select(Province, District, Commune, Village, IDPOOR, Treatment, FIESRowScore, everything())


 # Small FIES Data

SmallFIESData <- FIESData %>% 
  select(hhid, Treatment, FIESWorried:FIESWholeDay) %>% 
  # Join with the survey design data
  left_join(SurveyDesignData, by = "hhid") %>% 
  select(FIESWorried:FIESWholeDay, Treatment, regiontype) %>%
  # Change FIES variables to numeric variables
  mutate(across(c(FIESWorried:FIESWholeDay), as.numeric), 
         hhsampleweights = NA, 
         IndividualWeights = NA,
         hhsampleweights = as.numeric(hhsampleweights),
         IndividualWeights = as.numeric(IndividualWeights)) %>%
  select(FIESWorried:FIESWholeDay, hhsampleweights, IndividualWeights, Treatment, regiontype) %>%
  #Change treatment and regiontype to factor variables
  mutate(
    Treatment = as_factor(Treatment),
    regiontype = as_factor(regiontype))

# Write a csv file for the Small FIES Data
write.csv(SmallFIESData, "SmallFIESData.csv")


# Livelihoods Coping Strategies - Food Security Indicator
LCSFS <- LCSEN %>% 
  mutate(
    MaxcopingBehaviourFS = case_when(
      LCSENEngagedFood == "No" ~ "Household not adopting coping strategies",
      TRUE ~ MaxcopingBehaviourEN))

##################################################################################################################################################################















