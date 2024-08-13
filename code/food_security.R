library(tidyverse)
library(haven)
library(labelled)


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
LCSENIndicators <- LCSEN %>% 
  group_by(MaxcopingBehaviourEN) %>% 
  summarise(
    n = n(),
    Percentage = n / sum(n) * 100) %>% 
  ungroup()


##################################################################################################################################################################


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
  # Create treatment variable using the IDPOOR Variable
  mutate(
    Treatment = case_when(
      IDPOOR == "POOR_1" | IDPOOR == "POOR_2" ~ "Treatment Group",
      IDPOOR == "NEAR_POOR" ~ "Control Group",
      TRUE ~ "Missing"),
    Treatment  = factor(Treatment)) %>%
  # Set variable labels for the Treatment and rCSI variable
  set_variable_labels(
    Treatment = "Family Package Treatment Group or Control Group",
    rCSI = "Reduced Coping Strategies Index (rCSI")

# Calculate mean RCSI by treatment and control groups
rCSIDataIndicators <- rCSIData %>% 
  group_by(Province) %>% 
  summarise(
    MeanRCSI = mean(rCSI, na.rm = TRUE)) %>% 
  ungroup()


















