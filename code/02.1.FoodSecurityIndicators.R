## Loading libraries

library(tidyverse)
library(openxlsx)
library(showtext)
library(patchwork)
library(cowplot)
library(stringr)


source("code/02.SurveyDesign.R") # This file converts the data into survey designed data
#source("code/02.00.Functions.R") # Functions for significance tests and creating Word documents

#calculate_proportions_and_ttest(SvyMADData, "MAD", "Treatment")

###################################################################################################################################################################

# 1 Food Security Indicators

# a. Livelihoods Coping Strategies Essential Needs Indicator (LCS - EN)

# Calculate the percentage of households using coping strategies to meet essential needs
SvyLCSENMax <- SvyLCSENData %>% 
  group_by(Treatment, MaxcopingBehaviourEN) %>% 
  summarise(Pct_LCSENMax = survey_prop() * 100) %>% 
  select( MaxcopingBehaviourEN, Treatment, Pct_LCSENMax) %>% 
  pivot_wider(names_from = Treatment, values_from = Pct_LCSENMax) %>% 
  mutate(Diff = `Treatment Group` - `Control Group`) %>% 
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>% 
  mutate(Indicator = "LCS - EN") %>%
  select(Indicator, MaxcopingBehaviourEN, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MaxcopingBehaviourEN) %>% 
  # rount every numeric variable to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

write.xlsx(SvyLCSENMax, "report tables/SvyLCSENMax.xlsx")

# Test if the difference in the indicators by treatment is significant
LCSENTest <- svychisq(~MaxcopingBehaviourEN + Treatment, design = SvyLCSENData) # Not Significant
#################################################################################################



# Calculate the proportion of households using coping strategies to meet essential needs disgragated by regiontype
SvyLCSENMaxRegion <- SvyLCSENData %>% 
  group_by(regiontype, MaxcopingBehaviourEN) %>% 
  summarise(Pct_LCSENMax = survey_prop() * 100) %>% 
  select(regiontype, MaxcopingBehaviourEN, Pct_LCSENMax) %>% 
  rename(Proportion = Pct_LCSENMax,
         Disagregation = regiontype)

# # Calculate the indicator by diability 
# SvyLCSENMaxDisability <- SvyLCSENData %>% 
#   group_by(DisabCategory, MaxcopingBehaviourEN) %>% 
#   summarise(Pct_LCSENMax = survey_prop() * 100) %>% 
#   select(DisabCategory, MaxcopingBehaviourEN, Pct_LCSENMax) %>% 
#   rename(Proportion = Pct_LCSENMax,
#          Disagregation = DisabCategory)

# Calculate the indicator by treatment/cpmtro
SvyLCSENMaxTreatment <- SvyLCSENData %>% 
  group_by(Treatment, MaxcopingBehaviourEN) %>% 
  summarise(Pct_LCSENMax = survey_prop() * 100) %>% 
  select(Treatment, MaxcopingBehaviourEN, Pct_LCSENMax) %>% 
  rename(Proportion = Pct_LCSENMax,
         Disagregation = Treatment)


# Calculate the proportion of the population using coping strategies to afford food
# Livelihoods Coping Strategies Food Security Indicator
# Calculate the proportion of households using coping strategies to meet food security needs
SvyLCSFSMax <- SvyLCSFS %>% 
  group_by(Treatment, MaxcopingBehaviourFS) %>% 
  summarise(Pct_LCSFSMax = survey_prop() * 100) %>% 
  select( MaxcopingBehaviourFS, Treatment, Pct_LCSFSMax) %>% 
  pivot_wider(names_from = Treatment, values_from = Pct_LCSFSMax) %>% 
  mutate(Diff = `Treatment Group` - `Control Group`) %>% 
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>% 
  mutate(Indicator = "LCS - FS") %>%
  select(Indicator, MaxcopingBehaviourFS, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MaxcopingBehaviourFS)

#######################################################################################################
# Test if the difference in the indicators by treatment is significant
LCSFSTest <- svychisq(~MaxcopingBehaviourFS + Treatment, design = SvyLCSFS) # Not Significant

####################################################################################################
# LCSFS by region

SvyLCSFSMaxRegion <- SvyLCSFS %>% 
  group_by(regiontype, MaxcopingBehaviourFS) %>% 
  summarise(Pct_LCSFSMax = survey_prop() * 100) %>% 
  select(regiontype, MaxcopingBehaviourFS, Pct_LCSFSMax) %>% 
  rename(Proportion = Pct_LCSFSMax,
         Disagregation = regiontype)

########################################################################################################
# test if the difference in the indicators by region is significant
LCSFSChisqtest <- svychisq(~MaxcopingBehaviourFS + regiontype, design = SvyLCSFS) # Significant
#######################################################################################################

SvyLCSFSMax <- SvyLCSFS %>% 
  group_by( MaxcopingBehaviourFS) %>% 
  summarise(Pct_LCSFSMax = survey_prop() * 100) %>% 
  select( MaxcopingBehaviourFS, Pct_LCSFSMax) %>% 
  rename(Proportion = Pct_LCSFSMax,
         Disagregation = MaxcopingBehaviourFS)

# Merge the data
FoodSecurityCS <- bind_rows(SvyLCSENMax, SvyLCSFSMax) %>% 
  # Round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

# Write the data to an excel file
write.xlsx(FoodSecurityCS, "report tables/FoodSecurityCS.xlsx")

# Calculate the mean RCSI score

meanRCSI <- SvyrCSIData %>% 
  group_by(Treatment) %>% 
  summarise(MeanRCSI = survey_mean(rCSI)) %>% 
  select(Treatment, MeanRCSI) %>% 
  pivot_wider(names_from = Treatment, values_from = MeanRCSI) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>% 
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "rCSI") %>%
  select(Indicator, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  # Change every numeric variable to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

# Write an excel file
write.xlsx(meanRCSI, "report tables/meanRCSI.xlsx")

#################################################################################################

# Perform a t-test to test if the difference in the mean RCSI score is significant
rCSITest <- svyttest(rCSI ~ Treatment, design = SvyrCSIData) # Not significant

##################################################################################################

meanRCSIOveral <- SvyrCSIData %>% 
  summarise(MeanRCSI = survey_mean(rCSI)) %>% 
  mutate(Indicator = "rCSI") %>% 
  select(Indicator, MeanRCSI)

# Calculate the same indicators disaggreagted by province - this is key for vicualisations
## Livelihoods Coping Strategies Essential Needs Indicator (LCS - EN)
SvyLCSENMaxProvince <- SvyLCSENData %>% 
  mutate(MaxcopingBehaviourFS = factor(MaxcopingBehaviourEN,
                                       levels = c("Household not adopting coping strategies",
                                                  "Stress coping strategies",
                                                  "Crisis coping strategies",
                                                  "Emergency coping strategies"))) %>%
  group_by(Province, MaxcopingBehaviourEN) %>% 
  summarise(Pct_LCSENMax = survey_prop() * 100) %>% 
  select(Province, MaxcopingBehaviourEN, Pct_LCSENMax) %>% 
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(Province = if_else(Province == "Banteay Meanchey", "B. Meanchey", Province),
         Province = if_else(Province == "Kampong Cham", "K. Cham", Province),
         Province = if_else(Province == "Kampong Speu", "K. Speu", Province))

## Livelihoods Coping Strategies Essential Needs Indicator (LCS - FS)
SvyLCSENFoodProvinceTab <- SvyLCSFS %>% 
  mutate(MaxcopingBehaviourFS = factor(MaxcopingBehaviourFS,
                                       levels = c("Household not adopting coping strategies",
                                                  "Stress coping strategies",
                                                  "Crisis coping strategies",
                                                  "Emergency coping strategies"))) %>%
  group_by(Province, MaxcopingBehaviourFS) %>% 
  summarise(Pct_LCSENFood = survey_prop() * 100) %>% 
  select(Province, MaxcopingBehaviourFS, Pct_LCSENFood) %>%
  # Round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2)) %>% 
  mutate(Province = if_else(Province == "Banteay Meanchey", "B. Meanchey", Province))


# Save the data to an excel file
write.xlsx(SvyLCSENFoodProvinceTab, "report tables/SvyLCSENFoodProvince.xlsx")

## Reduced Coping Strategies Index (rCSI)
meanRCSIProvince <- SvyrCSIData %>% 
  group_by(regiontype) %>% 
  summarise(MeanRCSI = survey_mean(rCSI)) %>% 
  select(regiontype, MeanRCSI) 
###################################################################################################################################################################

# Individual components of food security sub-indicators
# 1. FIES
FIESWorriedProvince <- FIESData %>%
  count(Province, FIESWorried) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(FIESWorried == 1) %>%
  select(Province, Pct) %>%
  rename(FIESWorried = Pct)

FIESEatHealthyProvince <- FIESData %>%
  count(Province, FIESEatHealthy) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(FIESEatHealthy == 1) %>% 
  select(Province, Pct) %>%
  rename(FIESEatHealthy = Pct)


FIESFewFoodsProvince <- FIESData %>%
  count(Province, FIESFewFoods) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(FIESFewFoods == 1) %>% 
  select(Province, Pct) %>% 
  rename(FIESFewFoods = Pct)


FIESSkipMealProvince <- FIESData %>%
  count(Province, FIESSkipMeal) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(FIESSkipMeal == 1) %>% 
  select(Province, Pct) %>% 
  rename(FIESSkipMeal = Pct)

FIESAteLessProvince <- FIESData %>%
  count(Province, FIESAteLess) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(FIESAteLess == 1) %>% 
  select(Province, Pct) %>%
  rename(FIESAteLess = Pct)

FIESRanOutProvince <- FIESData %>%
  count(Province, FIESRanOut) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(FIESRanOut == 1) %>%
  select(Province, Pct) %>%
  rename(FIESRanOut = Pct)

FIESHungryProvince <- FIESData %>%
  count(Province, FIESHungry) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(FIESHungry == 1) %>%
  select(Province, Pct) %>%
  rename(FIESHungry = Pct)

FIESWholeDayProvince <- FIESData %>%
  count(Province, FIESWholeDay) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(FIESWholeDay == 1) %>% 
  select(Province, Pct) %>%
  rename(FIESWholeDay = Pct)

# Join all the food security indicators together
FIESProvince <- full_join(FIESWorriedProvince, FIESEatHealthyProvince, by = "Province") %>%
  full_join(FIESFewFoodsProvince, by = "Province") %>%
  full_join(FIESSkipMealProvince, by = "Province") %>%
  full_join(FIESAteLessProvince, by = "Province") %>%
  full_join(FIESRanOutProvince, by = "Province") %>%
  full_join(FIESHungryProvince, by = "Province") %>%
  full_join(FIESWholeDayProvince, by = "Province") 


# 2. RCSI
rCSILessQltyCatProvince <- rCSIData %>% 
  count(Province, rCSILessQltyCat) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(rCSILessQltyCat == "Yes") %>%
  select(Province, Pct) %>%
  rename(rCSILessQltyCat = Pct)

rCSIBorrowCatProvince <- rCSIData %>% 
  count(Province, rCSIBorrowCat) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(rCSIBorrowCat == "Yes") %>%
  select(Province, Pct) %>%
  rename(rCSIBorrowCat = Pct)


rCSIMealSizeCatProvince <- rCSIData %>% 
  count(Province, rCSIMealSizeCat) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(rCSIMealSizeCat == "Yes") %>%
  select(Province, Pct) %>% 
  rename(rCSIMealSizeCat = Pct)

rCSIMealNbCatProvince <- rCSIData %>% 
  count(Province, rCSIMealNbCat) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(rCSIMealNbCat == "Yes") %>%
  select(Province, Pct) %>%
  rename(rCSIMealNbCat = Pct)

rCSIMealAdultCatProvince <- rCSIData %>% 
  count(Province, rCSIMealAdultCat) %>%
  group_by(Province) %>%
  mutate(Pct = n / sum(n) * 100) %>% 
  filter(rCSIMealAdultCat == "Yes") %>%
  select(Province, Pct) %>%
  rename(rCSIMealAdultCat = Pct)

# Join all the RCSI Indicators
RCSIProvince <- full_join(rCSILessQltyCatProvince, rCSIBorrowCatProvince, by = "Province") %>%
  full_join(rCSIMealSizeCatProvince, by = "Province") %>%
  full_join(rCSIMealNbCatProvince, by = "Province") %>%
  full_join(rCSIMealAdultCatProvince, by = "Province")



rCSIMealSizeCatTreatment <- SvyrCSIData %>% 
  group_by(Treatment, rCSIMealSizeCat) %>%
  summarise(Pct = survey_prop() * 100) %>% 
  filter(rCSIMealSizeCat == "Yes") %>%
  select(Treatment, Pct) %>%
  pivot_wider(names_from = Treatment, values_from = Pct) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "Reduced meal size or portions") %>%
  select(Indicator, Overall, `Treatment Group`,  `Control Group`, Diff) %>%
  #round every numeric variable to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

rCSIAdultMealSizeCatTreatment <- SvyrCSIData %>% 
  group_by(Treatment, rCSIMealAdultCat) %>%
  summarise(Pct = survey_prop() * 100) %>% 
  filter(rCSIMealAdultCat == "Yes") %>%
  select(Treatment, Pct) %>%
  pivot_wider(names_from = Treatment, values_from = Pct) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "Reduced meal size or portions for adults") %>%
  select(Indicator, Overall, `Treatment Group`,  `Control Group`, Diff) %>%
  #round every numeric variable to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

rCSIBorrowCatTreatment <- SvyrCSIData %>% 
  group_by(Treatment, rCSIBorrowCat) %>%
  summarise(Pct = survey_prop() * 100) %>% 
  filter(rCSIBorrowCat == "Yes") %>%
  select(Treatment, Pct) %>%
  pivot_wider(names_from = Treatment, values_from = Pct) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "Borrowed food from relative or friend") %>%
  select(Indicator, Overall, `Treatment Group`,  `Control Group`, Diff) %>%
  #round every numeric variable to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

rCSIQualityCatTreatment <- SvyrCSIData %>% 
  group_by(Treatment, rCSILessQltyCat) %>%
  summarise(Pct = survey_prop() * 100) %>% 
  filter(rCSILessQltyCat == "Yes") %>%
  select(Treatment, Pct) %>%
  pivot_wider(names_from = Treatment, values_from = Pct) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "Relied on less prefered and less expensive food") %>%
  select(Indicator, Overall, `Treatment Group`,  `Control Group`, Diff) %>%
  #round every numeric variable to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

rCSINbMealCatTreatment <- SvyrCSIData %>% 
  group_by(Treatment, rCSIMealNbCat) %>%
  summarise(Pct = survey_prop() * 100) %>% 
  filter(rCSIMealNbCat == "Yes") %>%
  select(Treatment, Pct) %>%
  pivot_wider(names_from = Treatment, values_from = Pct) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "Reduced number of meals") %>%
  select(Indicator, Overall, `Treatment Group`,  `Control Group`, Diff) %>%
  #round every numeric variable to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

# Bind the data together
rCSICopingStrategiesTreatment <- bind_rows(rCSILessQltyCatTreatment, rCSIBorrowCatTreatment, rCSIMealSizeCatTreatment, rCSINbMealCatTreatment, rCSIAdultMealSizeCatTreatment) %>% 
  # Round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2)) %>% 
  arrange(Diff) %>% 
  mutate(Indicator = str_wrap(Indicator, 20)) %>% 
  # Change every numeric variable to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))
  
# Write the data to an excel file
write.xlsx(rCSICopingStrategiesTreatment, "report tables/rCSICopingStrategiesTreatment.xlsx")
##########################################################################################################################################
# Test if the difference in the indicators by treatment is significant
rCSILessQualCatTest <- svychisq(~rCSILessQltyCat + Treatment, design = SvyrCSIData) # Significant at 10%
rCSIBorrowCatTest <- svychisq(~rCSIBorrowCat + Treatment, design = SvyrCSIData) # Not significant
rCSIMealSizeCatTest <- svychisq(~rCSIMealSizeCat + Treatment, design = SvyrCSIData) # Significant at 10%
rCSIMealNbCatTest <- svychisq(~rCSIMealNbCat + Treatment, design = SvyrCSIData) # Not significant
rCSIMealAdultCatTest <- svychisq(~rCSIMealAdultCat + Treatment, design = SvyrCSIData) # Not significant
###########################################################################################################################################


# rCSI without disagregating by province
rCSILessQltyCat <- rCSIData %>% 
  count(rCSILessQltyCat) %>% 
  mutate(Pct = n / sum(n) * 100) %>% 
  select(rCSILessQltyCat, Pct) %>%
  pivot_wider(names_from = rCSILessQltyCat, values_from = Pct) %>% 
  mutate(strategy = "Relied on less prefered and less expensive food",
         Percentage = Yes) %>%
  select(strategy, Percentage)

rCSIBorrowCat <- rCSIData %>%
  count(rCSIBorrowCat) %>% 
  mutate(Pct = n / sum(n) * 100) %>% 
  select(rCSIBorrowCat, Pct) %>%
  pivot_wider(names_from = rCSIBorrowCat, values_from = Pct) %>% 
  mutate(strategy = "Borrowed food from relative or friend",
         Percentage = Yes) %>%
  select(strategy, Percentage)


rCSIMealSizeCat <- rCSIData %>%
  count(rCSIMealSizeCat) %>% 
  mutate(Pct = n / sum(n) * 100) %>% 
  select(rCSIMealSizeCat, Pct) %>%
  pivot_wider(names_from = rCSIMealSizeCat, values_from = Pct) %>% 
  mutate(strategy = "Reduced meal size or portions",
         Percentage = Yes) %>%
  select(strategy, Percentage)


rCSIMealNbCat <- rCSIData %>%
  count(rCSIMealNbCat) %>% 
  mutate(Pct = n / sum(n) * 100) %>% 
  select(rCSIMealNbCat, Pct) %>%
  pivot_wider(names_from = rCSIMealNbCat, values_from = Pct) %>% 
  mutate(strategy = "Reduced number of meals",
         Percentage = Yes) %>%
  select(strategy, Percentage)

rCSIMealAdultCat <- rCSIData %>%
  count(rCSIMealAdultCat) %>% 
  mutate(Pct = n / sum(n) * 100) %>% 
  select(rCSIMealAdultCat, Pct) %>%
  pivot_wider(names_from = rCSIMealAdultCat, values_from = Pct) %>% 
  mutate(strategy = "Reduced meal size or portions for adults",
         Percentage = Yes) %>%
  select(strategy, Percentage)

# Bind these coping strategies together
rCSICopingStrategies <- bind_rows(rCSILessQltyCat, rCSIBorrowCat, rCSIMealSizeCat, rCSIMealNbCat, rCSIMealAdultCat) %>% 
  # Round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2)) %>% 
  arrange(Percentage) %>% 
  mutate(strategy = str_wrap(strategy, 20)) %>% 
  # Change every numeric variable to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))


#########################################################################################################################

# Calculate who uses these strategies by regiontype - Less quality/preffered foods

rCSILessQltyWhoRegion <- SvyrCSIData %>% 
  drop_na(rCSILessQltyCat) %>% 
  group_by(regiontype, rCSILessQltyCat) %>%
  summarise(Pct = survey_prop() * 100) %>%
  select(regiontype, rCSILessQltyCat, Pct) %>%
  pivot_wider(names_from = rCSILessQltyCat, values_from = Pct) %>%
  mutate(Indicator = "Relied on less prefered and less expensive food") %>%
  select(Indicator, `Yes`) %>%
  rename(`Yes (%)` = `Yes`)

rCSIBorrowWhoRegion <- SvyrCSIData %>%
  drop_na(rCSIBorrowCat) %>% 
  group_by(regiontype, rCSIBorrowCat) %>%
  summarise(Pct = survey_prop() * 100) %>%
  select(regiontype, rCSIBorrowCat, Pct) %>%
  pivot_wider(names_from = rCSIBorrowCat, values_from = Pct) %>%
  mutate(Indicator = "Borrowed food from relative or friend") %>%
  select(Indicator, `Yes`) %>%
  rename(`Yes (%)` = `Yes`)

rCSIMealSizeWhoRegion <- SvyrCSIData %>%
  drop_na(rCSIMealSizeCat) %>% 
  group_by(regiontype, rCSIMealSizeCat) %>%
  summarise(Pct = survey_prop() * 100) %>%
  select(regiontype, rCSIMealSizeCat, Pct) %>%
  pivot_wider(names_from = rCSIMealSizeCat, values_from = Pct) %>%
  mutate(Indicator = "Reduced meal size or portions") %>%
  select(Indicator, `Yes`) %>%
  rename(`Yes (%)` = `Yes`)

rCSIMealNbWhoRegion <- SvyrCSIData %>%
  drop_na(rCSIMealNbCat) %>% 
  group_by(regiontype, rCSIMealNbCat) %>%
  summarise(Pct = survey_prop() * 100) %>%
  select(regiontype, rCSIMealNbCat, Pct) %>%
  pivot_wider(names_from = rCSIMealNbCat, values_from = Pct) %>%
  mutate(Indicator = "Reduced number of meals") %>%
  select(Indicator, `Yes`) %>%
  rename(`Yes (%)` = `Yes`)

rCSIMealAdultWhoRegion <- SvyrCSIData %>%
  drop_na(rCSIMealAdultCat) %>% 
  group_by(regiontype, rCSIMealAdultCat) %>%
  summarise(Pct = survey_prop() * 100) %>%
  select(regiontype, rCSIMealAdultCat, Pct) %>%
  pivot_wider(names_from = rCSIMealAdultCat, values_from = Pct) %>%
  mutate(Indicator = "Reduced meal size or portions for adults") %>%
  select(Indicator, `Yes`) %>%
  rename(`Yes (%)` = `Yes`)

# Merge the data for the visualisation
rCSICopingStrategiesRegion <- bind_rows(rCSILessQltyWhoRegion, rCSIBorrowWhoRegion, rCSIMealSizeWhoRegion, rCSIMealNbWhoRegion, rCSIMealAdultWhoRegion) %>% 
  # Round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2)) %>% 
  arrange(`Yes (%)`) %>% 
  mutate(Indicator = str_wrap(Indicator, 20)) %>% 
  # Change every numeric variable to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

rCSIMealSizeWho <- rCSIData %>% 
  drop_na(rCSIMealSizeWho) %>% 
  count(Province, rCSIMealSizeWho) %>% 
  mutate(Pct = n / sum(n) * 100) %>% 
  select(rCSIMealSizeWho, Pct) %>%
  pivot_wider(names_from = rCSIMealSizeWho, values_from = Pct) %>% 
  mutate(Indicator = "Reduced meal sizes (All HH Members)") %>% 
  select(Indicator, everything()) %>% 
  rename(`Male Adults (%)` = `Mainly adult male (18 and above)`,
         `Female Adults (%)` = `Mainly adult female (18 and above)`) %>% 
  mutate(`Other Options (%)` = `Mainly children &amp; youth male (&lt;18)` + `All adults equally` + `All family members equally`) %>%
  select(Indicator, `Male Adults (%)`, `Female Adults (%)`, `Other Options (%)`)


rCSIMealAdultWho <- rCSIData %>% 
  drop_na(rCSIMealAdultWho) %>% 
  count(rCSIMealAdultWho) %>% 
  mutate(Pct = n / sum(n) * 100) %>% 
  select(rCSIMealAdultWho, Pct) %>%
  pivot_wider(names_from = rCSIMealAdultWho, values_from = Pct) %>%
  mutate(Indicator = "Reduced meal sizes (Adults only)") %>% 
  select(Indicator, everything()) %>%
  rename(`Male Adults (%)` = `Mainly adult male (18 and above)`,
         `Female Adults (%)` = `Mainly adult female (18 and above)`,
         `Other Options (%)` = `All adults equally`) %>%
  select(Indicator, `Male Adults (%)`, `Female Adults (%)`, `Other Options (%)`)

# Merge the two tables for the visualisation
rCSIMealSizeWhoTable <- bind_rows(rCSIMealSizeWho, rCSIMealAdultWho) %>% 
  # Round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))


####################################################################################################################################################
# Statistical Tests - tests if the difference in all indicators by trestment is significant
# 1. LCS - EN
LCS_EN_Test <- svychisq(MaxcopingBehaviourEN ~ Treatment, design = SvyLCSENData)

# 2. LCS - FS
LCS_FS_Test <- svyglm(MaxcopingBehaviourFS ~ Treatment, design = SvyLCSFS)

# 3. Coping Strategies
