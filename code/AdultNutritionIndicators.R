## Loading libraries

library(tidyverse)
library(openxlsx)
library(showtext)
library(patchwork)
library(cowplot)
library(stringr)


source("code/SurveyDesign.R") # This file converts the data into survey designed data
source("code/Functions.R") # Functions for significance tests and creating Word documents

#calculate_proportions_and_ttest(SvyMADData, "MAD", "Treatment")

###################################################################################################################################################################



# 2. Maternal Nutrition Indicators

# Calculate minimum dietary diversity for women of reproductive age (MDD-W)
MDDWomen <- SvyDietQualityData %>% 
  filter(MDDGender == "Female") %>%
  filter(MDDAge >= 15 & MDDAge <= 49) %>%
  group_by(Treatment, MDDCategory) %>% 
  summarise(MDDWomen = survey_mean() * 100) %>% 
  filter(MDDCategory == 1) %>% 
  select(-MDDWomen_se) %>% 
  pivot_wider(names_from = Treatment, values_from = MDDWomen) %>% 
  mutate(Diff = `Treatment Group` - `Control Group`) %>% 
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>% 
  mutate(Indicator = "MDD-W") %>%
  select(Indicator, MDDCategory, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MDDCategory)

# Calculate MDD-W disagreggated by regiontype
MDDWomenRegion <- SvyDietQualityData %>% 
  filter(MDDGender == "Female") %>%
  filter(MDDAge >= 15 & MDDAge <= 49) %>%
  group_by(regiontype, MDDCategory) %>%
  summarise(MDDWomen = survey_mean() * 100) %>%
  filter(MDDCategory == 1) %>%
  select(-MDDWomen_se) %>%
  pivot_wider(names_from = regiontype, values_from = MDDWomen) %>%
  mutate(Diff = URBAN - RURAL) %>%
  mutate(Overall = (URBAN + RURAL)/2) %>%
  mutate(Indicator = "MDD-W") %>%
  select(Indicator, MDDCategory, Overall, RURAL, URBAN, Diff) %>%
  rename(Category = MDDCategory)

# # Calculate the proportion of women consuming all five food groups
# MDDWomen5Groups <- SvyDietQualityData %>% 
#   filter(MDDGender == "Female") %>%
#   filter(MDDAge >= 15 & MDDAge <= 49) %>%
#   group_by(Treatment, MDDAllGroupsCat) %>%
#   summarize(MDDWomen5Groups = survey_mean() * 100) %>% 
#   filter(MDDAllGroupsCat == "All Food groups consumed") %>%
#   select(-MDDWomen5Groups_se) %>%
#   pivot_wider(names_from = Treatment, values_from = MDDWomen5Groups) %>%
#   mutate(Diff = `Treatment Group` - `Control Group`) %>%
#   mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
#   mutate(Indicator = "MDD-W5G") %>%
#   select(Indicator, MDDAllGroupsCat, Overall, `Control Group`, `Treatment Group`, Diff) %>%
#   rename(Category = MDDAllGroupsCat)

# Calculate the Non Communicable Disease Risk Score
NCDRiskScore <- SvyDietQualityData %>%
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDGender) %>%
  summarise(NCDRiskScore = survey_mean(NCDRiskScore),
            Total = survey_total()) %>% 
  select(-c("NCDRiskScore_se", "Total_se", "Total")) %>% 
  pivot_wider(names_from = Treatment, values_from = NCDRiskScore) %>% 
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "NCD Risk") %>% 
  select(Indicator, MDDGender, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MDDGender)


NCDDRiskTot <- SvyDietQualityData %>%
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(NCDRiskScore = survey_mean(NCDRiskScore),
            Total = survey_total()) %>% 
  select(-c("NCDRiskScore_se", "Total_se", "Total")) %>% 
  pivot_wider(names_from = Treatment, values_from = NCDRiskScore) %>% 
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "NCD Risk") %>% 
  select(Indicator, Overall, `Control Group`, `Treatment Group`, Diff) 

# Calculate the Non Communicable Diseases Protective Foods Score
NCDProtectiveFoods <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDGender) %>%
  summarise(NCDProtectiveFoods = survey_mean(NCDProtScore),
            Total = survey_total()) %>%
  select(-c("NCDProtectiveFoods_se", "Total_se", "Total")) %>%
  pivot_wider(names_from = Treatment, values_from = NCDProtectiveFoods) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "NCD Protective Foods") %>%
  select(Indicator, MDDGender, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MDDGender)

# Calculate NCDProtectiveTot Score
NCDProtectiveTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(NCDProtectiveFoods = survey_mean(NCDProtScore),
            Total = survey_total()) %>% 
  select(-c("NCDProtectiveFoods_se", "Total_se", "Total")) %>%
  pivot_wider(names_from = Treatment, values_from = NCDProtectiveFoods) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "NCD Protective Foods") %>%
  select(Indicator, Overall, `Control Group`, `Treatment Group`, Diff)


# Calculate the GDRS Score
GDRSScore <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDGender) %>%
  summarise(GDRSScore = survey_mean(GDRScore),
            Total = survey_total()) %>% 
  select(-c("GDRSScore_se", "Total_se", "Total")) %>%
  pivot_wider(names_from = Treatment, values_from = GDRSScore) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "GDRS") %>%
  select(Indicator, MDDGender, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MDDGender)

# Calculate GDRSTot Score
GDRSTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(GDRSScore = survey_mean(GDRScore),
            Total = survey_total()) %>% 
  select(-c("GDRSScore_se", "Total_se", "Total")) %>%
  pivot_wider(names_from = Treatment, values_from = GDRSScore) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "GDRS") %>%
  select(Indicator, Overall, `Control Group`, `Treatment Group`, Diff)

# Calculate MDDScore, by gender and treatment
MDDScore <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDGender) %>%
  summarise(MDDScore = survey_mean(MDDScore),
            Total = survey_total()) %>% 
  select(-c("MDDScore_se", "Total_se", "Total")) %>%
  pivot_wider(names_from = Treatment, values_from = MDDScore) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MDD Score") %>%
  select(Indicator, MDDGender, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MDDGender)

# Calculate MDDScore, by treatment
MDDScoreTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(MDDScore = survey_mean(MDDScore),
            Total = survey_total()) %>% 
  select(-c("MDDScore_se", "Total_se", "Total")) %>%
  pivot_wider(names_from = Treatment, values_from = MDDScore) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MDD Score") %>%
  select(Indicator, Overall, `Control Group`, `Treatment Group`, Diff)

###Merge all the nutrition indicators####################################################
DQQNutritionIndicators <- bind_rows(MDDWomen, NCDRiskScore, NCDProtectiveFoods, GDRSScore, MDDScore) %>% 
  # Round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#write an excel file
write.xlsx(DQQNutritionIndicators, "report tables/DQQNutritionIndicators.xlsx")




# Calculate the same indicators based on province - important for future visualisations
## Minimum Dietary Diversity for womwn of reproductive age
SvyMDDWomenRegion <- SvyDietQualityData %>% 
  filter(MDDGender == "Female") %>%
  filter(MDDAge >= 15 & MDDAge <= 49) %>%
  group_by(regiontype, MDDCategory) %>%
  summarise(MDDWomen = survey_prop() * 100,
            MDDTot = survey_total()) %>% 
  filter(MDDCategory == 1) %>%
  select(-c("MDDWomen_se", "MDDTot_se", "MDDTot", "MDDCategory"))

# ## Consumption of all five food groups by womwn of reproductive age - 5 food groups
# MDDWomen5GroupsProvince <- SvyDietQualityData %>% 
#   filter(MDDGender == "Female") %>%
#   filter(MDDAge >= 15 & MDDAge <= 49) %>%
#   group_by(Treatment, Province, MDDAllGroupsCat) %>%
#   summarize(MDDWomen5Groups = survey_mean() * 100,
#             MDDTot = survey_total()) %>% 
#   filter(MDDAllGroupsCat == "All Food groups consumed")

# Calculate the Non Communicable Disease Risk Score
NCDRiskScoreRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype) %>%
  summarise(NCDRiskScore = survey_mean(NCDRiskScore),
            Total = survey_total())

# Calcualte the Non Communicable Diseases Protective Foods Score
NCDProtectiveFoodsRegion <- SvyDietQualityData %>% 
  group_by(regiontype) %>%
  summarise(NCDProtectiveFoods = survey_mean(NCDProtScore),
            Total = survey_total())

GDSScoreRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype) %>%
  summarise(GDRSScore = survey_mean(GDRScore),
            Total = survey_total())

GDSCScoreGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender) %>%
  summarise(GDRSScore = survey_mean(GDRScore),
            Total = survey_total())

###########################################################################################################################
