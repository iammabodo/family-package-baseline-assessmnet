## Loading libraries

library(tidyverse)
library(openxlsx)
library(showtext)
library(patchwork)
library(cowplot)
library(stringr)


source("code/02.SurveyDesign.R") # This file converts the data into survey designed data
source("code/02.00.Functions.R") # Functions for significance tests and creating Word documents

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
  dplyr::select(-MDDWomen_se) %>% 
  pivot_wider(names_from = Treatment, values_from = MDDWomen) %>% 
  mutate(Diff = `Treatment Group` - `Control Group`) %>% 
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>% 
  mutate(Indicator = "MDD-W") %>%
  dplyr::select(Indicator, MDDCategory, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MDDCategory)
# Write this into an excel file
write.xlsx(MDDWomen, "report tables/MDDWomen.xlsx")

#################################################################################################################
# Test if the difference in MDD-W is significant
MDDWomenChisq <- SvyDietQualityData %>% 
  filter(MDDGender == "Female") %>%
  filter(MDDAge >= 15 & MDDAge <= 49) %>%
  svychisq(~MDDCategory + Treatment, design = .) # Not significant

############################################################################################################
# Calculate MDD-W disagreggated by regiontype
MDDWomenRegion <- SvyDietQualityData %>% 
  filter(MDDGender == "Female") %>%
  filter(MDDAge >= 15 & MDDAge <= 49) %>%
  group_by(regiontype, MDDCategory) %>%
  summarise(MDDWomen = survey_mean() * 100) %>%
  ungroup() %>%
  filter(MDDCategory == 1) %>%
  dplyr::select(-MDDWomen_se) %>%
  rename(Percentage = MDDWomen) %>%
  mutate(Indicator = "MDD-W",
         MDDCategory = regiontype) %>%
  dplyr::select(Indicator, MDDCategory, Percentage)
  

# Calculate MDDWomen - uncategorised
MDDWomenTot <- SvyDietQualityData %>% 
  filter(MDDGender == "Female") %>%
  filter(MDDAge >= 15 & MDDAge <= 49) %>%
  group_by(MDDCategory) %>%
  summarise(MDDWomen = survey_mean() * 100) %>%
  filter(MDDCategory == 1) %>% 
  dplyr::select(-MDDWomen_se) %>%
  rename(Percentage = MDDWomen) %>% 
  mutate(Indicator = "MDD-W",
         MDDCategory = "Overall") %>%
  dplyr::select(Indicator, MDDCategory, Percentage)

# Bind the rows
MDDTable <- bind_rows(MDDWomenTot, MDDWomenRegion) %>% 
  # Round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2)) %>% 
  rename(Category = MDDCategory)

#################################################################################################################

#Test the difference in MDD-W by region
MDDWomenChisq <- svychisq(~MDDCategory + regiontype, design = SvyDietQualityData) # Not significant

#################################################################################################################

# Calculate calculate MDD by province
MDDWomenProvince <- SvyDietQualityData %>% 
  filter(MDDGender == "Female") %>%
  filter(MDDAge >= 15 & MDDAge <= 49) %>%
  group_by(Province, MDDCategory) %>%
  summarise(MDDWomen = survey_mean() * 100) %>%
  filter(MDDCategory == 1) %>%
  dplyr::select(-MDDWomen_se) %>%
  rename(Percentage = MDDWomen) %>%
  mutate(Indicator = "MDD-W",
         MDDCategory = Province) %>%
  dplyr::select(Indicator, MDDCategory, Percentage)



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
  dplyr::select(-c("NCDRiskScore_se", "Total_se", "Total")) %>% 
  pivot_wider(names_from = Treatment, values_from = NCDRiskScore) %>% 
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "NCD Risk") %>% 
  dplyr::select(Indicator, MDDGender, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MDDGender)


NCDDRiskTot <- SvyDietQualityData %>%
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(NCDRiskScore = survey_mean(NCDRiskScore),
            Total = survey_total()) %>% 
  dplyr::select(-c("NCDRiskScore_se", "Total_se", "Total")) %>% 
  pivot_wider(names_from = Treatment, values_from = NCDRiskScore) %>% 
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "NCD Risk") %>% 
  dplyr::select(Indicator, Overall, `Control Group`, `Treatment Group`, Diff) 

# Write this into an excel file
write.xlsx(NCDDRiskTot, "report tables/NCDRiskScore.xlsx")

###################################################################################################
# Test if the difference in NCD Risk is significant
NCDRiskChisq <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  svychisq(~NCDRiskScore + Treatment, design = .) # Not significant
###################################################################################################

# Calculate the Non Communicable Diseases Protective Foods Score
NCDProtectiveFoods <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDGender) %>%
  summarise(NCDProtectiveFoods = survey_mean(NCDProtScore),
            Total = survey_total()) %>%
  dplyr::select(-c("NCDProtectiveFoods_se", "Total_se", "Total")) %>%
  pivot_wider(names_from = Treatment, values_from = NCDProtectiveFoods) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "NCD Protective Foods") %>%
  dplyr::select(Indicator, MDDGender, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MDDGender)

# Calculate NCDProtectiveTot Score
NCDProtectiveTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(NCDProtectiveFoods = survey_mean(NCDProtScore),
            Total = survey_total()) %>% 
  dplyr::select(-c("NCDProtectiveFoods_se", "Total_se", "Total")) %>%
  pivot_wider(names_from = Treatment, values_from = NCDProtectiveFoods) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "NCD Protective Foods") %>%
  dplyr::select(Indicator, Overall, `Control Group`, `Treatment Group`, Diff)

# Write into an excel file
write.xlsx(NCDProtectiveTot, "report tables/NCDProtectiveFoods.xlsx")

###################################################################################################
# Test if the difference in NCD Protective Foods is significant
NCDProtectiveFoodsChisq <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  svychisq(~NCDProtScore + Treatment, design = .) # Not significant
###################################################################################################

# Calculate the GDRS Score
GDRSScore <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDGender) %>%
  summarise(GDRSScore = survey_mean(GDRScore),
            Total = survey_total()) %>% 
  dplyr::select(-c("GDRSScore_se", "Total_se", "Total")) %>%
  pivot_wider(names_from = Treatment, values_from = GDRSScore) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "GDRS") %>%
  dplyr::select(Indicator, MDDGender, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = MDDGender)

# Calculate GDRSTot Score
GDRSTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(GDRSScore = survey_mean(GDRScore),
            Total = survey_total()) %>% 
  dplyr::select(-c("GDRSScore_se", "Total_se", "Total")) %>%
  pivot_wider(names_from = Treatment, values_from = GDRSScore) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "GDRS") %>%
  dplyr::select(Indicator, Overall, `Control Group`, `Treatment Group`, Diff)

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
  dplyr::select(-c("MDDWomen_se", "MDDTot_se", "MDDTot", "MDDCategory"))

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

# Individual food groups consumed
#1. Staples
#a. Staples by Treatment
SvyStaples <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(Treatment, MDDStaples) %>%
  summarise(MDDStaplesCat = survey_mean() * 100) %>%
  filter(MDDStaples == 1) %>%
  dplyr::select(-c(MDDStaplesCat_se, MDDStaples)) %>% 
  rename(Disaggregation = Treatment) %>% 
  pivot_wider(names_from = Disaggregation, values_from = MDDStaplesCat) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "Staple Foods") %>%
  dplyr::select(Indicator, everything())

#b. Staples by Region
SvyStaplesRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(regiontype, MDDStaples) %>%
  summarise(MDDStaplesCat = survey_mean() * 100) %>%
  filter(MDDStaples == 1) %>%
  dplyr::select(-c(MDDStaplesCat_se, MDDStaples)) %>%
  pivot_wider(names_from = regiontype, values_from = MDDStaplesCat) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "Staple Foods") %>%
  dplyr::select(Indicator, everything())

# Join the two dataframes
StaplesData <- SvyStaples %>% 
  left_join(SvyStaplesRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))


#################################################################################################################

#Chisq test for staple foods
ChisqTab <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") 

StaplesChisqIE <- svychisq(~MDDStaples + Treatment, design = ChisqTab) # Not significant
StaplesChisqRegion <- svychisq(~MDDStaples + regiontype, design = ChisqTab) # Not Significant

#################################################################################################################

#2. Pulses
#a. Pulses by Treatment
SvyPulses <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(Treatment, MDDPulses) %>%
  summarise(MDDPulsesCat = survey_mean() * 100) %>%
  filter(MDDPulses == 1) %>%
  dplyr::select(-c(MDDPulsesCat_se, MDDPulses)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = MDDPulsesCat) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "Pulses") %>%
  dplyr::select(Indicator, everything())

#b. Pulses by Region
SvyPulsesRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(regiontype, MDDPulses) %>%
  summarise(MDDPulsesCat = survey_mean() * 100) %>%
  filter(MDDPulses == 1) %>%
  dplyr::select(-c(MDDPulsesCat_se, MDDPulses)) %>%
  pivot_wider(names_from = regiontype, values_from = MDDPulsesCat) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "Pulses") %>%
  dplyr::select(Indicator, everything())

# Join the two dataframes

PulsesData <- SvyPulses %>% 
  left_join(SvyPulsesRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for pulses
PulsesChisqIE <- svychisq(~MDDPulses + Treatment, design = ChisqTab) # Not significant
PulsesChisqRegion <- svychisq(~MDDPulses + regiontype, design = ChisqTab) # Not significant

#################################################################################################################

#3. NutsSeeds
#a. NutsSeeds by Treatment

SvyNutsSeeds <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(Treatment, MDDNutsSeeds) %>%
  summarise(MDDNutsSeedsCat = survey_mean() * 100) %>%
  filter(MDDNutsSeeds == 1) %>%
  dplyr::select(-c(MDDNutsSeedsCat_se, MDDNutsSeeds)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = MDDNutsSeedsCat) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "Nuts and Seeds") %>%
  dplyr::select(Indicator, everything())
  
#b. NutsSeeds by Region

SvyNutsSeedsRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(regiontype, MDDNutsSeeds) %>%
  summarise(MDDNutsSeedsCat = survey_mean() * 100) %>%
  filter(MDDNutsSeeds == 1) %>%
  dplyr::select(-c(MDDNutsSeedsCat_se, MDDNutsSeeds)) %>%
  pivot_wider(names_from = regiontype, values_from = MDDNutsSeedsCat) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "Nuts and Seeds") %>%
  dplyr::select(Indicator, everything())

# Join the two dataframes
NutsSeedsData <- SvyNutsSeeds %>% 
  left_join(SvyNutsSeedsRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for Nuts and Seeds
NutsSeedsChisqIE <- svychisq(~MDDNutsSeeds + Treatment, design = ChisqTab) # Not significant
NutsSeedsChisqRegion <- svychisq(~MDDNutsSeeds + regiontype, design = ChisqTab) # Not significant

#################################################################################################################

#4. Dairy
#a. Dairy by Treatment
SvyDairy <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(Treatment, MDDDiary) %>%
  summarise(MDDDairyCat = survey_mean() * 100) %>%
  filter(MDDDiary == 1) %>%
  dplyr::select(-c(MDDDairyCat_se, MDDDiary)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = MDDDairyCat) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "Dairy") %>%
  dplyr::select(Indicator, everything())

#b. Dairy by Region
SvyDairyRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(regiontype, MDDDiary) %>%
  summarise(MDDDairyCat = survey_mean() * 100) %>%
  filter(MDDDiary == 1) %>%
  dplyr::select(-c(MDDDairyCat_se, MDDDiary)) %>%
  pivot_wider(names_from = regiontype, values_from = MDDDairyCat) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "Dairy") %>%
  dplyr::select(Indicator, everything())

# Join the two dataframes
DairyData <- SvyDairy %>% 
  left_join(SvyDairyRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for Dairy
DairyChisqIE <- svychisq(~MDDDiary + Treatment, design = ChisqTab) # Not significant
DairyChisqRegion <- svychisq(~MDDDiary + regiontype, design = ChisqTab) # Not significant

#################################################################################################################

#5. Protein

#a. Protein by Treatment
SvyProtein <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(Treatment, MDDProtein) %>%
  summarise(MDDProteinCat = survey_mean() * 100) %>%
  filter(MDDProtein == 1) %>%
  dplyr::select(-c(MDDProteinCat_se, MDDProtein)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = MDDProteinCat) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "Protein") %>%
  dplyr::select(Indicator, everything())

#b. Protein by Region
SvyProteinRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(regiontype, MDDProtein) %>%
  summarise(MDDProteinCat = survey_mean() * 100) %>%
  filter(MDDProtein == 1) %>%
  dplyr::select(-c(MDDProteinCat_se, MDDProtein)) %>%
  pivot_wider(names_from = regiontype, values_from = MDDProteinCat) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "Protein") %>%
  dplyr::select(Indicator, everything())

# Join the two dataframes
ProteinData <- SvyProtein %>% 
  left_join(SvyProteinRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for Protein

ProteinChisqIE <- svychisq(~MDDProtein + Treatment, design = ChisqTab) # Not significant
ProteinChisqRegion <- svychisq(~MDDProtein + regiontype, design = ChisqTab) # Not significant

#################################################################################################################

#6. Eggs


#a. Eggs by Treatment

SvyEggs <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(Treatment, MDDEggs) %>%
  summarise(MDDEggsCat = survey_mean() * 100) %>%
  filter(MDDEggs == 1) %>%
  dplyr::select(-c(MDDEggsCat_se, MDDEggs)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = MDDEggsCat) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "Eggs") %>%
  dplyr::select(Indicator, everything())

#b. Eggs by Region

SvyEggsRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(regiontype, MDDEggs) %>%
  summarise(MDDEggsCat = survey_mean() * 100) %>%
  filter(MDDEggs == 1) %>%
  dplyr::select(-c(MDDEggsCat_se, MDDEggs)) %>%
  pivot_wider(names_from = regiontype, values_from = MDDEggsCat) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "Eggs") %>%
  dplyr::select(Indicator, everything())

# Join the two dataframes
EggsData <- SvyEggs %>% 
  left_join(SvyEggsRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for Eggs

EggsChisqIE <- svychisq(~MDDEggs + Treatment, design = ChisqTab) # Not significant
EggsChisqRegion <- svychisq(~MDDEggs + regiontype, design = ChisqTab) # Not significant

#################################################################################################################

# 7. Dark Green Vegetable

#a. Dark Green Vegetables by Treatment

SvyDarkGreenVeg <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(Treatment, MDDDarkGreenVeg) %>%
  summarise(MDDDarkGreenVegCat = survey_mean() * 100) %>%
  filter(MDDDarkGreenVeg == 1) %>%
  dplyr::select(-c(MDDDarkGreenVegCat_se, MDDDarkGreenVeg)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = MDDDarkGreenVegCat) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "Dark Green Vegetables") %>%
  dplyr::select(Indicator, everything())

#b. Dark Green Vegetables by Region

SvyDarkGreenVegRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(regiontype, MDDDarkGreenVeg) %>%
  summarise(MDDDarkGreenVegCat = survey_mean() * 100) %>%
  filter(MDDDarkGreenVeg == 1) %>%
  dplyr::select(-c(MDDDarkGreenVegCat_se, MDDDarkGreenVeg)) %>%
  pivot_wider(names_from = regiontype, values_from = MDDDarkGreenVegCat) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "Dark Green Vegetables") %>%
  dplyr::select(Indicator, everything())

# Join the two dataframes
DarkGreenVegData <- SvyDarkGreenVeg %>% 
  left_join(SvyDarkGreenVegRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for Dark Green Vegetables

DarkGreenVegChisqIE <- svychisq(~MDDDarkGreenVeg + Treatment, design = ChisqTab) # Not significant
DarkGreenVegChisqRegion <- svychisq(~MDDDarkGreenVeg + regiontype, design = ChisqTab) # significant at 10%

#################################################################################################################

# 8. Other Vegetables

#a. Other Vegetables by Treatment

SvyOtherVeg <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(Treatment, MDDOtherVeg) %>%
  summarise(MDDOtherVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVeg == 1) %>%
  dplyr::select(-c(MDDOtherVegCat_se, MDDOtherVeg)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = MDDOtherVegCat) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "Other Vegetables") %>%
  dplyr::select(Indicator, everything())

#b. Other Vegetables by Region

SvyOtherVegRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(regiontype, MDDOtherVeg) %>%
  summarise(MDDOtherVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVeg == 1) %>%
  dplyr::select(-c(MDDOtherVegCat_se, MDDOtherVeg)) %>%
  pivot_wider(names_from = regiontype, values_from = MDDOtherVegCat) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "Other Vegetables") %>%
  dplyr::select(Indicator, everything())

# Join the two dataframes
OtherVegData <- SvyOtherVeg %>% 
  left_join(SvyOtherVegRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for Other Vegetables
OtherVegChisqIE <- svychisq(~MDDOtherVeg + Treatment, design = ChisqTab) # Not significant
OtherVegChisqRegion <- svychisq(~MDDOtherVeg + regiontype, design = ChisqTab) # Not significant

#################################################################################################################

# 9. Vitamin A Rich Fruits and Vegetables

#a. Vitamin A Rich Fruits and Vegetables by Treatment

SvyOtherVitA <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(Treatment, MDDOtherVitA) %>%
  summarise(MDDVitAFruitVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVitA == 1) %>%
  dplyr::select(-c(MDDVitAFruitVegCat_se, MDDOtherVitA)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = MDDVitAFruitVegCat) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "Other Vitamin A Foods") %>%
  dplyr::select(Indicator, everything())

#b. Vitamin A Rich Fruits and Vegetables by Region

SvyOtherVitARegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(regiontype, MDDOtherVitA) %>%
  summarise(MDDVitAFruitVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVitA == 1) %>%
  dplyr::select(-c(MDDVitAFruitVegCat_se, MDDOtherVitA)) %>%
  pivot_wider(names_from = regiontype, values_from = MDDVitAFruitVegCat) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "Other Vitamin A Foods") %>%
  dplyr::select(Indicator, everything())

# Join the two dataframes

OtherVitAData <- SvyOtherVitA %>% 
  left_join(SvyOtherVitARegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for Vitamin A Rich Fruits and Vegetables

OtherVitAChisqIE <- svychisq(~MDDOtherVitA + Treatment, design = ChisqTab) # Not significant
OtherVitAChisqRegion <- svychisq(~MDDOtherVitA + regiontype, design = ChisqTab) # Not significant

#################################################################################################################


# 10. Other Fruits

#a. Other Fruits by Treatment

SvyOtherFruits <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(Treatment, MDDOtherFruits) %>%
  summarise(MDDOtherFruitsCat = survey_mean() * 100) %>%
  filter(MDDOtherFruits == 1) %>%
  dplyr::select(-c(MDDOtherFruitsCat_se, MDDOtherFruits)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = MDDOtherFruitsCat) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "Other Fruits") %>%
  dplyr::select(Indicator, everything())

#b. Other Fruits by Region

SvyOtherFruitsRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15 & MDDAge <=49) %>%
  filter(MDDGender == "Female") %>%
  group_by(regiontype, MDDOtherFruits) %>%
  summarise(MDDOtherFruitsCat = survey_mean() * 100) %>%
  filter(MDDOtherFruits == 1) %>%
  dplyr::select(-c(MDDOtherFruitsCat_se, MDDOtherFruits)) %>%
  pivot_wider(names_from = regiontype, values_from = MDDOtherFruitsCat) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "Other Fruits") %>%
  dplyr::select(Indicator, everything())

# Join the two dataframes
OtherFruitsData <- SvyOtherFruits %>% 
  left_join(SvyOtherFruitsRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for Other Fruits

OtherFruitsChisqIE <- svychisq(~MDDOtherFruits + Treatment, design = ChisqTab) # Not significant
OtherFruitsChisqRegion <- svychisq(~MDDOtherFruits + regiontype, design = ChisqTab) # Not significant

#################################################################################################################

# Merge the tables for all the food groups

DQQFoodGroupsTable <- bind_rows(StaplesData, PulsesData, NutsSeedsData, 
                           DairyData, ProteinData, EggsData, DarkGreenVegData, 
                           OtherVegData, OtherVitAData, OtherFruitsData) %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

# Export the table to an excel file
write.xlsx(DQQFoodGroupsTable, "report tables/DQQFoodGroupsTable.xlsx")


##################################################################################################################

# Calculate the NCD Scores

#1. NCD - Protective Score

# a. NCD Protective Foods by Treatment
SvyNCDProtectiveFoods <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(NCDProtScore = survey_mean(NCDProtScore)) %>% 
  dplyr::select(-c(NCDProtScore_se)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = NCDProtScore) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "NCD Protective Foods") %>%
  dplyr::select(Indicator, everything())

# b. NCD Protective Foods by Region
SvyNCDProtectiveFoodsRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype) %>%
  summarise(NCDProtScore = survey_mean(NCDProtScore)) %>% 
  dplyr::select(-c(NCDProtScore_se)) %>%
  pivot_wider(names_from = regiontype, values_from = NCDProtScore) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "NCD Protective Foods") %>%
  dplyr::select(Indicator, everything())

#c. NCD Protective Foods by Gender
SvyNCDProtectiveFoodsGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender) %>%
  summarise(NCDProtScore = survey_mean(NCDProtScore)) %>% 
  dplyr::select(-c(NCDProtScore_se)) %>%
  pivot_wider(names_from = MDDGender, values_from = NCDProtScore) %>%
  mutate(DiffGender = Male - Female,
         Indicator = "NCD Protective Foods") %>%
  dplyr::select(Indicator, everything())

# NCD Protective Foods overall
NCDProtectiveFoodsOveral <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  summarise(NCDProtScore = survey_mean(NCDProtScore)) %>% 
  dplyr::select(-c(NCDProtScore_se)) %>%
  mutate(Indicator = "NCD Protective Foods")

# Join the three dataframes - using the left join function
NCDProtectiveFoodsTable <- SvyNCDProtectiveFoods %>% 
  left_join(SvyNCDProtectiveFoodsRegion, by = "Indicator") %>% 
  left_join(SvyNCDProtectiveFoodsGender, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

##############################################################################################################

# Perform ttests for NCD Protective Foods
NCDProtectiveFoodsTreatTest <- svyttest(NCDProtScore ~ Treatment, design = SvyDietQualityData) # Significant at 10%
NCDProtectiveFoodsRegionTest <- svyttest(NCDProtScore ~ regiontype, design = SvyDietQualityData) # Not statistically significant
NCDProtectiveFoodsGenderTest <- svyttest(NCDProtScore ~ MDDGender, design = SvyDietQualityData) # Significant at 5%

###############################################################################################################


#2. NCD Risk Score

# a. NCD Risk Score by Treatment
SvyNCDRiskScore <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(NCDRiskScore = survey_mean(NCDRiskScore)) %>% 
  dplyr::select(-c(NCDRiskScore_se)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = NCDRiskScore) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "NCD Risk Score") %>%
  dplyr::select(Indicator, everything())

# b. NCD Risk Score by Region

SvyNCDRiskScoreRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype) %>%
  summarise(NCDRiskScore = survey_mean(NCDRiskScore)) %>% 
  dplyr::select(-c(NCDRiskScore_se)) %>%
  pivot_wider(names_from = regiontype, values_from = NCDRiskScore) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "NCD Risk Score") %>%
  dplyr::select(Indicator, everything())

#c. NCD Risk by gender
SvyNCDRiskScoreGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender) %>%
  summarise(NCDRiskScore = survey_mean(NCDRiskScore)) %>% 
  dplyr::select(-c(NCDRiskScore_se)) %>%
  pivot_wider(names_from = MDDGender, values_from = NCDRiskScore) %>%
  mutate(DiffGender = Male - Female,
         Indicator = "NCD Risk Score") %>%
  dplyr::select(Indicator, everything())

# NCD Risk score overall
NCDRiskScoreOveral <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  summarise(NCDRiskScore = survey_mean(NCDRiskScore)) %>% 
  dplyr::select(-c(NCDRiskScore_se)) %>%
  mutate(Indicator = "NCD Risk Score")

# Join the three dataframes - using the left join function
NCDRiskScoreTable <- SvyNCDRiskScore %>% 
  left_join(SvyNCDRiskScoreRegion, by = "Indicator") %>% 
  left_join(SvyNCDRiskScoreGender, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

##############################################################################################################

# Perform ttests for NCD Risk Score
NCDRiskScoreTreatTest <- svyttest(NCDRiskScore ~ Treatment, design = SvyDietQualityData) # Not significant
NCDRiskScoreRegionTest <- svyttest(NCDRiskScore ~ regiontype, design = SvyDietQualityData) #No longer Significant
NCDRiskScoreGenderTest <- svyttest(NCDRiskScore ~ MDDGender, design = SvyDietQualityData) # Not significant

###############################################################################################################


#3. GDS Score

# a. GDS Score by Treatment

SvyGDS <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(GDRScore = survey_mean(GDRScore)) %>% 
  dplyr::select(-c(GDRScore_se)) %>%
  rename(Disaggregation = Treatment) %>%
  pivot_wider(names_from = Disaggregation, values_from = GDRScore) %>%
  mutate(DiffIE = `Treatment Group` - `Control Group`,
         Indicator = "GDS Score") %>%
  dplyr::select(Indicator, everything())

# b. GDS Score by Region

SvyGDSRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype) %>%
  summarise(GDRScore = survey_mean(GDRScore)) %>% 
  dplyr::select(-c(GDRScore_se)) %>%
  pivot_wider(names_from = regiontype, values_from = GDRScore) %>%
  mutate(DiffReg = URBAN - RURAL,
         Indicator = "GDS Score") %>%
  dplyr::select(Indicator, everything())

#c. GDS Score by gender

SvyGDSGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender) %>%
  summarise(GDRScore = survey_mean(GDRScore)) %>% 
  dplyr::select(-c(GDRScore_se)) %>%
  pivot_wider(names_from = MDDGender, values_from = GDRScore) %>%
  mutate(DiffGender = Male - Female,
         Indicator = "GDS Score") %>%
  dplyr::select(Indicator, everything())

# Overall DGS Score
GDSScoreOveral <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  summarise(GDRScore = survey_mean(GDRScore)) %>% 
  dplyr::select(-c(GDRScore_se)) %>%
  mutate(Indicator = "GDS Score")



# Join the three dataframes - using the left join function
GDSScoreTable <- SvyGDS %>% 
  left_join(SvyGDSRegion, by = "Indicator") %>% 
  left_join(SvyGDSGender, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

##############################################################################################################

# Perform ttests for GDS Score


GDSScoreTreatTest <- svyttest(GDRScore ~ Treatment, design = SvyDietQualityData) # Not significant
GDSScoreRegionTest <- svyttest(GDRScore ~ regiontype, design = SvyDietQualityData) # Not significant
GDSScoreGenderTest <- svyttest(GDRScore ~ MDDGender, design = SvyDietQualityData) # Highly significant at 1%

###############################################################################################################

# Merge the tables for all the NCD Scores

NCDScoresTable <- bind_rows(NCDProtectiveFoodsTable, NCDRiskScoreTable, GDSScoreTable) %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

# Export the table to an excel file

write.xlsx(NCDScoresTable, "report tables/NCDScoresTable.xlsx")

###############################################################################################################





























































































