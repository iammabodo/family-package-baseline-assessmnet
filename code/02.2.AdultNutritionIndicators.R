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

# Calculate MDDScore, by treatment
MDDScoreTreat <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment) %>%
  summarise(MDDScore = survey_mean(MDDScore),
            Total = survey_total()) %>% 
  select(-c("MDDScore_se", "Total_se")) 

# Write into an excel file
write.xlsx(MDDScore, "report tables/MDDScore.xlsx")

# Calculate MDD Score by gender
MDDScoreGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender) %>%
  summarise(MDDScore = survey_mean(MDDScore),
            Total = survey_total())

# Write into an excel file
write.xlsx(MDDScoreGender, "report tables/MDDScoreGender.xlsx")

# Chalculate MDD Score by region
MDDScoreRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype) %>%
  summarise(MDDScore = survey_mean(MDDScore),
            Total = survey_total())

# Write into an excel file

write.xlsx(MDDScoreRegion, "report tables/MDDScoreRegion.xlsx")


# Calculate MDD Score overall
MDDScoreTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  summarise(MDDScore = survey_mean(MDDScore),
            Total = survey_total())

# Write into an excel file
write.xlsx(MDDScoreTot, "report tables/MDDScoreTot.xlsx")

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
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDStaples) %>%
  summarise(MDDStaplesCat = survey_mean() * 100) %>%
  filter(MDDStaples == 1) 

# Write this into an excel file
write.xlsx(SvyStaples, "report tables/SvyStaples.xlsx")


#b. Staples by Region
SvyStaplesRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDStaples) %>%
  summarise(MDDStaplesCat = survey_mean() * 100) %>%
  filter(MDDStaples == 1)

# Write this into an excel file
write.xlsx(SvyStaplesRegion, "report tables/SvyStaplesRegion.xlsx")

#c Calculate Staples by gender

SvyStaplesGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDStaples) %>%
  summarise(MDDStaplesCat = survey_mean() * 100) %>%
  filter(MDDStaples == 1)

# Write this into an excel file
write.xlsx(SvyStaplesGender, "report tables/SvyStaplesGender.xlsx")


#d Calculate overall Staples

SvyStaplesTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDStaples) %>%
  summarise(MDDStaplesCat = survey_mean() * 100) %>%
  filter(MDDStaples == 1)

# Write this into an excel file
write.xlsx(SvyStaplesTot, "report tables/SvyStaplesTot.xlsx")

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
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDPulses) %>%
  summarise(MDDPulsesCat = survey_mean() * 100) %>%
  filter(MDDPulses == 1) 

# Write this into an excel file
write.xlsx(SvyPulses, "report tables/SvyPulses.xlsx")

#b. Pulses by Region
SvyPulsesRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDPulses) %>%
  summarise(MDDPulsesCat = survey_mean() * 100) %>%
  filter(MDDPulses == 1)

# Write this into an excel file
write.xlsx(SvyPulsesRegion, "report tables/SvyPulsesRegion.xlsx")

#c Calculate Pulses by gender

SvyPulsesGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDPulses) %>%
  summarise(MDDPulsesCat = survey_mean() * 100) %>%
  filter(MDDPulses == 1)

# Write this into an excel file
write.xlsx(SvyPulsesGender, "report tables/SvyPulsesGender.xlsx")

# D Calculate overall Pulses

SvyPulsesTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDPulses) %>%
  summarise(MDDPulsesCat = survey_mean() * 100) %>%
  filter(MDDPulses == 1)



# Join the two dataframes

PulsesData <- SvyPulses %>% 
  left_join(SvyPulsesRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for pulses
PulsesChisqIE <- svychisq(~MDDPulses + Treatment, design = ChisqTab) # Not significant
PulsesChisqRegion <- svychisq(~MDDPulses + regiontype, design = ChisqTab) # Not significant
PulsesChisqGender <- svychisq(~MDDPulses + MDDGender, design = SvyDietQualityData) 

#################################################################################################################

#3. NutsSeeds
#a. NutsSeeds by Treatment

SvyNutsSeeds <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDNutsSeeds) %>%
  summarise(MDDNutsSeedsCat = survey_mean() * 100) %>%
  filter(MDDNutsSeeds == 1)

# Write this into an excel file
write.xlsx(SvyNutsSeeds, "report tables/SvyNutsSeeds.xlsx")
  
#b. NutsSeeds by Region

SvyNutsSeedsRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDNutsSeeds) %>%
  summarise(MDDNutsSeedsCat = survey_mean() * 100) %>%
  filter(MDDNutsSeeds == 1)

# Write this into an excel file
write.xlsx(SvyNutsSeedsRegion, "report tables/SvyNutsSeedsRegion.xlsx")

#c Calculate NutsSeeds by gender

SvyNutsSeedsGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDNutsSeeds) %>%
  summarise(MDDNutsSeedsCat = survey_mean() * 100) %>%
  filter(MDDNutsSeeds == 1)

# Write this into an excel file
write.xlsx(SvyNutsSeedsGender, "report tables/SvyNutsSeedsGender.xlsx")

# D Calculate overall NutsSeeds

SvyNutsSeedsTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDNutsSeeds) %>%
  summarise(MDDNutsSeedsCat = survey_mean() * 100) %>%
  filter(MDDNutsSeeds == 1)

# Join the two dataframes
NutsSeedsData <- SvyNutsSeeds %>% 
  left_join(SvyNutsSeedsRegion, by = "Indicator") %>% 
  # round all the numeric variables to 2 decimal places
  mutate_if(is.numeric, ~round(., 2))

#################################################################################################################

# Chisq test for Nuts and Seeds
NutsSeedsChisqIE <- svychisq(~MDDNutsSeeds + Treatment, design = ChisqTab) # Not significant
NutsSeedsChisqRegion <- svychisq(~MDDNutsSeeds + regiontype, design = ChisqTab) # Not significant
NutsSeedsChisqGender <- svychisq(~MDDNutsSeeds + MDDGender, design = SvyDietQualityData) 
#################################################################################################################

#4. Dairy
#a. Dairy by Treatment
SvyDairy <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDDiary) %>%
  summarise(MDDDairyCat = survey_mean() * 100) %>%
  filter(MDDDiary == 1) 

# Write this into an excel file
write.xlsx(SvyDairy, "report tables/SvyDairy.xlsx")

#b. Dairy by Region
SvyDairyRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDDiary) %>%
  summarise(MDDDairyCat = survey_mean() * 100) %>%
  filter(MDDDiary == 1) 

# Write this into an excel file
write.xlsx(SvyDairyRegion, "report tables/SvyDairyRegion.xlsx")

#c Calculate Diary by gender

SvyDairyGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDDiary) %>%
  summarise(MDDDairyCat = survey_mean() * 100) %>%
  filter(MDDDiary == 1)

# Write this into an excel file
write.xlsx(SvyDairyGender, "report tables/SvyDairyGender.xlsx")

# D Calculate overall Dairy

SvyDairyTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDDiary) %>%
  summarise(MDDDairyCat = survey_mean() * 100) %>%
  filter(MDDDiary == 1)

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
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDProtein) %>%
  summarise(MDDProteinCat = survey_mean() * 100) %>%
  filter(MDDProtein == 1) 

# Write this into an excel file
write.xlsx(SvyProtein, "report tables/SvyProtein.xlsx")

#b. Protein by Region
SvyProteinRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDProtein) %>%
  summarise(MDDProteinCat = survey_mean() * 100) %>%
  filter(MDDProtein == 1) 

# Write this into an excel file
write.xlsx(SvyProteinRegion, "report tables/SvyProteinRegion.xlsx")

#c Calculate Protein by gender

SvyProteinGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDProtein) %>%
  summarise(MDDProteinCat = survey_mean() * 100) %>%
  filter(MDDProtein == 1)

# Write this into an excel file
write.xlsx(SvyProteinGender, "report tables/SvyProteinGender.xlsx")

# D Calculate overall Protein

SvyProteinTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDProtein) %>%
  summarise(MDDProteinCat = survey_mean() * 100) %>%
  filter(MDDProtein == 1)

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
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDEggs) %>%
  summarise(MDDEggsCat = survey_mean() * 100) %>%
  filter(MDDEggs == 1) 

# Write this into an excel file
write.xlsx(SvyEggs, "report tables/SvyEggs.xlsx")

#b. Eggs by Region

SvyEggsRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDEggs) %>%
  summarise(MDDEggsCat = survey_mean() * 100) %>%
  filter(MDDEggs == 1) 

# Write this into an excel file
write.xlsx(SvyEggsRegion, "report tables/SvyEggsRegion.xlsx")

#c. Calculate Eggs by gender

SvyEggsGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDEggs) %>%
  summarise(MDDEggsCat = survey_mean() * 100) %>%
  filter(MDDEggs == 1)

# Write this into an excel file
write.xlsx(SvyEggsGender, "report tables/SvyEggsGender.xlsx")

#d. Calculate overall Eggs

SvyEggsTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDEggs) %>%
  summarise(MDDEggsCat = survey_mean() * 100) %>%
  filter(MDDEggs == 1)

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
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDDarkGreenVeg) %>%
  summarise(MDDDarkGreenVegCat = survey_mean() * 100) %>%
  filter(MDDDarkGreenVeg == 1) 

# Write this into an excel file
write.xlsx(SvyDarkGreenVeg, "report tables/SvyDarkGreenVeg.xlsx")

#b. Dark Green Vegetables by Region

SvyDarkGreenVegRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDDarkGreenVeg) %>%
  summarise(MDDDarkGreenVegCat = survey_mean() * 100) %>%
  filter(MDDDarkGreenVeg == 1) 

# Write this into an excel file
write.xlsx(SvyDarkGreenVegRegion, "report tables/SvyDarkGreenVegRegion.xlsx")

#c. Calculate Dark Green Vegetables by gender

SvyDarkGreenVegGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDDarkGreenVeg) %>%
  summarise(MDDDarkGreenVegCat = survey_mean() * 100) %>%
  filter(MDDDarkGreenVeg == 1)

# Write this into an excel file
write.xlsx(SvyDarkGreenVegGender, "report tables/SvyDarkGreenVegGender.xlsx")

# d Calculate overall Dark Green Vegetables

SvyDarkGreenVegTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDDarkGreenVeg) %>%
  summarise(MDDDarkGreenVegCat = survey_mean() * 100) %>%
  filter(MDDDarkGreenVeg == 1)

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
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDOtherVeg) %>%
  summarise(MDDOtherVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVeg == 1) 

# Write this into an excel file
write.xlsx(SvyOtherVeg, "report tables/SvyOtherVeg.xlsx")

#b. Other Vegetables by Region

SvyOtherVegRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDOtherVeg) %>%
  summarise(MDDOtherVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVeg == 1)

# Write this into an excel file
write.xlsx(SvyOtherVegRegion, "report tables/SvyOtherVegRegion.xlsx")

#c. Calculate Other Vegetables by gender

SvyOtherVegGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDOtherVeg) %>%
  summarise(MDDOtherVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVeg == 1)

# Write this into an excel file
write.xlsx(SvyOtherVegGender, "report tables/SvyOtherVegGender.xlsx")

# d Calculate overall Other Vegetables

SvyOtherVegTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDOtherVeg) %>%
  summarise(MDDOtherVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVeg == 1)

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
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDOtherVitA) %>%
  summarise(MDDVitAFruitVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVitA == 1) 

# Write this into an excel file
write.xlsx(SvyOtherVitA, "report tables/SvyOtherVitA.xlsx")

#b. Vitamin A Rich Fruits and Vegetables by Region

SvyOtherVitARegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDOtherVitA) %>%
  summarise(MDDVitAFruitVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVitA == 1) 


# Write this into an excel file
write.xlsx(SvyOtherVitARegion, "report tables/SvyOtherVitARegion.xlsx")

#c. Calculate Vitamin A Rich Fruits and Veget by gender

SvyOtherVitAGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDOtherVitA) %>%
  summarise(MDDVitAFruitVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVitA == 1)

# Write this into an excel file

write.xlsx(SvyOtherVitAGender, "report tables/SvyOtherVitAGender.xlsx")

# d Calculate overall Vitamin A Rich Fruits and Vegetables

SvyOtherVitATot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDOtherVitA) %>%
  summarise(MDDVitAFruitVegCat = survey_mean() * 100) %>%
  filter(MDDOtherVitA == 1)

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
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDOtherFruits) %>%
  summarise(MDDOtherFruitsCat = survey_mean() * 100) %>%
  filter(MDDOtherFruits == 1)

# Write this into an excel file
write.xlsx(SvyOtherFruits, "report tables/SvyOtherFruits.xlsx")

#b. Other Fruits by Region

SvyOtherFruitsRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDOtherFruits) %>%
  summarise(MDDOtherFruitsCat = survey_mean() * 100) %>%
  filter(MDDOtherFruits == 1) 

# Write this into an excel file
write.xlsx(SvyOtherFruitsRegion, "report tables/SvyOtherFruitsRegion.xlsx")

# Calculate Other fruits by gender

SvyOtherFruitsGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDOtherFruits) %>%
  summarise(MDDOtherFruitsCat = survey_mean() * 100) %>%
  filter(MDDOtherFruits == 1)

# Write this into an excel file

write.xlsx(SvyOtherFruitsGender, "report tables/SvyOtherFruitsGender.xlsx")

# d Calculate overall Other Fruits

SvyOtherFruitsTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDOtherFruits) %>%
  summarise(MDDOtherFruitsCat = survey_mean() * 100) %>%
  filter(MDDOtherFruits == 1)

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

# Consumption of Unheatlhy Foods by treatment

#1. Sugar Sweetened Beverages

#a. Sugar Sweetened Beverages by Treatment

SvySSBTreat <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDSweetBeverages) %>%
  summarise(MDDSSBCat = survey_mean() * 100) %>%
  filter(MDDSweetBeverages == 1)

# Write this into an excel file
write.xlsx(SvySSBTreat, "report tables/SvySSBTreat.xlsx")

#b. Sugar Sweetened Beverages by Region

SvySSBRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDSweetBeverages) %>%
  summarise(MDDSSBCat = survey_mean() * 100) %>%
  filter(MDDSweetBeverages == 1)

# Write this into an excel file
write.xlsx(SvySSBRegion, "report tables/SvySSBRegion.xlsx")

#c. Calculate Sugar Sweetened Bever

SvySSBGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDSweetBeverages) %>%
  summarise(MDDSSBCat = survey_mean() * 100) %>%
  filter(MDDSweetBeverages == 1)

# Write this into an excel file

write.xlsx(SvySSBGender, "report tables/SvySSBGender.xlsx")

# d Calculate overall Sugar Sweetened Beverages

SvySSBTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDSweetBeverages) %>%
  summarise(MDDSSBCat = survey_mean() * 100) %>%
  filter(MDDSweetBeverages == 1)

########################################################################
# Tests for Sugar Sweetened Beverages
SSBTreatChisq <- svychisq(~MDDSweetBeverages + Treatment, design = SvyDietQualityData) # Not significant
SSBRegionChisq <- svychisq(~MDDSweetBeverages + regiontype, design = SvyDietQualityData) #1% significant
SSBGenderChisq <- svychisq(~MDDSweetBeverages + MDDGender, design = SvyDietQualityData) # 5% significant
########################################################################

# Calculate the consumption of unhealthy foods


#2. Fast Foods

#a. Fast Foods by Treatment

SvyFastFoods <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDUnhealthyFoods) %>%
  summarise(MDDFastFoodsCat = survey_mean() * 100) %>%
  filter(MDDUnhealthyFoods== 1)

# Write this into an excel file
write.xlsx(SvyFastFoods, "report tables/SvyFastFoods.xlsx")

#b. Fast Foods by Region

SvyFastFoodsRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDUnhealthyFoods) %>%
  summarise(MDDFastFoodsCat = survey_mean() * 100) %>%
  filter(MDDUnhealthyFoods == 1)

# Write this into an excel file
write.xlsx(SvyFastFoodsRegion, "report tables/SvyFastFoodsRegion.xlsx")

#c. Calculate Fast Foods by gender

SvyFastFoodsGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDUnhealthyFoods) %>%
  summarise(MDDFastFoodsCat = survey_mean() * 100) %>%
  filter(MDDUnhealthyFoods == 1)

# Write this into an excel file

write.xlsx(SvyFastFoodsGender, "report tables/SvyFastFoodsGender.xlsx")

# d Calculate overall Fast Foods

SvyFastFoodsTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDUnhealthyFoods) %>%
  summarise(MDDFastFoodsCat = survey_mean() * 100) %>%
  filter(MDDUnhealthyFoods == 1)

###########################################################

# Tests for Fast Foods

FastFoodsChisqIE <- svychisq(~MDDUnhealthyFoods + Treatment, design = SvyDietQualityData) # Not significant
FastFoodsChisqRegion <- svychisq(~MDDUnhealthyFoods + regiontype, design = SvyDietQualityData) # 5% significant
FastFoodsChisqGender <- svychisq(~MDDUnhealthyFoods + MDDGender, design = SvyDietQualityData) # Not significant

##########################################################

#3. Baked/grain based sweets

#a. Baked/grain based sweets by Treatment

SvyBakedGoods <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDSweetsCake) %>%
  summarise(MDDBakedGoodsCat = survey_mean() * 100) %>%
  filter(MDDSweetsCake == 1)

# Write this into an excel file
write.xlsx(SvyBakedGoods, "report tables/SvyBakedGoods.xlsx")

#b. Baked/grain based sweets by Region

SvyBakedGoodsRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDSweetsCake) %>%
  summarise(MDDBakedGoodsCat = survey_mean() * 100) %>%
  filter(MDDSweetsCake == 1)

# Write this into an excel file
write.xlsx(SvyBakedGoodsRegion, "report tables/SvyBakedGoodsRegion.xlsx")

#c. Calculate Baked/grain based sweets by gender

SvyBakedGoodsGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDSweetsCake) %>%
  summarise(MDDBakedGoodsCat = survey_mean() * 100) %>%
  filter(MDDSweetsCake == 1)

# Write this into an excel file

write.xlsx(SvyBakedGoodsGender, "report tables/SvyBakedGoodsGender.xlsx")

# d Calculate overall Baked/grain based sweets

SvyBakedGoodsTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDSweetsCake) %>%
  summarise(MDDBakedGoodsCat = survey_mean() * 100) %>%
  filter(MDDSweetsCake == 1)

###########################################################

# Tests for Baked/grain based sweets

BakedGoodsChisqIE <- svychisq(~MDDSweetsCake + Treatment, design = SvyDietQualityData) # Not significant
BakedGoodsChisqRegion <- svychisq(~MDDSweetsCake + regiontype, design = SvyDietQualityData) # Not significant
BakedGoodsChisqGender <- svychisq(~MDDSweetsCake + MDDGender, design = SvyDietQualityData) # Not significant

##########################################################

#4. Other sweets

#a. Other sweets by Treatment

SvyOtherSweets <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDSweetsCandy) %>%
  summarise(MDDOtherSweetsCat = survey_mean() * 100) %>%
  filter(MDDSweetsCandy == 1)

# Write this into an excel file
write.xlsx(SvyOtherSweets, "report tables/SvyOtherSweets.xlsx")

#b. Other sweets by Region

SvyOtherSweetsRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDSweetsCandy) %>%
  summarise(MDDOtherSweetsCat = survey_mean() * 100) %>%
  filter(MDDSweetsCandy == 1)

# Write this into an excel file
write.xlsx(SvyOtherSweetsRegion, "report tables/SvyOtherSweetsRegion.xlsx")

#c. Calculate Other sweets by gender

SvyOtherSweetsGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDSweetsCandy) %>%
  summarise(MDDOtherSweetsCat = survey_mean() * 100) %>%
  filter(MDDSweetsCandy == 1)

# Write this into an excel file

write.xlsx(SvyOtherSweetsGender, "report tables/SvyOtherSweetsGender.xlsx")

# d Calculate overall Other sweets

SvyOtherSweetsTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDSweetsCandy) %>%
  summarise(MDDOtherSweetsCat = survey_mean() * 100) %>%
  filter(MDDSweetsCandy == 1)

###########################################################
# Other sweets tests

OtherSweetsChisqIE <- svychisq(~MDDSweetsCandy + Treatment, design = SvyDietQualityData) # Not significant
OtherSweetsChisqRegion <- svychisq(~MDDSweetsCandy + regiontype, design = SvyDietQualityData) # Not significant
OtherSweetsChisqGender <- svychisq(~MDDSweetsCandy + MDDGender, design = SvyDietQualityData) # Not significant

##########################################################


# Packed snacks

#a. Packed snacks by Treatment

SvyPackedSnacks <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDOtherChips) %>%
  summarise(MDDPackedSnacksCat = survey_mean() * 100) %>%
  filter(MDDOtherChips == 1)

# Write this into an excel file

write.xlsx(SvyPackedSnacks, "report tables/SvyPackedSnacks.xlsx")


#b. Packed snacks by Region

SvyPackedSnacksRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDOtherChips) %>%
  summarise(MDDPackedSnacksCat = survey_mean() * 100) %>%
  filter(MDDOtherChips == 1)

# Write this into an excel file

write.xlsx(SvyPackedSnacksRegion, "report tables/SvyPackedSnacksRegion.xlsx")

#c. Calculate Packed snacks by gender

SvyPackedSnacksGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDOtherChips) %>%
  summarise(MDDPackedSnacksCat = survey_mean() * 100) %>%
  filter(MDDOtherChips == 1)

# Write this into an excel file

write.xlsx(SvyPackedSnacksGender, "report tables/SvyPackedSnacksGender.xlsx")

# d Calculate overall Packed snacks

SvyPackedSnacksTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDOtherChips) %>%
  summarise(MDDPackedSnacksCat = survey_mean() * 100) %>%
  filter(MDDOtherChips == 1)

###########################################################

# Packed snacks tests

PackedSnacksChisqIE <- svychisq(~MDDOtherChips + Treatment, design = SvyDietQualityData) # Not significant
PackedSnacksChisqRegion <- svychisq(~MDDOtherChips + regiontype, design = SvyDietQualityData) # Not significant
PackedSnacksChisqGender <- svychisq(~MDDOtherChips + MDDGender, design = SvyDietQualityData) # Not significant

##########################################################

# Instant noodles

#a. Instant noodles by Treatment

SvyInstantNoodles <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDOtherNoodles) %>%
  summarise(MDDInstantNoodlesCat = survey_mean() * 100) %>%
  filter(MDDOtherNoodles == 1)

# Write this into an excel file

write.xlsx(SvyInstantNoodles, "report tables/SvyInstantNoodles.xlsx")

#b. Instant noodles by Region

SvyInstantNoodlesRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDOtherNoodles) %>%
  summarise(MDDInstantNoodlesCat = survey_mean() * 100) %>%
  filter(MDDOtherNoodles == 1)

# Write this into an excel file

write.xlsx(SvyInstantNoodlesRegion, "report tables/SvyInstantNoodlesRegion.xlsx")

#c. Calculate Instant noodles by gender


SvyInstantNoodlesGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDOtherNoodles) %>%
  summarise(MDDInstantNoodlesCat = survey_mean() * 100) %>%
  filter(MDDOtherNoodles == 1)

# Write this into an excel file

write.xlsx(SvyInstantNoodlesGender, "report tables/SvyInstantNoodlesGender.xlsx")

# d Calculate overall Instant noodles

SvyInstantNoodlesTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDOtherNoodles) %>%
  summarise(MDDInstantNoodlesCat = survey_mean() * 100) %>%
  filter(MDDOtherNoodles == 1)

###########################################################

# Instant noodles tests

InstantNoodlesChisqIE <- svychisq(~MDDOtherNoodles + Treatment, design = SvyDietQualityData) # Not significant
InstantNoodlesChisqRegion <- svychisq(~MDDOtherNoodles + regiontype, design = SvyDietQualityData) # Not significant
InstantNoodlesChisqGender <- svychisq(~MDDOtherNoodles + MDDGender, design = SvyDietQualityData) # Not significant

##########################################################

# Deep fried snacks

#a. Deep fried snacks by Treatment

SvyDeepFriedSnacks <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDOtherFriedFoods) %>%
  summarise(MDDDeepFriedSnacksCat = survey_mean() * 100) %>%
  filter(MDDOtherFriedFoods == 1)

# Write this into an excel file

write.xlsx(SvyDeepFriedSnacks, "report tables/SvyDeepFriedSnacks.xlsx")

#b. Deep fried snacks by Region

SvyDeepFriedSnacksRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDOtherFriedFoods) %>%
  summarise(MDDDeepFriedSnacksCat = survey_mean() * 100) %>%
  filter(MDDOtherFriedFoods == 1)

# Write this into an excel file

write.xlsx(SvyDeepFriedSnacksRegion, "report tables/SvyDeepFriedSnacksRegion.xlsx")


#c. Calculate Deep fried by gender

SvyDeepFriedSnacksGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDOtherFriedFoods) %>%
  summarise(MDDDeepFriedSnacksCat = survey_mean() * 100) %>%
  filter(MDDOtherFriedFoods == 1)

# Write this into an excel file

write.xlsx(SvyDeepFriedSnacksGender, "report tables/SvyDeepFriedSnacksGender.xlsx")

# d Calculate overall Deep fried snacks

SvyDeepFriedSnacksTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDOtherFriedFoods) %>%
  summarise(MDDDeepFriedSnacksCat = survey_mean() * 100) %>%
  filter(MDDOtherFriedFoods == 1)

###########################################################


# Deep fried snacks tests

DeepFriedSnacksChisqIE <- svychisq(~MDDOtherFriedFoods + Treatment, design = SvyDietQualityData) # Not significant
DeepFriedSnacksChisqRegion <- svychisq(~MDDOtherFriedFoods + regiontype, design = SvyDietQualityData) # Not significant
DeepFriedSnacksChisqGender <- svychisq(~MDDOtherFriedFoods + MDDGender, design = SvyDietQualityData) # Not significant

##########################################################

# Soft energy drinks

#a. Soft energy drinks by Treatment

SvySoftEnergyDrinks <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDDrinksSoftDrinks) %>%
  summarise(MDDSoftEnergyDrinksCat = survey_mean() * 100) %>%
  filter(MDDDrinksSoftDrinks == 1)

# Write this into an excel file

write.xlsx(SvySoftEnergyDrinks, "report tables/SvySoftEnergyDrinks.xlsx")

#b. Soft energy drinks by Region

SvySoftEnergyDrinksRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDDrinksSoftDrinks) %>%
  summarise(MDDSoftEnergyDrinksCat = survey_mean() * 100) %>%
  filter(MDDDrinksSoftDrinks == 1)

# Write this into an excel file

write.xlsx(SvySoftEnergyDrinksRegion, "report tables/SvySoftEnergyDrinksRegion.xlsx")

#c. Calculate Soft energy drinks by

SvySoftEnergyDrinksGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDDrinksSoftDrinks) %>%
  summarise(MDDSoftEnergyDrinksCat = survey_mean() * 100) %>%
  filter(MDDDrinksSoftDrinks == 1)

# Write this into an excel file

write.xlsx(SvySoftEnergyDrinksGender, "report tables/SvySoftEnergyDrinksGender.xlsx")

# d Calculate overall Soft energy drinks

SvySoftEnergyDrinksTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDDrinksSoftDrinks) %>%
  summarise(MDDSoftEnergyDrinksCat = survey_mean() * 100) %>%
  filter(MDDDrinksSoftDrinks == 1)

###########################################################

# Soft energy drinks tests

SoftEnergyDrinksChisqIE <- svychisq(~MDDDrinksSoftDrinks + Treatment, design = SvyDietQualityData) # Not significant
SoftEnergyDrinksChisqRegion <- svychisq(~MDDDrinksSoftDrinks + regiontype, design = SvyDietQualityData) # Not significant
SoftEnergyDrinksChisqGender <- svychisq(~MDDDrinksSoftDrinks + MDDGender, design = SvyDietQualityData) # Not significant


##########################################################


# Sweet Tea or coffee

#a. Sweet Tea or coffee by Treatment

SvySweetTea <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDDrinkTea) %>%
  summarise(MDDSweetTeaCat = survey_mean() * 100) %>%
  filter(MDDDrinkTea == 1)

# Write this into an excel file

write.xlsx(SvySweetTea, "report tables/SvySweetTea.xlsx")

#b. Sweet Tea or coffee by Region

SvySweetTeaRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDDrinkTea) %>%
  summarise(MDDSweetTeaCat = survey_mean() * 100) %>%
  filter(MDDDrinkTea == 1)

# Write this into an excel file

write.xlsx(SvySweetTeaRegion, "report tables/SvySweetTeaRegion.xlsx")

#c. Calculate Sweet Tea or coffee by gender

SvySweetTeaGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDDrinkTea) %>%
  summarise(MDDSweetTeaCat = survey_mean() * 100) %>%
  filter(MDDDrinkTea == 1)

# Write this into an excel file

write.xlsx(SvySweetTeaGender, "report tables/SvySweetTeaGender.xlsx")

# d Calculate overall Sweet Tea or coffee


SvySweetTeaTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDDrinkTea) %>%
  summarise(MDDSweetTeaCat = survey_mean() * 100) %>%
  filter(MDDDrinkTea == 1)


###########################################################

# Sweet Tea or coffee tests

SweetTeaChisqIE <- svychisq(~MDDDrinkTea + Treatment, design = SvyDietQualityData) # Not significant
SweetTeaChisqRegion <- svychisq(~MDDDrinkTea + regiontype, design = SvyDietQualityData) # Not significant
SweetTeaChisqGender <- svychisq(~MDDDrinkTea + MDDGender, design = SvyDietQualityData) # Not significant

##########################################################

# Friut Juice

#a. Fruit Juice by Treatment

SvyFruitJuice <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(Treatment, MDDDrinksFruitJuice) %>%
  summarise(MDDFruitJuiceCat = survey_mean() * 100) %>%
  filter(MDDDrinksFruitJuice == 1)

# Write this into an excel file

write.xlsx(SvyFruitJuice, "report tables/SvyFruitJuice.xlsx")

#b. Fruit Juice by Region

SvyFruitJuiceRegion <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(regiontype, MDDDrinksFruitJuice) %>%
  summarise(MDDFruitJuiceCat = survey_mean() * 100) %>%
  filter(MDDDrinksFruitJuice == 1)

# Write this into an excel file

write.xlsx(SvyFruitJuiceRegion, "report tables/SvyFruitJuiceRegion.xlsx")


#c. Calculate Fruit Juice by gender

SvyFruitJuiceGender <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDGender, MDDDrinksFruitJuice) %>%
  summarise(MDDFruitJuiceCat = survey_mean() * 100) %>%
  filter(MDDDrinksFruitJuice == 1)

# Write this into an excel file

write.xlsx(SvyFruitJuiceGender, "report tables/SvyFruitJuiceGender.xlsx")

# d Calculate overall Fruit Juice

SvyFruitJuiceTot <- SvyDietQualityData %>% 
  filter(MDDAge >= 15) %>%
  group_by(MDDDrinksFruitJuice) %>%
  summarise(MDDFruitJuiceCat = survey_mean() * 100) %>%
  filter(MDDDrinksFruitJuice == 1)

###########################################################

# Fruit Juice tests

FruitJuiceChisqIE <- svychisq(~MDDDrinksFruitJuice + Treatment, design = SvyDietQualityData) # Not significant
FruitJuiceChisqRegion <- svychisq(~MDDDrinksFruitJuice + regiontype, design = SvyDietQualityData) # Not significant
FruitJuiceChisqGender <- svychisq(~MDDDrinksFruitJuice + MDDGender, design = SvyDietQualityData) # Not significant

##########################################################








































































































































































































