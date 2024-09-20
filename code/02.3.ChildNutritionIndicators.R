

## Loading libraries

library(tidyverse)
library(openxlsx)
library(showtext)
library(patchwork)
library(cowplot)
library(stringr)


source("code/02.SurveyDesign.R") # This file converts the data into survey designed data
#source("code/02.00.Functions.R") # Functions for significance tests and creating Word documents


# Nutrition for children under the age of 2 years (23 months)

# Calculate the proportion of children who were breastfed over the last 24 hours
SvyMADBreasfed <- SvyMADData %>% 
  #filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>%
  dplyr::select(-c("PropotionBreastfed_se", "Total_se"))
  #Round the values to 2 decimal places

  

# Write the table to an excel file
write.xlsx(SvyMADBreasfed, "report tables/SvyMADBreasfed.xlsx")

# Calculate the proportion of children who were breastfed over the last 24 hours, by regiontype
SvyMADBreasfedRegion <- SvyMADData %>% 
  #filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se"))

# Write the table to an excel file
write.xlsx(SvyMADBreasfedRegion, "report tables/SvyMADBreasfedRegion.xlsx")

# Calculate the proportion of children who were breastfed over the last 24 hours, by gender

SvyMADBreasfedGender <- SvyMADData %>% 
  #filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se"))

# Write the table to an excel file
write.xlsx(SvyMADBreasfedGender, "report tables/SvyMADBreasfedGender.xlsx")

SvyBreastfedOverall <- SvyMADData %>% 
  #filter(ChildAgeMonths >= 12 & ChildAgeMonths <= 23) %>%
  group_by(PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total())

# Write the table to an excel file
write.xlsx(SvyBreastfedOverall, "report tables/SvyBreastfedOverall.xlsx")

SvyMADCBF <- SvyMADData %>% 
  filter(ChildAgeMonths >= 12 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se", "Total")) %>% 
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) %>% 
  filter(PCMADBreastfeeding == "Yes") %>% 
  pivot_wider(names_from = Treatment, values_from = PropotionBreastfed) %>% 
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "Breastfeeding") %>%
  dplyr::select(Indicator, PCMADBreastfeeding, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = PCMADBreastfeeding) %>% 
  mutate(Category = as_factor(Category)) %>% 
  #Roubd the values to 2 decimal places
  mutate(Overall = round(Overall, 2),
         `Control Group` = round(`Control Group`, 2),
         `Treatment Group` = round(`Treatment Group`, 2),
         Diff = round(Diff, 2))

# Write the table to an excel file
write.xlsx(SvyMADCBF, "report tables/SvyMADCBF.xlsx")

# Calculate the proportion of children who were breastfed over the last 24 hours, by regiontype
SvyMADCBFRegion <- SvyMADData %>% 
  filter(ChildAgeMonths >= 12 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se")) %>%
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) %>%
  #filter(PCMADBreastfeeding == "Yes") %>%
  rename(Disaggregation = regiontype) 


# Write the table to an excel file
write.xlsx(SvyMADCBFRegion, "report tables/SvyMADCBFRegion.xlsx")

# Calculate the proportion of children who were breastfed over the last 24 hours, by gender
SvyMADCBFGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 12 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se")) %>% 
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) %>%
  #filter(PCMADBreastfeeding == "Yes") %>%
  rename(Disaggregation = ChildGender) 

SvyMADCBFTreat <- SvyMADData %>% 
  filter(ChildAgeMonths >= 12 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se")) %>% 
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) %>%
  #filter(PCMADBreastfeeding == "Yes") %>%
  rename(Disaggregation = Treatment) 

SvyMADCBFOverall <- SvyMADData %>% 
  filter(ChildAgeMonths >= 12 & ChildAgeMonths <= 23) %>%
  group_by(PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se")) %>% 
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) 

# Write the table to an excel file
write.xlsx(SvyMADCBFGender, "report tables/SvyMADCBFGender.xlsx")
###############################################################################################
# Test the difference in the proportions of children who were breastfed in the last 24 hours
MADBrestfedProptestIE <-  SvyMADData %>% 
  filter(ChildAgeMonths >= 12 & ChildAgeMonths <= 23) %>%
  svychisq(~PCMADBreastfeeding + regiontype, design = .) # Not statistically significant at all levels

##############################################################################################

## Proportion of breasfed children in the last day, by regiontype
SvyMADBreastFeedindRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se", "Total")) %>%
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) %>%
  filter(PCMADBreastfeeding == "Yes") %>%
  rename(Disaggregation = regiontype) %>% 
  dplyr::select(Disaggregation, PCMADBreastfeeding, PropotionBreastfed)

# Calculate the proportion of children who were breastfed over the last 24 hours, by child gender
SvyMADBreasfedGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(PCMADBreastfeeding, ChildGender) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se", "Total")) %>% 
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding),
         ChildGender = as_factor(ChildGender)) %>%
  filter(PCMADBreastfeeding == "Yes") %>% 
  rename(Disaggregation = ChildGender) %>% 
  dplyr::select(Disaggregation, PCMADBreastfeeding, PropotionBreastfed)

SvyMADBreastfedTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se", "Total")) %>% 
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) %>% 
  filter(PCMADBreastfeeding == "Yes") %>% 
  rename(Disaggregation = Treatment)

OveralBreastFed <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("PropotionBreastfed_se", "Total_se", "Total")) %>% 
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) %>% 
  filter(PCMADBreastfeeding == "Yes")

# Combine the tables
SvyMADBreastfedTab <- rbind(SvyMADBreastFeedindRegionTab, SvyMADBreasfedGenderTab, SvyMADBreastfedTreatTab) %>% 
  dplyr::select(-c("PCMADBreastfeeding"))

#Write the tables to an excel file
write.xlsx(SvyMADBreastfedTab, "report tables/SvyMADBreastfedTab.xlsx")
###############################################################################################################################
##Testing the difference in the proportions of children who were breastfed in the last 24 hours
MADBrestfedProptestIE <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>% 
  svychisq(~PCMADBreastfeeding + Treatment, design = .) # Not statistically significant at all levels

MADBrestfedProptestGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  svychisq(~PCMADBreastfeeding + ChildGender, design = .) # Not Statistically significant at all levels

MADBrestfedProptestRegion <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  svychisq(~PCMADBreastfeeding + regiontype, design = .) # Not statistically significant
###############################################################################################################################


# Calculate the proportion of children meeting minimum diteray diversity
SvyMDDChildren <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MDDCat) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MDDChilden_se", "Total_se", "Total")) %>% 
  filter(MDDCat == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MDDChilden) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MDD") %>%
  dplyr::select(Indicator, MDDCat, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MDDCat) %>% 
  mutate(Category = as_factor(Category))

#############################################################################################################################
# Test the difference in the proportions of children who met the minimum dietary diversity
MDDChisqtestIE <-  SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MDDCat + Treatment, design = .) # Not Statistically significant at all levels

############################################################################################################################


SvyMDDChildrenTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MDDCat) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MDDChilden_se", "Total_se")) %>% 
  #filter(MDDCat == 1) %>% 
  rename(Disaggregation = Treatment) %>% 
  # Round to 2dp
  mutate(MDDChilden = round(MDDChilden, 2))

# Write the table to an excel file
write.xlsx(SvyMDDChildrenTreatTab, "report tables/SvyMDDChildrenTreatTab.xlsx")

# Calculate the proportion of children meeting minimum dietary diversity, by regiontype
SvyMDDChildrenRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, MDDCat) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MDDChilden_se", "Total_se")) %>% 
  #filter(MDDCat == 1) %>% 
  rename(Disaggregation = regiontype) %>% 
  # Round to 2dp
  mutate(MDDChilden = round(MDDChilden, 2))

# Write the table to an excel file
write.xlsx(SvyMDDChildrenRegionTab, "report tables/SvyMDDChildrenRegionTab.xlsx")

# Calculate the proportion of children meeting minimum dietary diversity, by tchild gender
SvyMDDChildrenGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, MDDCat) %>%
  summarise(MDDChilden = survey_prop() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MDDChilden_se", "Total_se")) %>% 
  #filter(MDDCat == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>%
  rename(Disaggregation = ChildGender) %>% 
  # Round to 2dp
  mutate(MDDChilden = round(MDDChilden, 2))

# Write the table to an excel file
write.xlsx(SvyMDDChildrenGenderTab, "report tables/SvyMDDChildrenGenderTab.xlsx")

SvyMDDChildrenOverall <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(MDDCat) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MDDChilden_se", "Total_se")) %>% 
  #filter(MDDCat == 1) %>% 
  # Round to 2dp
  mutate(MDDChilden = round(MDDChilden, 2))

# Combine the tables
SvyMDDChildrenTab <- rbind(SvyMDDChildrenTreatTab, SvyMDDChildrenRegionTab, SvyMDDChildrenGenderTab) %>% 
  dplyr::select(-c("MDDCat"))

################################################################################################################
# Write the tables to an excel file
write.xlsx(SvyMDDChildrenTab, "report tables/SvyMDDChildrenTab.xlsx")
################################################################################################################

#SvyMDD by age group
SvyMDDChildrenAgeTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, MDDCat) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MDDChilden_se", "Total_se", "Total")) %>% 
  filter(MDDCat == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, MDDChilden)


##################################################################################################################################
#Testing the difference in the proportions of children who met the minimum dietary diversity
MDDChisqtestIE <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MDDCat + Treatment, design = .) ## Biig difference and statistically Significant at 10% level
MDDChisqtestRegion <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MDDCat + regiontype, design = .) ## Small difference and not statistically significant at all levels
MDDChisqtestGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MDDCat + ChildGender, design = .) ## Not statistically significant at all levels
MDDChisqAge <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MDDCat + ChildAgeGroup, design = .) ## Highly statistically significant at all levels
##################################################################################################################################
## Calculate the proportion of children meeting minimum dietary diversity, by child gender
SvyMDDChildrenGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MDDCat, ChildGender) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MDDChilden_se", "Total_se", "Total")) %>% 
  filter(MDDCat == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MDDChilden) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MDD") %>%
  dplyr::select(Indicator, MDDCat, ChildGender, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MDDCat) %>% 
  mutate(Category = as_factor(Category),
         ChildGender = as_factor(ChildGender))


###############################################################################################

SvyMMFChildrenTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MMF) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFChildren_se", "Total_se")) %>% 
  filter(MMF == 1) %>% 
  rename(Disaggregation = Treatment) %>% 
  # round to 2dp
  mutate(MMFChildren = round(MMFChildren, 2))

# Write the table to an excel file
write.xlsx(SvyMMFChildrenTreatTab, "report tables/SvyMMFChildrenTreatTab.xlsx")

# Calculate the proportion of children who met MMF, by regiontype
SvyMMFChildrenRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, MMF) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFChildren_se", "Total_se")) %>% 
  #filter(MMF == 1) %>% 
  rename(Disaggregation = regiontype) %>% 
  # round to 2dp
  mutate(MMFChildren = round(MMFChildren, 2))

# Write the table to an excel file
write.xlsx(SvyMMFChildrenRegionTab, "report tables/SvyMMFChildrenRegionTab.xlsx")

# Calculate the proportion of children who met MMF, by child gender
SvyMMFChildrenGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, MMF) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFChildren_se", "Total_se")) %>% 
  #filter(MMF == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>% 
  rename(Disaggregation = ChildGender) %>%
  # round to 2dp
  mutate(MMFChildren = round(MMFChildren, 2))

# Write the table to an excel file
write.xlsx(SvyMMFChildrenGenderTab, "report tables/SvyMMFChildrenGenderTab.xlsx")

SvyMMFOverallTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(MMF) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFChildren_se", "Total_se")) %>% 
  #filter(MMF == 1) %>% 
  # round to 2dp
  mutate(MMFChildren = round(MMFChildren, 2))

# Write the table to an excel file
write.xlsx(SvyMMFOverallTab, "report tables/SvyMMFOverallTab.xlsx")

# Combine the tables
SvyMMFChildrenTab <- rbind(SvyMMFChildrenTreatTab, SvyMMFChildrenRegionTab, SvyMMFChildrenGenderTab) %>% 
  dplyr::select(-c("MMF"))

#####################################################################################################
# Write the tables to an excel file
write.xlsx(SvyMMFChildrenTab, "report tables/SvyMMFChildrenTab.xlsx")
#####################################################################################################
# Calculate the proportion of children who MMF  by age group
SvyMMFChildrenAgeTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, MMF) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFChildren_se", "Total_se", "Total")) %>% 
  filter(MMF == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, MMFChildren)

##################################################################################################################################
#testing the difference in the proportions of children who met the minimum meal frequency using the chisq test
MMFChisqtestIE <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MMF + Treatment, design = .) ## Not statistically significant at all levels
MMFChisqtestRegion <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MMF + regiontype, design = .) ## Not statistically significant at all levels
MMFChisqtestGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MMF + ChildGender, design = .) ## Not statistically significant at all levels
MMFChisqAge <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MMF + ChildAgeGroup, design = .) ## Highly statistically significant at all levels
#################################################################################################################################

# Calculate the proportion of children meeting MMF in each group
## Calculate the percentage of children who met MMF, by child gender
SvyMMFChildrenGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MMF, ChildGender) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFChildren_se", "Total_se")) %>% 
  filter(MMF == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MMFChildren) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MMF") %>%
  dplyr::select(Indicator, MMF, ChildGender, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MMF) %>% 
  mutate(Category = as_factor(Category),
         ChildGender = as_factor(ChildGender))


##################################################################################################

# ## Calculate the percentage of children who met MMFF, by child gender
# SvyMMFFChildrenGender <- SvyMADData %>% 
#   filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
#   filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
#   group_by(Treatment, MMFF, ChildGender) %>%
#   summarise(MMFFChildren = survey_mean() * 100,
#             Total = survey_total()) %>% 
#   dplyr::select(-c("MMFFChildren_se", "Total_se", "Total")) %>% 
#   filter(MMFF == 1) %>% 
#   pivot_wider(names_from = Treatment, values_from = MMFFChildren) %>%
#   mutate(Diff = `Treatment Group` - `Control Group`) %>%
#   mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
#   mutate(Indicator = "MMFF") %>%
#   dplyr::select(Indicator, MMFF, ChildGender, Overall, `Control Group`, `Treatment Group`, Diff) %>%
#   rename(Category = MMFF) %>% 
#   mutate(Category = as_factor(Category),
#          ChildGender = as_factor(ChildGender))

# Challculate the proportion of children who met MMFF, by regiontype
SvyMMFFChildrenRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
  group_by(regiontype, MMFF) %>%
  summarise(MMFFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFFChildren_se", "Total_se")) %>% 
  #filter(MMFF == 1) %>% 
  rename(Disaggregation  = regiontype)

# Calculate the proportion of children who met MMFF, by child gender
SvyMMFFChildrenGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
  group_by(ChildGender, MMFF) %>%
  summarise(MMFFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFFChildren_se", "Total_se")) %>% 
  #filter(MMFF == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>% 
  rename(Disaggregation = ChildGender)

# Calculate the proportion of children who met MMFF, by treatment
SvyMMFFChildrenTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
  group_by(Treatment, MMFF) %>%
  summarise(MMFFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFFChildren_se", "Total_se")) %>% 
  #filter(MMFF == 1) %>% 
  rename(Disaggregation = Treatment)

SvyMMFFOverallTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
  group_by(MMFF) %>%
  summarise(MMFFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFFChildren_se", "Total_se")) 

# Combining the tables

SvyChildrenMMFFTab <- rbind(SvyMMFFChildrenTreatTab, SvyMMFFChildrenRegionTab, SvyMMFFChildrenGenderTab) %>% 
  dplyr::select(-c("MMFF"))

# Write the tables to an excel file
write.xlsx(SvyChildrenMMFFTab, "report tables/SvyChildrenMMFFTab.xlsx")
##################################################################################################################################
# Perform the chisq test to test the difference in the proportions of children who met the minimum meal frequency
MMFFChisqtestIE <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MMFF + Treatment, design = .) ## Not statistically significant at all levels
MMFFChisqtestRegion <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MMFF + regiontype, design = .) ## The difference is statistically significant at 5% level
MMFFChisqtestGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~MMFF + ChildGender, design = .) ## The difference is not statistically significant at all levels
##################################################################################################################################

# Calculate the proportion of children who met MAD
SvyMADChildren <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MADChildren_se", "Total_se", "Total")) %>% 
  filter(MAD == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MADChildren) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MAD") %>%
  dplyr::select(Indicator, MAD, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MAD) %>% 
  mutate(Category = as_factor(Category)) 

# Write excel file
write.xlsx(SvyMADChildren, "report tables/SvyMADChildren.xlsx")

##########################################################################################################
# Test the difference in the proportions of children who met the minimum acceptable diet
MADChisqtestIE <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>% 
  svychisq(~MAD + Treatment, design = .) ## The difference is not statistically significant
##########################################################################################################

# Calculate the percentage of children who met MAD, by treatment
SvyMADChildrenTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MADChildren_se", "Total_se")) %>% 
  #filter(MAD == 1) %>% 
  rename(Disaggregation = Treatment)

# Calculate the proportion of children who met MAD, by child gender
SvyMADChildrenGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MADChildren_se", "Total_se")) %>% 
  #filter(MAD == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>% 
  rename(Disaggregation = ChildGender)

# Calculate the proportion of children who met MAD, by regiontype
SvyMADChildrenRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MADChildren_se", "Total_se")) %>% 
  #filter(MAD == 1) %>% 
  rename(Disaggregation = regiontype)

SvyMADOverall <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MADChildren_se", "Total_se")) 

# combine the tables
SvyChildrenMADTab <- rbind(SvyMADChildrenTreatTab, SvyMADChildrenRegionTab, SvyMADChildrenGenderTab) %>% 
  dplyr::select(-c("MAD")) %>% 
  # round the values to 2 decimal places
  mutate(MADChildren = round(MADChildren, 2))

# Write the tables to an excel file
write.xlsx(SvyChildrenMADTab, "report tables/SvyChildrenMADTab.xlsx")

# Calculate the proportion of children who met MAD, by age group

SvyMADChildrenAgeTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MADChildren_se", "Total_se", "Total")) %>% 
  filter(MAD == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, MADChildren)
##################################################################################################################################

# Perfom the chisq test to test the difference in the proportions of children who met the minimum acceptable diet
MADChisqtestIE <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>% 
  svychisq(~MAD + Treatment, design = .) ## The difference is weeakily statistically significant at 10% level
MADChisqtestRegion <- svychisq(~MAD + regiontype, design = SvyMADData) ## The difference is not statistically significant at all levels
MADChisqtestGender <- svychisq(~MAD + ChildGender, design = SvyMADData) ## The difference is not statistically significant at all levels
MADChisqtestAge <- svychisq(~MAD + ChildAgeGroup, design = SvyMADData) ## The difference is highly statistically significant at all levels

##################################################################################################################################

# Subsetting the data to be used in the chisq tests - It can also be used to calculate the proportions of children who met MixMF
SvyMADMixMFData <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 5) %>% 
  mutate(MixMF = case_when(
    PCMADBreastfeeding == 1 & (PCMADInfFormula == 1 | PCMADMilk == 1) ~ 1,
    TRUE ~ 0)) 

# Calculate the proportion of children who met MixMF, by regiontype
SvyMADMixMFRegionTab <- SvyMADMixMFData %>% 
  group_by(regiontype, MixMF) %>%
  summarise(MixMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MixMFChildren_se", "Total_se")) %>% 
  #filter(MixMF == 1) %>% 
  rename(Disaggregation  = regiontype)

# Calculate the proportion of children who met MixMF, by child gender
SvyMADMixMFGenderTab <- SvyMADMixMFData %>% 
  group_by(ChildGender, MixMF) %>%
  summarise(MixMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MixMFChildren_se", "Total_se")) %>% 
  #filter(MixMF == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>%
  rename(Disaggregation = ChildGender)

# Calculate the proportion of children who met MixMF, by treatment
SvyMADMixMFTreatTab <- SvyMADMixMFData %>% 
  group_by(Treatment, MixMF) %>%
  summarise(MixMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MixMFChildren_se", "Total_se")) %>% 
  #filter(MixMF == 1) %>% 
  rename(Disaggregation = Treatment)

SvyMADMixMFOverallTab <- SvyMADMixMFData %>% 
  group_by(MixMF) %>%
  summarise(MixMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MixMFChildren_se", "Total_se"))
  #filter(MixMF == 1) %>% 


# Combine the tables
SvyChildrenMixMFTab <- rbind(SvyMADMixMFTreatTab, SvyMADMixMFRegionTab, SvyMADMixMFGenderTab) %>% 
  dplyr::select(-c("MixMF"))

# Write the tables to an excel file
write.xlsx(SvyChildrenMixMFTab, "report tables/SvyChildrenMixMFTab.xlsx")

##################################################################################################################################
# perform the chisq test to test the difference in the proportions of children who met the mixed milk feeding

MixMFChisqtestIE <- svychisq(~MixMF + Treatment, design = SvyMADMixMFData) ## The difference is not statistically significant at all levels
MixMFChisqtestRegion <- svychisq(~MixMF + regiontype, design = SvyMADMixMFData) ## The difference is not statistically significant at all levels
MixMFChisqtestGender <- svychisq(~MixMF + ChildGender, design = SvyMADMixMFData) ## The difference is not statistically significant at all levels
##################################################################################################################################

# Calculate Sentinel Unhealthy Foods Consumption
#############################################################################################################################
# Calculate the proportion of children who consumed unhealthy foods, by regiontype
SvyMADUnhealthyFoodsRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, PCMADUnhealthyFds) %>%
  summarise(UnhealthyFoods = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("UnhealthyFoods_se", "Total_se")) %>% 
  #filter(PCMADUnhealthyFds == 1) %>% 
  rename(Disaggregation  = regiontype)

# Calculate the proportion of children who consumed unhealthy, by treatment
SvyMADUnhealthyFoodsTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PCMADUnhealthyFds) %>%
  summarise(UnhealthyFoods = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("UnhealthyFoods_se", "Total_se")) %>% 
  #filter(PCMADUnhealthyFds == 1) %>% 
  rename(Disaggregation = Treatment)

# Calculate the percentage of children who consumed unhealthy foods by child gender
SvyMADUnhealthyFoodsGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, PCMADUnhealthyFds) %>%
  summarise(UnhealthyFoods = survey_mean() * 100,
            Total = survey_total()) %>%
  dplyr::select(-c("UnhealthyFoods_se", "Total_se")) %>%
  #filter(PCMADUnhealthyFds == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>% 
  rename(Disaggregation = ChildGender)

# Overall Unhealthy Foods
SvyMADUnhealthyFoodsOverall <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADUnhealthyFds) %>%
  summarise(UnhealthyFoods = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("UnhealthyFoods_se", "Total_se")) %>% 
  #filter(PCMADUnhealthyFds == 1) %>% 
  rename(Percentage = UnhealthyFoods) %>% 
  mutate(Indicator = "UnhealthyFoods", 
         Category = "Overall") %>% 
  dplyr::select(Indicator, Category, Percentage)



# Combine the tables
SvyChildrenUnhealthyFoodsTab <- rbind(SvyMADUnhealthyFoodsTreatTab, SvyMADUnhealthyFoodsRegionTab, SvyMADUnhealthyFoodsGenderTab) %>% 
  dplyr::select(-c("PCMADUnhealthyFds"))

# Write the tables to an excel file
write.xlsx(SvyChildrenUnhealthyFoodsTab, "report tables/SvyChildrenUnhealthyFoodsTab.xlsx")

##################################################################################################################################
# Perform the chisq test to test the difference in the proportions of children who consumed unhealthy foods
UnhealthyFoodsChisqtestIE <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~PCMADUnhealthyFds + Treatment, design = .) ## The difference is statistically significant at all 10%
UnhealthyFoodsChisqtestRegion <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~PCMADUnhealthyFds + regiontype, design = .) ## The difference is not statistically significant at all levels
UnhealthyFoodsChisqtestGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  svychisq(~PCMADUnhealthyFds + ChildGender, design = .) ## The difference is not statistically significant at all levels
##################################################################################################################################

# Combine all the nutrition indicators tables
SvyMADNutritionIndicators <- SvyMADBreastfedTab %>% 
  left_join(SvyMDDChildrenTab, by = "Disaggregation") %>% 
  left_join(SvyMMFChildrenTab, by = "Disaggregation") %>% 
  left_join(SvyChildrenMMFFTab, by = "Disaggregation") %>% 
  left_join(SvyChildrenMADTab, by = "Disaggregation") %>%
  left_join(SvyChildrenMixMFTab, by = "Disaggregation") %>%
  left_join(SvyChildrenUnhealthyFoodsTab, by = "Disaggregation") %>% 
  pivot_longer(cols = c("PropotionBreastfed" : "UnhealthyFoods"), names_to = "Indicator", values_to = "Percentage") %>% 
  # round the values to 2 decimal places
  mutate(across(is.numeric, ~round(., 2)))

# Write an excel file
write.xlsx(SvyMADNutritionIndicators, "results/SvyMADNutritionIndicators.xlsx")

#Subset the data for MAD,MDD, MMF, MMFF, and Breasstfeeding

SvyChilNutritionIndicators <- SvyMADNutritionIndicators %>% 
  filter(Indicator %in% c("MADChildren", "MDDChilden", "MMFChildren", "MMFFChildren", "PropotionBreastfed")) %>% 
  mutate(Indicator = case_when(
    Indicator == "MADChildren" ~ "MAD",
    Indicator == "MDDChilden" ~ "MDD",
    Indicator == "MMFChildren" ~ "MMF",
    Indicator == "MMFFChildren" ~ "MMFF",
    Indicator == "PropotionBreastfed" ~ "CBF")) %>%
  # Change all numeric columns to 2 dp
  mutate(across(is.numeric, ~round(., 2)))

######################################################################################################################################

# ## Calculate the nutrition indicators using the formula
# SvyChildMDD <-  SvyMADData %>%  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>% calculate_proportions_and_ttest_nut(., "MDDCat", "Treatment")
# SvyChildMMF <- calculate_proportions_and_ttest_nut(SvyMADData, "MMF", "Treatment")
# SvyChildMMFF <- calculate_proportions_and_ttest_nut(SvyMADData, "MMFF", "Treatment")
# SvyChildMAD <- calculate_proportions_and_ttest_nut(SvyMADData, "MAD", "Treatment")
# SvyChildUnhealthyFoods <- calculate_proportions_and_ttest_nut(SvyMADData, "PCMADUnhealthyFds", "Treatment")
# SvyChildBreastfeeding <- calculate_proportions_and_ttest_nut(SvyMADData, "PCMADBreastfeeding", "Treatment")
# SvyChildMixMF <- calculate_proportions_and_ttest_nut(SvyMADData, "MixMF", "Treatment")

#####################################################################################################################################
SvyNutTable <- SvyMADChildrenAgeTab %>% 
  left_join(SvyMMFChildrenAgeTab, by = "Disaggregation") %>% 
  left_join(SvyMDDChildrenAgeTab, by = "Disaggregation") %>% 
  # Change all numeric columns to 2 dp
  mutate(across(is.numeric, ~round(., 2))) %>%
  pivot_longer(cols = c("MADChildren" : "MDDChilden"), names_to = "Indicator", values_to = "Percentage") %>% 
  mutate(Indicator = factor(Indicator, levels = c("MDDChilden", "MMFChildren", "MADChildren")),
         Disaggregation = factor(Disaggregation, levels = c("6-11 Months", "12-17 Months", "18-23 Months")))

##################################################################################################################################
# Child nutrition by age group
SvyCHildMADAgeGrp <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, MAD) %>% 
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MADChildren_se", "Total_se", "Total")) %>% 
  filter(MAD == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, MADChildren)

SvyCHildMDDAgeGrp <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, MDDCat) %>% 
  summarise(MDDChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MDDChildren_se", "Total_se", "Total")) %>% 
  filter(MDDCat == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, MDDChildren)

SvyCHildMMFAgeGrp <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, MMF) %>% 
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFChildren_se", "Total_se", "Total")) %>% 
  filter(MMF == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, MMFChildren)

SvyCHildMMFFAgeGrp <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
  group_by(ChildAgeGroup, MMFF) %>% 
  summarise(MMFFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("MMFFChildren_se", "Total_se", "Total")) %>% 
  filter(MMFF == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, MMFFChildren)

SvyChildBreastfeedingAgeGrp <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, PCMADBreastfeeding) %>%
  summarise(Breastfeeding = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("Breastfeeding_se", "Total_se", "Total")) %>% 
  filter(PCMADBreastfeeding == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, Breastfeeding)

# Merge the data and write to an excel file
SvyChildNutritionAgeGrp <- SvyCHildMADAgeGrp %>% 
  left_join(SvyCHildMDDAgeGrp, by = "Disaggregation") %>% 
  left_join(SvyCHildMMFAgeGrp, by = "Disaggregation") %>% 
  left_join(SvyCHildMMFFAgeGrp, by = "Disaggregation") %>% 
  left_join(SvyChildBreastfeedingAgeGrp, by = "Disaggregation") %>% 
  pivot_longer(cols = c("MADChildren" : "Breastfeeding"), names_to = "Indicator", values_to = "Percentage") %>% 
  mutate(Indicator = factor(Indicator, levels = c("MDDChildren", "MMFChildren", "MMFFChildren", "MADChildren", "Breastfeeding")),
         Disaggregation = factor(Disaggregation, levels = c("6-11 Months", "12-17 Months", "18-23 Months"))) %>% 
  # shorten the names of the indicators
  mutate(Indicator = case_when(
    Indicator == "MDDChildren" ~ "MDD",
    Indicator == "MMFChildren" ~ "MMF",
    Indicator == "MMFFChildren" ~ "MMFF",
    Indicator == "MADChildren" ~ "MAD",
    Indicator == "Breastfeeding" ~ "CBF"),
    Indicator = factor(Indicator, levels = c("CBF", "MDD", "MMF", "MMFF", "MAD"))) %>% 
  # Change all numeric columns to 2 dp
  mutate(across(is.numeric, ~round(., 0)))

#########################################################################################################################################

# Food groups consumed by children


#1 1 Calculate the percentage of children who consumed each food group
SvyMADStaplesTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADStaples) %>%
  summarise(Staples = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("Staples_se", "Total_se", "Total")) %>% 
  filter(PCMADStaples == 1) %>% 
  mutate(Indicator = "Staples") %>% 
  rename(Percentage = Staples) %>%
  select(Indicator, Percentage)

SvyMADProteinEggsTab <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADProteinEggs) %>%
  summarise(ProteinEggs = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ProteinEggs_se", "Total_se", "Total")) %>% 
  filter(PCMADProteinEggs == 1) %>% 
  mutate(Indicator = "Eggs") %>%
  rename(Percentage = ProteinEggs) %>% 
  select(Indicator, Percentage)

SvyMADLegumesTab <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADLegumes) %>%
  summarise(Legumes = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("Legumes_se", "Total_se", "Total")) %>% 
  filter(PCMADLegumes == 1) %>% 
  mutate(Indicator = "Legumes") %>%
  rename(Percentage = Legumes) %>% 
  select(Indicator, Percentage)

SvyMADDairyTab <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADDairy) %>%
  summarise(Dairy = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("Dairy_se", "Total_se", "Total")) %>% 
  filter(PCMADDairy == 1) %>% 
  mutate(Indicator = "Dairy") %>%
  rename(Percentage = Dairy) %>% 
  select(Indicator, Percentage)

SvyMADFleshFoodsTab <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADFleshFoods) %>%
  summarise(FleshFoods = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("FleshFoods_se", "Total_se", "Total")) %>% 
  filter(PCMADFleshFoods == 1) %>% 
  mutate(Indicator = "FleshFoods") %>%
  rename(Percentage = FleshFoods) %>% 
  select(Indicator, Percentage)

SvyMADVitATab <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADVitA) %>%
  summarise(VitA = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("VitA_se", "Total_se", "Total")) %>% 
  filter(PCMADVitA == 1) %>% 
  mutate(Indicator = "VitA") %>%
  rename(Percentage = VitA) %>% 
  select(Indicator, Percentage)

SvyMADOtherFruitsVegTab <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADOtherFruitsVeg) %>%
  summarise(OtherFruitsVeg = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("OtherFruitsVeg_se", "Total_se", "Total")) %>% 
  filter(PCMADOtherFruitsVeg == 1) %>% 
  mutate(Indicator = "OtherFruitsVeg") %>%
  rename(Percentage = OtherFruitsVeg) %>% 
  select(Indicator, Percentage)

# Combine the tables
SvyChildrenFoodGroupsTab <- rbind(SvyMADStaplesTab, 
                                  SvyMADProteinEggsTab, 
                                  SvyMADLegumesTab, 
                                  SvyMADDairyTab, 
                                  SvyMADFleshFoodsTab, 
                                  SvyMADVitATab, 
                                  SvyMADOtherFruitsVegTab) %>% 
  # Change all numeric columns to 2 dp
  mutate(Percentage = round(Percentage, 2))

# Write the tables to an excel file
write.xlsx(SvyChildrenFoodGroupsTab, "report tables/SvyChildrenFoodGroupsTab.xlsx")

##################################################################################################################################
# Calculate the same indicators by age group

SvyMADStaplesAgeGrp <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, PCMADStaples) %>%
  summarise(Staples = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("Staples_se", "Total_se", "Total")) %>% 
  filter(PCMADStaples == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, Staples)

SvyMADProteinEggsAgeGrp <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, PCMADProteinEggs) %>%
  summarise(ProteinEggs = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ProteinEggs_se", "Total_se", "Total")) %>% 
  filter(PCMADProteinEggs == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, ProteinEggs)

SvyMADLegumesAgeGrp <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, PCMADLegumes) %>%
  summarise(Legumes = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("Legumes_se", "Total_se", "Total")) %>% 
  filter(PCMADLegumes == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, Legumes)

SvyMADDairyAgeGrp <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, PCMADDairy) %>%
  summarise(Dairy = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("Dairy_se", "Total_se", "Total")) %>% 
  filter(PCMADDairy == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, Dairy)

SvyMADFleshFoodsAgeGrp <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, PCMADFleshFoods) %>%
  summarise(FleshFoods = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("FleshFoods_se", "Total_se", "Total")) %>% 
  filter(PCMADFleshFoods == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, FleshFoods)

SvyMADVitAAgeGrp <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, PCMADVitA) %>%
  summarise(VitA = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("VitA_se", "Total_se", "Total")) %>% 
  filter(PCMADVitA == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, VitA)

SvyMADOtherFruitsVegAgeGrp <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, PCMADOtherFruitsVeg) %>%
  summarise(OtherFruitsVeg = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("OtherFruitsVeg_se", "Total_se", "Total")) %>% 
  filter(PCMADOtherFruitsVeg == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  dplyr::select(Disaggregation, OtherFruitsVeg)

# Merge the data and write to an excel file
SvyChildrenFoodGroupsAgeGrp <- SvyMADStaplesAgeGrp %>% 
  left_join(SvyMADProteinEggsAgeGrp, by = "Disaggregation") %>% 
  left_join(SvyMADLegumesAgeGrp, by = "Disaggregation") %>% 
  left_join(SvyMADDairyAgeGrp, by = "Disaggregation") %>% 
  left_join(SvyMADFleshFoodsAgeGrp, by = "Disaggregation") %>% 
  left_join(SvyMADVitAAgeGrp, by = "Disaggregation") %>% 
  left_join(SvyMADOtherFruitsVegAgeGrp, by = "Disaggregation") %>% 
  pivot_longer(cols = c("Staples" : "OtherFruitsVeg"), names_to = "Indicator", values_to = "Percentage") %>% 
  mutate(Indicator = factor(Indicator, levels = c("Staples", "ProteinEggs", "Legumes", "Dairy", "FleshFoods", "VitA", "OtherFruitsVeg")),
         Disaggregation = factor(Disaggregation, levels = c("6-11 Months", "12-17 Months", "18-23 Months"))) %>% 
  # shorten the names of the indicators
  mutate(Indicator = case_when(
    Indicator == "Staples" ~ "Staples",
    Indicator == "ProteinEggs" ~ "Eggs",
    Indicator == "Legumes" ~ "Legumes",
    Indicator == "Dairy" ~ "Dairy",
    Indicator == "FleshFoods" ~ "Flesh Foods",
    Indicator == "VitA" ~ "Vit A",
    Indicator == "OtherFruitsVeg" ~ "Other Fruits + Veg"),
    Indicator = factor(Indicator, levels = c("Staples", "Eggs", "Legumes", "Dairy", "Flesh Foods", "Vit A", "Other Fruits + Veg"))) %>% 
  # Change all numeric columns to 2 dp
  mutate(across(is.numeric, ~round(., 0)))
###############################################################################################

# Exclusive breastfeeding br treatment

SvyMADExclBreastfeedingTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 5) %>%
  group_by(Treatment, MADEBF) %>%
  summarise(ExclBreastfeeding = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ExclBreastfeeding_se", "Total_se")) %>% 
  rename(Disaggregation = Treatment) %>% 
  # round the values to 2 decimal places
  mutate(ExclBreastfeeding = round(ExclBreastfeeding, 2))

# Write the tables to an excel file
write.xlsx(SvyMADExclBreastfeedingTreatTab, "report tables/SvyMADExclBreastfeedingTreatTab.xlsx")



# Calculate the proportion of children who met MAD, by regiontype
SvyMADExclBreastfeedingRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 5) %>%
  group_by(regiontype, MADEBF) %>%
  summarise(ExclBreastfeeding = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ExclBreastfeeding_se", "Total_se")) %>% 
  rename(Disaggregation  = regiontype) %>% 
  # round the values to 2 decimal places
  mutate(ExclBreastfeeding = round(ExclBreastfeeding, 2))

# Write the tables to an excel file
write.xlsx(SvyMADExclBreastfeedingRegionTab, "report tables/SvyMADExclBreastfeedingRegionTab.xlsx")

# Exclusive breastfeeding by gender

SvyMADExclBreastfeedingGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 5) %>%
  group_by(ChildGender, MADEBF) %>%
  summarise(ExclBreastfeeding = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ExclBreastfeeding_se", "Total_se")) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>% 
  rename(Disaggregation = ChildGender) %>% 
  # round the values to 2 decimal places
  mutate(ExclBreastfeeding = round(ExclBreastfeeding, 2))

# Write the tables to an excel file
write.xlsx(SvyMADExclBreastfeedingGenderTab, "report tables/SvyMADExclBreastfeedingGenderTab.xlsx")


# Overall exclusive breastfeeding
SvyMADExclBreastfeedingOverall <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 5) %>%
  group_by(MADEBF) %>%
  summarise(ExclBreastfeeding = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ExclBreastfeeding_se", "Total_se")) %>% 
  rename(Percentage = ExclBreastfeeding) %>% 
  mutate(Indicator = "ExclBreastfeeding", 
         Category = "Overall")

# Write the tables to an excel file


########################################################################################################


#Calculate the percentage of children consuming egg or flesh foods by treatment

SvyMADEggFlessTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PMADEggFlesh) %>%
  summarise(ProteinEggs = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ProteinEggs_se", "Total_se")) %>% 
  rename(Disaggregation = Treatment) %>% 
  # round the values to 2 decimal places
  mutate(ProteinEggs = round(ProteinEggs, 2))

# Write the tables to an excel file
write.xlsx(SvyMADEggFlessTreatTab, "report tables/SvyMADEggFlessTreatTab.xlsx")


# Calculate the proportion of children who consumed egg or flesh foods, by regiontype
SvyMADEggFlessRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, PMADEggFlesh) %>%
  summarise(ProteinEggs = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ProteinEggs_se", "Total_se")) %>% 
  rename(Disaggregation  = regiontype) %>% 
  # round the values to 2 decimal places
  mutate(ProteinEggs = round(ProteinEggs, 2))

# Write the tables to an excel file
write.xlsx(SvyMADEggFlessRegionTab, "report tables/SvyMADEggFlessRegionTab.xlsx")


# Calculate the percentage of children who consumed egg or by gender


SvyMADEggFlessGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, PMADEggFlesh) %>%
  summarise(ProteinEggs = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ProteinEggs_se", "Total_se")) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>%
  rename(Disaggregation = ChildGender) %>%
  # round the values to 2 decimal places
  mutate(ProteinEggs = round(ProteinEggs, 2))

# Write the tables to an excel file
write.xlsx(SvyMADEggFlessGenderTab, "report tables/SvyMADEggFlessGenderTab.xlsx")


# Overall Egg or Flesh Foods
SvyMADEggFlessOverall <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PMADEggFlesh) %>%
  summarise(ProteinEggs = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ProteinEggs_se", "Total_se")) %>% 
  rename(Percentage = ProteinEggs) %>% 
  mutate(Indicator = "ProteinEggs", 
         Category = "Overall")


##########################################################################################

#SweetBeverage Consumption

SvyMADSweetBeverageTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PMADSwtBeverages) %>%
  summarise(SweetBeverage = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetBeverage_se", "Total_se")) %>% 
  rename(Disaggregation = Treatment) %>% 
  # round the values to 2 decimal places
  mutate(SweetBeverage = round(SweetBeverage, 2))

# Write the tables to an excel file
write.xlsx(SvyMADSweetBeverageTreatTab, "report tables/SvyMADSweetBeverageTreatTab.xlsx")

# Calculate the proportion of children who consumed sweet beverages, by regiontype
SvyMADSweetBeverageRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, PMADSwtBeverages) %>%
  summarise(SweetBeverage = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetBeverage_se", "Total_se")) %>% 
  rename(Disaggregation  = regiontype) %>% 
  # round the values to 2 decimal places
  mutate(SweetBeverage = round(SweetBeverage, 2))

# Write the tables to an excel file
write.xlsx(SvyMADSweetBeverageRegionTab, "report tables/SvyMADSweetBeverageRegionTab.xlsx")

# Calculate the percentage of children who consumed sweet beverages by gender

SvyMADSweetBeverageGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, PMADSwtBeverages) %>%
  summarise(SweetBeverage = survey_mean() * 100,
            Total = survey_total()) %>%
  dplyr::select(-c("SweetBeverage_se", "Total_se")) %>%
  mutate(ChildGender = as_factor(ChildGender)) %>%
  rename(Disaggregation = ChildGender) %>%
  # round the values to 2 decimal places
  mutate(SweetBeverage = round(SweetBeverage, 2))

# Write the tables to an excel file
write.xlsx(SvyMADSweetBeverageGenderTab, "report tables/SvyMADSweetBeverageGenderTab.xlsx")


# Overall Sweet Beverages
N <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PMADSwtBeverages) %>%
  summarise(SweetBeverage = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetBeverage_se", "Total_se")) %>% 
  rename(Percentage = SweetBeverage) %>% 
  mutate(Indicator = "SweetBeverage", 
         Category = "Overall")

############################################################################################################

SvyMADBreastfeedingTota <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADBreastfeeding) %>%
  summarise(Breastfeeding = survey_mean() * 100,
            Total = survey_total()) %>% 
  mutate(PCMADBreastfeeding = as.factor(PCMADBreastfeeding)) %>%
  dplyr::select(-c("Breastfeeding_se", "Total_se", "Total")) %>% 
  filter(PCMADBreastfeeding == 1) %>% 
  #   mutate(Indicator = "Staples"
  mutate(Indicator = "Breastfeeding") %>%
  rename(Percentage = Breastfeeding) %>%
  select(Indicator, Percentage)


SvyMADStaplesAgeTot <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADStaples) %>%
  summarise(Staples = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("Staples_se", "Total_se", "Total")) %>% 
  filter(PCMADStaples == 1) %>% 
#   mutate(Indicator = "Staples"
  mutate(Indicator = "Staples") %>%
  rename(Percentage = Staples) %>%
  select(Indicator, Percentage)

SvyMADProteinEggsAgeGrpTot <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADProteinEggs) %>%
  summarise(ProteinEggs = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("ProteinEggs_se", "Total_se", "Total")) %>% 
  filter(PCMADProteinEggs == 1) %>% 
  mutate(Indicator = "Eggs") %>%
  rename(Percentage = ProteinEggs) %>%
  select(Indicator, Percentage)

SvyMADLegumesAgeGrpTot <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADLegumes) %>%
  summarise(Legumes = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("Legumes_se", "Total_se", "Total")) %>%
  filter(PCMADLegumes == 1) %>%
  mutate(Indicator = "Legumes") %>%
  rename(Percentage = Legumes) %>%
  select(Indicator, Percentage)

SvyMADDairyAgeGrpTot <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADDairy) %>%
  summarise(Dairy = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("Dairy_se", "Total_se", "Total")) %>%
  filter(PCMADDairy == 1) %>%
  mutate(Indicator = "Dairy") %>%
  rename(Percentage = Dairy) %>%
  select(Indicator, Percentage)


SvyMADFleshFoodsAgeGrpTot <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADFleshFoods) %>%
  summarise(FleshFoods = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("FleshFoods_se", "Total_se", "Total")) %>% 
  filter(PCMADFleshFoods == 1) %>% 
  mutate(Indicator = "FleshFoods") %>%
  rename(Percentage = FleshFoods) %>%
  select(Indicator, Percentage)


SvyMADVitAAgeGrpTot <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADVitA) %>%
  summarise(VitA = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("VitA_se", "Total_se", "Total")) %>%
  filter(PCMADVitA == 1) %>%
  mutate(Indicator = "VitA") %>%
  rename(Percentage = VitA) %>%
  select(Indicator, Percentage)


SvyMADOtherFruitsVegAgeGrpTot <- SvyMADData %>%
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADOtherFruitsVeg) %>%
  summarise(OtherFruitsVeg = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("OtherFruitsVeg_se", "Total_se", "Total")) %>% 
  filter(PCMADOtherFruitsVeg == 1) %>%
  mutate(Indicator = "OtherFruitsVeg") %>%
  rename(Percentage = OtherFruitsVeg) %>%
  select(Indicator, Percentage)


SvyChildrenFoodGroupsAgeGrpTot <- rbind(SvyMADStaplesAgeTot, 
                                  SvyMADProteinEggsAgeGrpTot, 
                                  SvyMADLegumesAgeGrpTot, 
                                  SvyMADDairyAgeGrpTot, 
                                  SvyMADFleshFoodsAgeGrpTot, 
                                  SvyMADVitAAgeGrpTot, 
                                  SvyMADOtherFruitsVegAgeGrpTot,
                                  SvyMADBreastfeedingTota) %>% 
  mutate(Percentage = round(Percentage, 2))

# Write the tables to an excel file

write.xlsx(SvyChildrenFoodGroupsAgeGrpTot, "report tables/SvyChildrenFoodGroupsAgeGrpTot.xlsx")


###################################################################################################

#Consumption of individual unhealth foods

# Sweet milk

SvyMADSweetMilkTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADMilkSwt) %>%
  summarise(SweetMilk = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetMilk_se", "Total_se")) %>% 
  mutate( Indicator = "Sweet Milk") %>% 
  # round the values to 2 decimal places
  mutate(SweetMilk = round(SweetMilk, 2)) %>% 
  filter(PCMADMilkSwt == 1) %>% 
  rename(Percentage = SweetMilk) %>% 
  select(Indicator, Percentage, Total)

# PCMADYogurtDrinkSwt

SvyMADSweetYogurtDrinkTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADYogurtDrinkSwt) %>%
  summarise(SweetYogurtDrink = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetYogurtDrink_se", "Total_se")) %>% 
  mutate( Indicator = "Yogurt Drink") %>% 
  # round the values to 2 decimal places
  mutate(SweetYogurtDrink = round(SweetYogurtDrink, 2)) %>% 
  filter(PCMADYogurtDrinkSwt == 1) %>% 
  rename(Percentage = SweetYogurtDrink) %>% 
  select(Indicator, Percentage, Total)

# PCMADOtherMilkSwtFlavoured

SvyMADSweetOtherMilkTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADOtherMilkSwtFlavoured) %>%
  summarise(SweetOtherMilk = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetOtherMilk_se", "Total_se")) %>% 
  mutate( Indicator = "Other Milk") %>% 
  # round the values to 2 decimal places
  mutate(SweetOtherMilk = round(SweetOtherMilk, 2)) %>% 
  filter(PCMADOtherMilkSwtFlavoured == 1) %>% 
  rename(Percentage = SweetOtherMilk) %>% 
  select(Indicator, Percentage, Total)

# PCMADTeaSwt

SvyMADSweetTeaTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADTeaSwt) %>%
  summarise(SweetTea = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetTea_se", "Total_se")) %>% 
  mutate( Indicator = "Sweet Tea") %>% 
  # round the values to 2 decimal places
  mutate(SweetTea = round(SweetTea, 2)) %>% 
  filter(PCMADTeaSwt == 1) %>% 
  rename(Percentage = SweetTea) %>% 
  select(Indicator, Percentage, Total)

# PCMADAnyOtherLiquidsSwt

SvyMADSweetOtherLiquidsTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADAnyOtherLiquidsSwt) %>%
  summarise(SweetOtherLiquids = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetOtherLiquids_se", "Total_se")) %>% 
  mutate( Indicator = "Other Sweet Liquids") %>% 
  # round the values to 2 decimal places
  mutate(SweetOtherLiquids = round(SweetOtherLiquids, 2)) %>% 
  filter(PCMADAnyOtherLiquidsSwt == 1) %>% 
  rename(Percentage = SweetOtherLiquids) %>% 
  select(Indicator, Percentage, Total)

# PCMADSoftDrink

SvyMADSweetSoftDrinkTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADSoftDrink) %>%
  summarise(SweetSoftDrink = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetSoftDrink_se", "Total_se")) %>% 
  mutate( Indicator = "Soft Drink") %>% 
  # round the values to 2 decimal places
  mutate(SweetSoftDrink = round(SweetSoftDrink, 2)) %>% 
  filter(PCMADSoftDrink == 1) %>% 
  rename(Percentage = SweetSoftDrink) %>% 
  select(Indicator, Percentage, Total)



#PCMADFruiteJuice

SvyMADSweetFruitJuiceTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADFruiteJuice) %>%
  summarise(SweetFruitJuice = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetFruitJuice_se", "Total_se")) %>% 
  mutate( Indicator = "Fruit Juice") %>% 
  # round the values to 2 decimal places
  mutate(SweetFruitJuice = round(SweetFruitJuice, 2)) %>% 
  filter(PCMADFruiteJuice == 1) %>% 
  rename(Percentage = SweetFruitJuice) %>% 
  select(Indicator, Percentage, Total)

# PCMADChocolateFrappe

SvyMADSweetChocolateFrappeTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADChocolateFrappe) %>%
  summarise(SweetChocolateFrappe = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetChocolateFrappe_se", "Total_se")) %>% 
  mutate( Indicator = "Chocolate Frappe") %>% 
  # round the values to 2 decimal places
  mutate(SweetChocolateFrappe = round(SweetChocolateFrappe, 2)) %>% 
  filter(PCMADChocolateFrappe == 1) %>% 
  rename(Percentage = SweetChocolateFrappe) %>% 
  select(Indicator, Percentage, Total)



# PCMADSweetsCakes 

SvyMADSweetCakesTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADSweetsCakes) %>%
  summarise(SweetCakes = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetCakes_se", "Total_se")) %>% 
  mutate( Indicator = "Sweet Cakes") %>% 
  # round the values to 2 decimal places
  mutate(SweetCakes = round(SweetCakes, 2)) %>% 
  filter(PCMADSweetsCakes == 1) %>% 
  rename(Percentage = SweetCakes) %>% 
  select(Indicator, Percentage, Total)

# PCMADSweetsCandy

SvyMADSweetCandyTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADSweetsCandy) %>%
  summarise(SweetCandy = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetCandy_se", "Total_se")) %>% 
  mutate( Indicator = "Sweet Candy") %>% 
  # round the values to 2 decimal places
  mutate(SweetCandy = round(SweetCandy, 2)) %>% 
  filter(PCMADSweetsCandy == 1) %>% 
  rename(Percentage = SweetCandy) %>% 
  select(Indicator, Percentage, Total)

# PCMADOtherChips

SvyMADSweetChipsTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADOtherChips) %>%
  summarise(SweetChips = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetChips_se", "Total_se")) %>% 
  mutate( Indicator = "Salty Chips") %>% 
  # round the values to 2 decimal places
  mutate(SweetChips = round(SweetChips, 2)) %>% 
  filter(PCMADOtherChips == 1) %>% 
  rename(Percentage = SweetChips) %>% 
  select(Indicator, Percentage, Total)


#PCMADOtherNoodles

SvyMADSweetNoodlesTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADOtherNoodles) %>%
  summarise(SweetNoodles = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetNoodles_se", "Total_se")) %>% 
  mutate( Indicator = "Instant Noodles") %>% 
  # round the values to 2 decimal places
  mutate(SweetNoodles = round(SweetNoodles, 2)) %>% 
  filter(PCMADOtherNoodles == 1) %>% 
  rename(Percentage = SweetNoodles) %>% 
  select(Indicator, Percentage, Total)


# PCMADOtherFriedChicken

SvyMADSweetFriedChickenTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADOtherFriedChicken) %>%
  summarise(SweetFriedChicken = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetFriedChicken_se", "Total_se")) %>% 
  mutate( Indicator = "Fried Chicken") %>% 
  # round the values to 2 decimal places
  mutate(SweetFriedChicken = round(SweetFriedChicken, 2)) %>% 
  filter(PCMADOtherFriedChicken == 1) %>% 
  rename(Percentage = SweetFriedChicken) %>% 
  select(Indicator, Percentage, Total)


# PCMADOtherEatOut

SvyMADSweetEatOutTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(PCMADOtherEatOut) %>%
  summarise(SweetEatOut = survey_mean() * 100,
            Total = survey_total()) %>% 
  dplyr::select(-c("SweetEatOut_se", "Total_se")) %>% 
  mutate( Indicator = "Eat Out") %>% 
  # round the values to 2 decimal places
  mutate(SweetEatOut = round(SweetEatOut, 2)) %>% 
  filter(PCMADOtherEatOut == 1) %>% 
  rename(Percentage = SweetEatOut) %>% 
  select(Indicator, Percentage, Total)

# Compine the tables

SvyMADSweetUnhealthyFoodsTreatTab <- rbind(SvyMADSweetMilkTreatTab, 
                                  SvyMADSweetYogurtDrinkTreatTab, 
                                  SvyMADSweetOtherMilkTreatTab, 
                                  SvyMADSweetTeaTreatTab, 
                                  SvyMADSweetOtherLiquidsTreatTab, 
                                  SvyMADSweetSoftDrinkTreatTab, 
                                  SvyMADSweetFruitJuiceTreatTab, 
                                  SvyMADSweetChocolateFrappeTreatTab, 
                                  SvyMADSweetCakesTreatTab, 
                                  SvyMADSweetCandyTreatTab, 
                                  SvyMADSweetChipsTreatTab, 
                                  SvyMADSweetNoodlesTreatTab, 
                                  SvyMADSweetFriedChickenTreatTab, 
                                  SvyMADSweetEatOutTreatTab) %>% 
  mutate(Percentage = round(Percentage, 2))

# Write the tables to an excel file

write.xlsx(SvyMADSweetUnhealthyFoodsTreatTab, "report tables/SvyMADSweetUnhealthyFoodsTreatTab.xlsx")


UnhealthyFoodsGrapg <- SvyMADSweetUnhealthyFoodsTreatTab %>% 
  filter(Indicator != "Chocolate Frappe" & Indicator != "Eat Out") %>% 
  ggplot(aes(x = fct_reorder(Indicator, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "#254769", width = 0.6) +
  geom_text(aes(label = paste0(Percentage, "%"), y = Percentage / 2), 
            size = 2.5, family = "opensans", color = "#ffffff") +
  coord_flip() +
  theme_clean() + 
  labs(
    title = "Unhealthy Foods/and or Drinks consumed by children",
    x = "Food Group",
    y = "Percentage",
    caption = "Data Source: Family Package Baseline"
  ) + 
  theme(
    # Theme the panel
    panel.background = element_rect(fill = "white", color = "white", size = 0.5),
    panel.border = element_blank(),
    # Theme the x-axis
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.line.x = element_blank(),
    # Theme the y-axis
    axis.text.y = element_text(family = "opensans", size = 7, color = "black"),
    axis.title.y = element_blank(),
    # Theme the titles
    plot.title = element_text(family = "opensans", size = 8, color = "black", face = "bold",
                              margin = margin(l = 0, b = 5)),
    plot.subtitle = element_text(family = "opensans", size = 7, color = "black", face = "italic",
                                 margin = margin(l = 0, b = 8)),
    plot.caption = element_text(family = "opensans", size = 6, color = "black", hjust = 0, margin = margin(t = 5, l = 0, b = 10)),
    # Underline strip text by adding a bottom border to the strip
    strip.background = element_blank(), # Removes the background and border
    strip.placement = "bottom",  # Places the strip text outside the plot
    strip.text = element_text(margin = margin(b = 0, unit = "cm"), family = "opensans", face = "bold"))



ggsave("figures/UnhealthyFoodsGraph.png", 
       plot = UnhealthyFoodsGrapg, 
       width = 6.27, height = 3.09, 
       units = "in", bg = "white")



















