

## Loading libraries

library(tidyverse)
library(openxlsx)
library(showtext)
library(patchwork)
library(cowplot)
library(stringr)


source("code/SurveyDesign.R") # This file converts the data into survey designed data
source("code/Functions.R") # Functions for significance tests and creating Word documents


# Nutrition for children under the age of 2 years (23 months)

# Calculate the proportion of children who were breastfed over the last 24 hours
SvyMADBreasfed <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("PropotionBreastfed_se", "Total_se", "Total")) %>% 
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) %>% 
  filter(PCMADBreastfeeding == "Yes") %>% 
  pivot_wider(names_from = Treatment, values_from = PropotionBreastfed) %>% 
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "Breastfeeding") %>%
  select(Indicator, PCMADBreastfeeding, Overall, `Control Group`, `Treatment Group`, Diff) %>% 
  rename(Category = PCMADBreastfeeding) %>% 
  mutate(Category = as_factor(Category))

## Proportion of breasfed children in the last day, by regiontype
SvyMADBreastFeedindRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("PropotionBreastfed_se", "Total_se", "Total")) %>%
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) %>%
  filter(PCMADBreastfeeding == "Yes") %>%
  rename(Disaggregation = regiontype) %>% 
  select(Disaggregation, PCMADBreastfeeding, PropotionBreastfed)

# Calculate the proportion of children who were breastfed over the last 24 hours, by child gender
SvyMADBreasfedGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(PCMADBreastfeeding, ChildGender) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("PropotionBreastfed_se", "Total_se", "Total")) %>% 
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding),
         ChildGender = as_factor(ChildGender)) %>%
  filter(PCMADBreastfeeding == "Yes") %>% 
  rename(Disaggregation = ChildGender) %>% 
  select(Disaggregation, PCMADBreastfeeding, PropotionBreastfed)

SvyMADBreastfedTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PCMADBreastfeeding) %>%
  summarise(PropotionBreastfed = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("PropotionBreastfed_se", "Total_se", "Total")) %>% 
  mutate(PCMADBreastfeeding = as_factor(PCMADBreastfeeding)) %>% 
  filter(PCMADBreastfeeding == "Yes") %>% 
  rename(Disaggregation = Treatment)

# Combine the tables
SvyMADBreastfedTab <- rbind(SvyMADBreastFeedindRegionTab, SvyMADBreasfedGenderTab, SvyMADBreastfedTreatTab) %>% 
  select(-c("PCMADBreastfeeding"))

###############################################################################################################################
##Testing the difference in the proportions of children who were breastfed in the last 24 hours
MADBrestfedProptestIE <- svychisq(~PCMADBreastfeeding + Treatment, design = SvyMADData) # Not statistically significant at all levels
MADBrestfedProptestGender <- svychisq(~PCMADBreastfeeding + ChildGender, design = SvyMADData) # Not Statistically significant at all levels
MADBrestfedProptestRegion <- svychisq(~PCMADBreastfeeding + regiontype, design = SvyMADData) # Not statistically significant
###############################################################################################################################


# Calculate the proportion of children meeting minimum diteray diversity
SvyMDDChildren <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MDDCat) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MDDChilden_se", "Total_se", "Total")) %>% 
  filter(MDDCat == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MDDChilden) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MDD") %>%
  select(Indicator, MDDCat, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MDDCat) %>% 
  mutate(Category = as_factor(Category))

SvyMDDChildrenTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MDDCat) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MDDChilden_se", "Total_se", "Total")) %>% 
  filter(MDDCat == 1) %>% 
  rename(Disaggregation = Treatment)

# Calculate the proportion of children meeting minimum dietary diversity, by regiontype
SvyMDDChildrenRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, MDDCat) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MDDChilden_se", "Total_se", "Total")) %>% 
  filter(MDDCat == 1) %>% 
  rename(Disaggregation = regiontype)

# Calculate the proportion of children meeting minimum dietary diversity, by tchild gender
SvyMDDChildrenGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, MDDCat) %>%
  summarise(MDDChilden = survey_prop() * 100,
            Total = survey_total()) %>% 
  select(-c("MDDChilden_se", "Total_se", "Total")) %>% 
  filter(MDDCat == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>%
  rename(Disaggregation = ChildGender)

# Combine the tables
SvyMDDChildrenTab <- rbind(SvyMDDChildrenTreatTab, SvyMDDChildrenRegionTab, SvyMDDChildrenGenderTab) %>% 
  select(-c("MDDCat"))

#SvyMDD by age group
SvyMDDChildrenAgeTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, MDDCat) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MDDChilden_se", "Total_se", "Total")) %>% 
  filter(MDDCat == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  select(Disaggregation, MDDChilden)


##################################################################################################################################
#Testing the difference in the proportions of children who met the minimum dietary diversity
MDDChisqtestIE <- svychisq(~MDDCat + Treatment, design = SvyMADData) ## Biig difference and statistically Significant at 5% level
MDDChisqtestRegion <- svychisq(~MDDCat + regiontype, design = SvyMADData) ## Small difference and not statistically significant at all levels
MDDChisqtestGender <- svychisq(~MDDCat + ChildGender, design = SvyMADData) ## Not statistically significant at all levels
MDDChisqAge <- svychisq(~MDDCat + ChildAgeGroup, design = SvyMADData) ## Highly statistically significant at all levels
##################################################################################################################################
## Calculate the proportion of children meeting minimum dietary diversity, by child gender
SvyMDDChildrenGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MDDCat, ChildGender) %>%
  summarise(MDDChilden = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MDDChilden_se", "Total_se", "Total")) %>% 
  filter(MDDCat == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MDDChilden) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MDD") %>%
  select(Indicator, MDDCat, ChildGender, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MDDCat) %>% 
  mutate(Category = as_factor(Category),
         ChildGender = as_factor(ChildGender))


# Calculate the proportion of children who met the MMF
SvyMMFChildren <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MMF) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFChildren_se", "Total_se", "Total")) %>% 
  filter(MMF == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MMFChildren) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MMF") %>%
  select(Indicator, MMF, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MMF) %>% 
  mutate(Category = as_factor(Category))

SvyMMFChildrenTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MMF) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFChildren_se", "Total_se", "Total")) %>% 
  filter(MMF == 1) %>% 
  rename(Disaggregation = Treatment)

# Calculate the proportion of children who met MMF, by regiontype
SvyMMFChildrenRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, MMF) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFChildren_se", "Total_se", "Total")) %>% 
  filter(MMF == 1) %>% 
  rename(Disaggregation = regiontype)

# Calculate the proportion of children who met MMF, by child gender
SvyMMFChildrenGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, MMF) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFChildren_se", "Total_se", "Total")) %>% 
  filter(MMF == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>% 
  rename(Disaggregation = ChildGender)

# Combine the tables
SvyMMFChildrenTab <- rbind(SvyMMFChildrenTreatTab, SvyMMFChildrenRegionTab, SvyMMFChildrenGenderTab) %>% 
  select(-c("MMF"))

# Calculate the proportion of children who MMF  by age group
SvyMMFChildrenAgeTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, MMF) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFChildren_se", "Total_se", "Total")) %>% 
  filter(MMF == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  select(Disaggregation, MMFChildren)

##################################################################################################################################
#testing the difference in the proportions of children who met the minimum meal frequency using the chisq test
MMFChisqtestIE <- svychisq(~MMF + Treatment, design = SvyMADData) ## Not statistically significant at all levels
MMFChisqtestRegion <- svychisq(~MMF + regiontype, design = SvyMADData) ## Not statistically significant at all levels
MMFChisqtestGender <- svychisq(~MMF + ChildGender, design = SvyMADData) ## Not statistically significant at all levels
MMFChisqAge <- svychisq(~MMF + ChildAgeGroup, design = SvyMADData) ## Highly statistically significant at all levels
#################################################################################################################################
# Calculate the proportion of children meeting MMF in each group
## Calculate the percentage of children who met MMF, by child gender
SvyMMFChildrenGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MMF, ChildGender) %>%
  summarise(MMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFChildren_se", "Total_se")) %>% 
  filter(MMF == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MMFChildren) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MMF") %>%
  select(Indicator, MMF, ChildGender, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MMF) %>% 
  mutate(Category = as_factor(Category),
         ChildGender = as_factor(ChildGender))

# Calculate the proportion of children who met the MMFF
SvyMMFFChildren <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
  group_by(Treatment, MMFF) %>%
  summarise(MMFFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFFChildren_se", "Total_se", "Total")) %>% 
  filter(MMFF == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MMFFChildren) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MMFF") %>%
  select(Indicator, MMFF, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MMFF) %>% 
  mutate(Category = as_factor(Category))

## Calculate the percentage of children who met MMFF, by child gender
SvyMMFFChildrenGender <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
  group_by(Treatment, MMFF, ChildGender) %>%
  summarise(MMFFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFFChildren_se", "Total_se", "Total")) %>% 
  filter(MMFF == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MMFFChildren) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MMFF") %>%
  select(Indicator, MMFF, ChildGender, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MMFF) %>% 
  mutate(Category = as_factor(Category),
         ChildGender = as_factor(ChildGender))

# Challculate the proportion of children who met MMFF, by regiontype
SvyMMFFChildrenRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
  group_by(regiontype, MMFF) %>%
  summarise(MMFFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFFChildren_se", "Total_se", "Total")) %>% 
  filter(MMFF == 1) %>% 
  rename(Disaggregation  = regiontype)

# Calculate the proportion of children who met MMFF, by child gender
SvyMMFFChildrenGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
  group_by(ChildGender, MMFF) %>%
  summarise(MMFFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFFChildren_se", "Total_se", "Total")) %>% 
  filter(MMFF == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>% 
  rename(Disaggregation = ChildGender)

# Calculate the proportion of children who met MMFF, by treatment
SvyMMFFChildrenTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  filter(!is.na(MMFF)) %>% # This code can be changed tp filter the children who are not being breastfed (Still gives the same results)
  group_by(Treatment, MMFF) %>%
  summarise(MMFFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MMFFChildren_se", "Total_se", "Total")) %>% 
  filter(MMFF == 1) %>% 
  rename(Disaggregation = Treatment)

# Combining the tables

SvyChildrenMMFFTab <- rbind(SvyMMFFChildrenTreatTab, SvyMMFFChildrenRegionTab, SvyMMFFChildrenGenderTab) %>% 
  select(-c("MMFF"))
##################################################################################################################################
# Perform the chisq test to test the difference in the proportions of children who met the minimum meal frequency
MMFFChisqtestIE <- svychisq(~MMFF + Treatment, design = SvyMADData) ## Not statistically significant at all levels
MMFFChisqtestRegion <- svychisq(~MMFF + regiontype, design = SvyMADData) ## The difference is statistically significant at 5% level
MMFFChisqtestGender <- svychisq(~MMFF + ChildGender, design = SvyMADData) ## The difference is not statistically significant at all levels
##################################################################################################################################

# Calculate the proportion of children who met MAD
SvyMADChildren <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MADChildren_se", "Total_se", "Total")) %>% 
  filter(MAD == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MADChildren) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MAD") %>%
  select(Indicator, MAD, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MAD) %>% 
  mutate(Category = as_factor(Category))

# Calculate the proportion of children who met MAD, by regiontype
SvyMADChildrenRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MADChildren_se", "Total_se", "Total")) %>% 
  filter(MAD == 1) %>% 
  rename(Disaggregation  = regiontype)

# Calculate the percentage of children who met MAD, by treatment
SvyMADChildrenTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MADChildren_se", "Total_se", "Total")) %>% 
  filter(MAD == 1) %>% 
  rename(Disaggregation = Treatment)

# Calculate the proportion of children who met MAD, by child gender
SvyMADChildrenGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MADChildren_se", "Total_se", "Total")) %>% 
  filter(MAD == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>% 
  rename(Disaggregation = ChildGender)

# combine the tables
SvyChildrenMADTab <- rbind(SvyMADChildrenTreatTab, SvyMADChildrenRegionTab, SvyMADChildrenGenderTab) %>% 
  select(-c("MAD"))

# Calculate the proportion of children who met MAD, by age group

SvyMADChildrenAgeTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildAgeGroup, MAD) %>%
  summarise(MADChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MADChildren_se", "Total_se", "Total")) %>% 
  filter(MAD == 1) %>% 
  rename(Disaggregation = ChildAgeGroup) %>% 
  select(Disaggregation, MADChildren)
##################################################################################################################################

# Perfom the chisq test to test the difference in the proportions of children who met the minimum acceptable diet
MADChisqtestIE <- svychisq(~MAD + Treatment, design = SvyMADData) ## The difference is weeakily statistically significant at 10% level
MADChisqtestRegion <- svychisq(~MAD + regiontype, design = SvyMADData) ## The difference is not statistically significant at all levels
MADChisqtestGender <- svychisq(~MAD + ChildGender, design = SvyMADData) ## The difference is not statistically significant at all levels
MADChisqtestAge <- svychisq(~MAD + ChildAgeGroup, design = SvyMADData) ## The difference is highly statistically significant at all levels

##################################################################################################################################
# calculate MIXED MILK FEEDING UNDER SIX MONTHS (MixMF)
SvyMADMixMF <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 5) %>% 
  mutate(MixMF = case_when(
    PCMADBreastfeeding == 1 & (PCMADInfFormula == 1 | PCMADMilk == 1) ~ 1,
    TRUE ~ 0)) %>%
  group_by(Treatment, MixMF) %>%
  summarise(MixMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MixMFChildren_se", "Total_se", "Total")) %>% 
  filter(MixMF == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = MixMFChildren) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "MixMF") %>%
  select(Indicator, MixMF, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = MixMF) %>% 
  mutate(Category = as_factor(Category))

# Subsetting the data to be used in the chisq tests - It can also be used to calculate the proportions of children who met MixMF
SvyMADMixMFData <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 5) %>% 
  mutate(MixMF = case_when(
    PCMADBreastfeeding == 1 & (PCMADInfFormula == 1 | PCMADMilk == 1) ~ 1,
    TRUE ~ 0)) 

# Calculate the proportion of children who met MixMF, by regiontype
SvyMADMixMFRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 5) %>% 
  mutate(MixMF = case_when(
    PCMADBreastfeeding == 1 & (PCMADInfFormula == 1 | PCMADMilk == 1) ~ 1,
    TRUE ~ 0)) %>%
  group_by(regiontype, MixMF) %>%
  summarise(MixMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MixMFChildren_se", "Total_se", "Total")) %>% 
  filter(MixMF == 1) %>% 
  rename(Disaggregation  = regiontype)

# Calculate the proportion of children who met MixMF, by child gender
SvyMADMixMFGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 5) %>% 
  mutate(MixMF = case_when(
    PCMADBreastfeeding == 1 & (PCMADInfFormula == 1 | PCMADMilk == 1) ~ 1,
    TRUE ~ 0)) %>%
  group_by(ChildGender, MixMF) %>%
  summarise(MixMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MixMFChildren_se", "Total_se", "Total")) %>% 
  filter(MixMF == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>%
  rename(Disaggregation = ChildGender)

# Calculate the proportion of children who met MixMF, by treatment
SvyMADMixMFTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 0 & ChildAgeMonths <= 5) %>% 
  mutate(MixMF = case_when(
    PCMADBreastfeeding == 1 & (PCMADInfFormula == 1 | PCMADMilk == 1) ~ 1,
    TRUE ~ 0)) %>%
  group_by(Treatment, MixMF) %>%
  summarise(MixMFChildren = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("MixMFChildren_se", "Total_se", "Total")) %>% 
  filter(MixMF == 1) %>% 
  rename(Disaggregation = Treatment)

# Combine the tables
SvyChildrenMixMFTab <- rbind(SvyMADMixMFTreatTab, SvyMADMixMFRegionTab, SvyMADMixMFGenderTab) %>% 
  select(-c("MixMF"))

##################################################################################################################################
# perform the chisq test to test the difference in the proportions of children who met the mixed milk feeding
MixMFChisqtestIE <- svychisq(~MixMF + Treatment, design = SvyMADMixMFData) ## The difference is not statistically significant at all levels
MixMFChisqtestRegion <- svychisq(~MixMF + regiontype, design = SvyMADMixMFData) ## The difference is not statistically significant at all levels
MixMFChisqtestGender <- svychisq(~MixMF + ChildGender, design = SvyMADMixMFData) ## The difference is not statistically significant at all levels
##################################################################################################################################

# Calculate Sentinel Unhealthy Foods Consumption

SvyMADUnhealthyFoods <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PCMADUnhealthyFds) %>%
  summarise(UnhealthyFoods = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("UnhealthyFoods_se", "Total_se", "Total")) %>% 
  filter(PCMADUnhealthyFds == 1) %>% 
  pivot_wider(names_from = Treatment, values_from = UnhealthyFoods) %>%
  mutate(Diff = `Treatment Group` - `Control Group`) %>%
  mutate(Overall = (`Control Group` + `Treatment Group`)/2) %>%
  mutate(Indicator = "UnhealthyFoods") %>%
  select(Indicator, PCMADUnhealthyFds, Overall, `Control Group`, `Treatment Group`, Diff) %>%
  rename(Category = PCMADUnhealthyFds) %>% 
  mutate(Category = as_factor(Category))


# Calculate the proportion of children who consumed unhealthy foods, by regiontype
SvyMADUnhealthyFoodsRegionTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(regiontype, PCMADUnhealthyFds) %>%
  summarise(UnhealthyFoods = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("UnhealthyFoods_se", "Total_se", "Total")) %>% 
  filter(PCMADUnhealthyFds == 1) %>% 
  rename(Disaggregation  = regiontype)

# Calculate the proportion of children who consumed unhealthy, by treatment
SvyMADUnhealthyFoodsTreatTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(Treatment, PCMADUnhealthyFds) %>%
  summarise(UnhealthyFoods = survey_mean() * 100,
            Total = survey_total()) %>% 
  select(-c("UnhealthyFoods_se", "Total_se", "Total")) %>% 
  filter(PCMADUnhealthyFds == 1) %>% 
  rename(Disaggregation = Treatment)

# Calculate the percentage of children who consumed unhealthy foods by child gender
SvyMADUnhealthyFoodsGenderTab <- SvyMADData %>% 
  filter(ChildAgeMonths >= 6 & ChildAgeMonths <= 23) %>%
  group_by(ChildGender, PCMADUnhealthyFds) %>%
  summarise(UnhealthyFoods = survey_mean() * 100,
            Total = survey_total()) %>%
  select(-c("UnhealthyFoods_se", "Total_se", "Total")) %>%
  filter(PCMADUnhealthyFds == 1) %>% 
  mutate(ChildGender = as_factor(ChildGender)) %>% 
  rename(Disaggregation = ChildGender)

# Combine the tables
SvyChildrenUnhealthyFoodsTab <- rbind(SvyMADUnhealthyFoodsTreatTab, SvyMADUnhealthyFoodsRegionTab, SvyMADUnhealthyFoodsGenderTab) %>% 
  select(-c("PCMADUnhealthyFds"))

##################################################################################################################################
# Perform the chisq test to test the difference in the proportions of children who consumed unhealthy foods
UnhealthyFoodsChisqtestIE <- svychisq(~PCMADUnhealthyFds + Treatment, design = SvyMADData) ## The difference is statistically significant at all 10%
UnhealthyFoodsChisqtestRegion <- svychisq(~PCMADUnhealthyFds + regiontype, design = SvyMADData) ## The difference is not statistically significant at all levels
UnhealthyFoodsChisqtestGender <- svychisq(~PCMADUnhealthyFds + ChildGender, design = SvyMADData) ## The difference is not statistically significant at all levels
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
  pivot_wider(names_from = Disaggregation, values_from = Percentage) %>% 
  # round the values to 2 decimal places
  mutate(across(is.numeric, ~round(., 2)))

# Write an excel file
write.xlsx(SvyMADNutritionIndicators, "results/SvyMADNutritionIndicators.xlsx")

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
