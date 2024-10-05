# 

library(tidyverse)
library(haven)
library(labelled)
library(survey)
library(srvyr)


# Load the data

ExlusiveBF <- read_dta("new data/sec4.dta") %>% 
  dplyr::select(hhid, pid, AGE_CH, sec4_10)

ExcluiveBFSec6 <- read_dta("new data/sec6.dta") %>% 
  dplyr::select(hhid, pid, age, sex)

# Merge the data
BreastFeedingData <- left_join(ExlusiveBF, ExcluiveBFSec6, by = c("hhid", "pid")) 
# Create the survey design object

# Write dta file
write_dta(BreastFeedingData, "clean data/BreastFeedingData.dta")

SvyBreastFeedingData <- BreastFeedingData %>% 
  as_survey_design(ids = clusterid, 
            strata = strataid, 
            weights = NULL)

options(survey.lonely.psu = "adjust")


# Calculate the percentage of exclusive breastfeeding by treatment group

SvyBreastFed2Treat <- SvyBreastFeedingData %>% 
  group_by(sec4_10) %>% 
  summarise(EBF = survey_prop() * 100,
            Total = survey_total()) %>% 
  mutate(EBF = round(EBF, 2)) %>% 
  dplyr::select(sec4_10, EBF, Total)


# Write data into an excel file

write.xlsx(SvyBreastFed2Treat, "report tables/BreastFeeding2Treat.xlsx")

# Calculate the percentage of exclusive breastfeeding by region type

SvyBreastFed2Region <- SvyBreastFeedingData %>% 
  group_by(regiontype, sec4_10) %>% 
  summarise(EBF = survey_prop() * 100,
            Total = survey_total()) %>% 
  mutate(EBF = round(EBF, 2)) %>% 
  dplyr::select(regiontype, sec4_10, EBF, Total)

# Write data into an excel file

write.xlsx(SvyBreastFed2Region, "report tables/BreastFeeding2Region.xlsx")


# Calculate the percentage of exclusive breastfeeding by gender

SvyBreastFed2Gender <- SvyBreastFeedingData %>% 
  group_by(sex, sec4_10) %>%
  summarise(EBF = survey_prop() * 100,
            Total = survey_total()) %>%
  mutate(EBF = round(EBF, 2)) %>%
  select(sex, sec4_10, EBF, Total)


# Write data into an excel file

write.xlsx(SvyBreastFed2Gender, "report tables/BreastFeeding2Gender.xlsx")



# Calculate the percentage of exclusive breastfeeding by treatment

SvyBreastFed2Treatment <- SvyBreastFeedingData %>% 
  group_by(Treatment, sec4_10) %>% 
  summarise(EBF = survey_prop() * 100,
            Total = survey_total()) %>% 
  mutate(EBF = round(EBF, 2)) %>% 
  select(Treatment, sec4_10, EBF, Total)

# write data into an excel file

write.xlsx(SvyBreastFed2Treatment, "report tables/BreastFeeding2Treatment.xlsx")



#####################################################################################

#Perform statistical significance tests

# Test the difference in exclusive breastfeeding by treatment group

SvyBreastFed2TreatTest <- svychisq(~sec4_10 + Treatment, design = SvyBreastFeedingData)

# Thest the difference in exclusive breastfeeding by region type

SvyBreastFed2RegionTest <- svychisq(~sec4_10 + regiontype, design = SvyBreastFeedingData)

# Test the difference in exclusive breastfeeding by gender

SvyBreastFed2GenderTest <- svychisq(~sec4_10 + sex, design = SvyBreastFeedingData)

































