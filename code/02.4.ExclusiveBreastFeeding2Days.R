# 

library(tidyverse)
library(haven)
library(labelled)
library(survey)
library(srvyr)


# Load the data

ExlusiveBF <- read_dta("new data/sec4.dta") %>% 
  select(hhid, pid, AGE_CH, sec4_10)

ExcluiveBFSec6 <- read_dta("new data/sec6.dta") %>% 
  select(hhid, pid, age, sex)

SurveyDesignDataEBF <- read_dta("new data/cover.dta") %>% 
  select(hhid, clusterid, IDPOOR, strataid, GPS__Latitude, GPS__Longitude, GPS__Accuracy, GPS__Altitude, GPS__Timestamp, regiontype, poorscore)

# Merge the data
BreastFeedingData <- left_join(ExlusiveBF, ExcluiveBFSec6, by = c("hhid", "pid")) %>% 
  left_join(SurveyDesignDataEBF, by = "hhid") %>% 
  # Create treatment variable
  mutate(Treatment = case_when(
    IDPOOR == 1 | IDPOOR == 2 ~ "Treatment Group",
    TRUE ~ "Control Group")) %>% 
  filter(sec4_10 != -99) %>% 
  # Change sec4_10 to factor
  mutate(sec4_10 = as_factor(sec4_10))

# Create the survey design object

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
  select(sec4_10, EBF, Total)


# Write data into an excel file

write.xlsx(SvyBreastFed2Treat, "report tables/BreastFeeding2Treat.xlsx")

# Calculate the percentage of exclusive breastfeeding by region type

SvyBreastFed2Region <- SvyBreastFeedingData %>% 
  group_by(regiontype, sec4_10) %>% 
  summarise(EBF = survey_prop() * 100,
            Total = survey_total()) %>% 
  mutate(EBF = round(EBF, 2)) %>% 
  select(regiontype, sec4_10, EBF, Total)

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

































