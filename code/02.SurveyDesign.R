library(tidyverse)
library(haven)
library(labelled)
library(survey)
library(srvyr)

## SOURCE Some files
# Source MDD.R, FoodSecurity.R and MADChildren.R, files Relevant files for the Baseline indicators calculation 
source("code/MDDDataCleaning.R")
source("code/FoodSecurityDataCleaning.R")
source("code/MADChildrenDataCleaning.R")

# Read cover data
SurveyDesignData <- read_dta("new data/cover.dta")


SurveyDesignData <- SurveyDesignData %>%
  select(hhid, clusterid, strataid, GPS__Latitude, GPS__Longitude, GPS__Accuracy, GPS__Altitude, GPS__Timestamp, regiontype, poorscore) 


SvyLCSENData <- LCSEN %>% 
  left_join(SurveyDesignData, by = "hhid") %>% 
  #Set the data to survey designed data
  as_survey(ids = clusterid, 
            strata = strataid, 
            weights = NULL)

# Merge the FIES Data with the Survey Design Data
SvyFIESData <- FIESData %>% 
  left_join(SurveyDesignData, by = "hhid") %>% 
  # Set the data to survey designed data
  as_survey_design(ids = clusterid, 
                   strata = strataid, 
                   weights = NULL)

# Merge the rCSI Data with the Survey Design Data
SvyrCSIData <- rCSIData %>% 
  left_join(SurveyDesignData, by = "hhid") %>% 
  #Set the data to survey designed data
  as_survey_design(ids = clusterid, 
                   strata = strataid, 
                   weights = NULL)


# Merge the MAD Data with the Survey Design Data
SvyMADData <- MADChildren %>% 
  left_join(SurveyDesignData, by = "hhid") %>% 
  #Set the data to survey designed data
  as_survey_design(ids = clusterid, 
                   strata = strataid, 
                   weights = NULL)

# Merge the DietQuality data with the Survey Design Data
SvyDietQualityData <- DietQuality %>% 
  left_join(SurveyDesignData, by = "hhid") %>% 
  #Set the data to survey designed data
  as_survey_design(ids = clusterid, 
                   strata = strataid, 
                   weights = NULL)


# Merge the Livelihoods Coping Strategies FoodSecurity data with the Survey Design Data
SvyLCSFS <- LCSFS %>% 
  left_join(SurveyDesignData, by = "interview__key") %>% 
  #Set the data to survey designed data
  as_survey_design(ids = clusterid, 
                   strata = strataid, 
                   weights = NULL)

## Complete setting the data for survey analysis

############################################################################################################################################################



