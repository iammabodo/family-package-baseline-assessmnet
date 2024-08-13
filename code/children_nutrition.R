library(tidyverse)
library(haven)


## Read in the data
children_nutrition <- read_dta("data/7. UNICEF_FPBaseline_Household Roster_V11_FINAL.dta") %>% 
  # Select releventa variables
  select(interview__key, interview__id, HHRoster__id, S1A_3, S1A_4, S1A_5a,
         DOB, DOB_Day, DOB_Month, DOB_Year, DOB_Age, Age_Month, S1A_5b, S1A_5c,
         S1A_6, AGE_Mother,AGE_Father, starts_with("S6_")) %>% 
  # Convert Age_Month to numeric
  mutate(Age_Month = as.numeric(Age_Month)) %>%
  filter(Age_Month >= 0 & Age_Month <= 24) %>%
  filter(S6_1 != -5555) %>%
  mutate(across(c(S6_4a, S6_4a2, S6_4b, S6_4c, S6_4d, S6_4e, S6_4f, S6_4g, S6_4h, S6_4i, 
                  S6_4j, S6_5, S6_6, S6_7, S6_8, S6_9, S6_10, S6_11, S6_12, S6_13, S6_14, 
                  S6_15, S6_16, S6_17, S6_18, S6_19, S6_20, S6_21, S6_22, S6_23, S6_24, 
                  S6_24b, S6_25), as.character)) %>%
  mutate(nofoods = if_else(rowSums(across(c(S6_4a, S6_4a2, S6_4b, S6_4c, S6_4d, S6_4e, S6_4f, S6_4g, S6_4h, S6_4i, 
                                            S6_4j, S6_5, S6_6, S6_7, S6_8, S6_9, S6_10, S6_11, S6_12, S6_13, S6_14, 
                                            S6_15, S6_16, S6_17, S6_18, S6_19, S6_20, S6_21, S6_22, S6_23, S6_24, 
                                            S6_24b, S6_25), ~ . == "No")) == length(c(S6_4a, S6_4a2, S6_4b, S6_4c, S6_4d, S6_4e, S6_4f, S6_4g, S6_4h, S6_4i, 
                                                                                      S6_4j, S6_5, S6_6, S6_7, S6_8, S6_9, S6_10, S6_11, S6_12, S6_13, S6_14, 
                                                                                      S6_15, S6_16, S6_17, S6_18, S6_19, S6_20, S6_21, S6_22, S6_23, S6_24, 
                                                                                      S6_24b, S6_25)), "Yes", "No"))
  

# Mutate No foods eaten variable
  
children_nutrition %>% count(nofoods)


  
nofoods <- children_nutrition %>% 
  filter(if_all(c(S6_4a, S6_4a2, S6_4b, S6_4c, S6_4d, S6_4e, S6_4f, S6_4g, S6_4h, S6_4i, 
                  S6_4j, S6_5, S6_6, S6_7, S6_8, S6_9, S6_10, S6_11, S6_12, S6_13, S6_14, 
                  S6_15, S6_16, S6_17, S6_18, S6_19, S6_20, S6_21, S6_22, S6_23, S6_24, S6_24b, S6_25), 
                ~ . != 1)) %>% 
  select(Age_Month, S6_26, S6_27, S6_4a, S6_4a2, S6_4b, S6_4c, S6_4d, S6_4e, S6_4f, S6_4g, S6_4h, S6_4i, 
         S6_4j, S6_5, S6_6, S6_7, S6_8, S6_9, S6_10, S6_11, S6_12, S6_13, S6_14, 
         S6_15, S6_16, S6_17, S6_18, S6_19, S6_20, S6_21, S6_22, S6_23, S6_24, S6_24b, S6_25)

nofoods %>% 
  summarise(meanage = mean(Age_Month, na.rm = TRUE),
            media = median(Age_Month, na.rm = TRUE),
            n = n())
