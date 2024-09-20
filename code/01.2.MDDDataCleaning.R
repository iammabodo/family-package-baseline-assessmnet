## Relevant Libraries


library(tidyverse)
library(janitor)
library(readxl)
library(haven)


## Data Import
DietQuality <- read_dta("new data/sec7.dta") %>% 
  # Selecting relevant columns
 dplyr::select(-"interview__id") %>%
  # Rename variables
  rename(
    MDDId = PID7_Resp,
    MDDStapCereal = sec7_1, # DQQ1
    MDDStapOther = sec7_2, # DQQ2
    MDDStapRoots = sec7_3, # DQQ3
    MDDStapPulse = sec7_4, # DQQ4
    MDDVegVitamin = sec7_5, # DQQ5
    MDDVegGreens = sec7_6, # DQQ6.1
    MDDVegPumpkin = sec7_7, # DQQ6.2
    MDDVegEggplant = sec7_8, # DQQ7.1
    MDDVegWaxGourd = sec7_9, # DQQ7.2
    MDDVegLettuce = sec7_10, # DQQ7.3
    MDDFruitRipe = sec7_11, # DQQ8
    MDDFruitOrange = sec7_12, # DQQ9
    MDDFruitBanana = sec7_13, # DQQ10.1
    MDDFruitMangosteen = sec7_14, # DQQ10.2
    MDDSweetsCake = sec7_15, # DQQ11
    MDDSweetsCandy = sec7_16, # DQQ12
    MDDProteinEgg = sec7_17, # DQQ13
    MDDProteinYogurt = sec7_18, # DQQ15
    MDDProteinProcessed = sec7_19, # DQQ16
    MDDProteinBeef = sec7_20, # DQQ17
    MDDProteinPork = sec7_21, # DQQ18
    MDDProteinChicken = sec7_22, # DQQ19
    MDDProteinFish = sec7_23, # DQQ20
    MDDOtherPeanuts = sec7_24, # DQQ21
    MDDOtherChips = sec7_25, # DQQ22
    MDDOtherNoodles = sec7_26, # DQQ23
    MDDOtherFriedFoods = sec7_27, # DQQ24
    MDDDrinkMilk = sec7_28, # DQQ25
    MDDDrinkTea = sec7_29, # DQQ26
    MDDDrinksFruitJuice = sec7_30, # DQQ27
    MDDDrinksSoftDrinks = sec7_31, # DQQ28
    MDDEatOut = sec7_32) %>% # DQQ29
  # Change variables to factor variables
  #mutate_at(vars(MDDStapCereal:MDDEatOut), as_factor) %>%
  mutate_at(vars(Province:Village, IDPOOR), as_factor) %>%
  #Mutate the food groups variables for the calculation of MDD
  mutate(MDDStaples = case_when(
    MDDStapCereal == 1 | MDDStapOther == 1 | MDDStapRoots == 1  ~ 1,
    TRUE ~ 0), # Checked - correct
    MDDPulses = case_when(
      MDDStapPulse == 1 ~ 1,
      TRUE ~ 0), # Checked - correct
    MDDNutsSeeds = case_when(
      MDDOtherPeanuts == 1 ~ 1,
      TRUE ~ 0), # Checked - correct
    MDDDiary = case_when(
      MDDProteinYogurt == 1 | MDDDrinkMilk == 1 ~ 1,
      TRUE ~ 0), # Checked - correct
    MDDProtein = case_when(
      MDDProteinProcessed  == 1 | MDDProteinBeef == 1 | MDDProteinPork == 1 | MDDProteinChicken == 1 | MDDProteinFish == 1 ~ 1,
      TRUE ~ 0), # Checked - correct
    MDDEggs = case_when(
      MDDProteinEgg == 1 ~ 1,
      TRUE ~ 0), # Checked - correct
    MDDDarkGreenVeg = case_when(
      MDDVegGreens == 1 | MDDVegPumpkin == 1  ~ 1,
      TRUE ~ 0), # Checked - correct
    MDDOtherVitA = case_when(
      MDDVegVitamin == 1 | MDDFruitRipe == 1  ~ 1,
      TRUE ~ 0), # Checked - correct
    MDDOtherVeg = case_when(
      MDDVegEggplant == 1 | MDDVegWaxGourd == 1 | MDDVegLettuce == 1  ~ 1,
      TRUE ~ 0), # Checked - correct
    MDDOtherFruits = case_when(
      MDDFruitOrange == 1 | MDDFruitBanana == 1 | MDDFruitMangosteen == 1  ~ 1,
      TRUE ~ 0), # Checked - correct
    NCDFruits = case_when(
      MDDFruitBanana == 1 | MDDFruitMangosteen == 1  ~ 1,
      TRUE ~ 0),
    NCDRiskUnprocemeat = case_when(
      MDDProteinBeef == 1 | MDDProteinPork == 1 ~ 1,
      TRUE ~ 0),
    NCDRiskOtherFastFds = case_when(
      MDDEatOut == 1 | MDDOtherNoodles == 1  ~ 1,
      TRUE ~ 0),
    MDDAllFruits = case_when(
      MDDVegVitamin == 1 | MDDFruitOrange == 1 | MDDFruitBanana == 1 | MDDFruitMangosteen == 1  ~ 1,
      TRUE ~ 0),
    MDDAllVegetables = case_when(
      MDDVegVitamin == 1 | MDDVegGreens == 1 | MDDVegPumpkin == 1 | MDDVegEggplant == 1 | MDDVegWaxGourd == 1 | MDDVegLettuce == 1  ~ 1,
      TRUE ~ 0),
    MDDAllPulses = case_when(
      MDDStapPulse == 1 | MDDOtherPeanuts == 1  ~ 1,
      TRUE ~ 0),
    MDDAllProteins = case_when(
      MDDProteinEgg == 1 | MDDProteinYogurt == 1 | MDDProteinProcessed == 1 | MDDProteinBeef == 1 | MDDProteinPork == 1 | MDDProteinChicken == 1 | MDDProteinFish == 1 | MDDDrinkMilk == 1 ~ 1,
      TRUE ~ 0),
    MDDSweetBeverages = case_when(
      MDDDrinkTea == 1 | MDDDrinksFruitJuice == 1 | MDDDrinksSoftDrinks == 1  ~ 1,
      TRUE ~ 0),
    MDDUnhealthyFoods = case_when(
      MDDSweetsCake == 1 | MDDSweetsCandy == 1 | MDDOtherChips == 1 | MDDOtherFriedFoods == 1 | MDDOtherNoodles == 1 ~ 1,
      TRUE ~ 0)) %>%
  # Convert food groups to numeric
  #mutate(across(MDDStapCereal:MDDAllProteins, as.double)) %>%
  # Create MDDScore variable
  rowwise() %>%
  mutate(MDDScore = sum((across(MDDStaples:MDDOtherFruits))), # Minimum Dietary Diversity Score
         # Consumption of all five food groups score
         MDDAllGroupsScore = sum(across(c(MDDStaples,MDDAllFruits:MDDAllProteins))),
         # Non communicable diseases (NCD) protective score
         NCDProtScore = sum(across(c(MDDStapOther, MDDStapPulse, MDDOtherPeanuts,
                                     MDDVegVitamin, MDDDarkGreenVeg, MDDOtherVeg, 
                                     MDDFruitRipe, MDDFruitOrange, NCDFruits))), # Checked - correct
         # Non communicable diseases (NCD) risk score
         NCDRiskScore = (MDDDrinksSoftDrinks + MDDSweetsCake + MDDSweetsCandy + 
                                     (MDDProteinProcessed * 2) + MDDOtherFriedFoods + 
                           NCDRiskOtherFastFds + MDDOtherChips + NCDRiskUnprocemeat),
         # Global Dietary Recommendations (GDR) Score
         GDRScore = NCDProtScore - NCDRiskScore + 9) %>% 
  ungroup() %>%
  # Mutate MDDCategory and MDDAllGroupscat variables
  mutate(MDDCategory = case_when(
    MDDScore >= 5 ~ 1,
    TRUE ~ 0),
    MDDAllGroupsCat = case_when(
      MDDAllGroupsScore == 5 ~ 1,
      TRUE ~ 0)) %>%
  # Mutate the family package treatment group variable
  mutate(Treatment = case_when(
    IDPOOR == "POOR_1" | IDPOOR == "POOR_2" ~ "Treatment Group",
    TRUE ~ "Control Group")) %>%
  # Change Treatment to factor variable
  mutate(Treatment = as_factor(Treatment),
         MDDCategory = as_factor(MDDCategory)) %>%
  # Set Variable Labels
  set_variable_labels(
    MDDId = "Diet Quality Respondent ID",
    MDDStapCereal = "Yesterday, did you eat Rice, Khmer rice pancake, Khmer noodles, glass noodles, bread, or porridge?",
    MDDStapOther = "Yesterday, did you eat Brown rice, corn, or popcorn?",
    MDDStapRoots = "Yesterday, did you eat potato, sweet potato, cassava, cassava noodles, taro, damlong daikla, or green banana?",
    MDDStapPulse = "Yesterday, did you eat soybeans, soymilk, peas, pigeon peas, red mung beans, or mung beans?Staple Pulse",
    MDDVegVitamin = "Yesterday, did you eat carrots, pumpkin, or sweet potatoes that are orange inside?",
    MDDVegGreens = "Yesterday, did you eat ivy gourd leaves, moringa leaves, green amaranth, water spinach, bok choy, or mustard greens?",
    MDDVegPumpkin = "Uesterday, did you eat pumpkin leaves, sweet leaf bush, choy sum, spinach, kale, or broccoli?",
    MDDVegEggplant = "Yesterday, did you eat eggplant, cauliflower, long beans, cabbage, bean sprouts, tomatoes, or okra?",
    MDDVegWaxGourd = "Yesterday, did you eat wax gourd, sponge gourd, bitter gourd, ridge gourd, bottle gourd, ivy gourd, or cucumber?",
    MDDVegLettuce = "Yesterday, did you eat lettuce, banana flower, mushrooms, bamboo shoots, white radish, green mango, or green papaya?",
    MDDFruitRipe = "Yesterday, did you eat Ripe mango, ripe papaya, or passion fruit?",
    MDDFruitOrange = "Yesterday, did you eat orange, mandarin, grapefruit, or pomelo?",
    MDDFruitBanana = "Yesterday, did you eat banana, watermelon, custard apple, pineapple, jackfruit, star fruit, or avocado?",
    MDDFruitMangosteen = "Yesterday, did you eat mangosteen, durian, rambutan, longan or langsat, guava, dragon fruit, or apple?",
    MDDSweetsCake = "Yesterday, did you eat cakes, donut, cookies, coconut sticky rice, sticky rice with coconut and egg, sticky rice with durian, sticky rice layer cake, or sweet sticky rice balls?",
    MDDSweetsCandy = "Yesterday, did you eat candy, chocolates, ice cream, lot svet, mung bean pudding, or coconut jellies?",
    MDDProteinEgg = "Yesterday, did you eat duck eggs or chicken eggs?",
    MDDProteinYogurt = "Yesterday, did you eat yogurt",
    MDDProteinProcessed = "Yesterday, did you eat sausages or ham?",
    MDDProteinBeef = "Yesterday, did you eat meat from beef, buffalo, lamb, or goat?Beef",
    MDDProteinPork = "Yesterday, did you eat meat from pork, frog, turtle, rat, mice, or wild animal?",
    MDDProteinChicken = "Yesterday, did you eat meat from chicken, duck, or goose?",
    MDDProteinFish = "Yesterday did you eat meat from fish, seafood, eel, small shrimp, canned fish, or fermented fish?",
    MDDOtherPeanuts = "Yesterday, did you eat peanuts, sunflower seeds, pumpkin seeds, or watermelon seeds?",
    MDDOtherChips = "Yesterday, did you eat potato chips or shrimp chips?",
    MDDOtherNoodles = "Yesterday, did you eat instant noodles",
    MDDOtherFriedFoods = "Fried Foods",
    MDDDrinkMilk = "Yesterday did you have fresh milk, UHT milk, or powdered milk?",
    MDDDrinkTea = "Yesterday, did you have sweetened tea, sweetened coffee, coffee frappe, chocolate frappe, or green tea frappe?",
    MDDDrinksFruitJuice = "Yesterday, did you have fruit juice, fruit drinks, sugarcane juice, or fruit shake?",
    MDDDrinksSoftDrinks = "Yesterday, did you have soft drinks such as Coca-Cola, Fanta, Sprite, Bacchus, or M-150?",
    MDDEatOut = "Yesterday, did you get food from any place like Burger King, KFC, Pizza Company, Five Star, Lucky Burger, or other places that serve burgers, fried chicken or pizza?",
    # 10 Food Group Variables for MDD Calculation
    MDDStaples = "Grains, white roots and tubers, and plantains",
    MDDPulses = "Pulses (beans, peas and lentils)",
    MDDNutsSeeds = "Nuts and seeds",
    MDDDiary = "Dairy",
    MDDProtein = "Meat, poultry, and fish",
    MDDEggs = "Eggs (Duck or Chicken)",
    MDDDarkGreenVeg = "Dark green leafy vegetables",
    MDDOtherVitA = "Other vitamin A-rich fruits and vegetables",
    MDDOtherVeg = "Other vegetables",
    MDDOtherFruits = "Other fruits",
    MDDScore = "Minimum Dietary Diversity Score",
    Treatment = "Family Package Treatment Group",
    MDDCategory = "Minimum Dietary Diversity Category",
    # All Food Groups Consumed
    MDDAllGroupsScore = "All Five Food Groups Consumed (Score)",
    MDDAllGroupsCat = "All Five Food Groups Consumed (Category)",
    MDDAllFruits = "All Fruits Consumed",
    MDDAllVegetables = "All Vegetables Consumed",
    MDDAllPulses = "All Pulses Consumed",
    MDDAllProteins = "All Proteins Consumed",
    # Non Communicable Diseases (NCD) Protective Score
    NCDProtScore = "NCD Protective Score",
    # Non Communicable Diseases (NCD) Risk Score
    NCDRiskScore = "NCD Risk Score",
    # Global Dietary Recommendations (GDR) Score
    GDRScore = "Global Dietary Recommendations (GDR) Score")

# Laod the Roster Data to look at the person responding to section 7
MDDHHRoster <- read_dta("new data/sec1A.dta") %>%
  dplyr::select(hhid, pid, sex, sec1_4, sec1_5a, sec1_6) %>%
  rename(
    MDDId = pid,
    MDDGender = sex,
    MDDRespRel = sec1_4,
    MDDAge = sec1_5a,
    MDDMarital = sec1_6) %>% 
  mutate(MDDGender = as_factor(MDDGender),
         MDDRespRel = as_factor(MDDRespRel),
         MDDMarital = as_factor(MDDMarital))

# Merge the two datasets
DietQuality <- left_join(DietQuality, MDDHHRoster, by = c("MDDId", "hhid"))



## Data Export in sav, stata, and csv formats
#write_sav(DietQuality, "data/DietQuality.sav")
write_dta(DietQuality, "data/DietQuality.dta")  
write_csv(DietQuality, "data/DietQuality.csv")  





