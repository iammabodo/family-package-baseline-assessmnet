## Relevant Libraries


library(tidyverse)
library(janitor)
library(readxl)
library(haven)


## Data Import
DietQuality <- read_dta("data/1. UNICEF_FPBaseline_Main_V26_FINAL.dta") %>% 
  # Selecting relevant columns
  select(interview__key, interview__id, Province, District, Commune, Village, HHID, IDPOOR, equitycardno, householduuid, OTHER_Sample, Sex,
         Count_HH_Disability, PID7_Resp,  S7_1:S7_32) %>%
  # Rename variables
  rename(
    HHDisabNm = Count_HH_Disability,
    MDDId = PID7_Resp,
    MDDStapCereal = S7_1,
    MDDStapOther = S7_2,
    MDDStapRoots = S7_3,
    MDDStapPulse = S7_4,
    MDDVegVitamin = S7_5,
    MDDVegGreens = S7_6,
    MDDVegPumpkin = S7_7,
    MDDVegEggplant = S7_8,
    MDDVegWaxGourd = S7_9,
    MDDVegLettuce = S7_10,
    MDDFruitRipe = S7_11,
    MDDFruitOrange = S7_12,
    MDDFruitBanana = S7_13,
    MDDFruitMangosteen = S7_14,
    MDDSweetsCake = S7_15,
    MDDSweetsCandy = S7_16,
    MDDProteinEgg = S7_17,
    MDDProteinYogurt = S7_18,
    MDDProteinProcessed = S7_19,
    MDDProteinBeef = S7_20,
    MDDProteinPork = S7_21,
    MDDProteinChicken = S7_22,
    MDDProteinFish = S7_23,
    MDDOtherPeanuts = S7_24,
    MDDOtherChips = S7_25,
    MDDOtherNoodles = S7_26,
    MDDOtherFriedFoods = S7_27,
    MDDDrinkMilk = S7_28,
    MDDDrinkTea = S7_29,
    MDDDrinksFruitJuice = S7_30,
    MDDDrinksSoftDrinks = S7_31,
    MDDEatOut = S7_32) %>% 
  # Change variables to factor variables
  mutate_at(vars(MDDStapCereal:MDDEatOut), as_factor) %>%
  mutate_at(vars(Province:Village, IDPOOR, Sex ), as_factor) %>%
  #Mutate the food groups variables
  mutate(MDDStaples = case_when(
    MDDStapCereal == "Yes" | MDDStapOther == "Yes" | MDDStapRoots == "Yes"  ~ 1,
    TRUE ~ 0),
    MDDPulses = case_when(
      MDDStapPulse == "Yes" ~ 1,
      TRUE ~ 0),
    MDDNutsSeeds = case_when(
      MDDOtherPeanuts == "Yes" ~ 1,
      TRUE ~ 0),
    MDDDiary = case_when(
      MDDProteinYogurt == "Yes" | MDDDrinkMilk == "Yes" ~ 1,
      TRUE ~ 0),
    MDDProtein = case_when(
      MDDProteinProcessed  == "Yes" | MDDProteinBeef == "Yes" | MDDProteinPork == "Yes" | MDDProteinChicken == "Yes" | MDDProteinFish == "Yes" ~ 1,
      TRUE ~ 0),
    MDDEggs = case_when(
      MDDProteinEgg == "Yes" ~ 1,
      TRUE ~ 0),
    MDDDarkGreenVeg = case_when(
      MDDVegGreens == "Yes" | MDDVegPumpkin == "Yes"  ~ 1,
      TRUE ~ 0),
    MDDOtherVitA = case_when(
      MDDVegVitamin == "Yes" | MDDFruitRipe == "Yes"  ~ 1,
      TRUE ~ 0),
    MDDOtherVeg = case_when(
      MDDVegEggplant == "Yes" | MDDVegWaxGourd == "Yes" | MDDVegLettuce == "Yes"  ~ 1,
      TRUE ~ 0),
    MDDOtherFruits = case_when(
      MDDFruitOrange == "Yes" | MDDFruitBanana == "Yes" | MDDFruitMangosteen == "Yes"  ~ 1,
      TRUE ~ 0)) %>% 
  # Convert food groups to numeric
  mutate(across(MDDStaples:MDDOtherFruits, as.double)) %>%
  # Create MDDScore variable
  rowwise() %>%
  mutate(MDDScore = sum((across(MDDStaples:MDDOtherFruits)))) %>% 
  ungroup() %>%
  # Mutate MDDCategory variable
  mutate(MDDCategory = case_when(
    MDDScore >= 5 ~ "Minimum Dietary Diversity Met",
    TRUE ~ "Minimum Dietary Diversity Not Met")) %>%
  # Mutate the family package treatment group variable
  mutate(Treatment = case_when(
    IDPOOR == "POOR_1" | IDPOOR == "POOR_2" ~ "Treatment Group",
    TRUE ~ "Control Group")) %>%
  # Change Treatment to factor variable
  mutate(Treatment = as_factor(Treatment),
         MDDCategory = as_factor(MDDCategory)) %>%
  # Set Variable Labels
  set_variable_labels(
    HHDisabNm = "Number of Household Members with Disability",
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
    # Food Group Variables
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
    MDDCategory = "Minimum Dietary Diversity Category")

## Data Export in sav, stata, and csv formats
write_sav(DietQuality, "data/DietQuality.sav")
write_dta(DietQuality, "data/DietQuality.dta")  
write_csv(DietQuality, "data/DietQuality.csv")  

## Indicators Calculation

# 1. Calculate the percentage of respondents who met the minimum dietary diversity, by treatment group
DietQuality %>%
  group_by(Treatment, Province) %>%
  summarise(MDDMet = sum(MDDCategory == "Minimum Dietary Diversity Met"),
            MDDNotMet = sum(MDDCategory == "Minimum Dietary Diversity Not Met"),
            Total = n(),
            MDDMetPerc = (MDDMet/Total)*100,
            MDNotMEPerc = (MDDNotMet/ Total) * 100) %>%
  ungroup()

# 2. All five food groups consumed by the respondent
  
  
  
  
  
  










