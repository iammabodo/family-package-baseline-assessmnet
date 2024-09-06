library(tidyverse)
library(haven)
library(labelled)


## Read in the data
MADChildren <- read_dta("new data/sec6.dta") %>% 
  # Select relevant variables
  dplyr::select(-interview__id) 

# Import the MAD Roster data to get the child's age

MADRoster <- read_dta("new data/sec1A.dta") %>% 
  # Select relevant variables
  dplyr::select(hhid, pid, sec1_4, DOB, DOB_Day, DOB_Month, DOB_Year, Age_Month, sec1_5b)

# Merge the two data sets

MADChildren <- MADChildren %>% 
  left_join(MADRoster, by = c("hhid", "pid")) 

# Clean and tidy the new 

MADChildren <- MADChildren %>% 
  # Convert Age_Month to numeric
  mutate(Age_Month = as.numeric(Age_Month)) %>%
  filter(Age_Month >= 0 & Age_Month <= 23) %>%
  # Rename Variables
  rename(
    ChildId = pid,
    BirthDate = DOB,
    BirthDateDay = DOB_Day,
    BirthDateMonth = DOB_Month,
    BirthDateYear = DOB_Year,
    ChildAgeMonths = Age_Month,
    ChildGender = sex,
    ChildHHRelationship = sec1_4,
    MonthsSinceLastBD = sec1_5b,
    PCMADBreastfeeding = sec6_1, #IYCF 4 
    PCMADBottleLiquid = sec6_2, #IYCF 5
    PCMADPlainWAter = sec6_3a, #IYCF 6A
    PCMADInfFormula = sec6_3b, #IYCF 6B
    PCMADInfFormulaNum = sec6_3c, #IYCF 6Bnum
    PCMADMilk = sec6_3d, #IYCF 6C.25
    PCMADMilkNum = sec6_3e, #IYCF 6Cnum
    PCMADMilkSwt = sec6_3f, #IYCF 6Cswt.26
    PCMADYogurtDrink = sec6_3g, #IYCF 6D
    PCMADYogurtDrinkNum = sec6_3h, #IYCF 6Dnum
    PCMADYogurtDrinkSwt = sec6_3i, #IYCF 6Dswt
    PCMADOtherMilk = sec6_3j, #IYCF 6K
    PCMADOtherMilkSwtFlavoured = sec6_3l, #IYCF 6Kswt
    PCMADChocolateFrappe = sec6_3m, #IYCF 6E
    PCMADCondensedMilk = sec6_3n, 
    PCMADFruiteJuice = sec6_3o, #IYCF 6F.27
    PCMADSoftDrink = sec6_3p, #IYCF 6G.28
    PCMADTea = sec6_3q, #IYCF 6H
    PCMADTeaSwt = sec6_3r, #IYCF 6Hswt.26
    PCMADBroth = sec6_3s, #IYCF 6I
    PCMADAnyOtherLiquids = sec6_3t, #IYCF 6J
    PCMADAnyOtherLiquidsSwt = sec6_3u, #IYCF 6Jswt
    PCMADYogurt = sec6_4a, #IYCF 7.15
    PCMADYogurtNum = sec6_4a2, #IYCF 7.15num
    PCMADStapCereal = sec6_4b, #IYCF 7.1
    PCMADOtherCereal = sec6_4c, #IYCF 7.2
    PCMADStapTubers = sec6_4d, #IYCF 7.3
    PCMADStapLegumes = sec6_4e, #IYCF 7.4
    PCMADVegCarrots = sec6_4f, #IYCF 7.5
    PCMADVegIvyGourd = sec6_4g, #IYCF 7.6.1
    PCMADVegPumpkin = sec6_4h, #IYCF 7.6.2
    PCMADVegEggplant = sec6_4i, #IYCF 7.7.1
    PCMADVegWaxGourd = sec6_4j, #IYCF 7.7.2
    PCMADVegLettuce = sec6_5, #IYCF 7.7.3
    PCMADFruitRipe = sec6_6, #IYCF 7.8
    PCMADFruitOrange = sec6_7, #IYCF 7.9
    PCMADFruitBanana = sec6_8, #IYCF 7.10.1
    PCMADFruitMangosteen = sec6_9, #IYCF 7.10.2
    PCMADSweetsCakes = sec6_10, #IYCF 7.11
    PCMADSweetsCandy = sec6_11, #IYCF 7.12
    PCMADProteinEggs = sec6_12, #IYCF 7.13
    PCMADProteinKidney = sec6_13, #IYCF 7org
    PCMADProteinSausages = sec6_14, #IYCF 7.16
    PCMADProteinBeef = sec6_15, #IYCF 7.17
    PCMADProteinPork = sec6_16, #IYCF 7.18
    PCMADProteinChicken = sec6_17, #IYCF 7.19
    PCMADProteinFish = sec6_18, #IYCF 7.20
    PCMADProteinCrikets = sec6_19, #IYCF 7insect
    PCMADOtherPeanuts = sec6_20, #IYCF 7.21
    PCMADOtherChips = sec6_21, #IYCF 7.22
    PCMADOtherNoodles = sec6_22, #IYCF 7.23
    PCMADOtherFriedChicken = sec6_23, #IYCF 7.24
    PCMADOtherSemiSolid = sec6_24, #IYCF 7R
    PCMADOtherSemiSolidNm = sec6_24b, #IYCF 7Rnm
    PCMADOtherEatOut = sec6_25, #IYCF 7.25
    PCMADCheck = sec6_26, #IYCF 7S
    PCMADNumber = sec6_27) %>% #IYCF 8
  # Change some variables values to missing if the value == -5555
  mutate(
    across(
      c(PCMADInfFormulaNum, PCMADMilkNum, PCMADYogurtDrinkNum, 
        PCMADYogurtNum, PCMADNumber, PCMADMilkSwt, PCMADYogurtDrinkSwt, 
        PCMADOtherMilkSwtFlavoured, PCMADTeaSwt, PCMADAnyOtherLiquidsSwt, PCMADCheck),
      ~ ifelse(.x == -5555, NA, .x)))%>%
  # Create variables for the calculation of relevant variables
  mutate(
    #Exclusive Breastfeeding under 6 months (EBF)
    MADEBF = case_when(
        PCMADBreastfeeding == 1 & (PCMADPlainWAter == 0 & PCMADInfFormula == 0 & PCMADMilk == 0 & 
        PCMADYogurtDrink == 0 & PCMADOtherMilk == 0  & PCMADChocolateFrappe == 0 & PCMADFruiteJuice == 0 &
        PCMADSoftDrink == 0 & PCMADTea == 0 & PCMADBroth == 0 & PCMADAnyOtherLiquids == 0) & PCMADYogurtDrink > 0  &
        PCMADStapCereal:PCMADCheck == 0~ 1,
      TRUE ~ 0),
    # Staples  - Grains, white/pale starchy roots, tubers, and plantains
    PCMADStaples = case_when(
      PCMADStapCereal == 1 | PCMADOtherCereal == 1 | PCMADStapTubers == 1 ~ 1,
      TRUE ~ 0),
    # Legumes - Beans, peas, lentils, nuts and seeds
    PCMADLegumes = case_when(
      PCMADStapLegumes == 1 | PCMADOtherPeanuts == 1 ~ 1,
      TRUE ~ 0),
    # Dairy products - milk, infant formula, yourgut and cheese
    PCMADDairy = case_when(
      PCMADInfFormula == 1 | PCMADMilk == 1 | PCMADYogurtDrink == 1 ~ 1,
      TRUE ~ 0),
    # Flesh foods - meat, fish, poultry, organ meats
    PCMADFleshFoods = case_when(
      # Ask Jyoti about the inclusion of organ meats
      PCMADProteinSausages == 1 | PCMADProteinBeef == 1 | PCMADProteinPork == 1 | PCMADProteinChicken == 1 | 
        PCMADProteinFish == 1 | PCMADProteinKidney == 1 ~ 1,
      TRUE ~ 0),
    # Vitamin A rich fruits and vegetables
    PCMADVitA = case_when(
      PCMADVegCarrots == 1 | PCMADVegIvyGourd == 1 | PCMADVegPumpkin == 1 | PCMADFruitRipe == 1 ~ 1,
      TRUE ~ 0),
    # Other fruits and vegetables
    PCMADOtherFruitsVeg = case_when(
      PCMADFruitOrange == 1 | PCMADFruitBanana == 1 | PCMADFruitMangosteen == 1 | PCMADVegEggplant == 1 | 
        PCMADVegWaxGourd == 1 | PCMADVegLettuce == 1 ~ 1,
      TRUE ~ 0),
    PCMADUnhealthyFds = case_when(
      PCMADSweetsCakes == 1 | PCMADSweetsCandy == 1 |  PCMADOtherChips == 1 | PCMADOtherNoodles == 1 | PCMADOtherFriedChicken == 1| PCMADOtherEatOut == 1 ~ 1,
      TRUE ~ 0)) %>%
  # Create the MDD Score variable
  mutate(
    MDDScore = PCMADBreastfeeding + PCMADStaples + 
      PCMADProteinEggs + PCMADLegumes + 
      PCMADDairy + PCMADFleshFoods + 
      PCMADVitA + PCMADOtherFruitsVeg,
    MDDCat = case_when(
      MDDScore >= 5 ~ 1,
      TRUE ~ 0)) %>%
  # Mutate MMF variables
  mutate(
    PCMADInfFormulaNum = replace_na(PCMADInfFormulaNum, 0),
    PCMADMilkNum = replace_na(PCMADMilkNum, 0),
    PCMADYogurtDrinkNum = replace_na(PCMADYogurtDrinkNum, 0),
    PCMADYogurtNum = replace_na(PCMADYogurtNum, 0),
    PCMADNumber = replace_na(PCMADNumber, 0),
    PCMADNumber = if_else(PCMADNumber < 0, 0, PCMADNumber),
    # How many times does the child drink infant formula, milk, or eat solid, semi-solid or soft foods - For Non Breastfed children
    feeds = if_else(PCMADBreastfeeding == 0, 0, NA_real_),  # Initialize 'feeds' variable to 0 where IYCF_4 == 0
    feeds = if_else(PCMADBreastfeeding == 0 & between(PCMADInfFormulaNum, 1, 7), feeds + PCMADInfFormulaNum, feeds),  # Add the number of times the child drank infant formula
    feeds = if_else(PCMADBreastfeeding == 0 & between(PCMADMilkNum, 1, 7), feeds + PCMADMilkNum, feeds),  # Add the number of times the child drink milk
    feeds = if_else(PCMADBreastfeeding == 0 & between(PCMADNumber, 1, 7), feeds + PCMADNumber, feeds),  # Add the number of times the child ate solid, semi-solid or soft foods
    MMF = case_when(  # Creating 'MMF' based on conditions
      PCMADBreastfeeding == 1 & between(ChildAgeMonths, 6, 8) & between(PCMADNumber, 2, 7) ~ 1,  # Currently breastfeeding and 6-8 months
      PCMADBreastfeeding == 1 & between(ChildAgeMonths, 9, 23) & between(PCMADNumber, 3, 7) ~ 1,  # Currently breastfeeding and 9-23 months
      PCMADBreastfeeding == 0 & between(ChildAgeMonths, 6, 23) & feeds >= 4 & between(PCMADNumber, 1, 7) ~ 1,  # Not breastfeeding and 6-23 months
          TRUE ~ 0  # Default to 0 if none of the conditions are met
        ),
    milkfeeds = if_else(PCMADBreastfeeding == 0, 0, NA_real_),  # Initialize 'milkfeeds' to 0 where IYCF_4 == 0
    milkfeeds = if_else(PCMADBreastfeeding == 0 & between(PCMADInfFormulaNum, 1, 7), milkfeeds + PCMADInfFormulaNum, milkfeeds),  # Add IYCF_6Bnum
    milkfeeds = if_else(PCMADBreastfeeding == 0 & between(PCMADMilkNum , 1, 7), milkfeeds + PCMADMilkNum , milkfeeds),  # Add IYCF_6Cnum
    milkfeeds = if_else(PCMADBreastfeeding == 0 & between(PCMADYogurtDrinkNum, 1, 7), milkfeeds + PCMADYogurtDrinkNum, milkfeeds),  # Add IYCF_7_15num
    milkfeeds = if_else(PCMADBreastfeeding == 0 & between(PCMADYogurtNum, 1, 7), milkfeeds + PCMADYogurtNum, milkfeeds),
    MMFF = case_when(
      milkfeeds >= 2 ~ 1,
      milkfeeds < 2 ~ 0,
    TRUE ~ NA)) %>%
  # generate the MAD variable
  mutate(
    MAD = case_when(
      MDDCat == 1 & MMF == 1 & (PCMADBreastfeeding == 1 | MMFF == 1) ~ 1,
      TRUE ~ 0)) %>%
  # Set Variable Labels
  set_variable_labels(
    ChildId = "Child ID",
    BirthDate = "Child Birth Date",
    BirthDateDay = "Child Birth Day",
    BirthDateMonth = "Child Birth Month",
    BirthDateYear = "Child Birth Year",
    ChildAgeMonths = "Child Age (Months)",
    ChildGender = "Gender of the child",
    ChildHHRelationship = "Relationship of the child to the head of the household",
    MonthsSinceLastBD = "Months since last birthday",
    PCMADBreastfeeding = "Was the child breastfed yesterday during the day or night?",
    PCMADBottleLiquid = "Did the child drink from a bottle with a nipple yesterday during the day or night?",
    PCMADPlainWAter = "Did the child drink plain water yesterday during the day or night?",
    PCMADInfFormula = "Did the child drink infant formula (such as France Bebe, Dumex, Similax or Nutrilatt) yesterday during the day or night?",
    PCMADInfFormulaNum = "How many times did the child drink infant formularyesterday during the day or night?",
    PCMADMilk = "Did the child drink milk such tinned, powdered or fresh animal milk yesterday during the day or night?",
    PCMADMilkNum = "How many times did the child drink milk yesterday during the day or night?",
    PCMADMilkSwt = "Was the milk sweetened or flavored type of milk?",
    PCMADYogurtDrink = "Did the child drink yogurt drinks yesterday during the day or night?",
    PCMADYogurtDrinkNum = "How many times did the child drink yogurt drinks yesterday during the day or night?",
    PCMADYogurtDrinkSwt = "Was the yogurt drink sweetened or flavored?",
    PCMADOtherMilk = "Did the child drink any other type of milk (such as Soymilk, Lactasoy, or green bean milk) yesterday during the day or night?",
    PCMADOtherMilkSwtFlavoured = "Was the other type of milk sweetened or flavored?",
    PCMADChocolateFrappe = "Did the child drink chocolate frappe yesterday during the day or night?",
    PCMADCondensedMilk = "Did the child drink condensed milk yesterday during the day or night?",
    PCMADFruiteJuice = "Did the child drink fruit juice, fruit drink, sugarcane juice or fruite shake, yesterday during the day or night?",
    PCMADSoftDrink = "Did the child drink soft drinks (such as Coca Cola, Pepsi, Fanta, Sprite, Bacchus or M-150) yesterday during the day or night?",
    PCMADTea = "Did the child drink tea, coffee or herbal drinks yesterday during the day or night?",
    PCMADTeaSwt = "Was the tea sweetened?",
    PCMADBroth = "Did the child drink clear broth or clear soup yesterday during the day or night?",
    PCMADAnyOtherLiquids = "Did the child drink any other type of liquid yesterday during the day or night?",
    PCMADAnyOtherLiquidsSwt = "Was the other type of liquid sweetened?",
    PCMADYogurt = "Did the child eat yogurt other than yoghurt drinks yesterday during the day or night?",
    PCMADYogurtNum = "How many times did the child eat yogurt yesterday during the day or night?",
    PCMADStapCereal = "Did the child eat rice, khmer rice pancake, khmer noodles, glass noodles, bread, porridge or bobor krub kroeung yesterday during the day or night?",
    PCMADOtherCereal = "Did the child eat brown rice, corn or popcorn yesterday during the day or night?",
    PCMADStapTubers = "Did the child eat cassava, sweet potato, taro, cassava noodles, sweet potato, green banana, damlong daikla or potato yesterday during the day or night?",
    PCMADStapLegumes = "Did the child eat soybeans, soymilk, peas, pigeon peas, soybeans, red mung beans or mung beans, yesterday during the day or night?",
    PCMADVegCarrots = "Did the child vegetables such as carrots, pumpkin, sweet potato that are yellow or orange inside, yesterday during the day or night?",
    PCMADVegIvyGourd = "Did the child eat ivy gourd leaves, moringa leaves, green amarath, water spinach, bok choy or mustard greens, yesterday during the day or night?",
    PCMADVegPumpkin = "Did the child eat pumpkin leaves , sweat leaf bush, choy sum, spinach, kale or broccoli, yesterday during the day or night?",
    PCMADVegEggplant = "Did the child eat eggplant, cauliflower, long beans, cabbage, bean sprouts, tomatoes, or okra, or okra, yesterday during the day or night?",
    PCMADVegWaxGourd = "Did the child eat wax gourd, sponge gourd, bitter gourd, ridge gourd, bottle gourd, ivy gourd or cucumber, yesterday during the day or night?",
    PCMADVegLettuce = "Did the child eat lettuce, banana flower, mushrooms, bamboo shoots, white radish, green mango or green papaya, yesterday during the day or night?",
    PCMADFruitRipe = "Did the child eat ripe fruits such as mango, papaya, passion fruit, yesterday during the day or night?",
    PCMADFruitOrange = "Did the child eat fruits such as orange, mandarin, grapefruit or pomelo, yesterday during the day or night?",
    PCMADFruitBanana = "Did the child eat fruits such as banana, watermelon, custard apple, pineaple, jackfruit, yesterday during the day or night?",
    PCMADFruitMangosteen = "Did the child eat fruits such as mangosteen, durian, rambutan, longan or langsat, guava, dragon fruit or apple, yesterday during the day or night?",
    PCMADSweetsCakes = "Did the child eat cakes, cookies, donut, coconut sticky rice, sticky rice with coconut and egg, sticky rice with durian, sticky rice layer cake, or sweet stickyrice balls, yesterday during the day or night?",
    PCMADSweetsCandy = "Did the child eat candy, chocolate, ice cream, lot svet, mung bean pudding or coconut jellies, yesterday during the day or night?",
    PCMADProteinEggs = "Did the child eat duck or chicken eggs, yesterday during the day or night?",
    PCMADProteinKidney = "Did the child eat kidney, liver, heart, lung or blood from animal origin, yesterday during the day or night?",
    PCMADProteinSausages = "Did the child eat sausages or ham, yesterday during the day or night?",
    PCMADProteinBeef = "Did the child eat meat from beef, buffalo, lamb or goat, yesterday during the day or night?",
    PCMADProteinPork = "Did the child eat meat from pork, frog, turtle, rat, mice, or wild animal, yesterday during the day or night?",
    PCMADProteinChicken = "Did the child eat meat from chicken, duck, or goose, yesterday during the day or night?",
    PCMADProteinFish = "Did the child eat fish, seafood, eel, small shrimp, canned fish, or fermented fish, yesterday during the day or night?",
    PCMADProteinCrikets = "Did the child eat crickets, bug bacon, cockroaches, snails, spiders, termites or grasshoppers, yesterday during the day or night?",
    PCMADOtherPeanuts = "Did the child eat peanuts, sunflower seeds, watermelon seeds or pumpkin seeds, yesterday during the day or night?",
    PCMADOtherChips = "Did the child eat potato chips or shrimp chips, yesterday during the day or night?",
    PCMADOtherNoodles = "Did the child eat instant noodles, yesterday during the day or night?",
    PCMADOtherFriedChicken = "Did the child eat fried chicken, fried banana, fried sweet potato or French fries, yesterday during the day or night?",
    PCMADOtherSemiSolid = "Did the child eat any other solid, semi-solid or soft food, yesterday during the day or night?",
    PCMADOtherSemiSolidNm = "What was the other solid, semi-solid or soft food that the child ate?",
    PCMADOtherEatOut = "Did the child eat food from any place like Burger King, Pizza Company, Five Star,  restaurant, food stall, or street vendor, yesterday during the day or night?",
    PCMADCheck = "Are you sure the child did not eat any of the foods we discussed so far, yesterday during the day or night?",
    PCMADNumber = "How many times did the child eat solid, semi-solid or soft foods, yesterday during the day or night?",
    PCMADStaples = "Staples",
    PCMADLegumes = "Legumes",
    PCMADDairy = "Dairy",
    PCMADFleshFoods = "Flesh Foods",
    PCMADVitA = "Vitamin A rich fruits and vegetables",
    PCMADOtherFruitsVeg = "Other fruits and vegetables",
    MDDScore = "Minimum Dietary Diversity Score",
    MDDCat = "Minimum Dietary Diversity Category")
  # Change labeled variables to factor variables

# Read in the Household Characteristics data

HHCharacteristics <- read_dta("new data/cover.dta") %>% 
  # Select relevant variables
  select(hhid, HHID, IDPOOR) %>% 
  # Create treatment variable
  mutate(Treatment = case_when(
    IDPOOR == 1 | IDPOOR == 2 ~ "Treatment Group",
    TRUE ~ "Control Group"))
 
# Merge the data with household data

MADChildren <- MADChildren %>% 
  left_join(HHCharacteristics, by = c("hhid", "HHID")) %>% 
  # Mutate child age groups
  mutate(
    ChildAgeGroup = case_when(
      ChildAgeMonths >= 0 & ChildAgeMonths <= 5 ~ "0-5 Months",
      ChildAgeMonths >= 6 & ChildAgeMonths <= 11 ~ "6-11 Months",
      ChildAgeMonths >= 12 & ChildAgeMonths <= 17 ~ "12-17 Months",
      ChildAgeMonths >= 18 & ChildAgeMonths <= 23 ~ "18-23 Months",
      TRUE ~ "Other"))

