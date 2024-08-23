### Loading relevant library

library(tidyverse)
library(sf)

## Selected Provinces: Banteay Meanchey Kampong Cham Kampong Speu Kampot  Kratie Phnom Penh Preah Sihanouk  Siemreap Tboung Khmum  
## Source the Indicator Calculation File

source("code/IndicatorCalculation.R")

COData <- st_read("data/shape/khm_bnd_admin1_gov_wfp_ed2022.shp") %>% 
  # Selecting the necessary columns
  select(Adm1_Name, geometry) %>%
  # Renaming the columns
  rename(Province = Adm1_Name) %>% 
  # Filtering the necessary provinces
  filter(
    Province == "Banteay Meanchey" | 
      Province == "Kampong Cham" | 
      Province == "Kampong Speu" | 
      Province == "Kampot" | 
      Province == "Kratie" | 
      Province == "Phnom Penh" | 
      Province == "Preah Sihanouk" | 
      Province == "Siemreap" | 
      Province == "Tboung Khmum") 

###########################################################################################################
# Reduced Coping Strategies Index

# Merge with the Indicator Data
RCSIndexMap <- COData %>% 
    left_join(meanRCSIProvince, by = "Province")


RCSIndexMap %>% 
  ggplot() +
  geom_sf(aes(fill = MeanRCSI), color = "black") +
  facet_wrap(~Treatment) +
  scale_fill_gradient(
    low = "#F7ECD1",
    high = "#D5B868") +
  theme_minimal() +
  labs(
    title = "Mean Resilience Capacity Score Index by Province",
    fill = "Mean RCSI"
  ) +
  theme(legend.position = "bottom")

###########################################################################################################

# Livelihoods Coping Strategies Essential Needs Index

LSCENMap <- COData %>% 
  left_join(SvyLCSENMaxProvince, by = "Province")

LSCENMap %>%
  ggplot() +
  geom_sf(aes(fill = `Stress coping strategies`), color = "black") +
  geom_text(aes(label = Province, geometry = geometry), 
            size = 3,
            stat = "sf_coordinates",
            color = "white") +
  facet_wrap(~Treatment) +
  scale_fill_gradient(
    low = "#F7ECD1",
    high = "#D5B868") +
  theme_minimal() +
  labs(
    title = "Max Livelihoods Coping Strategies Essential Needs Index by Province",
    fill = "Max LCSEN"
  ) +
  theme(legend.position = "bottom")


LSCENMap %>%
  ggplot() +
  geom_sf(aes(fill = `Crisis coping strategies`), color = "black") +
 # # geom_text(aes(label = Province, geometry = geometry), 
 #            size = 3,
 #            stat = "sf_coordinates",
 #            color = "white") +
  facet_wrap(~Treatment) +
  scale_fill_gradient(
    low = "#FBC7B3",
    high = "#F37847") +
  theme_minimal() +
  labs(
    title = "Max Livelihoods Coping Strategies Essential Needs (Crisis Coping Strategies) Index by Province",
    fill = "Max LCSEN"
  ) +
  theme(legend.position = "bottom")


LSCENMap %>%
  #filter(!is.na(`Emergency coping strategies`)) %>%
  ggplot() +
  geom_sf(aes(fill = `Emergency coping strategies`), color = "black") +
  # # geom_text(aes(label = Province, geometry = geometry), 
  #            size = 3,
  #            stat = "sf_coordinates",
  #            color = "white") +
  facet_wrap(~Treatment) +
  scale_fill_gradient(
    low = "#F4BFBF",
    high = "#C00000") +
  theme_minimal() +
  labs(
    title = "Max Livelihoods Coping Strategies Essential Needs (Crisis Coping Strategies) Index by Province",
    fill = "Max LCSEN"
  ) +
  theme(legend.position = "bottom")






