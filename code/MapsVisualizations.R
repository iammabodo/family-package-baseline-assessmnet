### Loading relevant library

library(tidyverse)
library(sf)
library(showtext)
library(patchwork)
library(cowplot)

## Selected Provinces: Banteay Meanchey Kampong Cham Kampong Speu Kampot  Kratie Phnom Penh Preah Sihanouk  Siemreap Tboung Khmum  
## Source the Indicator Calculation File

source("code/IndicatorCalculation.R")


## This data can be used to create the map of the country - visualisation of indicators by provinces
## However, with the set up of the impact evaluation, we are only interested in the selected provinces, which makes map visualisations irrelevant
COData <- st_read("data/shape/khm_bnd_admin1_gov_wfp_ed2022.shp") %>% 
  # Selecting the necessary columns
  select(Adm1_Name, geometry) %>%
  # Renaming the columns
  rename(Province = Adm1_Name) #%>% 
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
# # Reduced Coping Strategies Index
# 
# # Merge with the Indicator Data
# RCSIndexMap <- COData %>% 
#     left_join(meanRCSIProvince, by = "Province")
# 
# 
# RCSIndexMap %>% 
#   ggplot() +
#   geom_sf(aes(fill = MeanRCSI), color = "black") +
#   facet_wrap(~Treatment) +
#   scale_fill_gradient(
#     low = "#F7ECD1",
#     high = "#D5B868") +
#   theme_minimal() +
#   labs(
#     title = "Mean Resilience Capacity Score Index by Province",
#     fill = "Mean RCSI"
#   ) +
#   theme(legend.position = "bottom")
# 
# ###########################################################################################################
# 
# # Livelihoods Coping Strategies Essential Needs Index
# 
# LSCENMap <- COData %>% 
#   left_join(SvyLCSENMaxProvince, by = "Province")
# 
# LSCENMap %>%
#   ggplot() +
#   geom_sf(aes(fill = `Stress coping strategies`), color = "black") +
#   geom_text(aes(label = Province, geometry = geometry), 
#             size = 3,
#             stat = "sf_coordinates",
#             color = "white") +
#   facet_wrap(~Treatment) +
#   scale_fill_gradient(
#     low = "#F7ECD1",
#     high = "#D5B868") +
#   theme_minimal() +
#   labs(
#     title = "Max Livelihoods Coping Strategies Essential Needs Index by Province",
#     fill = "Max LCSEN"
#   ) +
#   theme(legend.position = "bottom")
# 
# 
# LSCENMap %>%
#   ggplot() +
#   geom_sf(aes(fill = `Crisis coping strategies`), color = "black") +
#  # # geom_text(aes(label = Province, geometry = geometry), 
#  #            size = 3,
#  #            stat = "sf_coordinates",
#  #            color = "white") +
#   facet_wrap(~Treatment) +
#   scale_fill_gradient(
#     low = "#FBC7B3",
#     high = "#F37847") +
#   theme_minimal() +
#   labs(
#     title = "Max Livelihoods Coping Strategies Essential Needs (Crisis Coping Strategies) Index by Province",
#     fill = "Max LCSEN"
#   ) +
#   theme(legend.position = "bottom")
# 
# 
# LSCENMap %>%
#   #filter(!is.na(`Emergency coping strategies`)) %>%
#   mutate(`Emergency coping strategies` = round(`Emergency coping strategies`, 2)) %>%
#   ggplot() +
#   geom_sf(aes(fill = `Emergency coping strategies`), color = "black") +
#   geom_text(aes(label = `Emergency coping strategies`, geometry = geometry),
#              size = 3,
#              stat = "sf_coordinates",
#              color = "black") +
#   facet_wrap(~Treatment) +
#   scale_fill_gradient(
#     low = "#F4BFBF",
#     high = "#C00000") +
#   #theme_minimal() +
#   labs(
#     title = "Max Livelihoods Coping Strategies Essential Needs (Crisis Coping Strategies) Index by Province",
#     fill = "Max LCSEN"
#   ) +
#   theme(legend.position = "bottom",
#         axis.title = element_blank(),
#         # Turn off axis text
#         axis.text.x = element_blank(),
#        axis.text.y =  element_blank())



## Let us graph the indicators
##Stacked Bar Chart - Livelihood Coping Strategies Food Security Indicator by Province

LCSFSGraph <- ggplot(SvyLCSENFoodProvinceTab, aes(x = Province, y = Pct_LCSENFood, fill = MaxcopingBehaviourFS)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  geom_text(aes(
    label = sprintf("%.1f%%", Pct_LCSENFood),
    color = ifelse(MaxcopingBehaviourFS %in% c("Household not adopting coping strategies", "Stress coping strategies"), "#4B2E2A", "white")
  ), 
  position = position_stack(vjust = 0.5), 
  size = 3.5) +
  scale_fill_manual(values = c(
    'Household not adopting coping strategies' = '#F1ECE8',
    'Stress coping strategies' = '#D5B868',
    'Crisis coping strategies' = '#F37847',
    'Emergency coping strategies' = '#C00000'
  )) +
  scale_color_identity() +  # This ensures the color mapping is applied directly
  labs(
    title = "Majority of Households are adopting coping strategies in order to be able to afford food",
    subtitle = "Although in Kampong Cham and Tboung Khmum, with a higher percentage of households were not adopting coping strategies",
    x = "Province",
    y = "Percentage of Households",
    fill = "Coping Strategy"
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(family = "opensans", colour =  "#4B2E2A", size = 8, face = "bold",
                               margin = margin(t = 0, b = 20)),
    axis.text.y = element_blank(),
    axis.title = element_text(family = "opensans", size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10, l = 10), angle = 90),
    plot.title = element_text(family = "opensans", size = 12, face = "bold", hjust = 0.5, colour = "#C55D2B"),
    plot.subtitle = element_text(family = "opensans", size = 10, face = "italic", hjust = 0.5, colour = "#C55D2B", lineheight = 1.5),
    # Legend styling
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank(),  # Legend title style
    legend.text = element_text(size = 7, family = "opensans", color = "#4B2E2A"),  # Legend text style
    #legend.background = element_rect(fill = "lightgrey", color = NA),  # Background color of the legend
    #legend.key = element_rect(fill = "white", color = "black"),  # Background of the legend keys
    legend.key.size = unit(0.5, "cm"),  # Size of the legend keys
    legend.spacing.x = unit(0.2, "cm"),  # Horizontal spacing between legend items
    #legend.box.background = element_rect(color = "black", size = 0.5),  # Border around the legend box
    legend.box.margin = margin(t = -10, b = 10)  # Margin around the legend box
  )

# Save the graph
ggsave("figures/Livelihoods_Coping_Strategies_by_Province_High_Quality.png", plot = LCSFSGraph)

####################################################################################################################################################

#Stacked Graph - Livelihoods Coping Strategies Essential Needs Indicator by Province
LCSENGraph <- ggplot(SvyLCSENMaxProvince, aes(x = Province, y = Pct_LCSENMax, fill = MaxcopingBehaviourEN)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  geom_text(aes(
    label = sprintf("%.1f%%", Pct_LCSENMax),
    color = ifelse(MaxcopingBehaviourEN %in% c("Household not adopting coping strategies", "Stress coping strategies"), "#4B2E2A", "white")
  ), 
  position = position_stack(vjust = 0.5), 
  size = 3.5) +
  scale_fill_manual(values = c(
    'Household not adopting coping strategies' = '#F1ECE8',
    'Stress coping strategies' = '#D5B868',
    'Crisis coping strategies' = '#F37847',
    'Emergency coping strategies' = '#C00000'
  )) +
  scale_color_identity() +  # This ensures the color mapping is applied directly
  labs(
    title = "Majority of Households are adopting coping strategies in order to meet essential needs",
    subtitle = "Although the situation in Kampong Cham and Tboung Khmum is different",
    x = "Province",
    y = "Percentage of Households",
    fill = "Coping Strategy"
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(family = "opensans", colour =  "#4B2E2A", size = 8, face = "bold",
                               margin = margin(t = 0, b = 20)),
    axis.text.y = element_blank(),
    axis.title = element_text(family = "opensans", size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10, l = 10), angle = 90),
    plot.title = element_text(family = "opensans", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(family = "opensans", size = 10, face = "italic", hjust = 0.5),
    # Legend styling
    # Legend styling
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank(),  # Legend title style
    legend.text = element_text(size = 7, family = "opensans", color = "#4B2E2A"),  # Legend text style
    #legend.background = element_rect(fill = "lightgrey", color = NA),  # Background color of the legend
    #legend.key = element_rect(fill = "white", color = "black"),  # Background of the legend keys
    legend.key.size = unit(0.5, "cm"),  # Size of the legend keys
    legend.spacing.x = unit(0.2, "cm"),  # Horizontal spacing between legend items
    #legend.box.background = element_rect(color = "black", size = 0.5),  # Border around the legend box
    legend.box.margin = margin(t = -10, b = 10))  # Margin around the legend box

## Combined Coping Srategies Indicator Graph
CombinedCopingStrategiesGraph <- LCSFSGraph + LCSENGraph


####################################################################################################################################################


#Minimum Dietary Diversity for Women of Reproductive Age Nutrition Indicators Visualisation
MDDProvincePlotData <- SvyMDDWomenProvince  %>% 
  left_join(COData, by = "Province")


MDDProvincePlotData %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = MDDWomen), color = "black", size = 0.2) +  # Adjust size and color of borders
  #geom_sf_text(aes(label = Province), size = 4, family = "Open Sans", fontface = "bold", color = "black") +  # Add province labels
  scale_fill_viridis_c(option = "A", name = "MDD Women (%)") +  # Choose a color scale
  theme_minimal() +  # Clean theme
  labs(
    title = "Minimum Dietary Diversity for Women (MDDWomen) by Province",
    subtitle = "Data visualization using sf package in R",
    caption = "Source: Family Package Baseline"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )










