### Loading relevant library

library(tidyverse)
library(sf)
library(showtext)
library(patchwork)
library(cowplot)
library(gridExtra)
library(gt)
library(grid)

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
                               margin = margin(t = -5, b = 20)),
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
    legend.box.margin = margin(t = -10, b = 10))    # Margin around the legend box


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

ProvinceFIES <- read_xlsx("report tables/FIES.xlsx")



FIESGraph <- ggplot(ProvinceFIES, aes(x = FIES, y = reorder(Province, FIES))) +  
  geom_bar(stat = "identity", fill = "#8d1f17", width = 0.7) +  # Set a blue fill color similar to the image and adjust bar width
  geom_vline(xintercept = 64.2, color = "black", size = 1) +  # Add a vertical line at x = 50
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0,75),
                     breaks = seq(0, 75, 10),
                     position = "top") +  # Set x-axis limits and breaks
  scale_y_discrete(expand = c(0, 0.5)) +  # Set y-axis limits
  theme(
    panel.background = element_rect(fill = "white"),  # Set panel background color
    panel.grid.major.x = element_line(colour = 'grey', size = 0.1),  # Remove horizontal grid lines
    axis.ticks.length = unit(0, "cm"),  # Remove axis ticks
    axis.title = element_blank(),  # Remove axis title
    axis.line.y.left = element_line(color = "black"),  # Set y-axis line color and size
    axis.text.y = element_blank(),  # Set y-axis text size, font, and color
    axis.text.x = element_blank()  # Set x-axis text size, font, and color
  ) +
  geom_text(aes(0,label = Province), 
            hjust = 0, size = 3.5, 
            family = "opensans", color = "#ffffff",
            nudge_x = 0.3) + # Add data labels
  geom_text(aes(x= 32, label = paste0(FIES, "%")), # Setting the x-axis position
            hjust = 0, size = 3, 
            family = "opensans", color = "#ffffff") +  # Add data labels
  labs(
    title = "Food Insecurity Experience Scale (FIES) by Province (%)",
    subtitle = "Majority of households are food insecure in almost all provinces, except in Banteay Meanchey, where less than 50% of households are food insecure",
    caption = "Data Source: Family Package Baseline"
  ) + 
  theme(plot.title = element_text(size = 12, family = "opensans", face = "bold"),
        plot.subtitle = element_text(size = 10, family = "opensans", face = "italic", lineheight = 1.5,
                                     margin = margin(t = 10, b = 10)),
        plot.caption = element_text(size = 10, hjust = 0, family = "opensans", color = "black"))


## Reduced Coping Strategies Index

ReducedCopingStrategiesGraph <- ggplot(rCSICopingStrategies, aes(x = reorder(strategy, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "#254769", width = 0.6) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = 10, size = 3.5, family = "opensans", color = "#ffffff") +
  #coord_flip() + # Flips the coordinates for better readability
  labs(title = "The most used consumption based was relying on less prefferd food",
       subtitle = "Whenever reducing meal sizes or portions was adopted, women household members are making a big sacrifice compared to men",
       x = "Coping Strategies Adopted",
       y = "Percentage",
       caption = "Data Source: Family Package Baseline") +
  theme_void() +
  theme(
    # Theme to the panel
    panel.background = element_rect(fill = "white", color = "white", size = 0.5),
    panel.grid.major.y = element_line(color = "grey", size = 0.5),
    # Theme the x- axis
    axis.text.x = element_text(lineheight = 1.8, family = "opensans", size = 8, color = "black", face = "bold", margin = margin(t = -10)),
    axis.title.x = element_text(family = "opensans", size = 10, color = "black", face = "bold", margin = margin(b = 10, t = 10)),
    axis.ticks.x  = element_blank(),
    # Theme the titles
    plot.title = element_text(family = "opensans", size = 12, color = "black", face = "bold", margin = margin(b = 10, t = 10, l = 20)),
    plot.subtitle = element_text(family = "opensans", size = 10, color = "black", face = "italic", margin = margin(b = 10, l = 20)),
    plot.caption = element_text(family = "opensans", size = 10, color = "black", margin = margin(t = 10, l = 20, b = 10), hjust = 0))


StrategiesTable <- rCSIMealSizeWhoTable %>% 
  gt() %>%
  tab_header(
    title = "Who is reducing Meal Sizes (%)") %>% 
  fmt_number(
    columns = c(`Male Adults`, `Female Adults`, `Other Options`),
    decimals = 1
  ) %>% 
  tab_options(
    heading.align = "left",
    #table.font.sizes = 10,
    table.align = "center",
    data_row.padding = px(5)) %>% 
  tab_style(
    style = cell_text(
      weight = "bold",
      color = "black",
      align = "center",
      font = "opensans"
    ),
    locations = cells_column_labels(
     everything()))
  


