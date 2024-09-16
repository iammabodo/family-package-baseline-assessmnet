### Loading relevant library for data visualisation

library(tidyverse)
library(sf)
library(showtext)
library(patchwork)
library(cowplot)
library(gridExtra)
library(gt)
library(grid)
library(png)
library(marquee)
library(colorspace)
library(ggthemes)



## Selected Provinces: Banteay Meanchey Kampong Cham Kampong Speu Kampot  Kratie Phnom Penh Preah Sihanouk  Siemreap Tboung Khmum  
## Source the Indicator Calculation File

source("code/02.1.FoodSecurityIndicators.R")
source("code/02.2.AdultNutritionIndicators.R")
source("code/02.3.ChildNutritionIndicators.R")

#Text for the graphs - setting the defalts
font_add_google("Open Sans","opensans")
showtext_auto()
showtext_opts(dpi = 200)


## This data can be used to create the map of the country - visualisation of indicators by provinces
## However, with the set up of the impact evaluation, we are only interested in the selected provinces, which makes map visualisations irrelevant
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


###########################################################################################################
## Let us graph the indicators
##Stacked Bar Chart - Livelihood Coping Strategies Food Security Indicator by Province

LCSFSGraph <- ggplot(SvyLCSENFoodProvinceTab, aes(x = Province, y = Pct_LCSENFood, fill = MaxcopingBehaviourFS)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  geom_text(aes(
    label = sprintf("%.1f%%", Pct_LCSENFood),
    color = ifelse(MaxcopingBehaviourFS %in% c("Household not adopting coping strategies", "Stress coping strategies"), "#4B2E2A", "white"),
    family = "opensans"
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
    title = "Livelihoods Coping Strategies - Food Security (%)",
    subtitle = "Percentage of households adopting coping strategies for food security",
    caption = "Data Source: Family Package Baseline",
    x = "Province",
    y = "Percentage of Households",
    fill = "Coping Strategy"
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(family = "opensans", colour =  "#4B2E2A", size = 8, face = "bold",
                               margin = margin(t = 0, b = 10)),
    axis.text.y = element_blank(),
    axis.title = element_text(family = "opensans", size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10, l = 10), angle = 90),
    plot.title = element_text(family = "opensans", size = 12, face = "bold", hjust = 0, colour = "#C55D2B",
                              margin = margin(t = 10, b = 5)),
    plot.subtitle = element_text(family = "opensans", size = 10, face = "italic", colour = "#C55D2B", lineheight = 1.5,
                                 margin = margin(t = 0, l = 0, b = 5)),
    plot.caption = element_text(family = "opensans", size = 10, hjust = 0, color = "black",
                                margin = margin(t = 0, l = 0, b = 5)),
    # Legend styling
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank(),  # Legend title style
    legend.text = element_text(size = 8, family = "opensans", color = "#4B2E2A"),  # Legend text style
    #legend.background = element_rect(fill = "lightgrey", color = NA),  # Background color of the legend
    #legend.key = element_rect(fill = "white", color = "black"),  # Background of the legend keys
    legend.key.size = unit(0.2, "cm"),  # Size of the legend keys
    legend.spacing.x = unit(0.2, "cm"),  # Horizontal spacing between legend items
    #legend.box.background = element_rect(color = "black", size = 0.5),  # Border around the legend box
    legend.box.margin = margin(t = -5, b = 10)  # Margin around the legend box
  )

# Save the graph
ggsave("figures/Livelihoods_Coping_Strategies_by_Province_High_Quality.png", 
       plot = LCSFSGraph, width = 6.28, height = 3.98, units = "in",
       bg = "white", dpi = 300)



####################################################################################################################################################

#Stacked Graph - Livelihoods Coping Strategies Essential Needs Indicator by Province
LCSENGraph <- ggplot(SvyLCSENMaxProvince, aes(x = Province, y = Pct_LCSENMax, fill = MaxcopingBehaviourEN)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  geom_text(aes(
    label = sprintf("%.1f%%", Pct_LCSENMax),
    color = ifelse(MaxcopingBehaviourEN %in% c("Household not adopting coping strategies", "Stress coping strategies"), "#4B2E2A", "white"),
    family = "opensans"
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
    title = "Livelihoods Coping Strategies - Essential Needs (%)",
    subtitle = "Percentage of households adopting coping strategies for essential needs",
    caption = "Data Source: Family Package Baseline",
    x = "Province",
    y = "Percentage of Households",
    fill = "Coping Strategy"
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(family = "opensans", colour =  "#4B2E2A", size = 8, face = "bold",
                               margin = margin(t = 0, b = 10)),
    axis.text.y = element_blank(),
    axis.title = element_text(family = "opensans", size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10, l = 10), angle = 90),
    plot.title = element_text(family = "opensans", size = 12, face = "bold", hjust = 0, colour = "#C55D2B",
                              margin = margin(t = 10, b = 5)),
    plot.subtitle = element_text(family = "opensans", size = 10, face = "italic", colour = "#C55D2B", lineheight = 1.5,
                                 margin = margin(t = 0, l = 0, b = 5)),
    plot.caption = element_text(family = "opensans", size = 10, hjust = 0, color = "black",
                                margin = margin(t = 0, l = 0, b = 5)),
    # Legend styling
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank(),  # Legend title style
    legend.text = element_text(size = 8, family = "opensans", color = "#4B2E2A"),  # Legend text style
    #legend.background = element_rect(fill = "lightgrey", color = NA),  # Background color of the legend
    #legend.key = element_rect(fill = "white", color = "black"),  # Background of the legend keys
    legend.key.size = unit(0.2, "cm"),  # Size of the legend keys
    legend.spacing.x = unit(0.2, "cm"),  # Horizontal spacing between legend items
    #legend.box.background = element_rect(color = "black", size = 0.5),  # Border around the legend box
    legend.box.margin = margin(t = -5, b = 10))   # Margin around the legend box


ggsave("figures/Livelihoods_Coping_Strategies_EN_by_Province.png", 
       plot = LCSENGraph, width = 6.28, height = 3.98, units = "in",
       bg = "white", dpi = 300)
    
    # Legend

#################################################################################################################################

# # Food Insecurity Experience Scale by Province
ProvinceFIES <- read_xlsx("report tables/FIES.xlsx")


md_text <- "Rural households are more food insecure than urban households (61% vs 52%)"

FIESGraph <- ggplot(ProvinceFIES, aes(x = FIES, y = reorder(Province, FIES))) +  
  geom_bar(stat = "identity", fill = "#8d1f17", width = 0.7) +  # Set a blue fill color similar to the image and adjust bar width
  #geom_vline(xintercept = 64.2, color = "black", size = 1) +  # Add a vertical line at x = 50
  # annotate(
  #   'marquee',
  #   x = 55, 
  #   y = 10,
  #   label = md_text,
  #   width = 0.5,
  #   hjust = 0.5,
  #   vjust = 0.5,
  #   fill = colorspace::lighten("dodgerblue1", 0.7),
  #   style = text_box_style
  # ) +
  # labs(title = md_text) +  # Add a title)
  # scale_x_continuous(expand = c(0, 0),
  #                    limits = c(0,75),
  #                    breaks = seq(0, 75, 10),
  #                    position = "top") +  # Set x-axis limits and breaks
  # scale_y_discrete(expand = c(0, 0.5)) +  # Set y-axis limits
  theme(
    panel.background = element_rect(fill = "white"),  # Set panel background color
    #panel.grid.major.x = element_line(colour = 'grey', size = 0.1),  # Remove horizontal grid lines
    axis.ticks.length = unit(0, "cm"),  # Remove axis ticks
    axis.title = element_blank(),  # Remove axis title
    axis.line.y.left = element_line(color = "black"),  # Set y-axis line color and size
    axis.text.y = element_blank(),  # Set y-axis text size, font, and color
    axis.text.x = element_blank()  # Set x-axis text size, font, and color
  ) +
  geom_text(aes(0,label = Province),
            hjust = 0, size = 2,
            family = "opensans", color = "#ffffff",
            nudge_x = 0.3) + # Add data labels
  geom_text(aes(x= 32, label = paste0(FIES, "%")), # Setting the x-axis position
            hjust = 0, size = 2,
            family = "opensans", color = "#ffffff") +  # Add data labels
  labs(
    title = "Food Insecurity Experience Scale (FIES) by Province (%)",
    subtitle = "Majority of households are food insecure in almost all provinces, except in Banteay Meanchey, where less than 50% of households are food insecure",
    caption = "Data Source: Family Package Baseline"
  ) + 
  theme(plot.title = element_text(size = 18, family = "opensans", hjust = 0, face = "bold"),
        plot.subtitle = element_text(size = 8, family = "opensans", hjust = 0, face = "italic", lineheight = 1.5,
                                     margin = margin(t = 0, b = 10)),
        plot.caption = element_text(size = 10, hjust = 0, family = "opensans", color = "black"))

# Save the graph
ggsave("figures/FIES_by_Province.png", 
       plot = FIESGraph, width = 6.28, height = 3.98, units = "in",
       bg = "white", dpi = 300)


####################################################################################################################################################
## Reduced Coping Strategies Index

ReducedCopingStrategiesGraph <- ggplot(rCSICopingStrategies, aes(x = reorder(strategy, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "#254769", width = 0.6) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = 5, size = 3.5, family = "opensans", color = "#ffffff") +
  #coord_flip() + # Flips the coordinates for better readability
  labs(title = "Consumption Based Coping Strategies Adopted (%)",
       subtitle = "Percentage of households using each coping strategy to afford food in the last seven days",
       x = "Coping Strategies Adopted",
       y = "Percentage",
       caption = "Data Source: Family Package Baseline") +
  theme_void() +
  theme(
    # Theme to the panel
    panel.background = element_rect(fill = "white", color = "white", size = 0.5),
    panel.grid.major.y = element_line(color = "grey", size = 0.5),
    # Theme the x- axis
    axis.text.x = element_text(lineheight = 1, family = "opensans", size = 8, color = "black", face = "bold", margin = margin(t = 0)),
    axis.title.x = element_blank(),
    axis.ticks.x  = element_blank(),
    # Theme the titles
    plot.title = element_text(family = "opensans", size = 14, color = "black", face = "bold", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(family = "opensans", size = 10, color = "black", face = "italic", hjust = 0, margin = margin(t = 0, b = 20)),
    plot.caption = element_text(family = "opensans", size = 10, color = "black", margin = margin(t = 10, l = 20, b = 10), hjust = 0),
    plot.margin = margin(10, 10, 10, 10))


ggsave("figures/rCSI.png", plot = ReducedCopingStrategiesGraph, 
       width = 6.27, height = 3.5, units = "in",
       bg = "white", dpi = 300)

####################################################################################################################################################
# Table to show which household members are using most severe coping strategies

StrategiesTable <- rCSIMealSizeWhoTable %>%
  gt() %>%
  opt_table_font(
    font = google_font("Open Sans")
  ) %>% 
  tab_header(
    title = md("**When reducing meal sizes is an option...**"),
    subtitle = md("*Female household members reducing meal sizes compared to men*")
  ) %>%
  tab_footnote(
    footnote = md("***Include adults only or children only or all family members***"),
    locations = cells_column_labels(`Other Options (%)`)
  ) %>% 
  fmt_number(
    columns = c(`Male Adults (%)`, `Female Adults (%)`, `Other Options (%)`),
    decimals = 1
  ) %>%
  tab_style(
    style = cell_fill(
      color = "#7A96BF"  # Apply this color to the entire "Female Adults" column including header
    ),
    locations = cells_body(columns = "Female Adults (%)")
  ) %>%
  tab_style(
    style = cell_fill(
      color = "#7A96BF"  # Apply the lighter color to the header of the "Female Adults (%)" column
    ),
    locations = cells_column_labels(columns = `Female Adults (%)`)) %>% 
  tab_style(
    style = cell_text(weight = "bold"),  # Bold the column headers
    locations = cells_column_labels(everything())
  ) %>%
  cols_label(
    `Male Adults (%)` = md("Male<br>Adults (%)"),
    `Female Adults (%)` = md("Female<br>Adults (%)"),
    `Other Options (%)` = md("Other<br>Options (%)")
  ) %>% 
  cols_align(align = "center",
             columns = c(`Male Adults (%)`, `Female Adults (%)`, `Other Options (%)`)) %>% 
  tab_style(style = "vertical-align:center",
            locations = cells_body(columns = Indicator)) %>% 
  cols_width(
    c(`Male Adults (%)`, `Female Adults (%)`, `Other Options (%)`)  ~ px(200)
  ) %>% 
  tab_options(
    table.font.names = "Open Sans",
    heading.align = "left",
    heading.background.color = "#7A96BF",
    #column_labels.align = "centre"  # Center the column headings
  )

#########################################################################################################################################################

#Visualiising child nutrition indicators

ChildNutritionGraph <- SvyChildNutritionAgeGrp %>% 
  ggplot(aes(x = Disaggregation, y = Percentage)) +
  geom_bar(stat = "identity", fill = "#354f52", width = 0.6) +
  geom_text(aes(label = paste0(Percentage, "%"), y = Percentage / 2), 
            size = 3, family = "opensans", color = "#ffffff") +
  coord_flip() +
  facet_wrap(~Indicator, nrow = 1) +
  theme_clean() +
  labs(
    title = "Selected Child Nutrition Indicators, by age group",
    subtitle = "Children aged 6-11 months have poor diet quality compared to other age groups",
    x = "Age Group",
    y = "Percentage",
    caption = "Data Source: Family Package Baseline"
  ) + 
  theme(
    # Theme the panel
    panel.background = element_rect(fill = "white", color = NA, size = 0.5),
    # Theme the x-axis
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.line.x = element_blank(),
    # Theme the y-axis
    axis.text.y = element_text(family = "opensans", size = 9, color = "black", face = "bold"),
    axis.title.y = element_blank(),
    # Theme the titles
    plot.title = element_text(family = "opensans", size = 12, color = "black", face = "bold",
                              margin = margin(l = 0, b = 5)),
    plot.subtitle = element_text(family = "opensans", size = 10, color = "black", face = "italic",
                                 margin = margin(l = 0, b = 10)),
    plot.caption = element_text(family = "opensans", size = 10, color = "black", hjust = 0, margin = margin(t = 0, l = 0, b = 10)),
    # Underline strip text by adding a bottom border to the strip
    strip.background = element_blank(), # Removes the background and border
    #strip.placement = "inside",  # Places the strip text outside the plot
    strip.text = element_text(margin = margin(b = 0, l = -40), family = "opensans", face = "bold"), # Adds margin at the bottom
    panel.border = element_blank())

ggsave("figures/ChildNutritionGraph.png", 
       plot = ChildNutritionGraph, 
       width = 6.27, height = 3.09, 
       units = "in", bg = "white")



#########################################################################################################################################################

# Visualising the SvyChildrenFoodGroupsAgeGrp table

ChildFoodGroupsGraph <- SvyChildrenFoodGroupsAgeGrp %>% 
  filter(Indicator != "Legumes") %>% 
  ggplot(aes(x = fct_reorder(Indicator, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "#254769", width = 0.6) +
  geom_text(aes(label = paste0(Percentage, "%"), y = Percentage / 2), 
            size = 3, family = "opensans", color = "#ffffff") +
  coord_flip() +
  facet_wrap(~Disaggregation) + 
  theme_clean() + 
  labs(
    title = "Food Groups consumed, by child age group",
    subtitle = "Children aged 6-11 months are consuming less of each food group compared to other age groups",
    x = "Food Group",
    y = "Percentage",
    caption = "Data Source: Family Package Baseline"
  ) + 
  theme(
    # Theme the panel
    panel.background = element_rect(fill = "white", color = "white", size = 0.5),
    panel.border = element_blank(),
    # Theme the x-axis
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.line.x = element_blank(),
    # Theme the y-axis
    axis.text.y = element_text(family = "opensans", size = 9, color = "black", face = "bold"),
    axis.title.y = element_blank(),
    # Theme the titles
    plot.title = element_text(family = "opensans", size = 12, color = "black", face = "bold",
                              margin = margin(l = 0, b = 5)),
    plot.subtitle = element_text(family = "opensans", size = 10, color = "black", face = "italic",
                                 margin = margin(l = 0, b = 8)),
    plot.caption = element_text(family = "opensans", size = 10, color = "black", hjust = 0, margin = margin(t = 5, l = 0, b = 10)),
    # Underline strip text by adding a bottom border to the strip
    strip.background = element_blank(), # Removes the background and border
    strip.placement = "bottom",  # Places the strip text outside the plot
    strip.text = element_text(margin = margin(b = 0, unit = "cm"), family = "opensans", face = "bold"))

ggsave("figures/ChildFoodGroupsGraph.png", 
       plot = ChildFoodGroupsGraph, 
       width = 6.27, height = 3.09, 
       units = "in", bg = "white")

#########################################################################################################################################################

# Visualise coping strategies by regiontype

rCSICopingStrategiesAll %>% 
  ggplot(aes(x = fct_rev(regiontype), y = `Yes (%)`)) +
  geom_bar(stat = "identity", fill = if_else(rCSICopingStrategiesAll$regiontype == "Overall Total", "#e28743","#254769"),
           width = 0.6) +
  scale_fill_identity() +
  geom_text(aes(label = paste0(`Yes (%)`, "%"), y = `Yes (%)` / 2), 
            size = 2, family = "opensans", color = "#ffffff") +
  coord_flip() + 
  facet_wrap(~fct_reorder(Indicator, `Yes (%)`), 
             nrow = 1) +
  theme_clean() + 
  labs(
    title = "Consumption Based Coping Strategies Adopted (%), by Province",
    subtitle = "Percentage of households using each coping strategy to afford food in the last seven days",
    x = "Coping Strategies Adopted",
    y = "Percentage",
    caption = "Data Source: Family Package Baseline"
  ) +
  theme(
    # Theme the panel
    panel.background = element_rect(fill = "white", color = "white", size = 0.5),
    panel.border = element_blank(),
    # Theme the x-axis
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.line.x = element_blank(),
    # Theme the y-axis
    axis.text.y = element_text(family = "opensans", size = 9, color = "black"),
    axis.title.y = element_blank(),
    # Theme the titles
    plot.title = element_text(family = "opensans", size = 12, color = "black", face = "bold",
                              margin = margin(l = 0, b = 5)),
    plot.subtitle = element_text(family = "opensans", size = 10, color = "black", face = "italic",
                                 margin = margin(l = 0, b = 8)),
    plot.caption = element_text(family = "opensans", size = 10, color = "black", hjust = 0, margin = margin(t = 5, l = 0, b = 10)),
    # Underline strip text by adding a bottom border to the strip
    strip.background = element_blank(), # Removes the background and border
    strip.placement = "bottom",  # Places the strip text outside the plot
    strip.text = element_text(margin = margin(b = 5, t = 5), family = "opensans", lineheight = 2.3, size = 5))






























































