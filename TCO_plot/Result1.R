#This script performs data manipulation, visualization of the total cost of ownership (TCO) 
#by state and vehicle type, and visualizes the TCO difference between eLCV and gasoline LCV. 

# Load necessary libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Read the TCO data
dataset <- read_excel("TCO_data/tco.xls")

# List of cost components to exclude
cost_components <- c("value", "sub", "ec", "annual_tax", "annual_reg", "mc", "ic", "final_val")

# Remove the specified cost components
dataset <- dataset %>%
  select(-all_of(cost_components))

# Set factor levels for vehicle types
dataset$Vehicle_Type <- factor(dataset$Vehicle_Type,
                               levels = c("Small LCV",
                                          "Medium LCV",
                                          "Large LCV"))

# Plot TCO by state and vehicle type
dataset %>% 
  filter(Vehicle_Type %in% c("Small LCV", "Medium LCV", "Large LCV"),
         Fuel_Type %in% c("EV", "Gasoline")) %>% 
  ggplot(aes(x = Fuel_Type, y = TCO, fill = interaction(Fuel_Type, Vehicle_Type))) +
  geom_col() + 
  facet_grid(rows = vars(Vehicle_Type), cols = vars(State_Name), 
             scales = "free_x", space = "free_x", switch = "x") +
  theme(strip.placement = "outside", strip.background = element_rect(fill = "white"), axis.title.x = element_blank()) +
  scale_y_continuous(limits=c(0, 150000)) +
  scale_fill_manual(values = c("#99D281", "#5E9447", "#BD7286", "#B53355", "#7093B1","#3272AB" )) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  labs(y = "Total Cost of Ownership ($)") +
  theme(strip.text = element_text(size = 15),
        axis.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

# Save the TCO plot
ggsave("TCO_percentage_by_state_vehicle_plot.png", width = 15, height = 12, dpi = 800)

