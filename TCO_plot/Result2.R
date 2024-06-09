# The code calculates the percentage of each cost component in the total cost of ownership (TCO) 
# and visualizes these percentages for different vehicle types and states.

# Load necessary libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Read the TCO data
dataset <- read_excel("TCO_data/tco.xls")

# List of cost components
cost_components <- c("value", "sub", "ec", "annual_tax", "annual_reg", "mc", "ic", "final_val")

# Calculate the percentage of each cost component in TCO
dataset <- dataset %>%
  mutate(across(all_of(cost_components), ~ .x / TCO, .names = "pct_{.col}"))

# List of cost components with prefix "pct_"
cost_components <- c("pct_value", "pct_sub", "pct_ec", "pct_annual_tax", "pct_annual_reg", "pct_mc", "pct_ic", "pct_final_val")

# Convert the dataset to long format
dataset_long <- dataset %>%
  pivot_longer(
    cols = cost_components,
    names_to = "name",
    values_to = "pct"
  )

# Custom labels for cost components
custom_labels <- c("pct_value" = "Initial Vehicle Value", "pct_reg" = "Registration Fee", "pct_sub" = "Government Subsidies", "pct_ec" = "Energy Cost",
                   "pct_annual_tax" = "Annual Vehicle Taxes", "pct_annual_reg" = "Renew Registration Fee",
                   "pct_mc" = "Maintenance Cost", "pct_ic" = "Insurance Cost", "pct_final_val" = "Vehicle Final Value")

# Filter data for Delaware
dataset_delaware_vehicles <- dataset_long %>%
  filter(State_Name == "Delaware",
         Vehicle_Type %in% c("Small LCV", "Medium LCV", "Large LCV"),
         Fuel_Type %in% c("EV", "Gasoline"))

# Plot the percentage of each cost component in Delaware
ggplot(dataset_delaware_vehicles, aes(fill = name, y = pct, x = Fuel_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~Vehicle_Type, scales = "free_x", space = "free_x", switch = "x") +
  scale_fill_discrete(name = "Cost Component", labels = custom_labels) +
  theme(strip.placement = "outside", strip.background = element_rect(fill = "white"), axis.title = element_blank()) +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Delaware")

# Filter data for visualization of all states
viz <- dataset_long %>%
  filter(Vehicle_Type %in% c("Small LCV", "Medium LCV", "Large LCV"),
         Fuel_Type %in% c("EV", "Gasoline"))

# Plot the percentage of each cost component for all states
ggplot() +
  geom_bar(data = viz, aes(fill = name, y = pct, x = Fuel_Type), stat = "identity", position = "stack") +
  geom_bar(data = viz %>% filter(name == "pct_value"), aes(fill = name, y = pct, x = Fuel_Type), stat = "identity", position = "stack", fill = NA, color = "#373737", size = 0.5) +
  facet_grid(rows = vars(Vehicle_Type), cols = vars(State_Name), scales = "free_x", space = "free_x", switch = "x") +
  scale_fill_discrete(name = "Cost Component", labels = custom_labels) +
  theme(strip.placement = "outside", strip.background = element_rect(fill = "white"), axis.title.x = element_blank()) +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(y = "Percentage of Each Cost Components in TCO (%)") +
  theme(legend.position = "bottom", strip.text = element_text(size = 15),
        axis.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.text = element_text(size = 13), legend.title = element_text(size = 15))

# Save the percentage cost component plot
ggsave("TCO_percentage_by_state_vehicle_plot.png", width = 15, height = 12, dpi = 800)
