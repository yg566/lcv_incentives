# This script performs a sensitivity analysis on changing the initial value in TCO calculation 
# between electric vehicles (EV) and gasoline vehicles across different states 
# and vehicle types. The analysis visualizes the TCO differences using a 
# heatmap for each state and vehicle type combination.

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# Read the Excel file containing initial value data
data3 <- read_excel("sensitive_analysis_data/initial_value.xlsx")

# Define columns to remove based on specified ranges and specific values
cols_to_remove <- c(seq(-0.9, -0.5, by = 0.1), seq(0.5, 0.9, by = 0.1), 1, -1)
cols_to_remove <- sprintf("%.1f", cols_to_remove)
cols_to_remove[cols_to_remove %in% c("-1.0", "1.0")] <- sprintf("%.0f", as.numeric(cols_to_remove[cols_to_remove %in% c("-1.0", "1.0")]))

# Remove the specified columns from the dataset
data3 <- data3 %>%
  select(-all_of(cols_to_remove))

# Function to calculate differences for a given state and vehicle type
calc_differences <- function(state, vehicle_type, data) {
  # Filter EV and Gasoline data for the given state and vehicle type
  ev_data <- data %>%
    filter(State_Name == state, Vehicle_Type == vehicle_type, Fuel == "EV") %>%
    select(matches('^(0|\\d+)(\\.\\d+)?$|^-*[0-9]+(\\.[0-9]+)?$')) %>%
    unlist() %>%
    as.numeric()
  
  gasoline_data <- data %>%
    filter(State_Name == state, Vehicle_Type == vehicle_type, Fuel == "Gasoline") %>%
    select(matches('^(0|\\d+)(\\.\\d+)?$|^-*[0-9]+(\\.[0-9]+)?$')) %>%
    unlist() %>%
    as.numeric()
  
  # Initialize a matrix to store the differences
  difference_matrix <- matrix(nrow = length(ev_data), ncol = length(gasoline_data))
  
  # Calculate the pairwise differences
  for (i in 1:length(ev_data)) {
    for (j in 1:length(gasoline_data)) {
      difference_matrix[i, j] <- gasoline_data[j] - ev_data[i]
    }
  }
  
  # Convert the matrix to a long format for ggplot
  difference_data <- as.data.frame(difference_matrix)
  names(difference_data) <- seq(-0.4, by = 0.1, length.out = ncol(difference_data))
  difference_data$EV_Mileage <- seq(-0.4, by = 0.1, length.out = nrow(difference_data))
  long_data <- tidyr::pivot_longer(difference_data, cols = -EV_Mileage, names_to = "Gasoline_Mileage", values_to = "Difference")
  
  long_data = long_data %>% mutate(state = state, vehicle_type = vehicle_type)
  return(long_data)
}

# Create a combination of states and vehicle types and calculate differences
out <- tidyr::expand_grid(
  states = c("Delaware", "Maine", "Ohio", "Arizona", "California", "New York", "District of Columbia"),
  vehicle_types = c("Small LCV", "Medium LCV", "Large LCV") 
) %>%
  mutate(id = 1:n()) %>%
  split(.$id) %>%
  purrr::map_dfr(~calc_differences(state = .$states, vehicle_type = .$vehicle_types, data = data3), .id = "id")

# Set the factor levels for vehicle types
out$vehicle_type <- factor(out$vehicle_type,
                           levels = c("Small LCV",
                                      "Medium LCV",
                                      "Large LCV"))

# Set options to avoid scientific notation
options(scipen = 10000)

# Generate the heatmap visualization
ggplot() +
  geom_tile(data = out, aes(x = as.numeric(Gasoline_Mileage), y = as.numeric(EV_Mileage), fill = Difference)) +
  scale_fill_gradient2(low = "#399213", high = "#7B143A", mid = "white", midpoint = 0) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + # Add this line for the diagonal
  theme_minimal() +
  labs(x = "Gasoline Vehicle Initial Value Discount Rate",
       y = "EV Initial Value Discount Rate",
       fill = "Difference") +
  facet_grid(cols = vars(state), rows = vars(vehicle_type)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(
    theme = theme(
      legend.key.width = unit(dev.size()[1] * 2, "inches")
    ))
  ) +
  theme(strip.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.y = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain")) +
  theme(legend.text = element_text(size = 13)) +
  theme(legend.title = element_text(size = 15)) +
  xlim(-0.4, 0.4) +
  ylim(-0.4, 0.4)

# Save the plot
ggsave("value.png", width = 16, height = 9, dpi = 800)
