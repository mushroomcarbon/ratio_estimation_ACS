# Load necessary libraries
library(dplyr)
library(tidyverse)
library(here)

# Read the data
data <- read.csv(here::here("data/data.csv"))

# Filter for rows where educational attainment (EDUCD) is equal to 116 (Doctoral degree)
doctoral_data <- data %>% filter(EDUCD == 116)

# Group by state (STATEICP) and count the number of doctoral degrees
statewise_doctoral_counts <- doctoral_data %>%
  group_by(STATEICP) %>%
  summarise(Doctoral_Count = n())

# Create a lookup table for STATEICP to state name mapping
state_lookup <- data.frame(
  STATEICP = c(1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 21, 22, 23, 24, 25, 31, 32, 33, 34, 35, 36, 37, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 51, 52, 53, 54, 56, 61, 62, 63, 64, 65, 66, 67, 68, 71, 72, 73, 81, 82, 83, 96, 97, 98, 99),
  State = c(
    "Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "Delaware",
    "New Jersey", "New York", "Pennsylvania", "Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin",
    "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota", "Virginia",
    "Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "North Carolina",
    "South Carolina", "Texas", "Kentucky", "Maryland", "Oklahoma", "Tennessee", "West Virginia",
    "Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", "California",
    "Oregon", "Washington", "Alaska", "Hawaii", "Puerto Rico", "State Groupings", "Military/Mil. Reservations",
    "District of Columbia", "State not identified"
  )
)

# Merge the state names into the doctoral counts dataframe
statewise_doctoral_counts <- statewise_doctoral_counts %>%
  left_join(state_lookup, by = "STATEICP")

# Select only the State and Doctoral_Count columns and remove STATEICP
statewise_doctoral_counts <- statewise_doctoral_counts %>%
  select(State, Doctoral_Count)

# Save the result to a CSV file
write.csv(statewise_doctoral_counts, here::here("data/statewise_doctoral_counts.csv"), row.names = FALSE)
