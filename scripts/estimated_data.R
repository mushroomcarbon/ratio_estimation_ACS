# Load necessary libraries
library(dplyr)
library(tidyverse)
library(here)

# Read the raw data file
data <- read.csv(here::here("data/data.csv"))

# Read the CSV file that contains the statewise doctoral counts with state names
statewise_doctoral_counts <- read.csv(here::here("data/statewise_doctoral_counts.csv"))

# Convert the 'State' column in `statewise_doctoral_counts` to match numeric 'STATEICP' in raw data
# Create a mapping of state names to 'STATEICP' codes
state_icp_mapping <- data.frame(
  STATEICP = c(
    1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 21, 22, 23, 24, 25, 31, 32, 33, 34, 35, 36, 37,
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 51, 52, 53, 54, 56, 61, 62, 63, 64, 65, 66,
    67, 68, 71, 72, 73, 81, 82, 83, 98
  ), # ICPSR codes
  State = c(
    "Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
    "Delaware", "New Jersey", "New York", "Pennsylvania", "Illinois", "Indiana", "Michigan",
    "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota",
    "South Dakota", "Virginia", "Alabama", "Arkansas", "Florida", "Georgia", "Louisiana",
    "Mississippi", "North Carolina", "South Carolina", "Texas", "Kentucky", "Maryland",
    "Oklahoma", "Tennessee", "West Virginia", "Arizona", "Colorado", "Idaho", "Montana",
    "Nevada", "New Mexico", "Utah", "Wyoming", "California", "Oregon", "Washington", "Alaska",
    "Hawaii", "Puerto Rico", "District of Columbia"
  ) # State names
)

# Convert state names back to 'STATEICP' codes for merging purposes
statewise_doctoral_counts <- statewise_doctoral_counts %>%
  left_join(state_icp_mapping, by = "State") %>%
  select(STATEICP, Doctoral_Count, State) # Retain necessary columns

# Calculate the actual total number of respondents in each state from raw data
statewise_total_counts <- data %>%
  group_by(STATEICP) %>%
  summarise(Actual_Total_Respondents = n())

# Merge the real data (with 'STATEICP') with the total respondents from raw data
statewise_data <- statewise_doctoral_counts %>%
  left_join(statewise_total_counts, by = "STATEICP")

# Define the total number of respondents in California (given value)
california_total_respondents <- 391171 # Given value for California

# Extract the number of doctoral respondents in California from the merged data
california_doctoral_count <- statewise_data %>%
  filter(State == "California") %>%
  pull(Doctoral_Count)

# Calculate the ratio for California using Laplace ratio estimator approach
california_ratio <- california_doctoral_count / california_total_respondents

# Estimate the total number of respondents for each state using the ratio estimator approach
statewise_data <- statewise_data %>%
  mutate(Estimated_Total_Respondents = Doctoral_Count / california_ratio)

# Remove the 'STATEICP' and 'Doctoral_Count' columns
statewise_data <- statewise_data %>%
  select(-STATEICP, -Doctoral_Count)

# Round 'Estimated_Total_Respondents' to the nearest integer
statewise_data <- statewise_data %>%
  mutate(Estimated_Total_Respondents = round(Estimated_Total_Respondents))

# Save the modified dataframe to a new CSV file
write.csv(statewise_data, here::here("data/final_estimated_total_respondents.csv"), row.names = FALSE)

# Print the resulting dataframe
print(statewise_data)
