# Load required libraries
library(tidyverse)
library(lubridate)

# Load mobility data
mobility_data <- read_csv("data/Global_Mobility_Report.csv")
# Load continent mapping dataset
continent_data <- read_csv("data/country_continent_mapping.csv")
# Load census data
census_data <- read_csv("data/COVID-19_cases_plus_census.csv")
# Load Texas COVID-19 data
cases_tx <- read_csv("data/COVID-19_cases_TX.csv")

# Merge mobility data with continent data on country code
merged_data <- mobility_data %>%
  left_join(continent_data %>% select(`alpha-2`, `sub-region`), by = c("country_region_code" = "alpha-2"))

# Filter non-missing sub-region
filtered_data <- merged_data %>% filter(!is.na(`sub-region`))

# Define the mobility metrics you want to plot
mobility_metrics <- c("retail_and_recreation_percent_change_from_baseline",
                      "grocery_and_pharmacy_percent_change_from_baseline",
                      "parks_percent_change_from_baseline",
                      "transit_stations_percent_change_from_baseline",
                      "workplaces_percent_change_from_baseline",
                      "residential_percent_change_from_baseline")

# Create a long format dataset by pivoting the selected metrics
long_mobility_data <- filtered_data %>%
  pivot_longer(cols = all_of(mobility_metrics), names_to = "mobility_metric", values_to = "percent_change") %>%
  filter(!is.na(percent_change))

# Group by date, sub-region, and mobility metric, then summarize
grouped_mobility_data <- long_mobility_data %>%
  group_by(date, `sub-region`, mobility_metric) %>%
  summarise(avg_mobility_change = mean(percent_change, na.rm = TRUE))

# Remove constant or zero variance columns
census_numeric <- census_data %>%
  select_if(is.numeric) %>%
  filter_all(any_vars(. != 0)) %>%
  select_if(~ var(.) != 0) %>%
  na.omit() %>%
  scale()

pca <- prcomp(census_numeric, center = TRUE, scale. = TRUE)
loadings <- pca$rotation

# Extract PC1 and PC2 scores
pca_scores <- as.data.frame(pca$x[, 1:2])
pca_scores$state <- census_data$state
pca_scores$county_name <- census_data$county_name

# Highlight Texas counties
pca_scores$highlight <- ifelse(pca_scores$state == "TX", "Texas", "Other")

# Calculate the daily change in confirmed cases and deaths
cases_delta <- cases_tx %>%
  arrange(county_fips_code, date) %>%
  group_by(county_fips_code) %>%
  mutate(
    daily_confirmed_cases = confirmed_cases - lag(confirmed_cases, default = 0),
    daily_deaths = deaths - lag(deaths, default = 0)
  ) %>%
  ungroup()

# Convert the date to week using floor_date from lubridate
cases_delta <- cases_delta %>%
  mutate(week = floor_date(date, "week"))

# Summarize the weekly total deaths and confirmed cases
weekly_cases <- cases_delta %>%
  group_by(week) %>%
  summarise(
    weekly_deaths = sum(daily_deaths, na.rm = TRUE),
    weekly_confirmed_cases = sum(daily_confirmed_cases, na.rm = TRUE)
  )

# Reshape the data for faceted plot
weekly_cases_long <- weekly_cases %>%
  pivot_longer(cols = c(weekly_deaths, weekly_confirmed_cases), names_to = "variable", values_to = "count")

# Create a lookup table for state abbreviations and full names
state_lookup <- tibble(
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                 "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                 "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                 "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                 "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  state_full = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                 "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                 "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                 "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                 "New Hampshire", "New Jersey", "New Mexico", "New York", 
                 "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
                 "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                 "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
                 "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

# Filter mobility data for US and Texas, and merge with census data
tx_mobility_data <- mobility_data %>%
  filter(country_region == "United States") %>%
  rename(mobility_date = date) %>%
  filter(!is.na(sub_region_1) & !is.na(sub_region_2)) %>%
  filter(sub_region_1 == "Texas")

# Merge the state names into the cases dataset using the lookup table
pca_state <- pca_scores %>%
  left_join(state_lookup, by = c("state" = "state_abbr"))

head(pca_state)

# Filter mobility data for US and Texas, ensure sub_region_1 and sub_region_2 are not NA
us_mobility_data <- mobility_data %>%
  filter(country_region == "United States", 
         !is.na(sub_region_1), 
         !is.na(sub_region_2)) %>%
  rename(mobility_date = date)

# Merge mobility and census data
census_and_mobility <- us_mobility_data %>%
  left_join(pca_state, by = c("sub_region_2" = "county_name", "sub_region_1" = "state_full"))

# Group cases_tx by week and summarize confirmed cases and deaths
cases_tx_filtered <- cases_tx %>%
  select(county_name, date, confirmed_cases, deaths)

cases_tx_weekly <- cases_tx_filtered %>%
  mutate(week = floor_date(date, unit = "week")) %>%
  group_by(county_name, week) %>%
  summarise(confirmed_cases = sum(confirmed_cases, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            .groups = 'drop')

cases_tx_long <- cases_tx_weekly %>%
  pivot_longer(cols = c(confirmed_cases, deaths), names_to = "metric", values_to = "count")

census_and_mobility_tx <- census_and_mobility %>%
  filter(sub_region_1 == "Texas")

final_merged_dataset <- census_and_mobility_tx %>%
  left_join(cases_tx_long, by = c("sub_region_2" = "county_name"))

glimpse(final_merged_dataset)

final_merged_dataset <- readRDS("data/final_merged_dataset.rds")
