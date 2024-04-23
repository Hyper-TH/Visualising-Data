

# Check the complete rows
complete_rows_df_2 <- merged_df_2[complete.cases(merged_df_2), ]
complete_rows_df_2

# Get the common years present in all entities (countries)
# Step 1: Count the number of unique entities per year
yearly_country_count <- complete_rows_df_2 %>%
  group_by(Year) %>%
  summarise(NumCountries = n_distinct(Country))

# Step 2: Find the total number of distinct entities
total_countries <- n_distinct(complete_rows_df_2$Country)

# Step 3: Filter the years where the number of entities matches the total number of entities
common_years <- yearly_country_count %>%
  filter(NumCountries == total_countries) %>%
  pull(Year)  # Extract the Years as a vector

print(common_years)

# Step 4: Use the common years to filter the original dataframe
common_years_df <- complete_rows_df_2 %>%
  filter(Year %in% common_years)

# No year found