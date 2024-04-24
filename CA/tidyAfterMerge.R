# Check the complete rows
complete_rows_df_2 <- merged_df_2[complete.cases(merged_df_2), ]
complete_rows_df_2

# Get the common years present in all entities (countries)
# Step 1: Count the number of unique entities per year
yearly_country_count <- complete_rows_df_2 %>%
  group_by(Year) %>%
  summarise(NumCountries = n_distinct(Country))

print(yearly_country_count)

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

print(common_years_df)
# No Common year found


# Filter the dataframe for developing countries and the specified years
df_developing <- merged_df_2 %>%
  filter(World_Status == "Developing", Year %in% c(2000, 2009, 2018))

# Group by country and summarise to check for complete cases in each year
complete_countries <- df_developing %>%
  group_by(Country) %>%
  summarise(
    Complete_Cases_2000 = sum(Year == 2000, na.rm = TRUE),
    Complete_Cases_2009 = sum(Year == 2009, na.rm = TRUE),
    Complete_Cases_2018 = sum(Year == 2018, na.rm = TRUE),
    All_Years_Complete = sum(complete.cases(.)[Year %in% c(2000, 2009, 2018)])
  ) %>%
  # Filter for countries with at least one complete case in all specified years
  filter(Complete_Cases_2000 > 0, Complete_Cases_2009 > 0, Complete_Cases_2018 > 0, All_Years_Complete == 3)

# Print the result
print(complete_countries)

# Filter for developing countries, count unique years, and get the top x
top_developing_countries <- merged_df_2 %>%
  filter(World_Status == "Developing") %>%
  group_by(Country) %>%
  summarise(Count_Years = n_distinct(Year)) %>%
  arrange(desc(Count_Years)) %>%
  slice_head(n = 30)

# Filter for developed countries, count unique years, and get the top x
top_developed_countries <- merged_df_2 %>%
  filter(World_Status == "Developed") %>%
  group_by(Country) %>%
  summarise(Count_Years = n_distinct(Year)) %>%
  arrange(desc(Count_Years)) %>%
  slice_head(n = 35)


# Check for country in the year 2000
country_has_2000 <- "Belgium" %in% merged_df_2$Country[merged_df_2$Year == 2000]

# Check for country in the year 2009
country_has_2009 <- "Belgium" %in% merged_df_2$Country[merged_df_2$Year == 2009]

# Check for country in the year 2018
country_has_2018 <- "Belgium" %in% merged_df_2$Country[merged_df_2$Year == 2018]

print(paste("Country has the year 2000:", country_has_2000))
print(paste("Country has the year 2009:", country_has_2009))
print(paste("Country has the year 2018:", country_has_2018))

# Cuba
# Turkey
# Eritrea
# Bolivia
# Bhutan
##
# Ireland
# United Kingdom
# Macao
# South Korea
# Belgium

subset_df <- merged_df_2 %>%
  filter(Country %in% c("Cuba", "Turkey", "Eritrea", "Bolivia", "Bhutan", 
                        "Ireland", "United Kingdom", "Macao", "South Korea", "Belgium") &
           Year %in% c(2000, 2009, 2018))

length(unique(merged_df_2$Country))
