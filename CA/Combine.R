# rm(merged_df)
# rm(merged_df_2)
# rm(world_df)

# Left join to include all rows from df1 and so on...
merged_df <- merge(education, learning, by = c("Country", "Year"), all.x = TRUE)
merged_df <- merge(merged_df, out_school, by = c("Country", "Year"), all.x = TRUE)
merged_df <- merge(merged_df, gender_gap, by = c("Country", "Year"), all.x = TRUE)

# Merged df is only for 2020 / 2015 and they have no data for these columns
merged_df <- merged_df[ , !names(merged_df) %in% c('Secondary_Education_Male', 'Secondary_Education_Female', 'Primary_Education_Male', 'Primary_Education_Female')]



# List of developing countries
developed_countries <- c(
  "Andorra", "Australia", "Austria", "Belgium", "Canada", "Croatia", "Cyprus",
  "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany",
  "Greece", "Hong Kong", "Iceland", "Ireland", "Israel", "Italy", "Japan",
  "Korea", "Latvia", "Lithuania", "Luxembourg", "Macao", "Malta", "Netherlands",
  "New Zealand", "Norway", "Portugal", "Puerto Rico", "San Marino", "Singapore",
  "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Taiwan Province of China",
  "United Kingdom", "United States"
)

merged_df <- merged_df %>%
  mutate(World_Status = case_when(
    Country %in% developed_countries ~ "Developed",
    TRUE ~ "Developing"
  ))


# Add population
# Select only the key columns and the Children_Population column
population_relevant <- population[, c("Country", "Year", "Children_Population_5_to_19")]
merged_df <- merge(merged_df, population_relevant, by = c("Country", "Year"), all.x = TRUE)

merged_2015 <- merged_df[merged_df$Year == 2015, ]
merged_2020 <- merged_df[merged_df$Year == 2020, ]

# Drop N/A columns
merged_2015 <- merged_2015[ , !names(merged_2015) %in% c('Learning_Years')]
merged_2020 <- merged_2020[ , !names(merged_2020) %in% c('Dropped_Out_Male')]
merged_2020 <- merged_2020[ , !names(merged_2020) %in% c('Dropped_Out_Female')]
education_ratio$Code <- NULL
out_of_school$Code <- NULL

merged_df_2 <- merge(out_of_school, gender_gap, by = c("Country", "Year"), all.x = TRUE)
merged_df_2 <- merge(merged_df_2, education_ratio, by = c("Country", "Year"), all.x = TRUE)

# Remove non-country values
selected_continents <- c(
  "World",
  "Arab World (WB)", 
  "Central Europe and the Baltics (WB)", 
  "EU (27)", 
  "East Asia and the Pacific (WB)", 
  "Europe and Central Asia (WB)", 
  "Latin America and Caribbean (WB)", 
  "Middle East and North Africa (WB)", 
  "North America (WB)", 
  "South Asia (WB)", 
  "Sub-Saharan Africa (WB)"
)

merged_df_2 <- merged_df_2 %>%
  filter(!(Country %in% selected_continents))

merged_df_2 <- merged_df_2 %>%
  mutate(World_Status = case_when(
    Country %in% developing_countries ~ "Developing",
    TRUE ~ "Developed"
  ))

# Merge population
merged_df_2 <- merge(merged_df_2, population_relevant, by = c("Country", "Year"), all.x = TRUE)

merged_df_2 <- merged_df_2[ , !names(merged_df_2) %in% c('With_Education_Share')]
merged_df_2 <- merged_df_2[ , !names(merged_df_2) %in% c('With_No_Education_Share')]


# Combine for world data
world_df <- merge(w_education, w_out_of_school, by = c("Country", "Year"), all.x = TRUE)
world_df <- merge(world_df, w_gender_gap, by = c("Country", "Year"), all.x = TRUE)

# Merge population 
world_df <- merge(world_df, population_relevant, by = c("Country", "Year"), all.x = TRUE)

# Remove rows where population is N/A
world_df <- world_df %>%
  filter(!is.na(Children_Population_5_to_19))

world_df <- world_df %>%
  mutate(Unenrolled_Secondary_Children = 200 - ((100 - Secondary_Education_Male) + (100 - Secondary_Education_Female)))

world_df <- world_df %>%
  mutate(Unenrolled_Primary_Children = 200 - ((100 - Primary_Education_Male) + (100 - Primary_Education_Female)))

world_df <- world_df[ , !names(world_df) %in% c('Country')]


# Rename columns for clarity
merged_df_2 <- merged_df_2 %>%
  rename(
    Dropped_Out_HS_Female = Dropped_Out_Female,
    Dropped_Out_HS_Male = Dropped_Out_Male,
  )

merged_2015 <- merged_2015 %>%
  rename(
    Dropped_Out_HS_Female = Dropped_Out_Female,
    Dropped_Out_HS_Male = Dropped_Out_Male,
  )

merged_2020 <- merged_2020 %>%
  rename(
    With_Education = With_Education_Share,
    Without_Education = With_No_Education_Share
  )

merged_2015 <- merged_2015 %>%
  rename(
    With_Education = With_Education_Share,
    Without_Education = With_No_Education_Share
  )

world_df <- world_df %>%
  rename(
    With_Education = With_Education_Share,
    Without_Education = With_No_Education_Share
  )


write.csv(merged_2015, file = "C:/COMPSCI_PROJECTS/R/CA/data/dataframes/merged_2015.csv", row.names = FALSE)
write.csv(merged_2020, file = "C:/COMPSCI_PROJECTS/R/CA/data/dataframes/merged_2020.csv", row.names = FALSE)
write.csv(merged_df_2, file = "C:/COMPSCI_PROJECTS/R/CA/data/dataframes/merged_df_2.csv", row.names = FALSE)
write.csv(world_df, file = "C:/COMPSCI_PROJECTS/R/CA/data/dataframes/world_df.csv", row.names = FALSE)
