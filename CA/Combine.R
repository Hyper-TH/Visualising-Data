rm(merged_df)
rm(merged_df_2)
rm(world_df)

# Left join to include all rows from df1 and so on...
merged_df <- merge(education, learning, by = c("Country", "Year"), all.x = TRUE)
merged_df <- merge(merged_df, out_school, by = c("Country", "Year"), all.x = TRUE)
merged_df <- merge(merged_df, gender_gap, by = c("Country", "Year"), all.x = TRUE)

# Merged df is only for 2020 / 2015 and they have no data for these columns
merged_df <- merged_df[ , !names(merged_df) %in% c('Secondary_Education_Male', 'Secondary_Education_Female', 'Primary_Education_Male', 'Primary_Education_Female')]



# List of developing countries
developing_countries <- c(
  "Albania", "Armenia", "Azerbaijan", "Belarus", "Bosnia & Herzegovina", "Georgia", 
  "Hungary", "Kosovo", "Macedonia (Former Yugoslav Republic)", "Moldova", "Montenegro", 
  "Poland", "Romania", "Slovakia", "Serbia", "Turkey", "Ukraine", "Algeria", "Egypt", 
  "Libya", "Morocco", "Tunisia", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
  "Cameroon", "Cabo Verde", "Central African Republic", "Chad", "Comoros", "Congo", 
  "Congo (Democratic Republic of the)", "Cote d'Ivoire", "Djibouti", "Equatorial Guinea", 
  "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
  "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", 
  "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "St. Helena", "Sao Tome & Principe", 
  "Senegal", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", 
  "Togo", "Uganda", "Zambia", "Zimbabwe", "Belize", "Costa Rica", "Cuba", "Dominica", 
  "Dominican Republic", "El Salvador", "Grenada", "Guatemala", "Haiti", "Honduras", 
  "Jamaica", "Mexico", "Montserrat", "Nicaragua", "Panama", "St. Lucia", "St. Vincent and the Grenadines", 
  "Argentina", "Bolivia", "Brazil", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", 
  "Suriname", "Venezuela", "Afghanistan", "Bangladesh", "Bhutan", "Cambodia", "China", 
  "India", "Indonesia", "Kazakhstan", "Korea (Democratic People’s Republic of)", "Kyrgyzstan", 
  "Lao People’s Democratic Republic", "Malaysia", "Maldives", "Mongolia", "Myanmar", 
  "Nepal", "Pakistan", "Philippines", "Sri Lanka", "Tajikistan", "Thailand", "Timor Leste", 
  "Turkmenistan", "Uzbekistan", "Vietnam", "Iran", "Iraq", "Jordan", "Lebanon", 
  "Syrian Arab Republic", "West Bank and Gaza Strip", "Yemen", "Cook Islands", "Fiji", 
  "Kiribati", "Marshall Islands", "Micronesia", "Nauru", "Niue", "Palau", "Papua New Guinea", 
  "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis & Futuna"
)

merged_df <- merged_df %>%
  mutate(World_Status = case_when(
    Country %in% developing_countries ~ "Developing",
    TRUE ~ "Developed"
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


## Combine out_of_school and gender_gap_filtered and education_ratio
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


# Merge population
merged_df_2 <- merge(merged_df_2, population_relevant, by = c("Country", "Year"), all.x = TRUE)

merged_df_2 <- merged_df_2[ , !names(merged_df_2) %in% c('With_Education_Share')]
merged_df_2 <- merged_df_2[ , !names(merged_df_2) %in% c('With_No_Education_Share')]

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


# Combine for world data
world_df <- merge(w_education, w_out_of_school, by = c("Entity", "Year"), all.x = TRUE)
world_df <- merge(world_df, w_gender_gap, by = c("Entity", "Year"), all.x = TRUE)

world_df <- world_df %>%
  rename(
    Country = Entity
  )


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

