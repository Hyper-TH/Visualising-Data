# Left join to include all rows from df1 and so on...
merged_df <- merge(education, learning, by = c("Entity", "Year"), all.x = TRUE)
merged_df <- merge(merged_temp, out_of_school, by = c("Entity", "Year"), all.x = TRUE)
merged_df <- merge(merged_df, gender_gap_education, by = c("Entity", "Year"), all.x = TRUE)

merged_df <- merged_df[ , !names(merged_df) %in% c('Secondary_Education_Male', 'Secondary_Education_Female', 'Primary_Education_Male', 'Primary_Education_Female')]

# Combine for world data
world_df <- merge(w_education, w_out_of_school, by = c("Entity", "Year"), all.x = TRUE)
world_df <- merge(world_df, w_gender_gap, by = c("Entity", "Year"), all.x = TRUE)

world_df <- world_df[ , !names(world_df) %in% c('Entity')]

# For column Developing/Developed
merged_df <- merged_df %>%
  rename(
    Country = Entity
  )

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

## TODO: Combine out_of_school and gender_gap_filtered and education_ratio
