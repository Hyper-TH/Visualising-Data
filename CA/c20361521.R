### PACKAGES ###
required_packages <- c("tidyverse", "knitr", "DT", "plotly", "ggvis", "manipulateWidget", "cowplot", "dplyr")

#Start up messages suppression
suppressPackageStartupMessages({
  
  # Check if the package you need is in the list and install if so
  for (package in required_packages) {
    # Checks if the package you want is already installed or not, if true then dont return any annoying messages
    if (!requireNamespace(package, quietly = TRUE)) {
      # Catch any annoying error or warning messages
      tryCatch({
        # Generic code we used to use for loading packages
        install.packages(package)
        library(package, character.only = TRUE)
      }, error = function(e) {
        #part of the try catch 
        cat("Error installing or loading package:", package, "\n")
      })
    } else {
      library(package, character.only = TRUE)
    }
  }
  
})
# Returns only this line of code once all are installed
cat("All required packages are loaded.\n")

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# Get data
mydata = file.path("C:/COMPSCI_PROJECTS/R/CA/data")

datapath1 <- file.path(mydata,'1- share-of-the-world-population-with-at-least-basic-education.csv')
datapath2 <- file.path(mydata,'2- learning-adjusted-years-of-school-lays.csv')
datapath3 <- file.path(mydata,'3- number-of-out-of-school-children.csv')
datapath4 <- file.path(mydata,'4- gender-gap-education-levels.csv')
datapath5 <- file.path(mydata,'population-and-demography.csv')

education_ratio <- read.csv(datapath1)
learning_years <- read.csv(datapath2)
out_of_school <- read.csv(datapath3)
gender_gap_education <- read.csv(datapath4)
population <- read.csv(datapath5)

## EDUCATION RATIO

# Rename columns
education_ratio <- education_ratio %>%
  rename(
    With_No_Education_Share = Share.of.population.with.no.formal.education..1820.2020,
    With_Education_Share = Share.of.population.with.some.formal.education..1820.2020
  )

# Create column to check the ratio if its equal to 100
education_ratio <- education_ratio %>%
  mutate(check = With_No_Education_Share + With_Education_Share)

unique(education_ratio$check)

# Avoid inconsistency of decimal error by rounding the values 
education_ratio <- education_ratio %>%
  mutate(
    With_No_Education_Share = round(With_No_Education_Share, 1),
    With_Education_Share = round(With_Education_Share, 1),
    check_1 = With_No_Education_Share + With_Education_Share
  )

unique(education_ratio$check_1)

# If values up to 100, drop the extra columns
education_ratio <- education_ratio[, !names(education_ratio) %in% c('check', 'check_1')]


## LEARNING YEARS
# Rename columns
learning_years <- learning_years %>%
  rename(
    Learning_Years = Learning.Adjusted.Years.of.School
  )

# Round the learning years to first decimal point
learning_years <- learning_years %>%
  mutate(Learning_Years = round(Learning_Years))

## OUT OF SCHOOL
# Rename columns
out_of_school <- out_of_school %>%
  rename(
    Dropped_Out_Male = Out.of.school.children..adolescents.and.youth.of.primary.and.secondary.school.age..male..number.,
    Dropped_Out_Female = Out.of.school.children..adolescents.and.youth.of.primary.and.secondary.school.age..female..number.
  )

## GENDER GAP EDUCATION
gender_gap_education <- gender_gap_education %>%
  rename(
    Tertiary_Education_Female = Combined.gross.enrolment.ratio.for.tertiary.education..female,
    Tertiary_Education_Male = Combined.gross.enrolment.ratio.for.tertiary.education..male,
    Primary_Education_Female = Combined.total.net.enrolment.rate..primary..female,
    Primary_Education_Male = Combined.total.net.enrolment.rate..primary..male,
    Secondary_Education_Female = Combined.total.net.enrolment.rate..secondary..female,
    Secondary_Education_Male = Combined.total.net.enrolment.rate..secondary..male,
  )


# Prepare for merging

# Remove Code
education <- education_ratio[ , !names(education_ratio) %in% c('Code')]
learning <- learning_years[ , !names(learning_years) %in% c('Code')]
out_school <- out_of_school[ , !names(out_of_school) %in% c('Code')]
gender_gap <- gender_gap_education[ , !names(gender_gap_education) %in% c('Code')]

education$Code <- NULL
learning$Code <- NULL
out_school$Code <- NULL
gender_gap$Code <- NULL

# Find common entities (country) between the first two datasets
common_entities_temp <- intersect(education_ratio$Entity, learning_years$Entity)

# Find common entities across all three datasets
common_entities <- intersect(common_entities_temp, out_of_school$Entity)

# Get all data for common entities and put it there
education <- education[education_ratio$Entity %in% common_entities, ]
learning <- learning[learning_years$Entity %in% common_entities, ]
out_school <- out_school[out_of_school$Entity %in% common_entities, ]
gender_gap <- gender_gap[gender_gap_education$Entity %in% common_entities, ]

# Rename columns
education_ratio <- education_ratio %>%
  rename(
    Country = Entity
  )

learning_years <- learning_years %>%
  rename(
    Country = Entity
  )


out_of_school <- out_of_school %>%
  rename(
    Country = Entity
  )

gender_gap_education <- gender_gap_education %>%
  rename(
    Country = Entity
  )

education <- education %>%
  rename(
    Country = Entity
  )

learning <- learning %>%
  rename(
    Country = Entity
  )


out_school <- out_school %>%
  rename(
    Country = Entity
  )

gender_gap <- gender_gap %>%
  rename(
    Country = Entity
  )


# FOR WORLD DATA
# OWID_WRL
# Remove all countries
w_education <- education_ratio[education_ratio$Country == "World", ]
w_gender_gap <- gender_gap_education[gender_gap_education$Country == "World", ]
w_out_of_school <- out_of_school[out_of_school$Country == "World", ]

# Drop Code columns
w_education$Code <- NULL
w_out_of_school$Code <- NULL
w_gender_gap$Code <- NULL


### POPULATION ###
# We need the years 1950-2020
# We need the age group Population, sum of (5-9, 10-14, 15-19)
nrow(population)
sum(is.na(population))

population <- population %>%
  mutate(Children_Population_5_to_19 = Population.aged.5.to.9.years + 
           Population.aged.10.to.14.years + 
           Population.aged.15.to.19.years)

population <- population %>%
  rename(
    Country = Country.name
  )

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


# Line chart of global access to education
# Base plot
world_df <- world_df %>%
  mutate(Children_Not_Educated = Children_Population_5_to_19 * With_Education / 100)

world_df <- world_df %>%
  mutate(Children_Educated = Children_Population_5_to_19 * Without_Education / 100)

# Create the base plot
global_access_education <- ggplot(world_df, aes(x = Year)) +
  geom_line(aes(y = Children_Not_Educated, group = 1, colour = "Children Not Getting Education")) +
  labs(y = "Population (Millions)", colour = "Indicator") +
  scale_y_continuous(
    name = "Population",
    label = label_number()
  ) +
  scale_x_continuous(breaks = seq(min(world_df$Year, na.rm = TRUE), max(world_df$Year, na.rm = TRUE), by = 10)) 

# Add the educated children
global_access_education <- global_access_education + geom_line(aes(y = Children_Educated, group = 1, colour = "Children Getting Education")) 

# Add the total children population on a secondary y-axis
global_access_education <- global_access_education + geom_line(aes(y = Children_Population_5_to_19, group = 1, colour = "Total Children Population (5-19)"))

# Customize the plot
global_access_education <- global_access_education + theme_minimal() +
  scale_colour_manual(values = c("Children Getting Education" = "blue", "Children Not Getting Education" = "red", "Total Children Population (5-19)" = "black")) +
  ggtitle("Basic Educational Access vs Total Population (Ages 5-19)")

global_access_education


# Correlation between Access to education and learning years
ggplot(merged_2020, aes(x = Learning_Years, y = With_Education, color = World_Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = World_Status)) +
  labs(x = "Learning Years", y = "Educated (%)", title = "Correlation between Learning Years and Education Access by World Status") +
  theme_minimal() +
  scale_color_manual(values = c("Developing" = "blue", "Developed" = "red"))

ggplot(merged_2020, aes(x = Learning_Years, y = Without_Education, color = World_Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = World_Status)) +
  labs(x = "Learning Years", y = "No Education Access (%)", title = "Correlation between Learning Years and No Education Access by World Status") +
  theme_minimal() +
  scale_color_manual(values = c("Developing" = "blue", "Developed" = "red"))


## EXTRA CHARTS ##

# Line chart for access to education (by OECD countries)
education_ratio$Year <- as.numeric(as.character(education_ratio$Year))
education_ratio$With_Education_Share <- as.numeric(education_ratio$With_Education_Share)

# Filter for rows where the Entity is 'OECD'
df_OECD <- education_ratio[grepl("OECD", education_ratio$Country), ]

# Create the line plot
ggplot(df_OECD, aes(x = Year, y = With_Education_Share, group = Country, color = Country)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green", "orange", "brown", "magenta", "cyan", "yellow")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray", colour = "gray")) +
  labs(title = "Ratio of population with basic education by OECD over the years",
       x = "Year",
       y = "Ratio in %",
       color = "Country") +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(min(df_OECD$Year), max(df_OECD$Year), by = 20))


# Line graph
# Turn to numeric
gender_gap_education$Tertiary_Education_Female <- as.numeric(gender_gap_education$Tertiary_Education_Female)
gender_gap_education$Tertiary_Education_Male <- as.numeric(gender_gap_education$Tertiary_Education_Male)

selected_entities <- c("United States", "Brazil", "Ireland", "Afghanistan", "Philippines", "India")
gender_gap_filtered <- gender_gap_education %>% 
  filter(Country %in% selected_entities) %>%
  filter(!is.na(Tertiary_Education_Female) & !is.na(Tertiary_Education_Male) & !is.na(Year))

# Find max value to set the limit
max_value_female <- max(gender_gap_filtered$Tertiary_Education_Female, na.rm = TRUE)  
max_value_male <- max(gender_gap_filtered$Tertiary_Education_Male, na.rm = TRUE)  

print(max_value_female)
print(max_value_male)

# Create the line plot
college_female <- ggplot(gender_gap_filtered, aes(x = Year, y = Tertiary_Education_Female, color = Country)) +
  geom_line() +
  labs(title = "Percentage of Females With College Education",
       x = "Year",
       y = "Tertiary Education Female (%)",
       color = "Entity") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(gender_gap_filtered$Year), max(gender_gap_filtered$Year), by = 20)) +
  scale_y_continuous(limits = c(0, max_value_female), breaks = seq(0, 125, by = 25))

# Create the line plot
college_male <- ggplot(gender_gap_filtered, aes(x = Year, y = Tertiary_Education_Male, color = Country)) +
  geom_line() +
  labs(title = "Percentage of Males With College Education",
       x = "Year",
       y = "Tertiary Education Male (%)",
       color = "Entity") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(gender_gap_filtered$Year), max(gender_gap_filtered$Year), by = 20)) +
  scale_y_continuous(limits = c(0, max_value_female), breaks = seq(0, 100, by = 25))

college_female
college_male


filtered_merged <- merged_df_2 %>%
  filter(
    Year %in% c(2000, 2009, 2018)
  )

# Create side-by-side bar chart
ggplot(filtered_merged, aes(x = factor(Year), 
                            y = Dropped_Out_HS_Male + Dropped_Out_HS_Female, 
                            fill = World_Status, 
                            group = interaction(World_Status, Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "High School Dropouts by Development Status and Year",
       x = "Year",
       y = "Total Number of Dropouts",
       fill = "Development Status") +
  scale_y_continuous(
    name = "Population",
    breaks = seq(0, max(filtered_merged$Dropped_Out_HS_Male + filtered_merged$Dropped_Out_HS_Female, na.rm = TRUE), by = 1000000),
    labels = label_number()  # This will format the numbers in a human-readable way
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Developed" = "blue", "Developing" = "red"))

# Find the maximum value of Learning_Years
merged_df_2020 <- merged_df %>%
  filter(Year %in% c(2020))

max_learning_years <- max(merged_df_2020$Learning_Years, na.rm = TRUE)

# Output the maximum learning years to use in the ggplot scale_x_continuous function
print(max_learning_years)

histogram <- ggplot(merged_df_2020, aes(x = Learning_Years, fill = World_Status)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Duration of Learning Years",
       x = "Learning Years",
       y = "Count of Countries",
       fill = "World Status (IMF)") +
  theme_minimal() +
  scale_fill_manual(values = c("Developing" = "lightblue", "Developed" = "lightgreen")) +
  scale_x_continuous(limits = c(0, max_learning_years), breaks = seq(0, max_learning_years, by = 1))  # Set x-axis breaks to go by 1

# Display the histogram
print(histogram)


# Create the scatter plot
ggplot(merged_2020, aes(x = Tertiary_Education_Male, y = Tertiary_Education_Female, color = World_Status)) +
  geom_point() +  
  labs(title = "College/University Education by Gender",
       x = "Tertiary Education - Female (%)",
       y = "Tertiary Education - Male (%)",
       color = "World Status (IMF)") +
  scale_color_manual(values = c("Developed" = "blue", "Developing" = "red")) +  # Set custom colors
  theme_minimal() 


filtered_df_2 <- merged_df_2 %>% 
  filter(Country != "India")


ggplot(filtered_df_2, aes(x = Dropped_Out_HS_Male + Dropped_Out_HS_Female, 
                          y = Children_Population_5_to_19, 
                          color = World_Status)) +
  geom_point() +
  labs(title = "Relationship Between Dropout HS Rates and Children Population",
       x = "Total Number of Dropouts",
       y = "Children Population Ages 5 to 19",
       color = "Development Status") +
  scale_y_continuous(
    name = "Population",
    label = label_number()
  ) +
  scale_x_continuous(
    name = "Total Number Of Dropouts",
    label = label_number()
  ) +
  theme_minimal()

summary(merged_df_2)

##### DASHBOARD EXAMPLE #####
# Create the line plot (Filtered)
college_female_filtered <- ggplot(gender_gap_filtered, aes(x = Year, y = Tertiary_Education_Female, color = Country)) +
  geom_line() +
  labs(title = "Percentage of Females With College Education",
       x = "Year",
       y = "Tertiary Education Female (%)",
       color = "Entity") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(gender_gap_filtered$Year), max(gender_gap_filtered$Year), by = 50)) +
  scale_y_continuous(limits = c(0, max_value_female), breaks = seq(0, 125, by = 25))

# Create the line plot
college_male_filtered <- ggplot(gender_gap_filtered, aes(x = Year, y = Tertiary_Education_Male, color = Country)) +
  geom_line() +
  labs(title = "Percentage of Males With College Education",
       x = "Year",
       y = "Tertiary Education Male (%)",
       color = "Entity") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(gender_gap_filtered$Year), max(gender_gap_filtered$Year), by = 50)) +
  scale_y_continuous(limits = c(0, max_value_female), breaks = seq(0, 100, by = 25))


combined_plots <- plot_grid(college_female_filtered, college_male_filtered, ncol=2, nrow=1)

combined_plots

combined_plots_2 <- plot_grid(combined_plots, global_access_education, ncol=1, nrow=2)
combined_plots_2