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
  ggtitle("Educational Access vs Total Population (Ages 5-19)")

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
df_OECD <- education_ratio[grepl("OECD", education_ratio$Entity), ]

# Create the line plot
ggplot(df_OECD, aes(x = Year, y = With_Education_Share, group = Entity, color = Entity)) +
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
 scale_x_continuous(breaks = seq(min(df_OECD$Year), max(df_OECD$Year), by = 10))


# Line graph
# Turn to numeric
gender_gap_education$Tertiary_Education_Female <- as.numeric(gender_gap_education$Tertiary_Education_Female)
gender_gap_education$Tertiary_Education_Male <- as.numeric(gender_gap_education$Tertiary_Education_Male)

selected_entities <- c("United States", "Brazil", "Ireland", "Afghanistan", "Philippines", "Nigeria", "India")
gender_gap_filtered <- gender_gap_education %>% 
  filter(Entity %in% selected_entities) %>%
  filter(!is.na(Tertiary_Education_Female) & !is.na(Tertiary_Education_Male) & !is.na(Year))

# Find max value to set the limit
max_value_female <- max(gender_gap_filtered$Tertiary_Education_Female, na.rm = TRUE)  
max_value_male <- max(gender_gap_filtered$Tertiary_Education_Male, na.rm = TRUE)  

print(max_value_female)
print(max_value_male)

# Create the line plot
ggplot(gender_gap_filtered, aes(x = Year, y = Tertiary_Education_Female, color = Entity)) +
  geom_line() +
  labs(title = "Tertiary Education Female by Country Over Years",
       x = "Year",
       y = "Tertiary Education Female (%)",
       color = "Entity") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(gender_gap_filtered$Year), max(gender_gap_filtered$Year), by = 20)) +
  scale_y_continuous(limits = c(0, max_value_female), breaks = seq(0, 125, by = 25))

# Create the line plot
ggplot(gender_gap_filtered, aes(x = Year, y = Tertiary_Education_Male, color = Entity)) +
  geom_line() +
  labs(title = "Tertiary Education Male by Country Over Years",
       x = "Year",
       y = "Tertiary Education Female (%)",
       color = "Entity") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(gender_gap_filtered$Year), max(gender_gap_filtered$Year), by = 20)) +
  scale_y_continuous(limits = c(0, max_value_female), breaks = seq(0, 100, by = 25))


