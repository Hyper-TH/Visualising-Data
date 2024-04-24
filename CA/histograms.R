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

