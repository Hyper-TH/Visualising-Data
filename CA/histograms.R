df_2020_developed <- merged_df %>%
  filter(World_Status == "Developed", Year == 2020) %>%
  # Sum up the Tertiary_Education_Female and Tertiary_Education_Male values for the visualization
  summarize(Female = sum(Tertiary_Education_Female, na.rm = TRUE),
            Male = sum(Tertiary_Education_Male, na.rm = TRUE))

df_2020_developing <- merged_df %>%
  filter(World_Status == "Developing", Year == 2020) %>%
  # Sum up the Tertiary_Education_Female and Tertiary_Education_Male values for the visualization
  summarize(Female = sum(Tertiary_Education_Female, na.rm = TRUE),
            Male = sum(Tertiary_Education_Male, na.rm = TRUE))

# Convert this data to a long format for easier plotting with ggplot2
df_long1 <- tidyr::pivot_longer(df_2020_developed, 
                               cols = c(Female, Male),
                               names_to = "Gender",
                               values_to = "Sum_Education")


# Convert this data to a long format for easier plotting with ggplot2
df_long2 <- tidyr::pivot_longer(df_2020_developing, 
                               cols = c(Female, Male),
                               names_to = "Gender",
                               values_to = "Sum_Education")

ggplot(df_long1, aes(x = Gender, y = Sum_Education, fill = Gender)) +
  geom_bar(stat = "identity") +  # Use geom_bar with identity stat to plot sums
  labs(title = "Total Tertiary Education by Gender for Developed Countries in 2020",
       x = "Gender",
       y = "Total students") +
  scale_y_continuous(limits = c(0, 4000)) + 
  theme_minimal()

ggplot(df_long2, aes(x = Gender, y = Sum_Education, fill = Gender)) +
  geom_bar(stat = "identity") +  # Use geom_bar with identity stat to plot sums
  labs(title = "Total Tertiary Education by Gender for Developing Countries in 2020",
       x = "Gender",
       y = "Total students") +
  scale_y_continuous(limits = c(0, 4000)) +  
  theme_minimal()