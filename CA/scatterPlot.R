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