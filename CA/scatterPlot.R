df_2020 <- merged_df %>% filter(Year == 2020)
summary(df_2020)

# Create the scatter plot
ggplot(df_2020, aes(x = Tertiary_Education_Female, y = Tertiary_Education_Male, color = World_Status)) +
  geom_point() +  # Use points for the scatter plot
  labs(title = "Tertiary Education by Gender in 2020",
       x = "Tertiary Education - Female (%)",
       y = "Tertiary Education - Male (%)",
       color = "World Status") +
  scale_color_manual(values = c("Developed" = "blue", "Developing" = "red")) +  # Set custom colors
  theme_minimal()  # Use a minimal theme