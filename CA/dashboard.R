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