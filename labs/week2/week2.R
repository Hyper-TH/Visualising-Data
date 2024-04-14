library(tidyverse)

# C:\Users\twila\OneDrive\COMPSCI\Year 4\SEM 2\Visualising Data\labs
mydata = file.path("C:","Users","twila","OneDrive","COMPSCI","Year 4","SEM 2","Visualising Data","labs","week2")
datapath = file.path(mydata,"IRCountyPop.csv")

df <- read.csv(datapath)


# DROP SUBSTRING POP
names(df) <- gsub("pop", "", names(df))

ncol(df)

## START CHATGPT
# Calculate the average population for each county
# na.rm means REMOVE NULL VALUES
df$average_population <- rowMeans(df[, 2:16], na.rm = TRUE)

# Order the data by average population in descending order
sdf <- df[order(-df$average_population), ]

# Select the top 6 counties
top_6_counties <- head(sdf, 6)

top_6_counties

# PIVOT
# Pivot the data based on county
# -county means pivolt all except for counties
pdf <- pivot_longer(top_6_counties, cols = -county, names_to = "year", values_to = "population")
pdf

# Create line chart for Dublin
# Filter data for Dublin county
ddf <- subset(pdf, county == "Dublin", year != "average_population")

# Convert 'year' to numeric for proper ordering
ddf$year <- as.numeric(ddf$year)

# Sort the data frame by 'year'
ddf <- ddf[order(ddf$year), ]

# Create the plot
ggplot(ddf, aes(x = year, y = population)) +
  geom_line() + 
  geom_point() + 
  labs(title = "Population Over Time for Dublin",
       x = "Year",
       y = "Population",
  ) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

# Create lines chart
pdf <- subset(pdf, year != "average_population")
ggplot(pdf, aes(x = year, y = population, group = county, color = county)) +
  geom_line() +
  geom_point() +  # Add points for each data point
  labs(title = "Population Over Time for Top 6 Average per Year",
       subset = "During and after the Famine",
       x = "Year",
       y = "Population",
       color = "County"
  ) +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()


# FOCUS ON DUBLIN AND ANTRIM
# Create lines chart
pdf <- subset(pdf, year != "average_population")

# Irish Famine 1845:1852
# Economic Boom Ireland 1995:2007
# 1960s sudden rise due to economic boom (Celtic)

ggplot(pdf, aes(x = year, y = population, group = county, color = county)) +
  geom_line() +
  geom_point() +  # Add points for each data point
  labs(title = "Population Over Time During and After the Famine",
       subtitle = "Dublin and Antrim Population Rise",
         x = "Year",
         y = "Population",
         color = "County"
       ) +
  scale_color_manual(values = c("Dublin" = "blue", "Antrim" = "red", "Other" = "lightgray")) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()
## END CHATGPT
