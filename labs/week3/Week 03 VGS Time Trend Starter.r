library(tidyverse)
library(tidyr)
library(dplyr)

mydata = file.path("C:","Users","twila","OneDrive","COMPSCI","Year 4","SEM 2","Visualising Data","labs","week3") 

datapath <- file.path(mydata,'vgsales.csv')
df <- read.csv(datapath)
head(df)


# Check to see if Global_Sales is a sum of the other sales using 'all.equal'.
# Calculate the sum of total sales
total_sales_sum <- rowSums(df[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")])

# Check if the sum of total sales is equal to the Global_Sales
are_equal <- all.equal(total_sales_sum, df$Global_Sales)

# Print the result
print(are_equal)

# Create new DF
df2 <- data.frame(
  "Year" = df$Year[df$Year != "N/A"],
  "Total_Sales" = df$Global_Sales[df$Year != "N/A"],
  "Name" =  df$Name[df$Year != "N/A"],
  "Genre" = df$Genre[df$Year != "N/A"]
)

# Print the new dataset
print(df2)

# Group by Year and Genre, and calculate the sum of Total_Sales
summarized_df <- df2 %>%
  group_by(Year, Genre) %>%
  summarise(Total_Sales = sum(Total_Sales))

# Create a new DataFrame with Year, Total Sales, and Genre columns
df3 <- data.frame(
  Year = summarized_df$Year,
  Total_Sales = summarized_df$Total_Sales,
  Genre = summarized_df$Genre
)

df3$Year <- as.integer(df3$Year)

# Print the new DataFrame
print(df3)


# Plot a bar chart
ggplot(df3, aes(x = Year, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Sales by Decade",
       x = "Year",
       y = "Total Sales") +
  scale_x_discrete(labels = function(x) as.integer(x)) +  # Ensure x-axis labels are integers
  theme_minimal()

# Stacked bar chart
ggplot(df2, aes(x = Year, y = Total_Sales, fill = Genre)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Total Sales", fill = "Genre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Global Sale by Each Year")

# Scatter Plot
ggplot(df3, aes(x = Year, y = Total_Sales)) +
  geom_point(color = "red") +  # Change point color to red
  labs(title = "Scatter Plot of Total Global Sales Each Year",
       x = "Year",
       y = "Total Global Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Categorical Variable
ggplot(df3, aes(x = Year, y = Total_Sales, color = Genre)) +
  geom_point() +  
  labs(title = "Scatter Plot of Total Global Sales Each Year",
       x = "Year",
       y = "Total Global Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Single Value
ggplot(df3, aes(x = Year, y = Total_Sales)) +
  geom_point() +  
  geom_line() + 
  labs(title = "Line Plot of Total Global Sales Each Year",
       x = "Year",
       y = "Total Global Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Categorical Values
ggplot(df3, aes(x = Year, y = Total_Sales, color = Genre)) +
  geom_point() +  
  geom_line() + 
  labs(title = "Lines Plot of Total Global Sales Each Year",
       x = "Year",
       y = "Total Global Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Evaluate
# I chose the video games dataset because I love video games. #
# In summary, there was a rise of sales in video games around the
# early 2000s, and it peaked in the years of 2010.
# Because of the evolution of technology and tools, developers have now
# more opportunity to make more games and be able to sell them to consumers