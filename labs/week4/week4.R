shhh <- suppressPackageStartupMessages
shhh(library(tidyverse))

library(zoo)
library(gganimate)
library(gifski)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(transformr)
library(babynames)
library(streamgraph)

install.packages("Streamgraph")

devtools::install_github("hrbrmstr/streamgraph",force = TRUE)

update.packages()

mydata = file.path('C:', 'Users', 'twila', 'OneDrive', 'COMPSCI', 'Year 4', 'SEM 2', 'Visualising Data', 'labs', 'week4')

datapath <- file.path(mydata,'mly532.csv')
df <- read.csv(datapath, skip=19)
head(df)

# 3 Check out the data

## Structure
str(df)

## Summary
summary(df)

## How many unique values are there for each variable
df%>%summarise_all(n_distinct)

## Change null values to N/A
df <- mutate_all(df, ~ replace(., is.na(.), 'N/A'))

# Step 2: Combine 'year' and 'month' into 'YearMonth'
df$YearMonth <- with(df, as.yearmon(paste(year, month), "%Y %m"))

# Step 3: Optionally convert to Date (first day of the month)
df$YearMonth <- as.Date(df$YearMonth)


#### #### #### 
max(df$maxtp)
min(df$maxtp)


breaks <- c(-Inf, 50, 150, Inf)
labels <- c("Short", "Medium", "Long")

# Tidy up data
df$date <- make_date(df$year, df$month, 1)
df$ym <- as.yearmon(df$date, "%Y %m")
df$maxtp <- as.numeric(df$maxtp)

df$maxAirTemp <- cut(df$maxtp, breaks = breaks, labels = labels, include.lowest = TRUE)

#YearMon, year, month and maxtp and gather the avg meant monthly
df2 <- df %>%
  group_by(ym, year, month, maxAirTemp) %>%
  summarise_at(vars(meant), list(monthly_meant=mean))

head(df2)

## CREATE LINE CHART
# Create a line chart
line_chart <- ggplot(data = df2, aes(x = ym, y = monthly_meant)) +
  geom_line() +
  labs(x = "Year and Month", y = "Mean Air Temperature (°C)", title = "Mean Air Temperature Over Time")

# Display the line chart
print(line_chart)

# Create a point chart
point_chart <- ggplot(data = df2, aes(x = ym, y = monthly_meant)) +
  geom_point() +
  labs(x = "Year and Month", y = "Mean Air Temperature (°C)", title = "Mean Air Temperature Over Time")

# Display the point chart
print(point_chart)

# Enhance with two other dimensions
enhanced_point_chart <- ggplot(data = df, aes(x = YearMonth, y = meant, size = maxAirTemp, color = maxtp)) +
  geom_point(alpha = 0.7) +  # Set transparency to see overlapping points
  labs(x = "Year and Month", y = "Mean Air Temperature (°C)",
       size = "Precipitation Amount (mm)", color = "Max Air Temperature (°C)",
       title = "Enhanced Mean Air Temperature Over Time") +
  theme(legend.position = "bottom")  # Move legend to the bottom

# Display the enhanced point chart
print(enhanced_point_chart)

# Point chart time

enhanced_point_chart_time <- ggplot(data = df, aes(x = YearMonth, y = meant, size = maxAirTemp, color = maxtp)) +
  geom_point(alpha = 0.7) +
  labs(x = "Year and Month", y = "Mean Air Temperature (°C)", 
       size = "Precipitation Amount (mm)", color = "Max Air Temperature (°C)", 
       title = "Mean Air Temperature Over Time: {frame_time}") +
  theme(legend.position = "bottom") +
  transition_time(YearMonth)

print(enhanced_point_chart_time)


anim_save(file.path('C:/Users/twila/OneDrive/COMPSCI/Year 4/SEM 2/Visualising Data/labs/week4/gifs', '', "enhanced_point_chart_time"), animation=last_animation())


# Point Chart Reveal

enhanced_point_chart_reveal <- ggplot(data = df, aes(x = YearMonth, y = meant, size = maxAirTemp, color = maxtp)) +
  geom_point(alpha = 0.7) +
  labs(x = "Year and Month", y = "Mean Air Temperature (°C)", 
       size = "Precipitation Amount (mm)", color = "Max Air Temperature (°C)", 
       title = "Mean Air Temperature Over Time: {frame_along}") +
  theme(legend.position = "bottom") +
  transition_reveal(YearMonth)

print(enhanced_point_chart_reveal)

anim_save(file.path('C:/Users/twila/OneDrive/COMPSCI/Year 4/SEM 2/Visualising Data/labs/week4/gifs', '', "enhanced_point_chart_reveal"), animation=last_animation())


#
options(repr.plot.width=16)

df2$ym <- as.POSIXct(df2$ym)
p <- ggplot(df2,
            aes(ym, monthly_meant, group = maxAirTemp, color = maxAirTemp)
) + geom_line()

p + transition_reveal(ym)


anim_save(file.path('C:/Users/twila/OneDrive/COMPSCI/Year 4/SEM 2/Visualising Data/labs/week4/gifs', '', "maxAirTemp1"), animation=last_animation())

# 
options(repr.plot.width=12, repr.plot.height=6)
myPlot <- ggplot(df2, aes(
  x = month,
  y = monthly_meant,
  color = maxAirTemp,
  size=2)) + theme_bw() +
  geom_point(aes(size=2)) +
  labs(subtitle = 'Year: {frame_time}',
       title = 'Monthly Max Air Temperature',
       x = 'Month', y = 'Meant', fill='Air Temperature')+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
myPlot + transition_time(year)

anim_save(file.path('C:/Users/twila/OneDrive/COMPSCI/Year 4/SEM 2/Visualising Data/labs/week4/gifs', '', "maxAirTemp2"), animation=last_animation())


# STREAMGRAPH
babynames %>%
  filter(grepl("^Ch", name)) %>%
  group_by(year, name) %>%
  tally(wt=n) %>%
  streamgraph("name", "n", "year")

###
mydata = file.path('C:', 'Users', 'twila', 'OneDrive', 'COMPSCI', 'Year 4', 'SEM 2', 'Visualising Data', 'labs', 'week4')

datapath <- file.path(mydata,'vaccine-preventable-disease-cases-by-county-and-year-2.csv')
df3 <- read.csv(datapath)
head(df3)

df3 %>%
  filter(grepl("^Ch", name)) %>%
  group_by(year, name) %>%
  tally(wt=n) %>%
  streamgraph("name", "n", "year")

