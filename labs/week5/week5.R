shhh <- suppressPackageStartupMessages
shhh(library(tidyverse))

library(ggthemes)
library(dplyr)
library(ggplot2)
library(transformr)
library(scales)
library(cowplot)

update.packages()

mydata = file.path("C:","Users","twila","OneDrive","COMPSCI","Year 4","SEM 2","Visualising Data","labs","week5") 

datapath <- file.path(mydata,'SCGOP2016.csv')
df <- read.csv(datapath)
head(df)
df

# labs
labs <- labs(title = 'Distribution plot', 
             subtitle='Total Votes for Ted Cruz', x = 'Votes')
theme <- theme_bw()


# Basic histogram 
histogram <- ggplot(df, aes(x=Ted.Cruz)) + 
  geom_histogram(bins=10) + labs + theme()

histogram

# Histogram 2 where NA values are ignored
histogram2 <- ggplot(df, aes(x=Ted.Cruz, na.rm = TRUE)) + 
  geom_histogram(bins=25) + labs + theme + 
  labs(x='Votes')

histogram2

# Histogram 3 NOT WORKING
histogram3 <- ggplot(df, aes(x=Ted.Cruz)) + 
  geom_histogram(bins=20, fill="Yellow", color="Blue") + 
  geom_freqpoly(binwidth=1, color="Red") + labs + theme + 
labs(x='Votes')

histogram3

# Pivot
pdf <- pivot_longer(df, cols = -County, names_to = "Candidate", values_to = "Votes")
pdf <- pdf %>%
  filter(Candidate != "Total") # Remove rows with Total as the candidate (SKEW VARIABLE)

# Box plot 
# Convert Candidate to a factor for proper box plot representation
pdf$Candidate <- as.factor(pdf$Candidate)

# Get top 3 candidates
top3 <- pdf %>%
  group_by(County) %>%
  arrange(desc(Votes)) %>%
  slice_max(order_by = Votes, n = 3) %>%
  ungroup()

top3
pdf

# BOX PLOT
ted_cruz_data <- pdf %>%
  filter(Candidate == "Ted.Cruz")

# Create a box plot for Ted Cruz
boxplot <- ggplot(ted_cruz_data, aes(x = Candidate, y = Votes)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Vote Distribution for Ted Cruz", x = "Candidate", y = "Votes") +
  theme_minimal()

boxplot2 <- ggplot(pdf, aes(x = Candidate, y = Votes)) +
  geom_boxplot(fill = "slateblue", alpha = 0.2) +
  theme_bw()

boxplot
boxplot2

# Check and Generate
lst <- sort(pdf$Votes)
median <- median(lst)
qs <- quantile(lst)
IQR <- IQR(lst)

median
qs
IQR

calc <- IQR * 1.5

# Max value is highest value inside the 3rd quartile, plus calc
maxinq2 <- max(lst[lst<qs[4]])
max <- maxinq2 + calc
max

# Min value is highest value inside 1st quartile, minus calc
maxinq1 <- max(lst[lst<qs[2]])
min <- maxinq1 - calc
min

theme_set(theme_classic())


# Density plot
density <- ggplot(pdf, aes(x = Votes, fill = Candidate)) + 
  geom_density(alpha = 0.75) + 
  labs(x = "Votes", y = "Density", title = "Vote Density per Candidate") +
  theme_minimal() +
  theme(legend.position = "right")

# Density plot with improved scale
density2 <- ggplot(pdf, aes(x = Votes, fill = Candidate)) + 
  geom_density(alpha = 0.5, adjust = 0.5) +  # adjust to control the smoothness
  scale_x_continuous(limits = c(0, NA), labels = scales::comma) +  # NA to use default max limit
  scale_y_continuous(labels = scales::comma_format(scale = 1e-4)) +  # scale the y-axis values for better readability
  labs(x = "Votes", y = "Density", title = "Vote Density per Candidate") +
  theme_minimal() +
  theme(legend.position = "right")


# Density plot with log scale
density3 <- ggplot(pdf, aes(x = Votes, fill = Candidate)) + 
  geom_density(alpha = 0.5, adjust = 0.5) +  # Use adjust to control the smoothness
  scale_x_log10(labels = comma) +  # Apply log scale to x-axis
  labs(x = "Votes (log scale)", y = "Density", title = "Vote Density per Candidate (Log Scale)") +
  theme_minimal() +
  theme(legend.position = "right")


# Density for top 3 candidates
density4 <- ggplot(top3, aes(x = Votes, fill = Candidate)) + 
  geom_density(alpha = 0.5, adjust = 0.5) +  # Use adjust to control the smoothness
  scale_x_log10(labels = comma) +  # Apply log scale to x-axis
  labs(x = "Votes (log scale)", y = "Density", title = "Vote Density per Candidate (Log Scale)") +
  theme_minimal() +
  theme(legend.position = "right")


density
density2
density3
density4

# Multiple Box Plot 
boxplot2

# Diverging BarChart
# Calculate the average votes for the county
pdf <- pdf %>%
  group_by(County) %>%
  mutate(Avg_Votes = mean(Votes))

# Calculate the divergence from the average
pdf$Divergence = pdf$Votes - pdf$Avg_Votes

# Create the diverging bar chart
diverging <- ggplot(pdf, aes(x = reorder(Candidate, Divergence), y = Divergence, fill = Divergence)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the coordinates to make it horizontal
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  labs(x = "Candidate", y = "Divergence from Average Votes", title = "Divergence of Candidate Votes from County Average") +
  theme_minimal()

diverging

# Side by Side bar chart
# Create a side-by-side bar chart
SideBySide <- ggplot(pdf, aes(x = County, y = Votes, fill = Candidate)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(x = "County", y = "Votes", title = "Votes by Candidate and County") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis text for readability

# Filtered for 2 candidates only
pdf2 <- pdf %>%
  filter(Candidate %in% c("Donald.J.Trump", "Jeb.Bush"))

SideBySide2 <- ggplot(pdf2, aes(x = County, y = Votes, fill = Candidate)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(x = "County", y = "Votes", title = "Votes for Donald J. Trump and Jeb Bush by County") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis text for readability

# Filtered for 2 counties only
pdf3 <- pdf2 %>%
  filter(County %in% c("Horry", "Greenville"))

SideBySide3 <- ggplot(pdf3, aes(x = County, y = Votes, fill = Candidate)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(x = "County", y = "Votes", title = "Votes for Donald J. Trump and Jeb Bush by County") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis text for readability


SideBySide
SideBySide2
SideBySide3


# GESTALT (on Density Chart)
# Similarity: Uses different shades to differentiate Candidates
# Proximity: Group elements of the same category (i.e., counties)
# Closure: Fully colored shapes
# Continuity: Smooth graphical representation
# Figure/Ground: Filled up shapes
# Symmetry and Order: Emphasized spread of data

# ACCENT (on Divergence Chart)
# Apprehension: Topic immediately recognizable through the legends and colours
# Clarity: Simple labeling and colors were used (minimalism)
# Consistency: No consistent theme (different themes were used)
# Efficiency: Some charts were subsets of the data 
# Necessity: Some charts included all counties/candidates, some only included 2-3 candidates
# Truthfulness: Clean data with no modification on raw data