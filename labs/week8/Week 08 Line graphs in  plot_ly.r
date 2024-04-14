######## List Packages #########

# List Of Packages - Add or remove as needed for future labs no need to change any other code
required_packages <- c("tidyverse", "knitr", "DT", "plotly", "ggvis", "manipulateWidget", "cowplot")

#Start up messages suppression
suppressPackageStartupMessages({
  
  # Check if the package you need is in the list and install if so
  for (package in required_packages) {
    # Checks if the package you want is already installed or not, if true then dont return any annoying messages
    if (!requireNamespace(package, quietly = TRUE)) {
      # Catch any annoying error or warning messages
      tryCatch({
        # Generic code we used to use for loading packages
        install.packages(package)
        library(package, character.only = TRUE)
      }, error = function(e) {
        #part of the try catch 
        cat("Error installing or loading package:", package, "\n")
      })
    } else {
      library(package, character.only = TRUE)
    }
  }
  
})
# Returns only this line of code once all are installed
cat("All required packages are loaded.\n")

mydata = file.path("C:","Users","twila","OneDrive","COMPSCI","Year 4","SEM 2","Visualising Data","labs","week8") 
datapath <- file.path(mydata,'Olympics.csv')
df <- read.csv(datapath)
head(df)
df

################ Filter the data for the year 1964 for volleyball#############
# Filter the data for the year 1964 for sport Volleyball
df_1964 <- subset(df, Year == 1964)

# Filter out rows with missing data in 'Sport' and 'Sex' columns
df_filtered <- df_1964[complete.cases(df_1964[c("Sport", "Sex")]), ]

# Filter the data for the specified sports
specified_sports <- c("Volleyball")
df_specified_sports <- subset(df_filtered, Sport %in% specified_sports)

# Aggregate the data to count the number of participants by Sport and Sex
participant_counts <- aggregate(ID ~ Sport + Sex, data = df_specified_sports, FUN = length)

# Create the bar plot
volleyball1964 <- ggplot(participant_counts, aes(x = Sport, y = ID, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Participation in Volleyball by Gender (Year: 1964)",
       x = "Sport",
       y = "Number of Participants",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################ Filter the data for the year 1920 for archery#############
# Filter the data for the year 1920 for sport Archery
df_1904 <- subset(df, Year == 1904)

# Filter out rows with missing data in 'Sport' and 'Sex' columns
df_filtered <- df_1904[complete.cases(df_1904[c("Sport", "Sex")]), ]

# Filter the data for the specified sports
specified_sports <- c("Archery")
df_specified_sports <- subset(df_filtered, Sport %in% specified_sports)

# Aggregate the data to count the number of participants by Sport and Sex
participant_counts <- aggregate(ID ~ Sport + Sex, data = df_specified_sports, FUN = length)

# Create the bar plot
archery1920 <- ggplot(participant_counts, aes(x = Sport, y = ID, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Participation in Archery by Gender (Year: 1904)",
       x = "Sport",
       y = "Number of Participants",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################ Filter the data for the year 1912 for swimming #############
# Filter the data for the year 1912 for sport swimming
df_1912 <- subset(df, Year == 1912)

# Filter out rows with missing data in 'Sport' and 'Sex' columns
df_filtered <- df_1912[complete.cases(df_1912[c("Sport", "Sex")]), ]

# Filter the data for the specified sports
specified_sports <- c("Swimming")
df_specified_sports <- subset(df_filtered, Sport %in% specified_sports)

# Aggregate the data to count the number of participants by Sport and Sex
participant_counts <- aggregate(ID ~ Sport + Sex, data = df_specified_sports, FUN = length)

# Create the bar plot
swimming1912 <- ggplot(participant_counts, aes(x = Sport, y = ID, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Participation in Swimming by Gender (Year: 1912)",
       x = "Sport",
       y = "Number of Participants",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



################ Filter the data for the year 2016 for mentioned sports #############
# Filter the data for the year 1964 for sport Volleyball
df_2016 <- subset(df, Year == 2016)

# Filter out rows with missing data in 'Sport' and 'Sex' columns
df_filtered <- df_2016[complete.cases(df_2016[c("Sport", "Sex")]), ]

# Filter the data for the specified sports
specified_sports <- c("Archery", "Volleyball", "Swimming")
df_specified_sports <- subset(df_filtered, Sport %in% specified_sports)

# Aggregate the data to count the number of participants by Sport and Sex
participant_counts <- aggregate(ID ~ Sport + Sex, data = df_specified_sports, FUN = length)

# Create the bar plot
sports2016 <- ggplot(participant_counts, aes(x = Sport, y = ID, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Participation in Archery, Volleyball, Swimming by Gender (Year: 2016)",
       x = "Sport",
       y = "Number of Participants",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sports2016


################ Line graph of percentage of female participants in the olympics over time #############
# Filter out rows with missing data in 'Sex' and 'Year' columns
df_filtered <- df[complete.cases(df[c("Sex", "Year")]), ]

# Calculate the total number of participants (both male and female) for each year
total_participants <- aggregate(ID ~ Year, data = df_filtered, FUN = length)

# Calculate the total number of female participants for each year
female_participants <- aggregate(ID ~ Year + Sex, data = df_filtered[df_filtered$Sex == "F", ], FUN = length)

# Calculate the percentage of female participants for each year
female_percentage <- merge(total_participants, female_participants, by = "Year", suffixes = c("_total", "_female"))
female_percentage$Percentage_Female <- (female_percentage$ID_female / female_percentage$ID_total) * 100

# Create the line plot
femalePercentagePlot <- ggplot(female_percentage, aes(x = Year, y = Percentage_Female)) +
  geom_line() +
  geom_point() +
  labs(title = "Percentage of Female Participants in the Olympics Over Time",
       x = "Year",
       y = "Percentage of Female Participants") +
  theme_minimal()

################ Line graph of percentage of male participants in the olympics over time #############
# Filter out rows with missing data in 'Sex' and 'Year' columns
df_filtered <- df[complete.cases(df[c("Sex", "Year")]), ]

# Calculate the total number of participants (both male and female) for each year
total_participants <- aggregate(ID ~ Year, data = df_filtered, FUN = length)

# Calculate the total number of female participants for each year
male_participants <- aggregate(ID ~ Year + Sex, data = df_filtered[df_filtered$Sex == "M", ], FUN = length)

# Calculate the percentage of female participants for each year
male_percentage <- merge(total_participants, male_participants, by = "Year", suffixes = c("_total", "_male"))
male_percentage$Percentage_Male <- (male_percentage$ID_male / male_percentage$ID_total) * 100

# Create the line plot
malePercentagePlot <- ggplot(male_percentage, aes(x = Year, y = Percentage_Male)) +
  geom_line() +
  geom_point() +
  labs(title = "Percentage of Male Participants in the Olympics Over Time",
       x = "Year",
       y = "Percentage of Male Participants") +
  theme_minimal()

##### DASHBOARD #####
combined_plots <- plot_grid(volleyball1964, swimming1912, archery1920, sports2016, ncol=2, nrow=2)

combined_plots

combined_plots2 <- plot_grid(femalePercentagePlot, malePercentagePlot, nrow=2, align='h')
combined_plots2