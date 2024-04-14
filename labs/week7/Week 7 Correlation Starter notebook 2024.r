######## List Packages #########

# List Of Packages - Add or remove as needed for future labs no need to change any other code
required_packages <- c("tmap", "leaflet", "leaflet.extras", "scales", "htmlwidgets", "sf", 
                       "dplyr","geojsonio","shiny","RColorBrewer","htmltools","profvis", "corrplot",
                       "GGally", "ggExtra", "ggpubr")

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


mydata = file.path("C:/Users/twila/OneDrive/COMPSCI/Year 4/SEM 2/Visualising Data/labs/week7")
datapath <- file.path(mydata, 'Pokemon.csv')
df <- read.csv(datapath)

str(df)

##### 1&2 PREPARE DATA AND START PLOTTING #####
df_subset <- df %>% select(-X., -Total, -Name, -Type.1, -Type.2, -Legendary)
df_subset <- df_subset[df_subset$Generation %in% c(1, 2), ]
df_subset <- df_subset %>% select(-Generation)

## SPLOM ##
options(repr.plot.width=12, repr.plot.height=12)
plot(df_subset)

options(repr.plot.width=8, repr.plot.height=8)
plot(df_subset)

options(repr.plot.width=4, repr.plot.height=4)
plot(df_subset)

ggpairs(df_subset)

## CORRELATION PLOT ##
cr = cor(df_subset)
cr

corrplot(cr, method = 'number')
corrplot(cr, method = 'color', order = 'alphabet')
corrplot.mixed(cr, lower = 'shade', upper = 'pie', order ='hclust')


##### 3. INVESTIGATE SPECIFIC COLUMNS #####


## Visualizing Correlation coefficients on a scatter plot

# R = 0.48: This is the correlation coefficient, 
# which measures the strength and direction of the 
# linear relationship between the two variables. #
# A value of 0.48 suggests a moderate positive linear correlation; 
# as "Attack" increases, "Defense" tends to increase as well.

# p < 2.2e-16: This is the p-value associated with a statistical test 
# (likely a correlation test or a regression analysis) that checks the 
# null hypothesis of no relationship between "Attack" and "Defense". 
# The p-value is very small (less than 2.2 x 10^-16), which is typically 
# below any conventional significance level (e.g., 0.05, 0.01). 
# It suggests that the observed correlation is statistically significant, 
# meaning there is strong evidence against the null hypothesis, 
# and we can infer that there is a statistically significant linear relationship 
# between "Attack" and "Defense" in the population from which the sample was drawn.

# Scatter plots
ggscatter(df_subset, x = "Attack", y = "Defense",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Attack", ylab = "Defense")

# Enhanced Scatter plots
# Categorical values: Generation (numerical), Legendary, Type1
df_subset2 <- df %>% select(-X., -Total)
df_subset2 <- df_subset2[df_subset2$Generation %in% c(1, 2, 3), ]
df_subset2 <- df_subset2[df_subset2$Type.1 %in% c("Fire", "Water", "Grass"), ]

## Factor
df_subset2$Generation <- as.factor(df_subset2$Generation)
df_subset2$Legendary <- as.factor(df_subset2$Legendary)
df_subset2$Type.1 <- as.factor(df_subset2$Type.1)

p <- ggplot(df_subset2, aes(x=Attack, y=Defense, shape = Legendary,
                          color = Type.1, size = Generation)) + 
  geom_point() + theme_bw() +
  theme(legend.position = "bottom")
p

ggMarginal(p, type="density")
ggMarginal(p, type="histogram", fill = "slateblue", xparams = list(bins=20))
ggMarginal(p, type="boxplot")

## QQ PLOTS ##

# Trend Line: If the data were perfectly normally distributed, 
# the points would lie exactly on this line. Since the points 
# in your plot roughly follow the line but deviate in the tails 
# (especially the upper tail), it suggests that the "Attack" data 
# is approximately normally distributed but with some deviations. 
# Specifically, the upper tail (right side) shows that the highest 
# values of "Attack" are larger than what would be expected in a 
# normal distribution (a sign of "heavy tails").

ggqqplot(df_subset$Attack, ylab = "Attack")
ggqqplot(df_subset$Defense, ylab = "defense")

##### 4. DEVELOP A BIG IDEA #####

# Who is your audience?
## People who have played/are familiar with Pokemon

# What are the advantages of your audience knowing your message?
## People who complete in tournaments can use this advantage to have  
## a better understanding/idea which generation/type of Pokemon to
## use for tournaments. Additionally, casual players can also have
## a basic familiarity of the most common types/stats per generation
## without having the need to know/play the generation.

# What do they lose if they don't get the message?
## General understanding of how distributed stats and types 
## are for each generation

# What is your message (Big Idea) in 1 sentence
## Portray the correlation between Attack and Defense 
## along with stat differences between legendaries and non-legendaries





