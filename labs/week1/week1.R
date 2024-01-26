R.Version()

# Determine lcation of the "make" command in the system
Sys.which("make")

# Retrieve system's PATH environment variable
Sys.getenv("PATH")

# Load tidyverse package
library("tidyverse")

# List datasets available in the datasets package
data(package = "datasets")

# List datasets available in the ggplot2 package
data(package = "ggplot2")

# Opens help documentation for dataset
?Orange 

# Load dataset
data("Orange")

# Display first few rows of the mtcars dataset
head(Orange)

# Returns the number of columns and rows
nrow(Orange)
ncol(Orange)

# Returns col and row names
rownames(Orange)
colnames(Orange)

# Provide summary statistics for the sleep dataset
summary(Orange)

# Provide structure of the sleep dataset
str(Orange)

# Returns class (type) of the "circumference" column 
class(Orange$circumference)

# Initiate a ggplot object using sleep dataset
ggplot(Orange)


colours <- c(rep("red",1), rep("orange",1), rep("yellow",1), rep("green",1), rep("blue",1))

Orange$age = as.factor(Orange$age)
Orange$circumference = as.factor(Orange$circumference)

# Create bar plot with "circumference" column as the x-axis variable
ggplot(Orange, aes(circumference, fill=circumference)) + geom_bar() 


# Pie chart 1
ggplot(Orange, aes(x = "", fill = circumference,)) +
  
  geom_bar(position = "fill") +
  geom_text(
    stat =  "count",
    aes(y = after_stat(..count..),
        label = after_stat(
          scales::percent(..count.. / sum(..count..), 1))),
    position = position_fill(.5),
  ) +
  coord_polar(theta = "y") +
  
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill  = "white"),
  ) 

# Pie chart 2
pie1 <- ggplot(Orange, aes(x = "", fill = factor(age))) +
  geom_bar() +
  coord_polar() + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill  = "white"),
  ) 

pie1

# Pie chart 3 
pie2 <- ggplot(Orange, aes(x = "", fill = factor(age))) +
  geom_bar(width=1) +
  coord_polar() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill  = "white"),
  ) 

pie2

# Coxcomb
ggplot(data = Orange) +
  geom_bar(mapping = aes(x = circumference, fill = circumference)) +
  coord_polar() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill  = "white"),
  ) 

ggplot(data = Orange) +
  geom_bar(mapping = aes(x = circumference, fill = circumference), width=2) +
  coord_polar() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill  = "white"),
  ) 


# Histogram (bins is interval)
ggplot(Orange,aes(x=as.integer(age))) + geom_histogram(fill=colours, bins=5)
