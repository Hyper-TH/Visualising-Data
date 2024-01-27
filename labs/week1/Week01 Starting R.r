R.Version()

# Determine lcation of the "make" command in the system
Sys.which("make")

# Retrieve system's PATH environment variable
Sys.getenv("PATH")

# Load tidyverse package
library("tidyverse")

# Install if not installed
# install.packages("tidyverse")
# library(tidyverse)

# List datasets available in the datasets package
data(package = "datasets")

# List datasets available in the ggplot2 package
data(package = "ggplot2")

# Opens help documentation for mtcars dataset
?mtcars

# Load mtcars dataset
data("mtcars")

# Display first few rows of the mtcars dataset
head(mtcars)

# Returns the number of columns and rows
nrow(mtcars)
ncol(mtcars)

# Returns col and row names
rownames(mtcars)
colnames(mtcars)

# Provide summary statistics for the mtcars dataset
summary(mtcars)

# Provide structure of the mtcars dataset
str(mtcars)

# Returns class (type) of the "hp" column 
class(mtcars$hp)

# Display the "hp" column of the mtcars dataset
mtcars$hp

# Returns the class of the subset of the mtcars dataset 
# containing only the "hp column"
class(mtcars["hp"])

# Display subset of mtcars dataset 
# only containing the "hp" column"
mtcars["hp"]

# Display entire mtcars dataset
mtcars

# Display "cyl" column of mtcars dataset 
mtcars$cyl

# Returns the unique values in the "cyl" column
unique(mtcars$cyl)

# Returns the class of the "cyl" column
class(mtcars$cyl)

# Creates a new DF and copies the mtcars dataset into it
df <- mtcars

# Display contents of new DF
df

# Returns class of the "gear" column 
class(mtcars$gear)

# Convert "gear" column in the DF to a factor
df$gear <- as.factor(df$gear)

# Returns class of "gear" column in DF after conversion
class(df$gear)

# Initiate a ggplot object using mtcars dataset
ggplot(mtcars)

# Convert "cyl" column in mtcars to a factor
mtcars$cyl <- as.factor(mtcars$cyl)

# GGPLOT2

# ggplot sets out the area of the graph
# First term defines what data is being used 
# and the aesthetics (aes()) of the graph
# Followed by + to add a new term, cannot be at the start of line
# Can save graph equation as building it and reuse parts of it
# MORE ON GGPLOT.R

# Create bar plot with "cyl" column as the x-axis variable
ggplot(mtcars, aes(cyl)) + geom_bar()

# Create ggplot object "p" with "cyl" column as the x-axis variable
# Adds a bar layer to the ggplot object "p"
p <- ggplot(mtcars, aes(cyl))
p <- p + geom_bar()
p

p <- ggplot(mtcars, aes(mpg))
p + geom_bar()

df <- mtcars
df$mpg <- as.factor(df$mpg)
p <- ggplot(df, aes(mpg))
p + geom_bar()

df <- mtcars
df$mpg <- as.integer(df$mpg)
p <- ggplot(df, aes(mpg))
p + geom_bar()
ggplot(data = mtcars, aes(x = cyl)) +
  geom_bar()

ggplot(data = mtcars, aes(x = "", fill = cyl)) +
  geom_bar()

ggplot(data = mtcars, aes(x = "", fill = cyl)) +
geom_bar() +
  coord_polar(theta = "y")

pie <- ggplot(mtcars, aes(x = "", fill = factor(cyl))) +
 geom_bar() +
coord_polar()
pie

pie <- ggplot(mtcars, aes(x = "", fill = factor(cyl))) +
 geom_bar(width = 1) +
coord_polar()
pie

ggplot(data = mtcars) +
geom_bar(mapping = aes(x = cyl, fill = cyl)) +
coord_polar()

ggplot(data = mtcars) +
geom_bar(mapping = aes(x = cyl, fill = cyl), width = 1) +
coord_polar()

ggplot(data = mtcars, aes(x = "", fill = cyl)) +
geom_bar() +
  coord_polar(theta = "y")

###

ggplot(data = mtcars) +
geom_bar(mapping = aes(x = "", fill = cyl), width = 1) +

theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank()

) +
coord_polar(theta = "y")

###

ggplot(data = mtcars) +
geom_bar(mapping = aes(x = "", fill = cyl), width = 1) +

theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),

) +
  
coord_polar(theta = "y")

###

ggplot(mtcars, aes(x = "", fill = cyl, )) +

geom_bar(position = "fill") +
  geom_text(
    stat =  "count",
    aes(y = after_stat(..count..),
        label = after_stat(
          scales::percent(..count.. / sum(..count..), 1))),
    position = position_fill(0.5),
  ) +
  coord_polar(theta = "y") +
  
theme(
   axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill  = "white"),
    )

###

ggplot(data = mtcars, aes(x = "", fill = cyl)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(y = after_stat(..count..),
        label = after_stat(
          scales::percent(..count.. / sum(..count..), 1))),
    position = position_fill(0.5),
  ) +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_void()

ggplot(mtcars, aes(x = "", fill = cyl, )) +

geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(y = after_stat(..count..),
        label = after_stat(
          scales::percent(..count.. / sum(..count..), 1))),
    position = position_fill(0.5),
  ) +
  coord_polar(theta = "y") +
theme(
   axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    )


ggplot(mtcars, aes(x = as.integer(cyl))) + geom_histogram(bins = 3)
