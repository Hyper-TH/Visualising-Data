# Extension of Week 01 Starting R.r


# Bar graph
# Show number of rows for each val of the column
# geom_bar
# Because there is only one var and two axes, graph is monochrome

?mtcars
summary(mtcars)
str(mtcars)

# X axis should use factors
mtcars$cyl = as.factor(mtcars$cyl)

##### BAR CHART #####
ggplot(mtcars, aes(cyl)) + geom_bar()

# Save plot layer and build on them
p <- ggplot(mtcars, aes(cyl))
p <- p + geom_bar()
p

# Because there is a column for every val 
# this does not work for real numbers
p <- ggplot(mtcars, aes(mpg))
p + geom_bar()

# Could make it categorical, still not great
mtcars$mpg <- as.factor(mtcars$mpg)
p <- ggplot(mtcars, aes(mpg))
p + geom_bar()

# Or integer
mtcars$mpg <- as.integer(mtcars$mpg)
p <- ggplot(mtcars, aes(mpg))
p + geom_bar()


##### PIE CHART #####

# To start pie chart, start with stacked bar then convert
# geometry to polar_coords
ggplot(data = mtcars, aes(x = "", fill = cyl)) + 
  geom_bar()


# To get a pie chart:
# The mapping must be changed from cartesian to polar
# Instead of using signed distances along the 2 coord axes
# polar coords specifies the location of a point P in the plane
# by its distance R from the origin and the angle theta made 
# between the line segment from the origin to P and the + X axis
# Data needs to be on the Y axis


##### PIE CHART #####
ggplot(mtcars, aes(x = "", fill = cyl)) + 
  geom_bar() +
  coord_polar(theta = "y")

##### PACMAN CHART #####
pacman <- ggplot(mtcars, aes(x = "", fill = cyl)) + 
  geom_bar() + 
  coord_polar()
pacman

##### BULLET CHART #####
bullet <- ggplot(mtcars, aes(x = "", fill = cyl)) +
  geom_bar(width = 1) + 
  coord_polar()
bullet

##### COXCOMB CHART #####
coxcomb <- ggplot(mtcars) + 
  geom_bar(mapping = aes(x = cyl, fill = cyl)) + 
  coord_polar()
coxcomb

coxcomb2 <- ggplot(mtcars) + 
  geom_bar(mapping = aes(x = cyl, fill = cyl), width = 1) + 
  coord_polar()
coxcomb2