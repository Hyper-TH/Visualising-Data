# Get data
mydata = file.path("C:/COMPSCI_PROJECTS/R/CA/data")

datapath1 <- file.path(mydata,'1- share-of-the-world-population-with-at-least-basic-education.csv')
datapath2 <- file.path(mydata,'2- learning-adjusted-years-of-school-lays.csv')
datapath3 <- file.path(mydata,'3- number-of-out-of-school-children.csv')
datapath4 <- file.path(mydata,'4- gender-gap-education-levels.csv')
datapath5 <- file.path(mydata,'population-and-demography.csv')

education_ratio <- read.csv(datapath1)
learning_years <- read.csv(datapath2)
out_of_school <- read.csv(datapath3)
gender_gap_education <- read.csv(datapath4)
population <- read.csv(datapath5)

## EDUCATION RATIO

# Rename columns
education_ratio <- education_ratio %>%
  rename(
    With_No_Education_Share = Share.of.population.with.no.formal.education..1820.2020,
    With_Education_Share = Share.of.population.with.some.formal.education..1820.2020
  )

# Create column to check the ratio if its equal to 100
education_ratio <- education_ratio %>%
  mutate(check = With_No_Education_Share + With_Education_Share)

unique(education_ratio$check)

# Avoid inconsistency of decimal error by rounding the values 
education_ratio <- education_ratio %>%
  mutate(
    With_No_Education_Share = round(With_No_Education_Share, 1),
    With_Education_Share = round(With_Education_Share, 1),
    check_1 = With_No_Education_Share + With_Education_Share
  )

unique(education_ratio$check_1)

# If values up to 100, drop the extra columns
education_ratio <- education_ratio[, !names(education_ratio) %in% c('check', 'check_1')]


## LEARNING YEARS
# Rename columns
learning_years <- learning_years %>%
  rename(
    Learning_Years = Learning.Adjusted.Years.of.School
  )

# Round the learning years to first decimal point
learning_years <- learning_years %>%
  mutate(Learning_Years = round(Learning_Years))

## OUT OF SCHOOL
# Rename columns
out_of_school <- out_of_school %>%
  rename(
    Dropped_Out_Male = Out.of.school.children..adolescents.and.youth.of.primary.and.secondary.school.age..male..number.,
    Dropped_Out_Female = Out.of.school.children..adolescents.and.youth.of.primary.and.secondary.school.age..female..number.
  )

## GENDER GAP EDUCATION
gender_gap_education <- gender_gap_education %>%
  rename(
    Tertiary_Education_Female = Combined.gross.enrolment.ratio.for.tertiary.education..female,
    Tertiary_Education_Male = Combined.gross.enrolment.ratio.for.tertiary.education..male,
    Primary_Education_Female = Combined.total.net.enrolment.rate..primary..female,
    Primary_Education_Male = Combined.total.net.enrolment.rate..primary..male,
    Secondary_Education_Female = Combined.total.net.enrolment.rate..secondary..female,
    Secondary_Education_Male = Combined.total.net.enrolment.rate..secondary..male,
  )


# Prepare for merging

# Remove Code
education <- education_ratio[ , !names(education_ratio) %in% c('Code')]
learning <- learning_years[ , !names(learning_years) %in% c('Code')]
out_school <- out_of_school[ , !names(out_of_school) %in% c('Code')]
gender_gap <- gender_gap_education[ , !names(gender_gap_education) %in% c('Code')]

education$Code <- NULL
learning$Code <- NULL
out_school$Code <- NULL
gender_gap$Code <- NULL

# Find common entities (country) between the first two datasets
common_entities_temp <- intersect(education_ratio$Entity, learning_years$Entity)

# Find common entities across all three datasets
common_entities <- intersect(common_entities_temp, out_of_school$Entity)

# Get all data for common entities and put it there
education <- education[education_ratio$Entity %in% common_entities, ]
learning <- learning[learning_years$Entity %in% common_entities, ]
out_school <- out_school[out_of_school$Entity %in% common_entities, ]
gender_gap <- gender_gap[gender_gap_education$Entity %in% common_entities, ]

# Rename columns
education <- education %>%
  rename(
    Country = Entity
  )

learning <- learning %>%
  rename(
    Country = Entity
  )


out_school <- out_school %>%
  rename(
    Country = Entity
  )

gender_gap <- gender_gap %>%
  rename(
    Country = Entity
  )


# FOR WORLD DATA
# OWID_WRL
# Remove all countries
w_education <- education_ratio[education_ratio$Entity == "World", ]
w_gender_gap <- gender_gap_education[gender_gap_education$Entity == "World", ]
w_out_of_school <- out_of_school[out_of_school$Entity == "World", ]

# Drop Code columns
w_education$Code <- NULL
w_out_of_school$Code <- NULL
w_gender_gap$Code <- NULL


### POPULATION ###
# We need the years 1950-2020
# We need the age group Population, sum of (5-9, 10-14, 15-19)
nrow(population)
sum(is.na(population))

population <- population %>%
  mutate(Children_Population_5_to_19 = Population.aged.5.to.9.years + 
           Population.aged.10.to.14.years + 
           Population.aged.15.to.19.years)

population <- population %>%
  rename(
    Country = Country.name
  )