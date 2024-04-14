# Run any of the install.packages() commands below for packages you don't have.
#  install.packages("tmap")
#  install.packages("leaflet")
#  install.packages("leaflet.extras")
#  install.packages("scales")
#  install.packages("htmlwidgets")
#  install.packages("sf")
#  install.packages("dplyr")
library(tidyverse)
library(raster)
library(RColorBrewer)
library(dplyr)
library("tmap")
library("scales")
library("leaflet")
library("sf")
library("leaflet.extras") #Needed for interactive map
library(htmlwidgets)
library(IRdisplay)

options(stringsAsFactors = FALSE)

library(sf)

####### Using ogrListLayers - IRL_adm1.shp and IRL_adm0.shp #######
library(sf)

file_path <- "C:/Users/twila/OneDrive/COMPSCI/Year 4/SEM 2/Visualising Data/labs/week6/IRLshapefile/IRL_adm"

layers <- st_layers(file_path)

print(layers)

## or
shapefile_path <- "C:/Users/twila/OneDrive/COMPSCI/Year 4/SEM 2/Visualising Data/labs/week6/IRLshapefile/IRL_adm"

shapefile <- st_read(shapefile_path)

layers <- st_layers(shapefile_path)$name

for (layer in layers) {
  data <- shapefile[shapefile$layer == layer, ]
  
  print(paste("Layer:", layer))
  print(st_geometry(data))
  print(st_crs(data))
  
  tm_shape(data) +
    tm_borders() +
    tm_fill("your_variable") +
    tm_layout(main.title = layer)
}


# Read layers with counties in it
shapefile_dir <- "C:/Users/twila/OneDrive/COMPSCI/Year 4/SEM 2/Visualising Data/labs/week6/IRLshapefile/IRL_adm"

IRgeo <- st_read(dsn = shapefile_dir, layer = "IRL_adm1")

# Print the first few rows of IRgeo to verify that it has been read correctly
print(head(IRgeo))


# Rename relevant column to County
names(IRgeo)[names(IRgeo) == "NAME_1"] <- "County"

# View IRgeo
print(IRgeo)

IRgeo$Province <- sapply(IRgeo$County, switch, 
                              
                              'Carlow' = 'Leinster',
                              'Cavan' = 'Ulster',
                              'Clare' = 'Munster',
                              'Cork' = 'Munster',
                              'Donegal' = 'Ulster',
                              'Dublin' = 'Leinster',
                              'Galway' = 'Connaught',
                              'Kerry' =  'Munster',
                              'Kildare' = 'Leinster',
                              'Kilkenny' = 'Leinster',
                              'Laoighis' = 'Leinster',
                              'Leitrim' = 'Connaught',
                              'Limerick' =  'Munster',
                              'Longford' = 'Leinster',
                              'Louth' = 'Leinster',
                              'Mayo' = 'Connaught',
                              'Meath' = 'Leinster',
                              'Monaghan' = 'Ulster',
                              'Offaly' = 'Leinster',
                              'Roscommon' = 'Connaught',
                              'Sligo' = 'Connaught',
                              'Tipperary' =  'Munster',
                              'Waterford' =  'Munster',
                              'Westmeath' = 'Leinster',
                              'Wexford' = 'Leinster',
                              'Wicklow' = 'Leinster')

# Generate a qtm, labelling each county by name and coloring them by province
qtm(IRgeo)


# Generate a quick thematic map using tmap
tm_shape(IRgeo) +
  tm_borders() +
  tm_fill("Province", title = "Province") +
  tm_text("County", size = 0.5) +
  tm_layout(main.title = "Counties with Province")


## STEP 4
mydata <- "C:/Users/twila/OneDrive/COMPSCI/Year 4/SEM 2/Visualising Data/labs/week6"
datapath <- file.path(mydata,'IRCountyPop.csv')
df <- read.csv(datapath)

df

# Rename column county to County
df <- df %>% rename(County = county)

# Order IRgeo and df by County
df_sorted <- df %>% arrange(County)
IRgeo_sorted <- IRgeo %>% arrange(County) 

# Drop Northern Ireland rows
df <- df %>% filter(!(County %in% c('Antrim', 'Armagh', 'Derry', 'Down', 'Fermanagh', 'Tyrone')))

# Rename laois to Laoighis
df$County[df$County == "Laois"] <- "Laoighis"

df

# Merge data
#Merge both datasets
IRmap <- merge(IRgeo, df, by = "County")

IRmap

## Step 5
# Create a static map for population in 1841
tm_shape(IRmap) +
  tm_fill("pop1841", palette = "Blues") +
  tm_borders() +
  tm_layout(main.title = "Population in 1841")

# Create a static map for population in 2001
tm_shape(IRmap) +
  tm_fill("pop2001", palette = "Blues") +
  tm_borders() +
  tm_layout(main.title = "Population in 2001")

## Step 7
library(leaflet)
library(sf)
library(RColorBrewer)
library(htmltools)

# 5 Years to map
years <- c("pop1961", "pop1971", "pop1981", "pop1991", "pop2001")

# Find min and max population values over all years
minpop <- min(unlist(lapply(years, function(year) min(IRmap[[year]], na.rm = TRUE))))
maxpop <- max(unlist(lapply(years, function(year) max(IRmap[[year]], na.rm = TRUE))))

minpop
maxpop

Palette1961 <- colorNumeric(palette="Greens", domain=c(minpop, maxpop))
Palette1971 <- colorNumeric(palette="Greens", domain=c(minpop, maxpop))
Palette1981 <- colorNumeric(palette="Greens", domain=c(minpop, maxpop))
Palette1991 <- colorNumeric(palette="Greens", domain=c(minpop, maxpop))
Palette2001 <- colorNumeric(palette="Greens", domain=c(minpop, maxpop))

popup1961 = paste(
  "County: ", IRmap$County, "<br>",
  "Population: ", IRmap$pop1961
) %>%
  lapply(htmltools::HTML)
popup1971 = paste(
  "County: ", IRmap$County, "<br>",
  "Population: ", IRmap$pop1971
) %>%
  lapply(htmltools::HTML)
popup1981 = paste(
  "County: ", IRmap$County, "<br>",
  "Population: ", IRmap$pop1981
) %>%
  lapply(htmltools::HTML)
popup1991 = paste(
  "County: ", IRmap$County, "<br>",
  "Population: ", IRmap$pop1971
) %>%
  lapply(htmltools::HTML)
popup2001 = paste(
  "County: ", IRmap$County, "<br>",
  "Population: ", IRmap$pop1981
) %>%
  lapply(htmltools::HTML)

# Create a leaflet map
map1 <- leaflet(IRmap) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(
    stroke=TRUE,
    weight = 1,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    popup = popup1961,
    color = ~Palette1961(IRmap$pop1961),
    group = '1961') %>%
  
  addPolygons(
    stroke=TRUE,
    weight = 1,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    color = ~Palette1971(IRmap$pop1971),
    popup = popup1971, 
    group = '1971') %>%
  
  addPolygons(
    stroke=TRUE,
    weight = 1,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    color = ~Palette1981(IRmap$pop1981),
    popup = popup1981,
    group = '1981') %>%
  
  addPolygons(
    stroke=TRUE,
    weight = 1,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    color = ~Palette1991(IRmap$pop1991),
    popup = popup1991, 
    group = '1991' ) %>%
  
  addPolygons(
    stroke=TRUE,
    weight = 1,
    smoothFactor = 0.2,
    fillOpacity = 0.75,
    color = ~Palette2001(IRmap$pop2001),
    popup = popup2001,
    group = '2001'
  ) %>%
  
  addLayersControl(
    baseGroups=c("1961", "1971", "1981", "1991", "2001"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE, exclusive=TRUE)
  )

map1
  
# Save the map as an HTML widget
htmltools::save_html(map1, file = "C:/Users/twila/OneDrive/COMPSCI/Year 4/SEM 2/Visualising Data/labs/week6/interactive_irish_pop_map.html")

