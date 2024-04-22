### PACKAGES ###
required_packages <- c("tidyverse", "knitr", "DT", "plotly", "ggvis", "manipulateWidget", "cowplot", "dplyr")

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

library(dplyr)
library(ggplot2)
library(tidyr)