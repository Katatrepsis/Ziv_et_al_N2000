############################################################################
### Purpose of this skript module 01 is to:
### 
### 01.1. load all libraries needed for subsequent analysis
###
### Authors: MB, CH
############################################################################

############################################################################
### 01.1. load all libraries needed for subsequent analysis, 
### automatically installs if libraries are missing
############################################################################

needed_libs <- c("ggplot2",# For plotting
                 "raster",# for adding map data
                 "rgdal", # for loading map data
                 "rgeos", # dependency for rgdal
                 "letsR"
)
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
sapply(needed_libs,usePackage)

rm(needed_libs)
