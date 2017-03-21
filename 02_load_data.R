############################################################################
### Purpose of this script module 03 is to:
###
### 02.1. Get Natura 2000 data
### 02.2. Get threat/service translation table directly from Github
###
### Authors: CH, MB
############################################################################

# set to temp wd suitable for downloading and extracting data files
setwd(path2temp %+% "/") 

############################################################################
### 02.1. Get Natura 2000 data
############################################################################

if (file.exists("PublicNatura2000End2015_csv.zip")==FALSE){
  download.file("https://www.dropbox.com/s/yaujzwuijyzluc6/PublicNatura2000End2015_csv.zip?dl=1", "PublicNatura2000End2015_csv.zip", mode="wb")
  unzip("PublicNatura2000End2015_csv.zip")
} else {unzip("PublicNatura2000End2015_csv.zip")}

############################################################################
### 02.2. Get threat/service translation table
############################################################################

### Load ecosystem service mapping table
setwd(path2wd %+% "/") 
MappingData <- read.csv("ES_translation.csv",header=TRUE)
MappingData <- as.data.frame(MappingData) 
### check variable types 
str(MappingData)

### make factors
MappingData$ACT_Code <- as.factor(MappingData$ACT_Code)

