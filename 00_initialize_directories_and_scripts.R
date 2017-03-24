############################################################################
###
### Some general comments on the structure of the scripts:
### Each module (00, 01 ...) builds on the previous ones and does NOT work 
### standalone - this avoids redundancies. Data, functions etc. will be 
### carried over from one module to another. Saving and loading of interim 
### outputs is done into path2temp. The data input comes directly from 
### a CSV file or the Natura2000 data portal, so loading local files 
### is not necessary. 
###
### Overall structure of the modules is:
### 
###  - 00_initialize_directories_and_scripts.R
###  - 01_load_libraries_and_functions.R
###  - 02_load_data.R
###  - 03_IUCN_analysis.R
###  - 04_core_analysis.R
###
### Authors: CH, MB
############################################################################

############################################################################
### 00.1. set and create the working and temporary directories
###
############################################################################

workingDir <- getwd()
tempDir <- "tempDirectory"

path2temp <- file.path(workingDir, tempDir)
path2wd <- file.path(workingDir)

if (file.exists(path2temp) & file.exists(path2wd)){
  path2temp <- file.path(workingDir, tempDir)
  path2wd <- file.path(workingDir)
} else {
  dir.create(path2temp)
  path2temp <- file.path(workingDir, tempDir)
  path2wd <- file.path(workingDir)
}

############################################################################
### 00.2. source all relevant R scripts
############################################################################

### helper function
"%+%" <- function(x,y)paste(x,y,sep="")

### load libraries, functions and google sheets 
ptm <- proc.time(); source(path2wd %+% "/01_load_libraries_and_functions.R"); proc.time() - ptm # ca. 30 seconds
ptm <- proc.time(); source(path2wd %+% "/02_load_data.R"); proc.time() - ptm # ca. 100 seconds
ptm <- proc.time(); source(path2wd %+% "/03_IUCN_analysis.R"); proc.time() - ptm # 300 seconds
ptm <- proc.time(); source(path2wd %+% "/04_core_analysis.R"); proc.time() - ptm # 190 seconds
