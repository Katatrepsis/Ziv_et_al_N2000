### Welcome to GitHub Pagesjjj

This Github Repository contains an archive of code for the manuscript _"A bird’s eye view over ecosystem services in Natura 2000 sites across Europe"_ (Ziv et al.), produced during the Facilitated Workshops on Ecosystem Services (FAWKES) II Workshop. This repository is designed to make the analysis and code within the paper as transparent and replicable as possible, such that all scripts work platform independent and all data are called from online sources. 

## Structure of the repository
The key files for the analyis are as follows:

# 00_initialize_directories_and_scripts.R
This script is basically the launchpad that will first define the working directories for scripts and data. New users should adjust the core working directory, and the script will organise files into that working directory or an accompanying temp directory. Two folders will be specified: path2temp (where the data is stored and outputs will be created) and path2wd (the working folder). Secondly, all other R files can be triggered from this section 00.2. So if you source 00 all other scripts will be run as well. Please be aware of run times for some of those scripts, which can be 20-30 minutes in some cases.

# 01_load_libraries_and_functions.R
This script loads all libraries needed for subsequent analysis and automatically installs if libraries are missing.

# 02_load_data.R
Loads all data from the web, based on a permament link to the Natura2000 dataset and a CSV file produced during the FAWKES II workshop with an expert consensus mapping of Natura2000 threats to ecosystem services (see manuscript for details). Data will be downloaded only once to your path2temp.

# 03_IUCN_script.R
This script uses the letsR package to download IUCN data for species that are focal species in the Natura2000 network. The data includes the IUCN "Population" field, which denotes the trends in the species.

# 04_core_analysis.R
This final script conducts the bulk of the analysis on the data.I
