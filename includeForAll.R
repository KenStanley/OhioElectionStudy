#
# includeForAll.R
#
# Put things in here that we want to include in every script  
#


library("tictoc")
library(tidyverse)
library(googlesheets4)
library(readxl)

rm(list=ls())
tic()
source ("sheet_ops_nice.R")
source("define_dirs.R") #Define data file directories.  i.e. data_in_dir, SWVF_dir
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
myVerbose = FALSE # This causes our load() commands (all of which include save=mySaveVersion) to print the data frames in the .rds file loaded 
mySaveVersion=2 # Some versions of R are not compatible with files saved with version 3 
supplementalDataSpreadsheet = "https://docs.google.com/spreadsheets/d/1bKtCI_yCgF7oVJy8P48VbX6-p3nsaY7cDAAUIEQ5QrY/edit#gid=0"



