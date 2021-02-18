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
source("define_dirs.R") #Define data file directories.  i.e. data_in_dir, data_out_dir
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))





