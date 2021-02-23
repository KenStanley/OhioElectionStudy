#
# collapseOhioVoterFile.R  
#
# This script required roughly 15 minutes, or roughly four times as long 
# as the first read and convert (as printed by toc() ) took on my MacBook Air with 8 Gb memory.
#
# produces aSWVF stored in "aSWVFcolumns.rds"
# and schoolDistByPrecinct and schoolDistByVoters stored in "schoolDistInfo.rds"
#

setwd("/Users/kenstanley/GitRepository/github/politicalRcode/OhioElectionStudy")
source( "includeForAll.R")

source("define_dirs.R")

library(dplyr)
library(tictoc)
library(tidyverse)
# library(expss)
voterIdentificationColumns = c(1,2,4:7,8,11:17,29:30,34:37,39:40,46,99,104,105,107)


tic()
SWVF_1_22 = read.csv(file=file.path(SWVF_dir,"SWVF_1_22.txt"))	
aSWVF_1_22 = SWVF_1_22[,voterIdentificationColumns]
toc()

tic()
SWVF_23_44 = read.csv(file=file.path(SWVF_dir,"SWVF_23_44.txt"))	
aSWVF_23_44 = SWVF_23_44[,voterIdentificationColumns]
toc()
tic()
SWVF_45_66 = read.csv(file=file.path(SWVF_dir,"SWVF_45_66.txt"))	
aSWVF_45_66 = SWVF_45_66[,voterIdentificationColumns]
toc()
tic()
SWVF_67_88 = read.csv(file=file.path(SWVF_dir,"SWVF_67_88.txt"))	
aSWVF_67_88 = SWVF_67_88[,voterIdentificationColumns]
toc()


tic()

voterIdentificationColumns = c(1,2,4:7,8,11:17,29:30,34:37,39:40,46,87:ncol(SWVF_1_22))

toc()

tic()
aSWVF_1_22 = SWVF_1_22[,voterIdentificationColumns]
aSWVF_23_44 = SWVF_23_44[,voterIdentificationColumns]
aSWVF_45_66 = SWVF_45_66[,voterIdentificationColumns]
aSWVF_67_88 = SWVF_67_88[,voterIdentificationColumns]

aSWVF = rbind(aSWVF_1_22,aSWVF_23_44,aSWVF_45_66,aSWVF_67_88)
toc()
save(aSWVF,file=file.path(SWVF_dir,"aSWVFcolumns.rds"),version=mySaveVersion) 

schoolDistByPrecinct = group_by(aSWVF,CITY_SCHOOL_DISTRICT,
                                EXEMPTED_VILL_SCHOOL_DISTRICT,LOCAL_SCHOOL_DISTRICT,
                                PRECINCT_NAME, PRECINCT_CODE ) %>% summarise(numVoters=n())

schoolDistByVoters = group_by(aSWVF,CITY_SCHOOL_DISTRICT,
                              EXEMPTED_VILL_SCHOOL_DISTRICT,LOCAL_SCHOOL_DISTRICT) %>% summarise(numVoters=n())

save( list=c("schoolDistByPrecinct", "schoolDistByVoters"),file="schoolDistInfo.rds",version=mySaveVersion)


