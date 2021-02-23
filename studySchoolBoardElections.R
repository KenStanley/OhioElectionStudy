#
#  studySchoolBoardElections.R 
#
#  This file takes 1 minute on my MacBook Air with 8 Gbytes of RAM 
#
#  Creates: SchoolDistPresResultsSummary saved in file="SchoolDistPresResultsSummary.rds")
#
#     This file used to craeate SchoolDistPresResultsFullSummary, identical to SchoolDistPresResultsSummary
#

source( "includeForAll.R") # includes an rm (list=ls())

tic()
# County2020RaceResultsByPrecinctSheet1 = read_excel(path=file.path(data_in_dir,"County2020RaceResultsByPrecinct.xlsx"),sheet=1)
# this is the only one we might have used County2020RaceResultsByPrecinctSheet2 = read_excel(path=file.path(SWVF_dir,"County2020RaceResultsByPrecinct.xlsx"),sheet=2)
# County2020RaceResultsByPrecinctSheet3 = read_excel(path=file.path(data_in_dir,"County2020RaceResultsByPrecinct.xlsx"),sheet=3)
# County2020RaceResultsByPrecinctSheet4 = read_excel(path=file.path(data_in_dir,"County2020RaceResultsByPrecinct.xlsx"),sheet=4)
# County2020RaceResultsByPrecinctSheet5 = read_excel(path=file.path(data_in_dir,"County2020RaceResultsByPrecinct.xlsx"),sheet=5)
# County2020RaceResultsByPrecinctSheet6 = read_excel(path=file.path(data_in_dir,"County2020RaceResultsByPrecinct.xlsx"),sheet=6)
toc()
tic()
# allColumns = colnames(County2020RaceResultsByPrecinctSheet2)
# allSchoolColumns = grepl("chool",allColumns)

load( file="schoolDistInfo.rds",verbose=myVerbose) # schoolDistByPrecinct and schoolDistByVoters which are produced in collapseOhioVoterFile.R 

schoolDistSize = group_by(schoolDistByPrecinct,CITY_SCHOOL_DISTRICT ) %>%
  summarize(totalVoters = sum(numVoters), numPrecincts = n()) 

legitSchoolDistByVoters = schoolDistByVoters[ which(schoolDistByVoters$numVoters > 200),]
legitSchoolDistByVoters$counta = 0 
legitSchoolDistByVoters$counta = legitSchoolDistByVoters$counta + 
  ( nchar(as.character(legitSchoolDistByVoters$CITY_SCHOOL_DISTRICT)) > 0 ) + 0 
legitSchoolDistByVoters$counta = legitSchoolDistByVoters$counta + 
  ( nchar(as.character(legitSchoolDistByVoters$EXEMPTED_VILL_SCHOOL_DISTRICT)) > 0 ) + 0 
legitSchoolDistByVoters$counta = legitSchoolDistByVoters$counta + 
  ( nchar(as.character(legitSchoolDistByVoters$LOCAL_SCHOOL_DISTRICT)) > 0 ) + 0 


#
# Sheet1 has a key, Sheet3 has the congressional results
#
# I found statewideresultsbyprecinct.xlsx on this page: 
#    https://www.ohiosos.gov/elections/election-results-and-data/2020/
#    under the heading "Results by Precinct (XLSX)" 
#    The following reports include data on voter registration and turnout, as well as breakdown by county and by precinct of the results for the following races:
#      President
# Or you can find it directly at: 
#    https://www.ohiosos.gov/globalassets/elections/2020/gen/statewideresultsbyprecinct.xlsx

National2020results = read_excel(path=file.path(SWVF_dir,"statewideresultsbyprecinct2020.xlsx"),sheet=2,skip=1)

#
# Get rid of rows we don't need
#
National2020resultsSansSummary = National2020results[ which(!is.na(National2020results$`Precinct Name`)),]

columnsToKeep = c( "County Name" ,"Precinct Name" ,
                   "Precinct Code" , "Joseph R. Biden and Kamala D. Harris (D)"   ,
                   "Donald J. Trump and Michael R. Pence (R)" )

National2020PresidentialResults = National2020resultsSansSummary[,columnsToKeep] %>% 
  dplyr::rename( "Biden" = "Joseph R. Biden and Kamala D. Harris (D)") %>%
  dplyr::rename( "Trump" = "Donald J. Trump and Michael R. Pence (R)" )  %>%
  dplyr::rename( "County" = "County Name" ) 

SchoolDistPresidentialResults = merge( x=schoolDistByPrecinct, 
                                       y=National2020PresidentialResults, 
                                       by.x = "PRECINCT_NAME",
                                       by.y = "Precinct Name" ) 

SchoolDistPresResultsSummary = group_by( SchoolDistPresidentialResults, County, 
                                         CITY_SCHOOL_DISTRICT,    EXEMPTED_VILL_SCHOOL_DISTRICT,
                                         LOCAL_SCHOOL_DISTRICT ) %>% summarise( Trump = sum(Trump),
                                                                                Biden = sum(Biden))

SchoolDistPresResultsSummary$percentTrump = SchoolDistPresResultsSummary$Trump / 
  (  SchoolDistPresResultsSummary$Trump  + SchoolDistPresResultsSummary$Biden )

save( list=c("SchoolDistPresResultsSummary"), file="SchoolDistPresResultsSummary.rds", version=mySaveVersion )

totalNumVoters = sum(schoolDistSize$totalVoters)
toc()

