#
#  collectSchoolLevyData.R - 18 Feb 2020 - 
#
#  Collects data from the local issues excel spreadsheets into a single data frame
#  stored in "allIssues.rds"
#
setwd("/Users/kenstanley/GitRepository/github/politicalRcode/OhioElectionStudy")

source( "includeForAll.R")
source("convertSchoolLevyToDataFrame.R")
spreadsheet = "https://docs.google.com/spreadsheets/d/15maCc8ujhcMqpYXDu7a3EWDZz8LN1kcl9w0KFD-pDBs/edit#gid=365319428"

term="all"
description="all"
question="all"
#
#  The 2012 results are in pdf form and hence harder to include 
#
fileNames = c("2016-03_localissuesresults.xlsx",		"2017-11-07_localissuesresults.xlsx",	"2019-08_summaryresults.xlsx",
              "2016-08_localissuesresults.xlsx",		"2018-05_localissuesresults.xlsx",		"2019-11-06_localissuesresults.xlsx",
              "2016-11_localissuesresults.xlsx",		"2018-08_localissuesresults.xlsx",	"2020-03-03_localissuesresults.xlsx",
              "2017-05_localissuesresults.xlsx",		"2018-11_localissues.xlsx",		"2020-08-04_localissuesresults.xlsx",
              "2017-08_localissuesresults.xlsx",		"2019-05_summaryresults.xlsx",		"2020-11-03_localissuesresults.xlsx",
              "2015-05_localissuesresults.xlsx", "2015-08_localissuesresults.xlsx", "2015-11_localissuesresults.xlsx",             
              "2013-05_localissuesresults.xlsx", "2013-08_localissuesresults.xlsx", "2013-11_localissuesresults.xlsx",
              "2014-05_localissuesresults.xlsx", "2014-08_localissuesresults.xlsx", "2014-11_localissuesresults.xlsx" ) #  No five year theseIssues in 


electionDates = c("mar2016",		"nov2017",	"aug2019",
                  "aug2016",		"may2018",		"nov2019",
                  "nov2016",		"aug2018",	"mar2020",
                  "may2017",		"nov2018",		"aug2020",
                  "aug2017",		"may2019",		"nov2020",
                  "may2015",  "aug2015", "nov2015",
                  "may2013",  "aug2013", "nov2013",
                  "may2014",   "aug2014", "nov2014") 

# "2016-11_localissuesresults.xlsx"
# "2018-11_localissuesresults.xlsx"


skips = 0*(1:length(electionDates))
skips[c(4,1,8,10,11,13,16,17,18,19,20,21,22,23,24)] = 1  # aug2016, mar2016, aug2018, may2017, aug2017

# sheet7 = read_excel(path=file.path(data_in_dir,"schoolIssues",fileNames[7]),sheet=1,skip=skips[7])
# sheet11 = read_excel(path=file.path(data_in_dir,"schoolIssues",fileNames[11]),sheet=1,skip=skips[11])

stopifnot(length(fileNames) == length( electionDates))

commonColumns = c("County"  , "Region", "Media Market" ,  "Subdivision Type"   ,
                  "Subdivision Name" , "Question Type" , "Purpose"  ,  "Description"    ,
                  "Millage"  ,   "Percent"   , "Dollar Amount"   ,  "Length"    ,
                  "Commencing Year or Effective Date", "Votes For"  , "Votes Against"  ,
                  "sixChars" ,  "percentYes"   , "election"    )
# , "Overlap 1"   , "Overlap 3"                         "Overlap 4"                        
#   [21] "Overlap 5"                         "Overlap 6"                        
#   [23] "Overlap 7"                         "Overlap 8"                        

firstIndex = 1 
for ( index in firstIndex:length(fileNames)) {
  
  
  issues = read_excel(path=file.path(data_in_dir,"schoolIssues",fileNames[index]),sheet=1,skip=skips[index])
  
  if( !( "Votes Against" %in% colnames(issues)) ) {
    print("Column names issues with index =",index," consider skip?")
    browser()
  } 
  
  print(paste("index =", index))
  

  theseIssues = convertSchoolLevyToDataFrame( issues,electionDates[index], term=term, 
                                    description=description,  question=question)
  
  if ( theseIssues == 0 ) {
    # do nothing 
  } else {
    stopifnot( nrow(theseIssues) >= 1)
    if ( index ==firstIndex ) {
      allconvertSchoolLevyToDataFrames = theseIssues[,commonColumns]
    } else {
      allconvertSchoolLevyToDataFrames = rbind(allconvertSchoolLevyToDataFrames,theseIssues[,commonColumns])
    }
  }

}

allIssues = allconvertSchoolLevyToDataFrames
save(allIssues,file="allIssues.rds")

