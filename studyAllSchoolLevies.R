#
#  studyAllSchoolLevies.R - 29 Jan 2020 - this appears to be the most
#    recent file - except that finalElectionStudy has now replaced it 
#
#  studySchoolLevies.R has the most documentation
#
#  next steps:
#    1) Compute the number of Dems and Reps that voted in each of these
#    elections in each School District - DONE 
#       The challenge is that we want information for some school districts
#       and some elections, but we don't want all 660+ school districts for
#       all 20ish elections 
#       maybe just write a routine that calculates this for one 
#       school district and one election, then call that as we 
#       see fit. 
#    2) update matchSchoolLeviesToPresidentialResults.R to pick up the school districts
#       that we are interested in
#
#    OK now do it - may2017levies is created by this file 
#       add the stuff that we want to it. 
#
#  Preceded by:
#     DvsRturnout.R
#     matchSchoolLeviesToPresidentialResults
#       which is preceded by studySchoolBoardElections
#
#  data frames 
#    electionDateConversion - converts 08-01-2019 to "aug2019"
#
#    additionals - a set for one election
#    allElectionPerformance - Information about statewide D and R vote rates 
#    allFiveYearAddsWithPres - All Levies defined by description and term 
#       with percentYes and percentTrump but no information about statewide 
#       D and R performance 
#    allKeyLeviesWithE_Date - allFiveYearAddsWithPres with eDate (08-01-2019) and election ("aug2019") 
#    allLeviesWithElectionTurnoutInfo - allKeyLeviesWithE_Date with allElectionPerformance
#    issues - temporary - just one excel file 
#
#    SchoolDistPresResultsFullSummary - Summary info from the state voter file "percentTrump"  by  "schoolDist", "County" 
#
#    allElectionPerformance, allFiveYearAddsWithPres, allLeviesWithElectionTurnoutInfo, electionDateConversion

setwd("/Users/kenstanley/GitRepository/github/politicalRcode/postTrump")

source( "includePostTrump.R")
source("fiveYearAdditional.R")
spreadsheet = "https://docs.google.com/spreadsheets/d/15maCc8ujhcMqpYXDu7a3EWDZz8LN1kcl9w0KFD-pDBs/edit#gid=365319428"


load(file="allMatches.rds")
load( file="SchoolDistPresResultsSummary.rds") # Produced in studySchoolBoardElections contains SchoolDistPresResultsFullSummary

term="all"
description="all"
question="all"

SchoolDistPresResultsFullSummary$schoolDist = paste(SchoolDistPresResultsFullSummary$CITY_SCHOOL_DISTRICT, 
                                                    SchoolDistPresResultsFullSummary$EXEMPTED_VILL_SCHOOL_DISTRICT,
                                                    SchoolDistPresResultsFullSummary$LOCAL_SCHOOL_DISTRICT,sep="")

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
              "2014-05_localissuesresults.xlsx", "2014-08_localissuesresults.xlsx", "2014-11_localissuesresults.xlsx" ) #  No five year additionals in 


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
  

  additionals = fiveYearAdditional( issues,electionDates[index], term=term, 
                                    description=description,  question=question)
  
  if ( additionals == 0 ) {
    # do nothing 
  } else {
    stopifnot( nrow(additionals) >= 1)
    if ( index ==firstIndex ) {
      allFiveYearAdditionals = additionals[,commonColumns]
    } else {
      allFiveYearAdditionals = rbind(allFiveYearAdditionals,additionals[,commonColumns])
    }
  }

}

setdiff( colnames(additionals ), commonColumns)
setdiff(  commonColumns, colnames(additionals ))

allFiveYearAddsWithSchoolDist = merge( x=allFiveYearAdditionals,
                                       y=allMatches,
                                       by=c("County", "Subdivision Name"),
                                       all.x=TRUE)

schoolLeviesColsToKeep = c( "County", "Subdivision Name"  , "Question Type",
                            "Purpose" , "Description" , "Millage" , "Percent" ,
                            "Dollar Amount"   ,"Length", "Votes For" , "Votes Against" ,  "percentYes"  )

# setdiff( colnames(allFiveYearAddsWithSchoolDist ), schoolLeviesColsToKeep)
# setdiff(  schoolLeviesColsToKeep, colnames(allFiveYearAddsWithSchoolDist ) ) 

fullSummaryColsToKeep = c( "County"  ,"percentTrump"  ,  "schoolDist"   )
allFiveYearAddsWithPres = merge( allFiveYearAddsWithSchoolDist, 
                                 SchoolDistPresResultsFullSummary[,fullSummaryColsToKeep],
                                 by=c("County","schoolDist"),
                                 all.x=TRUE) 


#
#  All we use from this results spreadsheet is the date conversion columns (V = "election" , W="sortable election date")
#
resultsSpreadsheet = "https://docs.google.com/spreadsheets/d/1no33oQarfqMYS-nlSqqZvszJngVuUowJLT_zirRH45c/edit#gid=44367585"

resultsPlus = read_sheet_nice( ss=resultsSpreadsheet, sheet=1)



electionDateConversion = unique( resultsPlus[,c("election" , "sortable election date" )]) %>%
  dplyr::rename("eDate" = "sortable election date")


# 
# electionDateConversion$election[1:21] = c(  "may2014", "aug2014",  "nov2014", 
#                                       "may2015", "aug2015",  "nov2015", 
#                                       "mar2016", "aug2016",  "nov2016",
#                                       "mar2017", "aug2017",  "nov2017", 
#                                       "mar2018", "aug2018",  "nov2018",
#                                       "mar2019", "aug2019",  "nov2019",
#                                       "may2020", "aug2020",  "nov2020"
#                                       )
# electionDateConversion$election[16:21] = c(  "mar2019", "aug2019",  "nov2019",
#                                             "may2020", "aug2020",  "nov2020")
# 
# electionDateConversionMore = electionDateConversion[1:6,]
# electionDateConversionMore = 

allKeyLeviesWithE_Date = merge(x=allFiveYearAddsWithPres,
                               y=electionDateConversion,
                               by.x="election",
                               by.y="election")

load(file="allElectionPerformance.rds") # cmoputed in DvsRturnout

performanceColumns = c("electionDate"  , "relDemPerformance",  "relDemTurnout" )


allLeviesWithElectionTurnoutInfo = merge( x=allKeyLeviesWithE_Date, 
                                          y=allElectionPerformance[,performanceColumns],
                                          by.x="election",
                                          by.y="electionDate")

# save(electionDateConversion,file="electionDateConversion.rds")

#    allElectionPerformance, allFiveYearAddsWithPres, allLeviesWithElectionTurnoutInfo, electionDateConversion
# allFiveYearRenewals = allLeviesWithElectionTurnoutInfo
# save( allFiveYearRenewals, file="allFiveYearRenewals.rds")
# allFiveYearAdditionals = allLeviesWithElectionTurnoutInfo
# save( allFiveYearAdditionals, file="allFiveYearAdditionals.rds")

# allLevies = allLeviesWithElectionTurnoutInfo
# save(allLevies,file="allLevies.rds")
allIssues = allLeviesWithElectionTurnoutInfo
save(allIssues,file="allIssues.rds")
term
description
question


# see finalElectionStudy.R for the actual study - obsolete, now compareAcrossLevyTypes.R  



# 
# browser()
# load( file="allDemVoterTables.rds")
# 
# load(file="turnoutAllElections.rds")
# 
# browser()
# 
# 
# 
# 
# allKeyLeviesSorted = allLeviesWithElectionTurnoutInfo[order(allLeviesWithElectionTurnoutInfo$"Description"  ,
#                                                             allLeviesWithElectionTurnoutInfo$"Subdivision Name"  ,
#                                                             allLeviesWithElectionTurnoutInfo$"eDate" ),]
# 
# 
# 
# allKeyLeviesSorted$redo = FALSE 
# allKeyLeviesSorted$redo[2:nrow(allKeyLeviesSorted)] = 
#   allKeyLeviesSorted$"Subdivision Name"[2:nrow(allKeyLeviesSorted)] == 
#   allKeyLeviesSorted$"Subdivision Name"[1:(nrow(allKeyLeviesSorted)-1)]
# 
# allKeyLeviesSorted$previous[2:nrow(allKeyLeviesSorted)] = 
#   allKeyLeviesSorted$"percentYes"[1:(nrow(allKeyLeviesSorted)-1)]
# allKeyLeviesSorted$previousDemTurnout[2:nrow(allKeyLeviesSorted)] = 
#   allKeyLeviesSorted$relDemTurnout[1:(nrow(allKeyLeviesSorted)-1)]
# allKeyLeviesSorted$previousDemPerformance[2:nrow(allKeyLeviesSorted)] = 
#   allKeyLeviesSorted$relDemPerformance[1:(nrow(allKeyLeviesSorted)-1)]
# 
# # Springfield Local Schools in Toledo are the only district that has 
# # run three renewals in the same election, so I just took them out 
# springfieldToledo = allKeyLeviesSorted$`Subdivision Name` ==  "Springfield Local School District" &
#   allKeyLeviesSorted$County == "Summit"
# # View(allKeyLeviesSorted[which(springfieldToledo),])
# allKeyLeviesSorted = allKeyLeviesSorted[which(springfieldToledo==0),]
# 
# allKeyLeviesSorted$previous[which(allKeyLeviesSorted$redo==FALSE)] = NA
# 
# # If we have two levies in one election, the second one in the table
# # should use the "previous" value from the first one in the table
# # 
# twoLeviesOneElection = 
#   allKeyLeviesSorted$redo[2:nrow(allKeyLeviesSorted)] & 
#   ( allKeyLeviesSorted$eDate[2:nrow(allKeyLeviesSorted)] == 
#       allKeyLeviesSorted$eDate[(2:nrow(allKeyLeviesSorted))-1] ) 
# 
# allKeyLeviesSorted$previous[which(twoLeviesOneElection)] = 
#   allKeyLeviesSorted$previous[which(twoLeviesOneElection)-1]
# allKeyLeviesSorted$previousDemTurnout[which(twoLeviesOneElection)] = 
#   allKeyLeviesSorted$previousDemTurnout[which(twoLeviesOneElection)-1]
# allKeyLeviesSorted$previousDemPerformance[which(twoLeviesOneElection)] = 
#   allKeyLeviesSorted$previousDemPerformance[which(twoLeviesOneElection)-1]
# 
# #
# # We do not yet support three levies in one election 
# #
# threeLeviesOneElection = twoLeviesOneElection[2:length(twoLeviesOneElection)] *
#   twoLeviesOneElection[(2:length(twoLeviesOneElection))-1]
# stopifnot(sum(threeLeviesOneElection) == 0 ) 
# 
# # View(allKeyLeviesSorted[which(threeLeviesOneElection>0),])
# 
# # 
# # allLengths = group_by( allKeyLeviesSorted, Length ) %>% summarize( numElectionsThisLength = n() )
# # allElections = group_by( allKeyLeviesSorted, election ) %>% summarize( numElections = n(),
# #                                                                        totalYes = sum(`Votes For`, na.rm=TRUE ),
# #                                                                        totalNo = sum(`Votes Against`, na.rm=TRUE ),
# # )
# # allElections$percentYes = allElections$totalYes / ( allElections$totalYes + 
# #                                                       allElections$totalNo )
# # 
# 
# allKeyLeviesSorted$competitive = allKeyLeviesSorted$previous > .5 & 
#   allKeyLeviesSorted$previous < .6 
# 
# redoElections = allKeyLeviesSorted[ which(allKeyLeviesSorted$redo & 
#                                             allKeyLeviesSorted$competitive), ]
# 
# redoByElection = group_by(allKeyLeviesSorted, redo,  election) %>%
#   summarise(numElections = n() )
# 
# 
# 
# yesVsPrevious <- lm(redoElections$percentYes ~ redoElections$previous)
# # plot( redoElections$percentYes ~ redoElections$previous )
# summary(yesVsPrevious)
# 
# 
# yesVsTrump <- lm(redoElections$percentYes ~ redoElections$percentTrump)
# # plot( redoElections$percentYes ~ redoElections$percentTrump )
# summary(yesVsTrump)
# 
# yesVsTrumpAllElections <- lm(allKeyLeviesSorted$percentYes ~ allKeyLeviesSorted$percentTrump)
# summary(yesVsTrumpAllElections)
# 
# 
# 
# yesVsRelDem <- lm(redoElections$percentYes ~ redoElections$relDemTurnout)
# # plot( redoElections$percentYes ~ redoElections$relDemTurnout )
# summary(yesVsRelDem)
# 
# yesVsRelDemPerf <- lm(redoElections$percentYes ~ redoElections$relDemPerformance)
# # plot( redoElections$percentYes ~ redoElections$relDemPerformance )
# summary(yesVsRelDemPerf)
# 
# 
# yesVsPreviousVsRelDem <- lm(redoElections$percentYes ~ redoElections$previous + redoElections$relDemPerformance )
# summary(yesVsPreviousVsRelDem)
# 
# 
# yesVsPreviousVsRelDemVsPRD <- lm(redoElections$percentYes ~ redoElections$previous + 
#                               redoElections$relDemPerformance + redoElections$previousDemPerformance)
# summary(yesVsPreviousVsRelDemVsPRD)
# 
# 
# 
# 
# 
# 
# 
# 
# yesVsPreviousVsPRD <- lm(redoElections$percentYes ~ redoElections$previous + redoElections$previousDemPerformance)
# summary(yesVsPreviousVsPRD)
# 
# # 
# # yesVsPreviousVsTerm <- lm(redoElections$percentYes ~ redoElections$previous + redoElections$Length )
# # summary(yesVsPreviousVsTerm)
# 
# yesVsPreviousVsElection <- lm(redoElections$percentYes ~ redoElections$previous + redoElections$election )
# summary(yesVsPreviousVsElection)
# 
# 
# yesVsPreviousVsTrump <- lm(redoElections$percentYes ~ redoElections$previous + redoElections$percentTrump)
# summary(yesVsPreviousVsTrump)
# 
# redoElections$Millage = as.numeric(redoElections$Millage)
# yesVsPreviousVsMillage <- lm(redoElections$percentYes ~ redoElections$previous + redoElections$Millage)
# summary(yesVsPreviousVsMillage)
# 
# ohioSchoolLevies = "https://docs.google.com/spreadsheets/d/1sOpFJeH1G4JQiER-HerZSplMpngN_MVmR1xqcok9U80/edit#gid=0"
# # spreadsheet = "https://docs.google.com/spreadsheets/d/15maCc8ujhcMqpYXDu7a3EWDZz8LN1kcl9w0KFD-pDBs/edit#gid=365319428"
# 
# write_sheet_nice(allKeyLeviesSorted, ss=ohioSchoolLevies, sheet=paste("term_",term,"_desc_",description,sep=""))
# write_sheet_nice(allElectionPerformance, ss=ohioSchoolLevies, sheet="allElectionPerformance")
# 
# allKeyLeviesSorted$quintile = floor(10*allKeyLeviesSorted$percentYes)/10
# 
# allRenewalsByResult = group_by( allKeyLeviesSorted, quintile  ) %>% summarise(numElections = n())
# 
# sd(allKeyLeviesSorted$percentYes)
# 
# allRenewalsByElection = group_by( allKeyLeviesSorted, election, redo  ) %>% summarise(numElections = n())
# # 
# # hist( redoElections$previous)
# # hist( redoElections$percentYes)
# # 
# # hist(allKeyLeviesSorted$`Votes For`[which(allKeyLeviesSorted$`Votes For` < 10000 &
# #                                             grepl("nov",allKeyLeviesSorted$election))])
# # hist(allKeyLeviesSorted$`Votes For`[which(allKeyLeviesSorted$`Votes For` < 2000 &
# #                                             grepl("may",allKeyLeviesSorted$election))])
# # hist(allKeyLeviesSorted$`Votes For`[which(  grepl("may",allKeyLeviesSorted$election))],
# #      xlab="Number of Yes Votes", main=paste("May", description,   " elections")) 
# # #
# # hist(allKeyLeviesSorted$`Votes For`[which(  grepl("may",allKeyLeviesSorted$election))],
# # xlab="Number of Yes Votes", main=paste("May", allKeyLeviesSorted$Description,   " elections")) 
# # 
# 
# load( file="turnoutAllElections.rds")
# 
# browser()
# may2017levies = allKeyLeviesSorted[which(allKeyLeviesSorted$election == "may2017"),]
# may2017leviesWturnout = merge( x=may2017levies, 
#                                y=turnoutAllElections,
#                                by.x="schoolDist",
#                                by.y="schoolDistrict",
#                                all.x=TRUE )
# 
# write_sheet_nice(may2017levies, ss=ohioSchoolLevies, sheet="may2017levies")
# write_sheet_nice(may2017leviesWturnout, ss=ohioSchoolLevies, sheet="may2017leviesWturnout")
# 
# 
# 
# 
# # ,  quintile ) 
# #
# # OK now we need to figure out how to include the information about 
# #   the mood of the election -
# #     First step - where do we compute it
# #     We want to include:
# #       Mood of this election 
# #       Mood of the election which our previous result comes from 
# #
# 
# 
# 
# 
# # 
# # 
# # passageVsTrump <- lm(allFiveYearAddsWithPres$percentYes ~ allFiveYearAddsWithPres$percentTrump)
# # 
# # spreadsheet = "https://docs.google.com/spreadsheets/d/15maCc8ujhcMqpYXDu7a3EWDZz8LN1kcl9w0KFD-pDBs/edit#gid=365319428"
# # 
# # write_sheet_nice(allFiveYearAddsWithPres, ss=spreadsheet, sheet="allFiveYearAdditions")
# # 
# # allFiveYearAddsWithPresAndMill = read_sheet_nice( ss=spreadsheet, sheet="allFiveYearAdditions")
# # 
# # passageVsMillage <- lm(allFiveYearAddsWithPresAndMill$percentYes ~ allFiveYearAddsWithPresAndMill$Mills)
# # passageVsMillVsTrump <- lm(allFiveYearAddsWithPresAndMill$percentYes ~ allFiveYearAddsWithPresAndMill$Mills
# #                            + allFiveYearAddsWithPresAndMill$percentTrump )
# # passageVsMillVsTrump <- lm(allFiveYearAddsWithPresAndMill$percentYes ~ allFiveYearAddsWithPresAndMill$Mills
# #                            + allFiveYearAddsWithPresAndMill$percentTrump + allFiveYearAddsWithPresAndMill$Permanent)
# # summary( passageVsMillVsTrump )
# # plot( passageVsMillVsTrump )
# # 
# # passageVsPermVsTrump <- lm(allFiveYearAddsWithPresAndMill$percentYes ~ 
# #                              allFiveYearAddsWithPresAndMill$percentTrump + allFiveYearAddsWithPresAndMill$Permanent)
# # summary( passageVsPermVsTrump )
# # plot( passageVsPermVsTrump )
# # 
# # 
# 
# # 
# # summary( passageVsTrump )
# # plot( passageVsTrump )
# # 
# # plot(allFiveYearAddsWithPres$percentYes ~ allFiveYearAddsWithPres$percentTrump)
# # 
# # 
# # 
# # 
# # View(resultsPlus[,c("County"  , "schoolDist" , "rerun", "percentYes"  ,     "election" ,
# #                     "previousResult"  )])
# # 
# # 
# # resultsWithPrevious = resultsPlus[ which( resultsPlus$rerun),]
# # resultsWithPreviousLM = lm( resultsWithPrevious$percentYes ~ resultsWithPrevious$previousResult + resultsWithPrevious$percentTrump )
# # resultsWithPreviousLM = lm( resultsWithPrevious$percentYes ~ resultsWithPrevious$previousResult  )
# # summary(resultsWithPreviousLM)
# # 
# # View(resultsWithPrevious)
