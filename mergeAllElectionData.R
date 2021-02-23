#
# mergeAllElectionData.R
#
# Merges data from 
# allIssuesWithMinScore - all issues with the best most recent match
# allElectionPerformance - 
# uniqueMatches, turnoutAllElections, SchoolDistPresResultsSummary,
# and betterDateConversion
#
mergeAllElectionData <- function( allIssuesWithMinScore, allElectionPerformance, 
                                  uniqueMatches, turnoutAllElections, SchoolDistPresResultsSummary,
                                  betterDateConversion, allIssuesWindex ) {
  
  presResulCommonColumns = c("County"  ,"Trump"  ,    "Biden" , "percentTrump"   )
 
  citySchoolDistPresResults = SchoolDistPresResultsSummary %>% group_by(CITY_SCHOOL_DISTRICT  ) %>%
    summarize( Biden = sum(Biden ), Trump = sum(Trump) )  %>% 
    dplyr::rename( schoolDist = CITY_SCHOOL_DISTRICT)
  exemptedSchoolDistPresResults = SchoolDistPresResultsSummary %>% group_by(EXEMPTED_VILL_SCHOOL_DISTRICT  ) %>%
    summarize( Biden = sum(Biden ), Trump = sum(Trump) ) %>%
    dplyr::rename( schoolDist = EXEMPTED_VILL_SCHOOL_DISTRICT)
  localSchoolDistPresResults = SchoolDistPresResultsSummary %>% group_by(LOCAL_SCHOOL_DISTRICT  ) %>%
    summarize( Biden = sum(Biden ), Trump = sum(Trump) )  %>%
    dplyr::rename( schoolDist = LOCAL_SCHOOL_DISTRICT)
  #
  
  # exemptedSchoolDistPresResults = 
  #   SchoolDistPresResultsSummary[,c(presResulCommonColumns,"EXEMPTED_VILL_SCHOOL_DISTRICT" )] %>%
  #   dplyr::rename( schoolDist = EXEMPTED_VILL_SCHOOL_DISTRICT)
  # localSchoolDistPresResults = 
  #   SchoolDistPresResultsSummary[,c(presResulCommonColumns,"LOCAL_SCHOOL_DISTRICT" )] %>%
  #   dplyr::rename( schoolDist = LOCAL_SCHOOL_DISTRICT)
  # 
  allSchoolDistPresResultsA = rbind(citySchoolDistPresResults, exemptedSchoolDistPresResults,
                                    localSchoolDistPresResults)
  
  allSchoolDistPresResults = allSchoolDistPresResultsA[which(nchar(as.character(allSchoolDistPresResultsA$schoolDist)) > 2 ),]
  allSchoolDistPresResults$percentTrump = allSchoolDistPresResults$Trump / 
    (allSchoolDistPresResults$Trump + allSchoolDistPresResults$Biden )
  
  #
  stopifnot( sum(duplicated(allIssuesWithMinScoreUnique[,c("election.x","schoolDist","percentYes.x")]))==0)
  
  # The only school district that we are throwing out is Ledgemont which no onger exists
  
  allIssuesWithPercentTrump = merge(allIssuesWithMinScore, allSchoolDistPresResults[,c("schoolDist","percentTrump")],by="schoolDist" ) 
  # allIssuesWithoutPercentTrump = anti_join(allIssuesWithMinScore, allSchoolDistPresResults[,c("schoolDist","percentTrump")],by="schoolDist" ) 
  
  allIssuesAll = allIssuesWithPercentTrump  %>% 
    dplyr::rename( index = index.x ) %>%
    dplyr::rename( election = election.x ) %>% 
    dplyr::rename( percentYes = percentYes.x ) %>% 
    dplyr::rename( eDate = eDate.x ) %>% 
    dplyr::rename( `Question Type` = `Question Type.x` ) %>% 
    dplyr::rename( Length = Length.x ) %>% 
    dplyr::rename( time = time.x ) %>% 
    dplyr::rename( type = type.x ) %>%
    dplyr::rename( purposeNow = purposeNow.x ) 
  
  allIssuesColumns = c( "index"    , "score"   , "County" , "Subdivision Name" ,  "schoolDist"  , "percentTrump",
                        "election"    ,  "eDate" , "Description.x" ,"percentYes" , "purposeNow" , "time" , "time.y" ,
                        "Length"      ,   "type" , "type.y","index.y", "Description.y", "election.y" , "purposeNow.y", "percentYes.y"   )   
  allIssues = allIssuesAll[,allIssuesColumns]
  
  #
  #  This just double checks that we have the right schoolDist
  #
  stopifnot(nrow( merge( allIssuesAll, uniqueMatches)) == nrow( allIssuesAll) ) 
  
  electionPerformanceColumns = c("electionDate" ,"demPerformance" ,"relDemPerformance", "relDemTurnout" ) # election = electionDate
  
  electionPerformance = allElectionPerformance[,electionPerformanceColumns]
  
  
  turnoutAllElections$numBallots = turnoutAllElections$demBallots + turnoutAllElections$repBallots + turnoutAllElections$unaBallots
  turnoutAllElections$numVoters = turnoutAllElections$demVoters + turnoutAllElections$repVoters + turnoutAllElections$unaVoters
  turnoutAllElections$numRegs = turnoutAllElections$Dreg + turnoutAllElections$Rreg + turnoutAllElections$Ureg
  
  turnoutAllElections$percentDemVotes = turnoutAllElections$demVoters / turnoutAllElections$numVoters
  turnoutAllElections$percentDemRegs = turnoutAllElections$Dreg / turnoutAllElections$numRegs
  turnoutAllElections$percentDemBallots = turnoutAllElections$demBallots / turnoutAllElections$numBallots
  
  turnoutColumns = c( "schoolDistrictField",  "election"  , 
                      "percentDemVotes",   "percentDemRegs", "percentDemBallots", "numRegs"  ) 
  
  turnoutAllElectionsBetter = turnoutAllElections[,turnoutColumns] %>%
    dplyr::rename( SWVFdate = election ) %>%
    dplyr::rename( schoolDist = schoolDistrictField ) 
  
  
  localTurnout = merge(x=turnoutAllElectionsBetter,
                       y=betterDateConversion, 
                       by="SWVFdate") 
  
  stopifnot(nrow(localTurnout) == nrow(turnoutAllElectionsBetter))
  
  
  # write_sheet_nice(SchoolDistPresResultsSummary,ss=EnrollmentFiguresSS,sheet="Temp") - this is how we know that SchoolDistPresResultsSummary has overlapping districts
  
  #
  # Now we put it all together 
  #
  # First, what do we want to pull together from the previous election?
  #    relDemTurnout and relDemTurnout for sure 
  # 
  
  # indexByElectionAndSD = allIssues[,c("index"  , "election"  , "County" , "Subdivision Name"  )]
  
  previousElectionPerformance = electionPerformance %>%
    dplyr::rename(  newPrevDemPerformance = demPerformance) %>%
    dplyr::rename(  newPrevRelDemPerformance = relDemPerformance) %>%
    dplyr::rename(  newPrevRelDemTurnout = relDemTurnout) 
  
  nrow(allIssues[duplicated(allIssues[,c("election","schoolDist","percentYes")]),])
  
  
  setdiff( c("election","schoolDist","percentYes") , colnames(allIssues))
  
  
  allIssuesWPrevEP = merge( x=allIssues, y=previousElectionPerformance,
                            by.x="election.y", 
                            by.y="electionDate")
  
  
  allIssuesWbothEP = merge( x=allIssuesWPrevEP, y=electionPerformance,
                            by.x="election", 
                            by.y="electionDate")
  
  # colsToView = c("election" , "demPerformance"   ,  "relDemPerformance" , "relDemTurnout"    )  
  #   
  # View(allIssuesWbothEP[,colsToView])
  
  
  #  There are many interesting columns in allIssuesWbothEP, we just use numRegs as
  #  a proxy for the size of the district. 
  allIssuesWlocal = merge(x=allIssuesWbothEP,
                          y=localTurnout[,c("election","schoolDist","numRegs")],
                          by=c("election","schoolDist")) 
  
  
  #
  #   Here we want to recreate the previous result showing the relDemPerformance and 
  #      newPrevRelDemPerformance improved results significantly (though the effect size is modest)
  # 
  #
  #  Great! I like this double check 
  #
  if ( 0 ) { 
    issuesToRecoverPercentYes = allIssuesWindex[,c("index","percentYes")] %>% 
      dplyr::rename( percentYesRecovered = percentYes )
    
    doubleCheckPrevPercentYesRecovered = merge( x=allIssuesWlocal, 
                                                y=issuesToRecoverPercentYes,
                                                by.x=c("index.y","percentYes.y"),
                                                by.y=c("index","percentYesRecovered") ) 
    
    stopifnot(nrow(doubleCheckPrevPercentYesRecovered)==nrow(allIssuesWlocal)) 
    doubleCheckPercentYesRecovered = merge( x=allIssuesWlocal, 
                                            y=issuesToRecoverPercentYes,
                                            by.x=c("index","percentYes"),
                                            by.y=c("index","percentYesRecovered") ) 
    
    stopifnot(nrow(doubleCheckPercentYesRecovered)==nrow(allIssuesWlocal)) 
    browser()
    
  } 
  
  # nrow(allIssues) # loses two issues from Ledgemont which is no longer a school district 
  # nrow(allIssuesWbothEP)
  # nrow(allIssuesWithMinScore)
  # 
  allIssuesWithMinScoreA = allIssuesWithMinScore %>% 
    dplyr::rename( index = index.x )
  missingIssues = anti_join(allIssues,allIssuesWPrevEP,
                            by="index")
  
  missingElections = group_by( missingIssues, election.y) %>% summarise(count=n())
  
  
  return <- allIssuesWlocal
  
  
}

