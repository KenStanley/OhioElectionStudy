#
# compareAcrossLevyTypes.R
#
# The newissues is worthless because by mixing all of the issues together, we are now including 
# issues that differ and hence we are not replicating the prevou match and because we are insisting on 
# the most recent election, instead of the best match, we are getting garbage. 
#
setwd("/Users/kenstanley/GitRepository/github/politicalRcode/OhioElectionStudy")

source( "includeForAll.R") # includes an rm (list=ls())
print(paste("Sys.time() = ", Sys.time()))
tic()

#
#  Hmm, where is percentTrump computed? - see SchoolDistPresResultsSummary.rds below 
#     There is a bit of an issue with percentTrump: overlapping districts. 
#     We might be able to deal with this by using group_by, but we probably ought 
#     to check it against the voter database just to be sure. Not a top priority for now
#     as percentTrump did not end up being helpful to us. 
#  And don't forget the enrollment figures that we looked up earlier 
EnrollmentFiguresSS = "https://docs.google.com/spreadsheets/d/1acDn5NyLzOGtJMC_3QdWIFi-aHHspXe_Jo66O6L8n_A/edit#gid=2004625916"
EnrollmentFiguresSheet = "may2017levies"
leanSheet = "electionCoefficients"
electionCoefficients = read_sheet_nice(ss=EnrollmentFiguresSS,sheet="electionCoefficients")
#
load(file="allIssuesWithMinScore.rds", verbose=myVerbose) 
# ugly hack 
allIssuesWithMinScoreUnique = allIssuesWithMinScore[!duplicated(allIssuesWithMinScore$index.x),]
nrow(allIssuesWithMinScoreUnique[duplicated(allIssuesWithMinScoreUnique[,c("election.x","schoolDist","percentYes.x")]),])

allIssuesWithMinScore = allIssuesWithMinScoreUnique

#
#  Inputs:
#
load(file="allElectionPerformance.rds", verbose=myVerbose) # computed in DvsRturnout - information about evey school district, this is where relDemPerformance comes from 
load(file="electionResultToVoterFileMatches.rds", verbose=myVerbose) # uniqueMatches = school district matches between voter file and election result files (excel) - matches done by hand 
load(file="turnoutAllElections.rds", verbose=myVerbose) # turnoutAllElections 
load(file="SchoolDistPresResultsSummary.rds", verbose=myVerbose) # SchoolDistPresResultsSummary and SchoolDistPresResultsFullSummary created in studySchoolBoardElections.R appear to be identical
spreadsheet = "https://docs.google.com/spreadsheets/d/1WqndSm8cuMsA_KLSSDB8IkYkgcg2k4ZFWudEckoNUiA/edit#gid=0"
# write_sheet_nice( allDateConversion, ss=spreadsheet,sheet="allDateConversion")
betterDateConversion = read_sheet_nice(  ss=spreadsheet,sheet="allDateConversion") # -  see addMoreInfo.r top commentsto see how this was created
load(file="allIssuesWindex.rds", verbose=myVerbose) # This has allIssues, not just those with a previous issue from the same district 

load(file="allIssuesWithMinNewScore.rds", verbose=myVerbose)

useNewScore = TRUE 
minScore = 0
maxScore = 12
maxNumRegs = 1e8 # 100000
printPreviousStudies = FALSE 
thisDescription = "Renewal" # "Additional" # Or "Renewal"
thisType =  "Levy" # "Levy"   # Or  "Income"
thisTime = "5"  # Or "all"  # not implemented 
thisPurpose = "Current" # or "PI"



#
#   End of Inputs:
#

source("mergeAllElectionData.R")
allIssuesWallInfoA = mergeAllElectionData ( allIssuesWithMinScore=allIssuesWithMinScore, 
                                            allElectionPerformance=allElectionPerformance, 
                                            uniqueMatches=uniqueMatches, 
                                            turnoutAllElections=turnoutAllElections, 
                                            SchoolDistPresResultsSummary=SchoolDistPresResultsSummary,
                                            betterDateConversion=betterDateConversion, 
                                            allIssuesWindex=allIssuesWindex ) 








electionLM = lm( allIssuesWallInfoA$percentYes ~ allIssuesWallInfoA$election   )
sumElectionOnly = summary(electionLM)
ECasDF = as.data.frame(sumElectionOnly$coefficients)[,1:2] %>% 
  dplyr::rename( lean = Estimate )

electionNameLength = nchar(rownames(ECasDF)[2:nrow(ECasDF)])
eLength = min(electionNameLength)
stopifnot( sd(electionNameLength) == 0 ) # This is harder if these aren't all the same length
ECasDF$election = substr( rownames(ECasDF),eLength-6,eLength) 

ECcolumns = c("election", "lean" )


allIssuesWallInfoB = merge( x=allIssuesWallInfoA, 
                            y=ECasDF[,ECcolumns],
                            by.x="election",
                            by.y="election") 

allIssuesWallInfo = merge( x=allIssuesWallInfoB, 
                           y=ECasDF[,ECcolumns],
                           by.x="election.y",
                           by.y="election") 


whichDescriptions = ( allIssuesWallInfo$Description.x == thisDescription ) | thisDescription=="all"
whichType  = ( allIssuesWallInfo$type == thisType ) | thisType=="all"
whichPurpose = ( allIssuesWallInfo$type == thisPurpose  ) | thisPurpose=="all"
whichTime = ( allIssuesWallInfo$time == thisTime  ) | thisTime=="all"

# browser()
# toc()
# print(paste("ine 153Sys.time() = ", Sys.time()))
# tic()

issuesFlag = allIssuesWallInfo$score < maxScore & allIssuesWallInfo$numRegs < maxNumRegs  & 
  allIssuesWallInfo$score > minScore & whichDescriptions & whichType & whichTime
issues = allIssuesWallInfo[which(issuesFlag),]
# issues = issues[2:nrow(issues),]

issuesAndTrump = lm( issues$percentYes ~   issues$percentTrump   )


issuesWithRelDemPerf = group_by( issues, election, relDemPerformance) %>% summarise(count=n())
issuesWithDescription = group_by( issues, Description.x) %>% summarise(count=n())


bogusPrevElInissues = issues$election==issues$previousElection
stopifnot(sum(bogusPrevElInissues,na.rm=TRUE) == 0 )


if ( printPreviousStudies) {
  basicLM = lm( issues$percentYes ~  issues$oldPrevious )
  sumBasicLM = summary(basicLM)
  rsq = round(10000*sumBasicLM$adj.r.squared)/10000
  print(paste(addedTerms," ", sumBasicLM$call[2],":: score = [",minScore,"-",maxScore,"] numRegs < ",maxNumRegs,
              "; rsq =",rsq,"; df= ",sumBasicLM$df[2],sep=""))
}

newBasicLM = lm( issues$percentYes ~  issues$percentYes.y )
sumNewBasicLM = summary(newBasicLM) # rsquared = .5022  now 0.4497 
rsq = round(1000*sumNewBasicLM$ adj.r.squared)/1000
addedTerms = ""
if ( thisDescription != "all")  addedTerms = paste(thisDescription,";",sep="")  
if ( thisType != "all")  addedTerms = paste( addedTerms, thisType, sep="" ) 

print(paste(addedTerms," ", sumNewBasicLM$call[2],":: score = [",minScore,"-",maxScore,"] numRegs < ",maxNumRegs,"; rsq =",rsq,
            "; df= ",sumNewBasicLM$df[2],sep=""))

electionLM = lm( issues$percentYes ~  issues$percentYes.y + issues$election)
sumElectionLM = summary(electionLM) # rsquared = .5022  now 0.4497 

columnsToCompare = c("index" , "election" ,"schoolDist" ,  "election.y"     ,  "previousElection"    ,   "percentYes.y" , 
                     "oldPrevious" , "index.y"  )


newBetterLM = lm( issues$percentYes ~  issues$percentYes.y + issues$lean.x)
sumNewBetterLM = summary(newBetterLM) # rsquared = .5022  now 0.4497 
rsq = round(1000*sumNewBetterLM$ adj.r.squared)/1000
print(paste(addedTerms," ", sumNewBetterLM$call[2],":: score = [",minScore,"-",maxScore,"] numRegs < ",maxNumRegs,"; rsq =",rsq,
            "; df= ",sumNewBetterLM$df[2],sep=""))
#

newBestLM = lm( issues$percentYes ~  issues$percentYes.y + issues$lean.x
                +  issues$lean.y)
sumNewBestLM = summary(newBestLM) # rsquared = .5022  now 0.4497 
rsq = round(1000*sumNewBestLM$ adj.r.squared)/1000
print(paste(addedTerms," ", sumNewBestLM$call[2],":: score = [",minScore,"-",maxScore,"] numRegs < ",maxNumRegs,"; rsq =",rsq,
            "; df= ",sumNewBestLM$df[2],sep=""))


toc()
print(paste("ine 212 Sys.time() = ", Sys.time()))


colnames(allIssuesWallInfo)

allIssueMatches = allIssuesWallInfo %>% 
  dplyr::rename( Description = Description.x) %>% 
  dplyr::rename( Purpose = purposeNow) %>% 
  dplyr::rename( purpose.y = purposeNow.y) %>% 
  dplyr::rename( Time = time ) %>% 
  dplyr::rename( PercentYes = percentYes ) %>% 
  dplyr::rename( description.y = Description.y ) %>% 
  dplyr::rename( Lean = lean.x) 

columnsToPrint = c(  "County"   ,  "schoolDist"  , "percentTrump"   ,   "numRegs" , 
                     "score"  ,
                     "index"   ,  "election"  ,  "Time"  , "Description" ,  "Purpose"  , "type"  , "Lean", "PercentYes" ,     
                     "index.y" , "election.y" ,  "time.y",  "description.y", "purpose.y" ,"type.y",  "lean.y"  , "percentYes.y"  )

setdiff(columnsToPrint, colnames(allIssueMatches) )
setdiff(colnames(allIssueMatches) , columnsToPrint)

allMatches = allIssueMatches[,columnsToPrint]

allMatches$score  = as.numeric( allMatches$score )

releaseSpreadsheet = "https://docs.google.com/spreadsheets/d/1EnEvJ-P4CC4gZpOr-3Dgni2gKwG17ZudjgdJwG88YxI/edit"
# write_sheet_nice(allMatches,ss=releaseSpreadsheet, sheet="allMatches")





#
# The idea of using one set of data to model and one to test
# is complicated by the fact that we use just one election to measure the lean
# and two elections to predict 
#
modelSize = 0.5

set.seed(125678)

source("computeLean.R")
# lean = computeLean( modelIssues=allIssues )
leanByElection = computeLean( modelIssues=allIssuesWindex )

allElectionLeans = unique( leanByElection$election)
allElections = unique( allIssuesWindex$election)

setdiff( allElections, allElectionLeans)
setdiff( allElectionLeans, allElections)

#
# In what appears to be blind luck, the one election that lm() drops is
# aug2018 - which is an election that we don't care about 
#
stopifnot( setdiff( allElections, allElectionLeans) == "aug2018")

medianLean = median( leanByElection$lean )
firstQlean = median(leanByElection$lean[which(leanByElection$lean<medianLean)])
thirdQlean = median(leanByElection$lean[which(leanByElection$lean>=medianLean)])


leanByElection$leanQuartile = 1
leanByElection$leanQuartile[leanByElection$lean > firstQlean ] = 2
leanByElection$leanQuartile[leanByElection$lean > medianLean ] = 3
leanByElection$leanQuartile[leanByElection$lean > thirdQlean ] = 4

issuesWQ_A = merge( issues, 
                    leanByElection[,c("election" , "leanQuartile")],
                    by="election")
issuesWQ = merge( x=issuesWQ_A, 
                  y=leanByElection[,c("election" , "leanQuartile")],
                  by="election.y",
                  by.y="election")
browser()

group_by( issuesWQ, leanQuartile.x ) %>% summarise(count=n()) 
group_by( issuesWQ, leanQuartile.y ) %>% summarise(count=n()) 

modelIssues = runif( nrow(issuesWQ) ) < modelSize 
testIssues = !modelIssues
# 
model = issuesWQ[modelIssues,]
test = issuesWQ[testIssues,]

group_by( model, leanQuartile.x ) %>% summarise(count=n()) 
group_by( test, leanQuartile.x ) %>% summarise(count=n()) 

leanLM = lm( model$percentYes ~ model$leanQuartile.x )
#
#  The group_by above shows that over half of the previous leans
#  are in the bottom quartile of leans, that may explain why 
#  the relationship with the previous lean is weak and in the
#  wrong direction 
#
twoLeansLM = lm( model$percentYes ~ model$leanQuartile.x + model$leanQuartile.y )

leanPlusLM  = lm( model$percentYes ~  model$percentYes.y + model$leanQuartile.x )
lean2PlusLM  = lm( model$percentYes ~  model$percentYes.y + model$leanQuartile.x + model$leanQuartile.y )

# leanPlusLM  = lm( percentYes ~  percentYes.y + leanQuartile.x, data=model[,c("percentYes","percentYes.y","leanQuartile.x")]  )
# leanPlusLM2  = lm( percentYes ~  percentYes.y + leanQuartile.x, data=model  )


meanPercentYes = mean( model$percentYes)

basicLM  = lm( percentYes ~  percentYes.y , data=model[,c("percentYes","percentYes.y","leanQuartile.x")]  )

leanLM  = lm( percentYes ~  percentYes.y + leanQuartile.x, data=model[,c("percentYes","percentYes.y","leanQuartile.x")]  )

bothLeansLM  = lm( percentYes ~  percentYes.y + leanQuartile.x + leanQuartile.y, data=model[,c("percentYes","percentYes.y","leanQuartile.x","leanQuartile.y")]  )


browser()
model$pred <- predict(leanPlusLM, newdata=model)
variance =sum( (model$percentYes - meanPercentYes)^2 )
residualVariance = sum( (model$percentYes - model$pred )^2)

rsquared = ( variance - residualVariance ) / variance


# countyByParty = group_by( aSWVF, COUNTY_NUMBER, PARTY_AFFILIATION ) %>% summarize( count = n() )
# 
# try  = reshape2::dcast(countyByParty,  COUNTY_NUMBER  ~  PARTY_AFFILIATION, value.var="count" )
# 
# write_sheet_nice( try, ss=releaseSpreadsheet, sheet="CountyPartyRegs")

# leahffcdp@gmail.com

