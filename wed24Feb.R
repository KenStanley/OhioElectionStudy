#
# wed24Feb.R
#
#
# This was run Wednesday, February 24, 2021 with the output used in the 
#
setwd("/Users/kenstanley/GitRepository/github/politicalRcode/OhioElectionStudy")


startClean = FALSE

if ( startClean) {
  source( "includeForAll.R") # includes an rm (list=ls())
  useLocalRdsFiles = TRUE  
} else {
  useLocalRdsFiles = FALSE # This is an abuse of the intent here   
  
}

#
#  Inputs:
#
source("myPrint.R")

printRsummary = FALSE 
printRinfo= FALSE

filename = "wed24Feb2021"
if (printRinfo) filename = paste(filename,"wInfo",sep="")
if (printRsummary) filename = paste(filename,"wRsummary",sep="")
filename = paste(filename,".txt",sep="")
sink(filename)
print(paste("Run:",Sys.time()))
if( useLocalRdsFiles ) {
  
  load(file="electionCoefficients.rds",verbose=myVerbose) # "electionCoefficients" 
  load(file="allIssuesWallInfo.rds",verbose=myVerbose)
  load(file="allElectionPerformance.rds", verbose=myVerbose) # "election performance" # computed in DvsRturnout - information about evey school district, this is where relDemPerformance comes from 
  load(file="electionResultToVoterFileMatches.rds", verbose=myVerbose) # "unique matches" #  uniqueMatches = school district matches between voter file and election result files (excel) - matches done by hand 
  load(file="turnoutAllElections.rds", verbose=myVerbose) # "election turnout" # turnoutAllElections 
  load(file="SchoolDistPresResultsSummary.rds", verbose=myVerbose) # SchoolDistPresResultsSummary and SchoolDistPresResultsFullSummary created in studySchoolBoardElections.R appear to be identical
  load(file="betterDateConversion.rds", verbose=myVerbose) # allDateConversion # converts the three types of election date that we use: nov2013	GENERAL.11.05.2013	11/1/2013
  load(file="allIssuesWindex.rds", verbose=myVerbose) # "all issues"  # This has allIssues, not just those with a previous issue from the same district 
} else {
  # not implemented yet - read the data from supplementalDataSpreadsheet
}
#
#   End of Inputs:
#

#
#  First we want to look at correlation with percentTrump 
#  For this we may want a fairly large input as we are not 
#  using the matching previous election
#

minScore = 0 # score refers to how closely the previous ballot issue iss to the current ballot issue
maxScore = 8000
maxNumRegs = 100000000 # number of voter registrations is a measure of the size of the district
thisDescription = "Additional" # "Renewal" #  "Renewal" # "Additional" # Or "Renewal"
thisType = "all" # "Levy" # "Levy"   # Or  "Income"
thisTime = "all"  # "5"  # Or "all"  
thisPurpose = "all" # "Current" # Current" # or "PI"

#
# Now we choose which ballot issues we want to study. 
#
source( "chooseIssuesToStudy.R")

issuesFlag = chooseIssuesToStudy( allIssues=allIssuesWallInfo, minScore = minScore , 
                                  maxScore = maxScore, 
                                  maxNumRegs = maxNumRegs ,
                                  thisDescription = thisDescription, 
                                  thisType = thisType,
                                  thisTime = thisTime,
                                  thisPurpose  = thisPurpose )
stopifnot( sum(is.na(issuesFlagCheck)) == 3) # I should look into these 3 sometime
issues = allIssuesWallInfo[which(issuesFlag),]

if ( length(unique(issues$Description.x))> 1 ) { 
  issuesAndTrump = lm( issues$percentYes ~   issues$percentTrump   + issues$Description.x  ) # + issues$purposeNow
} else {
  issuesAndTrump = lm( issues$percentYes ~   issues$percentTrump ) # + issues$purposeNow
}

trumpSum = summary(issuesAndTrump)
trumpLower = trumpSum$coefficients[2,1] - 2*trumpSum$coefficients[2,2]
trumpUpper = trumpSum$coefficients[2,1] + 2*trumpSum$coefficients[2,2]

myPrint("")


rsq = round(trumpSum$adj.r.squared,9)
rsq = round(trumpSum$r.squared,9)
residStdError = round(sd(trumpSum$residuals),9)

sdNoModel = sqrt(1-sd(issues$percentYes)^2) - residStdError

sqrt(sd(issues$percentYes)^2*(1-rsq))  - residStdError


issues$predict = predict(issuesAndTrump,issues)
issues$myResidual = issues$percentYes - issues$predict
# View( issues[,c("percentYes","predict","myResidual")])
head(issues$myResidual)
head(trumpSum$residuals)
min(issues$myResidual - trumpSum$residuals)
issues$minimalModelResid = issues$percentYes - mean(issues$percentYes)
sd(issues$minimalModelResid)
sd(issues$myResidual)

myPrint(paste("A simple model based solely on the percentage of the vote that Trump won in the district in 2020 yields a Trump coefficient on ", thisDescription,
            " levies = ",round(trumpSum$coefficients[2,1],3),
            " confidence interval=[",round(trumpLower,3),",",round(trumpUpper,3),"]",sep="")) 

myPrint(paste("One way to understand this Trump coefficient (which needs a better name) ",
            "is that for every 10% increase in Trump support, ",
            "we can expect a ",round(trumpSum$coefficients[2,1]*10,2),
            "% decrease in the number of Yes votes, on average on ",thisDescription,"s.",sep=""))

myPrint(paste("This simple model does not improve much on the minimalist model (anticipating exactly the mean of all levies) on ", 
            thisDescription," elections. It reduces the standard error from ",
            round(sd(issues$percentYes)*100,2),"% to ",
            round(sd(trumpSum$residuals)*100,2),"%.",
            sep=""))

if( printRsummary ) print(trumpSum)


issuesTrumpAndElection = lm( issues$percentYes ~   issues$percentTrump + 
                               issues$election + issues$purposeNow +
                               issues$time + issues$type ) # + issues$Description.x +issues$purposeNow
trumpSum = summary(issuesTrumpAndElection)
trumpLower = trumpSum$coefficients[2,1] - 2*trumpSum$coefficients[2,2]
trumpUpper = trumpSum$coefficients[2,1] + 2*trumpSum$coefficients[2,2]

myPrint(paste("A more sophisticated model which includes the date (and hence lean) of the election, the type of ballot (Levy, Bond, or Income), ",
            "purpose of the ballot (Current or PI) and the term (often 5 years)"," yields a Trump coefficient on ", thisDescription,
            " levies of ",round(trumpSum$coefficients[2,1],3),
            " confidence interval=[",round(trumpLower,3),",",round(trumpUpper,3),"]",sep="")) 


myPrint(paste("This model still fails to reduce the standard much on ",
            thisDescription," elections. It reduces the standard error from ",
            round(sd(issues$percentYes)*100,2),"% to ",
            round(sd(trumpSum$residuals)*100,2),"%.",
            sep=""))

if( printRsummary ) print(trumpSum)


myPrint(paste("Fortunately, we have a much more powerful predictor: results from previous elections in the district."))

myPrint(paste("We now switch our attention from Additional to Renewals because Renewals are more likely to have a previous election to compare against and there are more Renewals, offering our study more power.",
            " The first thing we notice is that the partisan effect on Renewals is less pronounced."))

thisDescription = "Renewal"
issuesFlag = chooseIssuesToStudy( allIssues=allIssuesWallInfo, minScore = 0 , 
                                  maxScore = 10000, 
                                  maxNumRegs = 1000000 ,
                                  thisDescription = thisDescription, 
                                  thisType = "all",
                                  thisTime = "all",
                                  thisPurpose  = "all" )
issues = allIssuesWallInfo[which(issuesFlag),]


issuesTrumpAndElection = lm( issues$percentYes ~   issues$percentTrump + 
                               issues$election + issues$purposeNow +
                               issues$time + issues$type ) # + issues$Description.x +issues$purposeNow
trumpSum = summary(issuesTrumpAndElection)
trumpLower = trumpSum$coefficients[2,1] - 2*trumpSum$coefficients[2,2]
trumpUpper = trumpSum$coefficients[2,1] + 2*trumpSum$coefficients[2,2]

myPrint(paste("The sophisticated model yields a Trump coefficient on ", thisDescription,
            " levies = ",round(trumpSum$coefficients[2,1],3),
            " confidence interval=[",round(trumpLower,3),",",round(trumpUpper,3),"]",sep="")) 


myPrint(paste("On ",thisDescription," levies, this model reduces the standard error from ",
            round(sd(issues$percentYes)*100,2),"% to ",
            round(sd(trumpSum$residuals)*100,2),"%.",
            sep=""))

if( printRsummary ) print(trumpSum)


#
# Now we look at those issues with a comparable previous issue, focusing on Renewals 
#

issuesFlag = chooseIssuesToStudy( allIssues=allIssuesWallInfo, minScore = 0 , 
                                  maxScore = 999999, 
                                  maxNumRegs = 1000000 ,
                                  thisDescription = "Renewal", 
                                  thisType = "all",
                                  thisTime = "all",
                                  thisPurpose  = "all" )
issues = allIssuesWallInfo[which(issuesFlag),]

newBasicLM = lm( issues$percentYes ~  issues$percentYes.y )
sumNewBasicLM = summary(newBasicLM) # rsquared = .5022  now 0.4497 
rsq = round(1000*sumNewBasicLM$adj.r.squared)/1000
residStdError = round(1000*sd(newBasicLM$residuals))/1000
addedTerms = ""
if ( thisDescription != "all")  addedTerms = paste(thisDescription,";",sep="")  
if ( thisType != "all")  addedTerms = paste( addedTerms, thisType, sep="" ) 

if (printRinfo) myPrint(paste(addedTerms," ", sumNewBasicLM$call[2],":: score = [",minScore,"-",maxScore,"] numRegs < ",maxNumRegs,"; rsq =",rsq,
            "; df= ",sumNewBasicLM$df[2],"; sd(resid)= ",residStdError, sep=""))



myPrint(paste("A model based strictly on the most similar previous ballot issue for this district ",
            "for all ",
            thisDescription," elections reduces the standard error from ",
            round(sd(issues$percentYes)*100,2),"% to ",
            round(sd(sumNewBasicLM$residuals)*100,2),"%.",
            sep=""))

if( printRsummary ) print(sumNewBasicLM)


myPrint("")
myPrint("")
myPrint(paste("We now further limit our study to Renewals with a previous ballot issue of the same type (Levy, Bond or Income) ",
            ", the same purpose (Current or P) and a similar term.",sep=""))


issuesFlag = chooseIssuesToStudy( allIssues=allIssuesWallInfo, minScore = 0 , 
                                  maxScore = 8, 
                                  maxNumRegs = 1000000 ,
                                  thisDescription = "Renewal", 
                                  thisType = "all",
                                  thisTime = "all",
                                  thisPurpose  = "all" )
issues = allIssuesWallInfo[which(issuesFlag),]

newBasicLM = lm( issues$percentYes ~  issues$percentYes.y )
sumNewBasicLM = summary(newBasicLM) # rsquared = .5022  now 0.4497 
rsq = round(1000*sumNewBasicLM$adj.r.squared)/1000
residStdError = round(1000*sd(newBasicLM$residuals))/1000
addedTerms = ""
if ( thisDescription != "all")  addedTerms = paste(thisDescription,";",sep="")  
if ( thisType != "all")  addedTerms = paste( addedTerms, thisType, sep="" ) 

if (printRinfo) myPrint(paste(addedTerms," ", sumNewBasicLM$call[2],":: score = [",minScore,"-",maxScore,"] numRegs < ",maxNumRegs,"; rsq =",rsq,
            "; df= ",sumNewBasicLM$df[2],"; sd(resid)= ",residStdError, sep=""))



myPrint(paste("A model based strictly on a previous ballot issue of the same type and purpose ",
            "for all ",
            thisDescription," elections reduces the standard error from ",
            round(sd(issues$percentYes)*100,2),"% to ",
            round(sd(sumNewBasicLM$residuals)*100,2),"%.",
            sep=""))

if( printRsummary ) print(sumNewBasicLM)



myPrint("Now we add to this model an estimate of the lean of the election.")

newBetterLM = lm( issues$percentYes ~  issues$percentYes.y + issues$lean2.x)
sumNewBetterLM = summary(newBetterLM) # rsquared = .5022  now 0.4497 
rsq = round(1000*sumNewBetterLM$ adj.r.squared)/1000
if (printRinfo) myPrint(paste(addedTerms," ", sumNewBetterLM$call[2],":: score = [",minScore,"-",maxScore,"] numRegs < ",maxNumRegs,"; rsq =",rsq,
            "; df= ",sumNewBetterLM$df[2],sep=""))
#


if (printRinfo) myPrint(paste(addedTerms," ", sumNewBetterLM$call[2],":: score = [",minScore,"-",maxScore,"] numRegs < ",maxNumRegs,"; rsq =",rsq,
                            "; df= ",sumNewBetterLM$df[2],"; sd(resid)= ",residStdError, sep=""))



myPrint(paste("Adding the lean of the election in which the ballot measure is run ",
            "for all ",
            thisDescription," elections reduces the standard error from ",
            round(sd(issues$percentYes)*100,2),"% to ",
            round(sd(sumNewBetterLM$residuals)*100,2),"%.",
            sep=""))

if( printRsummary ) print(sumNewBetterLM)


myPrint("")
myPrint("")
myPrint("We can make this model even better by adding an estimate of the lean of the previous election against which we compare.")


newBestLM = lm( issues$percentYes ~  issues$percentYes.y + issues$lean2.x
                +  issues$lean2.y)
sumNewBestLM = summary(newBestLM) # rsquared = .5022  now 0.4497 
rsq = round(1000*sumNewBestLM$ adj.r.squared)/1000


myPrint(paste("Adding the leans of both the election in which the ballot measure is run and the previous election against which we compare",
            "for all ",
            thisDescription," elections reduces the standard error from ",
            round(sd(issues$percentYes)*100,2),"% to ",
            round(sd(sumNewBestLM$residuals)*100,2),"%.",
            sep=""))


if (printRinfo) myPrint(paste(addedTerms," ", sumNewBestLM$call[2],":: score = [",minScore,"-",maxScore,"] numRegs < ",maxNumRegs,"; rsq =",rsq,
            "; df= ",sumNewBestLM$df[2],sep=""))

if( printRsummary ) print( sumNewBestLM)
# 
# oneLM = lm( issues$percentYes ~  issues$percentYes.y + issues$lean2.x ) 
# summary(oneLM)
# 
# 
# 
# columnsToCompare = c("index" , "election" ,"schoolDist" ,  "election.y"     ,  "previousElection"    ,   "percentYes.y" , 
#                      "oldPrevious" , "index.y"  )
# colnames(allIssuesWallInfo)
# 
# allIssueMatches = allIssuesWallInfo %>% 
#   dplyr::rename( Description = Description.x) %>% 
#   dplyr::rename( Purpose = purposeNow) %>% 
#   dplyr::rename( purpose.y = purposeNow.y) %>% 
#   dplyr::rename( Time = time ) %>% 
#   dplyr::rename( PercentYes = percentYes ) %>% 
#   dplyr::rename( description.y = Description.y ) %>% 
#   dplyr::rename( Lean = lean.x) 
# 
# columnsToPrint = c(  "County"   ,  "schoolDist"  , "percentTrump"   ,   "numRegs" , 
#                      "score"  ,
#                      "index"   ,  "election"  ,  "Time"  , "Description" ,  "Purpose"  , "type"  , "Lean", "PercentYes" ,     
#                      "index.y" , "election.y" ,  "time.y",  "description.y", "purpose.y" ,"type.y",  "lean.y"  , "percentYes.y"  )
# 
# setdiff(columnsToPrint, colnames(allIssueMatches) )
# setdiff(colnames(allIssueMatches) , columnsToPrint)
# 
# allMatches = allIssueMatches[,columnsToPrint]
# 
# allMatches$score  = as.numeric( allMatches$score )
# supplementalDataSpreadsheet
# # write_sheet_nice(allMatches,ss=releaseSpreadsheet, sheet="allMatches")
# 
# 
# 
sink()




