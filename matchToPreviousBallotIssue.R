#
#  matchToPreviousBallotIssue.R 
#
#    Takes one second to run 
#
#    Categorizes all issues in each of the following areas:
#        time = Length of time (in years) with continuing oeriod of time set to 40 and multiple times set to 37
#        type = "Levy", "Income", "Bond" [less common: "Combo", "Misc", "Sales"]
#        Description = "Renewal", "Additional", "Replacement" or "Other" 
#        purposeNow = "Current" or "PI"
#    Matches all issues against all other issues, scores them all and looks for the
#      a comparable ballot issue from the past. 
#        
#
#   Creates:
#     allIssuesWindex.rds - all issues 
#     allIssuesWithMinNewScore.rds - Chooses most recent levy 
#     allIssuesWithMinScore.rds - Chooses most recent similar levy
#
#

setwd("/Users/kenstanley/GitRepository/github/politicalRcode/OhioElectionStudy")

source( "includeForAll.R")

print(paste("Sys.time() = ", Sys.time()))
tic()
load(file="allIssues.rds", verbose=myVerbose)
load(file="electionResultToVoterFileMatches.rds", verbose=myVerbose) # uniqueMatches = school district matches between voter file and election result files (excel)

# "schoolDist" is no longer relevant as the uniqueMatches in matches4.rds is new and improved.
relevantColumns = c( "index", "election","eDate", "percentYes", "County"   , 
                     "Subdivision Name" , "Subdivision Type"  , "Question Type" ,
                     "purposeNow" ,  "Purpose",  "Description"  , "Millage"    ,
                     "Percent"   , "Dollar Amount"  ,  "Length" ,
                     "Commencing Year or Effective Date", "time",
                     "type")


columnsHere = c("County"   , "percentYes", 
                "Subdivision Type"  ,    "Question Type" ,
                "Purpose"   ,
                "Description" ,   "Millage" ,
                "Percent"    ,     "Dollar Amount"    )

#
# Several issues fro Licking Heights were duplicated - I never studied why, I just eliminate them here 
#
allIssues = allIssues[!duplicated(allIssues[,columnsHere]),]
allIssues$index = 1e5 + 1:nrow(allIssues)

#
# This treats continuing levies as 40 years and levies with multiple maturities
# as 37 years. The latter is not accurate, but is also not common enough to bother with. 
# 
unchangedVal=100
cptVal = 40
multipleVal = 37 

allIssues$time = unchangedVal
allIssues$time[grepl("c",allIssues$Length,ignore.case = TRUE)>0] = cptVal 
allIssues$time[grepl("pt",allIssues$Length,ignore.case = TRUE)>0] = cptVal 
allIssues$time[grepl("/",allIssues$Length)] = multipleVal
unchanged = allIssues$time == unchangedVal
allIssues$time[unchanged] = as.numeric( allIssues$Length[unchanged]) +0 
stopifnot( sum(is.na(allIssues$time)) < 10 ) # There are 9 of these as Feb 2021, we just treat them as cpt because we have to do something with them. 
allIssues$time[is.na(allIssues$time)] = cptVal 
# View(unique(allIssues[,c("Length","time")]))

allIssues$type = "Unknown"
allIssues$type[grepl("Levy",allIssues$`Question Type`,ignore.case = TRUE)] = "Levy"
allIssues$type[grepl("Combo",allIssues$`Question Type`,ignore.case = TRUE)] = "Combo"
allIssues$type[grepl("Bond",allIssues$`Question Type`,ignore.case = TRUE)] = "Bond"
allIssues$type[grepl("Income",allIssues$`Question Type`,ignore.case = TRUE)] = "Income"
allIssues$type[grepl("Misc",allIssues$`Question Type`,ignore.case = TRUE)] = "Misc"
allIssues$type[grepl("Sales",allIssues$`Question Type`,ignore.case = TRUE)] = "Sales"

# allIssues$desc = "Unknown"
allIssues$Description[grepl("crease",allIssues$Description,ignore.case = TRUE)] = "Replacement"
allIssues$Description[grepl("ubstit",allIssues$Description,ignore.case = TRUE)] = "Replacement"
allIssues$Description[!(allIssues$Description == "Renewal" | 
                          allIssues$Description == "Additional" | 
                          allIssues$Description == "Replacement" )] = "Other" 


# View(unique(allIssues[,c("Purpose")]))
allTypes = group_by(allIssues,type) %>% summarise(count=n())
# View(allTypes)

allIssues$purposeNow = "Unknown"
allIssues$purposeNow[grepl("district",allIssues$Purpose,ignore.case = TRUE)] = "Current"
allIssues$purposeNow[grepl("safety",allIssues$Purpose,ignore.case = TRUE)] = "Current"
allIssues$purposeNow[grepl("perm",allIssues$Purpose,ignore.case = TRUE)] = "PI"
allIssues$purposeNow[grepl("build",allIssues$Purpose,ignore.case = TRUE)] = "PI"
allIssues$purposeNow[grepl("acquire",allIssues$Purpose,ignore.case = TRUE)] = "PI"
allIssues$purposeNow[grepl("construct",allIssues$Purpose,ignore.case = TRUE)] = "PI"
allIssues$purposeNow[grepl("maintain",allIssues$Purpose,ignore.case = TRUE)] = "PI"
allIssues$purposeNow[grepl("facilities",allIssues$Purpose,ignore.case = TRUE)] = "PI"
allIssues$purposeNow[grepl("current",allIssues$Purpose,ignore.case = TRUE)] = "Current"
allIssues$purposeNow[grepl("operating",allIssues$Purpose,ignore.case = TRUE)] = "Current"
allIssues$purposeNow[grepl("requirements",allIssues$Purpose,ignore.case = TRUE)] = "Current"

setdiff( relevantColumns, colnames(allIssues) )
load(file="dateConversion.rds",verbose=myVerbose)
allIssuesWdate = merge( x=allIssues, 
                        y=dateConversion[,c("election","eDate")],
                        by="election")

intersect(colnames(dateConversion), colnames(allIssues))

allIssuesWsd = merge( allIssuesWdate[,relevantColumns], 
                      uniqueMatches, 
                      by=c("County" ,  "Subdivision Name" ))

allIssuesWindex = allIssuesWsd
save(allIssuesWindex,file="allIssuesWindex.rds",version=mySaveVersion)


allPurposes = group_by(allIssues,type,purposeNow,Purpose) %>% summarise(count=n())

colsToKeepPrevious = c( "County"  , "schoolDist" ,    "index"   ,"election"  ,"eDate", "percentYes" ,
                        "Question Type"   , "purposeNow"   ,   "Purpose" , "Description" , "Millage" ,
                        "Percent"    ,  "Dollar Amount"  , "Length" , "time"   ,   "type" )

allIssuesByAllIssues = merge( allIssuesWsd,
                              allIssuesWsd[,colsToKeepPrevious],
                              by=c("County"   , "schoolDist" ))  

allIssuesWithAPrevious = allIssuesByAllIssues[ which(allIssuesByAllIssues$eDate.x >  allIssuesByAllIssues$eDate.y ),]

allIssuesWithAPrevious$lengthScore = eval( abs( allIssuesWithAPrevious$time.x - allIssuesWithAPrevious$time.y ) ) 

allIssuesWithAPrevious$lengthScore[allIssuesWithAPrevious$lengthScore > 5] = 
  allIssuesWithAPrevious$lengthScore[allIssuesWithAPrevious$lengthScore > 5] / 10 + 4.5 

allIssuesWithAPrevious$typeScore = 10
allIssuesWithAPrevious$typeScore[allIssuesWithAPrevious$type.x == 
                                   allIssuesWithAPrevious$type.y ] = 0 

allIssuesWithAPrevious$descScore = 10
allIssuesWithAPrevious$descScore[allIssuesWithAPrevious$Description.x == 
                                   allIssuesWithAPrevious$Description.y ] = 0 

allIssuesWithAPrevious$purposeScore = 5
allIssuesWithAPrevious$purposeScore[allIssuesWithAPrevious$purposeNow.x ==
                                      allIssuesWithAPrevious$purposeNow.y ] = 0


allIssuesWithAPrevious$yearsSinceScore = (allIssuesWithAPrevious$eDate.x - allIssuesWithAPrevious$eDate.y) / 365 

allIssuesWithAPrevious$amountScore = 0

bothLevies = ( allIssuesWithAPrevious$type.x == "Levy"  ) & 
  ( allIssuesWithAPrevious$type.y == "Levy"  ) 
allIssuesWithAPrevious$amountScore[bothLevies] = pmin(5,abs( as.numeric(allIssuesWithAPrevious$Millage.x[bothLevies]) - 
                                                               as.numeric(allIssuesWithAPrevious$Millage.y[bothLevies]) )/2 ) 


bothIncome = allIssuesWithAPrevious$type.x == "Income" & allIssuesWithAPrevious$type.y == "Income"
allIssuesWithAPrevious$amountScore[bothIncome] = pmin(5,abs( as.numeric(allIssuesWithAPrevious$Percent.x[bothIncome]) - 
                                                               as.numeric(allIssuesWithAPrevious$Percent.y[bothIncome]) )*400 ) 

#  I was not able to find any obvious cases of a renewal of an earlier Additional levy
# 
# allIssuesWithAPrevious$renewAdditional = allIssuesWithAPrevious$Description.x=="Renewal" & 
#   allIssuesWithAPrevious$Description.y=="Additional"& 
#   allIssuesWithAPrevious$`Question Type.x` == allIssuesWithAPrevious$`Question Type.y` 
# sum(allIssuesWithAPrevious$renewAdditional)
# View( unique(allIssuesWithAPrevious[,c("purposeScore","purposeNow.x","purposeNow.y")])) # ,"Purpose.x","Purpose.y"
# View( unique(allIssuesWithAPrevious[,c("amountScore","Percent.x","Percent.y","type.x","type.y")]))

allIssuesWithAPrevious$score = allIssuesWithAPrevious$amountScore + allIssuesWithAPrevious$yearsSinceScore + 
  allIssuesWithAPrevious$purposeScore + allIssuesWithAPrevious$descScore + allIssuesWithAPrevious$typeScore + 
  allIssuesWithAPrevious$lengthScore

allIssuesWithAPrevious$newScore = allIssuesWithAPrevious$amountScore + 100* allIssuesWithAPrevious$yearsSinceScore + 
  allIssuesWithAPrevious$purposeScore + allIssuesWithAPrevious$descScore + allIssuesWithAPrevious$typeScore + 
  allIssuesWithAPrevious$lengthScore

bestScores = group_by( allIssuesWithAPrevious, index.x ) %>% summarise( minScore = min(score))

allIssuesWithMinScore = merge( allIssuesWithAPrevious, bestScores,  
                               by.x=c("index.x","score"),
                               by.y=c("index.x","minScore") ) 

allIssuesWithMinScore = allIssuesWithMinScore[which(!duplicated(allIssuesWithMinScore$index.x)),]


save(allIssuesWithMinScore, file="allIssuesWithMinScore.rds",version=mySaveVersion)


bestNewScores = group_by( allIssuesWithAPrevious, index.x ) %>% summarise( minNewScore = min(newScore))

allIssuesWithMinNewScore = merge( allIssuesWithAPrevious, bestNewScores,  
                               by.x=c("index.x","newScore"),
                               by.y=c("index.x","minNewScore") ) 

allIssuesWithMinNewScoreUnique = allIssuesWithMinNewScore[which(!duplicated(allIssuesWithMinNewScore$index.x)),]

save(allIssuesWithMinNewScoreUnique, file="allIssuesWithMinNewScore.rds",version=mySaveVersion)

toc()



