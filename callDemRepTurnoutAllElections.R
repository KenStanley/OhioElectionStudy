#
#  callDemRepTurnoutAllElections.R - Computes the Democratic, Republican and Unaffiliated 
#    turnout in all elections - this will be a rather large file and takes two hours to
#    complete(17 elections) on my MAcBook Air with 8 Gb of RAM. 
#
#  creates turnoutAllElections.rds
#

setwd("/Users/kenstanley/GitRepository/github/politicalRcode/OhioElectionStudy")

source( "includeForAll.R") # includes an rm (list=ls())
tic()
print(paste("Sys.time() = ", Sys.time()))

load(file=file.path(SWVF_dir, "aSWVFcolumns.rds") )
toc()

#
citySchoolDistricts = as.character( unique( aSWVF$CITY_SCHOOL_DISTRICT) ) 
exemptedSchoolDistricts =  as.character(  unique( aSWVF$EXEMPTED_VILL_SCHOOL_DISTRICT) ) 
localSchoolDistricts =  as.character(  unique( aSWVF$LOCAL_SCHOOL_DISTRICT) ) 

stopifnot( length( intersect(citySchoolDistricts,exemptedSchoolDistricts))==1) # They both have an empty string as an option 
stopifnot( length( intersect(citySchoolDistricts,localSchoolDistricts))==1) # They both have an empty string as an option 
stopifnot( length( intersect(exemptedSchoolDistricts,localSchoolDistricts))==1) # They both have an empty string as an option 

allDistrictsStatewide = c( citySchoolDistricts,exemptedSchoolDistricts,localSchoolDistricts )

c( citySchoolDistricts ) 

source("demRepTurnout.R")
source("demRepTurnoutOneSchoolField.R")

load(file="electionResultToVoterFileMatches.rds", verbose=myVerbose)

# dups = duplicated(allMatches$`Subdivision Name`) | duplicated(allMatches$schoolDist)
# View(allMatches)
# View(allMatches[dups,])
#
#  Somewhere licking valley is set to match to licking heights 
#  There are two political subdivisions named: Madison Local School District, one in Lake and one in Richland 
#    - this probably is not an issue as we now insist on a county match as
#      well as a jurisdiction match 
#  
#  I have no idea what is up with Rossford Exempted Village School District - #1	and
#       Rossford Exempted Village School District - #2	
#

allElections = c(   "GENERAL.11.05.2013" , "PRIMARY.05.07.2013"  , 
                    "PRIMARY.05.06.2014" , "GENERAL.11.04.2014"  ,
                    "PRIMARY.05.05.2015"  ,  "GENERAL.11.03.2015" ,          
                    "PRIMARY.03.15.2016" ,  "GENERAL.11.08.2016" ,          
                    "PRIMARY.05.02.2017" , "GENERAL.11.07.2017"  ,
                    "PRIMARY.05.08.2018", "GENERAL.08.07.2018", "GENERAL.11.06.2018"  ,
                    "PRIMARY.05.07.2019"  , "GENERAL.11.05.2019" ,
                    "PRIMARY.03.17.2020" , "GENERAL.11.03.2020"  )

for (electionIndex in 1:length(allElections )) { 
  
  
  print(paste("index=", electionIndex, " election=", allElections[electionIndex], " ", Sys.time()))
  source("demRepTurnout.R")
  source("demRepTurnoutOneSchoolField.R")
  
  tic()
  turnoutThisElection = demRepTurnout( aSWVF,  election=allElections[electionIndex],
                                       localSchoolDistricts=allDistrictsStatewide,
                                       exemptedSchoolDistricts=allDistrictsStatewide,
                                       citySchoolDisctricts=allDistrictsStatewide )
  
  columnsToKeep = c( "schoolDistrictField", "unaVoters",
                     "demVoters" ,  "repVoters" ,
                     "Ureg",  "Dreg" , "Rreg" , "DvoteR" ,"RvoteD"  ,
                     "demBallots"  ,"repBallots" , "unaBallots", "election" )
  
  turnoutThisElection$election= allElections[electionIndex]
  if ( ! ( "unaBallots" %in% colnames(turnoutThisElection) )) {
    browser()
    turnoutThisElection$unaBallots = NA 
  }
  if ( exists("turnoutAllElections")) {
    print(paste("rbind AAA at:", Sys.time())) 
    
    if (!setequal(colnames(turnoutAllElections),colnames(turnoutThisElection[,columnsToKeep]))) {
      browser()
      setdiff(colnames(turnoutAllElections),colnames(turnoutThisElection[,columnsToKeep]))
      setdiff(colnames(turnoutThisElection[,columnsToKeep]),colnames(turnoutAllElections))
    }
    turnoutAllElections = rbind( turnoutAllElections, turnoutThisElection[,columnsToKeep])
  } else {
    turnoutAllElections =  turnoutThisElection[,columnsToKeep]
    
  }
  toc()
  print(paste("Done at:", Sys.time())) 
  
  if (!setequal(colnames(turnoutAllElections),colnames(turnoutThisElection[,columnsToKeep]))) {
    browser()
    setdiff(colnames(turnoutAllElections),colnames(turnoutThisElection[,columnsToKeep]))
    setdiff(colnames(turnoutThisElection[,columnsToKeep]),colnames(turnoutAllElections))
  }
  
  
}
browser()
save(turnoutAllElections,file="turnoutAllElections.rds")
# load(file="turnoutAllElections.rds")
