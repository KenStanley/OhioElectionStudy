#
#  DvsRturnout.R - estimates the percentage of Democrats, eligible to vote, 
#  in this election did so. 
#
#  This script takes five to ten minutes on my MacBook Air with 8 Gbyes of RAM, 45 seconds to 
#  load aSWVFcolumns.rd, 115 seconds for the first loop and the rest (untimed) on the
#  final loop. 
#
# Everyone who lives in a given precinct is either eligible to vote
# in a given election or not, we found treating eligiblity as a binary 
# not to be helpful. There are many elections in which voters are eligible 
# to vote, but so few vote that it does not make sense to treat them as
# eligible. Instead we treat voter elibigibility as the number of votes in this election divided by
# the number of votes in the presidential election. The jurisdiction over which we 
# make this determination is key to this, but it is not clear what size that jurisdiction 
# ought to be. We use the Ward as the primary determination. However, many areas do not have wards and in those cases, I use 
# a combination of other features: county, city and city_school_district. 
# We could also do this at the precinct level, however precincts are small and 
# can be heavily partisan. In a world in which precincts are purely R or D, our ability
# to measure the partisanship of an election breaks down if we determine vote eligibility
# on a precinct by precinct basis. 
#

setwd("/Users/kenstanley/GitRepository/github/politicalRcode/OhioElectionStudy")
source( "includeForAll.R")

tic()
load(file=file.path(SWVF_dir, "aSWVFcolumns.rds") ) # Create this file by 
#       downloading the Statewide Voter Files from https://www6.ohiosos.gov/ords/f?p=VOTERFTP:STWD:::#stwdVtrFiles 
#       and running 
toc()

first_election = 24
last_election = 47
tic()
stopifnot(colnames(aSWVF)[first_election]=="PRIMARY.05.07.2013")
stopifnot(colnames(aSWVF)[last_election]=="GENERAL.11.03.2020")
for ( index in first_election:last_election) {

  thisResult = group_by( aSWVF[index]) %>% summarize( count = n( ))
  thisResult = group_by( aSWVF, PRIMARY.05.07.2019  ) %>% summarize( count = n( ))
  
  thisResult = group_by( aSWVF[which(nchar(as.character(aSWVF[,index]))>0), ],PARTY_AFFILIATION ) %>%
    summarise( numVotes=n() )
  colnames(thisResult)[2]= colnames(aSWVF[index])
  allVotersByAffiliation = group_by( aSWVF,PARTY_AFFILIATION ) %>% summarise( numVoters=n() )
  if (index == first_election) {
   allElectionsByAffiliation = thisResult
  }  else {
    allElectionsByAffiliation = merge(allElectionsByAffiliation,
                                      thisResult,
                                      by="PARTY_AFFILIATION",all.x=TRUE)
  }   
  
}
toc()

#
# many communities do not have a concept of Ward, for those we create a pseudo ward
# by 
source( "studyOneElection.R" )

aSWVF$pseudoWard = paste( aSWVF$COUNTY_NUMBER, aSWVF$CITY, aSWVF$CITY_SCHOOL_DISTRICT  )
wardExists = which(nchar(as.character(aSWVF$WARD)) > 0 )
aSWVF$pseudoWard[wardExists] = aSWVF$WARD[wardExists]

for ( index in first_election:last_election) {
  
  aSWVF$thisElection = aSWVF[,index]
  wardVotes = studyOneElection( aSWVF )
  
  # demPerf = 
  demPerformance = sum(wardVotes$demVotes)/sum(wardVotes$expectedDemVotes,na.rm=TRUE)
  repPerformance = sum(wardVotes$repVotes) / sum(wardVotes$expectedRepVotes,na.rm=TRUE)
  demVotes = sum(wardVotes$demVotes)
  repVotes = sum(wardVotes$repVotes)
  # 
  electionPerformance = data.frame( demPerformance, repPerformance, demVotes, repVotes )
  colnames(electionPerformance) = c( "demPerformance", "repPerformance", "demVotes", "repVotes")
  rownames(electionPerformance) = colnames(aSWVF)[index]
  if ( index == first_election) {
    allElectionPerformance = electionPerformance
  } else {
    allElectionPerformance = rbind(allElectionPerformance, electionPerformance) 
  }
}
allElectionPerformance$relDemPerformance = allElectionPerformance$demPerformance / allElectionPerformance$repPerformance

allElectionPerformance$election = ""
allElectionPerformance["PRIMARY.05.07.2013","election"] = "may2013"
allElectionPerformance["GENERAL.11.05.2013","election"] = "nov2013"
allElectionPerformance["PRIMARY.05.06.2014","election"] = "may2014"
allElectionPerformance["GENERAL.11.04.2014","election"] = "nov2014"
allElectionPerformance["PRIMARY.05.05.2015","election"] = "may2015"
allElectionPerformance["GENERAL.11.03.2015","election"] = "nov2015"
allElectionPerformance["PRIMARY.03.15.2016","election"] = "mar2016"
allElectionPerformance["GENERAL.11.08.2016","election"] = "nov2016"
allElectionPerformance["PRIMARY.05.02.2017","election"] = "may2017"
allElectionPerformance["GENERAL.11.07.2017","election"] = "nov2017"
allElectionPerformance["PRIMARY.05.08.2018","election"] = "may2018"
allElectionPerformance["GENERAL.08.07.2018","election"] = "aug2018"
allElectionPerformance["GENERAL.11.06.2018","election"] = "nov2018"
allElectionPerformance["PRIMARY.05.07.2019","election"] = "may2019"
allElectionPerformance["GENERAL.11.05.2019","election"] = "nov2019"
allElectionPerformance["PRIMARY.03.17.2020","election"] = "mar2020"
allElectionPerformance["GENERAL.11.03.2020","election"] = "nov2020"

allElectionPerformance$relDemTurnout = allElectionPerformance$demVotes / allElectionPerformance$repVotes

save(allElectionPerformance,file="allElectionPerformance.rds",version=mySaveVersion)


