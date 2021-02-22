#
# studyOneElection
#
# First we will figure out which precincts were elligible to vote.
# OK - that is harder than it seems 
#   What if we just drop the precincts with low vote totals?
#     We would be dropping mainly precincts that are not eligible but also
#     dropping a few precincts that are eligible to vote but have low voter 
#     involvement. 
#  So we would be dropping lower SES precincts 
#  The question that we are asking is whether this is a Democratic cycle or an R cycle 
#    In a strong Dem cycle, a few more Dem precinccts will be included, this will slighlty 
#    reduce the percentage of Dems voters that we calculate. 
#
#  So first we compute the ratio of votes in this election to the 2020 presidential 
#  Then we restrict ourselves to precincts whose vote is at least a given fraction of that ratio.
#  This fraction is set to cutoffHeuristic which is presently set to .20
#  If less than cutoffHeuristic / 2 of the voting population has moved from a voting precinct to a 
#  non voting precinct, the likelihood that enough people will move into a non-voting precinct 
#  to cause that precinct to look like a voting precinct is pretty small.
#
#  For may 2019, the vote was about 7.6% of the 2020 presidential vote. 
#  With cutoffHeuristic set to .2, we eliminated 4878 out of 8933 precincts and only got rid of 1.2% of 
#  the vote. 
#
#  For November 2014 (in which every precinct should have had the right to vote) this heuristic eliminated 
#  only 24 precincts representing 0.013% of the total vote.
#   Note that it is possible that some of these precincts did not exist in 2014 - though that would not
#   actually explain much. Those voters still exits. 
#
#   So the plan is now:
#   1) Identify the precincts that we consider to be eligible to vote in this election
#   2) Count how many Democrats and Republicans were eligible to vote in 
#
#   What if we did this a different way:
#   1) Determine what percentage of the voters in each precinct voted
#   2) Use that to determine how many Republicans and Democrats were "eligible to vote" or 
#      perhaps I should say "expected to vote" 
#   3) 
#
#   The downside to this: Assume that all precincts have 100 voters
#      1) Taken to the limit, consider that all precincts are either 100% D or 100% R in a Democratic year
#         60% of the Ds vote and 40% of the Rs vote. The Democratic precincts will be treated as
#         60% eligible and the R precincts will be considered 40% eligible. Both will be seen as 
#         voting consistently with their eligibility.
#      2) At the other extreme, if all precincts are 50-50, 60% of the Ds vote (hence 30 votes), 40% of the Rs 
#         vote (hence 20 votes), hence all precincts will show 50% eligiblility and Dems will be shown 
#         to vote at 60% while Rs vote at 40% 
#
#    What if we were to do this at a larger granularity, maybe even the school district level?
#    or the county or the state representative? What is THIS? 

studyOneElection = function( voterFile ) {
  
  voterFile$voted = nchar(as.character(voterFile$thisElection))> 0 
  voterFile$votedNov2020 = nchar(as.character(voterFile$GENERAL.11.03.2020))> 0 
  
  voterFile$demVote = voterFile$voted & voterFile$PARTY_AFFILIATION == "D"
  voterFile$repVote = voterFile$voted & voterFile$PARTY_AFFILIATION == "R"
  
  voterFile$demVoteNov2020 = voterFile$votedNov2020 & voterFile$PARTY_AFFILIATION == "D"
  voterFile$repVoteNov2020 = voterFile$votedNov2020 & voterFile$PARTY_AFFILIATION == "R"
  
  districtVotes = group_by(voterFile,CITY_SCHOOL_DISTRICT ) %>% 
    summarise( numVoted = sum(voted),numVoted2020 = sum(votedNov2020), count = n() )
  districtVotes$percentVote = districtVotes$numVoted / districtVotes$count
  districtVotes$percentOf2020Vote = districtVotes$numVoted / districtVotes$numVoted2020
  
  precinctVotes = group_by(voterFile,PRECINCT_CODE ) %>% 
    summarise( numVoted = sum(voted),numVoted2020 = sum(votedNov2020) , count = n() )
  precinctEligible = precinctVotes[ which(precinctVotes$numVoted > 10 ) ,] 
  precinctVotes$percentVoted = precinctVotes$numVoted / precinctVotes$count 
  precinctVotes$percentOf2020Vote = precinctVotes$numVoted / precinctVotes$numVoted2020 
  
  wardVotes = group_by(voterFile,WARD  ) %>% 
    summarise( numVoted = sum(voted),numVoted2020 = sum(votedNov2020) , count = n() )
  wardEligible = wardVotes[ which(wardVotes$numVoted > 10 ) ,] 
  wardVotes$percentVoted = wardVotes$numVoted / wardVotes$count 
  wardVotes$percentOf2020Vote = wardVotes$numVoted / wardVotes$numVoted2020 
  
  # precinctVotes$percent2020VotedBinned = 5 * round(precinctVotes$percentOf2020Vote * 20 )
  # precinctVotes$percentVotedBinned = 5 * round(precinctVotes$percentVoted * 20 )
  # percentBins = group_by(precinctVotes, percentVotedBinned) %>%
  #   summarise(  precinctsThisBin = n()  )
  # percent2020Bins = group_by(precinctVotes, percent2020VotedBinned) %>%
  #   summarise(  precinctsThisBin = n()  )
  
  percentStatewide = sum( voterFile$voted ) / nrow( voterFile )
  percentOfStatewide2020voters = sum( voterFile$voted ) / sum( voterFile$votedNov2020 )
  
  cutoffHeuristic = .20
  wardVotes$eligible = wardVotes$percentOf2020Vote > cutoffHeuristic * percentOfStatewide2020voters
  
  sum( wardVotes$numVoted)
  sum( wardVotes$numVoted[ which(wardVotes$eligible)])
  
  wardVotes$numVotedBinned = 10*round(wardVotes$numVoted/10)
  
  bins = group_by( wardVotes, numVotedBinned) %>% summarise( wardsThisBin = n() )
  singleBins = group_by( wardVotes, numVoted) %>% summarise( wardsThisBin = n() )
  
  wardlessVoters = aSWVF[which(nchar(as.character(aSWVF$WARD))==0),]
  
  
  wardVotes = group_by(voterFile,pseudoWard  ) %>% 
    summarise( numVoted = sum(voted),numVoted2020 = sum(votedNov2020) , 
               demVotes = sum(demVote), repVotes = sum(repVote), 
               demVotes2020 = sum(demVoteNov2020),
               repVotes2020 = sum(repVoteNov2020), count = n() )
  wardEligible = wardVotes[ which(wardVotes$numVoted > 10 ) ,] 
  wardVotes$percentVoted = wardVotes$numVoted / wardVotes$count 
  wardVotes$percentOf2020Vote = wardVotes$numVoted / wardVotes$numVoted2020 
  
  
  wardVotes$expectedDemVotes = wardVotes$demVotes2020 * ( wardVotes$numVoted / wardVotes$numVoted2020 ) 
  totalExpectedDemVotes = sum( wardVotes$expectedDemVotes )
  wardVotes$expectedRepVotes = wardVotes$repVotes2020 * ( wardVotes$numVoted / wardVotes$numVoted2020  ) 
  totalExpectedRepVotes = sum( wardVotes$expectedRepVotes )
  
  #
  # The way that we computed the expected number of Democrats and
  # Republicans who vote is low because many more unaffiliated voters
  # vote in Presidential elections than in off years 
  # 
  sum(is.na(wardVotes$expectedDemVotes))
  sum(wardVotes$expectedDemVotes,na.rm=TRUE)
  sum(wardVotes$demVotes)
  sum(voterFile$demVote)
  
  sum(wardVotes$expectedRepVotes,na.rm=TRUE)
  sum(wardVotes$repVotes)
  sum(voterFile$repVote)
  naTrouble = wardVotes[is.na(wardVotes$expectedDemVotes),]
  
  return <- wardVotes 
}