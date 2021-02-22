#
#  demRepTurnoutOneSchoolField.R - compute Dem and Rep turnout for all school districts 
#     in one election 
#
#  studySchoolLevies.R has the most documentation
#

demRepTurnoutOneSchoolField <- function( voterFile,  election="PRIMARY.05.02.2017", schoolDistricts ) {
  
  
  CSDdf = tibble::enframe(schoolDistricts,value = "schoolDistrictField")
  
  thisCSDvoterFile = merge( voterFile, CSDdf[,"schoolDistrictField"], by="schoolDistrictField")
  
  if ( nrow(thisCSDvoterFile) > 0 ) {
    
    
    
    colnames(thisCSDvoterFile)[3] = "ballot"
    
    
    ballotsPerSchoolDist = thisCSDvoterFile %>% 
      filter(ballot!="") %>% 
      group_by( ballot, schoolDistrictField ) %>% summarize( numVoters = n() )
    
    these_ballots = reshape2::dcast(ballotsPerSchoolDist,  schoolDistrictField  ~  ballot, value.var="numVoters" )
    
    
    if ( !("X" %in% colnames(these_ballots))) {
      print(Sys.time())
      browser()
    }
    
    ballot1  = these_ballots%>%   dplyr::rename( "unaBallots" = "X") 
    if ( !("D" %in% colnames(these_ballots))) {
      ballot2 = ballot1 
      ballot2$demBallots = NA
    }  else {
      ballot2 = ballot1 %>%   dplyr::rename( "demBallots" = "D")
    }  
    if ( !("R" %in% colnames(these_ballots))) {
      ballot3 = ballot2 
      ballot3$repBallots = NA
    }  else {
      ballot3 = ballot2 %>%   dplyr::rename( "repBallots" = "R")
    }
    ballots = ballot3
    
    votersPerSchoolDist = thisCSDvoterFile %>% 
      filter(ballot!="") %>% 
      group_by( PARTY_AFFILIATION, schoolDistrictField ) %>% summarize( numVoters = n() )
    
    these_voters = reshape2::dcast(votersPerSchoolDist,  schoolDistrictField  ~  PARTY_AFFILIATION, value.var="numVoters" )
    
    voters  = these_voters%>% dplyr::rename( "demVoters" = "D") %>%
      dplyr::rename( "repVoters" = "R") %>%
      dplyr::rename( "unaVoters" = "Var.2") 
    
    #
    # OK we still want total registered voters (D, R, Una ) and 
    #   we wnat DvotingR and RvotingD
    #
    all2020Voters = thisCSDvoterFile %>% 
      group_by( PARTY_AFFILIATION, schoolDistrictField ) %>% summarize( numVoters = n() )
    
    allRegs = reshape2::dcast(all2020Voters,  schoolDistrictField  ~  PARTY_AFFILIATION, value.var="numVoters" )
    
    allRegistrations =allRegs %>% dplyr::rename( Ureg = Var.2 ) %>% dplyr::rename( "Dreg" = "D") %>%
      dplyr::rename( "Rreg" = "R") 
    
    thisCSDvoterFile$DvoteR = 0+(thisCSDvoterFile$PARTY_AFFILIATION=="D" & thisCSDvoterFile$ballot=="R")
    thisCSDvoterFile$RvoteD = 0+(thisCSDvoterFile$PARTY_AFFILIATION=="R" & thisCSDvoterFile$ballot=="D")
    
    crossPartyVotes = thisCSDvoterFile %>% 
      group_by( schoolDistrictField ) %>% summarize( DvoteR = sum(DvoteR), 
                                                     RvoteD = sum(RvoteD))
    
    
    votersColumns = c( "schoolDistrictField", "unaVoters"   , "demVoters"  ,   
                       "repVoters"   )  
    regsColumns = c( "schoolDistrictField", "Ureg"   , "Dreg"  ,  "Rreg"   )
    allInfo2 = merge(voters[,votersColumns], allRegistrations[,regsColumns], by="schoolDistrictField")
    
    
    allInfo3 = merge( allInfo2, crossPartyVotes, by="schoolDistrictField")
    allInfo = merge( allInfo3, ballots, by="schoolDistrictField")
    return <- allInfo
  } else {
    return <- thisCSDvoterFile # We just want to return a data frame with no rows 
  }
  
  
  
}