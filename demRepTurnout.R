#
#  demRepTurnout.R - compute Dem and Rep turnout for all school districts 
#     in one election 
#
#  studySchoolLevies.R has the most documentation
#

demRepTurnout <- function( voterFile,  election="PRIMARY.05.02.2017", 
                           citySchoolDisctricts, exemptedSchoolDistricts, localSchoolDistricts  ) {
  
  
  columnsWeWant = c("PARTY_AFFILIATION" , election, "CITY_SCHOOL_DISTRICT", 
                    "EXEMPTED_VILL_SCHOOL_DISTRICT", "LOCAL_SCHOOL_DISTRICT","GENERAL.11.03.2020"  ) 
  
  thisVoterFile = voterFile[ which( voterFile$GENERAL.11.03.2020 == "X" ),columnsWeWant]
  
  if ( length(citySchoolDisctricts)>0 ) {
    
    citySchoolDistrictFields = c("PARTY_AFFILIATION" , election, "CITY_SCHOOL_DISTRICT", "GENERAL.11.03.2020"  )
    
    thisVoterFileCitySchoolDistrict = thisVoterFile[,citySchoolDistrictFields] %>%
      dplyr::rename( schoolDistrictField = CITY_SCHOOL_DISTRICT )
    
    citySchoolDistrictInfo = demRepTurnoutOneSchoolField( thisVoterFileCitySchoolDistrict,  
                                                          election=election, schoolDistricts=citySchoolDisctricts )
    
    allSchoolDistrictInfo = citySchoolDistrictInfo
  }
  if ( length(localSchoolDistricts)>0  ) {
    
    localSchoolDistrictFields = c("PARTY_AFFILIATION" , election, "LOCAL_SCHOOL_DISTRICT", "GENERAL.11.03.2020"  )
    
    thisVoterFileLocalSchoolDistrict = thisVoterFile[,localSchoolDistrictFields] %>%
      dplyr::rename( schoolDistrictField = LOCAL_SCHOOL_DISTRICT )
    
    localSchoolDistrictInfo = demRepTurnoutOneSchoolField( thisVoterFileLocalSchoolDistrict,  election="PRIMARY.05.02.2017", schoolDistricts=localSchoolDistricts )
    
    print(paste("rbind demRepTurnout.R BBB at:", Sys.time())) 
    
    if (exists("allSchoolDistrictInfo")) {
      if (!setequal(colnames(allSchoolDistrictInfo),colnames(localSchoolDistrictInfo))) {
        browser()
      }
      allSchoolDistrictInfo = rbind(allSchoolDistrictInfo, localSchoolDistrictInfo )
    } else {
      allSchoolDistrictInfo = localSchoolDistrictInfo
    }
  }

  
  stopifnot( exists( "allSchoolDistrictInfo"))
  
  if ( length(exemptedSchoolDistricts)>0  ) {
    
    # print( "This has not been tested yet")
    # print( "This has not been tested yet")
    # print( "This has not been tested yet")
    # browser()
    # browser()
    # browser()
    exemptedSchoolDistrictFields = c("PARTY_AFFILIATION" , election, "EXEMPTED_VILL_SCHOOL_DISTRICT", "GENERAL.11.03.2020"  )
    
    thisVoterFileExemptedSchoolDistrict = thisVoterFile[,exemptedSchoolDistrictFields] %>%
      dplyr::rename( schoolDistrictField = EXEMPTED_VILL_SCHOOL_DISTRICT )
    
    exemptedSchoolDistrictInfo = demRepTurnoutOneSchoolField( thisVoterFileExemptedSchoolDistrict,  election="PRIMARY.05.02.2017", schoolDistricts=exemptedSchoolDistricts )
    
    if (exists("allSchoolDistrictInfo")) {
      #  Amazingly, this rbind appears to work even though exemptedSchoolDistrictInfo has a 
      #  different set of column names. Presumably it works because exemptedSchoolDistrictInfo 
      #  is empty 
      print(paste("rbind demRepTurnout.R at:", Sys.time())) 
      
      if (!setequal(colnames(allSchoolDistrictInfo),colnames(exemptedSchoolDistrictInfo))) {
        browser()
      }
      allSchoolDistrictInfo = rbind(allSchoolDistrictInfo, exemptedSchoolDistrictInfo )
    } else {
      allSchoolDistrictInfo = exemptedSchoolDistrictInfo
    }
  }


  doubleCheckCitySD = FALSE 
  if ( doubleCheckCitySD ) { 
  CSDdf = tibble::enframe(citySchoolDisctricts,value = "CITY_SCHOOL_DISTRICT")
  
  thisCSDvoterFile = merge( thisVoterFile, CSDdf[,"CITY_SCHOOL_DISTRICT"], by="CITY_SCHOOL_DISTRICT")
  
  colnames(thisCSDvoterFile)[3] = "ballot"
  
  ballotsPerSchoolDist = thisCSDvoterFile %>% 
    filter(ballot!="") %>% 
    group_by( ballot, CITY_SCHOOL_DISTRICT ) %>% summarize( numVoters = n() )
  
  these_ballots = reshape2::dcast(ballotsPerSchoolDist,  CITY_SCHOOL_DISTRICT  ~  ballot, value.var="numVoters" )
  
  ballots  = these_ballots%>% dplyr::rename( "demBallots" = "D") %>%
    dplyr::rename( "repBallots" = "R") %>%
    dplyr::rename( "unaBallots" = "X") 
  
  
  votersPerSchoolDist = thisCSDvoterFile %>% 
    filter(ballot!="") %>% 
    group_by( PARTY_AFFILIATION, CITY_SCHOOL_DISTRICT ) %>% summarize( numVoters = n() )
  
  
  these_voters = reshape2::dcast(votersPerSchoolDist,  CITY_SCHOOL_DISTRICT  ~  PARTY_AFFILIATION, value.var="numVoters" )
  
  voters  = these_voters%>% dplyr::rename( "demVoters" = "D") %>%
    dplyr::rename( "repVoters" = "R") %>%
    dplyr::rename( "unaVoters" = "Var.2") 
  
  #
  # OK we still want total registered voters (D, R, Una ) and 
  #   we wnat DvotingR and RvotingD
  #
  all2020Voters = thisCSDvoterFile %>% 
    group_by( PARTY_AFFILIATION, CITY_SCHOOL_DISTRICT ) %>% summarize( numVoters = n() )
  
  allRegs = reshape2::dcast(all2020Voters,  CITY_SCHOOL_DISTRICT  ~  PARTY_AFFILIATION, value.var="numVoters" )
  
  
  allRegistrations =allRegs %>% dplyr::rename( Ureg = Var.2 ) %>% dplyr::rename( "Dreg" = "D") %>%
    dplyr::rename( "Rreg" = "R") 
  
  thisCSDvoterFile$DvoteR = 0+(thisCSDvoterFile$PARTY_AFFILIATION=="D" & thisCSDvoterFile$ballot=="R")
  thisCSDvoterFile$RvoteD = 0+(thisCSDvoterFile$PARTY_AFFILIATION=="R" & thisCSDvoterFile$ballot=="D")
  
  crossPartyVotes = thisCSDvoterFile %>% 
    group_by( CITY_SCHOOL_DISTRICT ) %>% summarize( DvoteR = sum(DvoteR), 
                                                    RvoteD = sum(RvoteD))
  
  votersColumns = c( "CITY_SCHOOL_DISTRICT", "unaVoters"   , "demVoters"  ,   
                     "repVoters"   )  
  regsColumns = c( "CITY_SCHOOL_DISTRICT", "Ureg"   , "Dreg"  ,  "Rreg"   )
  allInfo2 = merge(voters[,votersColumns], allRegistrations[,regsColumns], by="CITY_SCHOOL_DISTRICT")
  
  
  allInfo3 = merge( allInfo2, crossPartyVotes, by="CITY_SCHOOL_DISTRICT")
  allInfo = merge( allInfo3, ballots, by="CITY_SCHOOL_DISTRICT")
  browser()
  
  }
  
  return <- allSchoolDistrictInfo
  
  
}