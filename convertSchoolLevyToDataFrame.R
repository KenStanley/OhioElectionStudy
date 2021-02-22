#
# convertSchoolLevyToDataFrame.R
#
# Takes the data from an excel spreadsheet with local levy results from the Ohio Voter 
# Database and returns information about all school levies in a consistent format 
#   Converting column names 
#
convertSchoolLevyToDataFrame = function( schoolLevyIn, electionDate, term, description, question="Levy" ) {
  
  if ("Length of Levy in Years or Continuing Period of Time (CPT)" %in% 
      colnames(schoolLevyIn )) {
    schoolLevyIn = schoolLevyIn %>% dplyr::rename( "Length" = "Length of Levy in Years or Continuing Period of Time (CPT)")
  }
  
  if ("Length of Levy in Years or Continuing Period of Time (cpt)" %in% 
      colnames(schoolLevyIn )) {
    schoolLevyIn = schoolLevyIn %>% dplyr::rename( "Length" = "Length of Levy in Years or Continuing Period of Time (cpt)")
  }
  
  
  if ("cpt" %in% 
      colnames(schoolLevyIn )) {
    schoolLevyIn = schoolLevyIn %>% dplyr::rename( "Length" = "cpt")
  }
  
  if ("Votes \r\nFor" %in% 
      colnames(schoolLevyIn )) {
    schoolLevyIn = schoolLevyIn %>% dplyr::rename( "Votes For" = "Votes \r\nFor")
  }
  if ("Question \r\nType" %in% 
      colnames(schoolLevyIn )) {
    schoolLevyIn = schoolLevyIn %>% dplyr::rename( "Question Type" = "Question \r\nType")
  }
  
  if ("Media \r\nMarket" %in% 
      colnames(schoolLevyIn )) {
    schoolLevyIn = schoolLevyIn %>% dplyr::rename( "Media Market" = "Media \r\nMarket")
  }
  
  if ("Subdivision \r\nType" %in% 
      colnames(schoolLevyIn )) {
    schoolLevyIn = schoolLevyIn %>% dplyr::rename( "Subdivision Type" = "Subdivision \r\nType")
  }
  if ("Subdivision \r\nName" %in% 
      colnames(schoolLevyIn )) {
    schoolLevyIn = schoolLevyIn %>% dplyr::rename( "Subdivision Name" = "Subdivision \r\nName")
  }
  
  
  
  if ("Commencing year or Effective Date" %in% 
      colnames(schoolLevyIn )) {
    schoolLevyIn = schoolLevyIn %>% dplyr::rename( "Commencing Year or Effective Date" = "Commencing year or Effective Date")
  }
  
  
  schoolLevyIn$term = ( term=="all" |  schoolLevyIn$`Length` == term ) 
  
  schoolLevyIn$include = ( description=="all" |  schoolLevyIn$`Description` == description   )
  
  schoolLevyIn$notWithdrawn = ( !grepl("WITHDRAWN",schoolLevyIn$`Subdivision Name`,
                                      ignore.case = TRUE)   )
  
  if (question=="Levy") {
    

  schoolLevy = schoolLevyIn[ which( grepl("School", schoolLevyIn$`Subdivision Type`) & 
                                      ( schoolLevyIn$`Question Type`=="Levy" |
                                          schoolLevyIn$`Question Type`=="Tax levy" |
                                          schoolLevyIn$`Question Type`=="Tax Levy"   ) &
                                      schoolLevyIn$term &  schoolLevyIn$notWithdrawn & 
                                      schoolLevyIn$include),]
  } else {
    schoolLevy = schoolLevyIn[ which( grepl("School", schoolLevyIn$`Subdivision Type`)  &
                                        schoolLevyIn$term &  schoolLevyIn$notWithdrawn & 
                                        schoolLevyIn$include),]
    
  }
  
  
  
  
  if( nrow(schoolLevy) < 1 ) { 
    print(paste("no rows in school levy election = ", electionDate))
    return <- 0 
  } else {
    
    schoolLevy$sixChars = tolower(substr(schoolLevy$`Subdivision Name`,1,6) ) 
    schoolLevy$`Votes For` = as.numeric(schoolLevy$`Votes For`)
    schoolLevy$percentYes = schoolLevy$`Votes For` / 
      (schoolLevy$`Votes For` + schoolLevy$`Votes Against` )
    
    schoolLevy$election = substr(electionDate,1,nchar(electionDate)) 
    return <- schoolLevy 
    
  }
  

 

}