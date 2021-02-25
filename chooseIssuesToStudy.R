#
# chooseIssuesToStudy.R
#
chooseIssuesToStudy = function( allIssues, minScore = 0 , maxScore = 100000, 
                                maxNumRegs = 100000000 ,
                                thisDescription = "all", # Additional" # "Renewal" #  "Renewal" # "Additional" # Or "Renewal"
                                thisType = "all", # "Levy" # "Levy"   # Or  "Income"
                                thisTime = "all",  # "5"  # Or "all"  
                                thisPurpose  = "all") { # "Current" or "PI" 
  
  whichDescriptions = ( allIssues$Description.x == thisDescription ) | thisDescription=="all"
  whichType  = ( allIssues$type == thisType ) | thisType=="all"
  whichPurpose = ( allIssues$type == thisPurpose  ) | thisPurpose=="all"
  whichTime = ( allIssues$time == thisTime  ) | thisTime=="all"
  
  allIssuesFlag = allIssues$score < maxScore & allIssues$numRegs < maxNumRegs  & 
    allIssues$score > minScore & whichDescriptions & whichType & whichTime
  
  return <- allIssuesFlag
  
}