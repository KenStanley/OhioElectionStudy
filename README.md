# OhioElectionStudy
#
# https://github.com/KenStanley/OhioElectionStudy
#
   This set of files creates the model that Ken Stanley used to understand Ohio School 
   Levies in https://docs.google.com/document/d/1yw_P8Y0hCvn5XypTGIqJPYHAPhEqr-ZCONyeEsCptIo/edit#
   
   You will need to download the state voter files from https://www6.ohiosos.gov/ords/f?p=VOTERFTP:STWD:::#stwdVtrFiles or google (Download Ohio Statewide Voter Files) and click on "Statewide Voter Files"
   
   colnames(allIssuesWithMinScore)
 "index.x" - ID for this ballot issue                
 "score" - Heuristic used to choose the most similar recent election
 "County"                           
 "schoolDist"                       
 "Subdivision Name" - as seen in the election results file
 "election.x" - mar2017, nov2020, etc.                       
 "eDate.x" - sortable date                      
 "percentYes.x" - Election result for this ballot issue                 "type.x" - "Levy" Or  "Income"         
 "purpose.x" - "Current" or "PI"
 "Description.x" - "Renewal" or "Additional" 
 "Millage.x" - for Levies                       
 "Percent.x" - for Income tax issues                       
 "time.x" - years (5 is the most common)                       
                        
The following variables are for the most similar previous issue
 "index.y" 
 "election.y"                       
 "eDate.y"                          
 "percentYes.y"                     
 "purpose.y"                        
 "Description.y"                    
 "Millage.y"                        
 "Percent.y"                        
 "time.y"                           
 "type.y"    

These are the components of the heuristic used to choose which ballot issue to treat as the most similar recent issue 
 "lengthScore"                      
 "typeScore"                        
 "descScore"                        
 "purposeScore"                     
 "yearsSinceScore"                  
 "amountScore"                      

   
   
   
   
   