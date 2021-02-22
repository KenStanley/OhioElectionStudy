#
#  computeLean.R
#

computeLean <- function( modelIssues ) {
  
  
  electionLM = lm( modelIssues$percentYes ~ modelIssues$election   )
  sumElectionOnly = summary(electionLM)
  ECasDF = as.data.frame(sumElectionOnly$coefficients)[,1:2] %>% 
    dplyr::rename( lean = Estimate )
  
  #
  # Anyone who knows R well will laugh at this amateurish approach to collecting the
  # name of the election 
  #
  
  electionNameLength = nchar(rownames(ECasDF)[2:nrow(ECasDF)])
  eLength = min(electionNameLength)
  stopifnot( sd(electionNameLength) == 0 ) # This is harder if these aren't all the same length
  ECasDF$election = substr( rownames(ECasDF),eLength-6,eLength) 
  
  ECcolumns = c("election", "lean" )
  
  leans = ECasDF[2:nrow(ECasDF),ECcolumns]
  
  allElections = leans$election
  
  electionsShouldBe = c("mar2020","may2013","may2014","may2015","may2017","may2018","may2019","nov2013","nov2014"
                        ,"nov2015","nov2016","nov2017","nov2018","nov2019","nov2020")
  
  # allElections = c( "may2018","nov2018","may2017","nov2016","mar2020","nov2019","may2019","nov2020"
  #                   , "may2015","nov2014","nov2015")
  # 
  setdiff( allElections, electionsShouldBe)
  
  # browser()
  # stopifnot(setequal(electionsShouldBe,allElections))
  setdiff(electionsShouldBe,allElections)
  
  return <- leans
}


# 
# computeQuartileLean <- function( modelIssues ) {
#   
#   
#   l1 = mean( modelIssues$percentYes[which(modelIssues$leanQuartile=="first")])
#   l2 = mean( modelIssues$percentYes[which(modelIssues$leanQuartile=="second")])
#   l3 = mean( modelIssues$percentYes[which(modelIssues$leanQuartile=="third")])
#   l4 = mean( modelIssues$percentYes[which(modelIssues$leanQuartile=="fourth")])
#   c(l1,l2,l3,l4)
#   
#   quartileLM = lm( modelIssues$percentYes ~ modelIssues$leanQuartile   )
#   sumquartileOnly = summary(quartileLM)
#   ECasDF = as.data.frame(sumquartileOnly$coefficients)[,1:2] %>% 
#     dplyr::rename( lean = Estimate )
#   
#   #
#   # Anyone who knows R well will laugh at this amateurish approach to collecting the
#   # name of the quartile 
#   #
#   browser()
#   
#   quartileNameLength = nchar(rownames(ECasDF)[1:nrow(ECasDF)])
#   eLength = min(quartileNameLength)
#   stopifnot( sd(quartileNameLength) == 0 ) # This is harder if these aren't all the same length
#   ECasDF$quartile = substr( rownames(ECasDF),eLength-6,eLength) 
#   
#   ECcolumns = c("election", "lean" )
#   
#   leans = ECasDF[1:nrow(ECasDF),ECcolumns]
#   
#   allquartiles = leans$election
#   
#   quartilesShouldBe = c("mar2020","may2013","may2014","may2015","may2017","may2018","may2019","nov2013","nov2014"
#                         ,"nov2015","nov2016","nov2017","nov2018","nov2019","nov2020")
#   
#   # browser()
#   # stopifnot(setequal(quartilesShouldBe,allquartiles))
#   setdiff(quartilesShouldBe,allquartiles)
#   
#   return <- leans
# }
# 
# 
# 
# 
# 
# 
