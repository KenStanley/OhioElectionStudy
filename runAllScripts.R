#
#  runAllScripts
#
#  This script will run all scripts and create all of the rds files.
#  This is not necessary unless you want to recreate this from the original 
#  code or update it after a substantial addition to the input information 
#  i.e. data from a new election. 
#
#  Each of these scripts is a stand alone script based only on the inputs 
#  stated here. Each starts with a call to "includeForAll.R" which starts
#  with a call to rm(list=ls()) which removes all variables and data frames 
#  from the workspace. 
#
#  The only .rds file that we rely on to start this is: electionResultToVoterFileMatches.rds
#

print(paste("line 10::  Sys.time() = ", Sys.time()))
source("collapseOhioVoterFile.R	") # fifteen minutes; in: SWVF_1_22.txt, 	SWVF_23_44.txt,	SWVF_45_66.txt,	SWVF_67_88.txt; 
#                                                   out: aSWVFcolumns.rds, schoolDistInfo.rds
print(paste("line 13::  Sys.time() = ", Sys.time()))
source("callDemRepTurnoutAllElections.R") # two hours; in: aSWVFcolumns.rds, electionResultToVoterFileMatches.rds, 
#                                                     out: turnoutAllElections.rds
print(paste("line 16::  Sys.time() = ", Sys.time()))
source("DvsRturnout.r") # ten minutes; in: aSWVFcolumns; rds out: allElectionPerformance.rds
print(paste("line 18::  Sys.time() = ", Sys.time()))
source("collectSchoolLevyData.R") # ten seconds; in: electionData/*.xlsx; out: allIssues.rds
print(paste("line 20::  Sys.time() = ", Sys.time()))
source("matchToPreviousBallotIssue.R") # one second; in: allIssues.rds, electionResultToVoterFileMatches.rds, dateConversion.rds
#                                                   out: allIssuesWindex.rds, allIssuesWithMinScore.rds, allIssuesWithMinNewScore.rds (unused)
print(paste("line 23::  Sys.time() = ", Sys.time()))
source("studySchoolBoardElections.R") # one minute; in: schoolDistInfo.rds, statewideresultsbyprecinct2020.xlsx, County2020RaceResultsByPrecinct.xlsx
#                                                  out: SchoolDistPresResultsSummary.rds
print(paste("line 26::  Sys.time() = ", Sys.time()))
source("compareAcrossLevyTypes.R") # in: allIssuesWithMinScore.rds, allElectionPerformance.rds, electionResultToVoterFileMatches.rds, 
#                                        turnoutAllElections.rds, SchoolDistPresResultsSummary.rds, allIssuesWindex.rds, allIssuesWithMinNewScore.rds (unused)
#                                   out: None other than a few print statements 
print(paste("line 30::  Sys.time() = ", Sys.time()))

# releaseSpreadsheet = "https://docs.google.com/spreadsheets/d/1EnEvJ-P4CC4gZpOr-3Dgni2gKwG17ZudjgdJwG88YxI/edit"
# write_sheet_nice(uniqueMatches, ss=releaseSpreadsheet, sheet="unique matches" )
write_sheet_nice(dateConversion, ss=releaseSpreadsheet, sheet="dateconversion" )

