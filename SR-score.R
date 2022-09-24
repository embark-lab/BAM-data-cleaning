library(readxl)
library(haven)
library(cgwtools)
library(scorekeeper)

#Loads BAM Raw Data
load('BAM_raw.Rdata')

#Loads All Individual Scoresheets (Alphabetical by Measure)
ctq_scoresheet <- read.csv("scoresheets/CTQ_scoresheet.csv")

phq9_scoresheet <- read.csv("scoresheets/phq_scoresheet.csv")

#Cleans and saves cleaned data for each measure 

CTQ_scored <- scorekeep(BAM_raw, ctq_scoresheet)
CTQ <- CTQ_scored[[4]]


PHQ_scored <- scorekeep(BAM_raw, phq9_scoresheet)
PHQ9 <- PHQ_scored[[4]]

#Saves cleaned data 
save(PHQ9, file = 'BAM_clean.RData')
