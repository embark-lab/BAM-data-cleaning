library(readxl)
library(haven)
library(cgwtools)
library(scorekeeper)
library(purrr)
library(tibble)
library(dplyr)

# 0. DOWNLOAD DATA FROM REDCAP API TO data/ DIRECTORY

# 1. SCOREKEEP 

# Load Raw data; name cleaned data file for output
load('data/BAM_RA_raw.Rdata')  ## CHANGE THIS FOR NEW STUDIES

# Load Individual Scoresheets
filenames <- list.files('scoresheets_clean/') # MAKE SURE THIS IS THE CORRECT DIRECTORY NAME
filenames <- paste('scoresheets_clean', filenames, sep = '/' )
ldf <- lapply(filenames, read_xlsx)

# List of Measure Names from the Scoresheets
measures <- list.files('scoresheets_clean/') 
measures <- gsub('.xlsx*', '', measures)

# Names the scoresheets
names(ldf) <- measures

# Cleans and saves cleaned data for each measure 

x <- vector(mode = 'list', length = (length(measures)))
names(x) <- measures

tibble_func_1 <- function(x) {
  y = as_tibble(x)
  return (y) }

cleaned_data <- purrr::map(x, tibble_func_1)

for (i in 1:length(measures)) {
cleaned <- scorekeep(BAM_RA_raw, ldf[[i]])
cleaned_last <- cleaned[[max(ldf[[i]]$step)]]
cleaned_data[[i]] <-cleaned_last
}


## 2. POST SCOREKEEP DATA PROCESSING

# Weight Suppression Variables

# TMFS zero center

# Saves cleaned data 

BAM_Redcap <- cleaned_data # CHANGE THIS FOR STUDIES BASED ON WHAT YOU WANT NAME TO BE
save(BAM_Redcap, file = 'data/BAM_Redcap.RData') #CHANGE THIS FOR STUDIES BASED ON WHAT YOU WANT FILE TO BE

rm(list = ls())

load('data/BAM_Redcap.RData')
