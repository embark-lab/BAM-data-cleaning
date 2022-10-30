# 0. Load packages
library(readxl)
library(haven)
library(cgwtools)
library(scorekeeper)
library(purrr)
library(tibble)
library(dplyr)
library(REDCapR)
library(anytime)
library(labelled)
library(stringr)
library(Hmisc)


## This scoring file scores data from the 'Body Advocacy Movement (Scheduled)' Redcap project (Fall 2022 and later)
## And combines this scored data with previous scored data from the unscheduled Redcap project used before Fall 2022

# 1. Download Data from API
setwd("/Volumes/kschaumberg/BAM/BAM-Data-Cleaning/data")
uri <- 'https://redcap.ictr.wisc.edu/api/'
token <- 'xxxx MASKED -- NEED TO ADD FOR EACH PROJECT' # add redcap API TOKEN HERE
currentDate <- Sys.Date()


df <- redcap_read_oneshot(redcap_uri = uri, token = token)$data  |> 
  filter(redcap_event_name != 'consent_form_arm_1') 

# Writes a .csv file of the captured data from redcap

rawfilename = paste('BAM2_redcap_raw.', currentDate, '.csv', sep = "")
write.csv(df, file = rawfilename)

# Load Raw data; name cleaned data file for output, make record_ids 'BAM_ID' format
data <- read.csv(rawfilename) %>% 
  filter(record_id >=100) %>% 
  select (-X) %>% 
  mutate(record_id = as.character(record_id)) %>% 
  mutate(record_id = paste0('BAM_',record_id))

# 1.5 Clean known data issues

# 2. Scorekeep 


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
  cleaned <- scorekeep(data, ldf[[i]])
  cleaned_last <- cleaned[[max(ldf[[i]]$step)]]
  cleaned_data[[i]] <-cleaned_last
}


# 3. Post-Scorekeep Data Processing

# 3.1. Demographics

DCF <- data %>%
  select(record_id, redcap_event_name, sex_orientation, specify_sex_orientation, starts_with('dcf')) |> 
  filter (dcf_date > '2019-01-01') |> 
  mutate(
    dcf_date = anydate(dcf_date), #convert dcf_date to date type variable
    dcf_birthday = anydate(dcf_age), #convert dcf_age to date type variable and save as birthday
    dcf_age = (as.numeric(dcf_date - dcf_birthday))/365.25) %>% #calculate age in years  
  rename(Race_White = dcf_race___1, Race_AA_Black = dcf_race___2 , Race_Asian = dcf_race___3, Race_NativeAmerican = dcf_race___4, Race_Hispanic_Latino = dcf_race___5, Race_Pacific_Island = dcf_race___6, Race_other = dcf_race___7)  
DCF <- DCF |> 
  mutate (sex_orientation = labelled(DCF$sex_orientation, c('Heterosexual' = 1, 'Gay/Lesbian' = 2, 'Bisexual' = 3, 'Other' = 4))) |> 
  mutate (dcf_gender = recode (dcf_gender, `1` = 0, `2` = 1, `3` = 2), 
          dcf_gender = labelled(DCF$dcf_gender, c('Male' = 0, 'Female' = 1, 'Nonbinary' = 2))) |>   mutate(race_nonwhite = rowSums(across(c(Race_AA_Black:Race_other)))) |> 
  mutate(Race_nonwhite = case_when(race_nonwhite>0 ~1,
                                   race_nonwhite==0~ 0)) |> 
  mutate(Race_Mixed = rowSums(across(c(Race_White:Race_other))))|> 
  mutate(Race_Mixed = case_when(Race_Mixed > 1 ~1,
                                Race_Mixed <=1 ~ 0)) |> 
  mutate(Race = case_when(Race_Mixed == 1 ~ 5,
                          Race_AA_Black == 1 & Race_Mixed == 0 ~ 4,
                          Race_NativeAmerican == 1 & Race_Mixed == 0~ 3,
                          Race_Hispanic_Latino == 1 & Race_Mixed == 0 ~ 2,
                          Race_Asian == 1 & Race_Mixed == 0 ~ 1,
                          Race_Pacific_Island == 1 & Race_Mixed == 0 ~ 1, 
                          Race_White == 1 & Race_Mixed == 0 ~ 0,
  )) 

DCF <- DCF |> 
  mutate(Race = labelled(DCF$Race, c('White' = 0, 'AAPI' = 1, 'Hispanic/Latino' = 2, 'Native American' = 3, 'Black' = 4, 'Mixed Race' = 5))) |> 
  rename(timepoint = 'redcap_event_name') |> 
  rename(id = 'record_id') %>% 
  select(id, dcf_date,timepoint, dcf_age, dcf_student, sex_orientation, dcf_gender, dcf_gender_other, dcf_working, dcf_living,Race, Race_nonwhite, Race_White:Race_other)

label(DCF$dcf_age)="date of assessment minus birthday"
label(DCF$sex_orientation)="With which sexual orientation do you primarily identify?"
label(DCF$dcf_gender)="Gender"
label(DCF$dcf_date)="date of assessment"
label(DCF$dcf_student)="Are you a student?"
label(DCF$dcf_working)="Are you currently working?"
label(DCF$dcf_living)="Where are you living/staying right now?"


## 3.2. Weight Suppression Variables

WT_VARS <- data %>% 
  select(record_id, redcap_event_name, starts_with('ed100k')) %>% 
  mutate( ed100k_wt_high_corrected = ifelse(ed100k_most_lbs_kg == 2, ed100k_wt_hi_lb*2.2046, ed100k_wt_hi_lb)) %>% 
  mutate( ed100k_wt_suppress_high_current =  ed100k_wt_high_corrected - ed100k_wt_current_correct) %>%  
  mutate( ed100k_wt_suppress_high_AN = ed100k_wt_high_corrected - ed100k_wt_an_correct) |> 
  mutate( ed100k_wt_suppress_current_AN = ed100k_wt_current_correct - ed100k_wt_an_correct) |> 
  mutate( ed100k_wt_suppress_high_lowest = ed100k_wt_high_corrected - ed100k_wt_low_correct) |> 
  mutate( ed100k_bmi_suppress_high_current = round(703*ed100k_wt_high_corrected/(ed100k_height^2), 2) -  round(703*ed100k_wt_current_correct/(ed100k_height^2),2)) |> 
  mutate( ed100k_bmi_suppress_high_lowest = round(703*ed100k_wt_high_corrected/(ed100k_height^2), 2) -  round(703*ed100k_wt_low_correct/(ed100k_height^2),2)) |>
  mutate( ed100k_bmi_suppress_high_AN = round(703*ed100k_wt_high_corrected/(ed100k_height^2), 2) -  round(703*ed100k_wt_an_correct/(ed100k_height^2),2)) |>
  mutate( ed100k_bmi_suppress_current_AN = round(703*ed100k_wt_current_correct/(ed100k_height^2), 2) -  round(703*ed100k_wt_an_correct/(ed100k_height^2),2)) %>% 
  rename(timepoint = 'redcap_event_name') |> 
  rename(id = 'record_id') 

## Group Data

Group <- data %>% 
  select(record_id, randomization) %>% 
  rename(group = 'randomization') %>% 
  rename(id = 'record_id') %>% 
  mutate(group = recode(group, `1` = 'BP', `2` = 'FP')) %>% 
  filter (!is.na(group)) %>% 
  distinct()
  

# 4. Long Dataset

cleaned_data <- cleaned_data %>% 
  append(list(Group)) %>% 
  append(list(DCF)) %>% 
  append(list(WT_VARS)) 

names(cleaned_data)[[length(cleaned_data)-2]] <- 'Group'
names(cleaned_data)[[length(cleaned_data)-1]] <- 'DCF'
names(cleaned_data)[[length(cleaned_data)]] <- 'WT_VARS'


# 5. Clear environment 
rm(list = setdiff(ls(), 'cleaned_data'))

long_data <- cleaned_data[[1]]

j = 2
while (j <= length(cleaned_data)) {
  long_data <- full_join(long_data, cleaned_data[[j]])
  j = j+1
}


long_data <- long_data %>% 
  mutate(timepoint = recode(timepoint, baseline_arm_1 = 'baseline', postsession_arm_1= 'post', `8_week_postsession_arm_1` = '8wk')) %>% 
  filter(!timepoint %in% c('consent_form_arm_1', 'presession_iataat_arm_1', 'session_one_attend_arm_1', 'session_two_attend_arm_1', 'post_session_iat_arm_1', 'postsession_iataat_arm_1'))



# 5. Wide Dataset
cols <- colnames(long_data)
cols <- cols[-c(1,2)]

wide_data <- long_data %>% 
  tidyr::pivot_wider(names_from = timepoint, 
                     values_from = cols)


# Datasets

BAM2_redcap <- cleaned_data 
BAM2_redcap_long <- long_data
BAM2_redcap_wide <- wide_data


# Load  and join with 2020-2022 data
load('pre_scheduling/BAM1_redcap_long.RData')
load('pre_scheduling/BAM1_redcap_wide.RData')

BAM_redcap_long_merged <- full_join(BAM1_redcap_long, BAM2_redcap_long)
BAM_redcap_wide_merged <- full_join(BAM1_redcap_wide, BAM2_redcap_wide)

# Move old files
filenames <- as.list(list.files())
currentDate <- Sys.Date()


old_long <- filenames[grepl('BAM_redcap_long_merged[^|]*$', filenames)][[1]]
old_wide <- filenames[grepl('BAM_redcap_wide_merged[^|]*$', filenames)][[1]]
old_raw <- filenames[grepl('BAM2_redcap_raw[^|]*$', filenames)][[1]]

file.copy (from = paste0(old_long), to = paste0('old/',old_long))
file.remove (from = paste0(old_long))

file.copy (from = paste0(old_wide), to = paste0('old/',old_wide))
file.remove (from = paste0(old_wide))

file.copy (from = paste0(old_raw), to = paste0('old/',old_raw))
file.remove (from = paste0(old_raw))

# Save!
longfilename <- paste('BAM_redcap_long_merged.', currentDate, '.RData', sep = "")
save(BAM_redcap_long_merged, file = longfilename)
widefilename <- paste('BAM_redcap_wide_merged.', currentDate, '.RData', sep = "")
save(BAM_redcap_wide_merged, file = widefilename)

rm(list = ls())


