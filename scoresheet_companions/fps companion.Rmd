---
title: "fps companion"
author: "Danielle Lennon"
date: '2022-05-13'
output: html_document
---

## 1. Background

This companion file is for the shortened Fat Phobia Scale (FPS) which utilizes 14 questions regarding fatphobia instead of the total 50. It is intended to measure fat phobia levels in the tested population through various personality traits and whether there is bias when people apply them to an overweight person (Bacon et al., 2001) When tested, the shortened FPS questionnaire still demonstrated high reliability when compared to the original 50 question scale (Bacon et al., 2001). FPS uses a 5-point sliding scale with 1 being equal to a different negative descriptive variable for each respective question and 5 being equal to a different positive variable for each respective question. 


[Fat phobia scale revisited: the short form ](https://www.nature.com/articles/0801537)

##
## 2. Scoresheet

In order to clean the data:

- Select raw variables being used

- Rename variables to include descriptive terms from questions in the scale

- Recode scores so that the negative descriptor is labeled as 1 and the positive descriptor is labeled as 5

- Average the total scores and rename as `fps_mean`
##
## 3. Key Variables

`fps_mean` (average of scores in response to questions asking how different negative and positive descriptors should be applied to a fat person)

##
```{r, echo=FALSE}
library(readxl)
fps_clean <- read_excel("~/fps_clean.xlsx")
library (cgwtools)
library(scorekeeper)

cleaned_data <- scorekeep(BAM_RA_raw, tmfs_clean)

cleaned_tmfs_data <- cleaned_data[[4]]

```
