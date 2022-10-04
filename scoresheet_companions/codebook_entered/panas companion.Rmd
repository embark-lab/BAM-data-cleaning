---
title: "panas companion"
author: "Danielle Lennon"
date: '2022-05-12'
output: html_document
---

## 1. Background

This companion file is for the Positive and Negative Affect Schedule (PANAS) which was introduced in 1988 as a two-factor scale to measure one's positive and negative moods (Watson et al., 1988). Compared to other scales measuring positive and negative affect, PANAS has high internal consistency and validity (Watson et al., 1988). In addition, the scale also shows consistent reliabiltiy, even when measured across sexes (Crawford et al., 2004). PANAS uses a 5-point likert scale with 1 being equal to very slightly or not at all and 5 being equal to extremely. In terms of scoring, sums of scores are separated for questions regarding positive affect and questions reagrding negative affect. 


[Development and Validation of Brief Measures of Positive and Negative Affect: The PANAS Scales](https://psycnet.apa.org/fulltext/1988-31508-001.pdf?auth_token=bfee3b8066fabebd06ab79822ba512d821684b31)
[The Positive and Negative Affect Schedule (PANAS): Construct validity, measurement properties and normative data in a large non-clinical sample](https://bpspsychub.onlinelibrary.wiley.com/doi/pdf/10.1348/0144665031752934)
[The Positive and Negative Affect Schedule](https://ogg.osu.edu/media/documents/MB%20Stream/PANAS.pdf)
##
## 2. Scoresheet

In order to clean the data:

- Select raw variables being used

- Rename variables to include panas and numbering for order of questions in the scale 

- Sum the total scores of questions 1, 3, 5, 8, 9, 11, 13, 15, 16, 18 and rename as `panas_positive_sum`

- Sum the total scores of questions 2, 4, 6, 7, 10, 12, 14, 17, 19 and rename as `panas_negative_sum`

##
## 3. Key Variables
 
`panas_positive_sum` (sum of scores for questions identifying positive affect over the course of the last week)
`panas_negative_sum` (sum of scores for questions identifying negative affect over the course of the last week)

##
```{r, echo=FALSE}
library(readxl)
panas_clean <- read_excel("~/panas_clean.xlsx")
library (cgwtools)
library(scorekeeper)

cleaned_data <- scorekeep(BAM_RA_raw, panas_clean)

cleaned_panas_data <- cleaned_data[[4]]

```

