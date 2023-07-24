# Attendance mortality comparison

The objective of this code is to generate cancer mortality counts

R version 3.6.2

## Importing packages
```R
library(tidyverse)
library(data.table)
library(tableone)
library(lubridate)
library(dplyr)
library(forecast)
```

## Input files
1) ONS Gold & Aurum mortality files
2) Cancer master file
3) Cancer ICD 3 codelist

## Generating cancer-specific mortality counts
```R
# Load cancer master file 
cancer_master = readRDS("cancer_master.RDS")

# Load ONS files 
ONS_gold = read.csv("death_patient_22_001756_DM_gold.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
ONS_aurum = read.csv("death_patient_22_001756_DM_aurum.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

# Load cancer ICD 3 codelist
cancer_ICD_codelist = read.csv("Cancer_ICD.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

# ONS minimisation 
ONS_gold = ONS_gold %>% select(1,5,9:24)
ONS_aurum = ONS_aurum %>% select(1,5,9:24)
ONS_all = rbind(ONS_gold, ONS_aurum)

# Formatting and filtering
ONS_all = ONS_all %>% mutate(patid = as.character(patid))
    %>% mutate(dod = ymd(dod))
    %>% filter(patid %in% cancer_master$patid)
    %>% filter(dod > "2017-12-31")

# ONS filter for cancer ICD codes 
ICD_0 = as.data.frame(cancer_ICD_codelist$ICD, stringsAsFactors = FALSE)
ICD_3 = as.data.frame(cancer_ICD_codelist$ICD.3, stringsAsFactors = FALSE)
names(ICD_0) = names(ICD_3)
ICD_codes = rbind(ICD_0, ICD_3)
ICD_codes = ICD_codes %>% rename(codes = `cancer_ICD_codelist$ICD.3`)

# Convert ONS ICD codes to map to cancer ICD codes
ONS_gsub = as.data.frame(lapply(ONS_all[c(3:18)], function(y) gsub(".", "", y, fixed=TRUE)), stringsAsFactors = FALSE)
ONS_all[c(3:18)] = ONS_gsub

# ONS filter for cancer deaths 
ONS_cancer = ONS_all %>% filter_at(vars(3:18), any_vars(. %in% ICD_codes$codes))

# Mortality count per week
mortality_master = ONS_cancer %>% select(patid, dod) 
    %>% drop_na()
    %>% mutate(month_year = format(dod, "%b-%Y"))
    %>% mutate(year = format(dod, "%Y"))
    %>% mutate(week = week(ymd(dod)))

mortality_count_w = mortality_master %>% dplyr::count(year, week)
    %>% rename(count = n)

# Finding out which weeks are missing for count continuity
mortality_dates = data.frame(dates = seq(min(mortality_master$dod), max(mortality_master$dod), by="days"))
missing_dd = as.data.frame(mortality_dates[!mortality_dates$dates %in% mortality_master$dod, ])
missing_dd = missing_dd %>% mutate(patid = "NA")
    %>% select(c(2,1))
    %>% mutate(dod = 2)
missing_dd$month_year = format(missing_dd$dod, "%b-%Y")
missing_dd$year = format(missing_dd$dod, "%Y")
missing_dd = missing_dd %>% mutate(week = week(ymd(event_date))) 
missing_dd_w = missing_dd[!duplicated(missing_dd[c(4,5)]), ]

# Identify weeks with 0 count 
d_count_0 = missing_dd_w[, c(4,5)]
d_count_0$concat = paste(d_count_0$year, d_count_0$week)
mortality_count_w$concat = paste(mortality_count_w$year, mortality_count_w$week)

# These are the missing dates with 0 count from the main attendance master
missing_dd_w = d_count_0 %>% filter(!concat %in% mortality_count_w$concat)
missing_dd_w$concat = NULL
missing_dd_w$n = "0"
missing_dd_w$n = as.integer(missing_dd_w$n)
# 0 missing
mortality_count_w$concat = NULL

# Formatting week 53 as part of week 52
mortality_count_w$n[[158]] = (mortality_count_w$n[[158]] + mortality_count_w$n[[159]])
mortality_count_w$n[[105]] = (mortality_count_w$n[[105]] + mortality_count_w$n[[106]])
mortality_count_w$n[[52]] = (mortality_count_w$n[[52]] + mortality_count_w$n[[53]])
mortality_count_w = mortality_count_w %>% filter(!row_number() %in% c(53,106,159))

# Time from base variable
mortality_count_w$runningweek = 1:nrow(mortality_count_w)
```