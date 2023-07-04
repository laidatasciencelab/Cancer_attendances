# Cohort pre-processing

The objective of this code is wrangle raw cohort and attendance data into analysis ready format

R version 3.6.2

## Input files
1) CPRD GOLD & Aurum raw cohort files
2) CPRD GOLD & Aurum primary care attendance files
3) CPRD GOLD & Aurum secondary care attendance, inpatient critical care files
4) CPRD GOLD & Aurum secondary care attendance, inpatient admitted care files
5) CPRD GOLD & Aurum secondary care attendance, outpatient care files
6) Index of Multiple Deprivation linkage file 
7) Office of National Statistics Ethnicity linkage file 
8) Office of National Statistics Mortality linkage file 
9) CPRD GOLD & Aurum linkage eligibility file 
10) Cancer ICD 3 codelist
11) Other conditions ICD 3 codelist

## Importing packages 
```R
library(tidyverse)
library(data.table)
library(tableone)
library(lubridate)
library(dplyr)
library(sqldf)
```
## Creating base GOLD cohort (Part 1)
```R
# Load file for GOLD individuals with cancer diagnosis 
cancer_gold = readRDS("gold_cancer_patients_full.rds")

# Importing all unique individuals from list of individuals with cancer
cancer_gold_master = cancer_gold %>% select(patid) 
    %>% distinct(patid)

# Load file for raw GOLD cohort 
cohort_gold = read.csv("Gold_Cohort.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

# Minimise GOLD cohort file 
cohort_gold = cohort_gold %>% select(patid, pracid, prac_region, gender, dob, data_start, data_end)

# Formatting GOLD cohort 
cohort_gold = cohort_gold %>% mutate(dob = ymd(dob)) 
    %>% mutate(data_start = ymd(data_start)) 
    %>% mutate(data_end = ymd(data_end)) 
    %>% mutate(patid = as.character(patid))

# Merge distinct GOLD patids with minimised GOLD cohort file to form GOLD master
cancer_gold_master = merge(x=cancer_gold_master, y=cohort_gold, by="patid", all.x=TRUE)

# Load file for ONS mortality dates
ONS_gold = readRDS("ONS_gold.rds")

# Format ONS mortality file 
ONS_gold = ONS_gold %>% select(patid, dod) 
    %>% mutate(patid = as.character(patid)) 
    %>% mutate(dod = ymd(dod))

# Merge mortality data with GOLD master
cancer_gold_master = merge(x=cancer_gold_master, y=ONS_gold, by="patid", all.x=TRUE)
cancer_gold_master = cancer_gold_master %>% mutate(dead = ifelse(is.na(dod),0,1))

# Load file for ONS ethnicity for primary and secondary care
eth_db_gold = read.csv("cal_ethnic_cprd_demo_bio_3.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
eth_hes_gold = read.csv("hes_patient_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

# Format ethnicity for primary care 
eth_db_gold = eth_db_gold %>% select(patid, category) 
    %>% mutate(patid = as.character(patid))
    %>% mutate(category = recode(category,
                                                       '1' = 'White',
                                                       '2' = 'White',
                                                       '3' = 'White',
                                                       '4' = 'White',
                                                       '5' = 'Mixed',
                                                       '6' = 'Mixed',
                                                       '7' = 'Mixed',
                                                       '8' = 'Mixed',
                                                       '9' = 'Mixed',
                                                       '10' = 'Asian',
                                                       '11' = 'Asian',
                                                       '12' = 'Asian',
                                                       '13' = 'Asian',
                                                       '15' = 'Black',
                                                       '16' = 'Black',
                                                       '17' = 'Black',
                                                       '19' = 'Asian',
                                                       '20' = 'Other'))
    %>% drop_na(category)
    %>% distinct()
    %>% rename(eth_db_gold = category)

# Merge primary care ethnicity with GOLD master
cancer_gold_master = merge(x=cancer_gold_master, y=eth_db_gold, by="patid", all.x=TRUE)

# Format ethnicity for secondary care 
eth_hes_gold = eth_hes_gold %>% select(patid, gen_ethnicity)
    %>% mutate(patid = as.character(patid))
    %>% mutate(gen_ethnicity = recode(gen_ethnicity,
                                                       'White' = 'White',
                                                       'Other' = 'Other',
                                                       'Pakistani' = 'Asian',
                                                       'Indian' = 'Asian',
                                                       'Chinese' = 'Asian',
                                                       'Oth_Asian' = 'Asian',
                                                       'Bl_Afric' = 'Black',
                                                       'Bl_Carib' = 'Black',
                                                       'Bl_Other' = 'Black',
                                                       'Mixed' = 'Mixed',
                                                       'Bangladesi' = 'Asian'))
    %>% filter(gen_ethnicity %in% c("White", "Other", "Asian", "Black", "Mixed"))
    %>% drop_na(category)
    %>% distinct()
    %>% rename(eth_hes_gold = gen_ethnicity)

# Merge secondary care ethnicity with GOLD master
cancer_gold_master = merge(x=cancer_gold_master, y=eth_hes_gold, by="patid", all.x=TRUE)

# Ethnicity variable 
cancer_gold_master = cancer_gold_master %>% mutate(ethnicity = ifelse(!(is.na(eth_hes_gold)), eth_hes_gold, eth_db_gold))
    %>% replace(is.na(ethnicity), "Unknown")

# Remove redundant columns 
cancer_gold_master$eth_db_gold = NULL
cancer_gold_master$eth_hes_gold = NULL

# Load file for IMD 
IMD_gold = read.csv("patient_imd2019_22_001756.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

# Format IMD 
IMD_gold = IMD_gold %>% select(patid, e2019_imd_5)
    %>% mutate(patid = as.character(patid))
    %>% rename(IMD_5 = e2019_imd_5)
    %>% replace(is.na(IMD_5), "Unknown_IMD")

# Merge IMD with GOLD master 
cancer_gold_master = merge(x=cancer_gold_master, y=IMD_gold, by="patid", all.x=TRUE)

# Load linkage eligibility file
enhanced_egil_gold = read.csv("gold_cancer_patids_merge_source_397162.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

# QC linkage eligibility
cancer_gold_master = cancer_gold_master %>% filter(patid %in% enhance_egil_gold$patid)

# At this stage it is necessary to process the primary and secondary care data first in order to add the variables of "cancer index diagnosis", "cancer index date", "comorbidity count" and "Age"
```

## Identifying GOLD cohort cancer index diagnoses
```R
# Loading attendance data files 

# GOLD hes apc events epi
hes_apc_epi_diag = read.csv("hes_diagnosis_epi_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes apc events hosp
hes_apc_hosp_diag = read.csv("hes_diagnosis_hosp_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes apc events primary hosp 
hes_apc_hosp_p_diag = read.csv("hes_primary_diag_hosp_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes op clinical events
filenames = list.files("hesop_clin", pattern="*.txt", full.names=TRUE)
hes_op_yearly = lapply(filenames, function(i){read.csv(i,header=TRUE, sep="\t",stringsAsFactors = FALSE)})

# Identifying cancer diagnoses from primary care
CA_gp_gold = cancer_gold %>% select(patid, eventdate, condition)
    %>% distinct()
    %>% mutate(eventdate = ymd(eventdate))
    %>% rename(condition_index_date = eventdate)
    %>% drop_na(condition_index_date)

# Identifying cancer diagnoses from secondary care

# HES APC 



```

## Identifying GOLD cohort comorbidities diagnoses


## Creating base GOLD cohort (Part 2)






## Creating base Aurum cohort (Part 1)
```R
# Load file for Aurum individuals with cancer diagnosis 
cancer_aurum = readRDS("aurum_cancer_patients_full.rds")

# Importing all unique individuals from list of individuals with cancer
cancer_aurum_master = cancer_aurum %>% select(patid) 
    %>% distinct(patid)

# Load file for raw Aurum cohort 
cohort_aurum = read.csv("Aurum_Cohort.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

# Minimise Aurum cohort file 
cohort_aurum = cohort_aurum %>% select(patid, pracid, prac_region, gender, dob, data_start, data_end)

# Formatting Aurum cohort 
cohort_aurum = cohort_aurum %>% mutate(dob = ymd(dob)) 
    %>% mutate(data_start = ymd(data_start)) 
    %>% mutate(data_end = ymd(data_end)) 
    %>% mutate(patid = as.character(patid))

# Merge distinct Aurum patids with minimised Aurum cohort file to form Aurum master
cancer_aurum_master = merge(x=cancer_aurum_master, y=cohort_aurum, by="patid", all.x=TRUE)

# Load file for ONS mortality dates
ONS_aurum = readRDS("ONS_aurum.rds")

# Format ONS mortality file 
ONS_aurum = ONS_aurum %>% select(patid, dod) 
    %>% mutate(patid = as.character(patid)) 
    %>% mutate(dod = ymd(dod))

# Merge mortality data with Aurum master
cancer_aurum_master = merge(x=cancer_aurum_master, y=ONS_aurum, by="patid", all.x=TRUE)
cancer_aurum_master = cancer_aurum_master %>% mutate(dead = ifelse(is.na(dod),0,1))

# Load file for ONS ethnicity
eth_hes_aurum = read.csv("hes_patient_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

# Format ethnicity 
eth_hes_aurum = eth_hes_aurum %>% select(patid, gen_ethnicity)
    %>% mutate(patid = as.character(patid))
    %>% mutate(gen_ethnicity = recode(gen_ethnicity,
                                                       'White' = 'White',
                                                       'Other' = 'Other',
                                                       'Pakistani' = 'Asian',
                                                       'Indian' = 'Asian',
                                                       'Chinese' = 'Asian',
                                                       'Oth_Asian' = 'Asian',
                                                       'Bl_Afric' = 'Black',
                                                       'Bl_Carib' = 'Black',
                                                       'Bl_Other' = 'Black',
                                                       'Mixed' = 'Mixed',
                                                       'Bangladesi' = 'Asian'))
    %>% filter(gen_ethnicity %in% c("White", "Other", "Asian", "Black", "Mixed"))
    %>% drop_na(category)
    %>% distinct()
    %>% rename(eth_hes_aurum = ethnicity)
    %>% replace(is.na(ethnicity), "Unknown")

# Merge secondary care ethnicity with Aurum master
cancer_aurum_master = merge(x=cancer_aurum_master, y=eth_hes_aurum, by="patid", all.x=TRUE)

# Load file for IMD 
IMD_aurum = read.csv("patient_imd2019_22_001756.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

# Format IMD 
IMD_aurum = IMD_aurum %>% select(patid, e2019_imd_5)
    %>% mutate(patid = as.character(patid))
    %>% rename(IMD_5 = e2019_imd_5)
    %>% replace(is.na(IMD_5), "Unknown_IMD")

# Merge IMD with Aurum master 
cancer_aurum_master = merge(x=cancer_aurum_master, y=IMD_aurum, by="patid", all.x=TRUE)

# Load linkage eligibility file
enhanced_egil_aurum = read.csv("aurum_cancer_patids_merge_source_1100152.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

# QC linkage eligibility
cancer_aurum_master = cancer_aurum_master %>% filter(patid %in% enhanced_egil_aurum$patid)

# At this stage it is necessary to process the primary and secondary care data first in order to add the variables of "cancer index diagnosis", "cancer index date", "comorbidity count" and "Age"
```
## Identifying Aurum cohort cancer index diagnoses


## Identifying Aurum cohort comorbidities diagnoses


## Creating base Aurum cohort (Part 2)



## Combining Gold and Aurum cohort masters

```R
# QC sex variable
cancer_master = cancer_master %>% filter(gender %in% c("1", "2"))

# QC age variable (QC1)
cancer_gold_master$QC1 = difftime(cancer_gold_master$dod,cancer_gold_master$dob,units="weeks")

```