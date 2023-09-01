# Cohort pre-processing

The objective of this code is to wrangle raw cohort data into analysis-ready format.

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
12) Conditions unification mapping file

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
    %>% select(-eth_db_gold, -eth_hes_gold)

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
# Loading diagnosis data files 

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

# Load cancer ICD codelist
cancer_ICD_codelist = read.csv("Cancer_ICD.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

# HES APC episodes
CA_hesapc_epi_gold = hes_apc_epi_diag %>% select(patid, epistart, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(epistart = ymd(epistart))
# ICD3 codes came in a mix of 3 digit and 4 digit formats in the data. As such, to fully capture all cancer diagnoses, all 4 digit codes were minimised to 3 digits for filtering, and where available, the 4 digit code was then used to map to the specific diagnosis. 
CA_hesapc_epi_gold = CA_hesapc_epi_gold %>% mutate(ICD = gsub(".", "", CA_hesapc_epi_gold$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(CA_hesapc_epi_gold$ICD, 1, 4))
    %>% mutate(ICD3 = substr(CA_hesapc_epi_gold$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

# HES APC hospital (2 datasets)
CA_hesapc_hosp_gold_npr = hes_apc_hosp_diag %>% select(patid, admidate, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(admidate = ymd(admidate))

CA_hesapc_hosp_gold_npr = CA_hesapc_hosp_gold_npr %>% mutate(ICD = gsub(".", "", CA_hesapc_hosp_gold_npr$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(CA_hesapc_hosp_gold_npr$ICD, 1, 4))
    %>% mutate(ICD3 = substr(CA_hesapc_hosp_gold_npr$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

CA_hesapc_hosp_gold_pr = hes_apc_hosp_p_diag %>% select(patid, admidate, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(admidate = ymd(admidate))

CA_hesapc_hosp_gold_pr = CA_hesapc_hosp_gold_pr %>% mutate(ICD = gsub(".", "", CA_hesapc_epi_gold$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(CA_hesapc_hosp_gold_pr$ICD, 1, 4))
    %>% mutate(ICD3 = substr(CA_hesapc_hosp_gold_pr$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

CA_hesapc_hosp_gold = rbind(CA_hesapc_hosp_gold_npr, CA_hesapc_hosp_gold_pr)

# HES OP 
hes_op_minimised = lapply(
  hes_op_yearly,
  function(df){
    df = df %>% select(patid, diag_01, HES_yr)
        %>% mutate(patid = as.character(patid))
        %>% mutate(diag_01 = gsub(".","", df$diag_01, fixed=TRUE))
        %>% mutate(ICD4 = substr(df$diag_01, 1, 4))
        %>% mutate(ICD3 = substr(df$diag_01, 1, 3))
        %>% select(-diag_01)
        %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)
        %>% mutate(HES_yr = ymd(HES_yr))
    lubridate::day(df$HES_yr) = 1
    lubridate::month(df$HES_yr) = 4
    return(df)
  }
)

CA_hesop_gold = bind_rows(hes_op_minimised)

# Unifying all cancer event files - renaming date column 
colnames(CA_gp_gold)[2] = "cancer_index_date"
colnames(CA_hesapc_epi_gold)[2] = "cancer_index_date"
colnames(CA_hesapc_hosp_gold)[2] = "cancer_index_date"
colnames(CA_hesop_gold)[2] = "cancer_index_date"

# Unifying all event files - adding data tags 
CA_gp_gold$data_type = "GP"
CA_hesapc_epi_gold$data_type = "HES_apc_epi"
CA_hesapc_hosp_gold$data_type = "HES_apc_hosp"
CA_hesop_gold$data_type = "HES_op"

# Unifying all event files - ICD to CPRD translation for HES data
CA_hes_all_gold = rbind(CA_hesapc_epi_gold, CA_hesapc_hosp_gold, CA_hesop_gold)

# Merging 4 digit ICD3 codes
colnames(cancer_ICD_codelist)[1] = "ICD4"
colnames(cancer_ICD_codelist)[2] = "ICD3_code"
CA_hes_all_gold = merge(x=CA_hes_all_gold, y=cancer_ICD_codelist, by="ICD4", all.x=TRUE)

# Merging 3 digit ICD3 codes
colnames(cancer_ICD_codelist)[1] = "ICD4_code"
colnames(cancer_ICD_codelist)[2] = "ICD3"
colnames(cancer_ICD_codelist)[3] = "CPRD_final_ICD3"
CA_hes_all_gold = merge(x=CA_hes_all_gold, y=cancer_ICD_codelist, by="ICD3", all.x=TRUE)

# Unifying names for hes condition (CPRD_final = condition name in translation file)
CA_hes_all_gold = CA_hes_all_gold %>% select(-ICD3_code, -ICD4_code)
    %>% mutate(cancer_diagnosis = ifelse(!is.na(CPRD_final), CPRD_final, CPRD_final_ICD3))
    %>% distinct()
    %>% select(-CPRD_final, -CPRD_final_ICD3)

# Cancer event table
CA_hes_all_gold = CA_hes_all_gold %>% select(c(3,6,4,5,1,2))
    %>% select(c(1,2,3,4))
CA_gp_gold = CA_gp_gold %>% rename(cancer_diagnosis = condition)
    %>% select(c(1,3,2,4))
CA_all_gold = rbind(CA_gp_gold, CA_hes_all_gold)
CA_all_gold = CA_all_gold %>% filter(patid %in% cancer_gold_master$patid)

# Capturing earliest cancer diagnosis 
index_cancer = CA_all_gold %>% select(c(1,2,3))
DT = data.table(index_cancer)
index_cancer = unique(DT[order(cancer_index_date)], by="patid", fromLast = FALSE)
index_cancer = arrange(index_cancer, patid)
```

## Identifying GOLD cohort comorbidities diagnoses
```R
# Loading diagnosis data files 
gold_condition_files = list.files("Gold_conditions", pattern="*.txt", full.names=TRUE)
output.list = list()
# Otherwise previously loaded attendance data files used

# Identifying non-cancer diagnoses from primary care (individual files by non-cancer diagnosis)
for (i in 1:length(gold_condition_files)){
  # Extract condition name
  condition = basename(gold_condition_files[i]) %>%
    str_remove(pattern = "cal_") %>%
    str_remove(pattern = "_1_Disease.txt")
  # Read in file and reformat
  output.list[[i]] = fread(gold_condition_files[i], colClasses = "character") %>% 
    mutate(condition = condition)
}
# Minimisation of data fields 
gold_conditions = lapply(
  output.list, 
  function(df) {
    df = df %>% select(patid, eventdate, condition)
    return(df)
  }
)

MM_gp_gold = rbindlist(gold_conditions)
MM_gp_gold = MM_gp_gold %>% mutate(condition_index_date = ymd(condition_index_date))
    %>% filter(patid %in% cancer_gold_master$patid)
    %>% drop_na(condition_index_date)

# Capturing earliest diagnosis for each condition per patient (GP data)
DTmm = data.table(MM_gp_gold)
index_mm_gp_gold = unique(DTmm[order(condition,eventdate)], by=c("patid","condition"), fromLast=FALSE) 
index_mm_gp_gold = arrange(index_mm_gp_gold, patid)
index_mm_gp_gold = index_mm_gp_gold %>% mutate(condition_index_date = eventdate)
    %>% mutate(condition_diagnosis = condition)
    %>% select(c(1,3,2))
    %>% mutate(condition_index_date = ymd(condition_index_date))

# Identifying non-cancer diagnoses from secondary care
# Load non-cancer ICD codelist
MM_ICD_codelist = read.csv("Multimorbidity_ICD.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

# HES APC episodes
MM_hesapc_epi_gold = hes_apc_epi_diag %>% select(patid, epistart, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(epistart = ymd(epistart))
    %>% filter(patid %in% cancer_gold_master$patid)
# ICD3 codes came in a mix of 3 digit and 4 digit formats in the data. As such, to fully capture all cancer diagnoses, all 4 digit codes were minimised to 3 digits for filtering, and where available, the 4 digit code was then used to map to the specific diagnosis. 
MM_hesapc_epi_gold = MM_hesapc_epi_gold %>% mutate(ICD = gsub(".", "", MM_hesapc_epi_gold$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(MM_hesapc_epi_gold$ICD, 1, 4))
    %>% mutate(ICD3 = substr(MM_hesapc_epi_gold$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)


# HES APC hospital (2 datasets)
MM_hesapc_hosp_gold_npr = hes_apc_hosp_diag %>% select(patid, admidate, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(admidate = ymd(admidate))
    %>% filter(patid %in% cancer_gold_master$patid)

MM_hesapc_hosp_gold_npr = MM_hesapc_hosp_gold_npr %>% mutate(ICD = gsub(".", "", MM_hesapc_hosp_gold_npr$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(MM_hesapc_hosp_gold_npr$ICD, 1, 4))
    %>% mutate(ICD3 = substr(MM_hesapc_hosp_gold_npr$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

MM_hesapc_hosp_gold_pr = hes_apc_hosp_p_diag %>% select(patid, admidate, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(admidate = ymd(admidate))
    %>% filter(patid %in% cancer_gold_master$patid)

MM_hesapc_hosp_gold_pr = MM_hesapc_hosp_gold_pr %>% mutate(ICD = gsub(".", "", MM_hesapc_hosp_gold_pr$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(MM_hesapc_hosp_gold_pr$ICD, 1, 4))
    %>% mutate(ICD3 = substr(MM_hesapc_hosp_gold_pr$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

MM_hesapc_hosp_gold = rbind(MM_hesapc_hosp_gold_npr, MM_hesapc_hosp_gold_pr)

# HES OP 
hes_op_minimised = lapply(
  hes_op_yearly,
  function(df){
    df = df %>% select(patid, diag_01, HES_yr)
        %>% mutate(patid = as.character(patid))
        %>% filter(patid %in% cancer_gold_master$patid)
        %>% mutate(diag_01 = gsub(".","", df$diag_01, fixed=TRUE))
        %>% mutate(ICD4 = substr(df$diag_01, 1, 4))
        %>% mutate(ICD3 = substr(df$diag_01, 1, 3))
        %>% select(-diag_01)
        %>% filter(ICD3 %in% MM_ICD_codelist$ICD.3)
        %>% mutate(HES_yr = ymd(HES_yr))
    lubridate::day(df$HES_yr) = 1
    lubridate::month(df$HES_yr) = 4
    return(df)
  }
)

MM_hesop_gold = bind_rows(hes_op_minimised)

# Unifying all non-cancer event files - renaming date column
colnames(MM_gp_gold)[2] = "condition_index_date"
colnames(MM_hesapc_epi_gold)[2] = "condition_index_date"
colnames(MM_hesapc_hosp_gold)[2] = "condition_index_date"
colnames(MM_hesop_gold)[2] = "condition_index_date"

# Unifying all event files - adding data tags
MM_gp_gold$data_type = "GP"
MM_hesapc_epi_gold$data_type = "HES_apc_epi"
MM_hesapc_hosp_gold$data_type = "HES_apc_hosp"
MM_hesop_gold$data_type = "HES_op"

# Unifying all event files - ICD to CPRD translation for HES data
MM_hes_all_gold = rbind(MM_hesapc_epi_gold, MM_hesapc_hosp_gold, MM_hesop_gold)

# Merging 4 digit ICD3 codes
colnames(MM_ICD_codelist)[1] = "ICD4"
colnames(MM_ICD_codelist)[2] = "ICD3_code"
MM_hes_all_gold = merge(x=MM_hes_all_gold, y=MM_ICD_codelist, by="ICD4", all.x=TRUE)

# Merging 3 digit ICD3 codes
colnames(MM_ICD_codelist)[1] = "ICD4_code"
colnames(MM_ICD_codelist)[2] = "ICD3"
colnames(MM_ICD_codelist)[3] = "CPRD_final_ICD3"
MM_hes_all_gold = merge(x=MM_hes_all_gold, y=MM_ICD_codelist, by="ICD3", all.x=TRUE)

# Unifying names for hes condition (CPRD_final = condition name in translation file)
MM_hes_all_gold = MM_hes_all_gold %>% select(-ICD3_code, -ICD4_code)
    %>% mutate(cancer_diagnosis = ifelse(!is.na(CPRD_final), CPRD_final, CPRD_final_ICD3))
    %>% distinct()
    %>% select(-CPRD_final, -CPRD_final_ICD3)

# Non-cancer event table (HES only, as GP was done earlier)
MM_hes_all_gold = MM_hes_all_gold %>% select(c(3,6,4,5,1,2))
    %>% select(c(1,2,3,4))

# Capturing earliest diagnosis for each unique condition (HES data)
index_mm_hes_gold = MM_hes_all_gold %>% select(c(1,2,3))
DTmm = data.table(MM_hes_all_gold)
index_mm_hes_gold = unique(DTmm[order(condition_diagnosis,condition_index_date)], by=c("patid","condition_diagnosis"), fromLast=FALSE) 
index_mm_hes_gold = arrange(index_mm_hes_gold, patid)

# Merging primary and secondary care for all multimorbidity diagnoses
MM_gp_gold = MM_gp_gold %>% rename(condition_diagnosis = condition)
    %>% select(c(1,3,2,4))
MM_all_gold = rbind(MM_gp_gold, MM_hes_all_gold)

# Capturing earliest diagnosis for each unique condition (all data)
index_multimorbidity_gold = rbind(index_mm_gp_gold, index_mm_hes_gold)
DTmm = data.table(index_multimorbidity_gold)
index_multimorbidity_gold = unique(DTmm[order(condition_diagnosis,condition_index_date)], by=c("patid","condition_diagnosis"), fromLast=FALSE)
index_multimorbidity_gold = arrange(index_multimorbidity_gold, patid)

# Transformation of diagnosis table into wide format
index_multimorbidity_gold_wide = reshape(index_multimorbidity_gold, idvar="patid", v.names="condition_index_date", timevar="condition_diagnosis", direction="wide")
colnames(index_multimorbidity_gold_wide) = gsub(pattern="condition_index_date.", replacement="", x=colnames(index_multimorbidity_gold_wide))
index_multimorbidity_gold_wide = as.data.frame(index_multimorbidity_gold_wide)
```

## Creating base GOLD cohort (Part 2)
```R
# Index cancer diagnosis and index date variables
cancer_gold_master = merge(x=cancer_gold_master, y=index_cancer, by="patid", all.x=TRUE)
cancer_gold_master = cancer_gold_master %>% drop_na(cancer_index_date)

# Age variable 
cancer_gold_master$age = interval(start = cancer_gold_master$dob, end = cancer_gold_master$cancer_index_date)/duration(n=1, unit="years")

# Comorbidity count 
temp_mm_master = cancer_gold_master[, c(1,14)]
calculation_mm = merge(x=index_multimorbidity_gold_wide, y=temp_mm_master, by="patid", all.x=TRUE)
mm_patid = calculation_mm[, 1, drop=FALSE]
calculation_mm$patid = NULL

# Function to calculate time between index cancer date and comorbidities
timecalc = function(x){
  cancer_index = calculation_mm$cancer_index_date + 1
  calculation_mm$x = difftime(cancer_index, x, units = "days")
}

# Calculating sum of conditions prior to cancer index date 
calculation_mm = sapply(calculation_mm, timecalc)
calculation_mm[calculation_mm < 0 | is.na(calculation_mm)] = 0 
calculation_mm[calculation_mm > 0] = 1
calculation_mm = as.data.frame(calculation_mm)
calculation_mm$cancer_index_date = NULL

# Merging condition count with master cohort
mm_patid$condition_count = rowSums(calculation_mm)
cancer_gold_master = merge(x=cancer_gold_master, y=mm_patid, by="patid", all.x=TRUE)
cancer_gold_master = cancer_gold_master %>% mutate(condition_count = ifelse(is.na(condition_count), 0, condition_count))
```

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
```R
# Loading diagnosis data files 

# Aurum hes apc events epi
hes_apc_epi_diag = read.csv("hes_diagnosis_epi_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# Aurum hes apc events hosp
hes_apc_hosp_diag = read.csv("hes_diagnosis_hosp_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# Aurum hes apc events primary hosp 
hes_apc_hosp_p_diag = read.csv("hes_primary_diag_hosp_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# Aurum hes op clinical events
filenames = list.files("hesop_clin_A", pattern="*.txt", full.names=TRUE)
hes_op_yearly = lapply(filenames, function(i){read.csv(i,header=TRUE, sep="\t",stringsAsFactors = FALSE)})

# Loading Aurum cancer codelists
cancer_aurum_codelist = read.csv("cancer_aurum_codelist.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
codes_equivalent = read.csv("Aurum_map_cancer.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

# Identifying cancer diagnoses from primary care
aurum_files = list.files(path = "Aurum_Observation_subsets_headers", pattern = ".RDS", full.names = T)
output.list = list()

for (i in 1:length(aurum_files)){
  # read in file and reformat
  output.list[[i]] = readRDS(aurum_files[i]) %>% 
    select(patid, obsdate, medcodeid) %>%
    filter(medcodeid %in% cancer_aurum_codelist$medcodeid)
}

CA_gp_aurum = rbindlist(output.list)
CA_gp_aurum = CA_gp_aurum %>% distinct()
    %>% filter(patid %in% cancer_aurum_master)
    %>% rename(event_date = obsdate)
    %>% mutate(event_date = ymd(event_date))
    %>% drop_na(event_date)

# Unifying Aurum condition names with Gold and HES conditions
CA_gp_aurum = merge(x=CA_gp_aurum, y=cancer_aurum_codelist, by="medcodeid", all.x=TRUE)
CA_gp_aurum = merge(x=CA_gp_aurum, y=codes_equivalent, by="condition", all.x=TRUE)
CA_gp_aurum = CA_gp_aurum %>% select (-condition, -medcodeid)
    %>% rename(condition = unified_condition)

# Identifying cancer diagnoses from secondary care

# Load cancer ICD codelist
cancer_ICD_codelist = read.csv("Cancer_ICD.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

# HES APC episodes
CA_hesapc_epi_aurum = hes_apc_epi_diag %>% select(patid, epistart, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(epistart = ymd(epistart))
# ICD3 codes came in a mix of 3 digit and 4 digit formats in the data. As such, to fully capture all cancer diagnoses, all 4 digit codes were minimised to 3 digits for filtering, and where available, the 4 digit code was then used to map to the specific diagnosis. 
CA_hesapc_epi_aurum = CA_hesapc_epi_aurum %>% mutate(ICD = gsub(".", "", CA_hesapc_epi_aurum$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(CA_hesapc_epi_aurum$ICD, 1, 4))
    %>% mutate(ICD3 = substr(CA_hesapc_epi_aurum$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

# HES APC hospital (2 datasets)
CA_hesapc_hosp_aurum_npr = hes_apc_hosp_diag %>% select(patid, admidate, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(admidate = ymd(admidate))
    %>% filter(patid %in% cancer_aurum_master$patid)

CA_hesapc_hosp_aurum_npr = CA_hesapc_hosp_aurum_npr %>% mutate(ICD = gsub(".", "", CA_hesapc_hosp_aurum_npr$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(CA_hesapc_hosp_aurum_npr$ICD, 1, 4))
    %>% mutate(ICD3 = substr(CA_hesapc_hosp_aurum_npr$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

CA_hesapc_hosp_aurum_pr = hes_apc_hosp_p_diag %>% select(patid, admidate, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(admidate = ymd(admidate))
    %>% filter(patid %in% cancer_aurum_master$patid)

CA_hesapc_hosp_aurum_pr = CA_hesapc_hosp_aurum_pr %>% mutate(ICD = gsub(".", "", CA_hesapc_epi_aurum$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(CA_hesapc_hosp_aurum_pr$ICD, 1, 4))
    %>% mutate(ICD3 = substr(CA_hesapc_hosp_aurum_pr$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

CA_hesapc_hosp_aurum = rbind(CA_hesapc_hosp_aurum_npr, CA_hesapc_hosp_aurum_pr)

# HES OP 
hes_op_minimised = lapply(
  hes_op_yearly,
  function(df){
    df = df %>% select(patid, diag_01, HES_yr)
        %>% mutate(patid = as.character(patid))
        %>% filter(patid %in% cancer_aurum_master$patid)
        %>% mutate(diag_01 = gsub(".","", df$diag_01, fixed=TRUE))
        %>% mutate(ICD4 = substr(df$diag_01, 1, 4))
        %>% mutate(ICD3 = substr(df$diag_01, 1, 3))
        %>% select(-diag_01)
        %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)
        %>% mutate(HES_yr = ymd(HES_yr))
    lubridate::day(df$HES_yr) = 1
    lubridate::month(df$HES_yr) = 4
    return(df)
  }
)

CA_hesop_aurum = bind_rows(hes_op_minimised)

# Unifying all event files - renaming date column 
colnames(CA_gp_aurum)[2] = "event_date"
colnames(CA_hesapc_epi_aurum)[2] = "event_date"
colnames(CA_hesapc_hosp_aurum)[2] = "event_date"
colnames(CA_hesop_aurum)[2] = "event_date"

# Unifying all event files - adding data tags 
CA_gp_aurum$data_type = "GP"
CA_hesapc_epi_aurum$data_type = "HES_apc_epi"
CA_hesapc_hosp_aurum$data_type = "HES_apc_hosp"
CA_hesop_aurum$data_type = "HES_op"

# Unifying all event files - ICD to CPRD translation for HES data
CA_hes_all_aurum = rbind(CA_hesapc_epi_aurum, CA_hesapc_hosp_aurum, CA_hesop_aurum)

# Merging 4 digit ICD3 codes
colnames(cancer_ICD_codelist)[1] = "ICD4"
colnames(cancer_ICD_codelist)[2] = "ICD3_code"
CA_hes_all_aurum = merge(x=CA_hes_all_aurum, y=cancer_ICD_codelist, by="ICD4", all.x=TRUE)

# Merging 3 digit ICD3 codes
colnames(cancer_ICD_codelist)[1] = "ICD4_code"
colnames(cancer_ICD_codelist)[2] = "ICD3"
colnames(cancer_ICD_codelist)[3] = "CPRD_final_ICD3"
CA_hes_all_aurum = merge(x=CA_hes_all_aurum, y=cancer_ICD_codelist, by="ICD3", all.x=TRUE)

# Unifying names for hes condition (CPRD_final = condition name in translation file)
CA_hes_all_aurum = CA_hes_all_aurum %>% select(-ICD3_code, -ICD4_code)
    %>% mutate(cancer_diagnosis = ifelse(!is.na(CPRD_final), CPRD_final, CPRD_final_ICD3))
    %>% select(-CPRD_final, -CPRD_final_ICD3, -ICD3, -ICD4)
    %>% distinct()

# Cancer event table
CA_hes_all_aurum = CA_hes_all_aurum %>% select(c(1,2,4,3))
    %>% select(c(1,3,2,4))
CA_gp_aurum = CA_gp_aurum %>% rename(cancer_diagnosis = condition)
    %>% select(c(1,3,2,4))
CA_all_aurum = rbind(CA_gp_aurum, CA_hes_all_aurum)
CA_all_aurum = CA_all_aurum %>% filter(patid %in% cancer_aurum_master$patid)

# Capturing earliest cancer diagnosis 

index_cancer_aurum = CA_all_aurum %>% select(c(1,2,3))
DT = data.table(index_cancer_aurum)
index_cancer_aurum = unique(DT[order(cancer_index_date)], by="patid", fromLast = FALSE)
index_cancer_aurum = arrange(index_cancer_aurum, patid)
```

## Identifying Aurum cohort comorbidities diagnoses
```R
# Loading diagnosis data files 
aurum_files = list.files(path = "Aurum_Observation_subsets_headers", pattern = ".RDS", full.names = T)
output.list = list()
# Otherwise previously loaded attendance data files used

# Creating mapping lookup to map Aurum conditions to unified conditions
mm_aurum_codelist = merge(x=mm_aurum_codelist, y=word_codelist, by="condition", all.x=TRUE)

# Identifying non-cancer diagnoses from primary care (individual chunks of Aurum cohort)
for (i in 1:length(aurum_files)){
  # read in file and reformat
  output.list[[i]] = readRDS(aurum_files[i]) %>% 
    select(patid, obsdate, medcodeid) %>%
    filter(medcodeid %in% mm_aurum_codelist$code)
}

MM_gp_aurum = MM_gp_aurum %>% distinct
    %>% rename(event_date = obsdate)
    %>% filter(patid %in% cancer_aurum_master$patid)
    %>% mutate(event_date = ymd(event_date))
    %>% drop_na(event_date)

# Unifying Aurum conditions
colnames(mm_aurum_codelist)[1] = "medcodeid"
MM_gp_aurum = merge(x=MM_gp_aurum, y=mm_aurum_codelist, by="medcodeid", all.x=TRUE, allow.cartesian = TRUE)
MM_gp_aurum = MM_gp_aurum %>% select(-medcodeid)
    %>% rename(condition = unified_condition)

# Capturing earliest diagnosis for each condition per patient (GP data)
DTmm = data.table(MM_gp_aurum)
index_mm_gp_aurum = unique(DTmm[order(condition,event_date)], by=c("patid","condition"), fromLast=FALSE) 
index_mm_gp_aurum = arrange(index_mm_gp_aurum, patid)
index_mm_gp_aurum = index_mm_gp_aurum %>% mutate(condition_index_date = event_date)
    %>% drop_na(condition) # Removing all cancer diagnoses
    %>% mutate(condition_diagnosis = condition)
    %>% select(c(1,3,2))
    %>% mutate(condition_index_date = ymd(condition_index_date))

# Identifying non-cancer diagnoses from secondary care
# Load non-cancer ICD codelist
MM_ICD_codelist = read.csv("Multimorbidity_ICD.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

# HES APC episodes
MM_hesapc_epi_aurum = hes_apc_epi_diag %>% select(patid, epistart, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(epistart = ymd(epistart))
    %>% filter(patid %in% cancer_aurum_master$patid)
# ICD3 codes came in a mix of 3 digit and 4 digit formats in the data. As such, to fully capture all cancer diagnoses, all 4 digit codes were minimised to 3 digits for filtering, and where available, the 4 digit code was then used to map to the specific diagnosis. 
MM_hesapc_epi_aurum = MM_hesapc_epi_aurum %>% mutate(ICD = gsub(".", "", MM_hesapc_epi_aurum$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(MM_hesapc_epi_aurum$ICD, 1, 4))
    %>% mutate(ICD3 = substr(MM_hesapc_epi_aurum$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

# HES APC hospital (2 datasets)
MM_hesapc_hosp_aurum_npr = hes_apc_hosp_diag %>% select(patid, admidate, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(admidate = ymd(admidate))
    %>% filter(patid %in% cancer_aurum_master$patid)

MM_hesapc_hosp_aurum_npr = MM_hesapc_hosp_aurum_npr %>% mutate(ICD = gsub(".", "", MM_hesapc_hosp_aurum_npr$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(MM_hesapc_hosp_aurum_npr$ICD, 1, 4))
    %>% mutate(ICD3 = substr(MM_hesapc_hosp_aurum_npr$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

MM_hesapc_hosp_aurum_pr = hes_apc_hosp_p_diag %>% select(patid, admidate, ICD)
    %>% mutate(patid = as.character(patid))
    %>% mutate(admidate = ymd(admidate))
    %>% filter(patid %in% cancer_aurum_master$patid)

MM_hesapc_hosp_aurum_pr = MM_hesapc_hosp_aurum_pr %>% mutate(ICD = gsub(".", "", MM_hesapc_hosp_aurum_pr$ICD, fixed=TRUE))
    %>% mutate(ICD4 = substr(MM_hesapc_hosp_aurum_pr$ICD, 1, 4))
    %>% mutate(ICD3 = substr(MM_hesapc_hosp_aurum_pr$ICD, 1, 3))
    %>% select(-ICD)
    %>% filter(ICD3 %in% cancer_ICD_codelist$ICD.3)

MM_hesapc_hosp_aurum = rbind(MM_hesapc_hosp_aurum_npr, MM_hesapc_hosp_aurum_pr)

# HES OP 
hes_op_minimised = lapply(
  hes_op_yearly,
  function(df){
    df = df %>% select(patid, diag_01, HES_yr)
        %>% mutate(patid = as.character(patid))
        %>% filter(patid %in% cancer_aurum_master$patid)
        %>% mutate(diag_01 = gsub(".","", df$diag_01, fixed=TRUE))
        %>% mutate(ICD4 = substr(df$diag_01, 1, 4))
        %>% mutate(ICD3 = substr(df$diag_01, 1, 3))
        %>% select(-diag_01)
        %>% filter(ICD3 %in% MM_ICD_codelist$ICD.3)
        %>% mutate(HES_yr = ymd(HES_yr))
    lubridate::day(df$HES_yr) = 1
    lubridate::month(df$HES_yr) = 4
    return(df)
  }
)

MM_hesop_aurum = bind_rows(hes_op_minimised)

# Unifying all event files - renaming date column
colnames(MM_gp_aurum)[2] = "condition_index_date"
colnames(MM_hesapc_epi_aurum)[2] = "condition_index_date"
colnames(MM_hesapc_hosp_aurum)[2] = "condition_index_date"
colnames(MM_hesop_aurum)[2] = "condition_index_date"

# Unifying all event files - adding data tags
MM_gp_aurum$data_type = "GP"
MM_hesapc_epi_aurum$data_type = "HES_apc_epi"
MM_hesapc_hosp_aurum$data_type = "HES_apc_hosp"
MM_hesop_aurum$data_type = "HES_op"

# Unifying all event files - ICD to CPRD translation for HES data
MM_hes_all_aurum = rbind(MM_hesapc_epi_aurum, MM_hesapc_hosp_aurum, MM_hesop_aurum)

# Merging 4 digit ICD3 codes
colnames(MM_ICD_codelist)[1] = "ICD4"
colnames(MM_ICD_codelist)[2] = "ICD3_code"
MM_hes_all_aurum = merge(x=MM_hes_all_aurum, y=MM_ICD_codelist, by="ICD4", all.x=TRUE)

# Merging 3 digit ICD3 codes
colnames(MM_ICD_codelist)[1] = "ICD4_code"
colnames(MM_ICD_codelist)[2] = "ICD3"
colnames(MM_ICD_codelist)[3] = "CPRD_final_ICD3"
MM_hes_all_aurum = merge(x=MM_hes_all_aurum, y=MM_ICD_codelist, by="ICD3", all.x=TRUE)

# Unifying names for hes condition (CPRD_final = condition name in translation file)
MM_hes_all_aurum = MM_hes_all_aurum %>% select(-ICD3_code, -ICD4_code)
    %>% mutate(cancer_diagnosis = ifelse(!is.na(CPRD_final), CPRD_final, CPRD_final_ICD3))
    %>% distinct()
    %>% select(-CPRD_final, -CPRD_final_ICD3)

# Non-cancer event table (HES only, as GP was done earlier)
MM_hes_all_aurum = MM_hes_all_aurum %>% select(c(3,6,4,5,1,2))
    %>% select(c(1,2,3,4))

# Capturing earliest diagnosis for each unique condition (HES data)
index_mm_hes_aurum = MM_hes_all_aurum %>% select(c(1,2,3))
DTmm = data.table(MM_hes_all_aurum)
index_mm_hes_aurum = unique(DTmm[order(condition_diagnosis,condition_index_date)], by=c("patid","condition_diagnosis"), fromLast=FALSE) 
index_mm_hes_aurum = arrange(index_mm_hes_aurum, patid)

# Merging primary and secondary care for all multimorbidity diagnoses
MM_gp_aurum = MM_gp_aurum %>% rename(condition_diagnosis = condition)
    %>% rename(condition_index_date = event_date)
    %>% select(c(1,3,2))
MM_all_aurum = rbind(MM_gp_aurum, MM_hes_all_aurum)

# Capturing earliest diagnosis for each unique condition (all data)
index_multimorbidity_aurum = rbind(index_mm_gp_aurum, index_mm_hes_aurum)
DTmm = data.table(index_multimorbidity_aurum)
index_multimorbidity_aurum = unique(DTmm[order(condition_diagnosis,condition_index_date)], by=c("patid","condition_diagnosis"), fromLast=FALSE)
index_multimorbidity_aurum = arrange(index_multimorbidity_aurum, patid)

# Transformation of diagnosis table into wide format
index_multimorbidity_aurum_wide = reshape(index_multimorbidity_aurum, idvar="patid", v.names="condition_index_date", timevar="condition_diagnosis", direction="wide")
colnames(index_multimorbidity_aurum_wide) = gsub(pattern="condition_index_date.", replacement="", x=colnames(index_multimorbidity_aurum_wide))
index_multimorbidity_aurum_wide = as.data.frame(index_multimorbidity_aurum_wide)

```

## Creating base Aurum cohort (Part 2)
```R
# Index cancer diagnosis and index date variables
cancer_aurum_master = merge(x=cancer_aurum_master, y=index_cancer_aurum, by="patid", all.x=TRUE)
cancer_aurum_master = cancer_aurum_master %>% mutate(cancer_index_diagnosis = cancer_diagnosis)
    %>% drop_na(cancer_index_date)

# Age variable 
cancer_aurum_master$age = interval(start = cancer_aurum_master$dob, end = cancer_aurum_master$cancer_index_date)/duration(n=1, unit="years")

# Comorbidity count 
temp_mm_master = cancer_aurum_master[, c(1,13)]
calculation_mm = merge(x=index_multimorbidity_aurum_wide, y=temp_mm_master, by="patid", all.x=TRUE)
mm_patid = calculation_mm[, 1, drop=FALSE]
calculation_mm$patid = NULL

# Function to calculate time between index cancer date and comorbidities
timecalc = function(x){
  cancer_index = calculation_mm$cancer_index_date + 1
  calculation_mm$x = difftime(cancer_index, x, units = "days")
}

# Calculating sum of conditions prior to cancer index date 
calculation_mm = sapply(calculation_mm, timecalc)
calculation_mm[calculation_mm < 0 | is.na(calculation_mm)] = 0 
calculation_mm[calculation_mm > 0] = 1
calculation_mm = as.data.frame(calculation_mm)
calculation_mm$cancer_index_date = NULL

# Merging condition count with master cohort
mm_patid$condition_count = rowSums(calculation_mm)
cancer_aurum_master = merge(x=cancer_aurum_master, y=mm_patid, by="patid", all.x=TRUE)
cancer_aurum_master = cancer_aurum_master %>% mutate(condition_count = ifelse(is.na(condition_count), 0, condition_count))
```

## Combining Gold and Aurum cohort masters
```R
# Unifying column names
cancer_gold_master$followup = NULL
colnames(cancer_gold_master) = colnames(cancer_aurum_master)

# Combining and arranging by patid 
cancer_master = rbind(cancer_gold_master, cancer_aurum_master)
cancer_master = cancer_master %>% distinct()
cancer_master = arrange(cancer_master, patid)

# Remove patients not registered 1 year prior to study start (1998-01-01)
cancer_master = cancer_master %>% filter(data_start < "1997-01-01") 

# Remove patients with <365 days of follow up
cancer_master %>% mutate(study_end = ymd("2021-03-31"))
cancer_alive = cancer_master %>% filter(dead == "0")
    %>% mutate(endpoint = pmin(cancer_alive$study_end, cancer_alive$data_end))

cancer_dead = cancer_master %>% filter(dead == "1")
    %>% mutate(pmin(cancer_dead$study_end, cancer_dead$data_end, cancer_dead$dod))

cancer_master = rbind(cancer_alive, cancer_dead)
cancer_master %>% mutate(follow_up = difftime(cancer_master$endpoint, cancer_master$cancer_index_date, unit = "days"))
    %>% filter(follow_up >= 365)

# Remove patients where start of data < end of data
cancer_master = cancer_master %>% filter(data_start < data_end)

# Remove patients where date of death < date of birth
cancer_master = cancer_master %>% filter(dod < dob)

# Remove patients with errors in age
cancer_master = cancer_master %>% filter(age >= 0)

# Remove patients with ineligible gender
cancer_master = cancer_master %>% filter(gender %in% c("1", "2"))

# Remove patients with invalid condition counts, manually checked to be errors during input by practice
cancer_master = cancer_master %>% filter(condition_count <72)

# Adding diagnosis time variable 
cancer_master = cancer_master %>% mutate(diag_after_1 = ifelse(cancer_index_date < "2019-03-23", 0, 1))
```
## Incident diagnosis count 
```R
# Wide cohort Aurum
aurum_master = readRDS("cancer_aurum_master.rds")

# Wide cohort Gold
gold_master = readRDS("cancer_gold_master.rds")

# Diagnosis rate by year from 2010
gold_master = gold_master %>% select(patid, cancer_index_date) %>% filter(cancer_index_date > "2009-12-31") %>% mutate(year = format(cancer_index_date, "%Y"))

aurum_master = aurum_master %>% select(patid, cancer_index_date) %>% filter(cancer_index_date > "2009-12-31") %>% mutate(year = format(cancer_index_date, "%Y"))

diag_count_wide = rbind(gold_master, aurum_master)
table(diag_count_wide$year)

weekly_diag_wide = diag_count_wide %>% filter(cancer_index_date > "2012-12-31") %>% mutate(month_year = format(cancer_index_date, "%b-%Y")) %>% mutate(week = format(week(ymd(cancer_index_date))))

# Diagnosis count
diag_count_w = weekly_diag_wide %>% dplyr::count(year, week)

# Finding out which weeks are missing for count continuity
diag_dates = data.frame(dates = seq(min(weekly_diag_wide$cancer_index_date), max(weekly_diag_wide$cancer_index_date), by="days"))

missing_ad = as.data.frame(diag_dates[!diag_dates$dates %in% weekly_diag_wide$cancer_index_date, ])
colnames(missing_ad)[1] = "cancer_index_date"
missing_ad$month_year = format(missing_ad$cancer_index_date, "%b-%Y")
missing_ad$year = format(missing_ad$cancer_index_date, "%Y")
missing_ad = missing_ad %>% mutate(week = week(ymd(cancer_index_date)))

missing_ad_w = missing_ad[!duplicated(missing_ad[c(3,4)]), ]

diag_count_0 = missing_ad_w[, c(3,4)]
diag_count_0$concat = paste(diag_count_0$year, diag_count_0$week)

diag_count_w$concat = paste(diag_count_w$year, diag_count_w$week)

missing_ad_w = diag_count_0 %>% filter(!concat %in% diag_count_w$concat)
diag_count_w$concat = NULL
# All weeks accounted for 

# Formatting week 53 as part of week 52
diag_count_w$n[[423]] = (diag_count_w$n[[423]] + diag_count_w$n[[424]])
diag_count_w$n[[370]] = (diag_count_w$n[[370]] + diag_count_w$n[[371]])
diag_count_w$n[[317]] = (diag_count_w$n[[317]] + diag_count_w$n[[318]])
diag_count_w$n[[264]] = (diag_count_w$n[[264]] + diag_count_w$n[[265]])
diag_count_w$n[[211]] = (diag_count_w$n[[211]] + diag_count_w$n[[212]])
diag_count_w$n[[158]] = (diag_count_w$n[[158]] + diag_count_w$n[[159]])
diag_count_w$n[[105]] = (diag_count_w$n[[105]] + diag_count_w$n[[106]])
diag_count_w$n[[52]] = (diag_count_w$n[[52]] + diag_count_w$n[[53]])

diag_count_w = diag_count_w %>% filter(!row_number() %in% c(53,106,159,212,265,318,371,424))
```

# Output
Output file is a cohort file with 564,026 eligible individuals. One further step is later applied after attendance pre-processing to remove individuals who only have attendance dates prior to their cancer index diagnosis date. This brings the final count of eligible individuals to 561,116 individuals. Second output file is a count file for cancer incident diagnoses. 
