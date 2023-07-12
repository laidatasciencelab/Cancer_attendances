# Interrupted time-series analysis

The objective of this code is to wrangle processed attendance dates into stratified cohorts

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
1) Cancer master file
2) Processed HES APC attendance file
3) Processed HES OP attendance file

## Assigning stratifications
```R
# Load cancer master file 
cancer_master = readRDS(cancer_master.RDS)

# Load attendance files
att_hesop_all = readRDS("att_hesop_all.rds")
att_hesapc_all = readRDS("att_hesapc_all.rds")

# Generate attendance file (HES APC, OP)
att_stratification = rbind(att_hesapc_all, att_hesop_all)
att_stratification_2018 = att_stratification %>% filter(patid %in% cancer_master$patid)
    %>% filter(year >= 2018)
    %>% select(c(1:3,7))

# Assigning patient exposure variables to each attendance 
strat_variables = cancer_master %>% select(c(1,3,10:15))
att_stratification_2018 = merge(x=att_stratification, y=strat_variables, by="patid", all.x=TRUE)
att_stratification_2018 = att_stratification %>% relocate(tag, .after=condition_count)

# Assigning time exposure variable to each attendance
att_stratification_2018 = att_stratification_2018 %>% mutate(study_period = ifelse(att_stratification_2018$event_date < "2020-03-23", "PP", 
    ifelse(att_stratification_2018$event_date > "2020-03-22" & att_stratification_2018$event_date < "2020-06-22", "LD1", 
    ifelse(att_stratification_2018$event_date > "2020-06-21" & att_stratification_2018$event_date < "2020-09-21", "MR", 
    ifelse(att_stratification_2018$event_date > "2020-09-20" & att_stratification_2018$event_date < "2021-01-04", "LD2", 
    ifelse(att_stratification_2018$event_date > "2021-01-03" & att_stratification_2018$event_date < "2021-03-22", "LD3", "LL"))))))
```

## Stratified cohort baseline summary (by time exposure)

```R
# Spltting attendances into time-based exposure groups 
split(att_stratification_2018, study_period)
# Each study period is now a single dataframe 

study_period_cohorts = list(PP, LD1, MR, LD2, LD3, LL)

baseline_groups = lapply(
    study_period_cohorts,
    function(df){
        baseline_master = cancer_master %>% filter(patid %in% df$patid)
            %>% select(c(1,3,4,10,11,12,14,15,19))

        baseline_master$agegroup = cut(baseline_master$age, breaks = c("-1" ,"18", "30", "40", "50", "60", "70", "80", "200", include.lowest = TRUE))
        baseline_master$conditiongroup = cut(baseline_master$condition_count, breaks = c("-1","0","1","5","10","20","200", include.lowest = TRUE))

        myvars = c("region", "gender","ethnicity", "IMD_5", "cancer_index_diagnosis", "age", "condition_count", "agegroup", "conditiongroup", "diag_after_1")
        catvars = c("region", "gender","ethnicity", "IMD_5", "cancer_index_diagnosis", "agegroup", "conditiongroup", "diag_after_1")

        baseline_table = CreateTableOne(vars = myvars, data = baseline_master, factorVars = catvars)
        summary(baseline_table)
        baseline_output = print(baseline_table)
    return(baseline_table)
    }
)
```

## Rate and volume generation 
```R
# The following steps are repeated for each time-based exposure cohort (PP, LD1, MR, LD2, LD3, LL)

# # Spltting attendances into patient-based exposure groups
# IMD
PP = PP %>% mutate(IMD_5 = ifelse(is.na(IMD_5), "IMDunknown", IMD_5)) 
split(PP, IMD_5)
strat_list1 = list(IMD1, IMD2, IMD3, IMD4, IMD5, IMDunknown)

# Ethnicity
split(PP, ethnicity)
strat_list2 = list(asian, black, mixed, other, unknown, white)

# Age
PP$agegroup = cut(PP$age, breaks = c("-1" ,"18", "30", "40", "50", "60", "70", "80", "200", include.lowest = TRUE))
split(PP, agegroup)
strat_list3 = list(ageu18, age18_29, age30_39, age40_49, age50_59, age60_69, age70_79, ageo80)

# Condition count
PP$conditiongroup = cut(PP$condition_count, breaks = c("-1","0","1","5","10","20","200", include.lowest = TRUE))
split(PP, conditiongroup)
strat_list4 = list(comor0, comor1, comor2_5, comor6_10, comor11_20, comor21)

# Region
split(PP, region)
strat_list5 = list(reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10)

# Cancer diagnosis
split(PP, cancer_index_diagnosis)
strat_list6 = list(Primary_Malignancy_biliary_tract,
                    Primary_Malignancy_Bladder,
                    Primary_Malignancy_Bone_and_articular_cartila,
                    Primary_Malignancy_Brain_Other_CNS_and_Intrac,
                    Primary_Malignancy_Breast,
                    Primary_Malignancy_Cervical,
                    Primary_Malignancy_colorectal_and_anus,
                    Primary_Malignancy_Kidney_and_Ureter,
                    Primary_Malignancy_Liver,
                    Primary_Malignancy_Lung_and_trachea,
                    Primary_Malignancy_Malignant_Melanoma,
                    Primary_Malignancy_Mesothelioma,
                    Primary_Malignancy_Multiple_independent_sites,
                    Primary_Malignancy_Oesophageal,
                    Primary_Malignancy_Oropharyngeal,
                    Primary_Malignancy_Other_Organs,
                    Primary_Malignancy_Other_Skin_and_subcutaneou,
                    Primary_Malignancy_Ovarian,
                    Primary_Malignancy_Pancreatic,
                    Primary_Malignancy_Prostate,
                    Primary_Malignancy_Stomach,
                    Primary_Malignancy_Testicular,
                    Primary_Malignancy_Thyroid,
                    Primary_Malignancy_Uterine,
                    Secondary_Malignancy_Adrenal_gland,
                    Secondary_Malignancy_Bone,
                    Secondary_Malignancy_Bowel,
                    Secondary_Malignancy_Brain_Other_CNS_and_Intr,
                    Secondary_malignancy_Liver_and_intrahepatic_b,
                    Secondary_Malignancy_Lung,
                    Secondary_Malignancy_Lymph_Nodes,
                    Secondary_Malignancy_Other_organs,
                    Secondary_Malignancy_Pleura,
                    Secondary_Malignancy_retroperitoneum_and_peri,
                    Hodgkin_Lymphoma,
                    NonHodgkin_Lymphoma,
                    Leukaemia,
                    Multiple_myeloma_and_malignant_plasma_cell_ne)

# Diagnosis time
split(PP, diag_after_1)
strat_list7 = list(diag_new, diag_old)

# Create 2018 cancer master 
cancer_master_2018 = cancer_master %>% filter((dod > "2017-12-31") | is.na(dod))
    %>% filter(patid %in% att_stratification_2018$patid) 

# Create merge file to determine start points and end points   
earliest_date = cancer_master_2018 %>% select(c(8))
    %>% mutate(study_end = ymd("2021-03-31"))
earliest_date$earliest_date = apply(earliest_date, 1, min, na.rm=TRUE)

py_merge = cancer_master_2018 %>% select(c(1))
    %>% mutate(earliest_date = ymd(earliest_date$earliest_date))
    %>% mutate(cancer_diag_date = ymd(cancer_master_2018$cancer_index_date))

# Function to calculate person-years for each stratification level for PP
# Repeat for each "strat_listX"
person_years = lapply(
  strat_list1,
  function(df){
    model = df
    # End period needs to be 1 day greater than the end date to account for the end date
    cancer_index_check_period_end = ymd("2020-03-23")
    model = model %>% filter(cancer_index_date < cancer_index_check_period_end)
    print(nrow(model))
    model = model %>% select(c(1))
        %>% distinct()
    model = merge(x=model, y=py_merge, by="patid", all.x=TRUE)
    # Determine true start
    qy_merge = model %>% select(c(3))
        %>% mutate(period_start = ymd("2018-01-01"))
    qy_merge$start_date = apply(qy_merge, 1, max, na.rm=TRUE)
    model$period_start = qy_merge$start_date
    # Determine true end
    xy_merge = model %>% select(c(2))
        %>% mutate(period_end = ymd("2020-03-23"))
    xy_merge$end_date = apply(xy_merge, 1, min, na.rm=TRUE)
    model$period_end = xy_merge$end_date
    # Person-years calculation    
    model$person_years = interval(start = model$period_start, end = model$period_end)/duration(n=1, unit="years")
    sum = as.character(round(sum(model$person_years),2))
    return(sum)
  }
)

# Function to calculate person-years for each stratification level for LD1
# Repeat for each "strat_listX"
person_years = lapply(
  strat_list1,
  function(df){
    model = df
    # End period needs to be 1 day greater than the end date to account for the end date
    cancer_index_check_period_end = ymd("2020-06-22")
    model = model %>% filter(cancer_index_date < cancer_index_check_period_end)
    print(nrow(model))
    model = model %>% select(c(1))
        %>% distinct()
    model = merge(x=model, y=py_merge, by="patid", all.x=TRUE)
    # Determine true start
    qy_merge = model %>% select(c(3))
        %>% mutate(period_start = ymd("2020-03-23"))
    qy_merge$start_date = apply(qy_merge, 1, max, na.rm=TRUE)
    model$period_start = qy_merge$start_date
    # Determine true end
    xy_merge = model %>% select(c(2))
        %>% mutate(period_end = ymd("2020-06-22"))
    xy_merge$end_date = apply(xy_merge, 1, min, na.rm=TRUE)
    model$period_end = xy_merge$end_date
    # Person-years calculation    
    model$person_years = interval(start = model$period_start, end = model$period_end)/duration(n=1, unit="years")
    sum = as.character(round(sum(model$person_years),2))
    return(sum)
  }
)

# Function to calculate person-years for each stratification level for MR
# Repeat for each "strat_listX"
person_years = lapply(
  strat_list1,
  function(df){
    model = df
    # End period needs to be 1 day greater than the end date to account for the end date
    cancer_index_check_period_end = ymd("2020-09-21")
    model = model %>% filter(cancer_index_date < cancer_index_check_period_end)
    print(nrow(model))
    model = model %>% select(c(1))
        %>% distinct()
    model = merge(x=model, y=py_merge, by="patid", all.x=TRUE)
    # Determine true start
    qy_merge = model %>% select(c(3))
        %>% mutate(period_start = ymd("2020-06-22"))
    qy_merge$start_date = apply(qy_merge, 1, max, na.rm=TRUE)
    model$period_start = qy_merge$start_date
    # Determine true end
    xy_merge = model %>% select(c(2))
        %>% mutate(period_end = ymd("2020-09-21"))
    xy_merge$end_date = apply(xy_merge, 1, min, na.rm=TRUE)
    model$period_end = xy_merge$end_date
    # Person-years calculation    
    model$person_years = interval(start = model$period_start, end = model$period_end)/duration(n=1, unit="years")
    sum = as.character(round(sum(model$person_years),2))
    return(sum)
  }
)

# Function to calculate person-years for each stratification level for LD2
# Repeat for each "strat_listX"

# As OP data ends earlier, adjustments made to ensure period end = OP data end for patients who only have OP data
LD2_OP_patids = att_stratification_2018 %>% filter(tag == "OP") 
    %>% select(c(1)) 
    %>% distinct()
LD2_APC_patids = att_stratification_2018 %>% filter(tag == "APC") 
    %>% select(c(1)) 
    %>% distinct()

LD2_both_patids = inner_join(LD2_OP_patids, LD2_APC_patids)
LD2_OP_patids = LD2_OP_patids %>% filter(!patid %in% LD2_both_patids$patid)

LD2_earliest_date = cancer_master_2018[c(1,8)]
LD2_APC_patids = merge(x=LD2_APC_patids, y=LD2_earliest_date, by="patid", all.x=TRUE)
LD2_APC_patids$period_end = ymd("2021-01-04")
LD2_OP_patids = merge(x=LD2_OP_patids, y=LD2_earliest_date, by="patid", all.x=TRUE)
LD2_OP_patids$period_end = ymd("2020-11-01")

merge_LD2 = rbind(LD2_APC_patids, LD2_OP_patids)
LD2_end_date = merge_LD2[c(2,3)]
LD2_end_date$end_date = apply(LD2_end_date, 1, min, na.rm=TRUE)
merge_LD2 = merge_LD2 %>%  mutate(end_date = LD2_end_date$end_date)
    %>% select(c(1,4))

person_years = lapply(
  strat_list6,
  function(df){
    model = df
    cancer_index_check_period_end = ymd("2021-01-04")
    model = model %>% filter(cancer_index_date < cancer_index_check_period_end)
    print(nrow(model))
    model = model %>% select(c(1))
        %>% distinct()
    model = merge(x=model, y=merge_LD2, by="patid", all.x=TRUE)
    qy_merge = model[,1, drop = FALSE]
    cancer_master_2018 = cancer_master_2018[c(1,13)]
    qy_merge = merge(qy_merge, cancer_master_2018, by="patid", all.x=TRUE)
    dy_merge = qy_merge[,2, drop = FALSE]
    dy_merge$period_start = ymd("2020-09-21")
    qy_merge$start_date = apply(dy_merge, 1, max, na.rm=TRUE)
    model$period_start = qy_merge$start_date
    model$person_years = interval(start = model$period_start, end = model$end_date)/duration(n=1, unit="years")
    sum = as.character(round(sum(model$person_years),2))
    return(sum)
  }
)

# Function to calculate person-years for each stratification level for LD3
# Repeat for each "strat_listX"
person_years = lapply(
  strat_list1,
  function(df){
    model = df
    # End period needs to be 1 day greater than the end date to account for the end date
    cancer_index_check_period_end = ymd("2021-03-22")
    model = model %>% filter(cancer_index_date < cancer_index_check_period_end)
    print(nrow(model))
    model = model %>% select(c(1))
        %>% distinct()
    model = merge(x=model, y=py_merge, by="patid", all.x=TRUE)
    # Determine true start
    qy_merge = model %>% select(c(3))
        %>% mutate(period_start = ymd("2021-01-04"))
    qy_merge$start_date = apply(qy_merge, 1, max, na.rm=TRUE)
    model$period_start = qy_merge$start_date
    # Determine true end
    xy_merge = model %>% select(c(2))
        %>% mutate(period_end = ymd("2021-03-22"))
    xy_merge$end_date = apply(xy_merge, 1, min, na.rm=TRUE)
    model$period_end = xy_merge$end_date
    # Person-years calculation    
    model$person_years = interval(start = model$period_start, end = model$period_end)/duration(n=1, unit="years")
    sum = as.character(round(sum(model$person_years),2))
    return(sum)
  }
)

# Function to calculate person-years for each stratification level for LL
# Repeat for each "strat_listX"
person_years = lapply(
  strat_list1,
  function(df){
    model = df
    # End period needs to be 1 day greater than the end date to account for the end date
    cancer_index_check_period_end = ymd("2021-04-01")
    model = model %>% filter(cancer_index_date < cancer_index_check_period_end)
    print(nrow(model))
    model = model %>% select(c(1))
        %>% distinct()
    model = merge(x=model, y=py_merge, by="patid", all.x=TRUE)
    # Determine true start
    qy_merge = model %>% select(c(3))
        %>% mutate(period_start = ymd("2021-03-22"))
    qy_merge$start_date = apply(qy_merge, 1, max, na.rm=TRUE)
    model$period_start = qy_merge$start_date
    # Determine true end
    xy_merge = model %>% select(c(2))
        %>% mutate(period_end = ymd("2021-04-01"))
    xy_merge$end_date = apply(xy_merge, 1, min, na.rm=TRUE)
    model$period_end = xy_merge$end_date
    # Person-years calculation    
    model$person_years = interval(start = model$period_start, end = model$period_end)/duration(n=1, unit="years")
    sum = as.character(round(sum(model$person_years),2))
    return(sum)
  }
)
```

## Output
The person-years and attendance counts for each period for each stratification group to allow calculation of rates and rate ratios

