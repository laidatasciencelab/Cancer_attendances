# Attendance pre-processing

The objective of this code is to wrangle raw attendance data into analysis-ready format.

R version 3.6.2

## Importing packages
```R
library(tidyverse)
library(data.table)
library(tableone)
library(lubridate)
library(dplyr)
library(sqldf)
```

## Input files
1) CPRD GOLD & Aurum processed cohort files
2) CPRD GOLD & Aurum primary care processed attendance files
3) CPRD GOLD & Aurum secondary care attendance, inpatient critical care files
4) CPRD GOLD & Aurum secondary care attendance, inpatient admitted care files
5) CPRD GOLD & Aurum secondary care attendance, inpatient procedure files
6) CPRD GOLD & Aurum secondary care attendance, inpatient hospitalisation files
7) CPRD GOLD & Aurum secondary care attendance, outpatient care files

## Processing GOLD attendances
``` R
# Loading cancer master file 
cancer_master = readRDS("cancer_master.RDS")

# Loading attendance data files 
# GOLD hes apc events epi
hes_apc_epi_diag = read.csv("hes_diagnosis_epi_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes apc events hosp
hes_apc_hosp_diag = read.csv("hes_diagnosis_hosp_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes apc events primary hosp 
hes_apc_hosp_p_diag = read.csv("hes_primary_diag_hosp_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes a&e events
hes_ae_att = read.csv("hesae_attendance_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes apc events critical care
hes_apc_cc_att = read.csv("hes_ccare_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes apc events augmented care
hes_apc_ac_att = read.csv("hes_acp_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes apc events procedures
hes_apc_prod = read.csv("hes_procedures_epi_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes apc events hospitalisation
hes_apc_hosp_att = read.csv("hes_hospital_22_001756_DM.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# GOLD hes op events attendance
filenames = list.files("hesop_att", pattern="*.txt", full.names=TRUE)
hes_op_att = lapply(filenames, function(i){read.csv(i,header=TRUE, sep="\t",stringsAsFactors = FALSE)})

# HES APC attendances
# HES APC critical care
att_hesapc_cc_gold = hes_apc_cc_att %>% select(patid, admidate, discharged, epistart, epiend, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(admidate))
    %>% mutate(admidate = ymd(discharged))
    %>% mutate(admidate = ymd(epistart))
    %>% mutate(admidate = ymd(epiend))

att_hesapc_cc_gold$admi_dur = difftime(att_hesapc_cc_gold$discharged+1,att_hesapc_cc_gold$admidate,units="days")
att_hesapc_cc_gold$epi_dur = difftime(att_hesapc_cc_gold$epiend+1,att_hesapc_cc_gold$epistart,units="days")

att_hesapc_cc_gold = att_hesapc_cc_gold %>% mutate(ifelse(is.na(admi_dur), epi_dur, admi_dur))
    %>% mutate(ifelse(is.na(admidate), epistart, admidate))
    %>% select(-discharged, -epistart, -epiend, -admi_dur, -epi_dur)
    %>% rename(event_date = admidate)
    %>% drop_na(event_date)

# HES APC augmented care
att_hesapc_ac_gold = hes_apc_ac_att %>% select(patid, epistart, epiend, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(epistart))
    %>% mutate(admidate = ymd(epiend))

att_hesapc_ac_gold$duration = difftime(att_hesapc_ac_gold$epiend+1,att_hesapc_ac_gold$epistart,units="days")

att_hesapc_ac_gold = att_hesapc_ac_gold %>% select(-epiend)
    %>% rename(event_date = epistart)
    %>% drop_na(event_date)

# HES APC procedures
att_hesapc_prod_gold = hes_apc_prod %>% select(patid, admidate, discharged, epistart, epiend, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(admidate))
    %>% mutate(admidate = ymd(discharged))
    %>% mutate(admidate = ymd(epistart))
    %>% mutate(admidate = ymd(epiend))

att_hesapc_prod_gold$admi_dur = difftime(att_hesapc_prod_gold$discharged+1,att_hesapc_prod_gold$admidate,units="days")
att_hesapc_prod_gold$epi_dur = difftime(att_hesapc_prod_gold$epiend+1,att_hesapc_prod_gold$epistart,units="days")

att_hesapc_prod_gold = att_hesapc_prod_gold %>% mutate(ifelse(is.na(admi_dur), epi_dur, admi_dur))
    %>% mutate(ifelse(is.na(admidate), epistart, admidate))
    %>% select(-discharged, -epistart, -epiend, -admi_dur, -epi_dur)
    %>% rename(event_date = admidate)
    %>% drop_na(event_date)

# HES APC hospitalisations
att_hesapc_hosp_gold = hes_apc_hosp_att %>% select(patid, admidate, discharged, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(admidate))
    %>% mutate(admidate = ymd(discharged))

att_hesapc_hosp_gold$duration = difftime(att_hesapc_hosp_gold$discharged+1,att_hesapc_hosp_gold$admidate,units="days")

att_hesapc_hosp_gold = att_hesapc_hosp_gold %>% select(-discharged)
    %>% rename(event_date = admidate)
    %>% drop_na(event_date)

# HES APC episodes
att_hesapc_epidiag_gold = hes_apc_epi_diag %>% select(patid, epistart, epiend, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(epistart))
    %>% mutate(admidate = ymd(epiend))

att_hesapc_epidiag_gold$duration = difftime(att_hesapc_epidiag_gold$epiend+1,att_hesapc_epidiag_gold$epistart,units="days")

att_hesapc_epidiag_gold = att_hesapc_epidiag_gold %>% select(-epiend)
    %>% rename(event_date = epistart)
    %>% drop_na(event_date)

# HES APC hospital (2 datasets)
att_hesapc_hosppdiag_gold = hes_apc_hosp_p_diag %>% select(patid, admidate, discharged, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(admidate))
    %>% mutate(admidate = ymd(discharged))

att_hesapc_hosppdiag_gold$duration = difftime(att_hesapc_hosppdiag_gold$discharged+1,att_hesapc_hosppdiag_gold$admidate,units="days")

att_hesapc_hosppdiag_gold = att_hesapc_hosppdiag_gold %>% select(-discharged)
    %>% rename(event_date = admidate)
    %>% drop_na(event_date)

att_hesapc_hospnprdiag_gold = hes_apc_hosp_diag %>% select(patid, admidate, discharged, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(admidate))
    %>% mutate(admidate = ymd(discharged))

att_hesapc_hospnprdiag_gold$duration = difftime(att_hesapc_hospnprdiag_gold$discharged+1,att_hesapc_hospnprdiag_gold$admidate,units="days")

att_hesapc_hospnprdiag_gold = att_hesapc_hospnprdiag_gold %>% select(-discharged)
    %>% rename(event_date = admidate)
    %>% drop_na(event_date)

# Combining HES APC attendances
att_hesapc_all_gold = rbind(att_hesapc_ac_gold, att_hesapc_cc_gold, att_hesapc_epidiag_gold, att_hesapc_hosp_gold, att_hesapc_hospnprdiag_gold, att_hesapc_hosppdiag_gold, att_hesapc_prod_gold)

# Only keep unique SPNOs to avoid repeats, longest duration supercedes
DTmm = data.table(att_hesapc_all_gold)
att_hesapc_all_gold_spno = unique(DTmm[order(patid,duration)], by=c("patid","spno"), fromLast=TRUE)
att_hesapc_all_gold_spno = arrange(att_hesapc_all_gold_spno, spno)

att_hesapc_all_gold_spno = att_hesapc_all_gold_spno %>% select(-spno)

# GP attendances
colnames(CA_gp_gold) = colnames(MM_gp_gold)
att_gp_gold = rbind(CA_gp_gold, MM_gp_gold)
att_gp_gold = att_gp_gold %>% filter(patid %in% cancer_master$patid)
    %>% select(-condition_diagnosis)
    %>% rename(event_date = condition_index_date)
    %>% mutate(duration = 1)
    %>% drop_na(event_date)

# HES OP attendances
hes_op_att_minimised = lapply(
  hes_op_att,
  function(df){
    df = df %>% select(patid, ethnos, apptdate)
        %>% mutate(patid = as.character(patid))
        %>% filter(patid %in% cancer_master$patid)
        %>% mutate(apptdate = ymd(apptdate))
        %>% drop_na(apptdate)
    return(df)
  }
)

att_hesop_gold = bind_rows(hes_op_att_minimised)
att_hesop_gold = att_hesop_gold %>% select(c(1,3))
    %>% rename(event_date = 2)
    %>% mutate(duration = 1)

att_hesop_all_gold = rbind(CA_hesop_gold, MM_hesop_gold)
att_hesop_all_gold = att_hesop_all_gold %>% select(-ICD3, -ICD4)
    %>% rename(event_date = 2)
    %>% mutate(duration = 1)

att_hesop_all_gold = rbind(att_hesop_all_gold, att_hesop_gold)
```
## Processing Aurum attendances
```R
# All files previously loaded

# HES APC attendances
# HES APC critical care
att_hesapc_cc_aurum = hes_apc_cc_att %>% select(patid, admidate, discharged, epistart, epiend, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(admidate))
    %>% mutate(admidate = ymd(discharged))
    %>% mutate(admidate = ymd(epistart))
    %>% mutate(admidate = ymd(epiend))

att_hesapc_cc_aurum$admi_dur = difftime(att_hesapc_cc_aurum$discharged+1,att_hesapc_cc_aurum$admidate,units="days")
att_hesapc_cc_aurum$epi_dur = difftime(att_hesapc_cc_aurum$epiend+1,att_hesapc_cc_aurum$epistart,units="days")

att_hesapc_cc_aurum = att_hesapc_cc_aurum %>% mutate(ifelse(is.na(admi_dur), epi_dur, admi_dur))
    %>% mutate(ifelse(is.na(admidate), epistart, admidate))
    %>% select(-discharged, -epistart, -epiend, -admi_dur, -epi_dur)
    %>% rename(event_date = admidate)
    %>% drop_na(event_date)

# HES APC augmented care
att_hesapc_ac_aurum = hes_apc_ac_att %>% select(patid, epistart, epiend, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(epistart))
    %>% mutate(admidate = ymd(epiend))

att_hesapc_ac_aurum$duration = difftime(att_hesapc_ac_aurum$epiend+1,att_hesapc_ac_aurum$epistart,units="days")

att_hesapc_ac_aurum = att_hesapc_ac_aurum %>% select(-epiend)
    %>% rename(event_date = epistart)
    %>% drop_na(event_date)

# HES APC procedures
att_hesapc_prod_aurum = hes_apc_prod %>% select(patid, admidate, discharged, epistart, epiend, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(admidate))
    %>% mutate(admidate = ymd(discharged))
    %>% mutate(admidate = ymd(epistart))
    %>% mutate(admidate = ymd(epiend))

att_hesapc_prod_aurum$admi_dur = difftime(att_hesapc_prod_aurum$discharged+1,att_hesapc_prod_aurum$admidate,units="days")
att_hesapc_prod_aurum$epi_dur = difftime(att_hesapc_prod_aurum$epiend+1,att_hesapc_prod_aurum$epistart,units="days")

att_hesapc_prod_aurum = att_hesapc_prod_aurum %>% mutate(ifelse(is.na(admi_dur), epi_dur, admi_dur))
    %>% mutate(ifelse(is.na(admidate), epistart, admidate))
    %>% select(-discharged, -epistart, -epiend, -admi_dur, -epi_dur)
    %>% rename(event_date = admidate)
    %>% drop_na(event_date)

# HES APC hospitalisations
att_hesapc_hosp_aurum = hes_apc_hosp_att %>% select(patid, admidate, discharged, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(admidate))
    %>% mutate(admidate = ymd(discharged))

att_hesapc_hosp_aurum$duration = difftime(att_hesapc_hosp_aurum$discharged+1,att_hesapc_hosp_aurum$admidate,units="days")

att_hesapc_hosp_aurum = att_hesapc_hosp_aurum %>% select(-discharged)
    %>% rename(event_date = admidate)
    %>% drop_na(event_date)

# HES APC episodes
att_hesapc_epidiag_aurum = hes_apc_epi_diag %>% select(patid, epistart, epiend, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(epistart))
    %>% mutate(admidate = ymd(epiend))

att_hesapc_epidiag_aurum$duration = difftime(att_hesapc_epidiag_aurum$epiend+1,att_hesapc_epidiag_aurum$epistart,units="days")

att_hesapc_epidiag_aurum = att_hesapc_epidiag_aurum %>% select(-epiend)
    %>% rename(event_date = epistart)
    %>% drop_na(event_date)

# HES APC hospital (2 datasets)
att_hesapc_hosppdiag_aurum = hes_apc_hosp_p_diag %>% select(patid, admidate, discharged, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(admidate))
    %>% mutate(admidate = ymd(discharged))

att_hesapc_hosppdiag_aurum$duration = difftime(att_hesapc_hosppdiag_aurum$discharged+1,att_hesapc_hosppdiag_aurum$admidate,units="days")

att_hesapc_hosppdiag_aurum = att_hesapc_hosppdiag_aurum %>% select(-discharged)
    %>% rename(event_date = admidate)
    %>% drop_na(event_date)

att_hesapc_hospnprdiag_aurum = hes_apc_hosp_diag %>% select(patid, admidate, discharged, spno)
    %>% mutate(patid = as.character(patid))
    %>% mutate(spno = as.character(spno))
    %>% filter(patid %in% cancer_master$patid)
    %>% mutate(admidate = ymd(admidate))
    %>% mutate(admidate = ymd(discharged))

att_hesapc_hospnprdiag_aurum$duration = difftime(att_hesapc_hospnprdiag_aurum$discharged+1,att_hesapc_hospnprdiag_aurum$admidate,units="days")

att_hesapc_hospnprdiag_aurum = att_hesapc_hospnprdiag_aurum %>% select(-discharged)
    %>% rename(event_date = admidate)
    %>% drop_na(event_date)

# Combining HES APC attendances
att_hesapc_all_aurum = rbind(att_hesapc_ac_aurum, att_hesapc_cc_aurum, att_hesapc_epidiag_aurum, att_hesapc_hosp_aurum, att_hesapc_hospnprdiag_aurum, att_hesapc_hosppdiag_aurum, att_hesapc_prod_aurum)

# Only keep unique SPNOs to avoid repeats, longest duration supercedes
DTmm = data.table(att_hesapc_all_aurum)
att_hesapc_all_aurum_spno = unique(DTmm[order(patid,duration)], by=c("patid","spno"), fromLast=TRUE)
att_hesapc_all_aurum_spno = arrange(att_hesapc_all_aurum_spno, spno)

att_hesapc_all_aurum_spno = att_hesapc_all_aurum_spno %>% select(-spno)

# GP attendances
colnames(CA_gp_aurum) = colnames(MM_gp_aurum)
att_gp_aurum = rbind(CA_gp_aurum, MM_gp_aurum)
att_gp_aurum = att_gp_aurum %>% filter(patid %in% cancer_master$patid)
    %>% select(-condition_diagnosis)
    %>% rename(event_date = condition_index_date)
    %>% mutate(duration = 1)
    %>% drop_na(event_date)

# HES OP attendances
hes_op_att_minimised = lapply(
  hes_op_att,
  function(df){
    df = df %>% select(patid, ethnos, apptdate)
        %>% mutate(patid = as.character(patid))
        %>% filter(patid %in% cancer_master$patid)
        %>% mutate(apptdate = ymd(apptdate))
        %>% drop_na(apptdate)
    return(df)
  }
)

att_hesop_aurum = bind_rows(hes_op_att_minimised)
att_hesop_aurum = att_hesop_aurum %>% select(c(1,3))
    %>% rename(event_date = 2)
    %>% mutate(duration = 1)

att_hesop_all_aurum = rbind(CA_hesop_aurum, MM_hesop_aurum)
att_hesop_all_aurum = att_hesop_all_aurum %>% select(-ICD3, -ICD4)
    %>% rename(event_date = 2)
    %>% mutate(duration = 1)

att_hesop_all_aurum = rbind(att_hesop_all_aurum, att_hesop_aurum)
```
## Combining gold and aurum datasets and quality checks
```R
# Setting cancer date filter
cancer_index_date = cancer_master %>% select(c(1,13))

# GP attendance 
att_gp_all = rbind(att_gp_gold,att_gp_aurum)
att_gp_all = att_gp_all %>% filter(event_date > "1920-01-01", event_date < "2021-06-17")
att_gp_all = merge(att_gp_all, cancer_index_date, on="patid", all.x=TRUE)
att_gp_all = att_gp_all %>% filter(event_date > cancer_index_date)
    %>% select(-cancer_index_date)

# HES OP attendance
att_hesop_all = rbind(att_hesop_all_gold,att_hesop_all_aurum)
att_hesop_all = att_hesop_all %>% filter(event_date > "1920-01-01", event_date < "2021-06-17")
att_hesop_all = merge(att_hesop_all, cancer_index_date, on="patid", all.x=TRUE)
att_hesop_all = att_hesop_all %>% filter(event_date > cancer_index_date)
    %>% select(-cancer_index_date)

# HES APC attendance
att_hesapc_all = rbind(att_hesapc_all_gold_spno,att_hesapc_all_aurum_spno)
att_hesapc_all = att_hesapc_all %>% filter(event_date > "1920-01-01", event_date < "2021-06-17")
att_hesapc_all = merge(att_hesapc_all, cancer_index_date, on="patid", all.x=TRUE)
att_hesapc_all = att_hesapc_all %>% filter(event_date > cancer_index_date)
    %>% select(-cancer_index_date)
```
## QC-ing step for cohort to remove those with no attendances after index date
```R
# Combining all attendances
attendance_master = rbind(att_gp_all, att_hesapc_all, att_hesop_all)
patid_unique = attendance_master %>% distinct(patid)
cancer_master = cancer_master %>% filter(patid %in% patid_unique$patid)
```

## Output
Output file is 1) processed GP attendance file, 2) processed HES OP attendance file, 3) processed HES APC attendance file, 4) finalised eligible cohort master file 5) attendance master file