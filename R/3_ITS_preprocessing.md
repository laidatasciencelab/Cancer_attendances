# Interrupted time-series pre-processing

The objective of this code is to wrangle processed attendance date into weekly counts for the ITS analysis

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
1) Cancer master file
2) Attendance master file
3) Processed GP attendance file
4) Processed HES APC attendance file
5) Processed HES OP attendance file

## Cohort baseline summary
```R
# Load attendance master file
attendance_master_noae = readRDS("attendance_master_noae.RDS")

# Load cancer master file 
cancer_master = readRDS("cancer_master.RDS")

# Formatting for count generation 
attendance_master_noae = attendance_master_noae %>% filter(event_date > "2017-12-31")
    %>% mutate(month_year = format(event_date, "%b-%Y"))
    %>% mutate(year = format(event_date, "%Y"))
    %>% mutate(week = week(ymd(event_date)))

# Cohort filtering 
ITS_distinct = attendance_master_noae %>% distinct(patid) 
baseline_master = cancer_master %>% filter(patid %in% ITS_distinct$patid)
    %>% select(c(1,3,4,10,11,12,14,15,19))

baseline_master$agegroup = cut(baseline_master$age, breaks = c("-1" ,"18", "30", "40", "50", "60", "70", "80", "200", include.lowest = TRUE))
baseline_master$conditiongroup = cut(baseline_master$condition_count, breaks = c("-1","0","1","5","10","20","200", include.lowest = TRUE))

myvars = c("region", "gender","ethnicity", "IMD_5", "cancer_index_diagnosis", "age", "condition_count", "agegroup", "conditiongroup", "diag_after_1")
catvars = c("region", "gender","ethnicity", "IMD_5", "cancer_index_diagnosis", "agegroup", "conditiongroup", "diag_after_1")

baseline_table = CreateTableOne(vars = myvars, data = baseline_master, factorVars = catvars)
summary(baseline_table)
baseline_output = print(baseline_table)
```

## Generating weekly attendance counts for seasonality analysis (2010-2021)(GP)
```R
# Loading processed GP attendance file
att_gp_all = readRDS(att_gp_all.RDS)

# Formatting for count generation 
att_gp_all = att_gp_all %>% filter(event_date > "2009-12-31")
    %>% mutate(month_year = format(event_date, "%b-%Y"))
    %>% mutate(year = format(event_date, "%Y"))
    %>% mutate(week = week(ymd(event_date)))

# GP seasonality count 
att_gp_all_count_w = att_gp_all %>% dplyr::count(year, week)
    %>% rename(count = n)

# Finding out which weeks are missing for count continuity
att_dates = data.frame(dates = seq(min(att_gp_all$event_date), max(att_gp_all$event_date), by="days"))

missing_ad = as.data.frame(att_dates[!att_dates$dates %in% attendance_master_noae$event_date, ])
missing_ad = missing_ad %>% mutate(patid = "NA")
    %>% select(c(2,1))
    %>% mutate(event_date = 2)
missing_ad$month_year = format(missing_ad$event_date, "%b-%Y")
missing_ad$year = format(missing_ad$event_date, "%Y")
missing_ad = missing_ad %>% mutate(week = week(ymd(event_date))) 
    %>% filter(event_date < "2021-06-18")
missing_ad_w = missing_ad[!duplicated(missing_ad[c(4,5)]), ]

# Identify weeks with 0 count 
att_count_0 = missing_ad_w[, c(4,5)]
att_count_0$concat = paste(att_count_0$year, att_count_0$week)
att_gp_all_count_w$concat = paste(att_gp_all_count_w$year, att_gp_all_count_w$week)

missing_ad_w = att_count_0 %>% filter(!concat %in% att_gp_all_count_w$concat)
    %>% select(-concat)
    %>% mutate(n = 0)
    %>% mutate(n = as.integer(n))
    %>% rename(count = n)

# Adding 0 counts into attendance counts per week 
att_gp_all_count_w$concat = NULL
att_gp_all_count_w = rbind(att_gp_all_count_w, missing_ad_w)
att_gp_all_count_w = arrange(att_gp_all_count_w, year, week)

# Formatting week 53 as part of week 52
att_gp_all_count_w$n[[582]] = (att_gp_all_count_w$n[[582]] + att_gp_all_count_w$n[[583]])
att_gp_all_count_w$n[[529]] = (att_gp_all_count_w$n[[529]] + att_gp_all_count_w$n[[530]])
att_gp_all_count_w$n[[476]] = (att_gp_all_count_w$n[[476]] + att_gp_all_count_w$n[[477]])
att_gp_all_count_w$n[[423]] = (att_gp_all_count_w$n[[423]] + att_gp_all_count_w$n[[424]])
att_gp_all_count_w$n[[370]] = (att_gp_all_count_w$n[[370]] + att_gp_all_count_w$n[[371]])
att_gp_all_count_w$n[[317]] = (att_gp_all_count_w$n[[317]] + att_gp_all_count_w$n[[318]])
att_gp_all_count_w$n[[264]] = (att_gp_all_count_w$n[[264]] + att_gp_all_count_w$n[[265]])
att_gp_all_count_w$n[[211]] = (att_gp_all_count_w$n[[211]] + att_gp_all_count_w$n[[212]])
att_gp_all_count_w$n[[158]] = (att_gp_all_count_w$n[[158]] + att_gp_all_count_w$n[[159]])
att_gp_all_count_w$n[[105]] = (att_gp_all_count_w$n[[105]] + att_gp_all_count_w$n[[106]])
att_gp_all_count_w$n[[52]] = (att_gp_all_count_w$n[[52]] + att_gp_all_count_w$n[[53]])

att_gp_all_count_w = att_gp_all_count_w %>% filter(!row_number() %in% c(53,106,159,212,265,318,371,424,477,530,583))

# Time from base variable
att_gp_all_count_w$time_from_base = 1:nrow(att_gp_all_count_w)
```

## Generating weekly attendance counts for seasonality analysis (2010-2021)(HES OP)

```R
# Loading processed HES OP attendance file
att_hesop_all = readRDS(att_hesop_all.RDS)

# Formatting for count generation 
att_hesop_all = att_hesop_all %>% filter(event_date > "2009-12-31")
    %>% mutate(month_year = format(event_date, "%b-%Y"))
    %>% mutate(year = format(event_date, "%Y"))
    %>% mutate(week = week(ymd(event_date)))

# HES OP seasonality count 
att_hesop_all_count_w = att_hesop_all %>% dplyr::count(year, week)
    %>% rename(count = n)

# Finding out which weeks are missing for count continuity
att_dates = data.frame(dates = seq(min(att_hesop_all$event_date), max(att_hesop_all$event_date), by="days")) 
# All weeks accounted for 

# Formatting week 53 as part of week 52
att_hesop_all_count_w$n[[529]] = (att_hesop_all_count_w$n[[529]] + att_hesop_all_count_w$n[[530]])
att_hesop_all_count_w$n[[476]] = (att_hesop_all_count_w$n[[476]] + att_hesop_all_count_w$n[[477]])
att_hesop_all_count_w$n[[423]] = (att_hesop_all_count_w$n[[423]] + att_hesop_all_count_w$n[[424]])
att_hesop_all_count_w$n[[370]] = (att_hesop_all_count_w$n[[370]] + att_hesop_all_count_w$n[[371]])
att_hesop_all_count_w$n[[317]] = (att_hesop_all_count_w$n[[317]] + att_hesop_all_count_w$n[[318]])
att_hesop_all_count_w$n[[264]] = (att_hesop_all_count_w$n[[264]] + att_hesop_all_count_w$n[[265]])
att_hesop_all_count_w$n[[211]] = (att_hesop_all_count_w$n[[211]] + att_hesop_all_count_w$n[[212]])
att_hesop_all_count_w$n[[158]] = (att_hesop_all_count_w$n[[158]] + att_hesop_all_count_w$n[[159]])
att_hesop_all_count_w$n[[105]] = (att_hesop_all_count_w$n[[105]] + att_hesop_all_count_w$n[[106]])
att_hesop_all_count_w$n[[52]] = (att_hesop_all_count_w$n[[52]] + att_hesop_all_count_w$n[[53]])

att_hesop_all_count_w = att_hesop_all_count_w %>% filter(!row_number() %in% c(53,106,159,212,265,318,371,424,477,530))

# Time from base variable
att_hesop_all_count_w$time_from_base = 1:nrow(att_hesop_all_count_w)
```

## Generating weekly attendance counts for seasonality analysis (2010-2021)(HES APC)

```R
# Loading processed HES APC attendance file
att_hesapc_all = readRDS(att_hesapc_all.rds)

# Formatting for count generation 
att_hesapc_all = att_hesapc_all %>% filter(event_date > "2009-12-31")
    %>% mutate(month_year = format(event_date, "%b-%Y"))
    %>% mutate(year = format(event_date, "%Y"))
    %>% mutate(week = week(ymd(event_date)))

# HES APC seasonality count 
att_hesapc_all_count_w = att_hesapc_all %>% dplyr::count(year, week)
    %>% rename(count = n)

# Finding out which weeks are missing for count continuity
att_dates = data.frame(dates = seq(min(att_hesapc_all$event_date), max(att_hesapc_all$event_date), by="days"))
# All weeks accounted for 

# Formatting week 53 as part of week 52
att_hesapc_all_count_w$n[[582]] = (att_hesapc_all_count_w$n[[582]] + att_hesapc_all_count_w$n[[583]])
att_hesapc_all_count_w$n[[529]] = (att_hesapc_all_count_w$n[[529]] + att_hesapc_all_count_w$n[[530]])
att_hesapc_all_count_w$n[[476]] = (att_hesapc_all_count_w$n[[476]] + att_hesapc_all_count_w$n[[477]])
att_hesapc_all_count_w$n[[423]] = (att_hesapc_all_count_w$n[[423]] + att_hesapc_all_count_w$n[[424]])
att_hesapc_all_count_w$n[[370]] = (att_hesapc_all_count_w$n[[370]] + att_hesapc_all_count_w$n[[371]])
att_hesapc_all_count_w$n[[317]] = (att_hesapc_all_count_w$n[[317]] + att_hesapc_all_count_w$n[[318]])
att_hesapc_all_count_w$n[[264]] = (att_hesapc_all_count_w$n[[264]] + att_hesapc_all_count_w$n[[265]])
att_hesapc_all_count_w$n[[211]] = (att_hesapc_all_count_w$n[[211]] + att_hesapc_all_count_w$n[[212]])
att_hesapc_all_count_w$n[[158]] = (att_hesapc_all_count_w$n[[158]] + att_hesapc_all_count_w$n[[159]])
att_hesapc_all_count_w$n[[105]] = (att_hesapc_all_count_w$n[[105]] + att_hesapc_all_count_w$n[[106]])
att_hesapc_all_count_w$n[[52]] = (att_hesapc_all_count_w$n[[52]] + att_hesapc_all_count_w$n[[53]])

att_hesapc_all_count_w = att_hesapc_all_count_w %>% filter(!row_number() %in% c(53,106,159,212,265,318,371,424,477,530,583))

# Time from base variable
att_hesapc_all_count_w$time_from_base = 1:nrow(att_hesapc_all_count_w)
```
## Generating weekly attendance counts for ITS analysis (2018-2021)

```R
# Generating attendance count per week
attendance_count_w = attendance_master_noae %>% dplyr::count(year, week)
    %>% rename(count = n)
    %>% filter(event_date > "2017-12-31")

# Finding out which weeks are missing for count continuity
att_dates = data.frame(dates = seq(min(attendance_master_noae$event_date), max(attendance_master_noae$event_date), by="days"))

missing_ad = as.data.frame(att_dates[!att_dates$dates %in% attendance_master_noae$event_date, ])
missing_ad = missing_ad %>% mutate(patid = "NA")
    %>% select(c(2,1))
    %>% mutate(event_date = 2)
missing_ad$month_year = format(missing_ad$event_date, "%b-%Y")
missing_ad$year = format(missing_ad$event_date, "%Y")
missing_ad = missing_ad %>% mutate(week = week(ymd(event_date))) 
    %>% filter(event_date < "2021-06-18")
missing_ad_w = missing_ad[!duplicated(missing_ad[c(4,5)]), ]

# Identify weeks with 0 count 
att_count_0 = missing_ad_w[, c(4,5)]
att_count_0$concat = paste(att_count_0$year, att_count_0$week)
attendance_count_w$concat = paste(attendance_count_w$year, attendance_count_w$week)

missing_ad_w = att_count_0 %>% filter(!concat %in% attendance_count_w$concat)
    %>% select(-concat)
    %>% mutate(n = 0)
    %>% mutate(n = as.integer(n))
    %>% rename(count = n)

# Adding 0 counts into attendance counts per week 
attendance_count_w$concat = NULL
attendance_count_w = rbind(attendance_count_w, missing_ad_w)
attendance_count_w = arrange(attendance_count_w, year, week)

# Formatting week 53 as part of week 52
attendance_count_w$n[[158]] = (attendance_count_w$n[[158]] + attendance_count_w$n[[159]])
attendance_count_w$n[[105]] = (attendance_count_w$n[[105]] + attendance_count_w$n[[106]])
attendance_count_w$n[[52]] = (attendance_count_w$n[[52]] + attendance_count_w$n[[53]])
attendance_count_w = attendance_count_w %>% filter(!row_number() %in% c(53,106,159))

# Time from base variable
attendance_count_w$time_from_base = 1:nrow(attendance_count_w)
```

## Output

Output file is 1) Cohort ITS baseline table, 2) ITS weekly attendance counts, 3) GP seasonality weekly attendance counts, 4) HES OP seasonality weekly attendance counts, 5) HES APC seasonality weekly attendance counts