##### MSM off PrEP Analyses --------
# This script is structured to match the AIDS & Behavior manuscript entitled:
# Behavior change among HIV-negative men who have sex with men not using PrEP in the United States
# Goodreau SM, Barry MP, Hamilton DT, Williams AM, Wang L, Sanchez TH, Katz DA, Delaney KP

# prepare workspace:
# clear environment
rm(list = ls())

# call in packages
library(tidyverse)
library(haven)
library(dplyr)
library(tidyr)
library(tidyselect)
library(ggplot2)
library(trend)
library(DescTools)
library(msm)
library(EnvStats)
library(magrittr)
library(exact2x2)

# define functions 
tableNA <- function(x, ...){
   table(x, useNA = "ifany", ...)  
}

# call in datasets
hnm_np_2y <- read.csv("hnm_np_2y.csv")
hnm_np_2y <- hnm_np_2y[!is.na(hnm_np_2y$cai_12_new.1) & !is.na(hnm_np_2y$cai_12_new.2),] # remove people with NA for either CAI-p12 variable.

### Table 2: Participant characteristics

### Table 3 ---------
# Year-on-year comparisons for reports of any condomless anal sex (CAS) in the last 12 months among 
# HIV-negative men who have sex with men (MSM) who have not used pre-exposure prophylaxis (PrEP) in the last 12 months 
# for whom two consecutive years of data were available, American Men’s Internet Survey (AMIS) 2014-2019 (N=2,365)

# Column 1: N & col %
nrow(hnm_np_2y)

# Col 2 & 3: % reporting CAIp12 in Y1 and Y2
round(prop.table(table(hnm_np_2y$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "06_Black_Young",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "06_Black_Young",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "02_Black_Old",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "02_Black_Old",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "07_Hisp_Young",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "07_Hisp_Young",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "03_Hisp_Old",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "03_Hisp_Old",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "05_White_Young",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "05_White_Young",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "01_White_Old",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "01_White_Old",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "08_Other_Young",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "08_Other_Young",]$cai_12_new.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "04_Other_Old",]$cai_12_new.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "04_Other_Old",]$cai_12_new.2))*100,1)

# Col 5 & 6: McNemar Test & p-value
exact2x2((table(hnm_np_2y$cai_12_new.1, hnm_np_2y$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$year.1 == 2014,]$cai_12_new.1, hnm_np_2y[hnm_np_2y$year.1 == 2014,]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y[hnm_np_2y$year.1 == 2015,]$cai_12_new.1, hnm_np_2y[hnm_np_2y$year.1 == 2015,]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y[hnm_np_2y$year.1 == 2016,]$cai_12_new.1, hnm_np_2y[hnm_np_2y$year.1 == 2016,]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y[hnm_np_2y$year.1 == 2017,]$cai_12_new.1, hnm_np_2y[hnm_np_2y$year.1 == 2017,]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y[hnm_np_2y$year.1 == 2018,]$cai_12_new.1, hnm_np_2y[hnm_np_2y$year.1 == 2018,]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_12_new.1, hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_12_new.1, hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "06_Black_Young",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_age_cat.1 == "06_Black_Young",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "02_Black_Old",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_age_cat.1 == "02_Black_Old",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "07_Hisp_Young",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_age_cat.1 == "07_Hisp_Young",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "03_Hisp_Old",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_age_cat.1 == "03_Hisp_Old",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "05_White_Young",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_age_cat.1 == "05_White_Young",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "01_White_Old",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_age_cat.1 == "01_White_Old",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "08_Other_Young",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_age_cat.1 == "08_Other_Young",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "04_Other_Old",]$cai_12_new.1, hnm_np_2y[hnm_np_2y$race_age_cat.1 == "04_Other_Old",]$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

### Table 4 ---------
# Year-on-year comparisons for reports of any condomless anal sex (CAS) in the last 12 months by partner serostatus mong 
# HIV-negative men who have sex with men (MSM) who have not used pre-exposure prophylaxis (PrEP) in the last 12 months 
# for whom two consecutive years of data were available, American Men’s Internet Survey (AMIS) 2014-2019 (N=2,365)

### * Partners LWH -----
# Col 2 & 3: % reporting CAIp12 in Y1 and Y2
round(prop.table(table(hnm_np_2y$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "06_Black_Young",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "06_Black_Young",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "02_Black_Old",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "02_Black_Old",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "07_Hisp_Young",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "07_Hisp_Young",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "03_Hisp_Old",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "03_Hisp_Old",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "05_White_Young",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "05_White_Young",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "01_White_Old",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "01_White_Old",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "08_Other_Young",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "08_Other_Young",]$cai_pos_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "04_Other_Old",]$cai_pos_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "04_Other_Old",]$cai_pos_12.2))*100,1)


# Col 5 & 6: McNemar Test & p-value
exact2x2((table(hnm_np_2y$cai_pos_12.1, hnm_np_2y$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_pos_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


### * HIV-neg partners --------
# Col 2 & 3: % reporting CAIp12 in Y1 and Y2
round(prop.table(table(hnm_np_2y$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "06_Black_Young",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "06_Black_Young",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "02_Black_Old",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "02_Black_Old",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "07_Hisp_Young",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "07_Hisp_Young",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "03_Hisp_Old",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "03_Hisp_Old",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "05_White_Young",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "05_White_Young",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "01_White_Old",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "01_White_Old",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "08_Other_Young",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "08_Other_Young",]$cai_neg_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "04_Other_Old",]$cai_neg_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "04_Other_Old",]$cai_neg_12.2))*100,1)


# Col 5 & 6: McNemar Test & p-value
exact2x2((table(hnm_np_2y$cai_neg_12.1, hnm_np_2y$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_neg_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


### * Unk partners ------
# Col 2 & 3: % reporting CAIp12 in Y1 and Y2
round(prop.table(table(hnm_np_2y$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "06_Black_Young",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "06_Black_Young",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "02_Black_Old",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "02_Black_Old",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "07_Hisp_Young",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "07_Hisp_Young",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "03_Hisp_Old",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "03_Hisp_Old",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "05_White_Young",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "05_White_Young",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "01_White_Old",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "01_White_Old",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "08_Other_Young",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "08_Other_Young",]$cai_unk_12.2))*100,1)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "04_Other_Old",]$cai_unk_12.1))*100,1)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.1 == "04_Other_Old",]$cai_unk_12.2))*100,1)


# Col 5 & 6: McNemar Test & p-value
exact2x2((table(hnm_np_2y$cai_unk_12.1, hnm_np_2y$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$age_25.1 == 0,]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$age_25.1 == 1,]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Black",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "Hispanic",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_White",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$race_cat.1 == "NH_Other",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "1_<20k",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "2_<40k",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "3_<75k",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$income_cat.1 == "4_75k+",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$educ3.1 == "1_HS-or-less",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$educ3.1 == "2_ltCol",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$educ3.1 == "3_Col+",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "1_NorE",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "2_MidW",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "3_South",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$region4.1 == "4_West",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "1_LgCnt",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "2_LgFrg",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "3_Metro",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_unk_12.1, hnm_np_2y[hnm_np_2y$nchs4.1 == "4_Rural",]$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
