### SHMS Analyses :: for October 2023 R&R -----
### Prepare workspace -----
rm(list = ls())

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
library(visdat)
library(exact2x2)
library(stats)


# * define functions -----
tableNA <- function(x, ...){
  table(x, useNA = "ifany", ...)  
}

# * select necessary data -----
amis_2_years <- read.csv("amis_2yr_rr.csv")
amis_1_year <- read.csv("amis_1yr_rr.csv")

# * Generate variables and subset data -------
# * generate the new HNNP 2-year dataset
tableNA(amis_2_years$biomed_current.x,
        amis_2_years$biomed_current.y)

hnm_np_2y <- subset(amis_2_years, (neg_prep12_use_groups.x == "01_never-or-no-PrEP12" &
                                neg_prep12_use_groups.y == "01_never-or-no-PrEP12"))

#### `init` variable for initiating CAS between Y1 and Y2
hnm_np_2y$init <- ifelse((hnm_np_2y$cai_12_new.x == 0 &
                             hnm_np_2y$cai_12_new.y == 1),1,0)

### `cess` variable for "stopping" CAS between YI and Y2
hnm_np_2y$cess <- ifelse((hnm_np_2y$cai_12_new.x == 1 &
                             hnm_np_2y$cai_12_new.y == 0),1,0)

### `age_bin_cat` variable for regression
hnm_np_2y$age_bin_cat <- NA
hnm_np_2y$age_bin_cat <- factor(hnm_np_2y$age_25.x,
                                levels = c(0,1),
                                labels = c("<25", ">=25"))
###

hnm_np_2y_nc <- subset(hnm_np_2y, cai_12_new.x == 0)

hnm_np_2y_yc <- subset(hnm_np_2y, cai_12_new.x == 1)

hnm_np_2y_yc2 <- subset(hnm_np_2y, (N_cai_sps_12.x >= 1 &
                                       N_cai_sps_12.y >= 1))

amis_all <- read.csv("amis_all_rr.csv")

hnm_np_1y <- subset(amis_all, neg_prep12_use_groups == "01_never-or-no-PrEP12")

# subset hnm_np_2y  to remove NAs for binomial tests
hnm_np_2y_race <- subset(hnm_np_2y, !is.na(race_cat.x)) 
hnm_np_2y_income <- subset(hnm_np_2y, !is.na(income_cat.x))
hnm_np_2y_educ <- subset(hnm_np_2y, !is.na(educ3.x))
hnm_np_2y_caipos <- subset(hnm_np_2y, !is.na(cai_pos_12.x))
hnm_np_2y_caipos_race <- subset(hnm_np_2y_caipos, !is.na(race_cat.x))
hnm_np_2y_caipos_income <- subset(hnm_np_2y_caipos, !is.na(income_cat.x))
hnm_np_2y_caipos_educ <- subset(hnm_np_2y_caipos, !is.na(educ3.x))
hnm_np_2y_caineg <- subset(hnm_np_2y, !is.na(cai_neg_12.x))
hnm_np_2y_caineg_race <- subset(hnm_np_2y_caineg, !is.na(race_cat.x))
hnm_np_2y_caineg_income <- subset(hnm_np_2y_caineg, !is.na(income_cat.x))
hnm_np_2y_caineg_educ <- subset(hnm_np_2y_caineg, !is.na(educ3.x))
hnm_np_2y_caiunk <- subset(hnm_np_2y, !is.na(cai_unk_12.x))
hnm_np_2y_caiunk_race <- subset(hnm_np_2y_caiunk, !is.na(race_cat.x))
hnm_np_2y_caiunk_income <- subset(hnm_np_2y_caiunk, !is.na(income_cat.x))
hnm_np_2y_caiunk_educ <- subset(hnm_np_2y_caiunk, !is.na(educ3.x))

# Table 3: ------
### Year-on-year comparisons for reports of any condomless anal sex (CAS) in the last 12
### months among HIV-negative men who have sex with men (MSM) who have not used pre-
### exposure prophylaxis (PrEP) in the last 12 months for whom two consecutive years of data were
### available, American Men’s Internet Survey (AMIS) 2014-2019
# * * * Denom = hnm_np_2y

# overall
round(prop.table(table(hnm_np_2y$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y$cai_12_new.y))*100,2)[2]

exact2x2((table(hnm_np_2y$cai_12_new.x, hnm_np_2y$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# by survey year1

cbind(table(hnm_np_2y$year.x),
      round(prop.table(table(hnm_np_2y$year.x))*100,0))

round(prop.table(table(hnm_np_2y$year.x, hnm_np_2y$cai_12_new.x), margin = 1)*100,2)[,2]
round(prop.table(table(hnm_np_2y$year.x, hnm_np_2y$cai_12_new.y), margin = 1)*100,2)[,2]

exact2x2((table(hnm_np_2y[hnm_np_2y$year.x == 2014,]$cai_12_new.x, hnm_np_2y[hnm_np_2y$year.x == 2014,]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y[hnm_np_2y$year.x == 2015,]$cai_12_new.x, hnm_np_2y[hnm_np_2y$year.x == 2015,]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y[hnm_np_2y$year.x == 2016,]$cai_12_new.x, hnm_np_2y[hnm_np_2y$year.x == 2016,]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y[hnm_np_2y$year.x == 2017,]$cai_12_new.x, hnm_np_2y[hnm_np_2y$year.x == 2017,]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y[hnm_np_2y$year.x == 2018,]$cai_12_new.x, hnm_np_2y[hnm_np_2y$year.x == 2018,]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# by age
tableNA(hnm_np_2y$age_25.x)
round(prop.table(tableNA(hnm_np_2y$age_25.x))*100)

round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.x == 0,]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.x == 0,]$cai_12_new.y))*100,2)[2]

round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.x == 1,]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$age_25.x == 1,]$cai_12_new.y))*100,2)[2]

exact2x2((table(hnm_np_2y[hnm_np_2y$age_25.x == 0,]$cai_12_new.x, hnm_np_2y[hnm_np_2y$age_25.x == 0,]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y[hnm_np_2y$age_25.x == 1,]$cai_12_new.x, hnm_np_2y[hnm_np_2y$age_25.x == 1,]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# by race
table(is.na(hnm_np_2y$race_cat.x))
table(hnm_np_2y$race_cat.x)
round(prop.table(table(hnm_np_2y$race_cat.x))*100)

round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_cat.x == "NH_Black",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_cat.x == "NH_Black",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.x == "NH_Black",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_cat.x == "NH_Black",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_cat.x == "Hispanic",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_cat.x == "Hispanic",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.x == "Hispanic",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_cat.x == "Hispanic",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_cat.x == "NH_White",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_cat.x == "NH_White",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.x == "NH_White",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_cat.x == "NH_White",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_cat.x == "NH_Other",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_cat.x == "NH_Other",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_cat.x == "NH_Other",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_cat.x == "NH_Other",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by income
table(is.na(hnm_np_2y$income_cat.x))
table(hnm_np_2y$income_cat.x)
round(prop.table(table(hnm_np_2y$income_cat.x))*100)

round(prop.table(table(hnm_np_2y_income[hnm_np_2y_income$income_cat.x == "1_<20k",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_income[hnm_np_2y_income$income_cat.x == "1_<20k",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.x == "1_<20k",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$income_cat.x == "1_<20k",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_income[hnm_np_2y_income$income_cat.x == "2_<40k",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_income[hnm_np_2y_income$income_cat.x == "2_<40k",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.x == "2_<40k",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$income_cat.x == "2_<40k",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_income[hnm_np_2y_income$income_cat.x == "3_<75k",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_income[hnm_np_2y_income$income_cat.x == "3_<75k",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.x == "3_<75k",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$income_cat.x == "3_<75k",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_income[hnm_np_2y_income$income_cat.x == "4_75k+",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_income[hnm_np_2y_income$income_cat.x == "4_75k+",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$income_cat.x == "4_75k+",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$income_cat.x == "4_75k+",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by education
table(is.na(hnm_np_2y$educ3.x))
table(hnm_np_2y$educ3.x)
round(prop.table(table(hnm_np_2y$educ3.x))*100)

round(prop.table(table(hnm_np_2y_educ[hnm_np_2y_educ$educ3.x == "1_HS-or-less",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_educ[hnm_np_2y_educ$educ3.x == "1_HS-or-less",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.x == "1_HS-or-less",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$educ3.x == "1_HS-or-less",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_educ[hnm_np_2y_educ$educ3.x == "2_ltCol",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_educ[hnm_np_2y_educ$educ3.x == "2_ltCol",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.x == "2_ltCol",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$educ3.x == "2_ltCol",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_educ[hnm_np_2y_educ$educ3.x == "3_Col+",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_educ[hnm_np_2y_educ$educ3.x == "3_Col+",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$educ3.x == "3_Col+",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$educ3.x == "3_Col+",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by region
table(is.na(hnm_np_2y$region4.x))
table(hnm_np_2y$region4.x)
round(prop.table(table(hnm_np_2y$region4.x))*100)

round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.x == "1_NorE",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.x == "1_NorE",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$region4.x == "1_NorE",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$region4.x == "1_NorE",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.x == "2_MidW",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.x == "2_MidW",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$region4.x == "2_MidW",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$region4.x == "2_MidW",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.x == "3_South",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.x == "3_South",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$region4.x == "3_South",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$region4.x == "3_South",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.x == "4_West",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$region4.x == "4_West",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$region4.x == "4_West",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$region4.x == "4_West",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by urbanicity 
table(is.na(hnm_np_2y$nchs4.x))
table(hnm_np_2y$nchs4.x)
round(prop.table(table(hnm_np_2y$nchs4.x))*100)

round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.x == "1_LgCnt",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.x == "1_LgCnt",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.x == "1_LgCnt",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$nchs4.x == "1_LgCnt",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.x == "2_LgFrg",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.x == "2_LgFrg",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.x == "2_LgFrg",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$nchs4.x == "2_LgFrg",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.x == "3_Metro",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.x == "3_Metro",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.x == "3_Metro",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$nchs4.x == "3_Metro",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.x == "4_Rural",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$nchs4.x == "4_Rural",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$nchs4.x == "4_Rural",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$nchs4.x == "4_Rural",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# sub-analysis by race & age
table(is.na(hnm_np_2y$race_age_cat.x))
table(hnm_np_2y[hnm_np_2y$race_cat.x == "NH_Black",]$age_25.x)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.x == "NH_Black",]$age_25.x))*100)

round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_age_cat.x == "06_Black_Young",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_age_cat.x == "06_Black_Young",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "06_Black_Young",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_age_cat.x == "06_Black_Young",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_age_cat.x == "02_Black_Old",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_age_cat.x == "02_Black_Old",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "02_Black_Old",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_age_cat.x == "02_Black_Old",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


table(hnm_np_2y_race[hnm_np_2y_race$race_cat.x == "Hispanic",]$age_25.x)
round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_cat.x == "Hispanic",]$age_25.x))*100)

round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_age_cat.x == "07_Hisp_Young",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_age_cat.x == "07_Hisp_Young",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "07_Hisp_Young",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_age_cat.x == "07_Hisp_Young",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_age_cat.x == "03_Hisp_Old",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_race[hnm_np_2y_race$race_age_cat.x == "03_Hisp_Old",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "03_Hisp_Old",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_age_cat.x == "03_Hisp_Old",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


table(hnm_np_2y[hnm_np_2y$race_cat.x == "NH_White",]$age_25.x)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.x == "NH_White",]$age_25.x))*100)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "05_White_Young",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "05_White_Young",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "05_White_Young",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_age_cat.x == "05_White_Young",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "01_White_Old",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "01_White_Old",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "01_White_Old",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_age_cat.x == "01_White_Old",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


table(hnm_np_2y[hnm_np_2y$race_cat.x == "NH_Other",]$age_25.x)
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_cat.x == "NH_Other",]$age_25.x))*100)

round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "08_Other_Young",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "08_Other_Young",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "08_Other_Young",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_age_cat.x == "08_Other_Young",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "04_Other_Old",]$cai_12_new.x))*100,2)[2]
round(prop.table(table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "04_Other_Old",]$cai_12_new.y))*100,2)[2]
exact2x2((table(hnm_np_2y[hnm_np_2y$race_age_cat.x == "04_Other_Old",]$cai_12_new.x, hnm_np_2y[hnm_np_2y$race_age_cat.x == "04_Other_Old",]$cai_12_new.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


table(hnm_np_2y$race_age_cat.x)


# Table 4: ------
### Year-on-year comparisons of any condomless anal sex (CAS) in the last 12 months by partner serostatus 
### among HIV-negative men who have sex with men (MSM) who have not used pre-exposure prophylaxis (PrEP) 
### in the last 12 months for whom two subsequent years of data were available, American Men’s Internet 
### Survey (AMIS) 2014-2019
# * * * Denom = hnm_np_2y

# * (1) SPs LWH -----
# overall
round(prop.table(table(hnm_np_2y_caipos$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos$cai_pos_12.y))*100,2)[2]

exact2x2((table(hnm_np_2y$cai_pos_12.x, hnm_np_2y$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by age
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$age_25.x == 0,]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$age_25.x == 0,]$cai_pos_12.y))*100,2)[2]

round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$age_25.x == 1,]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$age_25.x == 1,]$cai_pos_12.y))*100,2)[2]

exact2x2((table(hnm_np_2y_caipos[hnm_np_2y_caipos$age_25.x == 0,]$cai_pos_12.x, hnm_np_2y_caipos[hnm_np_2y_caipos$age_25.x == 0,]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y_caipos[hnm_np_2y_caipos$age_25.x == 1,]$cai_pos_12.x, hnm_np_2y_caipos[hnm_np_2y_caipos$age_25.x == 1,]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by race
round(prop.table(table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_Black",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_Black",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_Black",]$cai_pos_12.x, hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_Black",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "Hispanic",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "Hispanic",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "Hispanic",]$cai_pos_12.x, hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "Hispanic",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_White",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_White",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_White",]$cai_pos_12.x, hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_White",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_Other",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_Other",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_Other",]$cai_pos_12.x, hnm_np_2y_caipos_race[hnm_np_2y_caipos_race$race_cat.x == "NH_Other",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by income
round(prop.table(table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "1_<20k",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "1_<20k",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "1_<20k",]$cai_pos_12.x, hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "1_<20k",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "2_<40k",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "2_<40k",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "2_<40k",]$cai_pos_12.x, hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "2_<40k",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "3_<75k",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "3_<75k",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "3_<75k",]$cai_pos_12.x, hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "3_<75k",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "4_75k+",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "4_75k+",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "4_75k+",]$cai_pos_12.x, hnm_np_2y_caipos_income[hnm_np_2y_caipos_income$income_cat.x == "4_75k+",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by education
round(prop.table(table(hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "1_HS-or-less",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "1_HS-or-less",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "1_HS-or-less",]$cai_pos_12.x, hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "1_HS-or-less",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "2_ltCol",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "2_ltCol",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "2_ltCol",]$cai_pos_12.x, hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "2_ltCol",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "3_Col+",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "3_Col+",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "3_Col+",]$cai_pos_12.x, hnm_np_2y_caipos_educ[hnm_np_2y_caipos_educ$educ3.x == "3_Col+",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by region
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "1_NorE",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "1_NorE",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "1_NorE",]$cai_pos_12.x, hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "1_NorE",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "2_MidW",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "2_MidW",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "2_MidW",]$cai_pos_12.x, hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "2_MidW",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "3_South",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "3_South",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "3_South",]$cai_pos_12.x, hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "3_South",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "4_West",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "4_West",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "4_West",]$cai_pos_12.x, hnm_np_2y_caipos[hnm_np_2y_caipos$region4.x == "4_West",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by urbanicity 
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "1_LgCnt",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "1_LgCnt",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "1_LgCnt",]$cai_pos_12.x, hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "1_LgCnt",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "2_LgFrg",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "2_LgFrg",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "2_LgFrg",]$cai_pos_12.x, hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "2_LgFrg",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "3_Metro",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "3_Metro",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "3_Metro",]$cai_pos_12.x, hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "3_Metro",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "4_Rural",]$cai_pos_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "4_Rural",]$cai_pos_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "4_Rural",]$cai_pos_12.x, hnm_np_2y_caipos[hnm_np_2y_caipos$nchs4.x == "4_Rural",]$cai_pos_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * (2) NEG SPs -------
# overall
round(prop.table(table(hnm_np_2y_caineg$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg$cai_neg_12.y))*100,2)[2]

exact2x2((table(hnm_np_2y$cai_neg_12.x, hnm_np_2y$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by age
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$age_25.x == 0,]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$age_25.x == 0,]$cai_neg_12.y))*100,2)[2]

round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$age_25.x == 1,]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$age_25.x == 1,]$cai_neg_12.y))*100,2)[2]

exact2x2((table(hnm_np_2y_caineg[hnm_np_2y_caineg$age_25.x == 0,]$cai_neg_12.x, hnm_np_2y_caineg[hnm_np_2y_caineg$age_25.x == 0,]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y_caineg[hnm_np_2y_caineg$age_25.x == 1,]$cai_neg_12.x, hnm_np_2y_caineg[hnm_np_2y_caineg$age_25.x == 1,]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by race
round(prop.table(table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_Black",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_Black",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_Black",]$cai_neg_12.x, hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_Black",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "Hispanic",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "Hispanic",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "Hispanic",]$cai_neg_12.x, hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "Hispanic",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_White",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_White",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_White",]$cai_neg_12.x, hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_White",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_Other",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_Other",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_Other",]$cai_neg_12.x, hnm_np_2y_caineg_race[hnm_np_2y_caineg_race$race_cat.x == "NH_Other",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by income
round(prop.table(table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "1_<20k",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "1_<20k",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "1_<20k",]$cai_neg_12.x, hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "1_<20k",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "2_<40k",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "2_<40k",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "2_<40k",]$cai_neg_12.x, hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "2_<40k",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "3_<75k",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "3_<75k",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "3_<75k",]$cai_neg_12.x, hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "3_<75k",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "4_75k+",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "4_75k+",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "4_75k+",]$cai_neg_12.x, hnm_np_2y_caineg_income[hnm_np_2y_caineg_income$income_cat.x == "4_75k+",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by education
round(prop.table(table(hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "1_HS-or-less",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "1_HS-or-less",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "1_HS-or-less",]$cai_neg_12.x, hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "1_HS-or-less",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "2_ltCol",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "2_ltCol",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "2_ltCol",]$cai_neg_12.x, hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "2_ltCol",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "3_Col+",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "3_Col+",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "3_Col+",]$cai_neg_12.x, hnm_np_2y_caineg_educ[hnm_np_2y_caineg_educ$educ3.x == "3_Col+",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by region
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "1_NorE",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "1_NorE",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "1_NorE",]$cai_neg_12.x, hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "1_NorE",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "2_MidW",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "2_MidW",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "2_MidW",]$cai_neg_12.x, hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "2_MidW",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "3_South",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "3_South",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "3_South",]$cai_neg_12.x, hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "3_South",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "4_West",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "4_West",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "4_West",]$cai_neg_12.x, hnm_np_2y_caineg[hnm_np_2y_caineg$region4.x == "4_West",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by urbanicity 
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "1_LgCnt",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "1_LgCnt",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "1_LgCnt",]$cai_neg_12.x, hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "1_LgCnt",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "2_LgFrg",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "2_LgFrg",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "2_LgFrg",]$cai_neg_12.x, hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "2_LgFrg",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "3_Metro",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "3_Metro",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "3_Metro",]$cai_neg_12.x, hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "3_Metro",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "4_Rural",]$cai_neg_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "4_Rural",]$cai_neg_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "4_Rural",]$cai_neg_12.x, hnm_np_2y_caineg[hnm_np_2y_caineg$nchs4.x == "4_Rural",]$cai_neg_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * (3) UNK stat SPs ------
# overall
round(prop.table(table(hnm_np_2y_caiunk$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk$cai_unk_12.y))*100,2)[2]

exact2x2((table(hnm_np_2y$cai_unk_12.x, hnm_np_2y$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by age
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$age_25.x == 0,]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$age_25.x == 0,]$cai_unk_12.y))*100,2)[2]

round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$age_25.x == 1,]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$age_25.x == 1,]$cai_unk_12.y))*100,2)[2]

exact2x2((table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$age_25.x == 0,]$cai_unk_12.x, hnm_np_2y_caiunk[hnm_np_2y_caiunk$age_25.x == 0,]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$age_25.x == 1,]$cai_unk_12.x, hnm_np_2y_caiunk[hnm_np_2y_caiunk$age_25.x == 1,]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by race
round(prop.table(table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_Black",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_Black",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_Black",]$cai_unk_12.x, hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_Black",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "Hispanic",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "Hispanic",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "Hispanic",]$cai_unk_12.x, hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "Hispanic",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_White",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_White",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_White",]$cai_unk_12.x, hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_White",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_Other",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_Other",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_Other",]$cai_unk_12.x, hnm_np_2y_caiunk_race[hnm_np_2y_caiunk_race$race_cat.x == "NH_Other",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by income
round(prop.table(table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "1_<20k",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "1_<20k",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "1_<20k",]$cai_unk_12.x, hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "1_<20k",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "2_<40k",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "2_<40k",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "2_<40k",]$cai_unk_12.x, hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "2_<40k",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "3_<75k",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "3_<75k",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "3_<75k",]$cai_unk_12.x, hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "3_<75k",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "4_75k+",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "4_75k+",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "4_75k+",]$cai_unk_12.x, hnm_np_2y_caiunk_income[hnm_np_2y_caiunk_income$income_cat.x == "4_75k+",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by education
round(prop.table(table(hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "1_HS-or-less",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "1_HS-or-less",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "1_HS-or-less",]$cai_unk_12.x, hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "1_HS-or-less",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "2_ltCol",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "2_ltCol",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "2_ltCol",]$cai_unk_12.x, hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "2_ltCol",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "3_Col+",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "3_Col+",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "3_Col+",]$cai_unk_12.x, hnm_np_2y_caiunk_educ[hnm_np_2y_caiunk_educ$educ3.x == "3_Col+",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by region
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "1_NorE",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "1_NorE",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "1_NorE",]$cai_unk_12.x, hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "1_NorE",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "2_MidW",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "2_MidW",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "2_MidW",]$cai_unk_12.x, hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "2_MidW",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "3_South",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "3_South",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "3_South",]$cai_unk_12.x, hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "3_South",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "4_West",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "4_West",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "4_West",]$cai_unk_12.x, hnm_np_2y_caiunk[hnm_np_2y_caiunk$region4.x == "4_West",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# by urbanicity 
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "1_LgCnt",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "1_LgCnt",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "1_LgCnt",]$cai_unk_12.x, hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "1_LgCnt",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "2_LgFrg",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "2_LgFrg",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "2_LgFrg",]$cai_unk_12.x, hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "2_LgFrg",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "3_Metro",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "3_Metro",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "3_Metro",]$cai_unk_12.x, hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "3_Metro",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "4_Rural",]$cai_unk_12.x))*100,2)[2]
round(prop.table(table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "4_Rural",]$cai_unk_12.y))*100,2)[2]
exact2x2((table(hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "4_Rural",]$cai_unk_12.x, hnm_np_2y_caiunk[hnm_np_2y_caiunk$nchs4.x == "4_Rural",]$cai_unk_12.y)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test



# Table 5: ------
### Logistic regression (odds ratio) results for initiation of condomless anal sex among HIV-negative 
### respondents who remained off pre-exposure prophylaxis PrEP for two subsequent years and did not report having had 
### condomless anal sex in the first year, American Men’s Internet Survey, 2014-2019
# * * * Denom = hnm_np_2y_nc

# * (Col 1) Col % ------
tableNA(hnm_np_2y_nc$age_25.x)
round(prop.table(tableNA(hnm_np_2y_nc$age_25.x))*100)

tableNA(hnm_np_2y_nc$race_cat.x)
round(prop.table(tableNA(hnm_np_2y_nc$race_cat.x))*100)

tableNA(hnm_np_2y_nc$income_cat.x)
round(prop.table(tableNA(hnm_np_2y_nc$income_cat.x))*100)

tableNA(hnm_np_2y_nc$educ3.x)
round(prop.table(tableNA(hnm_np_2y_nc$educ3.x))*100)

tableNA(hnm_np_2y_nc$region4.x)
round(prop.table(tableNA(hnm_np_2y_nc$region4.x))*100)

tableNA(hnm_np_2y_nc$nchs4.x)
round(prop.table(tableNA(hnm_np_2y_nc$nchs4.x))*100)

# * (Col 2) % who initiated CAS ----
tableNA(hnm_np_2y_nc[hnm_np_2y_nc$age_25.x == 0,]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$age_25.x == 0,]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$age_25.x == 1,]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$age_25.x == 1,]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$race_cat.x == "NH_Black",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$race_cat.x == "NH_Black",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$race_cat.x == "Hispanic",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$race_cat.x == "Hispanic",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$race_cat.x == "NH_White",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$race_cat.x == "NH_White",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$race_cat.x == "NH_Other",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$race_cat.x == "NH_Other",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$income_cat.x == "1_<20k",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$income_cat.x == "1_<20k",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$income_cat.x == "2_<40k",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$income_cat.x == "2_<40k",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$income_cat.x == "3_<75k",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$income_cat.x == "3_<75k",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$income_cat.x == "4_75k+",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$income_cat.x == "4_75k+",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$educ3.x == "1_HS-or-less",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$educ3.x == "1_HS-or-less",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$educ3.x == "2_ltCol",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$educ3.x == "2_ltCol",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$educ3.x == "3_Col+",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$educ3.x == "3_Col+",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$region4.x == "1_NorE",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$region4.x == "1_NorE",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$region4.x == "2_MidW",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$region4.x == "2_MidW",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$region4.x == "3_South",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$region4.x == "3_South",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$region4.x == "4_West",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$region4.x == "4_West",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$nchs4.x == "1_LgCnt",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$nchs4.x == "1_LgCnt",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$nchs4.x == "2_LgFrg",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$nchs4.x == "2_LgFrg",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$nchs4.x == "3_Metro",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$nchs4.x == "3_Metro",]$init))*100)[2]

tableNA(hnm_np_2y_nc[hnm_np_2y_nc$nchs4.x == "4_Rural",]$init)[2]
round(prop.table(tableNA(hnm_np_2y_nc[hnm_np_2y_nc$nchs4.x == "4_Rural",]$init))*100)[2]


# * (Col 3) Bivar associations and (Col 4) p-values ------
# age
t5_age_mod <- glm(init ~ relevel(age_bin_cat, ref = ">=25"), data = hnm_np_2y_nc, family = "binomial"(link="logit"))

round(exp(coef(t5_age_mod))[2],2)
round(exp(confint(t5_age_mod)[2,]),2)
summary(t5_age_mod)

# race
t5_race_mod <- glm(init ~ relevel(as.factor(race_cat.x), ref = "NH_White"), data = hnm_np_2y_nc, family = "binomial"(link="logit"))

round(exp(coef(t5_race_mod))[3],2) # Black
round(exp(confint(t5_race_mod)[3,]),2) # Black

round(exp(coef(t5_race_mod))[2],2) # Hispanic
round(exp(confint(t5_race_mod)[2,]),2) # Hispanic

round(exp(coef(t5_race_mod))[4],2) # Other
round(exp(confint(t5_race_mod)[4,]),2) # Other

summary(t5_race_mod) # p values

# income
t5_income_mod <- glm(init ~ relevel(as.factor(income_cat.x), ref = "4_75k+"), data = hnm_np_2y_nc, family = "binomial"(link="logit"))

round(exp(coef(t5_income_mod))[2],2) # level 1
round(exp(confint(t5_income_mod)[2,]),2) # level 1

round(exp(coef(t5_income_mod))[3],2) # level 2
round(exp(confint(t5_income_mod)[3,]),2) # level 2

round(exp(coef(t5_income_mod))[4],2) # level 3
round(exp(confint(t5_income_mod)[4,]),2) # level 3

summary(t5_income_mod)

# education
t5_educ_mod <- glm(init ~ relevel(as.factor(educ3.x), ref = "3_Col+"), data = hnm_np_2y_nc, family = "binomial"(link="logit"))

round(exp(coef(t5_educ_mod))[2],2) # level 1
round(exp(confint(t5_educ_mod)[2,]),2) # level 1

round(exp(coef(t5_educ_mod))[3],2) # level 2
round(exp(confint(t5_educ_mod)[3,]),2) # level 2

summary(t5_educ_mod)

# region
t5_reg_mod <- glm(init ~ relevel(as.factor(region4.x), ref = "3_South"), data = hnm_np_2y_nc, family = "binomial"(link="logit"))

round(exp(coef(t5_reg_mod))[2],2) # NE
round(exp(confint(t5_reg_mod)[2,]),2) # NE

round(exp(coef(t5_reg_mod))[3],2) # MW
round(exp(confint(t5_reg_mod)[3,]),2) # MW

round(exp(coef(t5_reg_mod))[4],2) # West
round(exp(confint(t5_reg_mod)[4,]),2) # West

summary(t5_reg_mod)

# density
t5_dens_mod <- glm(init ~ nchs4.x, data = hnm_np_2y_nc, family = "binomial"(link="logit"))

round(exp(coef(t5_dens_mod))[2],2) # 2
round(exp(confint(t5_dens_mod)[2,]),2) # 2

round(exp(coef(t5_dens_mod))[3],2) # 3
round(exp(confint(t5_dens_mod)[3,]),2) # 3

round(exp(coef(t5_dens_mod))[4],2) # 4
round(exp(confint(t5_dens_mod)[4,]),2) # 4

summary(t5_dens_mod)

# * (Col 5) Multivar associations and (Col 6) p-values ------
t5_multi_mod <- glm(
   init ~
      race_age_cat.x +
      relevel(as.factor(educ3.x), ref = "3_Col+") +
      relevel(as.factor(region4.x), ref = "3_South") +
      nchs4.x,
   data = hnm_np_2y_nc,
   family = "binomial"(link = "logit")
)

round(exp(coef(t5_multi_mod)),2)
round(exp(confint(t5_multi_mod)),2)

round(exp(coef(t5_multi_mod)[9]),2) # HS or less
round(exp(confint(t5_multi_mod)[9,]),2) # HS or less

round(exp(coef(t5_multi_mod)[10]),2) # lt college
round(exp(confint(t5_multi_mod)[10,]),2) # lt college

round(exp(coef(t5_multi_mod)[11]),2) # NE
round(exp(confint(t5_multi_mod)[11,]),2) # NE

round(exp(coef(t5_multi_mod)[12]),2) # MW
round(exp(confint(t5_multi_mod)[12,]),2) # MW

round(exp(coef(t5_multi_mod)[13]),2) # West
round(exp(confint(t5_multi_mod)[13,]),2) # West

round(exp(coef(t5_multi_mod)[14]),2) # NCHS-2
round(exp(confint(t5_multi_mod)[14,]),2) # NCHS-2

round(exp(coef(t5_multi_mod)[15]),2) # NCHS-3
round(exp(confint(t5_multi_mod)[15,]),2) # NCHS-3

round(exp(coef(t5_multi_mod)[16]),2) # NCHS-4
round(exp(confint(t5_multi_mod)[16,]),2) # NCHS-4

summary(t5_multi_mod)

# Table S1: ------
### Table S1: χ2 test results comparing respondents who completed one year vs. two years of the American Men’s 
### Internet Survey, each with initial year 2014-18
# * * * Ref = amis_1_year; Comp = amis_2_years

nrow(amis_1_year)
nrow(amis_2_years)

# (Col 1) 1-years ------
table(amis_1_year$age_25)
round(prop.table(table(amis_1_year$age_25))*100,0)

tableNA(amis_1_year$race_cat)
round(prop.table(tableNA(amis_1_year$race_cat))*100,0)

tableNA(amis_1_year$income_cat)
round(prop.table(tableNA(amis_1_year$income_cat))*100,0)

tableNA(amis_1_year$educ3)
round(prop.table(tableNA(amis_1_year$educ3))*100,0)

tableNA(amis_1_year$region4)
round(prop.table(tableNA(amis_1_year$region4))*100,0)

tableNA(amis_1_year$nchs4)
round(prop.table(tableNA(amis_1_year$nchs4))*100,0)

tableNA(amis_1_year$cai_12_new)
round(prop.table(tableNA(amis_1_year$cai_12_new))*100,0)

tableNA(amis_1_year$biomed_4)
round(prop.table(tableNA(amis_1_year$biomed_4))*100,0)

# (Col 2) 2-years ------
table(amis_2_years$age_25.x)
round(prop.table(table(amis_2_years$age_25.x))*100,0)

tableNA(amis_2_years$race_cat.x)
round(prop.table(tableNA(amis_2_years$race_cat.x))*100,0)

tableNA(amis_2_years$income_cat.x)
round(prop.table(tableNA(amis_2_years$income_cat.x))*100,0)

tableNA(amis_2_years$educ3.x)
round(prop.table(tableNA(amis_2_years$educ3.x))*100,0)

tableNA(amis_2_years$region4.x)
round(prop.table(tableNA(amis_2_years$region4.x))*100,0)

tableNA(amis_2_years$nchs4.x)
round(prop.table(tableNA(amis_2_years$nchs4.x))*100,0)

tableNA(amis_2_years$cai_12_new.x)
round(prop.table(tableNA(amis_2_years$cai_12_new.x))*100,0)

tableNA(amis_2_years$biomed_4.x)
round(prop.table(tableNA(amis_2_years$biomed_4.x))*100,0)

# (Col 3) ChiSq -----
age1 <- table(amis_1_year$age_25)
age2 <- table(amis_2_years$age_25.x)
chisq.test(cbind(c(age1[1], age1[2]), c(age2[1], age2[2])))

race1 <- table(amis_1_year$race_cat)
race2 <- table(amis_2_years$race_cat.x)
chisq.test(cbind(race1, race2))

inc1 <- table(amis_1_year$income_cat)
inc2 <- table(amis_2_years$income_cat.x)
chisq.test(cbind(inc1, inc2))

educ1 <- table(amis_1_year$educ3)
educ2 <- table(amis_2_years$educ3.x)
chisq.test(cbind(educ1, educ2))

region1 <- table(amis_1_year$region4)
region2 <- table(amis_2_years$region4.x)
chisq.test(cbind(region1, region2))

nchs1 <- table(amis_1_year$nchs4)
nchs2 <- table(amis_2_years$nchs4.x)
chisq.test(cbind(nchs1, nchs2))

cai1 <- table(amis_1_year$cai_12_new)
cai2 <- table(amis_2_years$cai_12_new.x)
chisq.test(cbind(cai1, cai2))

biomed1 <- table(amis_1_year$biomed_4)
biomed2 <- table(amis_2_years$biomed_4.x)
chisq.test(cbind(biomed1, biomed2))

# Table S2: -------
### Percent reporting condomless anal sex in the last twelve months among HIV-negative respondents who have not 
### used pre-exposure prophylaxis (PrEP) in the last twelve months, American Men’s Internet Survey, 2014-2019
# * * * Denom = hnm_np_1y

all_test_data <- c(2572, 5420, 5267, 4922, 5255, 5124, 3899, 8798, 8286, 7925, 8197, 7818)
all_test <- matrix(all_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(all_test, alternative = "one.sided")

young_test_data <- c(416, 1637, 1604, 1531, 2409, 2450, 648, 2689, 2552, 2540, 3923, 3806)
young_test <- matrix(young_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(young_test, alternative = "one.sided")

old_test_data <- c(2156, 3783, 3663, 3391, 2846, 2674, 3251, 6109, 5734, 5385, 4274, 4012)
old_test <- matrix(old_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(old_test, alternative = "one.sided")

bl_test_data <- c(127, 359, 416, 326, 322, 635, 196, 585, 693, 536, 524, 1020)
bl_test <- matrix(bl_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(bl_test, alternative = "one.sided")

h_test_data <- c(348, 748, 735, 812, 880, 845, 519, 1175, 1071, 1241, 1365, 1272)
h_test <- matrix(h_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(h_test, alternative = "one.sided")

w_test_data <- c(1920,3934,3732,3431,3656,3235,2915,6386,5895,5515,5641,4876)
w_test <- matrix(w_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(w_test, alternative = "one.sided")

o_test_data <- c(150,294,307,261,301,310,222,508,498,457,509,488)
o_test <- matrix(o_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(o_test, alternative = "one.sided")


# Table S3: ---------
### Logistic regression (odds ratio) results for cessation of condomless anal sex among HIV-negative respondents who 
### remained off PrEP for two subsequent years and reported having had condomless anal intercourse in the first year, 
### American Men’s Internet Survey, 2014-2019
# * * * Denom = hnm_np_2y_yc

# * (Col 1) Col % ------
tableNA(hnm_np_2y_yc$age_25.x)
round(prop.table(tableNA(hnm_np_2y_yc$age_25.x))*100)

tableNA(hnm_np_2y_yc$race_cat.x)
round(prop.table(tableNA(hnm_np_2y_yc$race_cat.x))*100)

tableNA(hnm_np_2y_yc$income_cat.x)
round(prop.table(tableNA(hnm_np_2y_yc$income_cat.x))*100)

tableNA(hnm_np_2y_yc$educ3.x)
round(prop.table(tableNA(hnm_np_2y_yc$educ3.x))*100)

tableNA(hnm_np_2y_yc$region4.x)
round(prop.table(tableNA(hnm_np_2y_yc$region4.x))*100)

tableNA(hnm_np_2y_yc$nchs4.x)
round(prop.table(tableNA(hnm_np_2y_yc$nchs4.x))*100)

# * (Col 2) % who ceased CAS ----
table(hnm_np_2y_yc[hnm_np_2y_yc$age_25.x == 0,]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$age_25.x == 0,]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$age_25.x == 1,]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$age_25.x == 1,]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$race_cat.x == "NH_Black",]$cess)
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$race_cat.x == "NH_Black",]$cess))*100)

table(hnm_np_2y_yc[hnm_np_2y_yc$race_cat.x == "Hispanic",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$race_cat.x == "Hispanic",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$race_cat.x == "NH_White",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$race_cat.x == "NH_White",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$race_cat.x == "NH_Other",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$race_cat.x == "NH_Other",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$income_cat.x == "1_<20k",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$income_cat.x == "1_<20k",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$income_cat.x == "2_<40k",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$income_cat.x == "2_<40k",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$income_cat.x == "3_<75k",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$income_cat.x == "3_<75k",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$income_cat.x == "4_75k+",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$income_cat.x == "4_75k+",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$educ3.x == "1_HS-or-less",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$educ3.x == "1_HS-or-less",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$educ3.x == "2_ltCol",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$educ3.x == "2_ltCol",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$educ3.x == "3_Col+",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$educ3.x == "3_Col+",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$region4.x == "1_NorE",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$region4.x == "1_NorE",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$region4.x == "2_MidW",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$region4.x == "2_MidW",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$region4.x == "3_South",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$region4.x == "3_South",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$region4.x == "4_West",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$region4.x == "4_West",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$nchs4.x == "1_LgCnt",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$nchs4.x == "1_LgCnt",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$nchs4.x == "2_LgFrg",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$nchs4.x == "2_LgFrg",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$nchs4.x == "3_Metro",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$nchs4.x == "3_Metro",]$cess))*100)[2]

table(hnm_np_2y_yc[hnm_np_2y_yc$nchs4.x == "4_Rural",]$cess)[2]
round(prop.table(table(hnm_np_2y_yc[hnm_np_2y_yc$nchs4.x == "4_Rural",]$cess))*100)[2]

# * (Col 3) Bivariate estimates and (Col 4) p-values ------
# age
s3_age_mod <- glm(cess ~ relevel(age_bin_cat, ref = ">=25"), data = hnm_np_2y_yc, family = "binomial"(link="logit"))

round(exp(coef(s3_age_mod))[2],2)
round(exp(confint(s3_age_mod)[2,]),2)
summary(s3_age_mod)

# race
s3_race_mod <- glm(cess ~ relevel(as.factor(race_cat.x), ref = "NH_White"), data = hnm_np_2y_yc, family = "binomial"(link="logit"))

round(exp(coef(s3_race_mod))[3],2) # Black
round(exp(confint(s3_race_mod)[3,]),2) # Black

round(exp(coef(s3_race_mod))[2],2) # Hispanic
round(exp(confint(s3_race_mod)[2,]),2) # Hispanic

round(exp(coef(s3_race_mod))[4],2) # Other
round(exp(confint(s3_race_mod)[4,]),2) # Other

summary(s3_race_mod) # p values

# income
s3_income_mod <- glm(cess ~ relevel(as.factor(income_cat.x), ref = "4_75k+"), data = hnm_np_2y_yc, family = "binomial"(link="logit"))

round(exp(coef(s3_income_mod))[2],2) # level 1
round(exp(confint(s3_income_mod)[2,]),2) # level 1

round(exp(coef(s3_income_mod))[3],2) # level 2
round(exp(confint(s3_income_mod)[3,]),2) # level 2

round(exp(coef(s3_income_mod))[4],2) # level 3
round(exp(confint(s3_income_mod)[4,]),2) # level 3

summary(s3_income_mod)

# education
s3_educ_mod <- glm(cess ~ relevel(as.factor(educ3.x), ref = "3_Col+"), data = hnm_np_2y_yc, family = "binomial"(link="logit"))

round(exp(coef(s3_educ_mod))[2],2) # level 1
round(exp(confint(s3_educ_mod)[2,]),2) # level 1

round(exp(coef(s3_educ_mod))[3],2) # level 2
round(exp(confint(s3_educ_mod)[3,]),2) # level 2

summary(s3_educ_mod)

# region
s3_reg_mod <- glm(cess ~ relevel(as.factor(region4.x), ref = "3_South"), data = hnm_np_2y_yc, family = "binomial"(link="logit"))

round(exp(coef(s3_reg_mod))[2],2) # NE
round(exp(confint(s3_reg_mod)[2,]),2) # NE

round(exp(coef(s3_reg_mod))[3],2) # MW
round(exp(confint(s3_reg_mod)[3,]),2) # MW

round(exp(coef(s3_reg_mod))[4],2) # West
round(exp(confint(s3_reg_mod)[4,]),2) # West

summary(s3_reg_mod)

# density
s3_dens_mod <- glm(cess ~ nchs4.x, data = hnm_np_2y_yc, family = "binomial"(link="logit"))

round(exp(coef(s3_dens_mod))[2],2) # 2
round(exp(confint(s3_dens_mod)[2,]),2) # 2

round(exp(coef(s3_dens_mod))[3],2) # 3
round(exp(confint(s3_dens_mod)[3,]),2) # 3

round(exp(coef(s3_dens_mod))[4],2) # 4
round(exp(confint(s3_dens_mod)[4,]),2) # 4

summary(s3_dens_mod)

# Table S4: ----------
### Measures of the distribution of number of condomless anal sex partners among respondents who stayed off pre-exposure 
### prophylaxis (PrEP) for two surveys, and reported at least one condomless anal sex partner in the last 12 months at 
### each survey, American Men’s Internet Survey 2014-2019 
# * * * Denom = hnp_np_2y_yc2

# * (Col 1) N ---------
# overall
nrow(hnm_np_2y_yc2)
table(hnm_np_2y_yc2$age_25.x)
table(is.na(hnm_np_2y_yc2$race_cat.x))
table(hnm_np_2y_yc2$race_cat.x)
table(is.na(hnm_np_2y_yc2$income_cat.x))
table(hnm_np_2y_yc2$income_cat.x)
table(is.na(hnm_np_2y_yc2$educ3.x))
table(hnm_np_2y_yc2$educ3.x)
table(hnm_np_2y_yc2$region4.x)
table(hnm_np_2y_yc2$nchs4.x)

# * (Col 2) Wilcoxon signed rank test stat and p-value --------
s4_test_all.1 <- hnm_np_2y_yc2$N_cai_sps_12.x
s4_test_all.2 <- hnm_np_2y_yc2$N_cai_sps_12.y

wilcox.test(s4_test_all.2, s4_test_all.1, paired=TRUE, 
            alternative = "greater", exact=F)

# age
s4_test_u25.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$age_25.x == 0,]$N_cai_sps_12.x
s4_test_u25.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$age_25.x == 0,]$N_cai_sps_12.y
wilcox.test(s4_test_u25.2, s4_test_u25.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_o25.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$age_25.x == 1,]$N_cai_sps_12.x
s4_test_o25.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$age_25.x == 1,]$N_cai_sps_12.y
wilcox.test(s4_test_o25.2, s4_test_o25.1, paired=TRUE, 
            alternative = "greater", exact=F)

# race
s4_test_b.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$race_cat.x == "NH_Black",]$N_cai_sps_12.x
s4_test_b.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$race_cat.x == "NH_Black",]$N_cai_sps_12.y
wilcox.test(s4_test_b.2, s4_test_b.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_h.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$race_cat.x == "Hispanic",]$N_cai_sps_12.x
s4_test_h.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$race_cat.x == "Hispanic",]$N_cai_sps_12.y
wilcox.test(s4_test_h.2, s4_test_h.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_w.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$race_cat.x == "NH_White",]$N_cai_sps_12.x
s4_test_w.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$race_cat.x == "NH_White",]$N_cai_sps_12.y
wilcox.test(s4_test_w.2, s4_test_w.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_o.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$race_cat.x == "NH_Other",]$N_cai_sps_12.x
s4_test_o.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$race_cat.x == "NH_Other",]$N_cai_sps_12.y
wilcox.test(s4_test_o.2, s4_test_o.1, paired=TRUE, 
            alternative = "greater", exact=F)

# inc
s4_test_inc1.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$income_cat.x == "1_<20k",]$N_cai_sps_12.x
s4_test_inc1.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$income_cat.x == "1_<20k",]$N_cai_sps_12.y
wilcox.test(s4_test_inc1.2, s4_test_inc1.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_inc2.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$income_cat.x == "2_<40k",]$N_cai_sps_12.x
s4_test_inc2.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$income_cat.x == "2_<40k",]$N_cai_sps_12.y
wilcox.test(s4_test_inc2.2, s4_test_inc2.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_inc3.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$income_cat.x == "3_<75k",]$N_cai_sps_12.x
s4_test_inc3.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$income_cat.x == "3_<75k",]$N_cai_sps_12.y
wilcox.test(s4_test_inc3.2, s4_test_inc3.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_inc4.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$income_cat.x == "4_75k+",]$N_cai_sps_12.x
s4_test_inc4.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$income_cat.x == "4_75k+",]$N_cai_sps_12.y
wilcox.test(s4_test_inc4.2, s4_test_inc4.1, paired=TRUE, 
            alternative = "greater", exact=F)

# education
s4_test_ed1.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$educ3.x == "1_HS-or-less",]$N_cai_sps_12.x
s4_test_ed1.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$educ3.x == "1_HS-or-less",]$N_cai_sps_12.y
wilcox.test(s4_test_ed1.2, s4_test_ed1.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_ed2.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$educ3.x == "2_ltCol",]$N_cai_sps_12.x
s4_test_ed2.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$educ3.x == "2_ltCol",]$N_cai_sps_12.y
wilcox.test(s4_test_ed2.2, s4_test_ed2.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_ed3.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$educ3.x == "3_Col+",]$N_cai_sps_12.x
s4_test_ed3.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$educ3.x == "3_Col+",]$N_cai_sps_12.y
wilcox.test(s4_test_ed3.2, s4_test_ed3.1, paired=TRUE, 
            alternative = "greater", exact=F)

# region
s4_test_reg1.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$region4.x == "1_NorE",]$N_cai_sps_12.x
s4_test_reg1.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$region4.x == "1_NorE",]$N_cai_sps_12.y
wilcox.test(s4_test_reg1.2, s4_test_reg1.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_reg2.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$region4.x == "2_MidW",]$N_cai_sps_12.x
s4_test_reg2.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$region4.x == "2_MidW",]$N_cai_sps_12.y
wilcox.test(s4_test_reg2.2, s4_test_reg2.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_reg3.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$region4.x == "3_South",]$N_cai_sps_12.x
s4_test_reg3.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$region4.x == "3_South",]$N_cai_sps_12.y
wilcox.test(s4_test_reg3.2, s4_test_reg3.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_reg4.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$region4.x == "4_West",]$N_cai_sps_12.x
s4_test_reg4.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$region4.x == "4_West",]$N_cai_sps_12.y
wilcox.test(s4_test_reg4.2, s4_test_reg4.1, paired=TRUE, 
            alternative = "greater", exact=F)

# density
s4_test_den1.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$nchs4.x == "1_LgCnt",]$N_cai_sps_12.x
s4_test_den1.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$nchs4.x == "1_LgCnt",]$N_cai_sps_12.y
wilcox.test(s4_test_den1.2, s4_test_den1.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_den2.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$nchs4.x == "2_LgFrg",]$N_cai_sps_12.x
s4_test_den2.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$nchs4.x == "2_LgFrg",]$N_cai_sps_12.y
wilcox.test(s4_test_den2.2, s4_test_den2.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_den3.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$nchs4.x == "3_Metro",]$N_cai_sps_12.x
s4_test_den3.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$nchs4.x == "3_Metro",]$N_cai_sps_12.y
wilcox.test(s4_test_den3.2, s4_test_den3.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_den4.1 <- hnm_np_2y_yc2[hnm_np_2y_yc2$nchs4.x == "4_Rural",]$N_cai_sps_12.x
s4_test_den4.2 <- hnm_np_2y_yc2[hnm_np_2y_yc2$nchs4.x == "4_Rural",]$N_cai_sps_12.y
wilcox.test(s4_test_den4.2, s4_test_den4.1, paired=TRUE, 
            alternative = "greater", exact=F)
