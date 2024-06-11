#### Script to eliminate 3rd and 4th appearances of individuals in AMIS -------

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
#library(snpar)
library(magrittr)
library(exact2x2)

# * define functions -----
tableNA <- function(x, ...){
   table(x, useNA = "ifany", ...)  
}

amis_study <- read.csv("amis_dataset_rr.csv")

# 1 NEW VARIABLES ------
# generate a `unique_id` variable
amis_study$unique_id <- as.numeric(paste(amis_study$respondent_id, amis_study$year, sep = ""))
head(amis_study$unique_id)
class(amis_study$unique_id)

# generate `link_YYYY` variables
amis_study_14 <- subset(amis_study, year == 2014)
amis_study_15 <- subset(amis_study, year == 2015)
amis_study_16 <- subset(amis_study, year == 2016)
amis_study_17 <- subset(amis_study, year == 2017)
amis_study_18 <- subset(amis_study, year == 2018)
amis_study_19 <- subset(amis_study, year == 2019)

#link variable for 2014 and 2015
amis_study_14$link_1415 <- amis_study_14$respondent_id
amis_study_15$link_1415 <- amis_study_15$lastid2014

#link variable for 2015 and 2016
amis_study_15$link_1516 <- amis_study_15$respondent_id
amis_study_16$link_1516 <- amis_study_16$lastid2015

#link variable for 2016 and 2017
amis_study_16$link_1617 <- amis_study_16$respondent_id
amis_study_17$link_1617 <- amis_study_17$lastid2016

#link variable for 2017 and 2018
amis_study_17$link_1718 <- amis_study_17$respondent_id
amis_study_18$link_1718 <- amis_study_18$lastid2017

#link variable for 2018 and 2019
amis_study_18$link_1819 <- amis_study_18$respondent_id
amis_study_19$link_1819 <- amis_study_19$lastid2018

# 2 MERGE ------
## merge "raw" dataset with 2 years of data linked (doesn't account for Rs with >2 years)
#2014, 2015
amis_study_1415 <- merge(x = amis_study_14, y = amis_study_15, by = "link_1415", all.x = F, all.y = F)

#2015, 2016
amis_study_1516 <- merge(x = amis_study_15, y = amis_study_16, by = "link_1516", all.x = F, all.y = F)

#2016, 2017
amis_study_1617 <- merge(x = amis_study_16, y = amis_study_17, by = "link_1617", all.x = F, all.y = F)

#2017, 2018
amis_study_1718 <- merge(x = amis_study_17, y = amis_study_18, by = "link_1718", all.x = F, all.y = F)

#2018, 2019
amis_study_1819 <- merge(x = amis_study_18, y = amis_study_19, by = "link_1819", all.x = F, all.y = F)

tableNA(amis_study_1415$lastid2014.y)

# 3 DUPLICATES -------
# identify duplicates in each of the 5, 2-year data sets
table(tableNA(amis_study_1415$link_1415)) # no duplicates
table(tableNA(amis_study_1516$link_1516)) # 4 duplicates
table(tableNA(amis_study_1617$link_1617)) # 14 duplicates
table(tableNA(amis_study_1718$link_1718)) # 5 duplicates
table(tableNA(amis_study_1819$link_1819)) # 5 duplicates

# 4 REMOVE -----
# remove the duplicates 
amis_study_1516 <- subset(amis_study_1516,
                          (link_1516 != 381 &
                           link_1516 != 1508 &
                           link_1516 != 25273 &
                           link_1516 != 40457))

amis_study_1617 <- subset(amis_study_1617,
                          (link_1617 != 8675 &
                           link_1617 != 11445 &
                           link_1617 != 25555))

amis_study_1718 <- subset(amis_study_1718,
                          (link_1718 != 231793 &
                           link_1718 != 268854 &
                           link_1718 != 276218 &
                           link_1718 != 276619 &
                           link_1718 != 291244))

amis_study_1819 <- subset(amis_study_1819,
                          (link_1819 != 40245 &
                           link_1819 != 43269 &
                           link_1819 != 47262 &
                           link_1819 != 47958 &
                           link_1819 != 75913))

# 5 IDENTIFY ----
# identify the >2 year participants

# 2014, 2015, 2016
amis_study_1415$link_141516 <- amis_study_1415$respondent_id.y
amis_study_16$link_141516 <- amis_study_16$lastid2015

amis_study_141516 <- merge(x = amis_study_1415, y = amis_study_16, by = "link_141516", all.x = F, all.y = F)

# 2014, 2015, 2016, 2017
amis_study_141516$link_14151617 <- amis_study_141516$respondent_id.y
amis_study_17$link_14151617 <- amis_study_17$lastid2016

amis_study_14151617 <- merge(x = amis_study_141516, y = amis_study_17, by = "link_14151617", all.x = F, all.y = F)
# tableNA(amis_study_14151617$link_14151617)

# 2015, 2016, 2017
amis_study_1516$link_151617 <- amis_study_1516$respondent_id.y
amis_study_17$link_151617 <- amis_study_17$lastid2016

amis_study_151617 <- merge(x = amis_study_1516, y = amis_study_17, by = "link_151617", all.x = F, all.y = F)

# 2016, 2017, 2018
amis_study_1617$link_161718 <- amis_study_1617$respondent_id.y
amis_study_18$link_161718 <- amis_study_18$lastid2017

amis_study_161718 <- merge(x = amis_study_1617, y = amis_study_18, by = "link_161718", all.x = F, all.y = F)

# 2017, 2018, 2019
amis_study_1718$link_171819 <- amis_study_1718$respondent_id.y
amis_study_19$link_171819 <- amis_study_19$lastid2018

amis_study_171819 <- merge(x = amis_study_1718, y = amis_study_19, by = "link_171819", all.x = F, all.y = F)

# 6 GET IDS ------
# get observation IDs for year 2/3 for 3-year Rs.
tableNA(amis_study_171819$unique_id.y, amis_study_171819$year.y)

rmv_15 <- amis_study_141516$unique_id.y
rmv_16 <- amis_study_151617$unique_id.y
rmv_17 <- amis_study_161718$unique_id.y
rmv_18 <- amis_study_171819$unique_id.y


# 7 REMOVE -----
# Remove the >2-year Rs from the second year in which they appear
amis_study_1415_new <- amis_study_1415
amis_study_1516_new <- subset(amis_study_1516, !(unique_id.x %in% rmv_15))
amis_study_1617_new <- subset(amis_study_1617, !(unique_id.x %in% rmv_16))
amis_study_1718_new <- subset(amis_study_1718, !(unique_id.x %in% rmv_17))
amis_study_1819_new <- subset(amis_study_1819, !(unique_id.x %in% rmv_18))

sum(nrow(amis_study_1415) - nrow(amis_study_1415_new))
sum(nrow(amis_study_1516) - nrow(amis_study_1516_new))
sum(nrow(amis_study_1617) - nrow(amis_study_1617_new))
sum(nrow(amis_study_1718) - nrow(amis_study_1718_new))
sum(nrow(amis_study_1819) - nrow(amis_study_1819_new))


sum(nrow(amis_study_1415_new),
    nrow(amis_study_1516_new),
    nrow(amis_study_1617_new),
    nrow(amis_study_1718_new),
    nrow(amis_study_1819_new))

amis_study_rr <- bind_rows(amis_study_1415_new,
                           amis_study_1516_new,
                           amis_study_1617_new,
                           amis_study_1718_new,
                           amis_study_1819_new)

# NEW N (ALL Rs): 2836 ------
write.csv(amis_study_rr, "amis_2yr_rr.csv")

##### GENERATE THE ONE-YEAR ONLY DATASET (Table S1)------
two_14 <- amis_study_1415_new$respondent_id.x
two_15 <- amis_study_1516_new$respondent_id.x
two_16 <- amis_study_1617_new$respondent_id.x
two_17 <- amis_study_1718_new$respondent_id.x
two_18 <- amis_study_1819_new$respondent_id.x

amis_1_year_14 <- subset(amis_study_14, !(respondent_id %in% two_14))
amis_1_year_15 <- subset(amis_study_15, !(respondent_id %in% two_15))
amis_1_year_16 <- subset(amis_study_16, !(respondent_id %in% two_16))
amis_1_year_17 <- subset(amis_study_17, !(respondent_id %in% two_17))
amis_1_year_18 <- subset(amis_study_18, !(respondent_id %in% two_18))

amis_1_year_1418 <- bind_rows(amis_1_year_14,
                              amis_1_year_15,
                              amis_1_year_16,
                              amis_1_year_17,
                              amis_1_year_18)

write.csv(amis_1_year_1418, "amis_1yr_rr.csv")

##### GENERATE THE ONE-YEAR DATASET + 2019 (TABLE S2)-------------
write.csv(amis_study, "amis_all_rr.csv")
