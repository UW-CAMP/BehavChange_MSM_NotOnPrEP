### SHMS Analyses -----
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
#library(snpar)
library(magrittr)


# * define functions -----
tableNA <- function(x, ...){
  table(x, useNA = "ifany", ...)  
}

# * select necessary data -----
# See README for instructions on obtaining data. 
# Insert name of data file into code here.
amis_raw <- read.csv("data/CAMP_Goodreau_2013-20_noFormat.csv")

#make variables lowercase
names(amis_raw) <- tolower(names(amis_raw))


# * generate final list of variable names -----
final_varlist <- c(
   "year", #survey year
   "pid", #participant ID
   "id_2013", #participant's ID from last year
   "id_2014",
   "id_2015",
   "id_2016",
   "id_2017",
   "id_2018",
   "id_2019",
   "region", #census region
   "fips", # FIPS code
   "nchs", #NCHS urban-rural
   "race_cat", #race categorized
   "race_aian", #Amer Ind./AK Nat.
   "race_asian", #Asian
   "race_blaa", #Black & Af Amer
   "race_hisp", #Hispanic
   "race_nhpi", #Nat. HI/Pac. Isl.
   "race_white", #white
   "educ_cat", #education categorized
   "income_cat", #income categorized
   "ins_cat", #insurance categorized
   "age", #age continuous
   "sex_id", #sexual identity
   "cai_12", #any condomless anal sex p12
   "cai_discord_12", #any CAI with a partner of opposite HIV stat
   "sextype_o_12", #had oral sex p12
   "sextype_a_12", #had anal sex p12
   "sextype_rim_12", #rimming p12
   "sextype_oth_12", #had some other type of sex p12
   "sextype_ref_12", #refused sex types p12
   "sextype_dk_12", #dk what types of sex p12
   "sextype_spec_12", #specified other sex types p12
   "top_12", #R topped p12
   "bottom_12", #R bottomed p12
   "unique_sps_any_12", #how many unique sex partners (of any type) p12
   "unique_sps_anal_12", #how many unique sex partners had anal sex with p12
   "unique_sps_oral_12", #how many unique sex partners had oral sex with p12
   "unique_sps_12_N_anal", #among unique SPs, how mamy were anal SPs p12
   "N_cai_sps_12", #how many people had CAI with p12
   "o1sp_12_type", #if 1 SP p12, what type of partner?
   "o1sp_sexwithothers", #if 1 SP p12, do you think he was having sex with others?
   "o1sp_know_status", #if 1 SP p12, do you know his HIV status?
   "o1sp_hiv_status", #if 1 SP p12 HIV stat known, what was his HIV status?
   "o1sp_prep", #if 1 SP p12, on PrEP?
   "o1sp_art", #if 1 SP p12, on ART?
   "multsp_12_type", #if multiple SPs p12, what types?
   "cai_hivunk_12", #CAI with men of unk HIV stat, p12
   "cai_hivpos_12", #CAI with men of positive HIV stat, p12
   "cai_hivneg_12", #CAI with men of negative HIV stat, p12
   "hiv_status", #R HIV status
   "hivtest_12_bin", #tested in the last 12 mo?
   "mo_last_test", #months since R's last test
   "mo_last_pos", #months since R's last positive test
   "evertest", #binary - ever been tested for HIV?
   "N_test_2yr", #number of HIV tests past 2 years
   "art_current", #R currently using ART?
   "art_no_why", #Why not using ART?
   "prep_aware", #R is aware of PrEP (distinct from "heard of" for our purposes?)
   "prep_used12", #R used PrEP p12
   "prep_willing", #R is willing to use PrEP
   "prep_elig", #R meets CDC eligibility
   "prep_used_ever", #R ever used PrEP
   "prep_current", #R currently uses PrEP
   "prep_doses30", #How many PrEP doses taken past 30 days
   "prep_months", #how many consec months used PrEP?
   "drug_ni_12", #R used non-inj drugs p12
   "meth_ni_12", #R used non-inj meth p12
   "meth_ni_12_freq", #frequency of meth use p12
   "race_ref",
   "race_notapply",
   "race_dk",
   "lsp_type", #last sex partner type
   "lsp_lse_month", #lsp month of last encounter
   "lsp_lse_year", #lsp year of last encounter
   "lsp_btm", #R bottomed with lsp
   "lsp_btm_condom_any", #R used condom when bottoming with lsp
   "lsp_btm_condom_entire", #R used condom throughout LSE when bottoming
   "lsp_top", #R topped with lsp
   "lsp_top_condom_any", #R used condom with lsp when topping
   "lsp_top_condom_entire", #R used condom throughout LSE when topping
   "lsp_know_status", #R knows lsp's HIV stat
   "lsp_hiv_status", #lsp's HIV stat
   "lsp_age", #lsp age
   "lsp_age_older", #lsp is older
   "lsp_age_younger", #lsp is younger
   "lsp_age_same",
   "lsp_age_ref",
   "lsp_age_dk",
   "lsp_race_aian",
   "lsp_race_asian",
   "lsp_race_blaa",
   "lsp_race_hisp",
   "lsp_race_nhpi",
   "lsp_race_white",
   "lsp_race_ref",
   "lsp_race_dk",
   "lsp_n_enc", #number of encounters with lsp (one-nigh vs. multiple)
   "lsp_length", #number for length of sexual relationship with lsp
   "lsp_length_unit", #unit for length of sexual relationship with lsp
   "lsp_sexagain", #do you expect to have sex again?
   "lsp_hehadswothers", #how likely was LSP having sex with others
   "lsp_ihadsexothers", #during your relationship with lsp, did you have sex with others? 
   "lsp_any_drugs", #did you use any substances before/during sex with lsp?
   "lsp_him_ever_meth", #as far as you know, has lsp ever used meth?
   "lsp_i_did_meth" #if used substances bef/dur sex with lsp, was it meth?
)

# Big Picture: Does the % of NOPs who've had CAI [any? with unk HIV stat?] .. go up year by year? -------


### Restrict to... -----
### * those with present and feasible age -----
amis_raw$age <- as.integer(amis_raw$age)
unique(amis_raw$age) # unreasonable ages: 211, 425, 199, 611, 999

amis_clean_a <- amis_raw[amis_raw$age <= 100,] #dropped 5 observations

### * those who live in the 50 states ----
table(amis_clean_a$x_nchs_rural2013, useNA = "always")
table(amis_clean_a$x_censdiv, useNA = "always") # 10 = territories; drop 10 & NA

amis_clean_b <- drop_na(amis_clean_a, x_censdiv) #dropped 155 obs
amis_clean_c <- amis_clean_b[amis_clean_b$x_censdiv != "10",] #dropped 92 obs

### * those who gave a definitive answer to "have you ever been tested for HIV?" -----
table(amis_clean_c$tested_ever, useNA = "always")

amis_clean_d <- drop_na(amis_clean_c, tested_ever) #dropped 18
amis_clean_e <- amis_clean_d[amis_clean_d$tested_ever == 0 | amis_clean_d$tested_ever == 1,] #dropped 64

### * remove 2020 responses -----
amis_clean_f <- amis_clean_e[amis_clean_e$year != "2020",] #dropped 23,046

### * remove 2 people with wacky SP numbers and partner types
amis_clean_g <- amis_clean_f %>% slice(-c(5844, 12592))


# final analytic sample
amis <- amis_clean_g # N = 69,861

amis13 <- subset(amis, year == 2013)
amis14 <- subset(amis, year == 2014)
amis15 <- subset(amis, year == 2015)
amis16 <- subset(amis, year == 2016)
amis17 <- subset(amis, year == 2017)
amis18 <- subset(amis, year == 2018)
amis19 <- subset(amis, year == 2019)


# * hiv3 -----
table(amis$hiv_status, amis$year, useNA = "ifany")

amis$hiv3 <- factor(amis$hiv_status,
                    levels = c(1, 2, 9),
                    labels = c("01_pos",
                               "02_neg",
                               "03_unk"))

table(amis$hiv3, amis$hiv_status, useNA = "ifany")

# * hiv2 -----
amis$hiv2[amis$hiv3 == "01_pos"] <- "01_pos"
amis$hiv2[amis$hiv3 == "02_neg" | amis$hiv3 == "03_unk"] <- "02_neg"

tableNA(amis$hiv2, amis$hiv3)


# names for new PrEP variables -----
amis$prep_aware_new <- NA
amis$prep_use12_new <- NA
amis$prep_usecur_new <- NA
amis$prep_useever_new <- NA

amis13 <- subset(amis, year == 2013)
amis14 <- subset(amis, year == 2014)
amis15 <- subset(amis, year == 2015)
amis16 <- subset(amis, year == 2016)
amis17 <- subset(amis, year == 2017)
amis18 <- subset(amis, year == 2018)
amis19 <- subset(amis, year == 2019)

### PrEP variables for later use in Biomed variables -----
# 2013 dataset ---- 
# * (1) prep_aware_new -----

tableNA(amis13$heard_prep)

amis13$prep_aware_new[amis13$heard_prep == 1] <- 1
amis13$prep_aware_new[amis13$heard_prep == 0] <- 0

tableNA(amis13$heard_prep, amis13$prep_aware_new) #check, using calc var 

# * (2) prep_use12_new -----
tableNA(amis13$used_prep)

amis13$prep_use12_new <- amis13$used_prep

tableNA(amis13$prep_use12_new)

# * (3) prep_usecur_new ----
# * * In 2013, we assumed anyone who used PrEP p12 was using it now. 
amis13$prep_usecur_new <- amis13$prep_use12_new

# * (4) prep_useever_new -----
# * * In 2013, we assumed anyone who used PrEP p12 are those who ever used it. 
amis13$prep_useever_new <- amis13$prep_use12_new

# 2014 dataset -----
tableNA(amis14$antrprev1, amis14$prep1)

amis14_preprandom <- subset(amis14, (is.na(prep1) & hiv2 == "02_neg"))

amis14 <- amis14[!is.na(amis14$prep1) | amis14$hiv2 == "01_pos",] #remove observations who were randomized to the wrong module (also drops 24 who never answered)
tableNA(amis14$prep1, amis14$hiv2) #check - only NAs are HIV-pos.
tableNA(amis14$antrprev1) #check

# * (1) prep_aware_new -----
tableNA(amis14$prep1) #no missingness
amis14$prep_aware_new <- amis14$prep1 #apply to new variable

amis14$prep_aware_new[amis14$hiv2 == "01_pos"] <- 0
tableNA(amis14$prep_aware_new)

# * (2) prep_use12_new -----
tableNA(amis14$prep4) #"have you taken PrEP, p12 mo?"

amis14$prep_use12_new[amis14$prep4 == 1] <- 1
amis14$prep_use12_new[amis14$prep4 == 0] <- 0

round(prop.table(tableNA(amis14$prep_use12_new))*100,1) #~4100 (90%) NA

# * * apply informative missingness -----
# * * * if you were unaware of PrEP, you have not taken it in the past 12 mo. ----
amis14$prep_use12_new[amis14$prep_aware_new == 0] <- 0
round(prop.table(tableNA(amis14$prep_use12_new))*100,1) #~2500 (54%) NA

# * * compare to calculated variable -----
tableNA(amis14$used_prep, amis14$prep_use12_new) # * * all of the NAs become "no" in the calculated variable; use calc var. -----
amis14$prep_use12_new <- amis14$used_prep 

### >>>Cannot use `prep2` or `prep3` variables to apply logic, as they are absent from the dataset. #####

# * (3) prep_usecur_new -----
# * * In 2014, we assumed past 12 months use = current use. -----
amis14$prep_usecur_new <- amis14$prep_use12_new

# * (4) prep_useever_new -----
# * * In 2014, we assumed past 12 months use = ever use. -----
amis14$prep_useever_new <- amis14$prep_use12_new

# 2015 dataset -----
# * (1) prep_aware_new -----
tableNA(amis15$heard_prep) # calculated variable has no missingness
amis15$prep_aware_new <- amis15$heard_prep

# * (2) prep_use12_new -----
tableNA(amis15$prep_revised, amis15$used_prep) #no missingness in calc'd var.

amis15$prep_use12_new <- amis15$used_prep

### >>>Cannot use intermediary variables `prep_discussed` and `prep_prescribed` to apply logic

# * (3) prep_usecur_new -----
# * * In 2015, we assumed past 12 months use = current use. -----
amis15$prep_usecur_new <- amis15$prep_use12_new

# * (4) prep_useever_new -----
# * * In 2015, we assumed past 12 months use = ever use. -----
amis15$prep_useever_new <- amis15$prep_use12_new

# 2016 dataset -----
# * (1) prep_aware_new -----
tableNA(amis16$antrprev1,amis16$heard_prep) #no missingness in calc var; use calc var.

amis16$prep_aware_new <- amis16$heard_prep

# * (2) prep_use12_new -----
tableNA(amis16$used_prep) #no missingness in calc var

amis16$prep_use12_new <- amis16$used_prep

# * (3) prep_usecur_new -----
amis16$prep_usecur_new[amis16$prep_current == 1] <- 1
amis16$prep_usecur_new[amis16$prep_current == 0] <- 0

round(prop.table(tableNA(amis16$prep_usecur_new))*100,1) #~9,400 (93%) NA

# * * apply informative missingness -----
# * * * if you were unaware of PrEP, you can't be currently using it. -----
amis16$prep_usecur_new[amis16$prep_aware_new == 0] <- 0

round(prop.table(tableNA(amis16$prep_usecur_new))*100,1) #~5,700 (56%) NA

# * * * if you haven't used PrEP in the last 12 mo, you can't currently be using it. -----
amis16$prep_usecur_new[amis16$prep_use12_new == 0] <- 0

round(prop.table(tableNA(amis16$prep_usecur_new))*100,1) #41 (<1%) NA

# * (4) prep_useever_new -----
amis16$prep_useever_new <- amis16$prep_revised

round(prop.table(tableNA(amis16$prep_useever_new))*100,1) #~9,300 (92%) NA
# * * apply informative missingness -----
# * * * if you were unaware of PrEP, you can't have ever taken it.
amis16$prep_useever_new[amis16$prep_aware_new == 0] <- 0
round(prop.table(tableNA(amis16$prep_useever_new))*100,1) #~5,600 (56%) NA

### >>>Cannot use intermediary variables `prep_discussed` and `prep_prescribed` to apply logic

# 2017 dataset -----
# * (1) prep_aware_new -----
amis17$prep_aware_new <- amis17$heard_prep
tableNA(amis17$prep_aware_new)

# * (2) prep_use12_new -----
amis17$prep_use12_new <- amis17$used_prep

# * (3) prep_usecur_new -----
amis17$prep_usecur_new[amis17$prep_current == 1] <- 1
amis17$prep_usecur_new[amis17$prep_current == 0] <- 0

round(prop.table(tableNA(amis17$prep_usecur_new))*100,1) #~8,900 (87%) NA

# * * apply informative missingness -----
# * * * if you were unaware of PrEP, you can't be currently taking it.
amis17$prep_usecur_new[amis17$prep_aware_new == 0] <- 0

round(prop.table(tableNA(amis17$prep_usecur_new))*100,1) #~5,400 (54%) NA

# * * * if you haven't used PrEP in the last 12 mo, you can't be taking it now.
amis17$prep_usecur_new[amis17$prep_use12_new == 0] <- 0

round(prop.table(tableNA(amis17$prep_usecur_new))*100,1) #6 (<1%) NA

# * (4) prep_useever_new -----
# * * apply informative missingness -----
# * * * if you were unaware of PrEP, you can't have ever taken it. -----
amis17$prep_useever_new[amis17$prep_aware_new == 0] <- 0

# * * * if you use PrEP now, you have ever used PrEP. -----
amis17$prep_useever_new[amis17$prep_usecur_new == 1] <- 1

# * * * if you used PrEP in the last 12 months, you have ever used PrEP. -----
amis17$prep_useever_new[amis17$prep_useever_new == 1] <- 1

round(prop.table(tableNA(amis17$prep_useever_new))*100,1) #~5,500 (56%) NA

# 2018 dataset -----
# * (1) prep_aware_new -----
amis18$prep_aware_new <- amis18$heard_prep

# * (2) prep_use12_new -----
amis18$prep_use12_new <- amis18$used_prep
amis18$prep_use12_new[amis18$prep_used == 0] <- 0

# * (3) prep_usecur_new -----
tableNA(amis18$prep_used)

amis18$prep_usecur_new[amis18$prep_current == 1] <- 1
amis18$prep_usecur_new[amis18$prep_current == 0] <- 0

round(prop.table(tableNA(amis18$prep_usecur_new))*100,1) #~2,600 (26%) NA

# * * apply informative missingness -----
# * * * if you didn't use PrEP p12, you can't be using it now. -----
amis18$prep_usecur_new[amis18$prep_use12_new == 0] <- 0
round(prop.table(tableNA(amis18$prep_usecur_new))*100,1) #<1% missing!

# * (4) prep_useever_new -----
tableNA(amis18$prep_ever)

amis18$prep_useever_new[amis18$prep_ever == 1] <- 1
amis18$prep_useever_new[amis18$prep_ever == 0] <- 0

round(prop.table(tableNA(amis18$prep_useever_new))*100,1) # ~5,900 (58%) NA

# * * apply informative missingness -----
# * * * if you weren't aware of PrEP, you can't have ever used it. -----
amis18$prep_useever_new[amis18$prep_aware_new == 0] <- 0

round(prop.table(tableNA(amis18$prep_useever_new))*100,1) # ~3,700 (36%) NA

# * * * if you've used PrEP in the last 12 months, you have ever used PrEP. -----
amis18$prep_useever_new[amis18$prep_use12_new == 1] <- 1

round(prop.table(tableNA(amis18$prep_useever_new))*100,1) # ~2,400 (23%) NA

### >>>Cannot use `prep_discussed` variablesto apply logic, as it is are absent from the dataset. #####

# 2019 dataset -----
# * (1) prep_aware_new -----
amis19$prep_aware_new <- amis19$heard_prep
tableNA(amis19$prep_aware_new)

# * (2) prep_used12_new -----
amis19$prep_use12_new <- amis19$used_prep
amis19$prep_use12_new[amis19$prep_used == 0] <- 0

# * (3) prep_usecur_new -----
tableNA(amis19$prep_current)

amis19$prep_usecur_new[amis19$prep_current == 0] <- 0
amis19$prep_usecur_new[amis19$prep_current == 1] <- 1

round(prop.table(tableNA(amis19$prep_usecur_new))*100,1) # ~8,700 (86%) NA

# * * apply informative missingness -----
# * * * if you were not aware of PrEP, you can't be using it now. -----
amis19$prep_usecur_new[amis19$prep_aware_new == 0] <- 0 # ~6,500 (64%) NA

# * * * if you didn't use PrEP in the last 12 mo, you can't be using it now. -----
amis19$prep_usecur_new[amis19$prep_use12_new == 0] <- 0 # ~11 (<1%) NA

# * (4) prep_useever_new -----
tableNA(amis19$prep_ever)

amis19$prep_useever_new[amis19$prep_ever == 0] <- 0
amis19$prep_useever_new[amis19$prep_ever == 1] <- 1

round(prop.table(tableNA(amis19$prep_useever_new))*100,1) # ~3,600 (36%) NA

# * * apply informative missingness -----
# * * * if you weren't aware of PrEP, you can't have ever used it. ----
amis19$prep_useever_new[amis19$prep_aware_new == 0] <- 0 # ~1,400 (14%) NA

# * * * if you used PrEP in the last 12 mo, you must have ever used it. ----
amis19$prep_useever_new[amis19$prep_use12_new == 1] <- 1 # 31 (<1%) NA

# * * * if you are using PrEP now, you must have ever used it. -----
amis19$prep_useever_new[amis19$prep_usecur_new == 1] <- 1 # 31 (<1%) NA

amis <- rbind(amis13,
              amis14,
              amis15,
              amis16,
              amis17,
              amis18,
              amis19)

# N = 65795 -----


### Clean up other variables for analysis -----

# * pid -----
amis$pid <- amis$respondent_id

# * year ----
table(amis$year)

# * region -----
amis$region <- NA
amis$region[amis$x_censdiv == 1] <- "1_NewEng"
amis$region[amis$x_censdiv == 2] <- "2_MidAtl"
amis$region[amis$x_censdiv == 3] <- "3_ENCent"
amis$region[amis$x_censdiv == 4] <- "4_WNCent"
amis$region[amis$x_censdiv == 5] <- "5_SouAtl"
amis$region[amis$x_censdiv == 6] <- "6_ESCent"
amis$region[amis$x_censdiv == 7] <- "7_WSCent"
amis$region[amis$x_censdiv == 8] <- "8_Mntn"
amis$region[amis$x_censdiv == 9] <- "9_Pac"
table(amis$x_censdiv, amis$region, useNA = "ifany") #check

# * region4 -----
amis$region4[amis$x_censdiv == 1 | amis$x_censdiv == 2] <- "1_NorE"
amis$region4[amis$x_censdiv == 3 | amis$x_censdiv == 4] <- "2_MidW"
amis$region4[amis$x_censdiv == 5 | amis$x_censdiv == 6 | amis$x_censdiv == 7] <- "3_South"
amis$region4[amis$x_censdiv == 8 | amis$x_censdiv == 9] <- "4_West"
tableNA(amis$region, amis$region4)


# * fips -----
tableNA(amis$city_c_zip)

amis$fips <- factor(amis$city_c_zip,
                    levels = c(1,
                               520,
                               720,
                               1123,
                               1600,
                               1920,
                               2080,
                               2160,
                               3360,
                               4480,
                               4920,
                               5000,
                               5380,
                               5560,
                               5600,
                               5640,
                               5720,
                               6160,
                               6440,
                               7320,
                               7360,
                               7440,
                               7600,
                               8840),
                    labels = c("00_unk",
                               "01_Atl",
                               "02_Balt",
                               "03_Bos",
                               "04_Chic",
                               "05_Dal",
                               "06_Den",
                               "07_Detr",
                               "08_Hous",
                               "09_LA",
                               "10_Memp",
                               "11_Mia",
                               "12_LI",
                               "13_NewOr",
                               "14_NYC",
                               "15_NwkNJ",
                               "16_VaBch",
                               "17_Phil",
                               "18_Ptld",
                               "19_SanD",
                               "20_SF",
                               "21_SJPR",
                               "22_Stl",
                               "23_DC"))

tableNA(amis$city_c_zip, amis$fips)

# * nchs -----
amis$nchs <- NA
amis$nchs[amis$x_nchs_rural2013 == 1] <- "1_LgCnt"
amis$nchs[amis$x_nchs_rural2013 == 2] <- "2_LgFrg"
amis$nchs[amis$x_nchs_rural2013 == 3] <- "3_MdMet"
amis$nchs[amis$x_nchs_rural2013 == 4] <- "4_SmMet"
amis$nchs[amis$x_nchs_rural2013 == 5] <- "5_Micro"
amis$nchs[amis$x_nchs_rural2013 == 6] <- "6_Noncore"
table(amis$nchs, amis$x_nchs_rural2013) #check

# * nchs4 -----
amis$nchs4[amis$nchs == "1_LgCnt"] <- "1_LgCnt"
amis$nchs4[amis$nchs == "2_LgFrg"] <- "2_LgFrg"
amis$nchs4[amis$nchs == "3_MdMet" | amis$nchs == "4_SmMet"] <- "3_Metro"
amis$nchs4[amis$nchs == "5_Micro" | amis$nchs == "6_Noncore"] <- "4_Rural"

# * race_aian -----
amis$race_aian <- NA
amis$race_aian[amis$racea == 1] <- 1
table(amis$race_aian, amis$racea, useNA = "always") #check

# * race_asian -----
amis$race_asian <- NA
amis$race_asian[amis$raceb == 1] <- 1
table(amis$race_asian, amis$raceb, useNA = "always") #check

# * race_blaa -----
amis$race_blaa <- NA
amis$race_blaa[amis$racec == 1] <- 1
table(amis$race_blaa, amis$racec, useNA = "always") #check

# * race_hisp -----
amis$race_hisp <- NA
amis$race_hisp[amis$hispanic == 1] <- 1
table(amis$race_hisp, amis$hispanic, useNA = "always") #check

# * race_nhpi -----
amis$race_nhpi <- NA
amis$race_nhpi[amis$raced == 1] <- 1
table(amis$race_nhpi, amis$raced, useNA = "always") #check

# * race_white -----
amis$race_white <- NA
amis$race_white[amis$racee == 1] <- 1
table(amis$race_white, amis$racee, useNA = "always") #check

# * race_cat -----
amis$race_cat <- NA

# yes" to hispanic becomes hispanic
amis$race_cat[amis$hispanic == 1] <- "Hispanic"

# # "PTNA" or "IDK" to hispanic becomes NA
# amis$race_cat[amis$hispanic == 7 | amis$hispanic == 9] <- NA #was "N.A." for audit

# no to hispanic and yes to Black becomes "NH_Black"
amis$race_cat[amis$hispanic == 0 & amis$racec == 1] <- "NH_Black"

# # no to hispanic and "PTNA" or "IDK" or "Does not apply" to race becomes NA
# amis$race_cat[amis$hispanic == 0 & (amis$racef == 1 | amis$raceg == 1 | amis$raceh == 1) & amis$racec != 1] <- NA #was "N.A." for audit

# no to hisp & yes to Asian, AI/AN, or NHPI becomes "NH_Other"
amis$race_cat[amis$hispanic == 0 & (amis$racea == 1 | amis$raceb == 1 | amis$raced == 1) & amis$racec != 1] <- "NH_Other"

# no to all others, yes to White becomes "NH_White"
amis$race_cat[amis$hispanic == 0 & amis$racea != 1 & amis$raceb != 1 & amis$racec != 1 & amis$raced != 1 & amis$racee == 1] <- "NH_White"

# # all others become NA
# amis$race_cat[amis$hispanic == 0 & amis$racea != 1 & amis$raceb != 1 & amis$racec != 1 & amis$raced != 1 & amis$racee != 1] <- NA #was "N.A." for audit

tableNA(amis$race_cat) 
# 55 were not assigned to "N.A."

round(prop.table(tableNA(amis$race_cat))*100,2)

# * educ_cat -----
amis$educ_cat <- NA
amis$educ_cat <- factor(amis$x_educat,
                           levels = 1:4,
                           labels = c("1_ltHS",
                                      "2_HS",
                                      "3_ltCol",
                                      "4_Col+"))
table(amis$educ_cat, amis$x_educat) #check

# * educ3 -----
amis$educ3 <- NA
amis$educ3[amis$educ_cat == "1_ltHS" | amis$educ_cat == "2_HS"] <- "1_HS-or-less"
amis$educ3[amis$educ_cat == "3_ltCol"] <- "2_ltCol"
amis$educ3[amis$educ_cat == "4_Col+"] <- "3_Col+"

tableNA(amis$educ_cat, amis$educ3)

# * income_cat -----
amis$income_cat <- NA
amis$income_cat <- factor(amis$income,
                          levels = 1:4,
                          labels = c("1_<20k",
                                     "2_<40k",
                                     "3_<75k",
                                     "4_75k+"))
table(amis$income_cat, amis$income) #check

# * age ----
hist(amis$age, breaks = 50)

# * age_25 -----
amis$age_25 <- ifelse(amis$age >= 25, 1, 0)

tableNA(amis$age, amis$age_25)

# * age_3_level -----
amis$age_3_level <- NA
amis$age_3_level[amis$age <= 19] <- "1_15-19"
amis$age_3_level[amis$age >= 20 & amis$age <= 24] <- "2_20-24"
amis$age_3_level[amis$age >= 25] <- "3_25+"

# * age_5_level -----
amis$age_5_level <- NA
amis$age_5_level[amis$age <= 19] <- "1_15-19"
amis$age_5_level[amis$age >= 20 & amis$age <= 24] <- "2_20-24"
amis$age_5_level[amis$age >= 25 & amis$age <= 29] <- "3_25-29"
amis$age_5_level[amis$age >= 30 & amis$age <= 39] <- "4_30-39"
amis$age_5_level[amis$age >= 40] <- "5_40+"

# * race_age_cat -----
amis$race_age_cat <- NA
amis$race_age_cat[amis$age_25 == 1 & amis$race_cat == "NH_White"] <- "01_White_Old"
amis$race_age_cat[amis$age_25 == 1 & amis$race_cat == "NH_Black"] <- "02_Black_Old"
amis$race_age_cat[amis$age_25 == 1 & amis$race_cat == "Hispanic"] <- "03_Hisp_Old"
amis$race_age_cat[amis$age_25 == 1 & amis$race_cat == "NH_Other"] <- "04_Other_Old"
amis$race_age_cat[amis$age_25 == 0 & amis$race_cat == "NH_White"] <- "05_White_Young"
amis$race_age_cat[amis$age_25 == 0 & amis$race_cat == "NH_Black"] <- "06_Black_Young"
amis$race_age_cat[amis$age_25 == 0 & amis$race_cat == "Hispanic"] <- "07_Hisp_Young"
amis$race_age_cat[amis$age_25 == 0 & amis$race_cat == "NH_Other"] <- "08_Other_Young"

tableNA(amis$race_age_cat, amis$race_cat)
tableNA(amis$race_age_cat, amis$age_25)



# * sex_id -----
table(amis$identity, useNA = "always")
table(amis$identity_screen, useNA = "always")
table(amis$identity, amis$identity_screen, useNA = "always")
table(amis$year, amis$identity, useNA = "always")
table(amis$year, amis$identity_screen, useNA = "always")

amis$sex_id <- NA
amis$sex_id <- factor(amis$identity,
                      levels = 1:4,
                      labels = c("3_het",
                                 "1_gay",
                                 "2_bi",
                                 "4_other"))

amis$sex_id[amis$year == 2013 & amis$identity == 1] <- "1_gay"
amis$sex_id[amis$year == 2013 & amis$identity == 2] <- "3_het"
amis$sex_id[amis$year == 2013 & amis$identity == 3] <- "2_bi"

table(amis$sex_id, amis$identity) #check

amis$sex_id[amis$identity_screen == 1] <- "1_gay"
amis$sex_id[amis$identity_screen == 3] <- "2_bi"

table(amis$sex_id, amis$identity_screen) #check

# * sextype_o_12 -----
amis$sextype_o_12 <- NA

table(amis$m_mp12type, amis$year, useNA = "ifany") #check for NAs in variables
table(amis$msmp12m_oral, amis$year, useNA = "ifany") #check for NAs in variables

amis$sextype_o_12[(amis$m_mp12type == 1 | amis$m_mp12type == 3) & amis$year == "2014"] <- 1
amis$sextype_o_12 <- ifelse((amis$m_mp12type == 1 | amis$m_mp12type == 3) & amis$year == "2014", 1, 0)
amis$sextype_o_12[amis$msmp12m_oral == 1] <- 1

amis$sextype_o_12[amis$year == 2013] <- NA
table(amis$sextype_o_12, amis$msmp12m_oral) #check for 2015-2019
table(amis$sextype_o_12, amis$m_mp12type, amis$year) #check for 2014

table(amis$sextype_o_12, amis$year, useNA = "ifany")

# * sextype_a_12 -----
amis$sextype_a_12 <- NA

table(amis$msmp12m_anal, amis$year, useNA = "ifany") #check for NAs in variables

amis$sextype_a_12[amis$year == 2013] <- NA
amis$sextype_a_12 <- ifelse((amis$m_mp12type == 2 | amis$m_mp12type == 3) & amis$year == "2014", 1, 0)
amis$sextype_a_12[amis$msmp12m_anal == 1] <- 1

table(amis$sextype_a_12, amis$year, useNA = "ifany")

# * sextype_rim_12 -----
amis$sextype_rim_12 <- NA

table(amis$msmp12m_rim, amis$year, useNA = "ifany") #2018,2019 only; no NA

amis$sextype_rim_12 <- amis$msmp12m_rim

table(amis$sextype_rim_12, amis$year) #check

# NOTE: are we interested in "other" "refuse" and/or "dk" sex type responses? -----

# * top_12 -----
amis$top_12 <- NA

table(amis$msmp12mpos_ias, amis$year, useNA = "ifany")

amis$top_12 <- amis$msmp12mpos_ias

table(amis$top_12, amis$year)

# * bottom_12 -----
amis$bottom_12 <- NA

table(amis$msmp12mpos_ras, amis$year, useNA = "ifany")

amis$bottom_12 <- amis$msmp12mpos_ras

table(amis$bottom_12, amis$year)

# * unique_sps_any_12 -----
amis$unique_sps_any_12 <- NA

table(amis$m_sx12m, amis$year, useNA = "ifany")

amis$unique_sps_any_12 <- ifelse(amis$year == "2013", amis$m_sx12m, amis$m_mp12oanum)

# * unique_sps_anal_12 -----
amis$unique_sps_anal_12 <- NA

table(amis$m_mp12anum, amis$year, useNA = "ifany")

amis$unique_sps_anal_12 <- amis$m_mp12anum

# * unique_sps_oral_12 -----
amis$unique_sps_oral_12 <- NA

table(amis$m_mp12onum, amis$year, useNA = "ifany")

amis$unique_sps_oral_12 <- amis$m_mp12onum

# * unique_sps_12_N_anal -----
amis$unique_sps_12_N_anal <- NA

table(amis$m_mp12manum, amis$year, useNA = "ifany")

amis$unique_sps_12_N_anal <- amis$m_mp12manum

# * o1sp_12_type -----
amis$o1sp_12_type <- NA

table(amis$m_m1sx, amis$year, useNA = "ifany")

amis$o1sp_12_type <- factor(amis$m_m1sx,
                            levels = c(1, 2),
                            labels = c("main", "casual"))
table(amis$m_m1sx, amis$o1sp_12_type, useNA = "ifany")

# * o1sp_sexwithothers -----
amis$o1sp_sexwithothers <- NA

amis$o1sp_sexwithothers <- ifelse(amis$year >= 2018, amis$m_mlpol1, "NA")

table(amis$o1sp_sexwithothers, amis$year, useNA = "ifany")

amis$o1sp_sexwithothers <- factor(amis$o1sp_sexwithothers,
                                  levels = c(0:3),
                                  labels = c("01_DefNot",
                                             "02_ProNot",
                                             "03_ProYes",
                                             "04_DefYes"))

# * o1sp_know_status -----
amis$o1sp_know_status <- NA

table(amis$m_mm1hsk, amis$year, useNA = "ifany")

amis$o1sp_know_status[amis$m_mm1hsk == 0] <- 0
amis$o1sp_know_status[amis$m_mm1hsk == 1] <- 1

table(amis$o1sp_know_status, amis$m_mm1hsk, useNA = "ifany")

# * o1sp_hiv_status -------
amis$o1sp_hiv_status <- NA

table(amis$m_m1hst, amis$year, useNA = "ifany")
# NOTE: missing in 2013 -------
amis$o1sp_hiv_status <- factor(amis$m_m1hst,
                               levels = c(1:3),
                               labels = c("01_pos",
                                          "02_neg",
                                          "03_ind"))

table(amis$o1sp_hiv_status, amis$year, useNA = "ifany")

# * o1sp_prep -----
amis$o1sp_prep <- NA

table(amis$m_m1_prep, amis$year, useNA = "ifany")

amis$o1sp_prep[amis$m_m1_prep == 1] <- 1
amis$o1sp_prep[amis$m_m1_prep == 0] <- 0

table(amis$o1sp_prep, amis$m_m1_prep, useNA = "ifany")

# * o1sp_art -------
amis$o1sp_art <- NA

table(amis$m_m1_art, amis$year, useNA = "ifany")

amis$o1sp_art[amis$m_m1_art == 1] <- 1
amis$o1sp_art[amis$m_m1_art == 0] <- 0

table(amis$o1sp_art, amis$m_m1_art, useNA = "ifany")

# * multsp_12_type -----
amis$multsp_12_type <- NA

table(amis$m_mtyp, amis$year, useNA = "ifany")
amis$multsp_12_type <- factor(amis$m_mtyp,
                              levels = c(1:3),
                              labels = c("01_OnlyMain",
                                         "02_OnlyCas",
                                         "03_Both"))

table(amis$m_mtyp, amis$multsp_12_type, useNA = "ifany")


# * hivtest_12_bin -----
table(amis$x_lasthivtest12, amis$year, useNA = "ifany")

amis$hivtest_12_bin <- amis$x_lasthivtest12

# * mo_last_test -----
table(amis$x_mo_lasthivtest, amis$year, useNA = "ifany")

# * mo_last_pos -----
table(amis$x_mo_poshivtest, amis$year, useNA = "ifany")

# NOTE: it appears that the "time since last test" var.s were calculated using a system date ~ not sure what it is/should ask Winslow -----

# * evertest -----
table(amis$tested_ever, amis$year, useNA = "ifany")

amis$evertest <- amis$tested_ever

# * N_test_2yr -----
amis$N_test_2yr <- NA

table(amis$test2yrs, useNA = "ifany")

amis$N_test_2yr <- amis$test2yrs

amis$N_test_2yr[amis$test2yrs >= 777] <- NA

table(amis$N_test_2yr, amis$test2yrs, useNA = "ifany")

amis$N_test_2yr[amis$evertest == 0] <- 0

# * art_current -----
table(amis$curramed, useNA = "ifany")

amis$art_current[amis$curramed == 1] <- 1
amis$art_current[amis$curramed == 0] <- 0

table(amis$art_current, amis$hiv_status, useNA = "ifany") #check that only PLWH were asked about ART use

# * art_no_why -----
amis$art_no_why <- NA

table(amis$whnomeds, amis$year, useNA = "ifany")

amis$art_no_why <- factor(amis$whnomeds,
                          levels = c(0, 1, 2, 3, 4, 7, 9),
                          labels = c("01_NotGoingHCP",
                                     "02_CD4&VLgood",
                                     "03_MoneyInsBar",
                                     "04_DontWant",
                                     "05_OthReason",
                                     "06_Decline",
                                     "07_DK"))

table(amis$art_no_why, amis$whnomeds, useNA = "ifany")

table(amis$art_no_why, amis$hiv_status, useNA = "ifany") #check that only PLWH were asked about ART use

# * prep_willing -----
tableNA(amis$willing_prep) # 4,334 NA in original variable.
round(prop.table(tableNA(amis$willing_prep))*100,1) # 6.2% NA

table(amis$willing_prep, amis$year, useNA = "ifany")

amis$prep_willing <- amis$willing_prep

# * prep_elig -----
amis$prep_elig <- NA

table(amis$prep_eligible, amis$year, useNA = "ifany")

amis$prep_elig <- amis$prep_eligible

# NOTE: eiligibility missing for all 2017, 2019 -----

# * prep_doses30 -----
table(amis$prep_doses, amis$year, useNA = "ifany")

amis$prep_doses30 <- factor(amis$prep_doses,
                            levels = c(1:3),
                            labels = c("<15",
                                       "16-29",
                                       "30"))

table(amis$prep_doses30, useNA = "ifany")

table(amis$prep_doses30, amis$prep_current, useNA = "ifany") # N=61 ppl who said "yes" to current PrEP did not answer adherence Q.

# * prep_months -----
table(amis$prep_months, amis$year, useNA = "ifany")

amis$prep_months[amis$prep_months == 7 | amis$prep_months == 9] <- NA

amis$prep_months <- factor(amis$prep_months,
                           levels = c(1:4),
                           labels = c(
                              "01_<2mo",
                              "02_2-6mo",
                              "03_7-12mo",
                              "04_>=12mo"
                           ))

table(amis$prep_months, amis$prep_current, useNA = "ifany") 

# NOTE: in earlier years, the question is about the LAST TIME you used PrEP. -----

# * biomed_current -----
amis$biomed_current <- NA

amis$biomed_current[amis$hiv2 == "02_neg" & amis$prep_usecur_new == 0] <- "1_NegNOP"
amis$biomed_current[amis$hiv2 == "02_neg" & amis$prep_usecur_new == 1] <- "2_NegYOP"
amis$biomed_current[amis$hiv2 == "01_pos" & amis$art_current == 0] <- "3_PosNOA"
amis$biomed_current[amis$hiv2 == "01_pos" & amis$art_current == 1] <- "4_PosYOA"

round(prop.table(tableNA(amis$biomed_current))*100,2)

# * biomed_cur_NA -----
amis$biomed_cur_NA <- NA

amis$biomed_cur_NA[amis$hiv2 == "02_neg" & amis$prep_usecur_new == 0] <- "1_NegNOP"
amis$biomed_cur_NA[amis$hiv2 == "02_neg" & amis$prep_usecur_new == 1] <- "2_NegYOP"
amis$biomed_cur_NA[amis$hiv2 == "02_neg" & is.na(amis$prep_usecur_new)] <- "3_NegUNK"
amis$biomed_cur_NA[amis$hiv2 == "01_pos" & amis$art_current == 0] <- "4_PosNOA"
amis$biomed_cur_NA[amis$hiv2 == "01_pos" & amis$art_current == 1] <- "5_PosYOA"
amis$biomed_cur_NA[amis$hiv2 == "01_pos" & is.na(amis$art_current)] <- "6_PosUNK"

round(prop.table(tableNA(amis$biomed_cur_NA))*100,2)


# * drug_ni_12 -----
table(amis$niuse12, amis$year, useNA = "ifany")

amis$drug_ni_12[amis$niuse12 == 1] <- 1
amis$drug_ni_12[amis$niuse12 == 0] <- 0

table(amis$niuse12, amis$drug_ni_12, useNA = "ifany")

# * meth_ni_12 -----
table(amis$niu12f, amis$year, useNA = "ifany")
amis$niu12f[amis$niu12f == 7] <- NA

table(amis$niuseg, amis$year, useNA = "ifany")

amis$meth_ni_12 <- NA
amis$meth_ni_12 <- ifelse(amis$year == 2013,
                          amis$niu12f,
                          amis$niuseg)

amis$meth_ni_12[amis$drug_ni_12 == 0]<- 0

table(amis$niu12f, amis$meth_ni_12, useNA = "ifany")
table(amis$niuseg, amis$meth_ni_12, useNA = "ifany")

# * meth_ni_12_freq -----
table(amis$niuseg_freq, amis$year, useNA = "ifany")

amis$meth_ni_12_freq <- NA
amis$niuseg_freq[amis$niuseg_freq == 77] <- NA

table(amis$niuseg_freq, amis$year, useNA = "ifany")

amis$meth_ni_12_freq <- factor(amis$niuseg_freq,
                               levels = c(1:7, 99),
                               labels = c(
                                  "01_>1/day",
                                  "02_1x/day",
                                  "03_>1/wk",
                                  "04_1x/wk",
                                  "05_>1/mo",
                                  "06_1x/mo",
                                  "07_<1/mo",
                                  "XX_DK"
                               ))

table(amis$meth_ni_12_freq)

# * lsp_type -----
amis$lsp_type <- NA

table(amis$m_mlmc, amis$year, useNA = "ifany") # 2013, 2014; 1 = main, 2 = casual
table(amis$m_mlmc_typ, amis$year, useNA = "ifany") # same coding

#remove NAs/refs
amis$m_mlmc[amis$m_mlmc >= 3] <- NA
amis$m_mlmc_typ[amis$m_mlmc_typ >= 7] <- NA

amis$lsp_type <- ifelse(amis$year <= 2014, amis$m_mlmc, amis$m_mlmc_typ)

table(amis$lsp_type, amis$year, useNA = "ifany") #check

# * lsp_lse_month -----
table(amis$m_mlcm, amis$year, useNA = "ifany") # supposed to be 2013 variable but values are weird.
table(amis$m_mlcmmonth, amis$year, useNA = "ifany")
# correct weird 2015 values
amis$m_mlcmmonth[amis$m_mlcmmonth == 10979] <- 1
amis$m_mlcmmonth[amis$m_mlcmmonth == 10980] <- 2
amis$m_mlcmmonth[amis$m_mlcmmonth == 10981] <- 3
amis$m_mlcmmonth[amis$m_mlcmmonth == 10982] <- 4
amis$m_mlcmmonth[amis$m_mlcmmonth == 10983] <- 5
amis$m_mlcmmonth[amis$m_mlcmmonth == 10984] <- 6
amis$m_mlcmmonth[amis$m_mlcmmonth == 10985] <- 7
amis$m_mlcmmonth[amis$m_mlcmmonth == 10986] <- 8
amis$m_mlcmmonth[amis$m_mlcmmonth == 10987] <- 9
amis$m_mlcmmonth[amis$m_mlcmmonth == 10988] <- 10
amis$m_mlcmmonth[amis$m_mlcmmonth == 10989] <- 11
amis$m_mlcmmonth[amis$m_mlcmmonth == 10990] <- 12
table(amis$m_mlcmmonth, amis$year, useNA = "ifany")

amis$lsp_lse_month <- factor(amis$m_mlcmmonth,
                             levels = c(1:12),
                             labels = c(
                                "JAN",
                                "FEB",
                                "MAR",
                                "APR",
                                "MAY",
                                "JUN",
                                "JUL",
                                "AUG",
                                "SEP",
                                "OCT",
                                "NOV",
                                "DEC"
                             ))

table(amis$lsp_lse_month, amis$m_mlcmmonth, useNA = "ifany")

# * lsp_lse_year -----
table(amis$m_mlcm, amis$year, useNA = "ifany") #same: 2013 variable values are wonky
table(amis$m_mlcmyear, amis$year, useNA = "ifany") #2014-2017

#correct wonky years
amis$m_mlcmyear[amis$m_mlcmyear == 20011] <- 2011
amis$m_mlcmyear[amis$m_mlcmyear == 20014] <- 2014
amis$m_mlcmyear[amis$m_mlcmyear == 20016] <- 2016
amis$m_mlcmyear[amis$m_mlcmyear == 20214] <- 2014
amis$m_mlcmyear[amis$m_mlcmyear == 21014] <- 2014
amis$m_mlcmyear[amis$m_mlcmyear == 2104] <- 2014
amis$m_mlcmyear[amis$m_mlcmyear == 2114] <- 2014
amis$m_mlcmyear[amis$m_mlcmyear == 2816] <- 2016
amis$m_mlcmyear[amis$m_mlcmyear == 2914] <- 2014
amis$m_mlcmyear[amis$m_mlcmyear == 2915] <- 2015
amis$m_mlcmyear[amis$m_mlcmyear == 2916] <- 2016
amis$m_mlcmyear[amis$m_mlcmyear == 3016] <- 2016
amis$m_mlcmyear[amis$m_mlcmyear == 2045] <- 2015
amis$m_mlcmyear[amis$m_mlcmyear == 2026] <- 2016
amis$m_mlcmyear[amis$m_mlcmyear == 2046] <- 2016
amis$m_mlcmyear[amis$m_mlcmyear == 2076] <- 2016

# * lsp_lse_lt1yr -----
amis$lsp_lse_lt1yr <- NA

#anyone who said sex with LSP was in the same calendar year or sooner become "yes" to LSP LSE within 1 year.
amis$lsp_lse_lt1yr[amis$year - amis$m_mlcmyear <= 0] <- 1

prop.table(tableNA(amis$lsp_lse_lt1yr))*100



# NOTE: What to do with years before 1948? -----

# * lsp_btm -----
table(amis$m_mlras, amis$year, useNA = "ifany") 
# NOTE: some year codebooks have 1=No, 2=Yes; most years have just 0/1 ~ assuming No/Yes -----

amis$lsp_btm[amis$m_mlras == 1] <- 1
amis$lsp_btm[amis$m_mlras == 0] <- 0

table(amis$m_mlras, amis$lsp_btm, useNA = "ifany") 

# * lsp_btm_condom_any -----
amis$lsp_btm_condom_any <- NA

table(amis$m_mlrasc, amis$year, useNA = "ifany")

amis$lsp_btm_condom_any[amis$m_mlrasc == 1 | amis$m_mlrasc == 2] <- 1
amis$lsp_btm_condom_any[amis$m_mlrasc == 0] <- 0

table(amis$lsp_btm_condom_any, amis$m_mlrasc, useNA = "ifany")

# * lsp_btm_condom_entire -----
amis$lsp_btm_condom_entire <- NA

amis$lsp_btm_condom_entire[amis$m_mlrast == 0] <- 0
amis$lsp_btm_condom_entire[amis$m_mlrast == 1] <- 1

amis$lsp_btm_condom_entire[amis$m_mlrasc == 2 & amis$year != 2013] <- 1
amis$lsp_btm_condom_entire[(amis$m_mlrasc == 0 | amis$m_mlrasc == 1) & amis$year != 2013] <- 0

table(amis$lsp_btm_condom_entire, amis$m_mlrast, useNA = "ifany")
table(amis$lsp_btm_condom_entire, amis$m_mlrasc, useNA = "ifany") #check

# * lsp_top -----
table(amis$m_mlias, amis$year, useNA = "ifany") 

amis$lsp_top[amis$m_mlias == 1] <- 1
amis$lsp_top[amis$m_mlias == 0] <- 0

table(amis$m_mlias, amis$lsp_top, useNA = "ifany") 

# * lsp_top_condom_any -----
amis$lsp_top_condom_any <- NA

table(amis$m_mliasc, amis$year, useNA = "ifany")

amis$lsp_top_condom_any[amis$m_mliasc == 1 | amis$m_mliasc == 2] <- 1
amis$lsp_top_condom_any[amis$m_mliasc == 0] <- 0

table(amis$lsp_top_condom_any, amis$m_mliasc, useNA = "ifany")

# * lsp_top_condom_entire -----
amis$lsp_top_condom_entire <- NA

table(amis$m_mliast, amis$year, useNA = "ifany")
# NOTE: lots of blanks and "." --- maybe a coding error (2013 only) -- probably a Winslow Q. -----

amis$lsp_top_condom_entire[amis$m_mliast == 0] <- 0
amis$lsp_top_condom_entire[amis$m_mliast == 1] <- 1

amis$lsp_top_condom_entire[amis$m_mliasc == 2 & amis$year != 2013] <- 1
amis$lsp_top_condom_entire[(amis$m_mliasc == 0 | amis$m_mliasc == 1) & amis$year != 2013] <- 0

table(amis$lsp_top_condom_entire, amis$m_mliast, useNA = "ifany")
table(amis$lsp_top_condom_entire, amis$m_mliasc, useNA = "ifany") #check

# * lsp_know_status -----
table(amis$m_mlkno, amis$year, useNA = "ifany")

amis$lsp_know_status[amis$m_mlkno == 1] <- 1
amis$lsp_know_status[amis$m_mlkno == 0] <- 0

table(amis$lsp_know_status, amis$m_mlkno, useNA = "ifany")

# * lsp_hiv_status -----
table(amis$m_mlhiv, amis$year, useNA = "ifany")

amis$lsp_hiv_status <- factor(amis$m_mlhiv,
                              levels = c(1:3),
                              labels = c(
                                 "1_neg",
                                 "2_pos",
                                 "3_ind"
                              ))

table(amis$m_mlhiv, amis$lsp_hiv_status, useNA = "ifany")

# * lsp_age ----
table(amis$m_mlage, amis$year, useNA = "ifany") #2013 only
table(amis$m_mlrage, amis$year, useNA = "ifany") #2014-2017

#remove unfeasible ages from 2013 var
amis$m_mlage[amis$m_mlage == 1 |
                amis$m_mlage >= 777] <- NA

#drop NA/DKs from later years
amis$m_mlrage[amis$m_mlrage >= 77] <- NA

#label categories for 2014-2017
amis$lsp_age <- factor(amis$m_mlrage,
                       levels = c(1:14),
                       labels = c(
                          "19_yngr",
                          "20-24",
                          "25-29",
                          "30-34",
                          "35-39",
                          "40-44",
                          "45-49",
                          "50-54",
                          "55-59",
                          "60-64",
                          "65-69",
                          "70-74",
                          "75-79",
                          "80+"
                       ))

table(amis$lsp_age, amis$m_mlrage, useNA = "ifany")

#make 2013 var match categories for later years.
amis$lsp_age[amis$m_mlage <= 19] <- "19_yngr"
amis$lsp_age[20 <= amis$m_mlage & amis$m_mlage <= 24] <- "20-24"
amis$lsp_age[25 <= amis$m_mlage & amis$m_mlage <= 29] <- "25-29"
amis$lsp_age[30 <= amis$m_mlage & amis$m_mlage <= 34] <- "30-34"
amis$lsp_age[35 <= amis$m_mlage & amis$m_mlage <= 39] <- "35-39"
amis$lsp_age[40 <= amis$m_mlage & amis$m_mlage <= 44] <- "40-44"
amis$lsp_age[45 <= amis$m_mlage & amis$m_mlage <= 49] <- "45-49"
amis$lsp_age[50 <= amis$m_mlage & amis$m_mlage <= 54] <- "50-54"
amis$lsp_age[55 <= amis$m_mlage & amis$m_mlage <= 59] <- "55-59"
amis$lsp_age[60 <= amis$m_mlage & amis$m_mlage <= 64] <- "60-64"
amis$lsp_age[65 <= amis$m_mlage & amis$m_mlage <= 69] <- "65-69"
amis$lsp_age[70 <= amis$m_mlage & amis$m_mlage <= 74] <- "70-74"
amis$lsp_age[75 <= amis$m_mlage & amis$m_mlage <= 69] <- "75-79"
amis$lsp_age[amis$m_mlage >= 80] <- "80+"

table(amis$m_mlage, amis$lsp_age, useNA = "ifany")
table(amis$m_mlrage, amis$lsp_age, useNA = "ifany") #check

# * lsp_age_older -----
amis$lsp_age_older <- NA

# NOTE: 2013 variable "m_mlea" not present; maybe used to calculate new var to match newer years? -----
table(amis$m_mlra1, amis$year, useNA = "ifany")

amis$lsp_age_older[amis$m_mlra1 == 1] <- 1

# * lsp_age_younger -----
amis$lsp_age_younger <- NA

table(amis$m_mlra2, amis$year, useNA = "ifany")

amis$lsp_age_younger[amis$m_mlra2 == 1 & amis$year != 2013] <- 1

# * lsp_age_same -----
amis$lsp_age_same <- NA

table(amis$m_mlra3, amis$year, useNA = "ifany")

amis$lsp_age_same[amis$m_mlra3 == 1 & amis$year != 2013] <- 1

# * lsp_race_aian -----
amis$lsp_race_aian[amis$m_mleth1 == 1] <- 1
table(amis$lsp_race_aian, amis$m_mleth1, useNA = "ifany")

# * lsp_race_asian -----
amis$lsp_race_asian[amis$m_mleth2 == 1] <- 1
table(amis$lsp_race_asian, amis$m_mleth2, useNA = "ifany")

# * lsp_race_blaa -----
amis$lsp_race_blaa[amis$m_mleth3 == 1] <- 1
table(amis$lsp_race_blaa, amis$m_mleth3, useNA = "ifany")

# * lsp_race_hisp -----
amis$lsp_race_hisp[amis$m_mleth4 == 1] <- 1
table(amis$lsp_race_hisp, amis$m_mleth4, useNA = "ifany")

# * lsp_race_nhpi -----
amis$lsp_race_nhpi[amis$m_mleth5 == 1] <- 1
table(amis$lsp_race_nhpi, amis$m_mleth5, useNA = "ifany")

# * lsp_race_white -----
amis$lsp_race_white[amis$m_mleth6 == 1] <- 1
table(amis$lsp_race_white, amis$m_mleth6, useNA = "ifany")

# * lsp_n_enc -----
amis$lsp_n_enc <- NA

table(amis$m_mlslt, amis$year, useNA = "ifany") 
# NOTE: "m_mlslt" exists in 2013 but not in the codebook & the numbers are suspicious -----

amis$lsp_n_enc[amis$m_mlslt == 1 & amis$year >= 2014] <- "1_once"
amis$lsp_n_enc[amis$m_mlslt == 2 & amis$year >= 2014] <- "2_two+"

table(amis$lsp_n_enc, amis$m_mlslt, useNA = "ifany")
table(amis$lsp_n_enc, amis$year, useNA = "ifany")

# * lsp_length -----
table(amis$m_mlt_n1, amis$year, useNA = "ifany") 
table(amis$m_mlt_n2, amis$year, useNA = "ifany")

amis$lsp_length_unit <- factor(amis$m_mlt_n2,
                             levels = c(1:3),
                             labels = c(
                                "days",
                                "months",
                                "years"
                             ))

amis$lsp_length[amis$m_mlt_n1 == 2006] <- "9 years"
amis$lsp_length[amis$m_mlt_n1 == 2011] <- "5 years"
amis$lsp_length[amis$m_mlt_n1 == 2015] <- "2 years"

amis$lsp_length_num <- amis$m_mlt_n1
amis$lsp_length_num[amis$lsp_length_num >= 2006] <- NA
table(amis$lsp_length_num, useNA = "ifany")


table(amis$lsp_length_num, amis$lsp_length_unit, useNA = "ifany")

amis$lsp_length[amis$lsp_length_num <= 1 & amis$lsp_length_unit == "days"] <- "01_one-night-stand"
amis$lsp_length[amis$lsp_length_num <= 31 & amis$lsp_length_num >=2 & amis$lsp_length_unit == "days"] <- "02_up-to-one-mo"
amis$lsp_length[amis$lsp_length_num >= 32 & amis$lsp_length_num <= 180 & amis$lsp_length_unit == "days"] <- "03_one-to-six-mo"
amis$lsp_length[amis$lsp_length_num >= 181 & amis$lsp_length_unit == "days"] <- "04_six-to-twelve-mo"
amis$lsp_length[amis$lsp_length_num == 0 & amis$lsp_length_unit == "months"] <- "02_up-to-one-mo"
amis$lsp_length[amis$lsp_length_num >= 1 & amis$lsp_length_num <= 6 & amis$lsp_length_unit == "months"] <- "03_one-to-six-mo"
amis$lsp_length[amis$lsp_length_num >= 7 & amis$lsp_length_num <= 12 & amis$lsp_length_unit == "months"] <- "04_six-to-twelve-mo"
amis$lsp_length[amis$lsp_length_num >= 13 & amis$lsp_length_num <= 24 & amis$lsp_length_unit == "months"] <- "05_one-to-two-yr"
amis$lsp_length[amis$lsp_length_num >= 25 & amis$lsp_length_unit == "months"] <- "06_over-two-yr"
amis$lsp_length[amis$lsp_length_num >= 1 & amis$lsp_length_num <=2 & amis$lsp_length_unit == "years"] <- "05_one-to-two-yr"
amis$lsp_length[amis$lsp_length_num >= 3 & amis$lsp_length_unit == "years"] <- "06_over-two-yr"

amis$lsp_length[amis$lsp_length == "2 years"] <- "05_one-to-two-yr"
amis$lsp_length[amis$lsp_length == "5 years" | amis$lsp_length == "9 years"] <- "06_over-two-yr"

table(amis$lsp_length)

# * lsp_sexagain -----
amis$lsp_sexagain <- NA

table(amis$m_mltagain, amis$year, useNA = "ifany")

amis$lsp_sexagain[amis$m_mltagain == 1] <- "1_yes"
amis$lsp_sexagain[amis$m_mltagain == 0] <- "0_no"
amis$lsp_sexagain[amis$m_mltagain == 9] <- "2_DK"

table(amis$m_mltagain, amis$lsp_sexagain, useNA = "ifany")

# * lsp_ihadsexothers -----
amis$lsp_ihadsexothers <- NA

table(amis$m_mlog1, amis$year, useNA = "ifany")

amis$lsp_ihadsexothers[amis$m_mlog1 == 1] <- 1
amis$lsp_ihadsexothers[amis$m_mlog1 == 0] <- 0

table(amis$lsp_ihadsexothers, amis$m_mlog1, useNA = "ifany")

# * lsp_any_drugs -----
amis$lsp_any_drugs <- NA

table(amis$m_mlhi, amis$year, useNA = "ifany")

amis$lsp_any_drugs <- factor(amis$m_mlhi,
                             levels = c(1:4,9),
                             labels = c(
                                "1_alc",
                                "2_drugs",
                                "3_both",
                                "4_neith",
                                "5_DK"
                             ))

table(amis$m_mlhi, amis$lsp_any_drugs, useNA = "ifany")

# * lsp_him_ever_meth -----
amis$lsp_him_ever_meth <- NA

table(amis$m_mlpcm, amis$year, useNA = "ifany") #2013 only 

amis$lsp_him_ever_meth[amis$year == 2013 & amis$m_mlpcm == 0] <- "1_DefNot"
amis$lsp_him_ever_meth[amis$year == 2013 & amis$m_mlpcm == 1] <- "2_PrbNot"
amis$lsp_him_ever_meth[amis$year == 2013 & amis$m_mlpcm == 2] <- "3_PrbYes"
amis$lsp_him_ever_meth[amis$year == 2013 & amis$m_mlpcm == 3] <- "4_DefYes"
amis$lsp_him_ever_meth[amis$year == 2013 & amis$m_mlpcm == 9] <- "X_DK"

table(amis$lsp_him_ever_meth, amis$m_mlpcm) #check 2013


table(amis$m_mlcmuse, amis$year, useNA = "ifany") #2015, 2016 only

amis$lsp_him_ever_meth[(amis$year == 2015 | amis$year == 2016) & amis$m_mlcmuse == 0] <- "1_DefNot"
amis$lsp_him_ever_meth[(amis$year == 2015 | amis$year == 2016) & amis$m_mlcmuse == 1] <- "2_PrbNot"
amis$lsp_him_ever_meth[(amis$year == 2015 | amis$year == 2016) & amis$m_mlcmuse == 2] <- "3_PrbYes"
amis$lsp_him_ever_meth[(amis$year == 2015 | amis$year == 2016) & amis$m_mlcmuse == 3] <- "4_DefYes"
amis$lsp_him_ever_meth[(amis$year == 2015 | amis$year == 2016) & amis$m_mlcmuse == 9] <- "X_DK"

table(amis$lsp_him_ever_meth, amis$m_mlcmuse, useNA = "ifany")

# * lsp_i_did_meth -----
amis$lsp_i_did_meth <- NA

table(amis$m_mlsdrf, amis$year, useNA = "ifany") #supposed to be the var for 2013 but 2013 is all NAs
table(amis$m_mlsdrg, amis$year, useNA = "ifany") #2014 only
table(amis$niuseg_lsex, amis$year, useNA = "ifany") #2015-2017 only

amis$lsp_i_did_meth[amis$m_mlsdrg == 1 | amis$niuseg_lsex == 1] <- 1
amis$lsp_i_did_meth[amis$m_mlsdrg == 0 | amis$niuseg_lsex == 0] <- 0

table(amis$lsp_i_did_meth, amis$m_mlsdrg, useNA = "ifany")
table(amis$lsp_i_did_meth, amis$niuseg_lsex, useNA = "ifany") #check

# * cai_12_new -----
amis$cai_12_new <- NA

amis$cai_12_new[amis$m_m1uas == 1] <- 1
amis$cai_12_new[amis$m_m1uas == 0] <- 0

amis$cai_12_new[amis$m_mp12type == 1 | amis$m_mp12type == 4] <- 0 

tableNA(amis$cai_12_new, amis$year)

amis13 <- subset(amis, year == 2013)
amis14 <- subset(amis, year == 2014)
amis15 <- subset(amis, year == 2015)
amis16 <- subset(amis, year == 2016)
amis17 <- subset(amis, year == 2017)
amis18 <- subset(amis, year == 2018)
amis19 <- subset(amis, year == 2019)

### * * 2013 -----
tableNA(amis13$m_sx12m) #how many male SPs?
## `m_mm1as` variable is missing "any anal sex with your one SP?" 
tableNA(amis13$m_mm1uas) # did you have CAI with your one SP?
## `m_mmhas` variable is missing "any anal sex with your N SPs?"
tableNA(amis13$m_mmhuas) # did you have CAI with any of your SPs?

# First, apply clear logic from "did you have CAI with any of your SPs?" question
amis13$cai_12_new[amis13$m_mm1uas == 1] <- 1
amis13$cai_12_new[amis13$m_mm1uas == 0] <- 0
amis13$cai_12_new[amis13$m_mmhuas == 1] <- 1
amis13$cai_12_new[amis13$m_mmhuas == 0] <- 0
round(prop.table(tableNA(amis13$cai_12_new))*100,1)

# Next, apply logic agreed upon with Steve:
amis13$cai_12_new[amis13$m_sx12m == 1 & is.na(amis13$m_mm1uas)] <- 0
amis13$cai_12_new[amis13$m_sx12m >= 1 & is.na(amis13$m_mmhuas)] <- 0
round(prop.table(tableNA(amis13$cai_12_new))*100,1) # 14 (<1%) now NA!

# Next, apply logic agreed upon with Steve:
amis13$cai_12_new[amis13$m_sx12m == 1 & is.na(amis13$m_mm1uas)] <- 0
amis13$cai_12_new[amis13$m_sx12m >= 1 & is.na(amis13$m_mmhuas)] <- 0
round(prop.table(tableNA(amis13$cai_12_new))*100,1) # 14 (<1%) now NA!

### * * 2014 -----
round(prop.table(tableNA(amis14$cai_12_new))*100,1) #48 (<1%) now NA!

amis14$cai_12_new <- NA

amis14$cai_12_new[amis14$m_mp12type == 1] <- 0 #if oral only, then CAI = 0
amis14$cai_12_new[amis14$m_mp12type == 4] <- 0 #if no sex p12, then CAI = 0
amis14$cai_12_new[amis14$m_m1uas == 0] <- 0 #if no CAI, then CAI = 0
amis14$cai_12_new[amis14$m_m1uas == 1] <- 1 #if yes CAI, then CAI = 0

round(prop.table(tableNA(amis14$cai_12_new))*100,1) #same breakdown of CAI

### * * 2015 -----
## `e_evermsm_anal` -- screener "ever anal sex" variable
tableNA(amis15$msmp12m_anal) #select all that apply, checked == 1
summary(amis15$m_mp12anum) # number of any A.I. SPs, 12 
summary(amis15$m_mp12oanum) # number or oral or anal SPs, 12
summary(amis15$m_mp12manum) # number of A.I. SPs, 12

tableNA(amis15$m_m1uas) #did you have CAI, p12?

amis15$cai_12_new <- NA #refresh the CAI p12 variable for 2015.

amis15$cai_12_new[amis15$msmp12m_anal == 0] <- 0 #if no AI p12, then no CAI p12.
amis15$cai_12_new[amis15$m_m1uas == 1] <- 1
amis15$cai_12_new[amis15$m_m1uas == 0] <- 0

amis15$cai_12_new[amis15$m_m1uasnum1 >= 1 | amis15$m_m1uasnum2 >= 1] <- 1
amis15$cai_12_new[amis15$msmp12m_none == 1] <- 0

round(prop.table(tableNA(amis15$cai_12_new))*100,1) #84 (<1%) NA

### * * 2016 -----
tableNA(amis16$msmp12m_anal) #select all that apply, checked == 1
summary(amis16$m_mp12anum) # number of any A.I. SPs, 12 
summary(amis16$m_mp12oanum) # number or oral or anal SPs, 12
summary(amis16$m_mp12manum) # number of A.I. SPs, 12
summary(amis16$m_m1uasnum1)
summary(amis16$m_m1uasnum2)

tableNA(amis16$m_m1uas) #did you have CAI, p12?

amis16$cai_12_new <- NA #refresh the CAI p12 variable for 2016.

amis16$cai_12_new[amis16$msmp12m_anal == 0] <- 0 #if no AI p12, then no CAI p12.
amis16$cai_12_new[amis16$m_m1uas == 1] <- 1
amis16$cai_12_new[amis16$m_m1uas == 0] <- 0

amis16$cai_12_new[amis16$m_m1uasnum1 >= 1 | amis16$m_m1uasnum2 >= 1] <- 1

round(prop.table(tableNA(amis16$cai_12_new))*100,1) #167 (1.6%) NA

# * * 2017 -----
round(prop.table(tableNA(amis17$cai_12_new))*100,1) #16% NA

tableNA(amis17$msmp12m_anal) # if 0, then cai_12_new == 0
summary(amis17$m_mp12anum) # number of any A.I. SPs, 12 
summary(amis17$m_mp12oanum) # number or oral or anal SPs, 12
summary(amis17$m_mp12manum) # number of A.I. SPs, 12
summary(amis17$m_m1uasnum1)
summary(amis17$m_m1uasnum2)
tableNA(amis17$m_m1uas)

amis17$cai_12_new[amis17$msmp12m_anal == 0] <- 0 #if no AI p12, then no CAI p12.
amis17$cai_12_new[amis17$m_m1uas == 1] <- 1
amis17$cai_12_new[amis17$m_m1uas == 0] <- 0

round(prop.table(tableNA(amis17$cai_12_new))*100,1) # 115 (1.1%) NA

# * * 2018 -----
round(prop.table(tableNA(amis18$cai_12_new))*100,1) #16% NA

tableNA(amis18$msmp12m_anal) # if 0, then cai_12_new == 0
summary(amis18$m_mp12anum) # number of any A.I. SPs, 12 
summary(amis18$m_mp12oanum) # number or oral or anal SPs, 12
summary(amis18$m_mp12manum) # number of A.I. SPs, 12
summary(amis18$m_m1uasnum1)
summary(amis18$m_m1uasnum2)
tableNA(amis18$m_m1uas)

amis18$cai_12_new[amis18$msmp12m_anal == 0] <- 0

amis18$cai_12_new[amis18$m_m1uas == 1] <- 1
amis18$cai_12_new[amis18$m_m1uas == 0] <- 0

round(prop.table(tableNA(amis18$cai_12_new))*100,1) #127 (1%) NA

# * * 2019 -----
round(prop.table(tableNA(amis19$cai_12_new))*100,1) # 14% NA

tableNA(amis19$msmp12m_anal) # if 0, then cai_12_new == 0
summary(amis19$m_mp12anum) # number of any A.I. SPs, 12 
summary(amis19$m_mp12oanum) # number or oral or anal SPs, 12
summary(amis19$m_mp12manum) # number of A.I. SPs, 12
summary(amis19$m_m1uasnum1)
summary(amis19$m_m1uasnum2)
tableNA(amis19$m_m1uas)

amis19$cai_12_new[amis19$msmp12m_anal == 0] <- 0

amis19$cai_12_new[amis19$m_m1uas == 1] <- 1
amis19$cai_12_new[amis19$m_m1uas == 0] <- 0

round(prop.table(tableNA(amis19$cai_12_new))*100,1) #66 (<1%) NA


# * N_cai_sps_12 -----

# * * 2013 ----
# should be NA for all for now.
amis13$N_cai_sps_12 <- NA

# * * 2014 -----
amis14$N_cai_sps_12 <- NA

tableNA(amis14$m_mp12oanum)
tableNA(amis14$m_mp12anum)

amis14$N_cai_sps_12 <- ifelse(!is.na(amis14$m_m1uasnum1), amis14$m_m1uasnum1, amis14$m_m1uasnum2)
amis14$N_cai_sps_12[amis14$cai_12_new == 0] <- 0 

round(prop.table(tableNA(amis14$cai_12_new, amis14$N_cai_sps_12), margin = 1)*100,1) #43% NA among those who report CAI p12

amis14$N_cai_sps_12[amis14$m_mp12type == 1 | amis14$m_mp12type == 4] <- 0
amis14$N_cai_sps_12[amis14$m_mp12oanum == 0] <- 0
amis14$N_cai_sps_12[amis14$m_mp12anum == 0] <- 0
amis14$N_cai_sps_12[amis14$m_mp12manum == 0] <- 0

amis14$N_cai_sps_12[amis14$m_mp12anum == 1 & amis14$cai_12_new == 1] <- 1

amis14$N_cai_sps_12[amis14$cai_12_new == 1 & amis14$m_mp12oanum == 1] <- 1
amis14$N_cai_sps_12[amis14$cai_12_new == 1 & amis14$m_mp12manum == 1] <- 1

round(prop.table(tableNA(amis14$cai_12_new, amis14$N_cai_sps_12), margin = 1)*100,1) #14% NA among those who report CAI p12

amis14_Ncai_NA <- amis14[amis14$cai_12_new == 1 & is.na(amis14$N_cai_sps_12),]

round(prop.table(tableNA(amis14_Ncai_NA$m_mp12oanum))*100,1)
round(prop.table(tableNA(amis14_Ncai_NA$m_mp12manum))*100,1)
round(prop.table(tableNA(amis14_Ncai_NA$m_mp12anum))*100,1)


# * * 2015 -----
amis15$N_cai_sps_12 <- NA

amis15$N_cai_sps_12 <- ifelse(!is.na(amis15$m_m1uasnum1), amis15$m_m1uasnum1, amis15$m_m1uasnum2)
amis15$N_cai_sps_12[amis15$cai_12_new == 0] <- 0 

round(prop.table(tableNA(amis15$cai_12_new, amis15$N_cai_sps_12), margin = 1)*100,1) #43% NA among those who report CAI p12

amis15$N_cai_sps_12[amis15$msmp12m_anal == 0] <- 0
amis15$N_cai_sps_12[amis15$m_mp12oanum == 0] <- 0
amis15$N_cai_sps_12[amis15$m_mp12anum == 0] <- 0
amis15$N_cai_sps_12[amis15$m_mp12manum == 0] <- 0

amis15$N_cai_sps_12[amis15$m_mp12anum == 1 & amis15$cai_12_new == 1] <- 1
amis15$N_cai_sps_12[amis15$cai_12_new == 1 & amis15$m_mp12oanum == 1] <- 1
amis15$N_cai_sps_12[amis15$cai_12_new == 1 & amis15$m_mp12manum == 1] <- 1

round(prop.table(tableNA(amis15$cai_12_new, amis15$N_cai_sps_12), margin = 1)*100,1) #17% NA among those who report CAI p12

amis15_Ncai_NA <- amis15[amis15$cai_12_new == 1 & is.na(amis15$N_cai_sps_12),]


round(prop.table(tableNA(amis15_Ncai_NA$m_mp12oanum))*100,1)
round(prop.table(tableNA(amis15_Ncai_NA$m_mp12manum))*100,1)
round(prop.table(tableNA(amis15_Ncai_NA$m_mp12anum))*100,1)


# * * 2016 -----
amis16$N_cai_sps_12 <- NA

amis16$N_cai_sps_12 <- ifelse(!is.na(amis16$m_m1uasnum1), amis16$m_m1uasnum1, amis16$m_m1uasnum2)
amis16$N_cai_sps_12[amis16$cai_12_new == 0] <- 0 

round(prop.table(tableNA(amis16$cai_12_new, amis16$N_cai_sps_12), margin = 1)*100,1) #49% NA among those who report CAI p12

amis16$N_cai_sps_12[amis16$msmp12m_anal == 0] <- 0
amis16$N_cai_sps_12[amis16$m_mp12oanum == 0] <- 0
amis16$N_cai_sps_12[amis16$m_mp12anum == 0] <- 0
amis16$N_cai_sps_12[amis16$m_mp12manum == 0] <- 0
amis16$N_cai_sps_12[amis16$msmp12m_none == 1] <- 0

amis16$N_cai_sps_12[amis16$m_mp12anum == 1 & amis16$cai_12_new == 1] <- 1
amis16$N_cai_sps_12[amis16$cai_12_new == 1 & amis16$m_mp12oanum == 1] <- 1
amis16$N_cai_sps_12[amis16$cai_12_new == 1 & amis16$m_mp12manum == 1] <- 1

round(prop.table(tableNA(amis16$cai_12_new, amis16$N_cai_sps_12), margin = 1)*100,1) #25% NA among those who report CAI p12

amis16_Ncai_NA <- amis16[amis16$cai_12_new == 1 & is.na(amis16$N_cai_sps_12),]

round(prop.table(tableNA(amis16_Ncai_NA$m_mp12oanum))*100,1)
round(prop.table(tableNA(amis16_Ncai_NA$m_mp12manum))*100,1)
round(prop.table(tableNA(amis16_Ncai_NA$m_mp12anum))*100,1)

# * * 2017 -----
amis17$N_cai_sps_12 <- NA

amis17$N_cai_sps_12 <- ifelse(!is.na(amis17$m_m1uasnum1), amis17$m_m1uasnum1, amis17$m_m1uasnum2)
amis17$N_cai_sps_12[amis17$cai_12_new == 0] <- 0 

round(prop.table(tableNA(amis17$cai_12_new, amis17$N_cai_sps_12), margin = 1)*100,1) #44% NA among those who report CAI p12

amis17$N_cai_sps_12[amis17$msmp12m_anal == 0] <- 0
amis17$N_cai_sps_12[amis17$m_mp12oanum == 0] <- 0
amis17$N_cai_sps_12[amis17$m_mp12anum == 0] <- 0
amis17$N_cai_sps_12[amis17$m_mp12manum == 0] <- 0
amis17$N_cai_sps_12[amis17$msmp12m_none == 1] <- 0

amis17$N_cai_sps_12[amis17$m_mp12anum == 1 & amis17$cai_12_new == 1] <- 1
amis17$N_cai_sps_12[amis17$cai_12_new == 1 & amis17$m_mp12oanum == 1] <- 1
amis17$N_cai_sps_12[amis17$cai_12_new == 1 & amis17$m_mp12manum == 1] <- 1

round(prop.table(tableNA(amis17$cai_12_new, amis17$N_cai_sps_12), margin = 1)*100,1) #21% NA among those who report CAI p12

amis17_Ncai_NA <- amis17[amis17$cai_12_new == 1 & is.na(amis17$N_cai_sps_12),]

round(prop.table(tableNA(amis17_Ncai_NA$m_mp12oanum))*100,1)
round(prop.table(tableNA(amis17_Ncai_NA$m_mp12manum))*100,1)
round(prop.table(tableNA(amis17_Ncai_NA$m_mp12anum))*100,1)

# * * 2018 -----
amis18$N_cai_sps_12 <- NA

amis18$N_cai_sps_12 <- ifelse(!is.na(amis18$m_m1uasnum1), amis18$m_m1uasnum1, amis18$m_m1uasnum2)
amis18$N_cai_sps_12[amis18$cai_12_new == 0] <- 0 

round(prop.table(tableNA(amis18$cai_12_new, amis18$N_cai_sps_12), margin = 1)*100,1) #92% NA among those who report CAI p12

amis18$N_cai_sps_12[amis18$msmp12m_anal == 0] <- 0
amis18$N_cai_sps_12[amis18$m_mp12oanum == 0] <- 0
amis18$N_cai_sps_12[amis18$m_mp12anum == 0] <- 0
amis18$N_cai_sps_12[amis18$m_mp12manum == 0] <- 0
amis18$N_cai_sps_12[amis18$msmp12m_none == 1] <- 0

amis18$N_cai_sps_12[amis18$m_mp12anum == 1 & amis18$cai_12_new == 1] <- 1
amis18$N_cai_sps_12[amis18$cai_12_new == 1 & amis18$m_mp12oanum == 1] <- 1
amis18$N_cai_sps_12[amis18$cai_12_new == 1 & amis18$m_mp12manum == 1] <- 1

round(prop.table(tableNA(amis18$cai_12_new, amis18$N_cai_sps_12), margin = 1)*100,1) #67% NA among those who report CAI p12

amis18_Ncai_NA <- amis18[amis18$cai_12_new == 1 & is.na(amis18$N_cai_sps_12),]

round(prop.table(tableNA(amis18_Ncai_NA$m_mp12oanum))*100,1)
round(prop.table(tableNA(amis18_Ncai_NA$m_mp12manum))*100,1)
round(prop.table(tableNA(amis18_Ncai_NA$m_mp12anum))*100,1)

# * * 2019 -----
amis19$N_cai_sps_12 <- NA

amis19$N_cai_sps_12 <- ifelse(!is.na(amis19$m_m1uasnum1), amis19$m_m1uasnum1, amis19$m_m1uasnum2)
amis19$N_cai_sps_12[amis19$cai_12_new == 0] <- 0 

round(prop.table(tableNA(amis19$cai_12_new, amis19$N_cai_sps_12), margin = 1)*100,1) #37% NA among those who report CAI p12

amis19$N_cai_sps_12[amis19$msmp12m_anal == 0] <- 0
amis19$N_cai_sps_12[amis19$m_mp12oanum == 0] <- 0
amis19$N_cai_sps_12[amis19$m_mp12anum == 0] <- 0
amis19$N_cai_sps_12[amis19$m_mp12manum == 0] <- 0
amis19$N_cai_sps_12[amis19$msmp12m_none == 1] <- 0

amis19$N_cai_sps_12[amis19$m_mp12anum == 1 & amis19$cai_12_new == 1] <- 1
amis19$N_cai_sps_12[amis19$cai_12_new == 1 & amis19$m_mp12oanum == 1] <- 1
amis19$N_cai_sps_12[amis19$cai_12_new == 1 & amis19$m_mp12manum == 1] <- 1

round(prop.table(tableNA(amis19$cai_12_new, amis19$N_cai_sps_12), margin = 1)*100,1) #6% NA among those who report CAI p12

amis19_Ncai_NA <- amis19[amis19$cai_12_new == 1 & is.na(amis19$N_cai_sps_12),]

round(prop.table(tableNA(amis19_Ncai_NA$m_mp12oanum))*100,1)
round(prop.table(tableNA(amis19_Ncai_NA$m_mp12manum))*100,1)
round(prop.table(tableNA(amis19_Ncai_NA$m_mp12anum))*100,1)

# Any AI p12 -------------------------

# * * 2013 -----
amis13$ai_12 <- NA

# * * 2014 -----
amis14$ai_12[amis14$m_mp12type == 2 | amis14$m_mp12type == 3] <- 1
amis14$ai_12[amis14$m_mp12type == 1 | amis14$m_mp12type == 4] <- 0

# * * 2015 -----
amis15$ai_12 <- ifelse(amis15$msmp12m_anal == 1, 1, 0)

# * * 2016 -----
amis16$ai_12 <- ifelse(amis16$msmp12m_anal == 1, 1, 0)

# * * 2017 -----
amis17$ai_12 <- ifelse(amis17$msmp12m_anal == 1, 1, 0)

# * * 2018 -----
amis18$ai_12 <- ifelse(amis18$msmp12m_anal == 1, 1, 0)

# * * 2019 -----
amis19$ai_12 <- ifelse(amis19$msmp12m_anal == 1, 1, 0)

# N total AI SPs p12 ----------------

# * * 2013 -----
amis13$n_ai_sp <- NA

# * * 2014 -----
amis14$n_ai_sp <- ifelse(is.na(amis14$m_mp12anum), amis14$m_mp12manum, amis14$m_mp12anum)
amis14$n_ai_sp[amis14$ai_12 == 0] <- 0

# * * 2015 -----
amis15$n_ai_sp <- ifelse(is.na(amis15$m_mp12anum), amis15$m_mp12manum, amis15$m_mp12anum)
amis15$n_ai_sp[amis15$ai_12 == 0] <- 0

# * * 2016 -----
amis16$n_ai_sp <- ifelse(is.na(amis16$m_mp12anum), amis16$m_mp12manum, amis16$m_mp12anum)
amis16$n_ai_sp[amis16$ai_12 == 0] <- 0

# * * 2017 -----
amis17$n_ai_sp <- ifelse(is.na(amis17$m_mp12anum), amis17$m_mp12manum, amis17$m_mp12anum)
amis17$n_ai_sp[amis17$ai_12 == 0] <- 0

# * * 2018 -----
amis18$n_ai_sp <- ifelse(is.na(amis18$m_mp12anum), amis18$m_mp12manum, amis18$m_mp12anum)
amis18$n_ai_sp[amis18$ai_12 == 0] <- 0

# * * 2019 -----

amis19$n_ai_sp <- ifelse(is.na(amis19$m_mp12anum), amis19$m_mp12manum, amis19$m_mp12anum)
amis19$n_ai_sp[amis19$ai_12 == 0] <- 0

# N total CAI SPs p12 ---------------
# * * 2013 -----
amis13$n_cai_sp <- NA

# * * 2014 -----
amis14$n_cai_sp <- ifelse(is.na(amis14$m_m1uasnum1), amis14$m_m1uasnum2, amis14$m_m1uasnum1)
amis14$n_cai_sp[amis14$n_ai_sp == 0] <- 0
amis14$n_cai_sp[amis14$ai_12 == 0] <- 0

# * * 2015 -----
amis15$n_cai_sp <- ifelse(is.na(amis15$m_m1uasnum1), amis15$m_m1uasnum2, amis15$m_m1uasnum1)
amis15$n_cai_sp[amis15$n_ai_sp == 0] <- 0
amis15$n_cai_sp[amis15$ai_12 == 0] <- 0

# * * 2016 -----
amis16$n_cai_sp <- ifelse(is.na(amis16$m_m1uasnum1), amis16$m_m1uasnum2, amis16$m_m1uasnum1)
amis16$n_cai_sp[amis16$n_ai_sp == 0] <- 0
amis16$n_cai_sp[amis16$ai_12 == 0] <- 0

# * * 2017 -----
amis17$n_cai_sp <- ifelse(is.na(amis17$m_m1uasnum1), amis17$m_m1uasnum2, amis17$m_m1uasnum1)
amis17$n_cai_sp[amis17$n_ai_sp == 0] <- 0
amis17$n_cai_sp[amis17$ai_12 == 0] <- 0

# * * 2018 -----
amis18$n_cai_sp <- ifelse(is.na(amis18$m_m1uasnum1), amis18$m_m1uasnum2, amis18$m_m1uasnum1)
amis18$n_cai_sp[amis18$n_ai_sp == 0] <- 0
amis18$n_cai_sp[amis18$ai_12 == 0] <- 0

# * * 2019 -----
amis19$n_cai_sp <- ifelse(is.na(amis19$m_m1uasnum1), amis19$m_m1uasnum2, amis19$m_m1uasnum1)
amis19$n_cai_sp[amis19$n_ai_sp == 0] <- 0
amis19$n_cai_sp[amis19$ai_12 == 0] <- 0

##### Merge the year-by-year datasets with `cai_12_new` variable -----

amis <- rbind(amis13,
              amis14,
              amis15,
              amis16,
              amis17,
              amis18,
              amis19)

round(prop.table(tableNA(amis$cai_12_new, amis$year), margin = 2)*100,1)
round(prop.table(table(amis$cai_12_new, amis$year), margin = 2)*100,1)
round(prop.table(tableNA(amis$ai_12, amis$year), margin = 2)*100,1)
round(prop.table(tableNA(amis$N_cai_sps_12, amis$year), margin = 2)*100,1)

# Generate condom frequency variable ----
amis$condom_use_cat <- NA

amis$condom_use_cat[amis$ai_12 == 0] <- "1_noAI"
amis$condom_use_cat[amis$ai_12 == 1 & amis$cai_12_new == 0] <- "2_always"
amis$condom_use_cat[amis$ai_12 == 1 & amis$cai_12_new == 1 & (amis$n_ai_sp >= amis$n_cai_sp)] <- "3_sometimes"
amis$condom_use_cat[amis$ai_12 == 1 & amis$cai_12_new == 1 & (amis$n_ai_sp == amis$n_cai_sp)] <- "4_never"

round(prop.table(table(amis$condom_use_cat, amis$year), margin=2)*100,0)


# * OLD:: N_cai_sps_12 -----
# NOTE: what is a reasonable cut-off for number of SPs, or number of CAI SPs? -----
# lots <- subset(amis, m_m1uasnum2 >= 365)
# 
# table(amis$m_m1uasnum1, amis$year, useNA = "ifany")
# table(amis$m_m1uasnum2, amis$year, useNA = "ifany")
# 
# sum(rowSums(is.na(cbind(amis$m_m1uasnum1, amis$m_m1uasnum2)))==0) # Confirms that nobody has a numerical answer to both

# amis$N_cai_sps_12 <- NA
# 
# amis$N_cai_sps_12 <- ifelse(!is.na(amis$m_m1uasnum1), amis$m_m1uasnum1, amis$m_m1uasnum2)
# table(amis$N_cai_sps_12, amis$m_m1uasnum1, useNA = "ifany")
# table(amis$N_cai_sps_12, amis$m_m1uasnum2, useNA = "ifany")
# 
# amis$N_cai_sps_12 <- as.numeric(amis$N_cai_sps_12) #make numeric

### Apply "0" to `N_cai_sps_12` after other variables are calculated -----


# CAI partner variables -----
tableNA(amis$m_m1uas) #CAI any
tableNA(amis$m_muahn) #CAI w neg
tableNA(amis$m_muahp) #CAI w pos
tableNA(amis$m_muauhs) #CAI w unknown status

# new variables to apply logic to:
amis$cai_neg_12 <- NA
amis$cai_pos_12 <- NA
amis$cai_unk_12 <- NA

tableNA(amis$sextype_a_12)

# * cai_neg_12 -----
tableNA(amis$cai_neg_12) # check breakdown

amis$cai_neg_12[amis$m_muahn == 1] <- 1
amis$cai_neg_12[amis$m_muahn == 0] <- 0
#no to any CAI becomes "no" to CAI w neg
amis$cai_neg_12[amis$m_m1uas == 0] <- 0 #moved >10k from NA to 0
#no to any AI becomes "no" w neg
amis$cai_neg_12[amis$sextype_a_12 == 0] <- 0 # >14k from NA to 0
# no to bottom and top p12 becomes "no" to CAI w pos
amis$cai_neg_12[amis$top_12 == 0 & amis$bottom_12 == 0] <- 0 #24 NA to 0
#yes to CAI but no to CAI w pos and no to CAI of unk becomes "yes" to CAI w neg
amis$cai_neg_12[amis$m_m1uas == 1 & amis$m_muahp == 0 & amis$m_muauhs == 0] <- 1 #moved 647 from NA to 1
#yes to CAI with LSP who was HIV-neg <1 year ago becomes "yes" to CAI w neg
amis$cai_neg_12[amis$lsp_lse_lt1yr == 1 & amis$lsp_hiv_status == "1_neg" & ((amis$lsp_btm == 1 & (amis$lsp_btm_condom_any == 0 | amis$lsp_btm_condom_entire ==0)) |
                                              (amis$lsp_top == 1 & (amis$lsp_top_condom_any == 0 | amis$lsp_top_condom_entire ==0)))] <- 1 #385 from NA to 1


# * cai_pos_12 -----
tableNA(amis$cai_pos_12) # check breakdown

amis$cai_pos_12[amis$m_muahp == 1] <- 1
amis$cai_pos_12[amis$m_muahp == 0] <- 0
#no to any CAI becomes "no" to CAI w pos
amis$cai_pos_12[amis$m_m1uas == 0] <- 0 #moved >10k from NA to 0
#no to any CAI becomes "no" to CAI w pos
amis$cai_pos_12[amis$sextype_a_12 == 0] <- 0 # >14k from NA to 0
# no to bottom and top p12 becomes "no" to CAI w pos
amis$cai_pos_12[amis$top_12 == 0 & amis$bottom_12 == 0] <- 0 #26 went from NA to 0
#yes to CAI but no to CAI w neg and no to CAI of unk becomes "yes" to CAI w pos
amis$cai_pos_12[amis$m_m1uas == 1 & amis$m_muahn == 0 & amis$m_muauhs == 0] <- 1 
#yes to CAI with LSP who was HIV-pos <1 year ago becomes "yes" to CAI w pos
amis$cai_pos_12[amis$lsp_lse_lt1yr == 1 & amis$lsp_hiv_status == "2_pos" & ((amis$lsp_btm == 1 & (amis$lsp_btm_condom_any == 0 | amis$lsp_btm_condom_entire ==0)) |
                                                                               (amis$lsp_top == 1 & (amis$lsp_top_condom_any == 0 | amis$lsp_top_condom_entire ==0)))] <- 1 #>150 from NA to 1

# * cai_unk_12 -----
prop.table(tableNA(amis$cai_unk_12))

amis$cai_unk_12 <- NA

amis$cai_unk_12[amis$m_muauhs == 1] <- 1
amis$cai_unk_12[amis$m_muauhs == 0] <- 0
#no to any CAI becomes "no" to CAI w unk HIV stat
amis$cai_unk_12[amis$m_m1uas == 0] <- 0 #moved >10k from NA to 0
amis$cai_unk_12[amis$cai_12_new == 0] <- 0 #moved >10k from NA to 0
#no to any CAI becomes "no" to CAI w unk
amis$cai_unk_12[amis$sextype_a_12 == 0] <- 0 # >1400 from NA to 0
# no to bottom and top p12 becomes "no" to CAI w pos
amis$cai_unk_12[amis$top_12 == 0 & amis$bottom_12 == 0] <- 0 #23 NA to 0
# among those with a single SP, if that SP HIV stat is unk, but yes to CAI, CAI with unk becomes yes.
amis$cai_unk_12[amis$cai_12_new == 1 & amis$m_mm1hsk == 0] <- 1 #656 NA to yes
# among those with a single SP, if that SP HIV stat is known but ind, but yes to CAI, CAI with unk becomes yes.
amis$cai_unk_12[amis$cai_12_new == 1 & amis$m_m1hst == 3] <- 1 #10 NA to yes
# #yes to CAI but no to CAI with neg and no to CAI with pos becomes "yes" to CAI with unk
# amis$cai_unk_12[amis$m_m1uas == 1 & amis$m_muahn == 0 & amis$m_muahp == 0] <- 1 #moved 14 from NA to 1 ##Others moved from 0??
# removed this chunk; it's sus
#yes to CAI with LSP who was HIV-unk <1 year ago becomes "yes" to CAI w unk
amis$cai_unk_12[amis$lsp_lse_lt1yr == 1 & (amis$lsp_hiv_status == "3_ind" | amis$lsp_know_status == 0) & ((amis$lsp_btm == 1 & (amis$lsp_btm_condom_any == 0 | amis$lsp_btm_condom_entire ==0)) |
                                                                               (amis$lsp_top == 1 & (amis$lsp_top_condom_any == 0 | amis$lsp_top_condom_entire ==0)))] <- 1 #>200 from NA to 1



round(prop.table(tableNA(amis$cai_neg_12))*100,1) #24% NA
round(prop.table(tableNA(amis$cai_pos_12))*100,1) #27% NA
round(prop.table(tableNA(amis$cai_unk_12))*100,1) #21% NA

round(prop.table(tableNA(amis$year, amis$cai_unk_12), margin = 1)*100,1)

# Investigate NAs for CAI with UNK by year -----
unkna_14 <- subset(amis, (year == 2014 & is.na(cai_unk_12)))
unkna_15 <- subset(amis, (year == 2015 & is.na(cai_unk_12)))
unkna_16 <- subset(amis, (year == 2016 & is.na(cai_unk_12)))
unkna_17 <- subset(amis, (year == 2017 & is.na(cai_unk_12)))
unkna_18 <- subset(amis, (year == 2018 & is.na(cai_unk_12)))
unkna_19 <- subset(amis, (year == 2019 & is.na(cai_unk_12)))

ai_14 <- subset(amis, (year == 2014 & (m_mp12type == 2 | m_mp12type == 3)))
ai_15 <- subset(amis, (year == 2015 & (msmp12m_anal == 1)))
ai_16 <- subset(amis, (year == 2016 & (msmp12m_anal == 1)))
ai_17 <- subset(amis, (year == 2017 & (msmp12m_anal == 1)))
ai_18 <- subset(amis, (year == 2018 & (msmp12m_anal == 1)))
ai_19 <- subset(amis, (year == 2019 & (msmp12m_anal == 1)))

# * Get N who endorse CAI
tableNA(ai_14$m_m1uas)
tableNA(ai_15$m_m1uas)
tableNA(ai_16$m_m1uas)
tableNA(ai_17$m_m1uas)
tableNA(ai_18$m_m1uas)
tableNA(ai_19$m_m1uas)


# * Get N who endorse CAI w unk
tableNA(ai_14$m_muauhs)
tableNA(ai_15$m_muauhs)
tableNA(ai_16$m_muauhs)
tableNA(ai_17$m_muauhs)
tableNA(ai_18$m_muauhs)
tableNA(ai_19$m_muauhs)

# Get N with non-missing values for CAI with UNK HIV
sum(!is.na(ai_14$cai_unk_12))
sum(!is.na(ai_15$cai_unk_12))
sum(!is.na(ai_16$cai_unk_12))
sum(!is.na(ai_17$cai_unk_12))
sum(!is.na(ai_18$cai_unk_12))
sum(!is.na(ai_19$cai_unk_12))


amis13 <- subset(amis, year == 2013)
amis14 <- subset(amis, year == 2014)
amis15 <- subset(amis, year == 2015)
amis16 <- subset(amis, year == 2016)
amis17 <- subset(amis, year == 2017)
amis18 <- subset(amis, year == 2018)
amis19 <- subset(amis, year == 2019)

# CAI w neg, by year
round(prop.table(tableNA(amis13$cai_neg_12))*100,1) #0% NA
round(prop.table(tableNA(amis14$cai_neg_12))*100,1) #31% NA
round(prop.table(tableNA(amis15$cai_neg_12))*100,1) #30% NA
round(prop.table(tableNA(amis16$cai_neg_12))*100,1) #34% NA
round(prop.table(tableNA(amis17$cai_neg_12))*100,1) #31% NA
round(prop.table(tableNA(amis18$cai_neg_12))*100,1) #30% NA
round(prop.table(tableNA(amis19$cai_neg_12))*100,1) #24% NA

# CAI with pos, by year
round(prop.table(tableNA(amis13$cai_pos_12))*100,1) #0% NA
round(prop.table(tableNA(amis14$cai_pos_12))*100,1) #35% NA
round(prop.table(tableNA(amis15$cai_pos_12))*100,1) #33% NA
round(prop.table(tableNA(amis16$cai_pos_12))*100,1) #34% NA
round(prop.table(tableNA(amis17$cai_pos_12))*100,1) #34% NA
round(prop.table(tableNA(amis18$cai_pos_12))*100,1) #28% NA
round(prop.table(tableNA(amis19$cai_pos_12))*100,1) #26% NA

# CAI w unk, by year
round(prop.table(tableNA(amis13$cai_unk_12))*100,1) #0% NA
round(prop.table(tableNA(amis14$cai_unk_12))*100,1) #29% NA
round(prop.table(tableNA(amis15$cai_unk_12))*100,1) #27% NA
round(prop.table(tableNA(amis16$cai_unk_12))*100,1) #32% NA
round(prop.table(tableNA(amis17$cai_unk_12))*100,1) #27% NA
round(prop.table(tableNA(amis18$cai_unk_12))*100,1) #22% NA
round(prop.table(tableNA(amis19$cai_unk_12))*100,1) #20% NA

# * cai_sp_cat -----
amis$cai_sp_cat <- NA

tableNA(amis$cai_neg_12)
tableNA(amis$cai_pos_12)
tableNA(amis$cai_unk_12)

amis$cai_sp_cat[amis$cai_12_new == 0] <- "1_xxx"
amis$cai_sp_cat[amis$cai_neg_12 == 0 & amis$cai_pos_12 == 0 & amis$cai_unk_12 == 0] <- "1_xxx"
amis$cai_sp_cat[amis$cai_neg_12 == 1 & amis$cai_pos_12 == 0 & amis$cai_unk_12 == 0] <- "2_Nxx"
amis$cai_sp_cat[amis$cai_neg_12 == 1 & amis$cai_pos_12 == 1 & amis$cai_unk_12 == 0] <- "3_NPx"
amis$cai_sp_cat[amis$cai_neg_12 == 1 & amis$cai_pos_12 == 0 & amis$cai_unk_12 == 1] <- "4_NxU"
amis$cai_sp_cat[amis$cai_neg_12 == 1 & amis$cai_pos_12 == 1 & amis$cai_unk_12 == 1] <- "5_NPU"
amis$cai_sp_cat[amis$cai_neg_12 == 0 & amis$cai_pos_12 == 1 & amis$cai_unk_12 == 0] <- "6_xPx"
amis$cai_sp_cat[amis$cai_neg_12 == 0 & amis$cai_pos_12 == 1 & amis$cai_unk_12 == 1] <- "7_xPU"
amis$cai_sp_cat[amis$cai_neg_12 == 0 & amis$cai_pos_12 == 0 & amis$cai_unk_12 == 1] <- "8_xxU"


#check
round(prop.table(tableNA(amis$cai_sp_cat))*100,1)
tableNA(amis$cai_sp_cat, amis$cai_neg_12)
tableNA(amis$cai_sp_cat, amis$cai_pos_12)
tableNA(amis$cai_sp_cat, amis$cai_unk_12)

# * cai_12
### Waiting to hear from AMIS staff on this
# amis$cai_12 <- amis$uas
# round(prop.table(tableNA(amis$uas, amis$m_m1uas), margin = 1)*100,1) #in the constructed variable, more than half who are "no" were NA to the "have you had CAI p12?"
# 
# tableNA(amis$m_m1uas, amis$m_muahn)
# tableNA(amis$m_m1uas, amis$m_muahp)
# tableNA(amis$m_m1uas, amis$m_muauhs)
# 
# tableNA(amis$uas, amis$m_muahn)
# tableNA(amis$uas, amis$m_muahp)
# tableNA(amis$uas, amis$m_muauhs)


### if you said "no" to any CAI then `N_cai_sps_12` == 0
tableNA(amis$N_cai_sps_12)
amis$N_cai_sps_12[amis$cai_12_new == 0] <- 0

tableNA(amis$N_cai_sps_12, amis$cai_12_new)

round(prop.table(tableNA(amis$N_cai_sps_12, amis$year), margin = 2)*100,1)

# ### anyone who said "ever PrEP" becomes "yes" to PrEP p12 in 2013:2015
# tableNA(amis_biomed$year, amis_biomed$prep_used12)
# tableNA(amis_biomed$year, amis_biomed$prep_used_ever)
# tableNA(amis_biomed$prep_used12, amis_biomed$prep_used_ever)
# amis_biomed$prep_used12[amis_biomed$year <= 2015 & amis_biomed$prep_used_ever == 1] <- 1
# tableNA(amis_biomed$prep_used12, amis_biomed$prep_used_ever) # no change


# amis_biomed$hiv_stat7 <- NA
# 
# amis_biomed$hiv_stat7[amis_biomed$evertest == 0] <- "01_NvrTest"
# amis_biomed$hiv_stat7[(amis_biomed$evertest == 1 | is.na(amis_biomed$evertest)) & amis_biomed$hiv2 == "02_neg" & amis_biomed$prep_used_ever == 0] <- "02_NvrPrEP"
# amis_biomed$hiv_stat7[(amis_biomed$evertest == 1 | is.na(amis_biomed$evertest)) & amis_biomed$hiv2 == "02_neg" & amis_biomed$prep_used12 == 0 & amis_biomed$prep_ever == 1] <- "03_PrEP>12"
# amis_biomed$hiv_stat7[(amis_biomed$evertest == 1 | is.na(amis_biomed$evertest)) & amis_biomed$hiv2 == "02_neg" & amis_biomed$prep_used12 == 1 & (amis_biomed$prep_current == 0 | is.na(amis_biomed$prep_current))] <- "04_PrEP<12"
# amis_biomed$hiv_stat7[(amis_biomed$evertest == 1 | is.na(amis_biomed$evertest)) & amis_biomed$hiv2 == "02_neg" & amis_biomed$prep_current == 1] <- "05_PrEPcur"
# amis_biomed$hiv_stat7[(amis_biomed$evertest == 1 | is.na(amis_biomed$evertest)) & amis_biomed$hiv2 == "01_pos" & amis_biomed$art_current == 1] <- "06_PosOnART"
# amis_biomed$hiv_stat7[(amis_biomed$evertest == 1 | is.na(amis_biomed$evertest)) & amis_biomed$hiv2 == "01_pos" & amis_biomed$art_current == 0] <- "07_PosNoART"
# 
# tableNA(amis_biomed$hiv_stat7)
# round(prop.table(tableNA(amis_biomed$hiv_stat7))*100,2) #removing 2014 un-answerable PrEP reduced NA from 31 to 29%.
# round(prop.table(tableNA(amis_biomed$hiv_stat7, amis_biomed$year), margin = 2)*100,2) # high levels of missingness remain
# 
# # check missingness by variable used in construction.
# tableNA(amis_biomed$hiv2) # no NA
# tableNA(amis_biomed$evertest) # no NA
# tableNA(amis_biomed$prep_used_ever) # NA = 23,112 <--- THIS is the problem variable :)
# 
# tableNA(amis_biomed$prep_used_ever, amis_biomed$prep_used12, amis_biomed$year)
# 
# tableNA(amis_biomed$prep_used12) # no NA
# tableNA(amis_biomed$prep_current) # 716 NA
# 
# # * whoever says "no" to PrEP p12 becomes "never PrEP" in 2013:2015 -----
# amis_biomed$prep_used_ever[amis_biomed$prep_used12 == 0 & amis_biomed$year <= 2015 & (amis_biomed$prep_current == 0 | is.na(amis_biomed$prep_current))] <- 0

# * * Re-run seven-level HIV stat/Biomed var code -----
# * biomed_traj7 -----
amis$biomed_traj7 <- NA

amis$biomed_traj7[amis$evertest == 0] <- "01_NvrTest"
amis$biomed_traj7[(amis$evertest == 1 |
                      is.na(amis$evertest)) &
                     amis$hiv2 == "02_neg" & amis$prep_useever_new == 0] <- "02_NvrPrEP"
amis$biomed_traj7[(amis$evertest == 1 |
                      is.na(amis$evertest)) &
                     amis$hiv2 == "02_neg" &
                     amis$prep_use12_new == 0 &
                     amis$prep_useever_new == 1] <- "03_PrEP>12"
amis$biomed_traj7[(amis$evertest == 1 |
                      is.na(amis$evertest)) &
                     amis$hiv2 == "02_neg" &
                     amis$prep_use12_new == 1 &
                     (amis$prep_usecur_new == 0 |
                         is.na(amis$prep_usecur_new))] <- "04_PrEP<12"
amis$biomed_traj7[(amis$evertest == 1 |
                      is.na(amis$evertest)) &
                     amis$hiv2 == "02_neg" & amis$prep_usecur_new == 1] <- "05_PrEPcur"
amis$biomed_traj7[(amis$evertest == 1 |
                      is.na(amis$evertest)) &
                     amis$hiv2 == "01_pos" & amis$art_current == 1] <- "06_PosOnART"
amis$biomed_traj7[(amis$evertest == 1 |
                      is.na(amis$evertest)) &
                     amis$hiv2 == "01_pos" & amis$art_current == 0] <- "07_PosNoART"

prop.table(tableNA(amis$biomed_traj7))*100
round(prop.table(tableNA(amis$biomed_traj7))*100,2) #assuming that people who said "no" to p12 PrEP in '13, '14', and '15 are NEVER PrEP drops NAs down to 17%
round(prop.table(tableNA(amis$biomed_traj7, amis$year), margin = 2)*100,2) # high levels of missingness remain

round(prop.table(tableNA(amis$prep_useever_new, amis$year), margin = 2)*100,2) # 2016-2018
round(prop.table(tableNA(amis$prep_usecur_new, amis$year), margin = 2)*100,2) # minimal missingness
round(prop.table(tableNA(amis$prep_use12_new, amis$year), margin = 2)*100,2) # none missing
round(prop.table(tableNA(amis$prep_aware_new, amis$year), margin = 2)*100,2) # none missing

NA16 <- subset(amis, (year == 2016 & is.na(biomed_traj7)))
NA17 <- subset(amis, (year == 2017 & is.na(biomed_traj7)))
NA18 <- subset(amis, (year == 2018 & is.na(biomed_traj7)))
NA14 <- subset(amis, (year == 2014 & is.na(prep_aware_new)))

# * biomed_traj5 ----
amis$biomed_traj5 <- NA

amis$biomed_traj5[amis$evertest == 0] <- "01_NvrTest"
amis$biomed_traj5[(amis$evertest == 1 | is.na(amis$evertest)) &
                     amis$hiv2 == "02_neg" & (amis$prep_useever_new == 0 | is.na(amis$prep_useever_new))] <- "02_never-or-noPrEP12"
amis$biomed_traj5[(amis$evertest == 1 |
                      is.na(amis$evertest)) &
                     amis$hiv2 == "02_neg" &
                     amis$prep_use12_new == 1 &
                     (amis$prep_usecur_new == 0 |
                         is.na(amis$prep_usecur_new))] <- "03_usedPrEP<12"
amis$biomed_traj5[amis$hiv2 == "02_neg" & amis$prep_usecur_new == 1] <- "04_PrEPcur"
amis$biomed_traj5[amis$hiv2 == "01_pos"] <- "05_pos"

round(prop.table(tableNA(amis$biomed_traj5))*100,2)
tableNA(amis$biomed_traj5, amis$year)

# Why NAs?
biomed_traj5NA <- subset(amis, is.na(amis$biomed_traj5))

tableNA(biomed_traj5NA$hiv2) # 100% HIV-neg
tableNA(biomed_traj5NA$heard_prep)
tableNA(biomed_traj5NA$prep_used)
tableNA(biomed_traj5NA$prep_current)

#every NA is aware of PrEP but has NOT used PrEP in the last 12 mo.
amis$biomed_traj5[is.na(amis$biomed_traj5)] <- "02_never-or-noPrEP12"

tableNA(amis$biomed_traj5, amis$year) # no more NA!!!


# * biomed_traj4 -----
amis$biomed_traj4 <- NA

amis$biomed_traj4[amis$biomed_traj5 == "01_NvrTest" | amis$biomed_traj5 == "02_never-or-noPrEP12"] <- "01_never-or-no-PrEP12"
amis$biomed_traj4[amis$biomed_traj5 == "03_usedPrEP<12"] <- "02_usedPrEP<12"
amis$biomed_traj4[amis$biomed_traj5 == "04_PrEPcur"] <- "03_PrEPcur"
amis$biomed_traj4[amis$biomed_traj5 == "05_pos"] <- "04_pos"

tableNA(amis$biomed_traj4)

# * neg_prep12_use_groups -----
amis$neg_prep12_use_groups <- NA
amis$neg_prep12_use_groups[amis$biomed_traj4 == "01_never-or-no-PrEP12"] <- "01_never-or-no-PrEP12"
amis$neg_prep12_use_groups[amis$biomed_traj5 == "04_PrEPcur"]  <- "02_PrEPcur"

tableNA(amis$biomed_traj4,amis$neg_prep12_use_groups)
sum(!is.na(amis$neg_prep12_use_groups)) #N=58,349 (denom)

# * biomed_traj3 ----
amis$biomed_traj3 <- NA

amis$biomed_traj3[amis$hiv2 == "01_pos"] <- "03_Pos"
amis$biomed_traj3[amis$hiv2 == "02_neg" & amis$prep_usecur_new == 1] <- "02_NegYOP"
amis$biomed_traj3[amis$hiv2 == "02_neg" & amis$prep_usecur_new == 0] <- "01_NegNOP"

tableNA(amis$biomed_traj3)
round(prop.table(tableNA(amis$biomed_traj3))*100,2) #< Ω1% missing!

### Figure S1 - biomed/HIV stat breakdown by year, 14-19 --------
amis_14_19 <- subset(amis, year >= 2014)
tableNA(amis_14_19$biomed_traj3, amis_14_19$neg_prep12_use_groups)

amis_14_19$sf1_group <- NA
amis_14_19$sf1_group[amis_14_19$neg_prep12_use_groups == "01_never-or-no-PrEP12"] <- "01_HNNP"
amis_14_19$sf1_group[amis_14_19$neg_prep12_use_groups == "02_PrEPcur"] <- "02_PrEPcur"
amis_14_19$sf1_group[amis_14_19$biomed_traj3 == "03_Pos"] <- "03_Pos"
amis_14_19$sf1_group[is.na(amis_14_19$neg_prep12_use_groups) & amis_14_19$biomed_traj3 == "01_NegNOP"] <- "04_NegUNK"

table(amis_14_19$year, amis_14_19$sf1_group)

round(prop.table(table(amis_14_19$sf1_group))*100,0)


#### Match up ID & last ID -----
amis$match_id_1415 <- NA
amis$match_id_1516 <- NA
amis$match_id_1617 <- NA
amis$match_id_1718 <- NA
amis$match_id_1819 <- NA

amis14 <- subset(amis, year == 2014)
amis15 <- subset(amis, year == 2015)
amis16 <- subset(amis, year == 2016)
amis17 <- subset(amis, year == 2017)
amis18 <- subset(amis, year == 2018)
amis19 <- subset(amis, year == 2019)


### match_id_x variable match-up

amis14$match_id_1415 <- amis14$respondent_id
amis15$match_id_1415 <- amis15$lastid2014
tableNA(amis15$lastid2014) #check

amis15$match_id_1516 <- amis15$respondent_id
amis16$match_id_1516 <- amis16$lastid2015
tableNA(amis16$lastid2015) #check

amis16$match_id_1617 <- amis16$respondent_id
amis17$match_id_1617 <- amis17$lastid2016
tableNA(amis17$lastid2016) #check

amis17$match_id_1718 <- amis17$respondent_id
amis18$match_id_1718 <- amis18$lastid2017
tableNA(amis18$lastid2017) #check

amis18$match_id_1819 <- amis18$respondent_id
amis19$match_id_1819 <- amis19$lastid2018
tableNA(amis19$lastid2018) #check

# * Match datasets on "match_id_x" using inner_join -----
amis1415 <- inner_join(amis14,
                       amis15,
                       by = "match_id_1415",
                       copy = F,
                       suffix = c(".14", ".15"),
                       keep = T)

amis1516 <- inner_join(amis15,
                       amis16,
                       by = "match_id_1516",
                       copy = F,
                       suffix = c(".15", ".16"),
                       keep = T)

amis1617 <- inner_join(amis16,
                       amis17,
                       by = "match_id_1617",
                       copy = F,
                       suffix = c(".16", ".17"),
                       keep = T)

amis1718 <- inner_join(amis17,
                       amis18,
                       by = "match_id_1718",
                       copy = F,
                       suffix = c(".17", ".18"),
                       keep = T)

amis1819 <- inner_join(amis18,
                       amis19,
                       by = "match_id_1819",
                       copy = F,
                       suffix = c(".18", ".19"),
                       keep = T)


### Merge all years for combined analyses -----
amis1415_match <- inner_join(amis14,
                       amis15,
                       by = "match_id_1415",
                       copy = F,
                       suffix = c(".1", ".2"),
                       keep = T)

amis1516_match <- inner_join(amis15,
                       amis16,
                       by = "match_id_1516",
                       copy = F,
                       suffix = c(".1", ".2"),
                       keep = T)

amis1617_match <- inner_join(amis16,
                       amis17,
                       by = "match_id_1617",
                       copy = F,
                       suffix = c(".1", ".2"),
                       keep = T)

amis1718_match <- inner_join(amis17,
                       amis18,
                       by = "match_id_1718",
                       copy = F,
                       suffix = c(".1", ".2"),
                       keep = T)

amis1819_match <- inner_join(amis18,
                       amis19,
                       by = "match_id_1819",
                       copy = F,
                       suffix = c(".1", ".2"),
                       keep = T)

amis_two_match <- rbind(amis1415_match,
                        amis1516_match,
                        amis1617_match,
                        amis1718_match,
                        amis1819_match)
###### REMOVE ######
###### Remove people who linked to multiple records in year 2 ----------------------------------
###### REMOVE ######

# no duplicates in 14-15.

table(tableNA(amis1516_match$lastid2015.2))

# duplicates in 15-16: 381, 1508, 25273, 40457
amis1516_match %>% count(lastid2015.2, sort = T)

amis1516_match <- subset(amis1516_match, lastid2015.2 != "381" & 
                                          lastid2015.2 != "1508" &
                                          lastid2015.2 != "25273" &
                                          lastid2015.2 != "40457")

# duplicates in 16-17: 8675, 11445, 25555
amis1617_match %>% count(lastid2016.2, sort = T)

amis1617_match <- subset(amis1617_match, lastid2016.2 != "8675" & 
                            lastid2016.2 != "11445" &
                            lastid2016.2 != "25555")

# duplicates in 17-18:
amis1718_match %>% count(lastid2017.2, sort = T)

amis1718_match <- subset(amis1718_match, lastid2017.2 != "231793" &
                            lastid2017.2 != "268854" &
                            lastid2017.2 != "276218" &
                            lastid2017.2 != "276619" &
                            lastid2017.2 != "291244")

#duplicates in 18-19:
amis1819_match %>% count(lastid2018.2, sort = T)

amis1819_match <- subset(amis1819_match, lastid2018.2 != "40245" &
                            lastid2018.2 != "43269" &
                            lastid2018.2 != "47262" &
                            lastid2018.2 != "47958" &
                            lastid2018.2 != "75913")

amis_two_match <- rbind(amis1415_match,
                        amis1516_match,
                        amis1617_match,
                        amis1718_match,
                        amis1819_match)

# write.csv(amis_two_match, "amis_2_year.csv")

#### time series analyses for the 6 PrEP categories -----

# * 2014-2015 -----
# * * Non-PrEP persisters -----
A1415 <- subset(amis1415, (biomed_traj3.14 == "01_NegNOP" & biomed_traj3.15 == "01_NegNOP"))
# * * * % reporting CAI
round(prop.table(tableNA(A1415$cai_12_new.14))*100,1)
round(prop.table(tableNA(A1415$cai_12_new.15))*100,1)
# * * * median N CAI SPs
summary(A1415$N_cai_sps_12.14)
summary(A1415$N_cai_sps_12.15)

# * * PrEP starters -----
B1415 <- subset(amis1415, (biomed_traj3.14 == "01_NegNOP" & biomed_traj3.15 == "02_NegYOP"))
# * * * % reporting CAI
round(prop.table(tableNA(B1415$cai_12_new.14))*100,1)
round(prop.table(tableNA(B1415$cai_12_new.15))*100,1)
# * * * median N CAI SPs
summary(B1415$N_cai_sps_12.14)
summary(B1415$N_cai_sps_12.15)

# * * PrEP droppers -----
C1415 <- subset(amis1415, (biomed_traj3.14 == "02_NegYOP" & biomed_traj3.15 == "01_NegNOP"))
# * * * % reporting CAI
round(prop.table(tableNA(C1415$cai_12_new.14))*100,1)
round(prop.table(tableNA(C1415$cai_12_new.15))*100,1)
# * * * median N CAI SPs
summary(C1415$N_cai_sps_12.14)
summary(C1415$N_cai_sps_12.15)

# * * PrEP persisters -----
D1415 <- subset(amis1415, (biomed_traj3.14 == "02_NegYOP" & biomed_traj3.15 == "02_NegYOP"))
# * * * % reporting CAI
round(prop.table(tableNA(D1415$cai_12_new.14))*100,1)
round(prop.table(tableNA(D1415$cai_12_new.15))*100,1)
# * * * median N CAI SPs
summary(D1415$N_cai_sps_12.14)
summary(D1415$N_cai_sps_12.15)

# * * Non-PrEP seroconverters -----
# none for this year

# * * PrEP seroconverters -----
# none for this year



# * 2015-2016 -----
tableNA(amis1516$biomed_traj3.15, amis1516$biomed_traj3.16) #N for each group
# * * Non-PrEP persisters -----
A1516 <- subset(amis1516, (biomed_traj3.15 == "01_NegNOP" & biomed_traj3.16 == "01_NegNOP"))
# * * * % reporting CAI
round(prop.table(tableNA(A1516$cai_12_new.15))*100,1)
round(prop.table(tableNA(A1516$cai_12_new.16))*100,1)
# * * * median N CAI SPs
summary(A1516$N_cai_sps_12.15)
summary(A1516$N_cai_sps_12.16)

# * * PrEP starters -----
B1516 <- subset(amis1516, (biomed_traj3.15 == "01_NegNOP" & biomed_traj3.16 == "02_NegYOP"))
# * * * % reporting CAI
round(prop.table(tableNA(B1516$cai_12_new.15))*100,1)
round(prop.table(tableNA(B1516$cai_12_new.16))*100,1)
# * * * median N CAI SPs
summary(B1516$N_cai_sps_12.15)
summary(B1516$N_cai_sps_12.16)

# * * PrEP droppers -----
C1516 <- subset(amis1516, (biomed_traj3.15 == "02_NegYOP" & biomed_traj3.16 == "01_NegNOP"))
# * * * % reporting CAI
round(prop.table(tableNA(C1516$cai_12_new.15))*100,1)
round(prop.table(tableNA(C1516$cai_12_new.16))*100,1)
# * * * median N CAI SPs
summary(C1516$N_cai_sps_12.15)
summary(C1516$N_cai_sps_12.16)

# * * PrEP persisters -----
D1516 <- subset(amis1516, (biomed_traj3.15 == "02_NegYOP" & biomed_traj3.16 == "02_NegYOP"))
# * * * % reporting CAI
round(prop.table(tableNA(D1516$cai_12_new.15))*100,1)
round(prop.table(tableNA(D1516$cai_12_new.16))*100,1)
# * * * median N CAI SPs
summary(D1516$N_cai_sps_12.15)
summary(D1516$N_cai_sps_12.16)

# * * Non-PrEP seroconverters -----
E1516 <- subset(amis1516, (biomed_traj3.15 == "01_NegNOP" & biomed_traj3.16 == "03_Pos"))
# * * * % reporting CAI
round(prop.table(tableNA(E1516$cai_12_new.15))*100,1)
round(prop.table(tableNA(E1516$cai_12_new.16))*100,1)
# * * * median N CAI SPs
summary(E1516$N_cai_sps_12.15)
summary(E1516$N_cai_sps_12.16)

# * * PrEP seroconverters -----
F1516 <- subset(amis1516, (biomed_traj3.15 == "02_NegYOP" & biomed_traj3.16 == "03_Pos"))
# * * * % reporting CAI
round(prop.table(tableNA(F1516$cai_12_new.15))*100,1)
round(prop.table(tableNA(F1516$cai_12_new.16))*100,1)
# * * * median N CAI SPs
summary(F1516$N_cai_sps_12.15)
summary(F1516$N_cai_sps_12.16)


# * 2016-2017 -----
tableNA(amis1617$biomed_traj3.16, amis1617$biomed_traj3.17) #N for each group
# * * Non-PrEP persisters -----
A1617 <- subset(amis1617, (biomed_traj3.16 == "01_NegNOP" & biomed_traj3.17 == "01_NegNOP"))
# * * * % reporting CAI
round(prop.table(tableNA(A1617$cai_12_new.16))*100,1)
round(prop.table(tableNA(A1617$cai_12_new.17))*100,1)
# * * * median N CAI SPs
summary(A1617$N_cai_sps_12.16)
summary(A1617$N_cai_sps_12.17)

# * * PrEP starters -----
B1617 <- subset(amis1617, (biomed_traj3.16 == "01_NegNOP" & biomed_traj3.17 == "02_NegYOP"))
# * * * % reporting CAI
round(prop.table(tableNA(B1617$cai_12_new.16))*100,1)
round(prop.table(tableNA(B1617$cai_12_new.17))*100,1)
# * * * median N CAI SPs
summary(B1617$N_cai_sps_12.16)
summary(B1617$N_cai_sps_12.17)

# * * PrEP droppers -----
C1617 <- subset(amis1617, (biomed_traj3.16 == "02_NegYOP" & biomed_traj3.17 == "01_NegNOP"))
# * * * % reporting CAI
round(prop.table(tableNA(C1617$cai_12_new.16))*100,1)
round(prop.table(tableNA(C1617$cai_12_new.17))*100,1)
# * * * median N CAI SPs
summary(C1617$N_cai_sps_12.16)
summary(C1617$N_cai_sps_12.17)

# * * PrEP persisters -----
D1617 <- subset(amis1617, (biomed_traj3.16 == "02_NegYOP" & biomed_traj3.17 == "02_NegYOP"))
# * * * % reporting CAI
round(prop.table(tableNA(D1617$cai_12_new.16))*100,1)
round(prop.table(tableNA(D1617$cai_12_new.17))*100,1)
# * * * median N CAI SPs
summary(D1617$N_cai_sps_12.16)
summary(D1617$N_cai_sps_12.17)

# * * Non-PrEP seroconverters -----
E1617 <- subset(amis1617, (biomed_traj3.16 == "01_NegNOP" & biomed_traj3.17 == "03_Pos"))
# * * * % reporting CAI
round(prop.table(tableNA(E1617$cai_12_new.16))*100,1)
round(prop.table(tableNA(E1617$cai_12_new.17))*100,1)
# * * * median N CAI SPs
summary(E1617$N_cai_sps_12.16)
summary(E1617$N_cai_sps_12.17)

# * * PrEP seroconverters -----
F1617 <- subset(amis1617, (biomed_traj3.16 == "02_NegYOP" & biomed_traj3.17 == "03_Pos"))
# * * * % reporting CAI
round(prop.table(tableNA(F1617$cai_12_new.16))*100,1)
round(prop.table(tableNA(F1617$cai_12_new.17))*100,1)
# * * * median N CAI SPs
summary(F1617$N_cai_sps_12.16)
summary(F1617$N_cai_sps_12.17)


# * 2017-2018 -----
tableNA(amis1718$biomed_traj3.17, amis1718$biomed_traj3.18) #N for each group
# * * Non-PrEP persisters -----
A1718 <- subset(amis1718, (biomed_traj3.17 == "01_NegNOP" & biomed_traj3.18 == "01_NegNOP"))
# * * * % reporting CAI
round(prop.table(tableNA(A1718$cai_12_new.17))*100,1)
round(prop.table(tableNA(A1718$cai_12_new.18))*100,1)
# * * * median N CAI SPs
summary(A1718$N_cai_sps_12.17)
summary(A1718$N_cai_sps_12.18)

# * * PrEP starters -----
B1718 <- subset(amis1718, (biomed_traj3.17 == "01_NegNOP" & biomed_traj3.18 == "02_NegYOP"))
# * * * % reporting CAI
round(prop.table(tableNA(B1718$cai_12_new.17))*100,1)
round(prop.table(tableNA(B1718$cai_12_new.18))*100,1)
# * * * median N CAI SPs
summary(B1718$N_cai_sps_12.17)
summary(B1718$N_cai_sps_12.18)

# * * PrEP droppers -----
C1718 <- subset(amis1718, (biomed_traj3.17 == "02_NegYOP" & biomed_traj3.18 == "01_NegNOP"))
# * * * % reporting CAI
round(prop.table(tableNA(C1718$cai_12_new.17))*100,1)
round(prop.table(tableNA(C1718$cai_12_new.18))*100,1)
# * * * median N CAI SPs
summary(C1718$N_cai_sps_12.17)
summary(C1718$N_cai_sps_12.18)

# * * PrEP persisters -----
D1718 <- subset(amis1718, (biomed_traj3.17 == "02_NegYOP" & biomed_traj3.18 == "02_NegYOP"))
# * * * % reporting CAI
round(prop.table(tableNA(D1718$cai_12_new.17))*100,1)
round(prop.table(tableNA(D1718$cai_12_new.18))*100,1)
# * * * median N CAI SPs
summary(D1718$N_cai_sps_12.17)
summary(D1718$N_cai_sps_12.18)

# * * Non-PrEP seroconverters -----
E1718 <- subset(amis1718, (biomed_traj3.17 == "01_NegNOP" & biomed_traj3.18 == "03_Pos"))
# * * * % reporting CAI
round(prop.table(tableNA(E1718$cai_12_new.17))*100,1)
round(prop.table(tableNA(E1718$cai_12_new.18))*100,1)
# * * * median N CAI SPs
summary(E1718$N_cai_sps_12.17)
summary(E1718$N_cai_sps_12.18)

# * * PrEP seroconverters -----
F1718 <- subset(amis1718, (biomed_traj3.17 == "02_NegYOP" & biomed_traj3.18 == "03_Pos"))
# * * * % reporting CAI
round(prop.table(tableNA(F1718$cai_12_new.17))*100,1)
round(prop.table(tableNA(F1718$cai_12_new.18))*100,1)
# * * * median N CAI SPs
summary(F1718$N_cai_sps_12.17)
summary(F1718$N_cai_sps_12.18)

# * 2018-2019 -----
tableNA(amis1819$biomed_traj3.18, amis1819$biomed_traj3.19) #N for each group
# * * Non-PrEP persisters -----
A1819 <- subset(amis1819, (biomed_traj3.18 == "01_NegNOP" & biomed_traj3.19 == "01_NegNOP"))
# * * * % reporting CAI
round(prop.table(tableNA(A1819$cai_12_new.18))*100,1)
round(prop.table(tableNA(A1819$cai_12_new.19))*100,1)
# * * * median N CAI SPs
summary(A1819$N_cai_sps_12.18)
summary(A1819$N_cai_sps_12.19)

# * * PrEP starters -----
B1819 <- subset(amis1819, (biomed_traj3.18 == "01_NegNOP" & biomed_traj3.19 == "02_NegYOP"))
# * * * % reporting CAI
round(prop.table(tableNA(B1819$cai_12_new.18))*100,1)
round(prop.table(tableNA(B1819$cai_12_new.19))*100,1)
# * * * median N CAI SPs
summary(B1819$N_cai_sps_12.18)
summary(B1819$N_cai_sps_12.19)

# * * PrEP droppers -----
C1819 <- subset(amis1819, (biomed_traj3.18 == "02_NegYOP" & biomed_traj3.19 == "01_NegNOP"))
# * * * % reporting CAI
round(prop.table(tableNA(C1819$cai_12_new.18))*100,1)
round(prop.table(tableNA(C1819$cai_12_new.19))*100,1)
# * * * median N CAI SPs
summary(C1819$N_cai_sps_12.18)
summary(C1819$N_cai_sps_12.19)

# * * PrEP persisters -----
D1819 <- subset(amis1819, (biomed_traj3.18 == "02_NegYOP" & biomed_traj3.19 == "02_NegYOP"))
# * * * % reporting CAI
round(prop.table(tableNA(D1819$cai_12_new.18))*100,1)
round(prop.table(tableNA(D1819$cai_12_new.19))*100,1)
# * * * median N CAI SPs
summary(D1819$N_cai_sps_12.18)
summary(D1819$N_cai_sps_12.19)

# * * Non-PrEP seroconverters -----
E1819 <- subset(amis1819, (biomed_traj3.18 == "01_NegNOP" & biomed_traj3.19 == "03_Pos"))
# * * * % reporting CAI
round(prop.table(tableNA(E1819$cai_12_new.18))*100,1)
round(prop.table(tableNA(E1819$cai_12_new.19))*100,1)
# * * * median N CAI SPs
summary(E1819$N_cai_sps_12.18)
summary(E1819$N_cai_sps_12.19)

# * * PrEP seroconverters -----
F1819 <- subset(amis1819, (biomed_traj3.18 == "02_NegYOP" & biomed_traj3.19 == "03_Pos"))
# * * * % reporting CAI
round(prop.table(tableNA(F1819$cai_12_new.18))*100,1)
round(prop.table(tableNA(F1819$cai_12_new.19))*100,1)
# * * * median N CAI SPs
summary(F1819$N_cai_sps_12.18)
summary(F1819$N_cai_sps_12.19)


### Time series analyses for three PrEP trend groups -----
# * install package for McNemar test:
library(exact2x2)

# * Global, all years combined -----
all_off <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12"))
all_start <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "02_PrEPcur"))
all_stayed <- subset(amis_two_match, (neg_prep12_use_groups.1 == "02_PrEPcur" & neg_prep12_use_groups.2 == "02_PrEPcur"))

####### WRITE ALL_OFF DATASET INTO A FILE
write.csv(all_off, "hnm_np_2y.csv")

# * * all participants -----
# stayed off group
round(prop.table(table(all_off$cai_12_new.1))*100,1)
round(prop.table(table(all_off$cai_12_new.2))*100,1)

# exact2x2((table(all_off$cai_12_new.1, all_off$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# started  group
round(prop.table(table(all_start$cai_12_new.1))*100,1)
round(prop.table(table(all_start$cai_12_new.2))*100,1)

# exact2x2((table(all_start$cai_12_new.1, all_start$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# stayed on group
round(prop.table(table(all_stayed$cai_12_new.1))*100,1)
round(prop.table(table(all_stayed$cai_12_new.2))*100,1)

# exact2x2((table(all_stayed$cai_12_new.1, all_stayed$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * <25 -- Global, all years combined -----
all_off_u25 <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & age_25.1 == 0))
all_start_u25 <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "02_PrEPcur" & age_25.1 == 0))
all_stayed_u25 <- subset(amis_two_match, (neg_prep12_use_groups.1 == "02_PrEPcur" & neg_prep12_use_groups.2 == "02_PrEPcur" & age_25.1 == 0))

# stayed off group
round(prop.table(table(all_off_u25$cai_12_new.1))*100,1)
round(prop.table(table(all_off_u25$cai_12_new.2))*100,1)

# exact2x2((table(all_off_u25$cai_12_new.1, all_off_u25$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# started  group
round(prop.table(table(all_start_u25$cai_12_new.1))*100,1)
round(prop.table(table(all_start_u25$cai_12_new.2))*100,1)

# exact2x2((table(all_start_u25$cai_12_new.1, all_start_u25$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# stayed on group
round(prop.table(table(all_stayed_u25$cai_12_new.1))*100,1)
round(prop.table(table(all_stayed_u25$cai_12_new.2))*100,1)

# exact2x2((table(all_stayed_u25$cai_12_new.1, all_stayed_u25$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * >=25 -- Global, all years combined -----
all_off_o25 <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & age_25.1 == 1))
all_start_o25 <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "02_PrEPcur" & age_25.1 == 1))
all_stayed_o25 <- subset(amis_two_match, (neg_prep12_use_groups.1 == "02_PrEPcur" & neg_prep12_use_groups.2 == "02_PrEPcur" & age_25.1 == 1))

# stayed off group
round(prop.table(table(all_off_o25$cai_12_new.1))*100,1)
round(prop.table(table(all_off_o25$cai_12_new.2))*100,1)

# exact2x2((table(all_off_o25$cai_12_new.1, all_off_o25$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# started  group
round(prop.table(table(all_start_o25$cai_12_new.1))*100,1)
round(prop.table(table(all_start_o25$cai_12_new.2))*100,1)

# exact2x2((table(all_start_o25$cai_12_new.1, all_start_o25$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# stayed on group
round(prop.table(table(all_stayed_o25$cai_12_new.1))*100,1)
round(prop.table(table(all_stayed_o25$cai_12_new.2))*100,1)

# exact2x2((table(all_stayed_o25$cai_12_new.1, all_stayed_o25$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * NH White -- Global, all years combined -----
all_off_nhw <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_White"))
all_start_nhw <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "02_PrEPcur" & race_cat.1 == "NH_White"))
all_stayed_nhw <- subset(amis_two_match, (neg_prep12_use_groups.1 == "02_PrEPcur" & neg_prep12_use_groups.2 == "02_PrEPcur" & race_cat.1 == "NH_White"))

# stayed off group
round(prop.table(table(all_off_nhw$cai_12_new.1))*100,1)
round(prop.table(table(all_off_nhw$cai_12_new.2))*100,1)

# exact2x2((table(all_off_nhw$cai_12_new.1, all_off_nhw$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# started  group
round(prop.table(table(all_start_nhw$cai_12_new.1))*100,1)
round(prop.table(table(all_start_nhw$cai_12_new.2))*100,1)

# exact2x2((table(all_start_nhw$cai_12_new.1, all_start_nhw$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# stayed on group
round(prop.table(table(all_stayed_nhw$cai_12_new.1))*100,1)
round(prop.table(table(all_stayed_nhw$cai_12_new.2))*100,1)

# exact2x2((table(all_stayed_nhw$cai_12_new.1, all_stayed_nhw$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * NH Black -- Global, all years combined -----
all_off_nhb <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_Black"))
all_start_nhb <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "02_PrEPcur" & race_cat.1 == "NH_Black"))
all_stayed_nhb <- subset(amis_two_match, (neg_prep12_use_groups.1 == "02_PrEPcur" & neg_prep12_use_groups.2 == "02_PrEPcur" & race_cat.1 == "NH_Black"))

# stayed off group
round(prop.table(table(all_off_nhb$cai_12_new.1))*100,1)
round(prop.table(table(all_off_nhb$cai_12_new.2))*100,1)

# exact2x2((table(all_off_nhb$cai_12_new.1, all_off_nhb$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# started  group
round(prop.table(table(all_start_nhb$cai_12_new.1))*100,1)
round(prop.table(table(all_start_nhb$cai_12_new.2))*100,1)

# exact2x2((table(all_start_nhb$cai_12_new.1, all_start_nhb$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# stayed on group
round(prop.table(table(all_stayed_nhb$cai_12_new.1))*100,1)
round(prop.table(table(all_stayed_nhb$cai_12_new.2))*100,1)

# exact2x2((table(all_stayed_nhb$cai_12_new.1, all_stayed_nhb$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * NH Other -- Global, all years combined -----
all_off_nho <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_Other"))
all_start_nho <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "02_PrEPcur" & race_cat.1 == "NH_Other"))
all_stayed_nho <- subset(amis_two_match, (neg_prep12_use_groups.1 == "02_PrEPcur" & neg_prep12_use_groups.2 == "02_PrEPcur" & race_cat.1 == "NH_Other"))

# stayed off group
round(prop.table(table(all_off_nho$cai_12_new.1))*100,1)
round(prop.table(table(all_off_nho$cai_12_new.2))*100,1)

# exact2x2((table(all_off_nho$cai_12_new.1, all_off_nho$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# started  group
round(prop.table(table(all_start_nho$cai_12_new.1))*100,1)
round(prop.table(table(all_start_nho$cai_12_new.2))*100,1)

# exact2x2((table(all_start_nho$cai_12_new.1, all_start_nho$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# stayed on group
round(prop.table(table(all_stayed_nho$cai_12_new.1))*100,1)
round(prop.table(table(all_stayed_nho$cai_12_new.2))*100,1)

# exact2x2((table(all_stayed_nho$cai_12_new.1, all_stayed_nho$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * Hispanic -- Global, all years combined -----
all_off_hisp <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "Hispanic"))
all_start_hisp <- subset(amis_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "02_PrEPcur" & race_cat.1 == "Hispanic"))
all_stayed_hisp <- subset(amis_two_match, (neg_prep12_use_groups.1 == "02_PrEPcur" & neg_prep12_use_groups.2 == "02_PrEPcur" & race_cat.1 == "Hispanic"))

# stayed off group
round(prop.table(table(all_off_hisp$cai_12_new.1))*100,1)
round(prop.table(table(all_off_hisp$cai_12_new.2))*100,1)

# exact2x2((table(all_off_hisp$cai_12_new.1, all_off_hisp$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# started  group
round(prop.table(table(all_start_hisp$cai_12_new.1))*100,1)
round(prop.table(table(all_start_hisp$cai_12_new.2))*100,1)

# exact2x2((table(all_start_hisp$cai_12_new.1, all_start_hisp$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# stayed on group
round(prop.table(table(all_stayed_hisp$cai_12_new.1))*100,1)
round(prop.table(table(all_stayed_hisp$cai_12_new.2))*100,1)

# # exact2x2((table(all_stayed_hisp$cai_12_new.1, all_stayed_hisp$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * "Stayed off PrEP" group -----
# * * Section 1: All Participants -----
# * * * 2014-2015
off_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12"))
round(prop.table(table(off_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_1415$cai_12_new.15))*100,1)

table(off_1415$cai_12_new.14)
table(off_1415$cai_12_new.15)

# exact2x2((table(off_1415$cai_12_new.14, off_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12"))
round(prop.table(table(off_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_1516$cai_12_new.15, off_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12"))
round(prop.table(table(off_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_1617$cai_12_new.16, off_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12"))
round(prop.table(table(off_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_1718$cai_12_new.17, off_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
off_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12"))
round(prop.table(table(off_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_1819$cai_12_new.18, off_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 2: Ages 15-24 -----
# * * * 2014-2015
off_u25_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age_25.15 == 0 ))
round(prop.table(table(off_u25_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_u25_1415$cai_12_new.15))*100,1)

# exact2x2((table(off_u25_1415$cai_12_new.14, off_u25_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_u25_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age_25.16 == 0 ))
round(prop.table(table(off_u25_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_u25_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_u25_1516$cai_12_new.15, off_u25_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_u25_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age_25.17 == 0 ))
round(prop.table(table(off_u25_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_u25_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_u25_1617$cai_12_new.16, off_u25_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_u25_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age_25.18 == 0 ))
round(prop.table(table(off_u25_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_u25_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_u25_1718$cai_12_new.17, off_u25_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
off_u25_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age_25.19 == 0 ))
round(prop.table(table(off_u25_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_u25_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_u25_1819$cai_12_new.18, off_u25_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 2A: Ages 15-19 -----
# * * * 2014-2015
off_youngest_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age.15 <= 19 ))
round(prop.table(table(off_youngest_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_youngest_1415$cai_12_new.15))*100,1)

# exact2x2((table(off_youngest_1415$cai_12_new.14, off_youngest_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_youngest_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age.16 <= 19))
round(prop.table(table(off_youngest_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_youngest_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_youngest_1516$cai_12_new.15, off_youngest_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_youngest_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age.17 <= 19))
round(prop.table(table(off_youngest_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_youngest_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_youngest_1617$cai_12_new.16, off_youngest_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_youngest_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age.18 <= 19))
round(prop.table(table(off_youngest_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_youngest_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_youngest_1718$cai_12_new.17, off_youngest_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * * 2018-2019
off_youngest_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age.19 <= 19))
round(prop.table(table(off_youngest_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_youngest_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_youngest_1819$cai_12_new.18, off_youngest_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * All years
all_off_youngest <- subset(all_off, age.1 <= 19)
round(prop.table(table(all_off_youngest$cai_12_new.1))*100,1)
round(prop.table(table(all_off_youngest$cai_12_new.2))*100,1)

# exact2x2((table(all_off_youngest$cai_12_new.1, all_off_youngest$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 2B: Ages 20-24 ——----
# * * * 2014-2015
off_20.24_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age.15 >= 20 & age_25.15 == 0 ))
round(prop.table(table(off_20.24_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_20.24_1415$cai_12_new.15))*100,1)

# exact2x2((table(off_20.24_1415$cai_12_new.14, off_20.24_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_20.24_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age.16 >= 20 & age_25.16 == 0))
round(prop.table(table(off_20.24_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_20.24_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_20.24_1516$cai_12_new.15, off_20.24_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_20.24_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age.17 >= 20 & age_25.17 == 0))
round(prop.table(table(off_20.24_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_20.24_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_20.24_1617$cai_12_new.16, off_20.24_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_20.24_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age.18 >= 20 & age_25.18 == 0))
round(prop.table(table(off_20.24_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_20.24_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_20.24_1718$cai_12_new.17, off_20.24_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
off_20.24_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age.19 >= 20 & age_25.19 == 0))
round(prop.table(table(off_20.24_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_20.24_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_20.24_1819$cai_12_new.18, off_20.24_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * All years
all_off_20.24 <- subset(all_off, age.1 >= 20 & age_25.1 == 0)
round(prop.table(table(all_off_20.24$cai_12_new.1))*100,1)
round(prop.table(table(all_off_20.24$cai_12_new.2))*100,1)

# exact2x2((table(all_off_20.24$cai_12_new.1, all_off_20.24$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 3: Ages 25 and older -----
# * * * 2014-2015
off_o25_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age_25.15 == 1 ))
round(prop.table(table(off_o25_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_o25_1415$cai_12_new.15))*100,1)

# exact2x2((table(off_o25_1415$cai_12_new.14, off_o25_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_o25_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age_25.16 == 1 ))
round(prop.table(table(off_o25_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_o25_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_o25_1516$cai_12_new.15, off_o25_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_o25_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age_25.17 == 1 ))
round(prop.table(table(off_o25_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_o25_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_o25_1617$cai_12_new.16, off_o25_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_o25_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age_25.18 == 1 ))
round(prop.table(table(off_o25_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_o25_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_o25_1718$cai_12_new.17, off_o25_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
off_o25_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age_25.19 == 1 ))
round(prop.table(table(off_o25_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_o25_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_o25_1819$cai_12_new.18, off_o25_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 4: non-Hispanic White -----
# * * * 2014-2015
off_nhw_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_White" ))
round(prop.table(table(off_nhw_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_nhw_1415$cai_12_new.15))*100,1)

# exact2x2((table(off_nhw_1415$cai_12_new.14, off_nhw_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_nhw_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_White" ))
round(prop.table(table(off_nhw_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_nhw_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_nhw_1516$cai_12_new.15, off_nhw_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_nhw_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_White" ))
round(prop.table(table(off_nhw_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_nhw_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_nhw_1617$cai_12_new.16, off_nhw_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_nhw_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_White" ))
round(prop.table(table(off_nhw_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_nhw_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_nhw_1718$cai_12_new.17, off_nhw_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
off_nhw_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_White" ))
round(prop.table(table(off_nhw_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_nhw_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_nhw_1819$cai_12_new.18, off_nhw_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 5: non-Hispanic, Black -----
# * * * 2014-2015
off_nhb_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_Black" ))
round(prop.table(table(off_nhb_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_nhb_1415$cai_12_new.15))*100,1)

# # exact2x2((table(off_nhb_1415$cai_12_new.14, off_nhb_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_nhb_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_Black" ))
round(prop.table(table(off_nhb_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_nhb_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_nhb_1516$cai_12_new.15, off_nhb_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_nhb_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_Black" ))
round(prop.table(table(off_nhb_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_nhb_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_nhb_1617$cai_12_new.16, off_nhb_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_nhb_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_Black" ))
round(prop.table(table(off_nhb_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_nhb_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_nhb_1718$cai_12_new.17, off_nhb_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
off_nhb_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_Black" ))
round(prop.table(table(off_nhb_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_nhb_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_nhb_1819$cai_12_new.18, off_nhb_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 6: non-Hispanic, Other -----
# * * * 2014-2015
off_nho_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_Other" ))
round(prop.table(table(off_nho_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_nho_1415$cai_12_new.15))*100,1)

# exact2x2((table(off_nho_1415$cai_12_new.14, off_nho_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_nho_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_Other" ))
round(prop.table(table(off_nho_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_nho_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_nho_1516$cai_12_new.15, off_nho_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_nho_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_Other" ))
round(prop.table(table(off_nho_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_nho_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_nho_1617$cai_12_new.16, off_nho_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_nho_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_Other" ))
round(prop.table(table(off_nho_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_nho_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_nho_1718$cai_12_new.17, off_nho_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
off_nho_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_Other" ))
round(prop.table(table(off_nho_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_nho_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_nho_1819$cai_12_new.18, off_nho_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 7: Hispanic -----
# * * * 2014-2015
off_hisp_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "Hispanic" ))
round(prop.table(table(off_hisp_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_hisp_1415$cai_12_new.15))*100,1)

# exact2x2((table(off_hisp_1415$cai_12_new.14, off_hisp_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_hisp_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "Hispanic" ))
round(prop.table(table(off_hisp_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_hisp_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_hisp_1516$cai_12_new.15, off_hisp_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_hisp_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "Hispanic" ))
round(prop.table(table(off_hisp_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_hisp_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_hisp_1617$cai_12_new.16, off_hisp_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_hisp_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "Hispanic" ))
round(prop.table(table(off_hisp_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_hisp_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_hisp_1718$cai_12_new.17, off_hisp_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
off_hisp_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "Hispanic" ))
round(prop.table(table(off_hisp_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_hisp_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_hisp_1819$cai_12_new.18, off_hisp_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 7A: Hisp, Ages 15-19 -----
# * * * 2014-2015
off_hisp_yngst_1415 <- subset(off_hisp_1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age.15 <= 19 ))
round(prop.table(table(off_hisp_yngst_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_hisp_yngst_1415$cai_12_new.15))*100,1)

# exact2x2((table(off_hisp_yngst_1415$cai_12_new.14, off_hisp_yngst_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_hisp_yngst_1516 <- subset(off_hisp_1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age.16 <= 19))
round(prop.table(table(off_hisp_yngst_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_hisp_yngst_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_hisp_yngst_1516$cai_12_new.15, off_hisp_yngst_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_hisp_yngst_1617 <- subset(off_hisp_1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age.17 <= 19))
round(prop.table(table(off_hisp_yngst_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_hisp_yngst_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_hisp_yngst_1617$cai_12_new.16, off_hisp_yngst_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_hisp_yngst_1718 <- subset(off_hisp_1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age.18 <= 19))
round(prop.table(table(off_hisp_yngst_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_hisp_yngst_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_hisp_yngst_1718$cai_12_new.17, off_hisp_yngst_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
off_hisp_yngst_1819 <- subset(off_hisp_1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age.19 <= 19))
round(prop.table(table(off_hisp_yngst_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_hisp_yngst_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_hisp_yngst_1819$cai_12_new.18, off_hisp_yngst_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * All years
all_off_hisp_youngest <- subset(all_off, (age.1 <= 19 & race_cat.1 == "Hispanic"))
round(prop.table(table(all_off_hisp_youngest$cai_12_new.1))*100,1)
round(prop.table(table(all_off_hisp_youngest$cai_12_new.2))*100,1)

# exact2x2((table(all_off_hisp_youngest$cai_12_new.1, all_off_hisp_youngest$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 7B: Hisp, Ages 20-24 ——--------
# * * * 2014-2015
off_hisp_20.24_1415 <- subset(off_hisp_1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age.15 >= 20 & age_25.15 == 0 ))
round(prop.table(table(off_hisp_20.24_1415$cai_12_new.14))*100,1)
round(prop.table(table(off_hisp_20.24_1415$cai_12_new.15))*100,1)

# exact2x2((table(off_hisp_20.24_1415$cai_12_new.14, off_hisp_20.24_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
off_hisp_20.24_1516 <- subset(off_hisp_1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age.16 >= 20 & age_25.16 == 0))
round(prop.table(table(off_hisp_20.24_1516$cai_12_new.15))*100,1)
round(prop.table(table(off_hisp_20.24_1516$cai_12_new.16))*100,1)

# exact2x2((table(off_hisp_20.24_1516$cai_12_new.15, off_hisp_20.24_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
off_hisp_20.24_1617 <- subset(off_hisp_1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age.17 >= 20 & age_25.17 == 0))
round(prop.table(table(off_hisp_20.24_1617$cai_12_new.16))*100,1)
round(prop.table(table(off_hisp_20.24_1617$cai_12_new.17))*100,1)

# exact2x2((table(off_hisp_20.24_1617$cai_12_new.16, off_hisp_20.24_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
off_hisp_20.24_1718 <- subset(off_hisp_1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age.18 >= 20 & age_25.18 == 0))
round(prop.table(table(off_hisp_20.24_1718$cai_12_new.17))*100,1)
round(prop.table(table(off_hisp_20.24_1718$cai_12_new.18))*100,1)

# exact2x2((table(off_hisp_20.24_1718$cai_12_new.17, off_hisp_20.24_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
off_hisp_20.24_1819 <- subset(off_hisp_1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age.19 >= 20 & age_25.19 == 0))
round(prop.table(table(off_hisp_20.24_1819$cai_12_new.18))*100,1)
round(prop.table(table(off_hisp_20.24_1819$cai_12_new.19))*100,1)

# exact2x2((table(off_hisp_20.24_1819$cai_12_new.18, off_hisp_20.24_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * All years
all_off_hisp_20.24 <- subset(all_off, (race_cat.1 == "Hispanic" & age.1 >= 20 & age_25.1 == 0))
round(prop.table(table(all_off_hisp_20.24$cai_12_new.1))*100,1)
round(prop.table(table(all_off_hisp_20.24$cai_12_new.2))*100,1)

# exact2x2((table(all_off_hisp_20.24$cai_12_new.1, all_off_hisp_20.24$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * Hisp, 15-24 -----
all_off_hisp_15.24 <- rbind(all_off_hisp_youngest, all_off_hisp_20.24)

round(prop.table(table(all_off_hisp_15.24$cai_12_new.1))*100,1)
round(prop.table(table(all_off_hisp_15.24$cai_12_new.2))*100,1)

# Income groups ------
all_off_LT20 <- subset(all_off, income_cat.1 == "1_<20k")
round(prop.table(table(all_off_LT20$cai_12_new.1))*100,1)
round(prop.table(table(all_off_LT20$cai_12_new.2))*100,1)

exact2x2((table(all_off_LT20$cai_12_new.1, all_off_LT20$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

all_off_2039 <- subset(all_off, income_cat.1 == "2_<40k")
round(prop.table(table(all_off_2039$cai_12_new.1))*100,1)
round(prop.table(table(all_off_2039$cai_12_new.2))*100,1)

exact2x2((table(all_off_2039$cai_12_new.1, all_off_2039$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

all_off_4074 <- subset(all_off, income_cat.1 == "3_<75k")
round(prop.table(table(all_off_4074$cai_12_new.1))*100,1)
round(prop.table(table(all_off_4074$cai_12_new.2))*100,1)

exact2x2((table(all_off_4074$cai_12_new.1, all_off_4074$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

all_off_AL75 <- subset(all_off, income_cat.1 == "4_75k+")
round(prop.table(table(all_off_AL75$cai_12_new.1))*100,1)
round(prop.table(table(all_off_AL75$cai_12_new.2))*100,1)

exact2x2((table(all_off_AL75$cai_12_new.1, all_off_AL75$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


### EDUCATION GROUPS ------
all_off_LTHS <- subset(all_off, educ3.1 == "1_HS-or-less")
round(prop.table(table(all_off_LTHS$cai_12_new.1))*100,0)
round(prop.table(table(all_off_LTHS$cai_12_new.2))*100,0)
exact2x2((table(all_off_LTHS$cai_12_new.1, all_off_LTHS$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_SC <- subset(all_off, educ3.1 == "2_ltCol")
round(prop.table(table(all_off_SC$cai_12_new.1))*100,0)
round(prop.table(table(all_off_SC$cai_12_new.2))*100,0)
exact2x2((table(all_off_SC$cai_12_new.1, all_off_SC$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_Col <- subset(all_off, educ3.1 == "3_Col+")
round(prop.table(table(all_off_Col$cai_12_new.1))*100,0)
round(prop.table(table(all_off_Col$cai_12_new.2))*100,0)
exact2x2((table(all_off_Col$cai_12_new.1, all_off_Col$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# exact2x2((table(all_off_hisp_15.24$cai_12_new.1, all_off_hisp_15.24$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * "Started PrEP" group -----
# * * Section 1: All Participants -----
# * * * 2014-2015
start_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "02_PrEPcur"))
round(prop.table(table(start_1415$cai_12_new.14))*100,1)
round(prop.table(table(start_1415$cai_12_new.15))*100,1)

# exact2x2((table(start_1415$cai_12_new.14, start_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
start_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "02_PrEPcur"))
round(prop.table(table(start_1516$cai_12_new.15))*100,1)
round(prop.table(table(start_1516$cai_12_new.16))*100,1)

# exact2x2((table(start_1516$cai_12_new.15, start_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
start_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "02_PrEPcur"))
round(prop.table(table(start_1617$cai_12_new.16))*100,1)
round(prop.table(table(start_1617$cai_12_new.17))*100,1)

# exact2x2((table(start_1617$cai_12_new.16, start_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
start_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "02_PrEPcur"))
round(prop.table(table(start_1718$cai_12_new.17))*100,1)
round(prop.table(table(start_1718$cai_12_new.18))*100,1)

# exact2x2((table(start_1718$cai_12_new.17, start_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
start_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "02_PrEPcur"))
round(prop.table(table(start_1819$cai_12_new.18))*100,1)
round(prop.table(table(start_1819$cai_12_new.19))*100,1)

# exact2x2((table(start_1819$cai_12_new.18, start_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 2: Ages 15-24 -----
# * * * 2014-2015
start_u25_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "02_PrEPcur" & age_25.15 == 0 ))
round(prop.table(table(start_u25_1415$cai_12_new.14))*100,1)
round(prop.table(table(start_u25_1415$cai_12_new.15))*100,1)

# exact2x2((table(start_u25_1415$cai_12_new.14, start_u25_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
start_u25_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "02_PrEPcur" & age_25.16 == 0 ))
round(prop.table(table(start_u25_1516$cai_12_new.15))*100,1)
round(prop.table(table(start_u25_1516$cai_12_new.16))*100,1)

# exact2x2((table(start_u25_1516$cai_12_new.15, start_u25_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
start_u25_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "02_PrEPcur" & age_25.17 == 0 ))
round(prop.table(table(start_u25_1617$cai_12_new.16))*100,1)
round(prop.table(table(start_u25_1617$cai_12_new.17))*100,1)

# exact2x2((table(start_u25_1617$cai_12_new.16, start_u25_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
start_u25_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "02_PrEPcur" & age_25.18 == 0 ))
round(prop.table(table(start_u25_1718$cai_12_new.17))*100,1)
round(prop.table(table(start_u25_1718$cai_12_new.18))*100,1)

# exact2x2((table(start_u25_1718$cai_12_new.17, start_u25_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
start_u25_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "02_PrEPcur" & age_25.19 == 0 ))
round(prop.table(table(start_u25_1819$cai_12_new.18))*100,1)
round(prop.table(table(start_u25_1819$cai_12_new.19))*100,1)

# exact2x2((table(start_u25_1819$cai_12_new.18, start_u25_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 3: Ages 25 and older -----
# * * * 2014-2015
start_o25_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "02_PrEPcur" & age_25.15 == 1 ))
round(prop.table(table(start_o25_1415$cai_12_new.14))*100,1)
round(prop.table(table(start_o25_1415$cai_12_new.15))*100,1)

# exact2x2((table(start_o25_1415$cai_12_new.14, start_o25_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
start_o25_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "02_PrEPcur" & age_25.16 == 1 ))
round(prop.table(table(start_o25_1516$cai_12_new.15))*100,1)
round(prop.table(table(start_o25_1516$cai_12_new.16))*100,1)

# exact2x2((table(start_o25_1516$cai_12_new.15, start_o25_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
start_o25_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "02_PrEPcur" & age_25.17 == 1 ))
round(prop.table(table(start_o25_1617$cai_12_new.16))*100,1)
round(prop.table(table(start_o25_1617$cai_12_new.17))*100,1)

# exact2x2((table(start_o25_1617$cai_12_new.16, start_o25_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
start_o25_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "02_PrEPcur" & age_25.18 == 1 ))
round(prop.table(table(start_o25_1718$cai_12_new.17))*100,1)
round(prop.table(table(start_o25_1718$cai_12_new.18))*100,1)

# exact2x2((table(start_o25_1718$cai_12_new.17, start_o25_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
start_o25_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "02_PrEPcur" & age_25.19 == 1 ))
round(prop.table(table(start_o25_1819$cai_12_new.18))*100,1)
round(prop.table(table(start_o25_1819$cai_12_new.19))*100,1)

# exact2x2((table(start_o25_1819$cai_12_new.18, start_o25_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 4: non-Hispanic White -----
# * * * 2014-2015
start_nhw_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "02_PrEPcur" & race_cat.15 == "NH_White" ))
round(prop.table(table(start_nhw_1415$cai_12_new.14))*100,1)
round(prop.table(table(start_nhw_1415$cai_12_new.15))*100,1)

# exact2x2((table(start_nhw_1415$cai_12_new.14, start_nhw_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
start_nhw_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "02_PrEPcur" & race_cat.16 == "NH_White" ))
round(prop.table(table(start_nhw_1516$cai_12_new.15))*100,1)
round(prop.table(table(start_nhw_1516$cai_12_new.16))*100,1)

# exact2x2((table(start_nhw_1516$cai_12_new.15, start_nhw_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
start_nhw_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "02_PrEPcur" & race_cat.17 == "NH_White" ))
round(prop.table(table(start_nhw_1617$cai_12_new.16))*100,1)
round(prop.table(table(start_nhw_1617$cai_12_new.17))*100,1)

# exact2x2((table(start_nhw_1617$cai_12_new.16, start_nhw_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
start_nhw_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "02_PrEPcur" & race_cat.18 == "NH_White" ))
round(prop.table(table(start_nhw_1718$cai_12_new.17))*100,1)
round(prop.table(table(start_nhw_1718$cai_12_new.18))*100,1)

# exact2x2((table(start_nhw_1718$cai_12_new.17, start_nhw_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
start_nhw_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "02_PrEPcur" & race_cat.19 == "NH_White" ))
round(prop.table(table(start_nhw_1819$cai_12_new.18))*100,1)
round(prop.table(table(start_nhw_1819$cai_12_new.19))*100,1)

# exact2x2((table(start_nhw_1819$cai_12_new.18, start_nhw_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 5: non-Hispanic, Black -----
# * * * 2014-2015
start_nhb_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "02_PrEPcur" & race_cat.15 == "NH_Black" ))
round(prop.table(table(start_nhb_1415$cai_12_new.14))*100,1)
round(prop.table(table(start_nhb_1415$cai_12_new.15))*100,1)

# exact2x2((table(start_nhb_1415$cai_12_new.14, start_nhb_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
start_nhb_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "02_PrEPcur" & race_cat.16 == "NH_Black" ))
round(prop.table(table(start_nhb_1516$cai_12_new.15))*100,1)
round(prop.table(table(start_nhb_1516$cai_12_new.16))*100,1)

# exact2x2((table(start_nhb_1516$cai_12_new.15, start_nhb_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
start_nhb_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "02_PrEPcur" & race_cat.17 == "NH_Black" ))
round(prop.table(table(start_nhb_1617$cai_12_new.16))*100,1)
round(prop.table(table(start_nhb_1617$cai_12_new.17))*100,1)

# exact2x2((table(start_nhb_1617$cai_12_new.16, start_nhb_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
start_nhb_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "02_PrEPcur" & race_cat.18 == "NH_Black" ))
round(prop.table(table(start_nhb_1718$cai_12_new.17))*100,1)
round(prop.table(table(start_nhb_1718$cai_12_new.18))*100,1)

# exact2x2((table(start_nhb_1718$cai_12_new.17, start_nhb_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
start_nhb_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "02_PrEPcur" & race_cat.19 == "NH_Black" ))
round(prop.table(table(start_nhb_1819$cai_12_new.18))*100,1)
round(prop.table(table(start_nhb_1819$cai_12_new.19))*100,1)

# exact2x2((table(start_nhb_1819$cai_12_new.18, start_nhb_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 6: non-Hispanic, Other -----
# * * * 2014-2015
start_nho_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "02_PrEPcur" & race_cat.15 == "NH_Other" ))
round(prop.table(table(start_nho_1415$cai_12_new.14))*100,1)
round(prop.table(table(start_nho_1415$cai_12_new.15))*100,1)

# exact2x2((table(start_nho_1415$cai_12_new.14, start_nho_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
start_nho_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "02_PrEPcur" & race_cat.16 == "NH_Other" ))
round(prop.table(table(start_nho_1516$cai_12_new.15))*100,1)
round(prop.table(table(start_nho_1516$cai_12_new.16))*100,1)

# exact2x2((table(start_nho_1516$cai_12_new.15, start_nho_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
start_nho_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "02_PrEPcur" & race_cat.17 == "NH_Other" ))
round(prop.table(table(start_nho_1617$cai_12_new.16))*100,1)
round(prop.table(table(start_nho_1617$cai_12_new.17))*100,1)

# exact2x2((table(start_nho_1617$cai_12_new.16, start_nho_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
start_nho_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "02_PrEPcur" & race_cat.18 == "NH_Other" ))
round(prop.table(table(start_nho_1718$cai_12_new.17))*100,1)
round(prop.table(table(start_nho_1718$cai_12_new.18))*100,1)

# exact2x2((table(start_nho_1718$cai_12_new.17, start_nho_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
start_nho_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "02_PrEPcur" & race_cat.19 == "NH_Other" ))
round(prop.table(table(start_nho_1819$cai_12_new.18))*100,1)
round(prop.table(table(start_nho_1819$cai_12_new.19))*100,1)

# exact2x2((table(start_nho_1819$cai_12_new.18, start_nho_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 7: Hispanic -----
# * * * 2014-2015
start_hisp_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "02_PrEPcur" & race_cat.15 == "Hispanic" ))
round(prop.table(table(start_hisp_1415$cai_12_new.14))*100,1)
round(prop.table(table(start_hisp_1415$cai_12_new.15))*100,1)

# exact2x2((table(start_hisp_1415$cai_12_new.14, start_hisp_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
start_hisp_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "02_PrEPcur" & race_cat.16 == "Hispanic" ))
round(prop.table(table(start_hisp_1516$cai_12_new.15))*100,1)
round(prop.table(table(start_hisp_1516$cai_12_new.16))*100,1)

# exact2x2((table(start_hisp_1516$cai_12_new.15, start_hisp_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
start_hisp_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "02_PrEPcur" & race_cat.17 == "Hispanic" ))
round(prop.table(table(start_hisp_1617$cai_12_new.16))*100,1)
round(prop.table(table(start_hisp_1617$cai_12_new.17))*100,1)

# exact2x2((table(start_hisp_1617$cai_12_new.16, start_hisp_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
start_hisp_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "02_PrEPcur" & race_cat.18 == "Hispanic" ))
round(prop.table(table(start_hisp_1718$cai_12_new.17))*100,1)
round(prop.table(table(start_hisp_1718$cai_12_new.18))*100,1)

# exact2x2((table(start_hisp_1718$cai_12_new.17, start_hisp_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
start_hisp_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "02_PrEPcur" & race_cat.19 == "Hispanic" ))
round(prop.table(table(start_hisp_1819$cai_12_new.18))*100,1)
round(prop.table(table(start_hisp_1819$cai_12_new.19))*100,1)

# exact2x2((table(start_hisp_1819$cai_12_new.18, start_hisp_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * "Stayed on PrEP" group -----
# * * Section 1: All Participants -----
# * * * 2014-2015
stayed_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "02_PrEPcur" & neg_prep12_use_groups.15 == "02_PrEPcur"))
round(prop.table(table(stayed_1415$cai_12_new.14))*100,1)
round(prop.table(table(stayed_1415$cai_12_new.15))*100,1)

table(stayed_1415$cai_12_new.14)
table(stayed_1415$cai_12_new.15)

# exact2x2((table(stayed_1415$cai_12_new.14, stayed_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
stayed_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "02_PrEPcur" & neg_prep12_use_groups.16 == "02_PrEPcur"))
round(prop.table(table(stayed_1516$cai_12_new.15))*100,1)
round(prop.table(table(stayed_1516$cai_12_new.16))*100,1)

# exact2x2((table(stayed_1516$cai_12_new.15, stayed_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
stayed_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "02_PrEPcur" & neg_prep12_use_groups.17 == "02_PrEPcur"))
round(prop.table(table(stayed_1617$cai_12_new.16))*100,1)
round(prop.table(table(stayed_1617$cai_12_new.17))*100,1)

# exact2x2((table(stayed_1617$cai_12_new.16, stayed_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
stayed_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "02_PrEPcur" & neg_prep12_use_groups.18 == "02_PrEPcur"))
round(prop.table(table(stayed_1718$cai_12_new.17))*100,1)
round(prop.table(table(stayed_1718$cai_12_new.18))*100,1)

# exact2x2((table(stayed_1718$cai_12_new.17, stayed_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
stayed_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "02_PrEPcur" & neg_prep12_use_groups.19 == "02_PrEPcur"))
round(prop.table(table(stayed_1819$cai_12_new.18))*100,1)
round(prop.table(table(stayed_1819$cai_12_new.19))*100,1)

# exact2x2((table(stayed_1819$cai_12_new.18, stayed_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 2: Ages 15-24 -----
# * * * 2014-2015
stayed_u25_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "02_PrEPcur" & neg_prep12_use_groups.15 == "02_PrEPcur" & age_25.15 == 0 ))
round(prop.table(table(stayed_u25_1415$cai_12_new.14))*100,1)
round(prop.table(table(stayed_u25_1415$cai_12_new.15))*100,1)

# exact2x2((table(stayed_u25_1415$cai_12_new.14, stayed_u25_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
stayed_u25_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "02_PrEPcur" & neg_prep12_use_groups.16 == "02_PrEPcur" & age_25.16 == 0 ))
round(prop.table(table(stayed_u25_1516$cai_12_new.15))*100,1)
round(prop.table(table(stayed_u25_1516$cai_12_new.16))*100,1)

# exact2x2((table(stayed_u25_1516$cai_12_new.15, stayed_u25_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
stayed_u25_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "02_PrEPcur" & neg_prep12_use_groups.17 == "02_PrEPcur" & age_25.17 == 0 ))
round(prop.table(table(stayed_u25_1617$cai_12_new.16))*100,1)
round(prop.table(table(stayed_u25_1617$cai_12_new.17))*100,1)

# exact2x2((table(stayed_u25_1617$cai_12_new.16, stayed_u25_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
stayed_u25_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "02_PrEPcur" & neg_prep12_use_groups.18 == "02_PrEPcur" & age_25.18 == 0 ))
round(prop.table(table(stayed_u25_1718$cai_12_new.17))*100,1)
round(prop.table(table(stayed_u25_1718$cai_12_new.18))*100,1)

# exact2x2((table(stayed_u25_1718$cai_12_new.17, stayed_u25_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
stayed_u25_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "02_PrEPcur" & neg_prep12_use_groups.19 == "02_PrEPcur" & age_25.19 == 0 ))
round(prop.table(table(stayed_u25_1819$cai_12_new.18))*100,1)
round(prop.table(table(stayed_u25_1819$cai_12_new.19))*100,1)

# exact2x2((table(stayed_u25_1819$cai_12_new.18, stayed_u25_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 3: Ages 25 and older -----
# * * * 2014-2015
stayed_o25_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "02_PrEPcur" & neg_prep12_use_groups.15 == "02_PrEPcur" & age_25.15 == 1 ))
round(prop.table(table(stayed_o25_1415$cai_12_new.14))*100,1)
round(prop.table(table(stayed_o25_1415$cai_12_new.15))*100,1)

# exact2x2((table(stayed_o25_1415$cai_12_new.14, stayed_o25_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
stayed_o25_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "02_PrEPcur" & neg_prep12_use_groups.16 == "02_PrEPcur" & age_25.16 == 1 ))
round(prop.table(table(stayed_o25_1516$cai_12_new.15))*100,1)
round(prop.table(table(stayed_o25_1516$cai_12_new.16))*100,1)

# exact2x2((table(stayed_o25_1516$cai_12_new.15, stayed_o25_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
stayed_o25_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "02_PrEPcur" & neg_prep12_use_groups.17 == "02_PrEPcur" & age_25.17 == 1 ))
round(prop.table(table(stayed_o25_1617$cai_12_new.16))*100,1)
round(prop.table(table(stayed_o25_1617$cai_12_new.17))*100,1)

# exact2x2((table(stayed_o25_1617$cai_12_new.16, stayed_o25_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
stayed_o25_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "02_PrEPcur" & neg_prep12_use_groups.18 == "02_PrEPcur" & age_25.18 == 1 ))
round(prop.table(table(stayed_o25_1718$cai_12_new.17))*100,1)
round(prop.table(table(stayed_o25_1718$cai_12_new.18))*100,1)

# exact2x2((table(stayed_o25_1718$cai_12_new.17, stayed_o25_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
stayed_o25_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "02_PrEPcur" & neg_prep12_use_groups.19 == "02_PrEPcur" & age_25.19 == 1 ))
round(prop.table(table(stayed_o25_1819$cai_12_new.18))*100,1)
round(prop.table(table(stayed_o25_1819$cai_12_new.19))*100,1)

# exact2x2((table(stayed_o25_1819$cai_12_new.18, stayed_o25_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 4: non-Hispanic White -----
# * * * 2014-2015
stayed_nhw_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "02_PrEPcur" & neg_prep12_use_groups.15 == "02_PrEPcur" & race_cat.15 == "NH_White" ))
round(prop.table(table(stayed_nhw_1415$cai_12_new.14))*100,1)
round(prop.table(table(stayed_nhw_1415$cai_12_new.15))*100,1)

# exact2x2((table(stayed_nhw_1415$cai_12_new.14, stayed_nhw_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
stayed_nhw_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "02_PrEPcur" & neg_prep12_use_groups.16 == "02_PrEPcur" & race_cat.16 == "NH_White" ))
round(prop.table(table(stayed_nhw_1516$cai_12_new.15))*100,1)
round(prop.table(table(stayed_nhw_1516$cai_12_new.16))*100,1)

# exact2x2((table(stayed_nhw_1516$cai_12_new.15, stayed_nhw_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
stayed_nhw_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "02_PrEPcur" & neg_prep12_use_groups.17 == "02_PrEPcur" & race_cat.17 == "NH_White" ))
round(prop.table(table(stayed_nhw_1617$cai_12_new.16))*100,1)
round(prop.table(table(stayed_nhw_1617$cai_12_new.17))*100,1)

# exact2x2((table(stayed_nhw_1617$cai_12_new.16, stayed_nhw_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
stayed_nhw_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "02_PrEPcur" & neg_prep12_use_groups.18 == "02_PrEPcur" & race_cat.18 == "NH_White" ))
round(prop.table(table(stayed_nhw_1718$cai_12_new.17))*100,1)
round(prop.table(table(stayed_nhw_1718$cai_12_new.18))*100,1)

# exact2x2((table(stayed_nhw_1718$cai_12_new.17, stayed_nhw_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
stayed_nhw_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "02_PrEPcur" & neg_prep12_use_groups.19 == "02_PrEPcur" & race_cat.19 == "NH_White" ))
round(prop.table(table(stayed_nhw_1819$cai_12_new.18))*100,1)
round(prop.table(table(stayed_nhw_1819$cai_12_new.19))*100,1)

# exact2x2((table(stayed_nhw_1819$cai_12_new.18, stayed_nhw_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Section 5: non-Hispanic, Black -----
# * * * 2014-2015
stayed_nhb_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "02_PrEPcur" & neg_prep12_use_groups.15 == "02_PrEPcur" & race_cat.15 == "NH_Black" ))
round(prop.table(table(stayed_nhb_1415$cai_12_new.14))*100,1)
round(prop.table(table(stayed_nhb_1415$cai_12_new.15))*100,1)

# exact2x2((table(stayed_nhb_1415$cai_12_new.14, stayed_nhb_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
stayed_nhb_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "02_PrEPcur" & neg_prep12_use_groups.16 == "02_PrEPcur" & race_cat.16 == "NH_Black" ))
round(prop.table(table(stayed_nhb_1516$cai_12_new.15))*100,1)
round(prop.table(table(stayed_nhb_1516$cai_12_new.16))*100,1)

# exact2x2((table(stayed_nhb_1516$cai_12_new.15, stayed_nhb_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
stayed_nhb_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "02_PrEPcur" & neg_prep12_use_groups.17 == "02_PrEPcur" & race_cat.17 == "NH_Black" ))
round(prop.table(table(stayed_nhb_1617$cai_12_new.16))*100,1)
round(prop.table(table(stayed_nhb_1617$cai_12_new.17))*100,1)

# exact2x2((table(stayed_nhb_1617$cai_12_new.16, stayed_nhb_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
stayed_nhb_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "02_PrEPcur" & neg_prep12_use_groups.18 == "02_PrEPcur" & race_cat.18 == "NH_Black" ))
round(prop.table(table(stayed_nhb_1718$cai_12_new.17))*100,1)
round(prop.table(table(stayed_nhb_1718$cai_12_new.18))*100,1)

# exact2x2((table(stayed_nhb_1718$cai_12_new.17, stayed_nhb_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
stayed_nhb_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "02_PrEPcur" & neg_prep12_use_groups.19 == "02_PrEPcur" & race_cat.19 == "NH_Black" ))
round(prop.table(table(stayed_nhb_1819$cai_12_new.18))*100,1)
round(prop.table(table(stayed_nhb_1819$cai_12_new.19))*100,1)

# exact2x2((table(stayed_nhb_1819$cai_12_new.18, stayed_nhb_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 6: non-Hispanic, Other -----
# * * * 2014-2015
stayed_nho_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "02_PrEPcur" & neg_prep12_use_groups.15 == "02_PrEPcur" & race_cat.15 == "NH_Other" ))
round(prop.table(table(stayed_nho_1415$cai_12_new.14))*100,1)
round(prop.table(table(stayed_nho_1415$cai_12_new.15))*100,1)

# exact2x2((table(stayed_nho_1415$cai_12_new.14, stayed_nho_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
stayed_nho_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "02_PrEPcur" & neg_prep12_use_groups.16 == "02_PrEPcur" & race_cat.16 == "NH_Other" ))
round(prop.table(table(stayed_nho_1516$cai_12_new.15))*100,1)
round(prop.table(table(stayed_nho_1516$cai_12_new.16))*100,1)

# exact2x2((table(stayed_nho_1516$cai_12_new.15, stayed_nho_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
stayed_nho_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "02_PrEPcur" & neg_prep12_use_groups.17 == "02_PrEPcur" & race_cat.17 == "NH_Other" ))
round(prop.table(table(stayed_nho_1617$cai_12_new.16))*100,1)
round(prop.table(table(stayed_nho_1617$cai_12_new.17))*100,1)

# exact2x2((table(stayed_nho_1617$cai_12_new.16, stayed_nho_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
stayed_nho_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "02_PrEPcur" & neg_prep12_use_groups.18 == "02_PrEPcur" & race_cat.18 == "NH_Other" ))
round(prop.table(table(stayed_nho_1718$cai_12_new.17))*100,1)
round(prop.table(table(stayed_nho_1718$cai_12_new.18))*100,1)

# exact2x2((table(stayed_nho_1718$cai_12_new.17, stayed_nho_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
stayed_nho_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "02_PrEPcur" & neg_prep12_use_groups.19 == "02_PrEPcur" & race_cat.19 == "NH_Other" ))
round(prop.table(table(stayed_nho_1819$cai_12_new.18))*100,1)
round(prop.table(table(stayed_nho_1819$cai_12_new.19))*100,1)

# exact2x2((table(stayed_nho_1819$cai_12_new.18, stayed_nho_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Section 7: Hispanic -----
# * * * 2014-2015
stayed_hisp_1415 <- subset(amis1415, (neg_prep12_use_groups.14 == "02_PrEPcur" & neg_prep12_use_groups.15 == "02_PrEPcur" & race_cat.15 == "Hispanic" ))
round(prop.table(table(stayed_hisp_1415$cai_12_new.14))*100,1)
round(prop.table(table(stayed_hisp_1415$cai_12_new.15))*100,1)

# exact2x2((table(stayed_hisp_1415$cai_12_new.14, stayed_hisp_1415$cai_12_new.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2015-2016
stayed_hisp_1516 <- subset(amis1516, (neg_prep12_use_groups.15 == "02_PrEPcur" & neg_prep12_use_groups.16 == "02_PrEPcur" & race_cat.16 == "Hispanic" ))
round(prop.table(table(stayed_hisp_1516$cai_12_new.15))*100,1)
round(prop.table(table(stayed_hisp_1516$cai_12_new.16))*100,1)

# exact2x2((table(stayed_hisp_1516$cai_12_new.15, stayed_hisp_1516$cai_12_new.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2016-2017
stayed_hisp_1617 <- subset(amis1617, (neg_prep12_use_groups.16 == "02_PrEPcur" & neg_prep12_use_groups.17 == "02_PrEPcur" & race_cat.17 == "Hispanic" ))
round(prop.table(table(stayed_hisp_1617$cai_12_new.16))*100,1)
round(prop.table(table(stayed_hisp_1617$cai_12_new.17))*100,1)

# exact2x2((table(stayed_hisp_1617$cai_12_new.16, stayed_hisp_1617$cai_12_new.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2017-2018
stayed_hisp_1718 <- subset(amis1718, (neg_prep12_use_groups.17 == "02_PrEPcur" & neg_prep12_use_groups.18 == "02_PrEPcur" & race_cat.18 == "Hispanic" ))
round(prop.table(table(stayed_hisp_1718$cai_12_new.17))*100,1)
round(prop.table(table(stayed_hisp_1718$cai_12_new.18))*100,1)

# exact2x2((table(stayed_hisp_1718$cai_12_new.17, stayed_hisp_1718$cai_12_new.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * * 2018-2019
stayed_hisp_1819 <- subset(amis1819, (neg_prep12_use_groups.18 == "02_PrEPcur" & neg_prep12_use_groups.19 == "02_PrEPcur" & race_cat.19 == "Hispanic" ))
round(prop.table(table(stayed_hisp_1819$cai_12_new.18))*100,1)
round(prop.table(table(stayed_hisp_1819$cai_12_new.19))*100,1)

# exact2x2((table(stayed_hisp_1819$cai_12_new.18, stayed_hisp_1819$cai_12_new.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


#### Time Series for CAI with UNK among "stayed off PrEP" group -----

# * All participants, by year -----
# * 2014-15 ----
round(prop.table(table(off_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_1415$cai_unk_12.14, off_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_1516$cai_unk_12.15, off_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_1617$cai_unk_12.16, off_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_1718$cai_unk_12.17, off_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_1819$cai_unk_12.18, off_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off$cai_unk_12.1))*100,1)
round(prop.table(table(all_off$cai_unk_12.2))*100,1)

# exact2x2((table(all_off$cai_unk_12.1, all_off$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * <25, by year -----
# * 2014-15 ----
round(prop.table(table(off_u25_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_u25_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_u25_1415$cai_unk_12.14, off_u25_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_u25_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_u25_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_u25_1516$cai_unk_12.15, off_u25_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_u25_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_u25_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_u25_1617$cai_unk_12.16, off_u25_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_u25_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_u25_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_u25_1718$cai_unk_12.17, off_u25_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_u25_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_u25_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_u25_1819$cai_unk_12.18, off_u25_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_u25$cai_unk_12.1))*100,1)
round(prop.table(table(all_off_u25 $cai_unk_12.2))*100,1)

# exact2x2((table(all_off_u25 $cai_unk_12.1, all_off_u25 $cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * >=25, by year -----
# * 2014-15 ----
round(prop.table(table(off_o25_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_o25_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_o25_1415$cai_unk_12.14, off_o25_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_o25_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_o25_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_o25_1516$cai_unk_12.15, off_o25_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_o25_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_o25_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_o25_1617$cai_unk_12.16, off_o25_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_o25_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_o25_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_o25_1718$cai_unk_12.17, off_o25_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_o25_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_o25_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_o25_1819$cai_unk_12.18, off_o25_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_o25$cai_unk_12.1))*100,1)
round(prop.table(table(all_off_o25 $cai_unk_12.2))*100,1)

# exact2x2((table(all_off_o25 $cai_unk_12.1, all_off_o25 $cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * NH White -----
# * 2014-15 ----
round(prop.table(table(off_nhw_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_nhw_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_nhw_1415$cai_unk_12.14, off_nhw_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_nhw_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_nhw_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_nhw_1516$cai_unk_12.15, off_nhw_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_nhw_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_nhw_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_nhw_1617$cai_unk_12.16, off_nhw_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_nhw_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_nhw_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_nhw_1718$cai_unk_12.17, off_nhw_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_nhw_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_nhw_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_nhw_1819$cai_unk_12.18, off_nhw_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_nhw$cai_unk_12.1))*100,1)
round(prop.table(table(all_off_nhw$cai_unk_12.2))*100,1)

# exact2x2((table(all_off_nhw $cai_unk_12.1, all_off_nhw $cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * NH Black -----
# * 2014-15 ----
round(prop.table(table(off_nhb_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_nhb_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_nhb_1415$cai_unk_12.14, off_nhb_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_nhb_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_nhb_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_nhb_1516$cai_unk_12.15, off_nhb_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_nhb_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_nhb_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_nhb_1617$cai_unk_12.16, off_nhb_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_nhb_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_nhb_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_nhb_1718$cai_unk_12.17, off_nhb_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_nhb_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_nhb_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_nhb_1819$cai_unk_12.18, off_nhb_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_nhb$cai_unk_12.1))*100,1)
round(prop.table(table(all_off_nhb $cai_unk_12.2))*100,1)

# exact2x2((table(all_off_nhb $cai_unk_12.1, all_off_nhb $cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * NH Other -----
# * 2014-15 ----
round(prop.table(table(off_nho_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_nho_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_nho_1415$cai_unk_12.14, off_nho_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_nho_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_nho_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_nho_1516$cai_unk_12.15, off_nho_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_nho_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_nho_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_nho_1617$cai_unk_12.16, off_nho_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_nho_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_nho_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_nho_1718$cai_unk_12.17, off_nho_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_nho_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_nho_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_nho_1819$cai_unk_12.18, off_nho_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_nho$cai_unk_12.1))*100,1)
round(prop.table(table(all_off_nho $cai_unk_12.2))*100,1)

# exact2x2((table(all_off_nho $cai_unk_12.1, all_off_nho $cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * Hispanic -----
# * 2014-15 ----
round(prop.table(table(off_hisp_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_hisp_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_hisp_1415$cai_unk_12.14, off_hisp_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_hisp_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_hisp_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_hisp_1516$cai_unk_12.15, off_hisp_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_hisp_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_hisp_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_hisp_1617$cai_unk_12.16, off_hisp_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_hisp_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_hisp_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_hisp_1718$cai_unk_12.17, off_hisp_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_hisp_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_hisp_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_hisp_1819$cai_unk_12.18, off_hisp_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_hisp$cai_unk_12.1))*100,1)
round(prop.table(table(all_off_hisp$cai_unk_12.2))*100,1)

# exact2x2((table(all_off_hisp $cai_unk_12.1, all_off_hisp $cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All participants, age 15-19 -----
# * 2014-15 ----
round(prop.table(table(off_youngest_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_youngest_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_youngest_1415$cai_unk_12.14, off_youngest_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_youngest_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_youngest_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_youngest_1516$cai_unk_12.15, off_youngest_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_youngest_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_youngest_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_youngest_1617$cai_unk_12.16, off_youngest_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_youngest_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_youngest_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_youngest_1718$cai_unk_12.17, off_youngest_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_youngest_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_youngest_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_youngest_1819$cai_unk_12.18, off_youngest_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_youngest$cai_unk_12.1))*100,1)
round(prop.table(table(all_off_youngest$cai_unk_12.2))*100,1)

# exact2x2((table(all_off_youngest$cai_unk_12.1, all_off_youngest$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All participants, age 20-24 -----
# * 2014-15 ----
round(prop.table(table(off_20.24_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_20.24_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_20.24_1415$cai_unk_12.14, off_20.24_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_20.24_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_20.24_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_20.24_1516$cai_unk_12.15, off_20.24_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_20.24_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_20.24_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_20.24_1617$cai_unk_12.16, off_20.24_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_20.24_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_20.24_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_20.24_1718$cai_unk_12.17, off_20.24_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_20.24_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_20.24_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_20.24_1819$cai_unk_12.18, off_20.24_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_20.24$cai_unk_12.1))*100,1)
round(prop.table(table(all_off_20.24$cai_unk_12.2))*100,1)

# exact2x2((table(all_off_20.24$cai_unk_12.1, all_off_20.24$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * Hispanic, 15-19 ------
# * 2014-15 ----
round(prop.table(table(off_hisp_yngst_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_hisp_yngst_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_hisp_yngst_1415$cai_unk_12.14, off_hisp_yngst_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_hisp_yngst_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_hisp_yngst_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_hisp_yngst_1516$cai_unk_12.15, off_hisp_yngst_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_hisp_yngst_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_hisp_yngst_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_hisp_yngst_1617$cai_unk_12.16, off_hisp_yngst_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_hisp_yngst_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_hisp_yngst_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_hisp_yngst_1718$cai_unk_12.17, off_hisp_yngst_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_hisp_yngst_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_hisp_yngst_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_hisp_yngst_1819$cai_unk_12.18, off_hisp_yngst_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_hisp_youngest$cai_unk_12.1))*100,1)
round(prop.table(table(all_off_hisp_youngest$cai_unk_12.2))*100,1)

# exact2x2((table(all_off_hisp_youngest$cai_unk_12.1, all_off_hisp_youngest$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * Hispanic, 20-24 ------
# * 2014-15 ----
round(prop.table(table(off_hisp_20.24_1415$cai_unk_12.14))*100,1)
round(prop.table(table(off_hisp_20.24_1415$cai_unk_12.15))*100,1)

# exact2x2((table(off_hisp_20.24_1415$cai_unk_12.14, off_hisp_20.24_1415$cai_unk_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_hisp_20.24_1516$cai_unk_12.15))*100,1)
round(prop.table(table(off_hisp_20.24_1516$cai_unk_12.16))*100,1)

# exact2x2((table(off_hisp_20.24_1516$cai_unk_12.15, off_hisp_20.24_1516$cai_unk_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_hisp_20.24_1617$cai_unk_12.16))*100,1)
round(prop.table(table(off_hisp_20.24_1617$cai_unk_12.17))*100,1)

# exact2x2((table(off_hisp_20.24_1617$cai_unk_12.16, off_hisp_20.24_1617$cai_unk_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_hisp_20.24_1718$cai_unk_12.17))*100,1)
round(prop.table(table(off_hisp_20.24_1718$cai_unk_12.18))*100,1)

# exact2x2((table(off_hisp_20.24_1718$cai_unk_12.17, off_hisp_20.24_1718$cai_unk_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_hisp_20.24_1819$cai_unk_12.18))*100,1)
round(prop.table(table(off_hisp_20.24_1819$cai_unk_12.19))*100,1)

# exact2x2((table(off_hisp_20.24_1819$cai_unk_12.18, off_hisp_20.24_1819$cai_unk_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_hisp_20.24$cai_unk_12.1))*100,1)
round(prop.table(table(all_off_hisp_20.24$cai_unk_12.2))*100,1)

# exact2x2((table(all_off_hisp_20.24$cai_unk_12.1, all_off_hisp_20.24$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


#### Time Series for CAI with HIV+ among "stayed off PrEP" group -----

# * All participants, by year -----
# * 2014-15 ----
round(prop.table(table(off_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_1415$cai_pos_12.14, off_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_1516$cai_pos_12.15, off_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_1617$cai_pos_12.16, off_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_1718$cai_pos_12.17, off_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_1819$cai_pos_12.18, off_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off$cai_pos_12.1))*100,1)
round(prop.table(table(all_off$cai_pos_12.2))*100,1)

exact2x2((table(all_off$cai_pos_12.1, all_off$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * <25, by year -----
# * 2014-15 ----
round(prop.table(table(off_u25_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_u25_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_u25_1415$cai_pos_12.14, off_u25_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_u25_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_u25_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_u25_1516$cai_pos_12.15, off_u25_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_u25_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_u25_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_u25_1617$cai_pos_12.16, off_u25_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_u25_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_u25_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_u25_1718$cai_pos_12.17, off_u25_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_u25_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_u25_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_u25_1819$cai_pos_12.18, off_u25_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_u25$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_u25 $cai_pos_12.2))*100,1)

# exact2x2((table(all_off_u25 $cai_pos_12.1, all_off_u25 $cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * >=25, by year -----
# * 2014-15 ----
round(prop.table(table(off_o25_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_o25_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_o25_1415$cai_pos_12.14, off_o25_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_o25_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_o25_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_o25_1516$cai_pos_12.15, off_o25_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_o25_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_o25_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_o25_1617$cai_pos_12.16, off_o25_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_o25_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_o25_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_o25_1718$cai_pos_12.17, off_o25_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_o25_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_o25_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_o25_1819$cai_pos_12.18, off_o25_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_o25$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_o25$cai_pos_12.2))*100,1)

# exact2x2((table(all_off_o25$cai_pos_12.1, all_off_o25$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * NH White -----
# * 2014-15 ----
round(prop.table(table(off_nhw_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_nhw_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_nhw_1415$cai_pos_12.14, off_nhw_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_nhw_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_nhw_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_nhw_1516$cai_pos_12.15, off_nhw_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_nhw_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_nhw_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_nhw_1617$cai_pos_12.16, off_nhw_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_nhw_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_nhw_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_nhw_1718$cai_pos_12.17, off_nhw_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_nhw_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_nhw_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_nhw_1819$cai_pos_12.18, off_nhw_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_nhw$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_nhw $cai_pos_12.2))*100,1)

# exact2x2((table(all_off_nhw $cai_pos_12.1, all_off_nhw $cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * NH Black -----
# * 2014-15 ----
round(prop.table(table(off_nhb_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_nhb_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_nhb_1415$cai_pos_12.14, off_nhb_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_nhb_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_nhb_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_nhb_1516$cai_pos_12.15, off_nhb_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_nhb_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_nhb_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_nhb_1617$cai_pos_12.16, off_nhb_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_nhb_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_nhb_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_nhb_1718$cai_pos_12.17, off_nhb_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_nhb_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_nhb_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_nhb_1819$cai_pos_12.18, off_nhb_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_nhb$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_nhb $cai_pos_12.2))*100,1)

# exact2x2((table(all_off_nhb $cai_pos_12.1, all_off_nhb $cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * NH Other -----
# * 2014-15 ----
round(prop.table(table(off_nho_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_nho_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_nho_1415$cai_pos_12.14, off_nho_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_nho_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_nho_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_nho_1516$cai_pos_12.15, off_nho_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_nho_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_nho_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_nho_1617$cai_pos_12.16, off_nho_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_nho_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_nho_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_nho_1718$cai_pos_12.17, off_nho_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_nho_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_nho_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_nho_1819$cai_pos_12.18, off_nho_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_nho$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_nho $cai_pos_12.2))*100,1)

# exact2x2((table(all_off_nho $cai_pos_12.1, all_off_nho $cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * Hispanic -----
# * 2014-15 ----
round(prop.table(table(off_hisp_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_hisp_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_hisp_1415$cai_pos_12.14, off_hisp_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_hisp_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_hisp_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_hisp_1516$cai_pos_12.15, off_hisp_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_hisp_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_hisp_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_hisp_1617$cai_pos_12.16, off_hisp_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_hisp_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_hisp_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_hisp_1718$cai_pos_12.17, off_hisp_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_hisp_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_hisp_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_hisp_1819$cai_pos_12.18, off_hisp_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_hisp$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_hisp$cai_pos_12.2))*100,1)

# exact2x2((table(all_off_hisp $cai_pos_12.1, all_off_hisp $cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All participants, age 15-19 -----
# * 2014-15 ----
round(prop.table(table(off_youngest_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_youngest_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_youngest_1415$cai_pos_12.14, off_youngest_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_youngest_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_youngest_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_youngest_1516$cai_pos_12.15, off_youngest_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_youngest_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_youngest_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_youngest_1617$cai_pos_12.16, off_youngest_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_youngest_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_youngest_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_youngest_1718$cai_pos_12.17, off_youngest_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_youngest_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_youngest_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_youngest_1819$cai_pos_12.18, off_youngest_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_youngest$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_youngest$cai_pos_12.2))*100,1)

# exact2x2((table(all_off_youngest$cai_pos_12.1, all_off_youngest$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All participants, age 20-24 -----
# * 2014-15 ----
round(prop.table(table(off_20.24_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_20.24_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_20.24_1415$cai_pos_12.14, off_20.24_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_20.24_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_20.24_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_20.24_1516$cai_pos_12.15, off_20.24_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_20.24_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_20.24_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_20.24_1617$cai_pos_12.16, off_20.24_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_20.24_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_20.24_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_20.24_1718$cai_pos_12.17, off_20.24_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_20.24_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_20.24_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_20.24_1819$cai_pos_12.18, off_20.24_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_20.24$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_20.24$cai_pos_12.2))*100,1)

# exact2x2((table(all_off_20.24$cai_pos_12.1, all_off_20.24$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * Hispanic, 15-19 ------
# * 2014-15 ----
round(prop.table(table(off_hisp_yngst_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_hisp_yngst_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_hisp_yngst_1415$cai_pos_12.14, off_hisp_yngst_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_hisp_yngst_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_hisp_yngst_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_hisp_yngst_1516$cai_pos_12.15, off_hisp_yngst_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_hisp_yngst_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_hisp_yngst_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_hisp_yngst_1617$cai_pos_12.16, off_hisp_yngst_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_hisp_yngst_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_hisp_yngst_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_hisp_yngst_1718$cai_pos_12.17, off_hisp_yngst_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_hisp_yngst_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_hisp_yngst_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_hisp_yngst_1819$cai_pos_12.18, off_hisp_yngst_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_hisp_youngest$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_hisp_youngest$cai_pos_12.2))*100,1)

# exact2x2((table(all_off_hisp_youngest$cai_pos_12.1, all_off_hisp_youngest$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * Hispanic, 20-24 ------
# * 2014-15 ----
round(prop.table(table(off_hisp_20.24_1415$cai_pos_12.14))*100,1)
round(prop.table(table(off_hisp_20.24_1415$cai_pos_12.15))*100,1)

# exact2x2((table(off_hisp_20.24_1415$cai_pos_12.14, off_hisp_20.24_1415$cai_pos_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_hisp_20.24_1516$cai_pos_12.15))*100,1)
round(prop.table(table(off_hisp_20.24_1516$cai_pos_12.16))*100,1)

# exact2x2((table(off_hisp_20.24_1516$cai_pos_12.15, off_hisp_20.24_1516$cai_pos_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_hisp_20.24_1617$cai_pos_12.16))*100,1)
round(prop.table(table(off_hisp_20.24_1617$cai_pos_12.17))*100,1)

# exact2x2((table(off_hisp_20.24_1617$cai_pos_12.16, off_hisp_20.24_1617$cai_pos_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_hisp_20.24_1718$cai_pos_12.17))*100,1)
round(prop.table(table(off_hisp_20.24_1718$cai_pos_12.18))*100,1)

# exact2x2((table(off_hisp_20.24_1718$cai_pos_12.17, off_hisp_20.24_1718$cai_pos_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_hisp_20.24_1819$cai_pos_12.18))*100,1)
round(prop.table(table(off_hisp_20.24_1819$cai_pos_12.19))*100,1)

# exact2x2((table(off_hisp_20.24_1819$cai_pos_12.18, off_hisp_20.24_1819$cai_pos_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_hisp_20.24$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_hisp_20.24$cai_pos_12.2))*100,1)

# exact2x2((table(all_off_hisp_20.24$cai_pos_12.1, all_off_hisp_20.24$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * Hispanic, 15-24 -----
round(prop.table(table(all_off_hisp_15.24$cai_pos_12.1))*100,1)
round(prop.table(table(all_off_hisp_15.24$cai_pos_12.2))*100,1)

# exact2x2((table(all_off_hisp_15.24$cai_pos_12.1, all_off_hisp_15.24$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

### Urbanicity -----
# Large, Central Counties POS/NEG/UNK STAT
all_off_nchs_LC <- subset(all_off, nchs4.1 == "1_LgCnt")
round(prop.table(table(all_off_nchs_LC$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_nchs_LC$cai_pos_12.2))*100,0)
exact2x2((table(all_off_nchs_LC$cai_pos_12.1, all_off_nchs_LC$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_nchs_LC$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_nchs_LC$cai_neg_12.2))*100,0)
exact2x2((table(all_off_nchs_LC$cai_neg_12.1, all_off_nchs_LC$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_nchs_LC$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_nchs_LC$cai_unk_12.2))*100,0)
exact2x2((table(all_off_nchs_LC$cai_unk_12.1, all_off_nchs_LC$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# Large, Fringe Counties POS/NEG/UNK STAT
all_off_nchs_LF <- subset(all_off, nchs4.1 == "2_LgFrg")
round(prop.table(table(all_off_nchs_LF$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_nchs_LF$cai_pos_12.2))*100,0)
exact2x2((table(all_off_nchs_LF$cai_pos_12.1, all_off_nchs_LF$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_nchs_LF$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_nchs_LF$cai_neg_12.2))*100,0)
exact2x2((table(all_off_nchs_LF$cai_neg_12.1, all_off_nchs_LF$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_nchs_LF$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_nchs_LF$cai_unk_12.2))*100,0)
exact2x2((table(all_off_nchs_LF$cai_unk_12.1, all_off_nchs_LF$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# Metro Counties POS/NEG/UNK STAT
all_off_nchs_M <- subset(all_off, nchs4.1 == "3_Metro")
round(prop.table(table(all_off_nchs_M$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_nchs_M$cai_pos_12.2))*100,0)
exact2x2((table(all_off_nchs_M$cai_pos_12.1, all_off_nchs_M$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_nchs_M$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_nchs_M$cai_neg_12.2))*100,0)
exact2x2((table(all_off_nchs_M$cai_neg_12.1, all_off_nchs_M$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_nchs_M$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_nchs_M$cai_unk_12.2))*100,0)
exact2x2((table(all_off_nchs_M$cai_unk_12.1, all_off_nchs_M$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# Rural Counties POS/NEG/UNK STAT
all_off_nchs_R <- subset(all_off, nchs4.1 == "4_Rural")
round(prop.table(table(all_off_nchs_R$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_nchs_R$cai_pos_12.2))*100,0)
exact2x2((table(all_off_nchs_R$cai_pos_12.1, all_off_nchs_R$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_nchs_R$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_nchs_R$cai_neg_12.2))*100,0)
exact2x2((table(all_off_nchs_R$cai_neg_12.1, all_off_nchs_R$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_nchs_R$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_nchs_R$cai_unk_12.2))*100,0)
exact2x2((table(all_off_nchs_R$cai_unk_12.1, all_off_nchs_R$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

### Census Region -----
# Northeast POS/NEG/UNK STAT
all_offreg_NE <- subset(all_off, region4.1 == "1_NorE")
round(prop.table(table(all_offreg_NE$cai_pos_12.1))*100,0)
round(prop.table(table(all_offreg_NE$cai_pos_12.2))*100,0)
exact2x2((table(all_offreg_NE$cai_pos_12.1, all_offreg_NE$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_offreg_NE$cai_neg_12.1))*100,0)
round(prop.table(table(all_offreg_NE$cai_neg_12.2))*100,0)
exact2x2((table(all_offreg_NE$cai_neg_12.1, all_offreg_NE$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_offreg_NE$cai_unk_12.1))*100,0)
round(prop.table(table(all_offreg_NE$cai_unk_12.2))*100,0)
exact2x2((table(all_offreg_NE$cai_unk_12.1, all_offreg_NE$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# Midwest POS/NEG/UNK STAT
all_offreg_MW <- subset(all_off, region4.1 == "2_MidW")
round(prop.table(table(all_offreg_MW$cai_pos_12.1))*100,0)
round(prop.table(table(all_offreg_MW$cai_pos_12.2))*100,0)
exact2x2((table(all_offreg_MW$cai_pos_12.1, all_offreg_MW$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_offreg_MW$cai_neg_12.1))*100,0)
round(prop.table(table(all_offreg_MW$cai_neg_12.2))*100,0)
exact2x2((table(all_offreg_MW$cai_neg_12.1, all_offreg_MW$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_offreg_MW$cai_unk_12.1))*100,0)
round(prop.table(table(all_offreg_MW$cai_unk_12.2))*100,0)
exact2x2((table(all_offreg_MW$cai_unk_12.1, all_offreg_MW$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# South POS/NEG/UNK STAT
all_offreg_So <- subset(all_off, region4.1 == "3_South")
round(prop.table(table(all_offreg_So$cai_pos_12.1))*100,0)
round(prop.table(table(all_offreg_So$cai_pos_12.2))*100,0)
exact2x2((table(all_offreg_So$cai_pos_12.1, all_offreg_So$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_offreg_So$cai_neg_12.1))*100,0)
round(prop.table(table(all_offreg_So$cai_neg_12.2))*100,0)
exact2x2((table(all_offreg_So$cai_neg_12.1, all_offreg_So$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_offreg_So$cai_unk_12.1))*100,0)
round(prop.table(table(all_offreg_So$cai_unk_12.2))*100,0)
exact2x2((table(all_offreg_So$cai_unk_12.1, all_offreg_So$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# West POS/NEG/UNK STAT
all_offreg_We <- subset(all_off, region4.1 == "4_West")
round(prop.table(table(all_offreg_We$cai_pos_12.1))*100,0)
round(prop.table(table(all_offreg_We$cai_pos_12.2))*100,0)
exact2x2((table(all_offreg_We$cai_pos_12.1, all_offreg_We$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_offreg_We$cai_neg_12.1))*100,0)
round(prop.table(table(all_offreg_We$cai_neg_12.2))*100,0)
exact2x2((table(all_offreg_We$cai_neg_12.1, all_offreg_We$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_offreg_We$cai_unk_12.1))*100,0)
round(prop.table(table(all_offreg_We$cai_unk_12.2))*100,0)
exact2x2((table(all_offreg_We$cai_unk_12.1, all_offreg_We$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

### Income Groups -----
round(prop.table(table(all_off_LT20$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_LT20$cai_pos_12.2))*100,0)
exact2x2((table(all_off_LT20$cai_pos_12.1, all_off_LT20$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_LT20$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_LT20$cai_neg_12.2))*100,0)
exact2x2((table(all_off_LT20$cai_neg_12.1, all_off_LT20$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_LT20$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_LT20$cai_unk_12.2))*100,0)
exact2x2((table(all_off_LT20$cai_unk_12.1, all_off_LT20$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test



round(prop.table(table(all_off_2039$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_2039$cai_pos_12.2))*100,0)
exact2x2((table(all_off_2039$cai_pos_12.1, all_off_2039$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_2039$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_2039$cai_neg_12.2))*100,0)
exact2x2((table(all_off_2039$cai_neg_12.1, all_off_2039$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_2039$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_2039$cai_unk_12.2))*100,0)
exact2x2((table(all_off_2039$cai_unk_12.1, all_off_2039$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test



round(prop.table(table(all_off_4074$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_4074$cai_pos_12.2))*100,0)
exact2x2((table(all_off_4074$cai_pos_12.1, all_off_4074$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_4074$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_4074$cai_neg_12.2))*100,0)
exact2x2((table(all_off_4074$cai_neg_12.1, all_off_4074$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_4074$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_4074$cai_unk_12.2))*100,0)
exact2x2((table(all_off_4074$cai_unk_12.1, all_off_4074$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test



round(prop.table(table(all_off_AL75$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_AL75$cai_pos_12.2))*100,0)
exact2x2((table(all_off_AL75$cai_pos_12.1, all_off_AL75$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_AL75$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_AL75$cai_neg_12.2))*100,0)
exact2x2((table(all_off_AL75$cai_neg_12.1, all_off_AL75$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_AL75$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_AL75$cai_unk_12.2))*100,0)
exact2x2((table(all_off_AL75$cai_unk_12.1, all_off_AL75$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


### Education -----
round(prop.table(table(all_off_LTHS$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_LTHS$cai_pos_12.2))*100,0)
exact2x2((table(all_off_LTHS$cai_pos_12.1, all_off_LTHS$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_LTHS$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_LTHS$cai_neg_12.2))*100,0)
exact2x2((table(all_off_LTHS$cai_neg_12.1, all_off_LTHS$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_LTHS$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_LTHS$cai_unk_12.2))*100,0)
exact2x2((table(all_off_LTHS$cai_unk_12.1, all_off_LTHS$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(all_off_SC$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_SC$cai_pos_12.2))*100,0)
exact2x2((table(all_off_SC$cai_pos_12.1, all_off_SC$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_SC$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_SC$cai_neg_12.2))*100,0)
exact2x2((table(all_off_SC$cai_neg_12.1, all_off_SC$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_SC$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_SC$cai_unk_12.2))*100,0)
exact2x2((table(all_off_SC$cai_unk_12.1, all_off_SC$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


round(prop.table(table(all_off_Col$cai_pos_12.1))*100,0)
round(prop.table(table(all_off_Col$cai_pos_12.2))*100,0)
exact2x2((table(all_off_Col$cai_pos_12.1, all_off_Col$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_Col$cai_neg_12.1))*100,0)
round(prop.table(table(all_off_Col$cai_neg_12.2))*100,0)
exact2x2((table(all_off_Col$cai_neg_12.1, all_off_Col$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

round(prop.table(table(all_off_Col$cai_unk_12.1))*100,0)
round(prop.table(table(all_off_Col$cai_unk_12.2))*100,0)
exact2x2((table(all_off_Col$cai_unk_12.1, all_off_Col$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

#### Time Series for CAI with HIV-neg among "stayed off PrEP" group -----

# * All participants, by year -----
# * 2014-15 ----
round(prop.table(table(off_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_1415$cai_neg_12.14, off_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_1516$cai_neg_12.15, off_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_1617$cai_neg_12.16, off_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_1718$cai_neg_12.17, off_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_1819$cai_neg_12.18, off_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off$cai_neg_12.1))*100,1)
round(prop.table(table(all_off$cai_neg_12.2))*100,1)

# exact2x2((table(all_off$cai_neg_12.1, all_off$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * <25, by year -----
# * 2014-15 ----
round(prop.table(table(off_u25_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_u25_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_u25_1415$cai_neg_12.14, off_u25_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_u25_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_u25_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_u25_1516$cai_neg_12.15, off_u25_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_u25_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_u25_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_u25_1617$cai_neg_12.16, off_u25_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_u25_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_u25_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_u25_1718$cai_neg_12.17, off_u25_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_u25_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_u25_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_u25_1819$cai_neg_12.18, off_u25_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_u25$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_u25 $cai_neg_12.2))*100,1)

# exact2x2((table(all_off_u25 $cai_neg_12.1, all_off_u25 $cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * >=25, by year -----
# * 2014-15 ----
round(prop.table(table(off_o25_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_o25_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_o25_1415$cai_neg_12.14, off_o25_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_o25_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_o25_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_o25_1516$cai_neg_12.15, off_o25_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_o25_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_o25_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_o25_1617$cai_neg_12.16, off_o25_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_o25_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_o25_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_o25_1718$cai_neg_12.17, off_o25_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_o25_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_o25_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_o25_1819$cai_neg_12.18, off_o25_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_o25$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_o25 $cai_neg_12.2))*100,1)

# exact2x2((table(all_off_o25 $cai_neg_12.1, all_off_o25 $cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * NH White -----
# * 2014-15 ----
round(prop.table(table(off_nhw_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_nhw_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_nhw_1415$cai_neg_12.14, off_nhw_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_nhw_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_nhw_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_nhw_1516$cai_neg_12.15, off_nhw_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_nhw_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_nhw_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_nhw_1617$cai_neg_12.16, off_nhw_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_nhw_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_nhw_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_nhw_1718$cai_neg_12.17, off_nhw_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_nhw_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_nhw_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_nhw_1819$cai_neg_12.18, off_nhw_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_nhw$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_nhw $cai_neg_12.2))*100,1)

# exact2x2((table(all_off_nhw $cai_neg_12.1, all_off_nhw $cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * NH Black -----
# * 2014-15 ----
round(prop.table(table(off_nhb_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_nhb_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_nhb_1415$cai_neg_12.14, off_nhb_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_nhb_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_nhb_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_nhb_1516$cai_neg_12.15, off_nhb_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_nhb_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_nhb_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_nhb_1617$cai_neg_12.16, off_nhb_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_nhb_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_nhb_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_nhb_1718$cai_neg_12.17, off_nhb_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_nhb_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_nhb_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_nhb_1819$cai_neg_12.18, off_nhb_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_nhb$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_nhb $cai_neg_12.2))*100,1)

# exact2x2((table(all_off_nhb $cai_neg_12.1, all_off_nhb $cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * NH Other -----
# * 2014-15 ----
round(prop.table(table(off_nho_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_nho_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_nho_1415$cai_neg_12.14, off_nho_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_nho_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_nho_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_nho_1516$cai_neg_12.15, off_nho_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_nho_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_nho_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_nho_1617$cai_neg_12.16, off_nho_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_nho_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_nho_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_nho_1718$cai_neg_12.17, off_nho_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_nho_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_nho_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_nho_1819$cai_neg_12.18, off_nho_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_nho$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_nho $cai_neg_12.2))*100,1)

# exact2x2((table(all_off_nho $cai_neg_12.1, all_off_nho $cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * Hispanic -----
# * 2014-15 ----
round(prop.table(table(off_hisp_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_hisp_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_hisp_1415$cai_neg_12.14, off_hisp_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_hisp_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_hisp_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_hisp_1516$cai_neg_12.15, off_hisp_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_hisp_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_hisp_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_hisp_1617$cai_neg_12.16, off_hisp_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_hisp_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_hisp_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_hisp_1718$cai_neg_12.17, off_hisp_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_hisp_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_hisp_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_hisp_1819$cai_neg_12.18, off_hisp_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_hisp$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_hisp$cai_neg_12.2))*100,1)

# exact2x2((table(all_off_hisp $cai_neg_12.1, all_off_hisp $cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All participants, age 15-19 -----
# * 2014-15 ----
round(prop.table(table(off_youngest_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_youngest_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_youngest_1415$cai_neg_12.14, off_youngest_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_youngest_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_youngest_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_youngest_1516$cai_neg_12.15, off_youngest_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_youngest_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_youngest_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_youngest_1617$cai_neg_12.16, off_youngest_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_youngest_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_youngest_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_youngest_1718$cai_neg_12.17, off_youngest_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_youngest_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_youngest_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_youngest_1819$cai_neg_12.18, off_youngest_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_youngest$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_youngest$cai_neg_12.2))*100,1)

# exact2x2((table(all_off_youngest$cai_neg_12.1, all_off_youngest$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All participants, age 20-24 -----
# * 2014-15 ----
round(prop.table(table(off_20.24_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_20.24_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_20.24_1415$cai_neg_12.14, off_20.24_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_20.24_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_20.24_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_20.24_1516$cai_neg_12.15, off_20.24_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_20.24_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_20.24_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_20.24_1617$cai_neg_12.16, off_20.24_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_20.24_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_20.24_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_20.24_1718$cai_neg_12.17, off_20.24_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_20.24_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_20.24_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_20.24_1819$cai_neg_12.18, off_20.24_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_20.24$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_20.24$cai_neg_12.2))*100,1)

# exact2x2((table(all_off_20.24$cai_neg_12.1, all_off_20.24$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * Hispanic, 15-19 ------
# * 2014-15 ----
round(prop.table(table(off_hisp_yngst_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_hisp_yngst_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_hisp_yngst_1415$cai_neg_12.14, off_hisp_yngst_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_hisp_yngst_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_hisp_yngst_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_hisp_yngst_1516$cai_neg_12.15, off_hisp_yngst_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_hisp_yngst_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_hisp_yngst_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_hisp_yngst_1617$cai_neg_12.16, off_hisp_yngst_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_hisp_yngst_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_hisp_yngst_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_hisp_yngst_1718$cai_neg_12.17, off_hisp_yngst_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_hisp_yngst_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_hisp_yngst_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_hisp_yngst_1819$cai_neg_12.18, off_hisp_yngst_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_hisp_youngest$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_hisp_youngest$cai_neg_12.2))*100,1)

# exact2x2((table(all_off_hisp_youngest$cai_neg_12.1, all_off_hisp_youngest$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * Hispanic, 20-24 ------
# * 2014-15 ----
round(prop.table(table(off_hisp_20.24_1415$cai_neg_12.14))*100,1)
round(prop.table(table(off_hisp_20.24_1415$cai_neg_12.15))*100,1)

# exact2x2((table(off_hisp_20.24_1415$cai_neg_12.14, off_hisp_20.24_1415$cai_neg_12.15)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2015-16 ----
round(prop.table(table(off_hisp_20.24_1516$cai_neg_12.15))*100,1)
round(prop.table(table(off_hisp_20.24_1516$cai_neg_12.16))*100,1)

# exact2x2((table(off_hisp_20.24_1516$cai_neg_12.15, off_hisp_20.24_1516$cai_neg_12.16)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2016-17 ----
round(prop.table(table(off_hisp_20.24_1617$cai_neg_12.16))*100,1)
round(prop.table(table(off_hisp_20.24_1617$cai_neg_12.17))*100,1)

# exact2x2((table(off_hisp_20.24_1617$cai_neg_12.16, off_hisp_20.24_1617$cai_neg_12.17)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2017-18 ----
round(prop.table(table(off_hisp_20.24_1718$cai_neg_12.17))*100,1)
round(prop.table(table(off_hisp_20.24_1718$cai_neg_12.18))*100,1)

# exact2x2((table(off_hisp_20.24_1718$cai_neg_12.17, off_hisp_20.24_1718$cai_neg_12.18)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * 2018-19 ----
round(prop.table(table(off_hisp_20.24_1819$cai_neg_12.18))*100,1)
round(prop.table(table(off_hisp_20.24_1819$cai_neg_12.19))*100,1)

# exact2x2((table(off_hisp_20.24_1819$cai_neg_12.18, off_hisp_20.24_1819$cai_neg_12.19)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * All years combined ----
round(prop.table(table(all_off_hisp_20.24$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_hisp_20.24$cai_neg_12.2))*100,1)

# exact2x2((table(all_off_hisp_20.24$cai_neg_12.1, all_off_hisp_20.24$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * Hispanic, 15-24 --------
# * All years combined ----
round(prop.table(table(all_off_hisp_15.24$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_hisp_15.24$cai_neg_12.2))*100,1)

# exact2x2((table(all_off_hisp_15.24$cai_neg_12.1, all_off_hisp_15.24$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test




# * * * Hispanic, by age -----
all_off_hisp_u25 <- subset(amis_two_match, 
                           (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & 
                               neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & 
                               race_cat.2 == "Hispanic" &
                               age_25.2 == 0))

all_off_hisp_o25 <- subset(amis_two_match, 
                           (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & 
                               neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & 
                               race_cat.2 == "Hispanic" &
                               age_25.2 == 1))

# * * Hisp, young -----
round(prop.table(table(all_off_hisp_u25$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_hisp_u25$cai_neg_12.2))*100,1)

# exact2x2((table(all_off_hisp_u25$cai_neg_12.1, all_off_hisp_u25$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


# * * Hisp, older -----
round(prop.table(table(all_off_hisp_o25$cai_neg_12.1))*100,1)
round(prop.table(table(all_off_hisp_o25$cai_neg_12.2))*100,1)

# exact2x2((table(all_off_hisp_o25$cai_neg_12.1, all_off_hisp_o25$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

####### TABLE 5 ----------------
##### T5: >=0 SPs ----------
amis_0 <- subset(amis, N_cai_sps_12 >= 0)

#### Match up ID & last ID -----
amis_0$match_id_0415 <- NA
amis_0$match_id_0516 <- NA
amis_0$match_id_0617 <- NA
amis_0$match_id_0718 <- NA
amis_0$match_id_0819 <- NA

amis_014 <- subset(amis_0, year == 2014)
amis_015 <- subset(amis_0, year == 2015)
amis_016 <- subset(amis_0, year == 2016)
amis_017 <- subset(amis_0, year == 2017)
amis_018 <- subset(amis_0, year == 2018)
amis_019 <- subset(amis_0, year == 2019)

### match_id_x variable match-up

amis_014$match_id_0415 <- amis_014$respondent_id
amis_015$match_id_0415 <- amis_015$lastid2014
tableNA(amis_015$lastid2014) #check

amis_015$match_id_0516 <- amis_015$respondent_id
amis_016$match_id_0516 <- amis_016$lastid2015
tableNA(amis_016$lastid2015) #check

amis_016$match_id_0617 <- amis_016$respondent_id
amis_017$match_id_0617 <- amis_017$lastid2016
tableNA(amis_017$lastid2016) #check

amis_017$match_id_0718 <- amis_017$respondent_id
amis_018$match_id_0718 <- amis_018$lastid2017
tableNA(amis_018$lastid2017) #check

amis_018$match_id_0819 <- amis_018$respondent_id
amis_019$match_id_0819 <- amis_019$lastid2018
tableNA(amis_019$lastid2018) #check

# * Match datasets on "match_id_x" using inner_join -----
amis_01415 <- inner_join(amis_014,
                         amis_015,
                         by = "match_id_0415",
                         copy = F,
                         suffix = c(".14", ".15"),
                         keep = T)

amis_01516 <- inner_join(amis_015,
                         amis_016,
                         by = "match_id_0516",
                         copy = F,
                         suffix = c(".15", ".16"),
                         keep = T)

amis_01617 <- inner_join(amis_016,
                         amis_017,
                         by = "match_id_0617",
                         copy = F,
                         suffix = c(".16", ".17"),
                         keep = T)

amis_01718 <- inner_join(amis_017,
                         amis_018,
                         by = "match_id_0718",
                         copy = F,
                         suffix = c(".17", ".18"),
                         keep = T)

amis_01819 <- inner_join(amis_018,
                         amis_019,
                         by = "match_id_0819",
                         copy = F,
                         suffix = c(".18", ".19"),
                         keep = T)

### Merge all years for combined analyses -----
amis_01415_match <- inner_join(amis_014,
                               amis_015,
                               by = "match_id_0415",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_01516_match <- inner_join(amis_015,
                               amis_016,
                               by = "match_id_0516",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_01617_match <- inner_join(amis_016,
                               amis_017,
                               by = "match_id_0617",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_01718_match <- inner_join(amis_017,
                               amis_018,
                               by = "match_id_0718",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_01819_match <- inner_join(amis_018,
                               amis_019,
                               by = "match_id_0819",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_0_two_match <- rbind(amis_01415_match,
                          amis_01516_match,
                          amis_01617_match,
                          amis_01718_match,
                          amis_01819_match)

### Limit to N CAI SPs >=2 -----
off_0_0415 <- subset(amis_01415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12"))
off_0_0516 <- subset(amis_01516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12"))
off_0_0617 <- subset(amis_01617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12"))
off_0_0718 <- subset(amis_01718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12"))
off_0_0819 <- subset(amis_01819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12"))

off_0_u25_0415 <- subset(amis_01415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age_25.15 == 0))
off_0_u25_0516 <- subset(amis_01516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age_25.16 == 0))
off_0_u25_0617 <- subset(amis_01617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age_25.17 == 0))
off_0_u25_0718 <- subset(amis_01718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age_25.18 == 0))
off_0_u25_0819 <- subset(amis_01819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age_25.19 == 0))

off_0_o25_0415 <- subset(amis_01415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age_25.15 == 1))
off_0_o25_0516 <- subset(amis_01516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age_25.16 == 1))
off_0_o25_0617 <- subset(amis_01617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age_25.17 == 1))
off_0_o25_0718 <- subset(amis_01718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age_25.18 == 1))
off_0_o25_0819 <- subset(amis_01819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age_25.19 == 1))

off_0_nhw_0415 <- subset(amis_01415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_White"))
off_0_nhw_0516 <- subset(amis_01516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_White"))
off_0_nhw_0617 <- subset(amis_01617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_White"))
off_0_nhw_0718 <- subset(amis_01718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_White"))
off_0_nhw_0819 <- subset(amis_01819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_White"))

off_0_nhb_0415 <- subset(amis_01415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_Black"))
off_0_nhb_0516 <- subset(amis_01516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_Black"))
off_0_nhb_0617 <- subset(amis_01617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_Black"))
off_0_nhb_0718 <- subset(amis_01718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_Black"))
off_0_nhb_0819 <- subset(amis_01819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_Black"))

off_0_nho_0415 <- subset(amis_01415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_Other"))
off_0_nho_0516 <- subset(amis_01516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_Other"))
off_0_nho_0617 <- subset(amis_01617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_Other"))
off_0_nho_0718 <- subset(amis_01718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_Other"))
off_0_nho_0819 <- subset(amis_01819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_Other"))

off_0_hisp_0415 <- subset(amis_01415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "Hispanic"))
off_0_hisp_0516 <- subset(amis_01516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "Hispanic"))
off_0_hisp_0617 <- subset(amis_01617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "Hispanic"))
off_0_hisp_0718 <- subset(amis_01718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "Hispanic"))
off_0_hisp_0819 <- subset(amis_01819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "Hispanic"))

all_off_0 <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12"))
all_off_0_u25 <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & age_25.1 == 0))
all_off_0_o25 <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & age_25.1 == 1))
all_off_0_nhw <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_White"))
all_off_0_nhb <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_Black"))
all_off_0_nho <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_Other"))
all_off_0_hisp <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "Hispanic"))

all_off_0_lc <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "1_LgCnt"))
all_off_0_lf <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "2_LgFrg"))
all_off_0_metro <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "3_Metro"))
all_off_0_rural <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "4_Rural"))

all_off_0_lths <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & educ3.1 == "1_HS-or-less"))
all_off_0_ltcol <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & educ3.1 == "2_ltCol"))
all_off_0_col <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & educ3.1 == "3_Col+"))

all_off_0_lt20 <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "1_<20k"))
all_off_0_lt40 <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "2_<40k"))
all_off_0_lt75 <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "3_<75k"))
all_off_0_75 <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "4_75k+"))

all_off_0_ne <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "1_NorE"))
all_off_0_mw <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "2_MidW"))
all_off_0_so <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "3_South"))
all_off_0_we <- subset(amis_0_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "4_West"))


##### Get 75th, 90th, 95th %-iles for N CAI SPs, among those with at least 2 CAI SPs, among the PrEP Groups -----
# * All participants -----
# * * 2014,15 -----
round(quantile(off_0_0415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_0415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_0_0516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_0516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_0_0617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_0617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_0_0718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_0718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_0_0819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_0819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_0)
round(quantile(all_off_0$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * <25 -----
# * * 2014,15 -----
round(quantile(off_0_u25_0415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_u25_0415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_0_u25_0516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_u25_0516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_0_u25_0617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_u25_0617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_0_u25_0718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_u25_0718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_0_u25_0819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_u25_0819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_0_u25)
round(quantile(all_off_0_u25$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_u25$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))


# * >=25 -----
# * * 2014,15 -----
round(quantile(off_0_o25_0415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_o25_0415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_0_o25_0516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_o25_0516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_0_o25_0617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_o25_0617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_0_o25_0718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_o25_0718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_0_o25_0819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_o25_0819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_0_o25)
round(quantile(all_off_0_o25$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_o25$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * NH White -----
# * * 2014,15 -----
round(quantile(off_0_nhw_0415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nhw_0415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_0_nhw_0516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nhw_0516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_0_nhw_0617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nhw_0617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_0_nhw_0718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nhw_0718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_0_nhw_0819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nhw_0819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_0_nhw)
round(quantile(all_off_0_nhw$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_nhw$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * NH Black -----
# * * 2014,15 -----
round(quantile(off_0_nhb_0415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nhb_0415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_0_nhb_0516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nhb_0516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_0_nhb_0617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nhb_0617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_0_nhb_0718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nhb_0718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_0_nhb_0819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nhb_0819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_0_nhb)
round(quantile(all_off_0_nhb$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_nhb$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * NH Other -----
# * * 2014,15 -----
round(quantile(off_0_nho_0415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nho_0415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_0_nho_0516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nho_0516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_0_nho_0617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nho_0617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_0_nho_0718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nho_0718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_0_nho_0819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_nho_0819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_0_nho)
round(quantile(all_off_0_nho$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_nho$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * Hispanic -----
# * * 2014,15 -----
round(quantile(off_0_hisp_0415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_hisp_0415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_0_hisp_0516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_hisp_0516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_0_hisp_0617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_hisp_0617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_0_hisp_0718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_hisp_0718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_0_hisp_0819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_0_hisp_0819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_0_hisp)
round(quantile(all_off_0_hisp$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_hisp$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# Wilcoxon signed rank Test & Quantiles for N CAI SPs, year over year, among those with >= 2 -----
# All participants
ncai_y1_all <- c((all_off_0$N_cai_sps_12.1))
ncai_y2_all <- c((all_off_0$N_cai_sps_12.2))

wilcox.test(ncai_y2_all, ncai_y1_all, paired=TRUE, 
            alternative = "greater", exact=F)

## What we're basically asking is: is the number of Rs who 
## shift their #CAIps up more than expected?
## And here's a nice way to get some descriptives on this
# 
# ncai_y1to2_shift_all <- (ncai_y2_all - ncai_y1_all)
# c(sum(ncai_y1to2_shift_all<0), 
#   sum(ncai_y1to2_shift_all==0), 
#   sum(ncai_y1to2_shift_all>0))
# # 72 shift down, 75 stay same, 97 shift up.  More up, but not quite enough to be significant
# 
# ncai_y1to2_shift_u25 <- (ncai_y2_u25 - ncai_y1_u25)
# c(sum(ncai_y1to2_shift_u25<0), 
#   sum(ncai_y1to2_shift_u25==0), 
#   sum(ncai_y1to2_shift_u25>0))
# # 13 shift down, 23 stay same, 22 shift up.  Significantly up!



# under 25
ncai_y1_u25 <- c((all_off_0_u25$N_cai_sps_12.1))
ncai_y2_u25 <- c((all_off_0_u25$N_cai_sps_12.2))

wilcox.test(ncai_y2_u25, ncai_y1_u25, paired=TRUE, 
            alternative = "greater", exact=F)

# 25+
ncai_y1_o25 <- c((all_off_0_o25$N_cai_sps_12.1))
ncai_y2_o25 <- c((all_off_0_o25$N_cai_sps_12.2))

wilcox.test(ncai_y2_o25, ncai_y1_o25, paired=TRUE, 
            alternative = "greater", exact=F)

# NH White
ncai_y1_nhw <- c((all_off_0_nhw$N_cai_sps_12.1))
ncai_y2_nhw <- c((all_off_0_nhw$N_cai_sps_12.2))

wilcox.test(ncai_y2_nhw, ncai_y1_nhw, paired=TRUE, 
            alternative = "greater", exact=F)

# NH Black
ncai_y1_nhb <- c((all_off_0_nhb$N_cai_sps_12.1))
ncai_y2_nhb <- c((all_off_0_nhb$N_cai_sps_12.2))

wilcox.test(ncai_y2_nhb, ncai_y1_nhb, paired=TRUE, 
            alternative = "greater", exact=F)

# NH Other
ncai_y1_nho <- c((all_off_0_nho$N_cai_sps_12.1))
ncai_y2_nho <- c((all_off_0_nho$N_cai_sps_12.2))

wilcox.test(ncai_y2_nho, ncai_y1_nho, paired=TRUE, 
            alternative = "greater", exact=F)

# Hispanic
ncai_y1_hisp <- c((all_off_0_hisp$N_cai_sps_12.1))
ncai_y2_hisp <- c((all_off_0_hisp$N_cai_sps_12.2))

wilcox.test(ncai_y2_hisp, ncai_y1_hisp, paired=TRUE, 
            alternative = "greater", exact=F)


# large, central
ncai_y1_lc <- c((all_off_0_lc$N_cai_sps_12.1))
ncai_y2_lc <- c((all_off_0_lc$N_cai_sps_12.2))

wilcox.test(ncai_y2_lc, ncai_y1_lc, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_lc$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_lc$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_lc)

# large, fringe
ncai_y1_lf <- c((all_off_0_lf$N_cai_sps_12.1))
ncai_y2_lf <- c((all_off_0_lf$N_cai_sps_12.2))

wilcox.test(ncai_y2_lf, ncai_y1_lf, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_lf$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_lf$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_lf)

# metro
ncai_y1_metro <- c((all_off_0_metro$N_cai_sps_12.1))
ncai_y2_metro <- c((all_off_0_metro$N_cai_sps_12.2))

wilcox.test(ncai_y2_metro, ncai_y1_metro, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_metro$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_metro$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_metro)

# rural
ncai_y1_rural <- c((all_off_0_rural$N_cai_sps_12.1))
ncai_y2_rural <- c((all_off_0_rural$N_cai_sps_12.2))

wilcox.test(ncai_y2_rural, ncai_y1_rural, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_rural$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_rural$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_rural)

# less than hs
ncai_y1_lths <- c((all_off_0_lths$N_cai_sps_12.1))
ncai_y2_lths <- c((all_off_0_lths$N_cai_sps_12.2))

wilcox.test(ncai_y2_lths, ncai_y1_lths, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_lths$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_lths$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_lths)

# some college
ncai_y1_ltcol <- c((all_off_0_ltcol$N_cai_sps_12.1))
ncai_y2_ltcol <- c((all_off_0_ltcol$N_cai_sps_12.2))

wilcox.test(ncai_y2_ltcol, ncai_y1_ltcol, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_ltcol$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_ltcol$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_ltcol)

# college and beyond
ncai_y1_col <- c((all_off_0_col$N_cai_sps_12.1))
ncai_y2_col <- c((all_off_0_col$N_cai_sps_12.2))

wilcox.test(ncai_y2_col, ncai_y1_col, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_col$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_col$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_col)

# lt 20k
ncai_y1_lt20 <- c((all_off_0_lt20$N_cai_sps_12.1))
ncai_y2_lt20 <- c((all_off_0_lt20$N_cai_sps_12.2))

wilcox.test(ncai_y2_lt20, ncai_y1_lt20, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_lt20$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_lt20$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_lt20)

# lt 40k
ncai_y1_lt40 <- c((all_off_0_lt40$N_cai_sps_12.1))
ncai_y2_lt40 <- c((all_off_0_lt40$N_cai_sps_12.2))

wilcox.test(ncai_y2_lt40, ncai_y1_lt40, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_lt40$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_lt40$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_lt40)

# lt 75k
ncai_y1_lt75 <- c((all_off_0_lt75$N_cai_sps_12.1))
ncai_y2_lt75 <- c((all_off_0_lt75$N_cai_sps_12.2))

wilcox.test(ncai_y2_lt75, ncai_y1_lt75, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_lt75$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_lt75$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_lt75)

# >= 75k
ncai_y1_75 <- c((all_off_0_75$N_cai_sps_12.1))
ncai_y2_75 <- c((all_off_0_75$N_cai_sps_12.2))

wilcox.test(ncai_y2_75, ncai_y1_75, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_75$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_75$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_75)

# northeast
ncai_y1_ne <- c((all_off_0_ne$N_cai_sps_12.1))
ncai_y2_ne <- c((all_off_0_ne$N_cai_sps_12.2))

wilcox.test(ncai_y2_ne, ncai_y1_ne, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_ne$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_ne$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_ne)

# midwest
ncai_y1_mw <- c((all_off_0_mw$N_cai_sps_12.1))
ncai_y2_mw <- c((all_off_0_mw$N_cai_sps_12.2))

wilcox.test(ncai_y2_mw, ncai_y1_mw, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_mw$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_mw$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_mw)

# south
ncai_y1_so <- c((all_off_0_so$N_cai_sps_12.1))
ncai_y2_so <- c((all_off_0_so$N_cai_sps_12.2))

wilcox.test(ncai_y2_so, ncai_y1_so, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_so$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_so$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_so)

# west
ncai_y1_we <- c((all_off_0_we$N_cai_sps_12.1))
ncai_y2_we <- c((all_off_0_we$N_cai_sps_12.2))

wilcox.test(ncai_y2_we, ncai_y1_we, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_0_we$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_0_we$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_0_we)

##### T5: >= 1 SPs -----------------------
amis_1 <- subset(amis, N_cai_sps_12 >= 1)

#### Match up ID & last ID -----
amis_1$match_id_1415 <- NA
amis_1$match_id_1516 <- NA
amis_1$match_id_1617 <- NA
amis_1$match_id_1718 <- NA
amis_1$match_id_1819 <- NA

amis_114 <- subset(amis_1, year == 2014)
amis_115 <- subset(amis_1, year == 2015)
amis_116 <- subset(amis_1, year == 2016)
amis_117 <- subset(amis_1, year == 2017)
amis_118 <- subset(amis_1, year == 2018)
amis_119 <- subset(amis_1, year == 2019)

### match_id_x variable match-up

amis_114$match_id_1415 <- amis_114$respondent_id
amis_115$match_id_1415 <- amis_115$lastid2014
tableNA(amis_115$lastid2014) #check

amis_115$match_id_1516 <- amis_115$respondent_id
amis_116$match_id_1516 <- amis_116$lastid2015
tableNA(amis_116$lastid2015) #check

amis_116$match_id_1617 <- amis_116$respondent_id
amis_117$match_id_1617 <- amis_117$lastid2016
tableNA(amis_117$lastid2016) #check

amis_117$match_id_1718 <- amis_117$respondent_id
amis_118$match_id_1718 <- amis_118$lastid2017
tableNA(amis_118$lastid2017) #check

amis_118$match_id_1819 <- amis_118$respondent_id
amis_119$match_id_1819 <- amis_119$lastid2018
tableNA(amis_119$lastid2018) #check

# * Match datasets on "match_id_x" using inner_join -----
amis_11415 <- inner_join(amis_114,
                         amis_115,
                         by = "match_id_1415",
                         copy = F,
                         suffix = c(".14", ".15"),
                         keep = T)

amis_11516 <- inner_join(amis_115,
                         amis_116,
                         by = "match_id_1516",
                         copy = F,
                         suffix = c(".15", ".16"),
                         keep = T)

amis_11617 <- inner_join(amis_116,
                         amis_117,
                         by = "match_id_1617",
                         copy = F,
                         suffix = c(".16", ".17"),
                         keep = T)

amis_11718 <- inner_join(amis_117,
                         amis_118,
                         by = "match_id_1718",
                         copy = F,
                         suffix = c(".17", ".18"),
                         keep = T)

amis_11819 <- inner_join(amis_118,
                         amis_119,
                         by = "match_id_1819",
                         copy = F,
                         suffix = c(".18", ".19"),
                         keep = T)

### Merge all years for combined analyses -----
amis_11415_match <- inner_join(amis_114,
                               amis_115,
                               by = "match_id_1415",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_11516_match <- inner_join(amis_115,
                               amis_116,
                               by = "match_id_1516",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_11617_match <- inner_join(amis_116,
                               amis_117,
                               by = "match_id_1617",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_11718_match <- inner_join(amis_117,
                               amis_118,
                               by = "match_id_1718",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_11819_match <- inner_join(amis_118,
                               amis_119,
                               by = "match_id_1819",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_1_two_match <- rbind(amis_11415_match,
                          amis_11516_match,
                          amis_11617_match,
                          amis_11718_match,
                          amis_11819_match)

### Limit to N CAI SPs >=2 -----
off_1_1415 <- subset(amis_11415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12"))
off_1_1516 <- subset(amis_11516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12"))
off_1_1617 <- subset(amis_11617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12"))
off_1_1718 <- subset(amis_11718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12"))
off_1_1819 <- subset(amis_11819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12"))

off_1_u25_1415 <- subset(amis_11415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age_25.15 == 0))
off_1_u25_1516 <- subset(amis_11516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age_25.16 == 0))
off_1_u25_1617 <- subset(amis_11617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age_25.17 == 0))
off_1_u25_1718 <- subset(amis_11718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age_25.18 == 0))
off_1_u25_1819 <- subset(amis_11819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age_25.19 == 0))

off_1_o25_1415 <- subset(amis_11415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age_25.15 == 1))
off_1_o25_1516 <- subset(amis_11516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age_25.16 == 1))
off_1_o25_1617 <- subset(amis_11617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age_25.17 == 1))
off_1_o25_1718 <- subset(amis_11718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age_25.18 == 1))
off_1_o25_1819 <- subset(amis_11819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age_25.19 == 1))

off_1_nhw_1415 <- subset(amis_11415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_White"))
off_1_nhw_1516 <- subset(amis_11516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_White"))
off_1_nhw_1617 <- subset(amis_11617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_White"))
off_1_nhw_1718 <- subset(amis_11718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_White"))
off_1_nhw_1819 <- subset(amis_11819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_White"))

off_1_nhb_1415 <- subset(amis_11415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_Black"))
off_1_nhb_1516 <- subset(amis_11516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_Black"))
off_1_nhb_1617 <- subset(amis_11617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_Black"))
off_1_nhb_1718 <- subset(amis_11718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_Black"))
off_1_nhb_1819 <- subset(amis_11819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_Black"))

off_1_nho_1415 <- subset(amis_11415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_Other"))
off_1_nho_1516 <- subset(amis_11516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_Other"))
off_1_nho_1617 <- subset(amis_11617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_Other"))
off_1_nho_1718 <- subset(amis_11718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_Other"))
off_1_nho_1819 <- subset(amis_11819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_Other"))

off_1_hisp_1415 <- subset(amis_11415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "Hispanic"))
off_1_hisp_1516 <- subset(amis_11516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "Hispanic"))
off_1_hisp_1617 <- subset(amis_11617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "Hispanic"))
off_1_hisp_1718 <- subset(amis_11718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "Hispanic"))
off_1_hisp_1819 <- subset(amis_11819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "Hispanic"))

all_off_1 <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12"))
all_off_1_u25 <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & age_25.1 == 0))
all_off_1_o25 <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & age_25.1 == 1))
all_off_1_nhw <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_White"))
all_off_1_nhb <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_Black"))
all_off_1_nho <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_Other"))
all_off_1_hisp <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "Hispanic"))

all_off_1_lc <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "1_LgCnt"))
all_off_1_lf <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "2_LgFrg"))
all_off_1_metro <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "3_Metro"))
all_off_1_rural <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "4_Rural"))

all_off_1_lths <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & educ3.1 == "1_HS-or-less"))
all_off_1_ltcol <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & educ3.1 == "2_ltCol"))
all_off_1_col <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & educ3.1 == "3_Col+"))

all_off_1_lt20 <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "1_<20k"))
all_off_1_lt40 <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "2_<40k"))
all_off_1_lt75 <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "3_<75k"))
all_off_1_75 <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "4_75k+"))

all_off_1_ne <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "1_NorE"))
all_off_1_mw <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "2_MidW"))
all_off_1_so <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "3_South"))
all_off_1_we <- subset(amis_1_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "4_West"))


##### Get 75th, 90th, 95th %-iles for N CAI SPs, among those with at least 2 CAI SPs, among the PrEP Groups -----
# * All participants -----
# * * 2014,15 -----
round(quantile(off_1_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_1_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_1_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_1_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_1_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_1)
round(quantile(all_off_1$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * <25 -----
# * * 2014,15 -----
round(quantile(off_1_u25_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_u25_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_1_u25_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_u25_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_1_u25_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_u25_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_1_u25_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_u25_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_1_u25_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_u25_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_1_u25)
round(quantile(all_off_1_u25$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_u25$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))


# * >=25 -----
# * * 2014,15 -----
round(quantile(off_1_o25_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_o25_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_1_o25_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_o25_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_1_o25_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_o25_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_1_o25_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_o25_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_1_o25_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_o25_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_1_o25)
round(quantile(all_off_1_o25$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_o25$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * NH White -----
# * * 2014,15 -----
round(quantile(off_1_nhw_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nhw_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_1_nhw_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nhw_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_1_nhw_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nhw_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_1_nhw_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nhw_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_1_nhw_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nhw_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_1_nhw)
round(quantile(all_off_1_nhw$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_nhw$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * NH Black -----
# * * 2014,15 -----
round(quantile(off_1_nhb_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nhb_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_1_nhb_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nhb_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_1_nhb_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nhb_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_1_nhb_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nhb_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_1_nhb_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nhb_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_1_nhb)
round(quantile(all_off_1_nhb$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_nhb$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * NH Other -----
# * * 2014,15 -----
round(quantile(off_1_nho_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nho_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_1_nho_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nho_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_1_nho_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nho_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_1_nho_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nho_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_1_nho_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_nho_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_1_nho)
round(quantile(all_off_1_nho$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_nho$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * Hispanic -----
# * * 2014,15 -----
round(quantile(off_1_hisp_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_hisp_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_1_hisp_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_hisp_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_1_hisp_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_hisp_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_1_hisp_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_hisp_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_1_hisp_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_1_hisp_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
nrow(all_off_1_hisp)
round(quantile(all_off_1_hisp$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_hisp$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# Wilcoxon signed rank Test & Quantiles for N CAI SPs, year over year, among those with >= 2 -----
# All participants
ncai_y1_all <- c((all_off_1$N_cai_sps_12.1))
ncai_y2_all <- c((all_off_1$N_cai_sps_12.2))

wilcox.test(ncai_y2_all, ncai_y1_all, paired=TRUE, 
            alternative = "greater", exact=F)

## What we're basically asking is: is the number of Rs who 
## shift their #CAIps up more than expected?
## And here's a nice way to get some descriptives on this
# 
# ncai_y1to2_shift_all <- (ncai_y2_all - ncai_y1_all)
# c(sum(ncai_y1to2_shift_all<0), 
#   sum(ncai_y1to2_shift_all==0), 
#   sum(ncai_y1to2_shift_all>0))
# # 72 shift down, 75 stay same, 97 shift up.  More up, but not quite enough to be significant
# 
# ncai_y1to2_shift_u25 <- (ncai_y2_u25 - ncai_y1_u25)
# c(sum(ncai_y1to2_shift_u25<0), 
#   sum(ncai_y1to2_shift_u25==0), 
#   sum(ncai_y1to2_shift_u25>0))
# # 13 shift down, 23 stay same, 22 shift up.  Significantly up!



# under 25
ncai_y1_u25 <- c((all_off_1_u25$N_cai_sps_12.1))
ncai_y2_u25 <- c((all_off_1_u25$N_cai_sps_12.2))

wilcox.test(ncai_y2_u25, ncai_y1_u25, paired=TRUE, 
            alternative = "greater", exact=F)

# 25+
ncai_y1_o25 <- c((all_off_1_o25$N_cai_sps_12.1))
ncai_y2_o25 <- c((all_off_1_o25$N_cai_sps_12.2))

wilcox.test(ncai_y2_o25, ncai_y1_o25, paired=TRUE, 
            alternative = "greater", exact=F)

# NH White
ncai_y1_nhw <- c((all_off_1_nhw$N_cai_sps_12.1))
ncai_y2_nhw <- c((all_off_1_nhw$N_cai_sps_12.2))

wilcox.test(ncai_y2_nhw, ncai_y1_nhw, paired=TRUE, 
            alternative = "greater", exact=F)

# NH Black
ncai_y1_nhb <- c((all_off_1_nhb$N_cai_sps_12.1))
ncai_y2_nhb <- c((all_off_1_nhb$N_cai_sps_12.2))

wilcox.test(ncai_y2_nhb, ncai_y1_nhb, paired=TRUE, 
            alternative = "greater", exact=F)

# NH Other
ncai_y1_nho <- c((all_off_1_nho$N_cai_sps_12.1))
ncai_y2_nho <- c((all_off_1_nho$N_cai_sps_12.2))

wilcox.test(ncai_y2_nho, ncai_y1_nho, paired=TRUE, 
            alternative = "greater", exact=F)

# Hispanic
ncai_y1_hisp <- c((all_off_1_hisp$N_cai_sps_12.1))
ncai_y2_hisp <- c((all_off_1_hisp$N_cai_sps_12.2))

wilcox.test(ncai_y2_hisp, ncai_y1_hisp, paired=TRUE, 
            alternative = "greater", exact=F)


# large, central
ncai_y1_lc <- c((all_off_1_lc$N_cai_sps_12.1))
ncai_y2_lc <- c((all_off_1_lc$N_cai_sps_12.2))

wilcox.test(ncai_y2_lc, ncai_y1_lc, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_lc$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_lc$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_lc)

# large, fringe
ncai_y1_lf <- c((all_off_1_lf$N_cai_sps_12.1))
ncai_y2_lf <- c((all_off_1_lf$N_cai_sps_12.2))

wilcox.test(ncai_y2_lf, ncai_y1_lf, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_lf$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_lf$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_lf)

# metro
ncai_y1_metro <- c((all_off_1_metro$N_cai_sps_12.1))
ncai_y2_metro <- c((all_off_1_metro$N_cai_sps_12.2))

wilcox.test(ncai_y2_metro, ncai_y1_metro, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_metro$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_metro$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_metro)

# rural
ncai_y1_rural <- c((all_off_1_rural$N_cai_sps_12.1))
ncai_y2_rural <- c((all_off_1_rural$N_cai_sps_12.2))

wilcox.test(ncai_y2_rural, ncai_y1_rural, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_rural$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_rural$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_rural)

# less than hs
ncai_y1_lths <- c((all_off_1_lths$N_cai_sps_12.1))
ncai_y2_lths <- c((all_off_1_lths$N_cai_sps_12.2))

wilcox.test(ncai_y2_lths, ncai_y1_lths, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_lths$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_lths$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_lths)

# some college
ncai_y1_ltcol <- c((all_off_1_ltcol$N_cai_sps_12.1))
ncai_y2_ltcol <- c((all_off_1_ltcol$N_cai_sps_12.2))

wilcox.test(ncai_y2_ltcol, ncai_y1_ltcol, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_ltcol$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_ltcol$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_ltcol)

# college and beyond
ncai_y1_col <- c((all_off_1_col$N_cai_sps_12.1))
ncai_y2_col <- c((all_off_1_col$N_cai_sps_12.2))

wilcox.test(ncai_y2_col, ncai_y1_col, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_col$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_col$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_col)

# lt 20k
ncai_y1_lt20 <- c((all_off_1_lt20$N_cai_sps_12.1))
ncai_y2_lt20 <- c((all_off_1_lt20$N_cai_sps_12.2))

wilcox.test(ncai_y2_lt20, ncai_y1_lt20, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_lt20$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_lt20$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_lt20)

# lt 40k
ncai_y1_lt40 <- c((all_off_1_lt40$N_cai_sps_12.1))
ncai_y2_lt40 <- c((all_off_1_lt40$N_cai_sps_12.2))

wilcox.test(ncai_y2_lt40, ncai_y1_lt40, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_lt40$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_lt40$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_lt40)

# lt 75k
ncai_y1_lt75 <- c((all_off_1_lt75$N_cai_sps_12.1))
ncai_y2_lt75 <- c((all_off_1_lt75$N_cai_sps_12.2))

wilcox.test(ncai_y2_lt75, ncai_y1_lt75, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_lt75$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_lt75$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_lt75)

# >= 75k
ncai_y1_75 <- c((all_off_1_75$N_cai_sps_12.1))
ncai_y2_75 <- c((all_off_1_75$N_cai_sps_12.2))

wilcox.test(ncai_y2_75, ncai_y1_75, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_75$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_75$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_75)

# northeast
ncai_y1_ne <- c((all_off_1_ne$N_cai_sps_12.1))
ncai_y2_ne <- c((all_off_1_ne$N_cai_sps_12.2))

wilcox.test(ncai_y2_ne, ncai_y1_ne, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_ne$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_ne$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_ne)

# midwest
ncai_y1_mw <- c((all_off_1_mw$N_cai_sps_12.1))
ncai_y2_mw <- c((all_off_1_mw$N_cai_sps_12.2))

wilcox.test(ncai_y2_mw, ncai_y1_mw, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_mw$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_mw$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_mw)

# south
ncai_y1_so <- c((all_off_1_so$N_cai_sps_12.1))
ncai_y2_so <- c((all_off_1_so$N_cai_sps_12.2))

wilcox.test(ncai_y2_so, ncai_y1_so, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_so$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_so$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_so)

# west
ncai_y1_we <- c((all_off_1_we$N_cai_sps_12.1))
ncai_y2_we <- c((all_off_1_we$N_cai_sps_12.2))

wilcox.test(ncai_y2_we, ncai_y1_we, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_1_we$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_1_we$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))
nrow(all_off_1_we)

##### T5: >= 2 SPs ------------------------------
### Select only those with at least 2 CAI SPs -----

amis_2 <- subset(amis, N_cai_sps_12 >= 2)

#### Match up ID & last ID -----
amis_2$match_id_1415 <- NA
amis_2$match_id_1516 <- NA
amis_2$match_id_1617 <- NA
amis_2$match_id_1718 <- NA
amis_2$match_id_1819 <- NA

amis_214 <- subset(amis_2, year == 2014)
amis_215 <- subset(amis_2, year == 2015)
amis_216 <- subset(amis_2, year == 2016)
amis_217 <- subset(amis_2, year == 2017)
amis_218 <- subset(amis_2, year == 2018)
amis_219 <- subset(amis_2, year == 2019)

### match_id_x variable match-up

amis_214$match_id_1415 <- amis_214$respondent_id
amis_215$match_id_1415 <- amis_215$lastid2014
tableNA(amis_215$lastid2014) #check

amis_215$match_id_1516 <- amis_215$respondent_id
amis_216$match_id_1516 <- amis_216$lastid2015
tableNA(amis_216$lastid2015) #check

amis_216$match_id_1617 <- amis_216$respondent_id
amis_217$match_id_1617 <- amis_217$lastid2016
tableNA(amis_217$lastid2016) #check

amis_217$match_id_1718 <- amis_217$respondent_id
amis_218$match_id_1718 <- amis_218$lastid2017
tableNA(amis_218$lastid2017) #check

amis_218$match_id_1819 <- amis_218$respondent_id
amis_219$match_id_1819 <- amis_219$lastid2018
tableNA(amis_219$lastid2018) #check

# * Match datasets on "match_id_x" using inner_join -----
amis_21415 <- inner_join(amis_214,
                         amis_215,
                         by = "match_id_1415",
                         copy = F,
                         suffix = c(".14", ".15"),
                         keep = T)

amis_21516 <- inner_join(amis_215,
                         amis_216,
                         by = "match_id_1516",
                         copy = F,
                         suffix = c(".15", ".16"),
                         keep = T)

amis_21617 <- inner_join(amis_216,
                         amis_217,
                         by = "match_id_1617",
                         copy = F,
                         suffix = c(".16", ".17"),
                         keep = T)

amis_21718 <- inner_join(amis_217,
                         amis_218,
                         by = "match_id_1718",
                         copy = F,
                         suffix = c(".17", ".18"),
                         keep = T)

amis_21819 <- inner_join(amis_218,
                         amis_219,
                         by = "match_id_1819",
                         copy = F,
                         suffix = c(".18", ".19"),
                         keep = T)

### Merge all years for combined analyses -----
amis_21415_match <- inner_join(amis_214,
                               amis_215,
                               by = "match_id_1415",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_21516_match <- inner_join(amis_215,
                               amis_216,
                               by = "match_id_1516",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_21617_match <- inner_join(amis_216,
                               amis_217,
                               by = "match_id_1617",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_21718_match <- inner_join(amis_217,
                               amis_218,
                               by = "match_id_1718",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_21819_match <- inner_join(amis_218,
                               amis_219,
                               by = "match_id_1819",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

amis_2_two_match <- rbind(amis_21415_match,
                          amis_21516_match,
                          amis_21617_match,
                          amis_21718_match,
                          amis_21819_match)


### Limit to N CAI SPs >=2 -----
off_2_1415 <- subset(amis_21415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12"))
off_2_1516 <- subset(amis_21516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12"))
off_2_1617 <- subset(amis_21617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12"))
off_2_1718 <- subset(amis_21718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12"))
off_2_1819 <- subset(amis_21819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12"))

off_2_u25_1415 <- subset(amis_21415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age_25.15 == 0))
off_2_u25_1516 <- subset(amis_21516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age_25.16 == 0))
off_2_u25_1617 <- subset(amis_21617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age_25.17 == 0))
off_2_u25_1718 <- subset(amis_21718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age_25.18 == 0))
off_2_u25_1819 <- subset(amis_21819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age_25.19 == 0))

off_2_o25_1415 <- subset(amis_21415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & age_25.15 == 1))
off_2_o25_1516 <- subset(amis_21516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & age_25.16 == 1))
off_2_o25_1617 <- subset(amis_21617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & age_25.17 == 1))
off_2_o25_1718 <- subset(amis_21718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & age_25.18 == 1))
off_2_o25_1819 <- subset(amis_21819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & age_25.19 == 1))

off_2_nhw_1415 <- subset(amis_21415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_White"))
off_2_nhw_1516 <- subset(amis_21516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_White"))
off_2_nhw_1617 <- subset(amis_21617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_White"))
off_2_nhw_1718 <- subset(amis_21718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_White"))
off_2_nhw_1819 <- subset(amis_21819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_White"))

off_2_nhb_1415 <- subset(amis_21415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_Black"))
off_2_nhb_1516 <- subset(amis_21516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_Black"))
off_2_nhb_1617 <- subset(amis_21617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_Black"))
off_2_nhb_1718 <- subset(amis_21718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_Black"))
off_2_nhb_1819 <- subset(amis_21819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_Black"))

off_2_nho_1415 <- subset(amis_21415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "NH_Other"))
off_2_nho_1516 <- subset(amis_21516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "NH_Other"))
off_2_nho_1617 <- subset(amis_21617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "NH_Other"))
off_2_nho_1718 <- subset(amis_21718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "NH_Other"))
off_2_nho_1819 <- subset(amis_21819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "NH_Other"))

off_2_hisp_1415 <- subset(amis_21415, (neg_prep12_use_groups.14 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & race_cat.15 == "Hispanic"))
off_2_hisp_1516 <- subset(amis_21516, (neg_prep12_use_groups.15 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & race_cat.16 == "Hispanic"))
off_2_hisp_1617 <- subset(amis_21617, (neg_prep12_use_groups.16 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & race_cat.17 == "Hispanic"))
off_2_hisp_1718 <- subset(amis_21718, (neg_prep12_use_groups.17 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & race_cat.18 == "Hispanic"))
off_2_hisp_1819 <- subset(amis_21819, (neg_prep12_use_groups.18 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.19 == "01_never-or-no-PrEP12" & race_cat.19 == "Hispanic"))

all_off_2 <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12"))
all_off_2_u25 <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & age_25.1 == 0))
all_off_2_o25 <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & age_25.1 == 1))
all_off_2_nhw <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_White"))
all_off_2_nhb <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_Black"))
all_off_2_nho <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "NH_Other"))
all_off_2_hisp <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & race_cat.1 == "Hispanic"))

all_off_2_lc <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "1_LgCnt"))
all_off_2_lf <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "2_LgFrg"))
all_off_2_metro <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "3_Metro"))
all_off_2_rural <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & nchs4.1 == "4_Rural"))

all_off_2_lths <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & educ3.1 == "1_HS-or-less"))
all_off_2_ltcol <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & educ3.1 == "2_ltCol"))
all_off_2_col <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & educ3.1 == "3_Col+"))

all_off_2_lt20 <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "1_<20k"))
all_off_2_lt40 <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "2_<40k"))
all_off_2_lt75 <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "3_<75k"))
all_off_2_75 <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & income_cat.1 == "4_75k+"))

all_off_2_ne <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "1_NorE"))
all_off_2_mw <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "2_MidW"))
all_off_2_so <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "3_South"))
all_off_2_we <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "01_never-or-no-PrEP12" & region4.1 == "4_West"))


##### Get 75th, 90th, 95th %-iles for N CAI SPs, among those with at least 2 CAI SPs, among the PrEP Groups -----
# * All participants -----
# * * 2014,15 -----
round(quantile(off_2_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_2_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_2_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_2_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_2_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
round(quantile(all_off_2$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * <25 -----
# * * 2014,15 -----
round(quantile(off_2_u25_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_u25_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_2_u25_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_u25_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_2_u25_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_u25_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_2_u25_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_u25_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_2_u25_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_u25_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
round(quantile(all_off_2_u25$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_u25$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))


# * >=25 -----
# * * 2014,15 -----
round(quantile(off_2_o25_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_o25_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_2_o25_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_o25_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_2_o25_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_o25_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_2_o25_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_o25_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_2_o25_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_o25_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
round(quantile(all_off_2_o25$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_o25$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * NH White -----
# * * 2014,15 -----
round(quantile(off_2_nhw_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nhw_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_2_nhw_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nhw_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_2_nhw_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nhw_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_2_nhw_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nhw_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_2_nhw_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nhw_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
round(quantile(all_off_2_nhw$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_nhw$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * NH Black -----
# * * 2014,15 -----
round(quantile(off_2_nhb_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nhb_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_2_nhb_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nhb_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_2_nhb_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nhb_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_2_nhb_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nhb_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_2_nhb_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nhb_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
round(quantile(all_off_2_nhb$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_nhb$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * NH Other -----
# * * 2014,15 -----
round(quantile(off_2_nho_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nho_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_2_nho_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nho_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_2_nho_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nho_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_2_nho_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nho_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_2_nho_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_nho_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
round(quantile(all_off_2_nho$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_nho$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# * Hispanic -----
# * * 2014,15 -----
round(quantile(off_2_hisp_1415$N_cai_sps_12.14, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_hisp_1415$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))

# * * 2015,16 -----
round(quantile(off_2_hisp_1516$N_cai_sps_12.15, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_hisp_1516$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))

# * * 2016,17 -----
round(quantile(off_2_hisp_1617$N_cai_sps_12.16, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_hisp_1617$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))

# * * 2017,18 -----
round(quantile(off_2_hisp_1718$N_cai_sps_12.17, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_hisp_1718$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))

# * * 2018,19 -----
round(quantile(off_2_hisp_1819$N_cai_sps_12.18, c(.75, .9, .95), na.rm = T))
round(quantile(off_2_hisp_1819$N_cai_sps_12.19, c(.75, .9, .95), na.rm = T))

# * * All years -----
round(quantile(all_off_2_hisp$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_hisp$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# Wilcoxon signed rank Test & Quantiles for N CAI SPs, year over year, among those with >= 2 -----
# All participants
ncai_y1_all <- c((all_off_2$N_cai_sps_12.1))
ncai_y2_all <- c((all_off_2$N_cai_sps_12.2))

wilcox.test(ncai_y2_all, ncai_y1_all, paired=TRUE, 
            alternative = "greater", exact=F)

## What we're basically asking is: is the number of Rs who 
## shift their #CAIps up more than expected?
## And here's a nice way to get some descriptives on this
# 
# ncai_y1to2_shift_all <- (ncai_y2_all - ncai_y1_all)
# c(sum(ncai_y1to2_shift_all<0), 
#   sum(ncai_y1to2_shift_all==0), 
#   sum(ncai_y1to2_shift_all>0))
# # 72 shift down, 75 stay same, 97 shift up.  More up, but not quite enough to be significant
# 
# ncai_y1to2_shift_u25 <- (ncai_y2_u25 - ncai_y1_u25)
# c(sum(ncai_y1to2_shift_u25<0), 
#   sum(ncai_y1to2_shift_u25==0), 
#   sum(ncai_y1to2_shift_u25>0))
# # 13 shift down, 23 stay same, 22 shift up.  Significantly up!



# under 25
ncai_y1_u25 <- c((all_off_2_u25$N_cai_sps_12.1))
ncai_y2_u25 <- c((all_off_2_u25$N_cai_sps_12.2))

wilcox.test(ncai_y2_u25, ncai_y1_u25, paired=TRUE, 
            alternative = "greater", exact=F)

# 25+
ncai_y1_o25 <- c((all_off_2_o25$N_cai_sps_12.1))
ncai_y2_o25 <- c((all_off_2_o25$N_cai_sps_12.2))

wilcox.test(ncai_y2_o25, ncai_y1_o25, paired=TRUE, 
            alternative = "greater", exact=F)

# NH White
ncai_y1_nhw <- c((all_off_2_nhw$N_cai_sps_12.1))
ncai_y2_nhw <- c((all_off_2_nhw$N_cai_sps_12.2))

wilcox.test(ncai_y2_nhw, ncai_y1_nhw, paired=TRUE, 
            alternative = "greater", exact=F)

# NH Black
ncai_y1_nhb <- c((all_off_2_nhb$N_cai_sps_12.1))
ncai_y2_nhb <- c((all_off_2_nhb$N_cai_sps_12.2))

# NH Other
ncai_y1_nho <- c((all_off_2_nho$N_cai_sps_12.1))
ncai_y2_nho <- c((all_off_2_nho$N_cai_sps_12.2))

# Hispanic
ncai_y1_hisp <- c((all_off_2_hisp$N_cai_sps_12.1))
ncai_y2_hisp <- c((all_off_2_hisp$N_cai_sps_12.2))

wilcox.test(ncai_y2_hisp, ncai_y1_hisp, paired=TRUE, 
            alternative = "greater", exact=F)


# large, central
ncai_y1_lc <- c((all_off_2_lc$N_cai_sps_12.1))
ncai_y2_lc <- c((all_off_2_lc$N_cai_sps_12.2))

wilcox.test(ncai_y2_lc, ncai_y1_lc, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_lc$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_lc$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# large, fringe
ncai_y1_lf <- c((all_off_2_lf$N_cai_sps_12.1))
ncai_y2_lf <- c((all_off_2_lf$N_cai_sps_12.2))

wilcox.test(ncai_y2_lf, ncai_y1_lf, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_lf$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_lf$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# metro
ncai_y1_metro <- c((all_off_2_metro$N_cai_sps_12.1))
ncai_y2_metro <- c((all_off_2_metro$N_cai_sps_12.2))

wilcox.test(ncai_y2_metro, ncai_y1_metro, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_metro$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_metro$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# rural
ncai_y1_rural <- c((all_off_2_rural$N_cai_sps_12.1))
ncai_y2_rural <- c((all_off_2_rural$N_cai_sps_12.2))

wilcox.test(ncai_y2_rural, ncai_y1_rural, paired=TRUE, 
            alternative = "greater", exact=F)

# less than hs
ncai_y1_lths <- c((all_off_2_lths$N_cai_sps_12.1))
ncai_y2_lths <- c((all_off_2_lths$N_cai_sps_12.2))

wilcox.test(ncai_y2_lths, ncai_y1_lths, paired=TRUE, 
            alternative = "greater", exact=F)

# some college
ncai_y1_ltcol <- c((all_off_2_ltcol$N_cai_sps_12.1))
ncai_y2_ltcol <- c((all_off_2_ltcol$N_cai_sps_12.2))

wilcox.test(ncai_y2_ltcol, ncai_y1_ltcol, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_ltcol$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_ltcol$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# college and beyond
ncai_y1_col <- c((all_off_2_col$N_cai_sps_12.1))
ncai_y2_col <- c((all_off_2_col$N_cai_sps_12.2))

wilcox.test(ncai_y2_col, ncai_y1_col, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_col$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_col$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# lt 20k
ncai_y1_lt20 <- c((all_off_2_lt20$N_cai_sps_12.1))
ncai_y2_lt20 <- c((all_off_2_lt20$N_cai_sps_12.2))

wilcox.test(ncai_y2_lt20, ncai_y1_lt20, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_lt20$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_lt20$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# lt 40k
ncai_y1_lt40 <- c((all_off_2_lt40$N_cai_sps_12.1))
ncai_y2_lt40 <- c((all_off_2_lt40$N_cai_sps_12.2))

wilcox.test(ncai_y2_lt40, ncai_y1_lt40, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_lt40$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_lt40$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# lt 75k
ncai_y1_lt75 <- c((all_off_2_lt75$N_cai_sps_12.1))
ncai_y2_lt75 <- c((all_off_2_lt75$N_cai_sps_12.2))

wilcox.test(ncai_y2_lt75, ncai_y1_lt75, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_lt75$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_lt75$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# >= 75k
ncai_y1_75 <- c((all_off_2_75$N_cai_sps_12.1))
ncai_y2_75 <- c((all_off_2_75$N_cai_sps_12.2))

wilcox.test(ncai_y2_75, ncai_y1_75, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_75$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_75$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# northeast
ncai_y1_ne <- c((all_off_2_ne$N_cai_sps_12.1))
ncai_y2_ne <- c((all_off_2_ne$N_cai_sps_12.2))

wilcox.test(ncai_y2_ne, ncai_y1_ne, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_ne$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_ne$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# midwest
ncai_y1_mw <- c((all_off_2_mw$N_cai_sps_12.1))
ncai_y2_mw <- c((all_off_2_mw$N_cai_sps_12.2))

wilcox.test(ncai_y2_mw, ncai_y1_mw, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_mw$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_mw$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# south
ncai_y1_so <- c((all_off_2_so$N_cai_sps_12.1))
ncai_y2_so <- c((all_off_2_so$N_cai_sps_12.2))

wilcox.test(ncai_y2_so, ncai_y1_so, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_so$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_so$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# west
ncai_y1_we <- c((all_off_2_we$N_cai_sps_12.1))
ncai_y2_we <- c((all_off_2_we$N_cai_sps_12.2))

wilcox.test(ncai_y2_we, ncai_y1_we, paired=TRUE, 
            alternative = "greater", exact=F)

round(quantile(all_off_2_we$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(all_off_2_we$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

amis14 <- subset(amis, year == 2014)
amis15 <- subset(amis, year == 2015)
amis16 <- subset(amis, year == 2016)
amis17 <- subset(amis, year == 2017)
amis18 <- subset(amis, year == 2018)
amis19 <- subset(amis, year == 2019)

### Investigate missingness for variables in CAI with UNK HIV stat partner -----

sum(!is.na(ai_14$cai_12_new))
sum(!is.na(ai_15$cai_12_new))
sum(!is.na(ai_16$cai_12_new))
sum(!is.na(ai_17$cai_12_new))
sum(!is.na(ai_18$cai_12_new))
sum(!is.na(ai_19$cai_12_new))

sum(!is.na(ai_14$cai_unk_12))
sum(!is.na(ai_15$cai_unk_12))
sum(!is.na(ai_16$cai_unk_12))
sum(!is.na(ai_17$cai_unk_12))
sum(!is.na(ai_18$cai_unk_12))
sum(!is.na(ai_19$cai_unk_12))




#### Investigate N CAI SPs missingness for Emory -----

## * O + A -----
amis14 <- subset(amis, year == 2014)
amis15 <- subset(amis, year == 2015)
amis16 <- subset(amis, year == 2016)
amis17 <- subset(amis, year == 2017)
amis18 <- subset(amis, year == 2018)
amis19 <- subset(amis, year == 2019)

OA14 <- subset(amis14, (m_mp12type == 3))
OA15 <- subset(amis15, (msmp12m_anal == 1 & msmp12m_oral == 1))
OA16 <- subset(amis16, (msmp12m_anal == 1 & msmp12m_oral == 1))
OA17 <- subset(amis17, (msmp12m_anal == 1 & msmp12m_oral == 1))
OA18 <- subset(amis18, (msmp12m_anal == 1 & msmp12m_oral == 1))
OA19 <- subset(amis19, (msmp12m_anal == 1 & msmp12m_oral == 1))

# 2014 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(OA14$m_mp12oanum))
sum(!is.na(OA14$m_mp12manum))
sum(!is.na(OA14$m_m1uas))
table(OA14$m_m1uas)
sum(!is.na(OA14$N_cai_sps_12))
sum(!is.na(OA14$m_m1uasnum2))

# 2015 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(OA15$m_mp12oanum))
sum(!is.na(OA15$m_mp12manum))
sum(!is.na(OA15$m_m1uas))
table(OA15$m_m1uas)
sum(!is.na(OA15$N_cai_sps_12))
sum(!is.na(OA15$m_m1uasnum2))

# 2016 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(OA16$m_mp12oanum))
sum(!is.na(OA16$m_mp12manum))
sum(!is.na(OA16$m_m1uas))
table(OA16$m_m1uas)
sum(!is.na(OA16$N_cai_sps_12))
sum(!is.na(OA16$m_m1uasnum2))

# 2017 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(OA17$m_mp12oanum))
sum(!is.na(OA17$m_mp12manum))
sum(!is.na(OA17$m_m1uas))
table(OA17$m_m1uas)
sum(!is.na(OA17$N_cai_sps_12))
sum(!is.na(OA17$m_m1uasnum2))

# 2018 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(OA18$m_mp12oanum))
sum(!is.na(OA18$m_mp12manum))
sum(!is.na(OA18$m_m1uas))
table(OA18$m_m1uas)
sum(!is.na(OA18$N_cai_sps_12))
sum(!is.na(OA18$m_m1uasnum2))

# 2019 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(OA19$m_mp12oanum))
sum(!is.na(OA19$m_mp12manum))
sum(!is.na(OA19$m_m1uas))
table(OA19$m_m1uas)
sum(!is.na(OA19$N_cai_sps_12))
sum(!is.na(OA19$m_m1uasnum2))

# A only ----

A_14 <- subset(amis14, (m_mp12type == 2))
A_15 <- subset(amis15, (msmp12m_anal == 1 & msmp12m_oral == 0))
A_16 <- subset(amis16, (msmp12m_anal == 1 & msmp12m_oral == 0))
A_17 <- subset(amis17, (msmp12m_anal == 1 & msmp12m_oral == 0))
A_18 <- subset(amis18, (msmp12m_anal == 1 & msmp12m_oral == 0))
A_19 <- subset(amis19, (msmp12m_anal == 1 & msmp12m_oral == 0))

# 2014 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(A_14$m_mp12anum))
sum(!is.na(A_14$m_m1uas))
table(A_14$m_m1uas)
sum(!is.na(A_14$N_cai_sps_12))
sum(!is.na(A_14$m_m1uasnum1))

# 2015 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(A_15$m_mp12anum))
sum(!is.na(A_15$m_m1uas))
table(A_15$m_m1uas)
sum(!is.na(A_15$N_cai_sps_12))
sum(!is.na(A_15$m_m1uasnum1))

# 2016 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(A_16$m_mp12anum))
sum(!is.na(A_16$m_m1uas))
table(A_16$m_m1uas)
sum(!is.na(A_16$N_cai_sps_12))
sum(!is.na(A_16$m_m1uasnum1))

# 2017 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(A_17$m_mp12anum))
sum(!is.na(A_17$m_m1uas))
table(A_17$m_m1uas)
sum(!is.na(A_17$N_cai_sps_12))
sum(!is.na(A_17$m_m1uasnum1))

# 2018 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(A_18$m_mp12anum))
sum(!is.na(A_18$m_m1uas))
table(A_18$m_m1uas)
sum(!is.na(A_18$N_cai_sps_12))
sum(!is.na(A_18$m_m1uasnum1))

# 2019 Ns for "universe of Rs who endorsed both oral and anal sex 
sum(!is.na(A_19$m_mp12anum))
sum(!is.na(A_19$m_m1uas))
table(A_19$m_m1uas)
sum(!is.na(A_19$N_cai_sps_12))
sum(!is.na(A_19$m_m1uasnum1))

### Table 1 -----
tableNA(amis$biomed_traj4, amis$year)

neg19 <- subset(amis, (year == "2019" & hiv2 == "02_neg"))

neg19$sf1_group <- NA
neg19$sf1_group[neg19$neg_prep12_use_groups == "01_never-or-no-PrEP12"] <- "01_HNNP"
neg19$sf1_group[neg19$neg_prep12_use_groups == "02_PrEPcur"] <- "02_PrEPcur"
neg19$sf1_group[neg19$biomed_traj3 == "03_Pos"] <- "03_Pos"
neg19$sf1_group[is.na(neg19$neg_prep12_use_groups) & neg19$biomed_traj3 == "01_NegNOP"] <- "04_NegUNK"

# round(prop.table(tableNA(amis19$cai_12_new))*100,1) # 14% NA
# 
# tableNA(amis19$msmp12m_anal) # if 0, then cai_12_new == 0
# summary(amis19$m_mp12anum) # number of any A.I. SPs, 12 
# summary(amis19$m_mp12oanum) # number or oral or anal SPs, 12
# summary(amis19$m_mp12manum) # number of A.I. SPs, 12
# summary(amis19$m_m1uasnum1)
# summary(amis19$m_m1uasnum2)
# tableNA(amis19$m_m1uas)
# 
# amis19$cai_12_new[amis19$msmp12m_anal == 0] <- 0
# 
# amis19$cai_12_new[amis19$m_m1uas == 1] <- 1
# amis19$cai_12_new[amis19$m_m1uas == 0] <- 0
# 
# round(prop.table(tableNA(amis19$cai_12_new))*100,1) #66 (<1%) NA

neg19$condom_cat <- NA
neg19$condom_cat[neg19$cai_12_new == 0] <- "1_100%"
neg19$condom_cat[neg19$cai_12_new == 1 & (neg19$N_cai_sps_12 == neg19$n_ai_sp)] <- "3_noSPs"
neg19$condom_cat[neg19$cai_12_new == 1 & (neg19$N_cai_sps_12 < neg19$n_ai_sp)] <- "2_some"



nrow(amis) # N = 65,795
tableNA(amis$year)
round(prop.table(tableNA(amis$year))*100,1)

# * "NegNOPs" only -----
amis_noprep <- subset(amis, neg_prep12_use_groups == "01_never-or-no-PrEP12") #N=53,865
amis_noprep_cai <- subset(amis_noprep, cai_12_new == 1) #N=32,396

# * * age -----
summary(amis_noprep$age)

# * * race -----
tableNA(amis_noprep$race_cat)
round(prop.table(tableNA(amis_noprep$race_cat))*100,1)

# * * region -----
tableNA(amis_noprep$region4)
round(prop.table(tableNA(amis_noprep$region4))*100,1)

# * * urbancity -----
tableNA(amis_noprep$nchs)
round(prop.table(tableNA(amis_noprep$nchs))*100,1)

# * * education -----
tableNA(amis_noprep$educ_cat)
round(prop.table(tableNA(amis_noprep$educ_cat))*100,1)

# * * income -----
tableNA(amis_noprep$income_cat)
round(prop.table(tableNA(amis_noprep$income_cat))*100,1)

# * * sexual identity ----
tableNA(amis_noprep$sex_id)
round(prop.table(tableNA(amis_noprep$sex_id))*100,1)

# * * CAI, p12 -----
tableNA(amis_noprep$cai_12_new)
round(prop.table(tableNA(amis_noprep$cai_12_new))*100,1)

# * * CAI, with HIV-pos -----
tableNA(amis_noprep_cai$cai_pos_12)
round(prop.table(tableNA(amis_noprep_cai$cai_pos_12))*100,1)

# * * CAI, with HIV-unk -----
tableNA(amis_noprep_cai$cai_unk_12)
round(prop.table(tableNA(amis_noprep_cai$cai_unk_12))*100,1)

# * * N CAI SPs -----
summary(amis_noprep_cai$N_cai_sps_12)

# * * * those with >1 CAI SP -----
amis_noprep_cai_2 <- subset(amis_noprep_cai, N_cai_sps_12 >= 2)
summary(amis_noprep_cai_2$N_cai_sps_12)

tableNA(amis_noprep_cai_2$cai_pos_12)
round(prop.table(tableNA(amis_noprep_cai_2$cai_pos_12))*100,1)

tableNA(amis_noprep_cai_2$cai_unk_12)
round(prop.table(tableNA(amis_noprep_cai_2$cai_unk_12))*100,1)



# * * Table 1 for those who stayed off of PrEP -----
### Table 1 -----
tableNA(all_off$neg_prep12_use_groups, all_off$year)

nrow(all_off) # N =
tableNA(all_off$year.1)
round(prop.table(tableNA(all_off$year.1))*100,1)

# * * age -----
summary(all_off$age.1)

# * * race -----
tableNA(all_off$race_cat.1)
round(prop.table(tableNA(all_off$race_cat.1))*100,1)

# * * region -----
tableNA(all_off$region4.1)
round(prop.table(tableNA(all_off$region4.1))*100,1)

# * * urbancity -----
tableNA(all_off$nchs4.1)
round(prop.table(tableNA(all_off$nchs4.1))*100,1)

# * * education -----
tableNA(all_off$educ3.1)
round(prop.table(tableNA(all_off$educ3.1))*100,1)

# * * income -----
tableNA(all_off$income_cat.1)
round(prop.table(tableNA(all_off$income_cat.1))*100,1)

# * * sexual identity ----
tableNA(all_off$sex_id.1)
round(prop.table(tableNA(all_off$sex_id.1))*100,1)

# * * CAI Y1 ----
table(all_off$cai_12_new.1)
round(prop.table(table(all_off$cai_12_new.1))*100,1)

# * * CAI Y2 ----
table(all_off$cai_12_new.2)
round(prop.table(table(all_off$cai_12_new.2))*100,1)

# * * * * July Issue 5 investigation -----
# * * * * * Item 1 -----
oa2cai <- subset(amis, (m_mp12oanum >= 2 & m_m1uas == 1))

tableNA(oa2cai$year)
tableNA(oa2cai$year, oa2cai$m_mp12manum)

# * * * * Item 2 -----
oa2cai_2 <- subset(amis, (m_mp12manum >= 2 & m_m1uas == 1))

tableNA(oa2cai_2$year)
tableNA(oa2cai_2$year, oa2cai_2$m_m1uasnum2)


# Investigate CAI with unk missingess -----
# * all years -----
caiNA <- subset(amis, (is.na(cai_unk_12)))
tableNA(caiNA$m_m1sx, caiNA$m_mp12oanum)
tableNA(caiNA$m_mm1hsk) #do you know one SP HIV stat? 0 == NO == unknown.
tableNA(caiNA$m_m1hst) # single SP HIV status! 3 == IND = unknown.

# * 2016 -----
cai16 <- subset(amis, (year == 2016 & is.na(cai_unk_12)))
tableNA(cai16$m_m1sx)

# Time Series for CAI partner status groups -----
# * NOP, All participants -----
tableNA(all_off$cai_sp_cat.1, all_off$cai_sp_cat.2)

# * NOP, Hispanic participants ----
tableNA(all_off_hisp$cai_sp_cat.1, all_off_hisp$cai_sp_cat.2)

# Missingness Investigation 07/16/2022 -----
OA14CAI_2 <- subset(OA14, (m_mp12oanum >= 2 & m_m1uas == 1))
OA15CAI_2 <- subset(OA15, (m_mp12oanum >= 2 & m_m1uas == 1))
OA16CAI_2 <- subset(OA16, (m_mp12oanum >= 2 & m_m1uas == 1))
OA17CAI_2 <- subset(OA17, (m_mp12oanum >= 2 & m_m1uas == 1))
OA18CAI_2 <- subset(OA18, (m_mp12oanum >= 2 & m_m1uas == 1))
OA19CAI_2 <- subset(OA19, (m_mp12oanum >= 2 & m_m1uas == 1))

nrow(OA14CAI_2)
tableNA(OA14CAI_2$m_mp12manum)
table(is.na(OA14CAI_2$m_m1uasnum2))

nrow(OA15CAI_2)
table(is.na(OA15CAI_2$m_mp12manum))
table(is.na(OA15CAI_2$m_m1uasnum2))

nrow(OA16CAI_2)
table(is.na(OA16CAI_2$m_mp12manum))
table(is.na(OA16CAI_2$m_m1uasnum2))

nrow(OA17CAI_2)
table(is.na(OA17CAI_2$m_mp12manum))
table(is.na(OA17CAI_2$m_m1uasnum2))

nrow(OA18CAI_2)
table(is.na(OA18CAI_2$m_mp12manum))
table(is.na(OA18CAI_2$m_m1uasnum2))

nrow(OA19CAI_2)
table(is.na(OA19CAI_2$m_mp12manum))
table(is.na(OA19CAI_2$m_m1uasnum2))

# 
# D1516 <- subset(amis1516_wide, (biomed_traj3.2015 == "02_NegYOP" & biomed_traj3.2016 == "02_NegYOP"))
# round(prop.table(table(D1516$uas.2015))*100,0)
# round(prop.table(table(D1516$uas.2016))*100,0)
# summary(D1516$N_cai_sps_12.2015, na.rm = T)
# summary(D1516$N_cai_sps_12.2016, na.rm = T)
# 
# E1516 <- subset(amis1516_wide, (biomed_traj3.2015 == "01_NegNOP" & biomed_traj3.2016 == "03_Pos"))
# round(prop.table(table(E1516$uas.2015))*100,0)
# round(prop.table(table(E1516$uas.2016))*100,0)
# summary(E1516$N_cai_sps_12.2015, na.rm = T)
# summary(E1516$N_cai_sps_12.2016, na.rm = T)
# 
# F1516 <- subset(amis1516_wide, (biomed_traj3.2015 == "02_NegYOP" & biomed_traj3.2016 == "03_Pos"))
# round(prop.table(table(F1516$uas.2015))*100,0)
# round(prop.table(table(F1516$uas.2016))*100,0)
# summary(F1516$N_cai_sps_12.2015, na.rm = T)
# summary(F1516$N_cai_sps_12.2016, na.rm = T)
# 
# 
# # 2016-2017 matched dataset -----
# 
# amis1617 <- amis1617[,c("year", "match_id_1617", "uas", "N_cai_sps_12", "biomed_traj3")]
# 
# amis1617 <- amis1617[order(amis1617$match_id_1617, amis1617$year),]
# 
# amis1617_wide <- reshape(amis1617,
#                          timevar = "year",
#                          idvar = "match_id_1617",
#                          v.names = c("uas", "N_cai_sps_12", "biomed_traj3"),
#                          direction = "wide")
# 
# tableNA(amis1617_wide$biomed_traj3.2016, amis1617_wide$biomed_traj3.2017)
# 
# amis1617_wide <- drop_na(amis1617_wide, biomed_traj3.2016)
# amis1617_wide <- drop_na(amis1617_wide, biomed_traj3.2017)
# 
# tableNA(amis1617_wide$biomed_traj3.2016, amis1617_wide$biomed_traj3.2017)
# 
# A1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "01_NegNOP" & biomed_traj3.2017 == "01_NegNOP"))
# round(prop.table(table(A1617$uas.2016))*100,0)
# round(prop.table(table(A1617$uas.2017))*100,0)
# summary(A1617$N_cai_sps_12.2016, na.rm = T)
# summary(A1617$N_cai_sps_12.2017, na.rm = T)
# 
# B1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "01_NegNOP" & biomed_traj3.2017 == "02_NegYOP"))
# round(prop.table(table(B1617$uas.2016))*100,0)
# round(prop.table(table(B1617$uas.2017))*100,0)
# summary(B1617$N_cai_sps_12.2016, na.rm = T)
# summary(B1617$N_cai_sps_12.2017, na.rm = T)
# 
# C1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "02_NegYOP" & biomed_traj3.2017 == "01_NegNOP"))
# round(prop.table(table(C1617$uas.2016))*100,0)
# round(prop.table(table(C1617$uas.2017))*100,0)
# summary(C1617$N_cai_sps_12.2016, na.rm = T)
# summary(C1617$N_cai_sps_12.2017, na.rm = T)
# 
# D1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "02_NegYOP" & biomed_traj3.2017 == "02_NegYOP"))
# round(prop.table(table(D1617$uas.2016))*100,0)
# round(prop.table(table(D1617$uas.2017))*100,0)
# summary(D1617$N_cai_sps_12.2016, na.rm = T)
# summary(D1617$N_cai_sps_12.2017, na.rm = T)
# 
# E1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "01_NegNOP" & biomed_traj3.2017 == "03_Pos"))
# round(prop.table(table(E1617$uas.2016))*100,0)
# round(prop.table(table(E1617$uas.2017))*100,0)
# summary(E1617$N_cai_sps_12.2016, na.rm = T)
# summary(E1617$N_cai_sps_12.2017, na.rm = T)
# 
# F1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "02_NegYOP" & biomed_traj3.2017 == "03_Pos"))
# round(prop.table(table(F1617$uas.2016))*100,0)
# round(prop.table(table(F1617$uas.2017))*100,0)
# summary(F1617$N_cai_sps_12.2016, na.rm = T)
# summary(F1617$N_cai_sps_12.2017, na.rm = T)
# 
# 
# # 2017-2018 matched dataset -----
# 
# amis1718 <- amis1718[,c("year", "match_id_1718", "uas", "N_cai_sps_12", "biomed_traj3")]
# 
# amis1718 <- amis1718[order(amis1718$match_id_1718, amis1718$year),]
# 
# amis1718_wide <- reshape(amis1718,
#                          timevar = "year",
#                          idvar = "match_id_1718",
#                          v.names = c("uas", "N_cai_sps_12", "biomed_traj3"),
#                          direction = "wide")
# 
# amis1718_wide <- drop_na(amis1718_wide, biomed_traj3.2017)
# amis1718_wide <- drop_na(amis1718_wide, biomed_traj3.2018)
# 
# tableNA(amis1718_wide$biomed_traj3.2017, amis1718_wide$biomed_traj3.2018)
# 
# A1718 <- subset(amis1718_wide, (biomed_traj3.2017 == "01_NegNOP" & biomed_traj3.2018 == "01_NegNOP"))
# round(prop.table(table(A1718$uas.2017))*100,0)
# round(prop.table(table(A1718$uas.2018))*100,0)
# summary(A1718$N_cai_sps_12.2017, na.rm = T)
# summary(A1718$N_cai_sps_12.2018, na.rm = T)
# 
# B1718 <- subset(amis1718_wide, (biomed_traj3.2017 == "01_NegNOP" & biomed_traj3.2018 == "02_NegYOP"))
# round(prop.table(table(B1718$uas.2017))*100,0)
# round(prop.table(table(B1718$uas.2018))*100,0)
# summary(B1718$N_cai_sps_12.2017, na.rm = T)
# summary(B1718$N_cai_sps_12.2018, na.rm = T)
# 
# C1718 <- subset(amis1718_wide, (biomed_traj3.2017 == "02_NegYOP" & biomed_traj3.2018 == "01_NegNOP"))
# round(prop.table(table(C1718$uas.2017))*100,0)
# round(prop.table(table(C1718$uas.2018))*100,0)
# summary(C1718$N_cai_sps_12.2017, na.rm = T)
# summary(C1718$N_cai_sps_12.2018, na.rm = T)
# 
# D1718 <- subset(amis1718_wide, (biomed_traj3.2017 == "02_NegYOP" & biomed_traj3.2018 == "02_NegYOP"))
# round(prop.table(table(D1718$uas.2017))*100,0)
# round(prop.table(table(D1718$uas.2018))*100,0)
# summary(D1718$N_cai_sps_12.2017, na.rm = T)
# summary(D1718$N_cai_sps_12.2018, na.rm = T)
# 
# # # 2014-2015 matched dataset -----
# 
# amis1415 <- amis1415[,c("year", "match_id_1415", "uas", "N_cai_sps_12", "biomed_traj3")]
# 
# amis1415 <- amis1415[order(amis1415$match_id_1415, amis1415$year),]
# 
# amis1415_wide <- reshape(amis1415,
#                          timevar = "year",
#                          idvar = "match_id_1415",
#                          v.names = c("uas", "N_cai_sps_12", "biomed_traj3"),
#                          direction = "wide")
# 
# amis1415_wide <- drop_na(amis1415_wide, biomed_traj3.2014)
# amis1415_wide <- drop_na(amis1415_wide, biomed_traj3.2015)
# 
# tableNA(amis1415_wide$biomed_traj3.2014, amis1415_wide$biomed_traj3.2015)
# 
# A1415 <- subset(amis1415_wide, (biomed_traj3.2014 == "01_NegNOP" & biomed_traj3.2015 == "01_NegNOP"))
# round(prop.table(table(A1415$uas.2014))*100,1)
# round(prop.table(table(A1415$uas.2015))*100,1)
# summary(A1415$N_cai_sps_12.2014, na.rm = T)
# summary(A1415$N_cai_sps_12.2015, na.rm = T)
# 
# B1415 <- subset(amis1415_wide, (biomed_traj3.2014 == "01_NegNOP" & biomed_traj3.2015 == "02_NegYOP"))
# round(prop.table(table(B1415$uas.2014))*100,1)
# round(prop.table(table(B1415$uas.2015))*100,1)
# summary(B1415$N_cai_sps_12.2014, na.rm = T)
# summary(B1415$N_cai_sps_12.2015, na.rm = T)
# 
# C1415 <- subset(amis1415_wide, (biomed_traj3.2014 == "02_NegYOP" & biomed_traj3.2015 == "01_NegNOP"))
# round(prop.table(table(C1415$uas.2014))*100,1)
# round(prop.table(table(C1415$uas.2015))*100,1)
# summary(C1415$N_cai_sps_12.2014, na.rm = T)
# summary(C1415$N_cai_sps_12.2015, na.rm = T)
# 
# D1415 <- subset(amis1415_wide, (biomed_traj3.2014 == "02_NegYOP" & biomed_traj3.2015 == "02_NegYOP"))
# round(prop.table(table(D1415$uas.2014))*100,1)
# round(prop.table(table(D1415$uas.2015))*100,1)
# summary(D1415$N_cai_sps_12.2014, na.rm = T)
# summary(D1415$N_cai_sps_12.2015, na.rm = T)
# 
# # 2015-2016 matched dataset -----
# 
# amis1516 <- amis1516[,c("year", "match_id_1516", "uas", "N_cai_sps_12", "biomed_traj3")]
# 
# amis1516 <- amis1516[order(amis1516$match_id_1516, amis1516$year),]
# 
# amis1516_wide <- reshape(amis1516,
#                          timevar = "year",
#                          idvar = "match_id_1516",
#                          v.names = c("uas", "N_cai_sps_12", "biomed_traj3"),
#                          direction = "wide")
# 
# amis1516_wide <- drop_na(amis1516_wide, biomed_traj3.2015)
# amis1516_wide <- drop_na(amis1516_wide, biomed_traj3.2016)
# 
# tableNA(amis1516_wide$biomed_traj3.2015, amis1516_wide$biomed_traj3.2016)
# 
# A1516 <- subset(amis1516_wide, (biomed_traj3.2015 == "01_NegNOP" & biomed_traj3.2016 == "01_NegNOP"))
# round(prop.table(table(A1516$uas.2015))*100,0)
# round(prop.table(table(A1516$uas.2016))*100,0)
# summary(A1516$N_cai_sps_12.2015, na.rm = T)
# summary(A1516$N_cai_sps_12.2016, na.rm = T)
# 
# B1516 <- subset(amis1516_wide, (biomed_traj3.2015 == "01_NegNOP" & biomed_traj3.2016 == "02_NegYOP"))
# round(prop.table(table(B1516$uas.2015))*100,0)
# round(prop.table(table(B1516$uas.2016))*100,0)
# summary(B1516$N_cai_sps_12.2015, na.rm = T)
# summary(B1516$N_cai_sps_12.2016, na.rm = T)
# 
# C1516 <- subset(amis1516_wide, (biomed_traj3.2015 == "02_NegYOP" & biomed_traj3.2016 == "01_NegNOP"))
# round(prop.table(table(C1516$uas.2015))*100,0)
# round(prop.table(table(C1516$uas.2016))*100,0)
# summary(C1516$N_cai_sps_12.2015, na.rm = T)
# summary(C1516$N_cai_sps_12.2016, na.rm = T)
# 
# D1516 <- subset(amis1516_wide, (biomed_traj3.2015 == "02_NegYOP" & biomed_traj3.2016 == "02_NegYOP"))
# round(prop.table(table(D1516$uas.2015))*100,0)
# round(prop.table(table(D1516$uas.2016))*100,0)
# summary(D1516$N_cai_sps_12.2015, na.rm = T)
# summary(D1516$N_cai_sps_12.2016, na.rm = T)
# 
# E1516 <- subset(amis1516_wide, (biomed_traj3.2015 == "01_NegNOP" & biomed_traj3.2016 == "03_Pos"))
# round(prop.table(table(E1516$uas.2015))*100,0)
# round(prop.table(table(E1516$uas.2016))*100,0)
# summary(E1516$N_cai_sps_12.2015, na.rm = T)
# summary(E1516$N_cai_sps_12.2016, na.rm = T)
# 
# F1516 <- subset(amis1516_wide, (biomed_traj3.2015 == "02_NegYOP" & biomed_traj3.2016 == "03_Pos"))
# round(prop.table(table(F1516$uas.2015))*100,0)
# round(prop.table(table(F1516$uas.2016))*100,0)
# summary(F1516$N_cai_sps_12.2015, na.rm = T)
# summary(F1516$N_cai_sps_12.2016, na.rm = T)
# 
# 
# # 2016-2017 matched dataset -----
# 
# amis1617 <- amis1617[,c("year", "match_id_1617", "uas", "N_cai_sps_12", "biomed_traj3")]
# 
# amis1617 <- amis1617[order(amis1617$match_id_1617, amis1617$year),]
# 
# amis1617_wide <- reshape(amis1617,
#                          timevar = "year",
#                          idvar = "match_id_1617",
#                          v.names = c("uas", "N_cai_sps_12", "biomed_traj3"),
#                          direction = "wide")
# 
# tableNA(amis1617_wide$biomed_traj3.2016, amis1617_wide$biomed_traj3.2017)
# 
# amis1617_wide <- drop_na(amis1617_wide, biomed_traj3.2016)
# amis1617_wide <- drop_na(amis1617_wide, biomed_traj3.2017)
# 
# tableNA(amis1617_wide$biomed_traj3.2016, amis1617_wide$biomed_traj3.2017)
# 
# A1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "01_NegNOP" & biomed_traj3.2017 == "01_NegNOP"))
# round(prop.table(table(A1617$uas.2016))*100,0)
# round(prop.table(table(A1617$uas.2017))*100,0)
# summary(A1617$N_cai_sps_12.2016, na.rm = T)
# summary(A1617$N_cai_sps_12.2017, na.rm = T)
# 
# B1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "01_NegNOP" & biomed_traj3.2017 == "02_NegYOP"))
# round(prop.table(table(B1617$uas.2016))*100,0)
# round(prop.table(table(B1617$uas.2017))*100,0)
# summary(B1617$N_cai_sps_12.2016, na.rm = T)
# summary(B1617$N_cai_sps_12.2017, na.rm = T)
# 
# C1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "02_NegYOP" & biomed_traj3.2017 == "01_NegNOP"))
# round(prop.table(table(C1617$uas.2016))*100,0)
# round(prop.table(table(C1617$uas.2017))*100,0)
# summary(C1617$N_cai_sps_12.2016, na.rm = T)
# summary(C1617$N_cai_sps_12.2017, na.rm = T)
# 
# D1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "02_NegYOP" & biomed_traj3.2017 == "02_NegYOP"))
# round(prop.table(table(D1617$uas.2016))*100,0)
# round(prop.table(table(D1617$uas.2017))*100,0)
# summary(D1617$N_cai_sps_12.2016, na.rm = T)
# summary(D1617$N_cai_sps_12.2017, na.rm = T)
# 
# E1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "01_NegNOP" & biomed_traj3.2017 == "03_Pos"))
# round(prop.table(table(E1617$uas.2016))*100,0)
# round(prop.table(table(E1617$uas.2017))*100,0)
# summary(E1617$N_cai_sps_12.2016, na.rm = T)
# summary(E1617$N_cai_sps_12.2017, na.rm = T)
# 
# F1617 <- subset(amis1617_wide, (biomed_traj3.2016 == "02_NegYOP" & biomed_traj3.2017 == "03_Pos"))
# round(prop.table(table(F1617$uas.2016))*100,0)
# round(prop.table(table(F1617$uas.2017))*100,0)
# summary(F1617$N_cai_sps_12.2016, na.rm = T)
# summary(F1617$N_cai_sps_12.2017, na.rm = T)
# 
# 
# # 2017-2018 matched dataset -----
# 
# amis1718 <- amis1718[,c("year", "match_id_1718", "uas", "N_cai_sps_12", "biomed_traj3")]
# 
# amis1718 <- amis1718[order(amis1718$match_id_1718, amis1718$year),]
# 
# amis1718_wide <- reshape(amis1718,
#                          timevar = "year",
#                          idvar = "match_id_1718",
#                          v.names = c("uas", "N_cai_sps_12", "biomed_traj3"),
#                          direction = "wide")
# 
# amis1718_wide <- drop_na(amis1718_wide, biomed_traj3.2017)
# amis1718_wide <- drop_na(amis1718_wide, biomed_traj3.2018)
# 
# tableNA(amis1718_wide$biomed_traj3.2017, amis1718_wide$biomed_traj3.2018)
# 
# A1718 <- subset(amis1718_wide, (biomed_traj3.2017 == "01_NegNOP" & biomed_traj3.2018 == "01_NegNOP"))
# round(prop.table(table(A1718$uas.2017))*100,0)
# round(prop.table(table(A1718$uas.2018))*100,0)
# summary(A1718$N_cai_sps_12.2017, na.rm = T)
# summary(A1718$N_cai_sps_12.2018, na.rm = T)
# 
# B1718 <- subset(amis1718_wide, (biomed_traj3.2017 == "01_NegNOP" & biomed_traj3.2018 == "02_NegYOP"))
# round(prop.table(table(B1718$uas.2017))*100,0)
# round(prop.table(table(B1718$uas.2018))*100,0)
# summary(B1718$N_cai_sps_12.2017, na.rm = T)
# summary(B1718$N_cai_sps_12.2018, na.rm = T)
# 
# C1718 <- subset(amis1718_wide, (biomed_traj3.2017 == "02_NegYOP" & biomed_traj3.2018 == "01_NegNOP"))
# round(prop.table(table(C1718$uas.2017))*100,0)
# round(prop.table(table(C1718$uas.2018))*100,0)
# summary(C1718$N_cai_sps_12.2017, na.rm = T)
# summary(C1718$N_cai_sps_12.2018, na.rm = T)
# 
# D1718 <- subset(amis1718_wide, (biomed_traj3.2017 == "02_NegYOP" & biomed_traj3.2018 == "02_NegYOP"))
# round(prop.table(table(D1718$uas.2017))*100,0)
# round(prop.table(table(D1718$uas.2018))*100,0)
# summary(D1718$N_cai_sps_12.2017, na.rm = T)
# summary(D1718$N_cai_sps_12.2018, na.rm = T)
# 
# E1718 <- subset(amis1718_wide, (biomed_traj3.2017 == "01_NegNOP" & biomed_traj3.2018 == "03_Pos"))
# round(prop.table(table(E1718$uas.2017))*100,0)
# round(prop.table(table(E1718$uas.2018))*100,0)
# summary(E1718$N_cai_sps_12.2017, na.rm = T)
# summary(E1718$N_cai_sps_12.2018, na.rm = T)
# 
# F1718 <- subset(amis1718_wide, (biomed_traj3.2017 == "02_NegYOP" & biomed_traj3.2018 == "03_Pos"))
# round(prop.table(table(F1718$uas.2017))*100,0)
# round(prop.table(table(F1718$uas.2018))*100,0)
# summary(F1718$N_cai_sps_12.2017, na.rm = T)
# summary(F1718$N_cai_sps_12.2018, na.rm = T)
# 
# 
# # 2018-2019 matched dataset -----
# 
# amis1819 <- amis1819[,c("year", "match_id_1819", "uas", "N_cai_sps_12", "biomed_traj3")]
# 
# amis1819 <- amis1819[order(amis1819$match_id_1819, amis1819$year),]
# 
# amis1819_wide <- reshape(amis1819,
#                          timevar = "year",
#                          idvar = "match_id_1819",
#                          v.names = c("uas", "N_cai_sps_12", "biomed_traj3"),
#                          direction = "wide")
# 
# amis1819_wide <- drop_na(amis1819_wide, biomed_traj3.2018)
# amis1819_wide <- drop_na(amis1819_wide, biomed_traj3.2019)
# 
# tableNA(amis1819_wide$biomed_traj3.2018, amis1819_wide$biomed_traj3.2019)
# 
# A1819 <- subset(amis1819_wide, (biomed_traj3.2018 == "01_NegNOP" & biomed_traj3.2019 == "01_NegNOP"))
# round(prop.table(table(A1819$uas.2018))*100,0)
# round(prop.table(table(A1819$uas.2019))*100,0)
# summary(A1819$N_cai_sps_12.2018, na.rm = T)
# summary(A1819$N_cai_sps_12.2019, na.rm = T)
# 
# B1819 <- subset(amis1819_wide, (biomed_traj3.2018 == "01_NegNOP" & biomed_traj3.2019 == "02_NegYOP"))
# round(prop.table(table(B1819$uas.2018))*100,0)
# round(prop.table(table(B1819$uas.2019))*100,0)
# summary(B1819$N_cai_sps_12.2018, na.rm = T)
# summary(B1819$N_cai_sps_12.2019, na.rm = T)
# 
# C1819 <- subset(amis1819_wide, (biomed_traj3.2018 == "02_NegYOP" & biomed_traj3.2019 == "01_NegNOP"))
# round(prop.table(table(C1819$uas.2018))*100,0)
# round(prop.table(table(C1819$uas.2019))*100,0)
# summary(C1819$N_cai_sps_12.2018, na.rm = T)
# summary(C1819$N_cai_sps_12.2019, na.rm = T)
# 
# D1819 <- subset(amis1819_wide, (biomed_traj3.2018 == "02_NegYOP" & biomed_traj3.2019 == "02_NegYOP"))
# round(prop.table(table(D1819$uas.2018))*100,0)
# round(prop.table(table(D1819$uas.2019))*100,0)
# summary(D1819$N_cai_sps_12.2018, na.rm = T)
# summary(D1819$N_cai_sps_12.2019, na.rm = T)
# 
# E1819 <- subset(amis1819_wide, (biomed_traj3.2018 == "01_NegNOP" & biomed_traj3.2019 == "03_Pos"))
# round(prop.table(table(E1819$uas.2018))*100,0)
# round(prop.table(table(E1819$uas.2019))*100,0)
# summary(E1819$N_cai_sps_12.2018, na.rm = T)
# summary(E1819$N_cai_sps_12.2019, na.rm = T)
# 
# F1819 <- subset(amis1819_wide, (biomed_traj3.2018 == "02_NegYOP" & biomed_traj3.2019 == "03_Pos"))
# round(prop.table(table(F1819$uas.2018))*100,0)
# round(prop.table(table(F1819$uas.2019))*100,0)
# summary(F1819$N_cai_sps_12.2018, na.rm = T)
# summary(F1819$N_cai_sps_12.2019, na.rm = T)
# 
# #######################################################################
# #######################################################################
# #######################################################################
# 
# # Wide dataset 14-15 N = 115. ---
# 
# amis1516$id2 <- ifelse(amis1516$year == 2015, amis1516$respondent_id, NA)
# amis1516$id2 <- ifelse(amis1516$year == 2016, amis1516$lastid2015, NA)
# 
# amis1516 <- amis1516[order(amis1516$id2, amis1516$year),]
# 
# amis1516_wide <- reshape(amis1516,
#                          timevar =  "year",
#                          idvar = "id2",
#                          direction = "wide")
# # Wide dataset 15-16 N = 819 ** multiple rows match for year = 2016?
# 
# amis1617$id2 <- ifelse(amis1617$year == 2016, amis1617$respondent_id, NA)
# amis1617$id2 <- ifelse(amis1617$year == 2017, amis1617$lastid2016, NA)
# 
# amis1617 <- amis1617[order(amis1617$id2, amis1617$year),]
# 
# amis1617_wide <- reshape(amis1617,
#                          timevar =  "year",
#                          idvar = "id2",
#                          direction = "wide")
# # Wide dataset 16-17 N = 1078 ** multiple rows match for year = 2017?
# 
# amis1718$id2 <- ifelse(amis1718$year == 2017, amis1718$respondent_id, NA)
# amis1718$id2 <- ifelse(amis1718$year == 2018, amis1718$lastid2017, NA)
# 
# amis1718 <- amis1718[order(amis1718$id2, amis1718$year),]
# 
# amis1718_wide <- reshape(amis1718,
#                          timevar =  "year",
#                          idvar = "id2",
#                          direction = "wide")
# # Wide dataset 17-18 N = 926 ** multiple rows match for year = 2018?
# 
# amis1819$id2 <- ifelse(amis1819$year == 2018, amis1819$respondent_id, NA)
# amis1819$id2 <- ifelse(amis1819$year == 2019, amis1819$lastid2018, NA)
# 
# amis1819 <- amis1819[order(amis1819$id2, amis1819$year),]
# 
# amis1819_wide <- reshape(amis1819,
#                          timevar =  "year",
#                          idvar = "id2",
#                          direction = "wide")
# # Wide dataset 18-19 N = 925 ** multiple rows match for year = 2019?
# 
# ## How many to expect in each pair:
# # amis1415 <- amis1415[!is.na(amis1415$lastid2014),] #N = 114
# # amis1516 <- amis1516[!is.na(amis1516$lastid2015),] #N = 821
# # amis1617 <- amis1617[!is.na(amis1617$lastid2016),] #N = 1090
# # amis1718 <- amis1718[!is.na(amis1718$lastid2017),] #N = 932
# # amis1819 <- amis1819[!is.na(amis1819$lastid2018),] #N = 929
# 
# tableNA(amis$lastid2014, amis$year)
# tableNA(amis$lastid2015, amis$year)
# tableNA(amis$lastid2016, amis$year)
# tableNA(amis$lastid2017, amis$year) ## Violation of uniqueness
# tableNA(amis$lastid2018, amis$year)
# 
# table(table(amis$lastid2014))
# table(table(amis$lastid2015))
# table(table(amis$lastid2016))
# table(table(amis$lastid2017))
# table(table(amis$lastid2018))

# # * hiv_stat7 -----
# amis$hiv_stat7 <- NA
# 
# amis$hiv_stat7[amis$evertest == 0] <- "01_NvrTest"
# amis$hiv_stat7[(amis$evertest == 1 | is.na(amis$evertest)) & amis$hiv2 == "02_neg" & amis$prep_used_ever == 0] <- "02_NvrPrEP"
# amis$hiv_stat7[(amis$evertest == 1 | is.na(amis$evertest)) & amis$hiv2 == "02_neg" & amis$prep_used12 == 0 & amis$prep_ever == 1] <- "03_PrEP>12"
# amis$hiv_stat7[(amis$evertest == 1 | is.na(amis$evertest)) & amis$hiv2 == "02_neg" & amis$prep_used12 == 1 & (amis$prep_current == 0 | is.na(amis$prep_current))] <- "04_PrEP<12"
# amis$hiv_stat7[(amis$evertest == 1 | is.na(amis$evertest)) & amis$hiv2 == "02_neg" & amis$prep_current == 1] <- "05_PrEPcur"
# amis$hiv_stat7[(amis$evertest == 1 | is.na(amis$evertest)) & amis$hiv2 == "01_pos" & amis$art_current == 1] <- "06_PosOnART"
# amis$hiv_stat7[(amis$evertest == 1 | is.na(amis$evertest)) & amis$hiv2 == "01_pos" & amis$art_current == 0] <- "07_PosNoART"
# 
# round(prop.table(tableNA(amis$hiv_stat7))*100,2) #31% NA ~ why?
# round(prop.table(tableNA(amis$hiv_stat7, amis$year), margin = 2)*100,2) # >40% NA in 2014:2017
# 
# # examine NAs by variable
# hiv7NA <- subset(amis, is.na(hiv_stat7))
# 
# tableNA(hiv7NA$year)
# tableNA(hiv7NA$hiv2)
# tableNA(hiv7NA$evertest) #all NAs are among those with HIV test ever
# tableNA(hiv7NA$prep_used_ever) #N=461 have "no" for `prep_used_ever`; all others have NA
# tableNA(hiv7NA$prep_used12) #0 are NA for this variable ~ 1 is "yes"?
# tableNA(hiv7NA$prep_current)  # 1 is NA, all others are "no"
# tableNA(hiv7NA$art_current) # all NA
# 
# # `evertest`
# tableNA(amis$hiv_stat7, amis$evertest) #checks out
# 
# # `prep_aware`
# tableNA(amis$hiv_stat7, amis$prep_aware) # 461 no to `prep_aware` have NA for hiv_stat7
# ###
# weird_0 <- subset(amis, (is.na(hiv_stat7) & prep_aware == 0))
# tableNA(weird_0$year) #concentrated in 2015, 2016
# tableNA(weird_0$prep_used_ever) #checks out
# tableNA(weird_0$prep_used12) #checks out
# tableNA(weird_0$prep_current) #checks out
# 
# # `prep_used12`
# tableNA(amis$hiv_stat7, amis$prep_used12) # 1 said "yes" to used p12 but NA?
# ###
# weird_1 <- subset(amis, (is.na(amis$hiv_stat7) & amis$prep_used12 == 1))
# tableNA(weird_1$hiv3) # that observation is HIV+.
# tableNA(weird_1$art_current) # they have NA for current ART use.
# ###
# 
# # `prep_used_ever`
# tableNA(amis$hiv_stat7, amis$prep_used_ever) #LOTS of missingness for this variable.
# tableNA(amis$prep_used_ever, amis$year) 
# # no one in 2013, 2014 have --> should be 0s?
# # no one in 2017? seems weird.
# 
# # `prep_current`
# tableNA(amis$hiv_stat7, amis$prep_current) # 
# 
# 
# # * hiv_stat3 (old) ----
# amis$hiv_stat_group3[amis$tested_ever == 0 |
#                         (amis$hiv_status == "02_neg" & amis$prep_used_ever == 0) |
#                         (amis$hiv_status == "02_neg" & amis$prep_used12 == 0 & amis$prep_used_ever == 1) |
#                         (amis$hiv_status == "02_neg" & amis$prep_used12 == 1)
#                      ] <- "01_NegNOP"
# 
# amis$hiv_stat_group3[amis$hiv_status == "02_neg" & amis$prep_current == 1] <- "02_NegOP"
# 
# amis$hiv_stat_group3[(amis$hiv_status == "01_pos" & amis$art_current == 1) |
#                         (amis$hiv_status == "01_pos" & amis$art_current == 0)] <- "03_Pos"
# 
# table(amis$hiv_stat_group3, amis$hiv_stat_group7, useNA = "ifany")

### DIAGNOSTICS ------
table(amis$prep_current, amis$tested_ever, useNA = "ifany")

plot(round(prop.table(table(amis$age, amis$tested_ever, useNA = "ifany"), 1)*100,1)[,2])

table(amis$hiv_status, useNA= "ifany")



# NOTE: are we interested in vars for LSP race (1) unk (2)? -----

# HIV-positive, HIV-negative participants (remove those of unknown status?) "all"

# O.P./N.O.P. "neg"

# N.O.P. "nop"


### Descriptive Analysis -----
### DESCRIPTIVE ANALYSES -----
### (1) Stacked barplot of 3-level HIV status/PrEP group -----
# Note: NAs are concentrated in 2016-2018. Other years look OK!; weird that prop of Never Test gets higher? Will look at age.
ggplot(data=subset(amis, !is.na(biomed_traj3)), aes(x=year, fill=biomed_traj3)) +
   geom_bar(position = "fill") +
   xlab("Year") +
   ylab("Proportion") +
   ggtitle("Figure X")

### (2) Stacked barplot of 7-level HIV status/PrEP group -----
ggplot(data=subset(amis), aes(x=year, fill=biomed_traj7)) +
   geom_bar(position = "fill") +
   xlab("Year") +
   ylab("Proportion") +
   ggtitle("Figure X")

### (3) Stacked barplot of 5-level HIV status/PrEP group -----
ggplot(data=subset(amis), aes(x=year, fill=biomed_traj5)) +
   geom_bar() +
   xlab("Year") +
   ylab("Proportion") +
   ggtitle("Figure X")

### (3) Stacked barplot of `biomed_current` groups -----
#note: NAs are in earlier years when PrEP current use wasn't assessed.
ggplot(data=subset(amis), aes(x=year, fill=biomed_current)) +
   geom_bar(position = "fill") +
   xlab("Year") +
   ylab("Proportion") +
   ggtitle("Figure X")

### What's up with increase in Never Tested? -----
amis %>% group_by(year) %>% summarise(mean_age = mean(age)) #mean age goes down
amis %>% group_by(year) %>% summarise(med_age = median(age)) #as does median


# 1: ARV use among "all" by HIV status 

# 2: Trends in PrEP use among "neg" (never, yes >12 mo, yes <=12 mo, current)

# 3: Among (a) HIV-positive, (b) neg. OPs, (c) neg. NOPs, trends in C.A.I. report
# adjust for age, race/eth, region (interaction between race/eth*region)

# INFERENTIAL ANALYSES -----
# Prepare dataset: restrict to those who....
# (1) remain off PrEP, 
# (2) no CAI year 1, 
# and compare based on an "initiated CAI y2."
logit_data <- subset(all_off, cai_12_new.1 == 0)
tableNA(all_off$cai_12_new.1) #only 18 missing for Y1.
logit_data$cai_init <- NA
logit_data$cai_init[logit_data$cai_12_new.2 == 1] <- 1
logit_data$cai_init[logit_data$cai_12_new.2 == 0] <- 0
#outcome variable = `cai_init`
logit_data <- subset(logit_data, !is.na(cai_init))

# Logistic Regression Analyses for CAS initiation -----
# * * bivariate OR estimates
# * * * N, % of total who initiated CAI
tableNA(logit_data$cai_init)
round(prop.table(tableNA(logit_data$cai_init))*100,0)

# * * * year (linear) ----
year_mod <- glm(cai_init ~ year.1, data = logit_data, family=binomial(link="logit"))

tableNA(logit_data$year.1)
round(prop.table(tableNA(logit_data$year.1))*100,0)

round(exp(coef(year_mod)),2)[2]
round(exp(confint(year_mod)),2)[2,]


# * * * age (2-level) -----
age_mod <- glm(cai_init ~ relevel(factor(age_25.1), ref = "1"), data = logit_data, family=binomial(link="logit"))

tableNA(logit_data$age_25.1)
round(prop.table(tableNA(logit_data$age_25.1))*100,0)

tableNA(logit_data$age_25.1, logit_data$cai_init)
round(prop.table(tableNA(logit_data$age_25.1, logit_data$cai_init), margin = 1)*100,0)

round(exp(coef(age_mod)),2)
round(exp(confint(age_mod)),2)
summary(age_mod)

# * * * race (4-cats) -----
race_mod <- glm(cai_init ~ relevel(factor(race_cat.1), ref = "NH_White"), data = logit_data, family=binomial(link="logit"))

tableNA(logit_data$race_cat.1)
round(prop.table(tableNA(logit_data$race_cat.1))*100,0)

tableNA(logit_data$race_cat.1, logit_data$cai_init)
round(prop.table(tableNA(logit_data$race_cat.1, logit_data$cai_init), margin = 1)*100,0)

round(exp(coef(race_mod)),2)
round(exp(confint(race_mod)),2)
summary(race_mod)

# * * * income (4-cats) -----
inc_mod <- glm(cai_init ~ relevel(factor(income_cat.1), ref = "4_75k+"), data = logit_data, family=binomial(link="logit"))

tableNA(logit_data$income_cat.1)
round(prop.table(tableNA(logit_data$income_cat.1))*100,0)

tableNA(logit_data$income_cat.1, logit_data$cai_init)
round(prop.table(tableNA(logit_data$income_cat.1, logit_data$cai_init), margin = 1)*100,0)

round(exp(coef(inc_mod)),2)
round(exp(confint(inc_mod)),2)
summary(inc_mod)

# * * * education (3-level) -----
educ_mod <- glm(cai_init ~ relevel(factor(educ3.1), ref = "3_Col+"), data = logit_data, family=binomial(link="logit"))

tableNA(logit_data$educ3.1)
round(prop.table(tableNA(logit_data$educ3.1))*100,0)

tableNA(logit_data$educ3.1, logit_data$cai_init)
round(prop.table(tableNA(logit_data$educ3.1, logit_data$cai_init), margin = 1)*100,0)

round(exp(coef(educ_mod)),2)
round(exp(confint(educ_mod)),2)
summary(educ_mod)

# * * * region (4-level) -----
reg_mod <- glm(cai_init ~ relevel(factor(region4.1), ref = "3_South"), data = logit_data, family=binomial(link="logit"))

tableNA(logit_data$region4.1)
round(prop.table(tableNA(logit_data$region4.1))*100,0)

tableNA(logit_data$region4.1, logit_data$cai_init)
round(prop.table(tableNA(logit_data$region4.1, logit_data$cai_init), margin = 1)*100,0)

round(exp(coef(reg_mod)),2)
round(exp(confint(reg_mod)),2)
summary(reg_mod)

# * * * urban-rural (4-level) -----
dens4_mod <- glm(cai_init ~ nchs4.1, data = logit_data, family=binomial(link="logit"))

tableNA(logit_data$nchs4.1)
round(prop.table(tableNA(logit_data$nchs4.1))*100,0)

tableNA(logit_data$nchs4.1, logit_data$cai_init)
round(prop.table(tableNA(logit_data$nchs4.1, logit_data$cai_init), margin = 1)*100,0)

round(exp(coef(dens4_mod)),2)
round(exp(confint(dens4_mod)),2)
summary(dens4_mod)

# * * multivar with age + race only ----
multi1_mod <- glm(cai_init ~ 
                   relevel(factor(age_25.1), ref = "1") +
                  relevel(factor(race_cat.1), ref = "NH_White"),
                 data = logit_data, 
                 family=binomial(link="logit")
)

summary(multi1_mod)

# * * multivar with age + race + intxn only ----
multi2_mod <- glm(cai_init ~ 
                     relevel(factor(age_25.1), ref = "1")*relevel(factor(race_cat.1), ref = "NH_White"),
                  data = logit_data, 
                  family=binomial(link="logit")
)

summary(multi2_mod)


# * * multivariate OR model with signif items with <5% NA -----
multi_mod <- glm(cai_init ~ 
                   relevel(factor(age_25.1), ref = "1") +
                   relevel(factor(educ3.1), ref = "3_Col+") +
                    relevel(factor(nchs4.1), ref = "1_LgCnt"),
                data = logit_data, 
                family=binomial(link="logit")
)

summary(multi_mod)

round(exp(coef(multi_mod)),2)
round(exp(confint(multi_mod)),2)

# * * multivar OR mod with signif items, <5% NA + race, no intxn -----
multi_mod_wrace <- glm(cai_init ~ 
                    relevel(factor(age_25.1), ref = "1") +
                       relevel(factor(race_cat.1), ref = "NH_White") +
                    relevel(factor(educ3.1), ref = "3_Col+") +
                    relevel(factor(nchs4.1), ref = "1_LgCnt"),
                 data = logit_data, 
                 family=binomial(link="logit")
)

summary(multi_mod_wrace)

round(exp(coef(multi_mod_wrace)),2)
round(exp(confint(multi_mod_wrace)),2)

# * * multivar OR mod with signif items, <5% NA + race, with age*race intxn -----
logit_data$race_cat.1 <- as.factor(logit_data$race_cat.1)
logit_data$race_cat.1 <- relevel(logit_data$race_cat.1, ref = "NH_White")

logit_data$age_25.1 <- as.factor(logit_data$age_25.1)
logit_data$age_25.1 <- relevel(logit_data$age_25.1, ref = "1")
   
# added in Census region 2022.09.22 
multi_mod_wrace_intx <- glm(cai_init ~ 
                          race_age_cat.1 +
                          relevel(factor(educ3.1), ref = "3_Col+") + relevel(factor(region4.1), ref = "3_South") +
                          relevel(factor(nchs4.1), ref = "1_LgCnt"),
                       data = logit_data, 
                       family=binomial(link="logit"))

summary(multi_mod_wrace_intx)

cbind(round(exp(coef(multi_mod_wrace_intx)),2),
round(exp(confint(multi_mod_wrace_intx)),2))
summary(multi_mod_wrace_intx)



# interaction race/age ORs & 95% CIs 
# 
# OR_bl_age <- exp(0.56070 -0.04035 -0.52260)
# se_bl_age <- deltamethod(g=~exp(x1 + x2 + x3),
#                          mean = coef(multi_mod_wrace_intx)[c("age_25.10", "race_cat.1NH_Black","age_25.10:race_cat.1NH_Black")],
#                          cov = vcov(multi_mod_wrace_intx)[c("age_25.10", "race_cat.1NH_Black","age_25.10:race_cat.1NH_Black"),
#                                                           c("age_25.10", "race_cat.1NH_Black","age_25.10:race_cat.1NH_Black")])
# 
# round(cbind(OR_bl_age, OR_bl_age-1.96*se_bl_age, OR_bl_age+1.96*se_bl_age),2)
# 
# 
# OR_h_age <- exp(0.56070 + 0.08628 + 0.36111)
# se_h_age <- deltamethod(g=~exp(x1 + x2),
#                          mean = coef(multi_mod_wrace_intx)[c("age_25.10","race_cat.1Hispanic","age_25.10:race_cat.1Hispanic")],
#                          cov = vcov(multi_mod_wrace_intx)[c("age_25.10","race_cat.1Hispanic","age_25.10:race_cat.1Hispanic"),
#                                                           c("age_25.10","race_cat.1Hispanic","age_25.10:race_cat.1Hispanic")])
# 
# round(cbind(OR_h_age, OR_h_age-1.96*se_h_age, OR_h_age+1.96*se_h_age),2)
# 
# 
# OR_o_age <- exp(0.56070 -0.05904 -0.73693)
# se_o_age <- deltamethod(g=~exp(x1 + x2),
#                          mean = coef(multi_mod_wrace_intx)[c("age_25.10","race_cat.1NH_Other","age_25.10:race_cat.1NH_Other")],
#                          cov = vcov(multi_mod_wrace_intx)[c("age_25.10","race_cat.1NH_Other","age_25.10:race_cat.1NH_Other"),
#                                                           c("age_25.10","race_cat.1NH_Other","age_25.10:race_cat.1NH_Other")])
# 
# round(cbind(OR_o_age, OR_o_age-1.96*se_o_age, OR_o_age+1.96*se_o_age),2)
# 





# * * multivariate "kitchen sink" OR model -----
kitchensink_mod <- glm(cai_init ~ year.1 + 
                       relevel(factor(age_25.1), ref = "1")*relevel(factor(race_cat.1), ref = "NH_White") +
                       relevel(factor(income_cat.1), ref = "4_75k+") +
                       relevel(factor(educ3.1), ref = "3_Col+") +
                       relevel(factor(region4.1), ref = "3_South"),
                    data = logit_data, 
                    family=binomial(link="logit")
                    )

summary(kitchensink_mod)

round(exp(coef(kitchensink_mod)),2)
round(exp(confint(kitchensink_mod)),2)

# * * kitchen sink, no income -----
kitchensink2_mod <- glm(cai_init ~ year.1 + 
                          relevel(factor(age_25.1), ref = "1")*relevel(factor(race_cat.1), ref = "NH_White") +
                          relevel(factor(educ_cat.1), ref = "4_Col+") +
                          relevel(factor(region4.1), ref = "3_South"),
                       data = logit_data, 
                       family=binomial(link="logit")
)

summary(kitchensink2_mod)

round(exp(coef(kitchensink2_mod)),2)
round(exp(confint(kitchensink2_mod)),2)

# Prepare dataset: restrict to those who....
# (1) remain off PrEP, 
# (2) yes CAI year 1, 
# and compare based on an "did not have CAI y2."
logit_data2 <- subset(all_off, cai_12_new.1 == 1)
tableNA(all_off$cai_12_new.1) #only 18 missing for Y1.
logit_data2$cai_stop <- NA
logit_data2$cai_stop[logit_data2$cai_12_new.2 == 0] <- 1
logit_data2$cai_stop[logit_data2$cai_12_new.2 == 1] <- 0
#outcome variable = `cai_stop`

logit_data2 <- subset(logit_data2, !is.na(cai_stop))


# Logistic Regression Analyses for CAS cessation -----
# * * bivariate OR estimates
# * * * N, % of total who initiated CAI
tableNA(logit_data2$cai_stop)
round(prop.table(tableNA(logit_data2$cai_stop))*100,0)


# * * * year (linear) ----
year_mod2 <- glm(cai_stop ~ year.1, data = logit_data2, family=binomial(link="logit"))

tableNA(logit_data2$year.1)
round(prop.table(tableNA(logit_data2$year.1))*100,0)

round(exp(coef(year_mod2)),2)
round(exp(confint(year_mod2)),2)


# * * * age (2-level) -----
age_mod2 <- glm(cai_stop ~ relevel(factor(age_25.1), ref = "1"), data = logit_data2, family=binomial(link="logit"))

tableNA(logit_data2$age_25.1)
round(prop.table(tableNA(logit_data2$age_25.1))*100,0)

tableNA(logit_data2$age_25.1, logit_data2$cai_stop)
round(prop.table(tableNA(logit_data2$age_25.1, logit_data2$cai_stop), margin = 1)*100,0)

round(exp(coef(age_mod2)),2)
round(exp(confint(age_mod2)),2)
summary(age_mod2)

# * * * race (4-cats) -----
race_mod2 <- glm(cai_stop ~ relevel(factor(race_cat.1), ref = "NH_White"), data = logit_data2, family=binomial(link="logit"))

tableNA(logit_data2$race_cat.1)
round(prop.table(tableNA(logit_data2$race_cat.1))*100,0)

tableNA(logit_data2$race_cat.1, logit_data2$cai_stop)
round(prop.table(tableNA(logit_data2$race_cat.1, logit_data2$cai_stop), margin = 1)*100,0)

round(exp(coef(race_mod2)),2)
round(exp(confint(race_mod2)),2)
summary(race_mod2)

# * * * income (4-cats) -----
inc_mod2 <- glm(cai_stop ~ relevel(factor(income_cat.1), ref = "4_75k+"), data = logit_data2, family=binomial(link="logit"))

tableNA(logit_data2$income_cat.1)
round(prop.table(tableNA(logit_data2$income_cat.1))*100,0)

tableNA(logit_data2$income_cat.1, logit_data2$cai_stop)
round(prop.table(tableNA(logit_data2$income_cat.1, logit_data2$cai_stop), margin = 1)*100,0)

round(exp(coef(inc_mod2)),2)
round(exp(confint(inc_mod2)),2)
summary(inc_mod2)

# * * * education (3-level) -----
educ_mod2 <- glm(cai_stop ~ relevel(factor(educ3.1), ref = "3_Col+"), data = logit_data2, family=binomial(link="logit"))

tableNA(logit_data2$educ3.1)
round(prop.table(tableNA(logit_data2$educ3.1))*100,0)

tableNA(logit_data2$educ3.1, logit_data2$cai_stop)
round(prop.table(tableNA(logit_data2$educ3.1, logit_data2$cai_stop), margin = 1)*100,0)

round(exp(coef(educ_mod2)),2)
round(exp(confint(educ_mod2)),2)
summary(educ_mod2)

# * * * region (4-level) -----
reg_mod2 <- glm(cai_stop ~ relevel(factor(region4.1), ref = "3_South"), data = logit_data2, family=binomial(link="logit"))

tableNA(logit_data2$region4.1)
round(prop.table(tableNA(logit_data2$region4.1))*100,0)

tableNA(logit_data2$region4.1, logit_data2$cai_stop)
round(prop.table(tableNA(logit_data2$region4.1, logit_data2$cai_stop), margin = 1)*100,0)

round(exp(coef(reg_mod2)),2)
round(exp(confint(reg_mod2)),2)
summary(reg_mod2)

# * * * urban-rural (4-level) -----
dens4_mod2 <- glm(cai_stop ~ nchs4.1, data = logit_data2, family=binomial(link="logit"))

tableNA(logit_data2$nchs4.1)
round(prop.table(tableNA(logit_data2$nchs4.1))*100,0)

tableNA(logit_data2$nchs4.1, logit_data2$cai_stop)
round(prop.table(tableNA(logit_data2$nchs4.1, logit_data2$cai_stop), margin = 1)*100,0)

round(exp(coef(dens4_mod2)),2)
round(exp(confint(dens4_mod2)),2)
summary(dens4_mod2)

# Manuscript Tables & Results -----
# * Supp Table 1 -----
# * * Col i: 
off_prep_1419 <- subset(amis, (year >= 2014 & year <= 2019 & neg_prep12_use_groups == "01_never-or-no-PrEP12"))

tableNA(off_prep_1419$age_25)
round(prop.table(tableNA(off_prep_1419$age_25))*100,0)

tableNA(off_prep_1419$race_cat)
round(prop.table(tableNA(off_prep_1419$race_cat))*100,0)

tableNA(off_prep_1419$region4)
round(prop.table(tableNA(off_prep_1419$region4))*100,0)

tableNA(off_prep_1419$year)
round(prop.table(tableNA(off_prep_1419$year))*100,0)

tableNA(off_prep_1419$nchs4)
round(prop.table(tableNA(off_prep_1419$nchs4))*100,0)

tableNA(off_prep_1419$age_25, off_prep_1419$race_cat)
round(prop.table(tableNA(off_prep_1419$age_25, off_prep_1419$race_cat),margin=2)*100,0)

tableNA(off_prep_1419$income_cat)
round(prop.table(tableNA(off_prep_1419$income_cat))*100,0)

tableNA(off_prep_1419$educ3)
round(prop.table(tableNA(off_prep_1419$educ3))*100,0)

# * * Col 1:
off_prep_1418 <- subset(amis, (year >= 2014 & year <= 2018 & neg_prep12_use_groups == "01_never-or-no-PrEP12"))
# ******** Remove those who appear twice -----
off_prep_1418$id2 <- NA
off_prep_1418$id2[!is.na(off_prep_1418$lastid2014)] <- 1
off_prep_1418$id2[!is.na(off_prep_1418$lastid2015)] <- 1
off_prep_1418$id2[!is.na(off_prep_1418$lastid2016)] <- 1
off_prep_1418$id2[!is.na(off_prep_1418$lastid2017)] <- 1
off_prep_1418$id2[!is.na(off_prep_1418$lastid2018)] <- 1

off_prep_1418 <- subset(off_prep_1418, is.na(id2))


tableNA(off_prep_1418$age_25)
round(prop.table(tableNA(off_prep_1418$age_25))*100,0)

tableNA(off_prep_1418$race_cat)
round(prop.table(tableNA(off_prep_1418$race_cat))*100,0)

tableNA(off_prep_1418$region4)
round(prop.table(tableNA(off_prep_1418$region4))*100,0)

tableNA(off_prep_1418$year)
round(prop.table(tableNA(off_prep_1418$year))*100,0)

tableNA(off_prep_1418$nchs4)
round(prop.table(tableNA(off_prep_1418$nchs4))*100,0)

tableNA(off_prep_1418$age_25, off_prep_1418$race_cat)
round(prop.table(tableNA(off_prep_1418$age_25, off_prep_1418$race_cat),margin=2)*100,0)

# * * Col 2:
tableNA(all_off$age_25.1)
round(prop.table(tableNA(all_off$age_25.1))*100,0)

tableNA(all_off$race_cat.1)
round(prop.table(tableNA(all_off$race_cat.1))*100,0)

tableNA(all_off$region4.1)
round(prop.table(tableNA(all_off$region4.1))*100,0)

tableNA(all_off$year.1)
round(prop.table(tableNA(all_off$year.1))*100,0)

tableNA(all_off$nchs4.1)
round(prop.table(tableNA(all_off$nchs4.1))*100,0)

tableNA(all_off$age_25.1, all_off$race_cat.1)
round(prop.table(tableNA(all_off$age_25.1, all_off$race_cat.1),margin=2)*100,0)

# * Col 3:
chisq.test(cbind(c(11952, 23069), c(565, 1856)))

chisq.test(cbind(c(2422, 5134, 24748, 2075), c(125, 275, 1873, 125))) 

chisq.test(cbind(c(6426, 7300, 13403, 7892), c(425, 518, 923, 555))) 

chisq.test(cbind(c(3899, 8711, 7661, 7159, 7591), c(37, 590, 703, 554, 537))) 

chisq.test(cbind(c(13012, 7615, 10856, 3532), c(985, 494, 746, 196))) 

chisq.test(cbind(c(2675, 2459), c(117, 158))) 

chisq.test(cbind(c(7338, 17410), c(365, 1508))) 

# * Col 4-6:
# * * region4 strat
all_off_ne <- subset(all_off, region4.1 == "1_NorE")

round(prop.table(table(all_off_ne$cai_12_new.1))*100,1)
round(prop.table(table(all_off_ne$cai_12_new.2))*100,1)

# exact2x2((table(all_off_ne$cai_12_new.1, all_off_ne$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_mw <- subset(all_off, region4.1 == "2_MidW")

round(prop.table(table(all_off_mw$cai_12_new.1))*100,1)
round(prop.table(table(all_off_mw$cai_12_new.2))*100,1)

# exact2x2((table(all_off_mw$cai_12_new.1, all_off_mw$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_so <- subset(all_off, region4.1 == "3_South")

round(prop.table(table(all_off_so$cai_12_new.1))*100,1)
round(prop.table(table(all_off_so$cai_12_new.2))*100,1)

# exact2x2((table(all_off_so$cai_12_new.1, all_off_so$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_we <- subset(all_off, region4.1 == "4_West")

round(prop.table(table(all_off_we$cai_12_new.1))*100,1)
round(prop.table(table(all_off_we$cai_12_new.2))*100,1)

# exact2x2((table(all_off_we$cai_12_new.1, all_off_we$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * year strat 
all_off_14 <- subset(all_off, year.1 == 2014)

round(prop.table(table(all_off_14$cai_12_new.1))*100,1)
round(prop.table(table(all_off_14$cai_12_new.2))*100,1)

# exact2x2((table(all_off_14$cai_12_new.1, all_off_14$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_15 <- subset(all_off, year.1 == 2015)

round(prop.table(table(all_off_15$cai_12_new.1))*100,1)
round(prop.table(table(all_off_15$cai_12_new.2))*100,1)

# exact2x2((table(all_off_15$cai_12_new.1, all_off_15$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_16 <- subset(all_off, year.1 == 2016)

round(prop.table(table(all_off_16$cai_12_new.1))*100,1)
round(prop.table(table(all_off_16$cai_12_new.2))*100,1)

# exact2x2((table(all_off_16$cai_12_new.1, all_off_16$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_17 <- subset(all_off, year.1 == 2017)

round(prop.table(table(all_off_17$cai_12_new.1))*100,1)
round(prop.table(table(all_off_17$cai_12_new.2))*100,1)

# exact2x2((table(all_off_17$cai_12_new.1, all_off_17$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_18 <- subset(all_off, year.1 == 2018)

round(prop.table(table(all_off_18$cai_12_new.1))*100,1)
round(prop.table(table(all_off_18$cai_12_new.2))*100,1)

# exact2x2((table(all_off_18$cai_12_new.1, all_off_18$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

# * * nchs4 strat
all_off_lc <- subset(all_off, nchs4.1 == "1_LgCnt")

round(prop.table(table(all_off_lc$cai_12_new.1))*100,1)
round(prop.table(table(all_off_lc$cai_12_new.2))*100,1)

# exact2x2((table(all_off_lc$cai_12_new.1, all_off_lc$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_lf <- subset(all_off, nchs4.1 == "2_LgFrg")

round(prop.table(table(all_off_lf$cai_12_new.1))*100,1)
round(prop.table(table(all_off_lf$cai_12_new.2))*100,1)

# exact2x2((table(all_off_lf$cai_12_new.1, all_off_lf$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_ms <- subset(all_off, nchs4.1 == "3_Metro")

round(prop.table(table(all_off_ms$cai_12_new.1))*100,1)
round(prop.table(table(all_off_ms$cai_12_new.2))*100,1)

# exact2x2((table(all_off_ms$cai_12_new.1, all_off_ms$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_nc <- subset(all_off, nchs4.1 == "4_Rural")

round(prop.table(table(all_off_nc$cai_12_new.1))*100,1)
round(prop.table(table(all_off_nc$cai_12_new.2))*100,1)

# exact2x2((table(all_off_nc$cai_12_new.1, all_off_nc$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_h25 <- subset(all_off, (race_cat.1 == "Hispanic" & age_25.1 == 1))

round(prop.table(table(all_off_h25$cai_12_new.1))*100,1)
round(prop.table(table(all_off_h25$cai_12_new.2))*100,1)

# exact2x2((table(all_off_h25$cai_12_new.1, all_off_h25$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_w14 <- subset(all_off, (race_cat.1 == "NH_White" & age_25.1 == 0))

round(prop.table(table(all_off_w14$cai_12_new.1))*100,1)
round(prop.table(table(all_off_w14$cai_12_new.2))*100,1)

# exact2x2((table(all_off_w14$cai_12_new.1, all_off_w14$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


all_off_w25 <- subset(all_off, (race_cat.1 == "NH_White" & age_25.1 == 1))

round(prop.table(table(all_off_w25$cai_12_new.1))*100,1)
round(prop.table(table(all_off_w25$cai_12_new.2))*100,1)

# exact2x2((table(all_off_w25$cai_12_new.1, all_off_w25$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test

### R&R Table 4 :: add year dyads 
tableNA(all_off$cai_12_new.1)

tab4 <- subset(all_off, !is.na(cai_12_new.1) & !is.na(cai_12_new.2))

tab4_1415 <- subset(tab4, year.1 == 2014)
tab4_1516 <- subset(tab4, year.1 == 2015)
tab4_1617 <- subset(tab4, year.1 == 2016)
tab4_1718 <- subset(tab4, year.1 == 2017)
tab4_1819 <- subset(tab4, year.1 == 2018)

##### AD HOC - get year-on-year dyads by partner HIV stats (2023.11.03) -------
nrow(tab4_1415)
nrow(tab4_1516)
nrow(tab4_1617)
nrow(tab4_1718)
nrow(tab4_1819)


round(prop.table(table(tab4_1415$cai_pos_12.1))*100,1)[2]
round(prop.table(table(tab4_1516$cai_pos_12.1))*100,1)[2]
round(prop.table(table(tab4_1617$cai_pos_12.1))*100,1)[2]
round(prop.table(table(tab4_1718$cai_pos_12.1))*100,1)[2]
round(prop.table(table(tab4_1819$cai_pos_12.1))*100,1)[2]

round(prop.table(table(tab4_1415$cai_pos_12.2))*100,1)[2]
round(prop.table(table(tab4_1516$cai_pos_12.2))*100,1)[2]
round(prop.table(table(tab4_1617$cai_pos_12.2))*100,1)[2]
round(prop.table(table(tab4_1718$cai_pos_12.2))*100,1)[2]
round(prop.table(table(tab4_1819$cai_pos_12.2))*100,1)[2]

exact2x2((table(tab4_1415$cai_pos_12.1, tab4_1415$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1516$cai_pos_12.1, tab4_1516$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1617$cai_pos_12.1, tab4_1617$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1718$cai_pos_12.1, tab4_1718$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1819$cai_pos_12.1, tab4_1819$cai_pos_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test



round(prop.table(table(tab4_1415$cai_neg_12.1))*100,1)[2]
round(prop.table(table(tab4_1516$cai_neg_12.1))*100,1)[2]
round(prop.table(table(tab4_1617$cai_neg_12.1))*100,1)[2]
round(prop.table(table(tab4_1718$cai_neg_12.1))*100,1)[2]
round(prop.table(table(tab4_1819$cai_neg_12.1))*100,1)[2]

round(prop.table(table(tab4_1415$cai_neg_12.2))*100,1)[2]
round(prop.table(table(tab4_1516$cai_neg_12.2))*100,1)[2]
round(prop.table(table(tab4_1617$cai_neg_12.2))*100,1)[2]
round(prop.table(table(tab4_1718$cai_neg_12.2))*100,1)[2]
round(prop.table(table(tab4_1819$cai_neg_12.2))*100,1)[2]

exact2x2((table(tab4_1415$cai_neg_12.1, tab4_1415$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1516$cai_neg_12.1, tab4_1516$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1617$cai_neg_12.1, tab4_1617$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1718$cai_neg_12.1, tab4_1718$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1819$cai_neg_12.1, tab4_1819$cai_neg_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test



round(prop.table(table(tab4_1415$cai_unk_12.1))*100,1)[2]
round(prop.table(table(tab4_1516$cai_unk_12.1))*100,1)[2]
round(prop.table(table(tab4_1617$cai_unk_12.1))*100,1)[2]
round(prop.table(table(tab4_1718$cai_unk_12.1))*100,1)[2]
round(prop.table(table(tab4_1819$cai_unk_12.1))*100,1)[2]

round(prop.table(table(tab4_1415$cai_unk_12.2))*100,1)[2]
round(prop.table(table(tab4_1516$cai_unk_12.2))*100,1)[2]
round(prop.table(table(tab4_1617$cai_unk_12.2))*100,1)[2]
round(prop.table(table(tab4_1718$cai_unk_12.2))*100,1)[2]
round(prop.table(table(tab4_1819$cai_unk_12.2))*100,1)[2]

#exact2x2((table(tab4_1415$cai_unk_12.1, tab4_1415$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1516$cai_unk_12.1, tab4_1516$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1617$cai_unk_12.1, tab4_1617$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1718$cai_unk_12.1, tab4_1718$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1819$cai_unk_12.1, tab4_1819$cai_unk_12.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test


#########

# col 1
cbind(table(tab4$year.1),
round(prop.table(table(tab4$year.1))*100))

# col 2
round(prop.table(table(tab4_1415$cai_12_new.1))*100,2)[2]
round(prop.table(table(tab4_1516$cai_12_new.1))*100,2)[2]
round(prop.table(table(tab4_1617$cai_12_new.1))*100,2)[2]
round(prop.table(table(tab4_1718$cai_12_new.1))*100,2)[2]
round(prop.table(table(tab4_1819$cai_12_new.1))*100,2)[2]

# col 3
round(prop.table(table(tab4_1415$cai_12_new.2))*100,2)[2]
round(prop.table(table(tab4_1516$cai_12_new.2))*100,2)[2]
round(prop.table(table(tab4_1617$cai_12_new.2))*100,2)[2]
round(prop.table(table(tab4_1718$cai_12_new.2))*100,2)[2]
round(prop.table(table(tab4_1819$cai_12_new.2))*100,2)[2]

# col 5
exact2x2((table(tab4_1415$cai_12_new.1, tab4_1415$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1516$cai_12_new.1, tab4_1516$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1617$cai_12_new.1, tab4_1617$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1718$cai_12_new.1, tab4_1718$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test
exact2x2((table(tab4_1819$cai_12_new.1, tab4_1819$cai_12_new.2)), alternative="greater", conf.level=0.95, paired=T) #one-sided McNemar test



# * Figure 1 (trend tests) -----
noprep_1419_all <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12"))
round(prop.table(tableNA(noprep_1419_all$cai_12_new, noprep_1419_all$year),margin=2)*100,0)
tableNA(noprep_1419_all$year)


noprep_1419_14.25 <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & age_25 == 0))
round(prop.table(tableNA(noprep_1419_14.25$cai_12_new, noprep_1419_14.25$year),margin=2)*100,0)
tableNA(noprep_1419_14.25$year)


noprep_1419_o25 <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & age_25 == 1))
round(prop.table(tableNA(noprep_1419_o25$cai_12_new, noprep_1419_o25$year),margin=2)*100,0)
tableNA(noprep_1419_o25$year)


noprep_1419_bl <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & race_cat == "NH_Black"))
round(prop.table(tableNA(noprep_1419_bl$cai_12_new, noprep_1419_bl$year),margin=2)*100,0)
tableNA(noprep_1419_bl$year)

noprep_1419_h <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & race_cat == "Hispanic"))
round(prop.table(tableNA(noprep_1419_h$cai_12_new, noprep_1419_h$year),margin=2)*100,0)
tableNA(noprep_1419_h$year)


noprep_1419_w <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & race_cat == "NH_White"))
round(prop.table(tableNA(noprep_1419_w$cai_12_new, noprep_1419_w$year),margin=2)*100,0)
tableNA(noprep_1419_w$year)


noprep_1419_o <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & race_cat == "NH_Other"))
round(prop.table(tableNA(noprep_1419_o$cai_12_new, noprep_1419_o$year),margin=2)*100,0)
tableNA(noprep_1419_o$year)


noprep_1419_lt20 <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & income_cat == "1_<20k"))
round(prop.table(tableNA(noprep_1419_lt20$cai_12_new, noprep_1419_lt20$year),margin=2)*100,0)
tableNA(noprep_1419_lt20$year)

noprep_1419_20.40 <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & income_cat == "2_<40k"))
round(prop.table(tableNA(noprep_1419_20.40$cai_12_new, noprep_1419_20.40$year),margin=2)*100,0)
tableNA(noprep_1419_20.40$year)

noprep_1419_40.75 <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & income_cat == "3_<75k"))
round(prop.table(tableNA(noprep_1419_40.75$cai_12_new, noprep_1419_40.75$year),margin=2)*100,0)
tableNA(noprep_1419_40.75$year)

noprep_1419_75k <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & income_cat == "4_75k+"))
round(prop.table(tableNA(noprep_1419_75k$cai_12_new, noprep_1419_75k$year),margin=2)*100,0)
tableNA(noprep_1419_75k$year)


noprep_1419_ltHS <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & educ3 == "1_HS-or-less"))
round(prop.table(tableNA(noprep_1419_ltHS$cai_12_new, noprep_1419_ltHS$year),margin=2)*100,0)
tableNA(noprep_1419_ltHS$year)

noprep_1419_ltCol <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & educ3 == "2_ltCol"))
round(prop.table(tableNA(noprep_1419_ltCol$cai_12_new, noprep_1419_ltCol$year),margin=2)*100,0)
tableNA(noprep_1419_ltCol$year)

noprep_1419_Col <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & educ3 == "3_Col+"))
round(prop.table(tableNA(noprep_1419_Col$cai_12_new, noprep_1419_Col$year),margin=2)*100,0)
tableNA(noprep_1419_Col$year)


noprep_1419_NE <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & region4 == "1_NorE"))
round(prop.table(tableNA(noprep_1419_NE$cai_12_new, noprep_1419_NE$year),margin=2)*100,0)
tableNA(noprep_1419_NE$year)

noprep_1419_MW <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & region4 == "2_MidW"))
round(prop.table(tableNA(noprep_1419_MW$cai_12_new, noprep_1419_MW$year),margin=2)*100,0)
tableNA(noprep_1419_MW$year)

noprep_1419_So <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & region4 == "3_South"))
round(prop.table(tableNA(noprep_1419_So$cai_12_new, noprep_1419_So$year),margin=2)*100,0)
tableNA(noprep_1419_So$year)

noprep_1419_We <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & region4 == "4_West"))
round(prop.table(tableNA(noprep_1419_We$cai_12_new, noprep_1419_We$year),margin=2)*100,0)
tableNA(noprep_1419_We$year)


noprep_1419_LC <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & nchs4 == "1_LgCnt"))
round(prop.table(tableNA(noprep_1419_LC$cai_12_new, noprep_1419_LC$year),margin=2)*100,0)
tableNA(noprep_1419_LC$year)

noprep_1419_LF <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & nchs4 == "2_LgFrg"))
round(prop.table(tableNA(noprep_1419_LF$cai_12_new, noprep_1419_LF$year),margin=2)*100,0)
tableNA(noprep_1419_LF$year)

noprep_1419_Met <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & nchs4 == "3_Metro"))
round(prop.table(tableNA(noprep_1419_Met$cai_12_new, noprep_1419_Met$year),margin=2)*100,0)
tableNA(noprep_1419_Met$year)

noprep_1419_Rur <- subset(amis, (year >= 2014 & neg_prep12_use_groups == "01_never-or-no-PrEP12" & nchs4 == "4_Rural"))
round(prop.table(tableNA(noprep_1419_Rur$cai_12_new, noprep_1419_Rur$year),margin=2)*100,0)
tableNA(noprep_1419_Rur$year)

# * Trend tests:
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

inc_1_test_data <- c(267, 746, 679, 537, 757, 794, 402, 1158, 1054, 855, 1145, 1190)
inc_1_test <- matrix(inc_1_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(inc_1_test, alternative = "one.sided")

inc_2_test_data <- c(437, 1003, 851, 774, 1024, 1021, 663, 1572, 1340, 1222, 1504, 1478)
inc_2_test <- matrix(inc_2_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(inc_2_test, alternative = "one.sided")

inc_3_test_data <- c(637, 1218, 1228, 1064, 1301, 1287, 942, 1941, 1873, 1722, 1962, 1888)
inc_3_test <- matrix(inc_3_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(inc_3_test, alternative = "one.sided")

inc_4_test_data <- c(992, 1503, 1520, 1565, 1611, 1468, 1507, 2520, 2388, 2554, 2559, 2271)
inc_4_test <- matrix(inc_4_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(inc_4_test, alternative = "one.sided")


ed1_test_data <- c(232, 730, 756, 800, 1217, 1183, 385, 1275, 1300, 1417, 2163, 1991)
ed1_test <- matrix(ed1_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(ed1_test, alternative = "one.sided")

ed2_test_data <- c(858, 1864, 1792, 1503, 1941, 1936, 1237, 2962, 2723, 2341, 2800, 2755)
ed2_test <- matrix(ed2_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(ed2_test, alternative = "one.sided")

ed3_test_data <- c(1466, 2762, 2662, 2552, 2054, 1983, 2246, 4447, 4128, 4050, 3135, 3023)
ed3_test <- matrix(ed3_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(ed3_test, alternative = "one.sided")


NE_test_data <- c(422, 1012, 970, 908, 784, 823, 671, 1790, 1560, 1486, 1297, 1317)
NE_test <- matrix(NE_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(NE_test, alternative = "one.sided")

MW_test_data <- c(554, 1183, 1045,  976, 1159, 1073,  830, 1870, 1660, 1554, 1830, 1636)
MW_test <- matrix(MW_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(MW_test, alternative = "one.sided")

So_test_data <- c(1029, 2007, 2112, 1890, 2093, 2188, 1542, 3179, 3262, 3054, 3168, 3279)
So_test <- matrix(So_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(So_test, alternative = "one.sided")

We_test_data <- c(567, 1218, 1140, 1148, 1219, 1040, 856, 1959, 1804, 1831, 1902, 1586)
We_test <- matrix(We_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(We_test, alternative = "one.sided")


LC_test_data <- c(1050, 2078, 2059, 1956, 1736, 1657, 1567, 3361, 3221, 3072, 2693, 2558)
LC_test <- matrix(LC_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(LC_test, alternative = "one.sided")

LF_test_data <- c(491, 1064, 1148, 1058, 1094, 1126, 790, 1808, 1849, 1800, 1772, 1730)
LF_test <- matrix(LF_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(LF_test, alternative = "one.sided")

Met_test_data <- c(778, 1722, 1531, 1472, 1846, 1749, 1173, 2722, 2415, 2349, 2828, 2631)
Met_test <- matrix(Met_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(Met_test, alternative = "one.sided")

Rur_test_data <- c(253, 553, 529, 435, 579, 592, 368, 904, 801, 703, 903, 898)
Rur_test <- matrix(Rur_test_data, nrow=2, ncol=6,byrow=T)
CochranArmitageTest(Rur_test, alternative = "one.sided")

# Investigate total sample demo breakdown, by year -----
tableNA(amis$year, amis$age_5_level)
round(prop.table(tableNA(amis$year, amis$age_5_level), margin = 1)*100, 1)

tableNA(amis$year, amis$race_cat)
round(prop.table(tableNA(amis$year, amis$race_cat), margin = 1)*100, 1)

tableNA(amis$year, amis$income_cat)
round(prop.table(tableNA(amis$year, amis$income_cat), margin = 1)*100, 1)

tableNA(amis$year, amis$nchs4)
round(prop.table(tableNA(amis$year, amis$nchs4), margin = 1)*100, 1)

tableNA(amis$year, amis$region4)
round(prop.table(tableNA(amis$year, amis$region4), margin = 1)*100, 1)

tableNA(amis$year, amis$educ_cat)
round(prop.table(tableNA(amis$year, amis$educ_cat), margin = 1)*100, 1)

amis_2014 <- subset(amis, year == 2014)
amis_2015 <- subset(amis, year == 2015)
amis_2016 <- subset(amis, year == 2016)
amis_2017 <- subset(amis, year == 2017)
amis_2018 <- subset(amis, year == 2018)

median(amis_2014$age)
median(amis_2015$age)
median(amis_2016$age)
median(amis_2017$age)
median(amis_2018$age)

# Investigate Demographic Breakdown of Analytic sample, by year -------
tableNA(all_off$year.1, all_off$age_5_level.1)
round(prop.table(tableNA(all_off$year.1, all_off$age_5_level.1), margin = 1)*100, 1)

tableNA(all_off$year.1, all_off$race_cat.1)
round(prop.table(tableNA(all_off$year.1, all_off$race_cat.1), margin = 1)*100, 1)

tableNA(all_off$year.1, all_off$income_cat.1)
round(prop.table(tableNA(all_off$year.1, all_off$income_cat.1), margin = 1)*100, 1)

tableNA(all_off$year.1, all_off$nchs4.1)
round(prop.table(tableNA(all_off$year.1, all_off$nchs4.1), margin = 1)*100, 1)

tableNA(all_off$year.1, all_off$region4.1)
round(prop.table(tableNA(all_off$year.1, all_off$region4.1), margin = 1)*100, 1)

tableNA(all_off$year.1, all_off$educ_cat.1)
round(prop.table(tableNA(all_off$year.1, all_off$educ_cat.1), margin = 1)*100, 1)

all_off_2014 <- subset(all_off, year.1 == 2014)
all_off_2015 <- subset(all_off, year.1 == 2015)
all_off_2016 <- subset(all_off, year.1 == 2016)
all_off_2017 <- subset(all_off, year.1 == 2017)
all_off_2018 <- subset(all_off, year.1 == 2018)


median(all_off_2014$age.1)
median(all_off_2015$age.1)
median(all_off_2016$age.1)
median(all_off_2017$age.1)
median(all_off_2018$age.1)


### Age investigation -----
nrow(all_off)
summary(all_off$age.1)

summary(all_off_2014$age.1)
summary(all_off_2015$age.1)
summary(all_off_2016$age.1)
summary(all_off_2017$age.1)
summary(all_off_2018$age.1)

# * by race
# * * Black
all_off_b <- subset(all_off, race_cat.1 == "NH_Black")
nrow(all_off_b)

all_off_2014_b <- subset(all_off_2014, race_cat.1 == "NH_Black")
all_off_2015_b <- subset(all_off_2015, race_cat.1 == "NH_Black")
all_off_2016_b <- subset(all_off_2016, race_cat.1 == "NH_Black")
all_off_2017_b <- subset(all_off_2017, race_cat.1 == "NH_Black")
all_off_2018_b <- subset(all_off_2018, race_cat.1 == "NH_Black")

summary(all_off_2014_b$age.1)
summary(all_off_2015_b$age.1)
summary(all_off_2016_b$age.1)
summary(all_off_2017_b$age.1)
summary(all_off_2018_b$age.1)
summary(all_off_b$age.1)

# * * Hispanic
all_off_h <- subset(all_off, race_cat.1 == "Hispanic")
nrow(all_off_h)

all_off_2014_h <- subset(all_off_2014, race_cat.1 == "Hispanic")
all_off_2015_h <- subset(all_off_2015, race_cat.1 == "Hispanic")
all_off_2016_h <- subset(all_off_2016, race_cat.1 == "Hispanic")
all_off_2017_h <- subset(all_off_2017, race_cat.1 == "Hispanic")
all_off_2018_h <- subset(all_off_2018, race_cat.1 == "Hispanic")

summary(all_off_2014_h$age.1)
summary(all_off_2015_h$age.1)
summary(all_off_2016_h$age.1)
summary(all_off_2017_h$age.1)
summary(all_off_2018_h$age.1)
summary(all_off_h$age.1)

# * * Other race
all_off_o <- subset(all_off, race_cat.1 == "NH_Other")

all_off_2014_o <- subset(all_off_2014, race_cat.1 == "NH_Other")
all_off_2015_o <- subset(all_off_2015, race_cat.1 == "NH_Other")
all_off_2016_o <- subset(all_off_2016, race_cat.1 == "NH_Other")
all_off_2017_o <- subset(all_off_2017, race_cat.1 == "NH_Other")
all_off_2018_o <- subset(all_off_2018, race_cat.1 == "NH_Other")

summary(all_off_2014_o$age.1)
summary(all_off_2015_o$age.1)
summary(all_off_2016_o$age.1)
summary(all_off_2017_o$age.1)
summary(all_off_2018_o$age.1)
summary(all_off_o$age.1)

# * * White
all_off_w <- subset(all_off, race_cat.1 == "NH_White")
nrow(all_off_w)

all_off_2014_w <- subset(all_off_2014, race_cat.1 == "NH_White")
all_off_2015_w <- subset(all_off_2015, race_cat.1 == "NH_White")
all_off_2016_w <- subset(all_off_2016, race_cat.1 == "NH_White")
all_off_2017_w <- subset(all_off_2017, race_cat.1 == "NH_White")
all_off_2018_w <- subset(all_off_2018, race_cat.1 == "NH_White")

summary(all_off_2014_w$age.1)
summary(all_off_2015_w$age.1)
summary(all_off_2016_w$age.1)
summary(all_off_2017_w$age.1)
summary(all_off_2018_w$age.1)
summary(all_off_w$age.1)

# * by density
# * * urban/central
all_off_central <- subset(all_off, nchs4.1 == "1_LgCnt")

all_off_2014_central <- subset(all_off_2014, nchs4.1 == "1_LgCnt")
all_off_2015_central <- subset(all_off_2015, nchs4.1 == "1_LgCnt")
all_off_2016_central <- subset(all_off_2016, nchs4.1 == "1_LgCnt")
all_off_2017_central <- subset(all_off_2017, nchs4.1 == "1_LgCnt")
all_off_2018_central <- subset(all_off_2018, nchs4.1 == "1_LgCnt")

summary(all_off_2014_central$age.1)
summary(all_off_2015_central$age.1)
summary(all_off_2016_central$age.1)
summary(all_off_2017_central$age.1)
summary(all_off_2018_central$age.1)
summary(all_off_central$age.1)

# * * fringe
all_off_fringe <- subset(all_off, nchs4.1 == "2_LgFrg")

all_off_2014_fringe <- subset(all_off_2014, nchs4.1 == "2_LgFrg")
all_off_2015_fringe <- subset(all_off_2015, nchs4.1 == "2_LgFrg")
all_off_2016_fringe <- subset(all_off_2016, nchs4.1 == "2_LgFrg")
all_off_2017_fringe <- subset(all_off_2017, nchs4.1 == "2_LgFrg")
all_off_2018_fringe <- subset(all_off_2018, nchs4.1 == "2_LgFrg")

summary(all_off_2014_fringe$age.1)
summary(all_off_2015_fringe$age.1)
summary(all_off_2016_fringe$age.1)
summary(all_off_2017_fringe$age.1)
summary(all_off_2018_fringe$age.1)
summary(all_off_fringe$age.1)

# * * metro
all_off_metro <- subset(all_off, nchs4.1 == "3_Metro")

all_off_2014_metro <- subset(all_off_2014, nchs4.1 == "3_Metro")
all_off_2015_metro <- subset(all_off_2015, nchs4.1 == "3_Metro")
all_off_2016_metro <- subset(all_off_2016, nchs4.1 == "3_Metro")
all_off_2017_metro <- subset(all_off_2017, nchs4.1 == "3_Metro")
all_off_2018_metro <- subset(all_off_2018, nchs4.1 == "3_Metro")

summary(all_off_2014_metro$age.1)
summary(all_off_2015_metro$age.1)
summary(all_off_2016_metro$age.1)
summary(all_off_2017_metro$age.1)
summary(all_off_2018_metro$age.1)
summary(all_off_metro$age.1)

# * * rural
all_off_rural <- subset(all_off, nchs4.1 == "4_Rural")

all_off_2014_rural <- subset(all_off_2014, nchs4.1 == "4_Rural")
all_off_2015_rural <- subset(all_off_2015, nchs4.1 == "4_Rural")
all_off_2016_rural <- subset(all_off_2016, nchs4.1 == "4_Rural")
all_off_2017_rural <- subset(all_off_2017, nchs4.1 == "4_Rural")
all_off_2018_rural <- subset(all_off_2018, nchs4.1 == "4_Rural")

summary(all_off_2014_rural$age.1)
summary(all_off_2015_rural$age.1)
summary(all_off_2016_rural$age.1)
summary(all_off_2017_rural$age.1)
summary(all_off_2018_rural$age.1)
summary(all_off_rural$age.1)

# * education
# * * HS, less
all_off_hsl <- subset(all_off, educ3.1 == "1_HS-or-less")

all_off_2014_hsl <- subset(all_off_2014, educ3.1 == "1_HS-or-less")
all_off_2015_hsl <- subset(all_off_2015, educ3.1 == "1_HS-or-less")
all_off_2016_hsl <- subset(all_off_2016, educ3.1 == "1_HS-or-less")
all_off_2017_hsl <- subset(all_off_2017, educ3.1 == "1_HS-or-less")
all_off_2018_hsl <- subset(all_off_2018, educ3.1 == "1_HS-or-less")

summary(all_off_2014_hsl$age.1)
summary(all_off_2015_hsl$age.1)
summary(all_off_2016_hsl$age.1)
summary(all_off_2017_hsl$age.1)
summary(all_off_2018_hsl$age.1)
summary(all_off_hsl$age.1)

# * * some college
all_off_ltc <- subset(all_off, educ3.1 == "2_ltCol")

all_off_2014_ltc <- subset(all_off_2014, educ3.1 == "2_ltCol")
all_off_2015_ltc <- subset(all_off_2015, educ3.1 == "2_ltCol")
all_off_2016_ltc <- subset(all_off_2016, educ3.1 == "2_ltCol")
all_off_2017_ltc <- subset(all_off_2017, educ3.1 == "2_ltCol")
all_off_2018_ltc <- subset(all_off_2018, educ3.1 == "2_ltCol")

summary(all_off_2014_ltc$age.1)
summary(all_off_2015_ltc$age.1)
summary(all_off_2016_ltc$age.1)
summary(all_off_2017_ltc$age.1)
summary(all_off_2018_ltc$age.1)
summary(all_off_ltc$age.1)

# * * college and beyond
all_off_col <- subset(all_off, educ3.1 == "3_Col+")

all_off_2014_col <- subset(all_off_2014, educ3.1 == "3_Col+")
all_off_2015_col <- subset(all_off_2015, educ3.1 == "3_Col+")
all_off_2016_col <- subset(all_off_2016, educ3.1 == "3_Col+")
all_off_2017_col <- subset(all_off_2017, educ3.1 == "3_Col+")
all_off_2018_col <- subset(all_off_2018, educ3.1 == "3_Col+")

summary(all_off_2014_col$age.1)
summary(all_off_2015_col$age.1)
summary(all_off_2016_col$age.1)
summary(all_off_2017_col$age.1)
summary(all_off_2018_col$age.1)
summary(all_off_col$age.1)

# * income
# * * <20k
all_off_20k <- subset(all_off, income_cat.1 == "1_<20k")

all_off_2014_20k <- subset(all_off_2014, income_cat.1 == "1_<20k")
all_off_2015_20k <- subset(all_off_2015, income_cat.1 == "1_<20k")
all_off_2016_20k <- subset(all_off_2016, income_cat.1 == "1_<20k")
all_off_2017_20k <- subset(all_off_2017, income_cat.1 == "1_<20k")
all_off_2018_20k <- subset(all_off_2018, income_cat.1 == "1_<20k")

summary(all_off_2014_20k$age.1)
summary(all_off_2015_20k$age.1)
summary(all_off_2016_20k$age.1)
summary(all_off_2017_20k$age.1)
summary(all_off_2018_20k$age.1)
summary(all_off_20k$age.1)

# * * 20-39.99k
all_off_40k <- subset(all_off, income_cat.1 == "2_<40k")

all_off_2014_40k <- subset(all_off_2014, income_cat.1 == "2_<40k")
all_off_2015_40k <- subset(all_off_2015, income_cat.1 == "2_<40k")
all_off_2016_40k <- subset(all_off_2016, income_cat.1 == "2_<40k")
all_off_2017_40k <- subset(all_off_2017, income_cat.1 == "2_<40k")
all_off_2018_40k <- subset(all_off_2018, income_cat.1 == "2_<40k")

summary(all_off_2014_40k$age.1)
summary(all_off_2015_40k$age.1)
summary(all_off_2016_40k$age.1)
summary(all_off_2017_40k$age.1)
summary(all_off_2018_40k$age.1)
summary(all_off_40k$age.1)

# * * 40-74.99k
all_off_75k <- subset(all_off, income_cat.1 == "3_<75k")

all_off_2014_75k <- subset(all_off_2014, income_cat.1 == "3_<75k")
all_off_2015_75k <- subset(all_off_2015, income_cat.1 == "3_<75k")
all_off_2016_75k <- subset(all_off_2016, income_cat.1 == "3_<75k")
all_off_2017_75k <- subset(all_off_2017, income_cat.1 == "3_<75k")
all_off_2018_75k <- subset(all_off_2018, income_cat.1 == "3_<75k")

summary(all_off_2014_75k$age.1)
summary(all_off_2015_75k$age.1)
summary(all_off_2016_75k$age.1)
summary(all_off_2017_75k$age.1)
summary(all_off_2018_75k$age.1)
summary(all_off_75k$age.1)

# * * 75k+
all_off_75_ <- subset(all_off, income_cat.1 == "4_75k+")

all_off_2014_75_ <- subset(all_off_2014, income_cat.1 == "4_75k+")
all_off_2015_75_ <- subset(all_off_2015, income_cat.1 == "4_75k+")
all_off_2016_75_ <- subset(all_off_2016, income_cat.1 == "4_75k+")
all_off_2017_75_ <- subset(all_off_2017, income_cat.1 == "4_75k+")
all_off_2018_75_ <- subset(all_off_2018, income_cat.1 == "4_75k+")

summary(all_off_2014_75_$age.1)
summary(all_off_2015_75_$age.1)
summary(all_off_2016_75_$age.1)
summary(all_off_2017_75_$age.1)
summary(all_off_2018_75_$age.1)
summary(all_off_75_$age.1)

# * region
# * * northeast
table(all_off_ne$region4.1)

all_off_2014_ne <- subset(all_off_ne, year.1 == 2014)
nrow(all_off_2014_ne)
all_off_2015_ne <- subset(all_off_ne, year.1 == 2015)
nrow(all_off_2015_ne)
all_off_2016_ne <- subset(all_off_ne, year.1 == 2016)
nrow(all_off_2016_ne)
all_off_2017_ne <- subset(all_off_ne, year.1 == 2017)
nrow(all_off_2017_ne)
all_off_2018_ne <- subset(all_off_ne, year.1 == 2018)
nrow(all_off_2018_ne)

summary(all_off_2014_ne$age.1)
summary(all_off_2015_ne$age.1)
summary(all_off_2016_ne$age.1)
summary(all_off_2017_ne$age.1)
summary(all_off_2018_ne$age.1)
summary(all_off_ne$age.1)


# * * midwest
table(all_off_mw$region4.1)

all_off_2014_mw <- subset(all_off_mw, year.1 == 2014)
nrow(all_off_2014_mw)
all_off_2015_mw <- subset(all_off_mw, year.1 == 2015)
nrow(all_off_2015_mw)
all_off_2016_mw <- subset(all_off_mw, year.1 == 2016)
nrow(all_off_2016_mw)
all_off_2017_mw <- subset(all_off_mw, year.1 == 2017)
nrow(all_off_2017_mw)
all_off_2018_mw <- subset(all_off_mw, year.1 == 2018)
nrow(all_off_2018_mw)

summary(all_off_2014_mw$age.1)
summary(all_off_2015_mw$age.1)
summary(all_off_2016_mw$age.1)
summary(all_off_2017_mw$age.1)
summary(all_off_2018_mw$age.1)
summary(all_off_mw$age.1)

# * * south
table(all_off_so$region4.1)

all_off_2014_so <- subset(all_off_so, year.1 == 2014)
nrow(all_off_2014_so)
all_off_2015_so <- subset(all_off_so, year.1 == 2015)
nrow(all_off_2015_so)
all_off_2016_so <- subset(all_off_so, year.1 == 2016)
nrow(all_off_2016_so)
all_off_2017_so <- subset(all_off_so, year.1 == 2017)
nrow(all_off_2017_so)
all_off_2018_so <- subset(all_off_so, year.1 == 2018)
nrow(all_off_2018_so)

summary(all_off_2014_so$age.1)
summary(all_off_2015_so$age.1)
summary(all_off_2016_so$age.1)
summary(all_off_2017_so$age.1)
summary(all_off_2018_so$age.1)
summary(all_off_so$age.1)

# * * west
table(all_off_we$region4.1)

all_off_2014_we <- subset(all_off_we, year.1 == 2014)
nrow(all_off_2014_we)
all_off_2015_we <- subset(all_off_we, year.1 == 2015)
nrow(all_off_2015_we)
all_off_2016_we <- subset(all_off_we, year.1 == 2016)
nrow(all_off_2016_we)
all_off_2017_we <- subset(all_off_we, year.1 == 2017)
nrow(all_off_2017_we)
all_off_2018_we <- subset(all_off_we, year.1 == 2018)
nrow(all_off_2018_we)

summary(all_off_2014_we$age.1)
summary(all_off_2015_we$age.1)
summary(all_off_2016_we$age.1)
summary(all_off_2017_we$age.1)
summary(all_off_2018_we$age.1)
summary(all_off_we$age.1)

### September 20th meeting - new analyses -----
start_stay_2yr <- subset(amis_two_match, neg_prep12_use_groups.1 == "01_never-or-no-PrEP12")
start_stay_2yr <- drop_na(start_stay_2yr, neg_prep12_use_groups.2)
start_stay_2yr_2CAS <- subset(start_stay_2yr, N_cai_sps_12.1 >=2)
start_2CAS <- subset(start_stay_2yr_2CAS, neg_prep12_use_groups.2 == "02_PrEPcur")
stayoff_2CAS <- subset(start_stay_2yr_2CAS, neg_prep12_use_groups.2 == "01_never-or-no-PrEP12")

table(start_stay_2yr$neg_prep12_use_groups.2)
table(start_stay_2yr$neg_prep12_use_groups.2, start_stay_2yr$cai_12_new.1)
round(prop.table(table(start_stay_2yr$neg_prep12_use_groups.2, start_stay_2yr$cai_12_new.1), margin=1)*100,0)

summary(start_2CAS$N_cai_sps_12.1)
summary(stayoff_2CAS$N_cai_sps_12.1)

### Post-hoc analyses for manuscript -------
round(quantile(all_off$N_cai_sps_12.1, c(.75, .9), na.rm = T))
round(quantile(all_off$N_cai_sps_12.2, c(.75, .9), na.rm = T))

round(quantile(all_start$N_cai_sps_12.1, c(.75, .9), na.rm = T))
round(quantile(all_start$N_cai_sps_12.2, c(.75, .9), na.rm = T))

all_start_2 <- subset(amis_2_two_match, (neg_prep12_use_groups.1 == "01_never-or-no-PrEP12" & neg_prep12_use_groups.2 == "02_PrEPcur"))

nrow(all_off_2)
round(quantile(all_off_2$N_cai_sps_12.1, c(.75, .9), na.rm = T))
round(quantile(all_off_2$N_cai_sps_12.2, c(.75, .9), na.rm = T))

nrow(all_start_2)
round(quantile(all_start_2$N_cai_sps_12.1, c(.75, .9), na.rm = T))
round(quantile(all_start_2$N_cai_sps_12.2, c(.75, .9), na.rm = T))



### Table S1 -----
tableNA(amis$neg_prep12_use_groups)

amis$biomed_4 <- NA
amis$biomed_4[amis$hiv3 == "01_pos"] <- "01_pos"
amis$biomed_4[amis$neg_prep12_use_groups == "02_PrEPcur"] <- "02_negOP"
amis$biomed_4[amis$neg_prep12_use_groups == "01_never-or-no-PrEP12"] <- "03_HNNP"
amis$biomed_4[is.na(amis$neg_prep12_use_groups) & amis$hiv3 != "01_pos"] <- "04_negUNK"

tableNA(amis$biomed_4, amis$hiv3)
tableNA(amis$biomed_4, amis$biomed_current)

### S1, Row 2 - everyone who could be in two survey years -----

#### Match up ID & last ID -----
amis$match_id_1415 <- NA
amis$match_id_1516 <- NA
amis$match_id_1617 <- NA
amis$match_id_1718 <- NA
amis$match_id_1819 <- NA

amis14 <- subset(amis, year == 2014)
amis15 <- subset(amis, year == 2015)
amis16 <- subset(amis, year == 2016)
amis17 <- subset(amis, year == 2017)
amis18 <- subset(amis, year == 2018)
amis19 <- subset(amis, year == 2019)

### match_id_x variable match-up

amis14$match_id_1415 <- amis14$respondent_id
amis15$match_id_1415 <- amis15$lastid2014
tableNA(amis15$lastid2014) #check

amis15$match_id_1516 <- amis15$respondent_id
amis16$match_id_1516 <- amis16$lastid2015
tableNA(amis16$lastid2015) #check

amis16$match_id_1617 <- amis16$respondent_id
amis17$match_id_1617 <- amis17$lastid2016
tableNA(amis17$lastid2016) #check

amis17$match_id_1718 <- amis17$respondent_id
amis18$match_id_1718 <- amis18$lastid2017
tableNA(amis18$lastid2017) #check

amis18$match_id_1819 <- amis18$respondent_id
amis19$match_id_1819 <- amis19$lastid2018
tableNA(amis19$lastid2018) #check

# * Match datasets on "match_id_x" using inner_join -----
amis1415 <- inner_join(amis14,
                       amis15,
                       by = "match_id_1415",
                       copy = F,
                       suffix = c(".14", ".15"),
                       keep = T)

amis1516 <- inner_join(amis15,
                       amis16,
                       by = "match_id_1516",
                       copy = F,
                       suffix = c(".15", ".16"),
                       keep = T)

amis1617 <- inner_join(amis16,
                       amis17,
                       by = "match_id_1617",
                       copy = F,
                       suffix = c(".16", ".17"),
                       keep = T)

amis1718 <- inner_join(amis17,
                       amis18,
                       by = "match_id_1718",
                       copy = F,
                       suffix = c(".17", ".18"),
                       keep = T)

amis1819 <- inner_join(amis18,
                       amis19,
                       by = "match_id_1819",
                       copy = F,
                       suffix = c(".18", ".19"),
                       keep = T)

### Merge all years for combined analyses -----
amis1415_match <- inner_join(amis14,
                             amis15,
                             by = "match_id_1415",
                             copy = F,
                             suffix = c(".1", ".2"),
                             keep = T)

amis1516_match <- inner_join(amis15,
                             amis16,
                             by = "match_id_1516",
                             copy = F,
                             suffix = c(".1", ".2"),
                             keep = T)

amis1617_match <- inner_join(amis16,
                             amis17,
                             by = "match_id_1617",
                             copy = F,
                             suffix = c(".1", ".2"),
                             keep = T)

amis1718_match <- inner_join(amis17,
                             amis18,
                             by = "match_id_1718",
                             copy = F,
                             suffix = c(".1", ".2"),
                             keep = T)

amis1819_match <- inner_join(amis18,
                             amis19,
                             by = "match_id_1819",
                             copy = F,
                             suffix = c(".1", ".2"),
                             keep = T)


# Match up 2-year 14/15 people with 2014 and then 2015...
#amis14$dedup <- amis14$respondent_id
#amis1415_match$dedup <- amis1415_match$match_id_1415.1

amis14_dedup <- amis14
amis14_dedup %<>% filter(!respondent_id %in% amis1415_match$match_id_1415.1)

amis15_dedup <- amis15
amis15_dedup %<>% filter(!respondent_id %in% amis1516_match$match_id_1516.1) #remove those who appear in 15-16
amis15_dedup %<>% filter(!lastid2014 %in% amis1415_match$match_id_1415.1) #remove those who appear in 14-15

amis16_dedup <- amis16
amis16_dedup %<>% filter(!respondent_id %in% amis1617_match$match_id_1617.1) #remove those who appear in 16-17
amis16_dedup %<>% filter(!lastid2015 %in% amis1516_match$match_id_1516.1) #remove those who appear in 15-16

amis17_dedup <- amis17
amis17_dedup %<>% filter(!respondent_id %in% amis1718_match$match_id_1718.1) #remove those who appear in 17-18
amis17_dedup %<>% filter(!lastid2016 %in% amis1617_match$match_id_1617.1) #remove those who appear in 16-18

amis18_dedup <- amis18
amis18_dedup %<>% filter(!respondent_id %in% amis1819_match$match_id_1819.1) #remove those who appear in 18-19
amis18_dedup %<>% filter(!lastid2017 %in% amis1718_match$match_id_1718.1) #remove those who appear in 17-18


amis_dedup <- bind_rows(amis14_dedup,
                     amis15_dedup,
                     amis16_dedup,
                     amis17_dedup,
                     amis18_dedup)


amis_match <- rbind(amis1415_match,
                        amis1516_match,
                        amis1617_match,
                        amis1718_match,
                        amis1819_match)
#
# Prepare to join matched years with non-matched years -----
# 
# # First Year match variable
# amis_match$respondent_id[amis_match$year.1 == 2014] <- amis_match$match_id_1415.1
# amis_match$respondent_id[amis_match$year.1 == 2015] <- amis_match$match_id_1516.1
# amis_match$respondent_id[amis_match$year.1 == 2016] <- amis_match$match_id_1617.1
# amis_match$respondent_id[amis_match$year.1 == 2017] <- amis_match$match_id_1718.1
# amis_match$respondent_id[amis_match$year.1 == 2018] <- amis_match$match_id_1819.1
# 
# #isolate to just those who took surv in 2014:2018
# all_step1 <- subset(amis, year >=2014 & year <= 2018)
# 
# 
# 
# 
# #match observations with first of two-year obs
# y1_match<- merge(x=amis_match, y=s1_row1, by = "respondent_id", all = T)
# 
# #remove those who appear as year 1
# y1_singles <- s1_row1_match[(is.na(y1_match$year.1 )),]
# 
# 
# 
# 
# 
# 
# 
# 
# # Second year match variable
# amis_match$respondent_id[amis_match$year.1 == 2014] <- amis_match$match_id_1415.2
# amis_match$respondent_id[amis_match$year.1 == 2015] <- amis_match$match_id_1516.2
# amis_match$respondent_id[amis_match$year.1 == 2016] <- amis_match$match_id_1617.2
# amis_match$respondent_id[amis_match$year.1 == 2017] <- amis_match$match_id_1718.2
# amis_match$respondent_id[amis_match$year.1 == 2018] <- amis_match$match_id_1819.2
# 
# y2_match <- merge(x=amis_match, y)
# 
# 
# 
# 
# # Second year match variable
# amis_match$respondent_id[amis_match$year.2 == 2015] <- amis_match$lastid2014.2
# amis_match$respondent_id[amis_match$year.2 == 2016] <- amis_match$lastid2015.2
# amis_match$respondent_id[amis_match$year.2 == 2017] <- amis_match$lastid2016.2
# amis_match$respondent_id[amis_match$year.2 == 2018] <- amis_match$lastid2017.2
# amis_match$respondent_id[amis_match$year.2 == 2019] <- amis_match$lastid2018.2
# 
# #tell second year observations to use respondent_id as linkvar2, so they match up
# s1_row1_match2 <- merge(x=amis_match, y=s1_row1_clean, by = "respondent_id", all = T)
# 
# 
# s1_row1_clean2 <- s1_row1_match2[(is.na(s1_row1_match2$year.2.x) & is.na(s1_row1_match2$year.2.y)),]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# round((nrow(s1_row1_clean))/sum(nrow(s1_row1_clean), nrow(amis_match))*100,0)
# round((nrow(amis_match))/sum(nrow(s1_row1_clean), nrow(amis_match))*100,0)

########### Get N & % for table S1 -------------
# age groups
tableNA(amis_dedup$age_25)
round(prop.table(tableNA(amis_dedup$age_25))*100,0)

tableNA(amis_match$age_25.1)
round(prop.table(tableNA(amis_match$age_25.1))*100,0)

chisq.test(cbind(c(12180, 27828), c(710, 2899)))

# race groups
tableNA(amis_dedup$race_cat)
round(prop.table(tableNA(amis_dedup$race_cat))*100,0)

tableNA(amis_match$race_cat.1)
round(prop.table(tableNA(amis_match$race_cat.1))*100,0)

chisq.test(cbind(c(3223, 5903, 27843, 2313), c(231, 405, 2745, 186)))

# income
tableNA(amis_dedup$income_cat)
round(prop.table(tableNA(amis_dedup$income_cat))*100,0)

tableNA(amis_match$income_cat.1)
round(prop.table(tableNA(amis_match$income_cat.1))*100,0)

chisq.test(cbind(c(4981, 6799, 9031, 12474), c(373, 622, 985, 1234)))

# education
tableNA(amis_dedup$educ3)
round(prop.table(tableNA(amis_dedup$educ3))*100,0)

tableNA(amis_match$educ3.1)
round(prop.table(tableNA(amis_match$educ3.1))*100,0)

chisq.test(cbind(c(6868, 13186, 19415), c(260, 1051, 2282)))

# census region
tableNA(amis_dedup$region4)
round(prop.table(tableNA(amis_dedup$region4))*100,0)

tableNA(amis_match$region4.1)
round(prop.table(tableNA(amis_match$region4.1))*100,0)

chisq.test(cbind(c(7275, 8153, 15393, 9187), c(645, 724, 1370, 870)))

# year
tableNA(amis_dedup$year)
round(prop.table(tableNA(amis_dedup$year))*100,0)

tableNA(amis_match$year.1)
round(prop.table(tableNA(amis_match$year.1))*100,0)

chisq.test(cbind(c(45028, 9378, 8614, 8401, 8579), c(55, 795, 1037, 886, 880)))

# NCHS class
tableNA(amis_dedup$nchs4)
round(prop.table(tableNA(amis_dedup$nchs4))*100,0)

tableNA(amis_match$nchs4.1)
round(prop.table(tableNA(amis_match$nchs4.1))*100,0)

chisq.test(cbind(c(15938, 8437, 11877, 3750), c(1613, 711, 1031, 254)))

# CAS, p12
tableNA(amis_dedup$cai_12_new)
round(prop.table(tableNA(amis_dedup$cai_12_new))*100,0)

tableNA(amis_match$cai_12_new.1)
round(prop.table(tableNA(amis_match$cai_12_new.1))*100,0)

chisq.test(cbind(c(13120, 26382), c(946, 2676)))

# HIV/biomed group
tableNA(amis_dedup$biomed_4)
round(prop.table(tableNA(amis_dedup$biomed_4))*100,0)

tableNA(amis_match$biomed_4.1)
round(prop.table(tableNA(amis_match$biomed_4.1))*100,0)

chisq.test(cbind(c(4082, 2504, 33023, 399), c(362, 483, 2687, 77)))

### HOW MANY INDIVIDUAL PEOPLE TOOK AMIS? -----
###### Once and for all, how does the sample break down in terms of single, double, etc.-year respondents? How many unique respondents are there?

amis_study <- subset(amis, year >= 2014 & year <= 2019)

write.csv(amis_study, "amis_dataset_rr.csv")

amis_study$unique_id <- as.numeric(paste(amis_study$respondent_id, amis_study$year, sep = ""))
head(amis_study$unique_id)
class(amis_study$unique_id)

amis_study_14 <- subset(amis_study, year == 2014)
amis_study_15 <- subset(amis_study, year == 2015)
amis_study_16 <- subset(amis_study, year == 2016)
amis_study_17 <- subset(amis_study, year == 2017)
amis_study_18 <- subset(amis_study, year == 2018)
amis_study_19 <- subset(amis_study, year == 2019)

### FIRST: LINK 2-YEAR SETS -----
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

#2014, 2015
amis_study_1415 <- merge(x = amis_study_14, y = amis_study_15, by = "link_1415", all.x = F, all.y = F)

#2015, 2016
amis_study_1516 <- merge(x = amis_study_15, y = amis_study_16, by = "link_1516", all.x = F, all.y = F)

amis_study_1516  <- subset(amis_study_1516 , lastid2015.y != "381" & lastid2015.y != "1508" & lastid2015.y != "25273" & lastid2015.y != "40457")

#2016, 2017
amis_study_1617 <- merge(x = amis_study_16, y = amis_study_17, by = "link_1617", all.x = F, all.y = F)

amis_study_1617 <- subset(amis_study_1617, lastid2016.y != "8675" & lastid2016.y != "11445" & lastid2016.y != "25555")

#2017, 2018
amis_study_1718 <- merge(x = amis_study_17, y = amis_study_18, by = "link_1718", all.x = F, all.y = F)

amis_study_1718 <- subset(amis_study_1718, lastid2017.y != "231793" & lastid2017.y != "268854" & lastid2017.y != "276218" & lastid2017.y != "276619" & lastid2017.y != "291244")

#2018, 2019
amis_study_1819 <- merge(x = amis_study_18, y = amis_study_19, by = "link_1819", all.x = F, all.y = F)

amis_study_1819 <- subset(amis_study_1819, lastid2018.y != "40245" & lastid2018.y != "43269" & lastid2018.y != "47262" & lastid2018.y != "47958" & lastid2018.y != "75913")

amis_study_2_years <- bind_rows(amis_study_1415, 
                            amis_study_1516, 
                            amis_study_1617, 
                            amis_study_1718, 
                            amis_study_1819)

#2014, 2015, 2016
amis_study_1415$link_141516 <- amis_study_1415$respondent_id.y
amis_study_16$link_141516 <- amis_study_16$lastid2015

amis_study_141516 <- merge(x = amis_study_1415, y = amis_study_16, by = "link_141516", all.x = F, all.y = F)

#2014, 2015, 2016, 2017
amis_study_141516$link_14151617 <- amis_study_141516$respondent_id.y
amis_study_17$link_14151617 <- amis_study_17$lastid2016

amis_study_14151617 <- merge(x = amis_study_141516, y = amis_study_17, by = "link_14151617", all.x = F, all.y = F)

#2014, 2015, 2016, 2017, 2018 -- NO OBSERVATIONS
# amis_study_14151617$link_1415161718 <- amis_study_14151617$respondent_id.y
# amis_study_18$link_1415161718 <- amis_study_18$lastid2017
# 
# amis_study_1415161718 <- merge(x = amis_study_14151617, y = amis_study_18, by = "link_1415161718", all.x = F, all.y = F)

#2015, 2016, 2017
amis_study_1516$link_151617 <- amis_study_1516$respondent_id.y
amis_study_17$link_151617 <- amis_study_17$lastid2016

amis_study_151617 <- merge(x = amis_study_1516, y = amis_study_17, by = "link_151617", all.x = F, all.y = F)

# 2015, 2016, 2017, 2018 -- NO OBSERVATIONS
# amis_study_151617$link_15161718 <- amis_study_151617$respondent_id.y
# amis_study_18$link_15161718 <- amis_study_18$lastid2017
# 
# amis_study_15161718 <- merge(x = amis_study_151617, y = amis_study_18, by = "link_15161718", all.x = F, all.y = F)

# 2016, 2017, 2018
amis_study_1617$link_161718 <- amis_study_1617$respondent_id.y
amis_study_18$link_161718 <- amis_study_18$lastid2017

amis_study_161718 <- merge(x = amis_study_1617, y = amis_study_18, by = "link_161718", all.x = F, all.y = F)

# 2016, 2017, 2018, 2019
amis_study_161718$link_16171819 <- amis_study_161718$respondent_id.y
amis_study_19$link_16171819 <- amis_study_19$lastid2018

amis_study_16171819 <- merge(x = amis_study_161718, y = amis_study_19, by = "link_16171819", all.x = F, all.y = F)

# 2017, 2018, 2019
amis_study_1718$link_171819 <- amis_study_1718$respondent_id.y
amis_study_19$link_171819 <- amis_study_19$lastid2018

amis_study_171819 <- merge(x = amis_study_1718, y = amis_study_19, by = "link_171819", all.x = F, all.y = F)

# at the end, remove the 2019-only people.
amis_study <- subset(amis_study, year <= 2018)

# amis_study_1415$unique_identifier <- paste(amis_study_1415$year.x, amis_study_1415$respondent_id.x, sep = "")
# amis_study_1516$unique_identifier <- paste(amis_study_1516$year.x, amis_study_1516$respondent_id.x, sep = "")
# amis_study_1617$unique_identifier <- paste(amis_study_1617$year.x, amis_study_1617$respondent_id.x, sep = "")
# amis_study_1718$unique_identifier <- paste(amis_study_1718$year.x, amis_study_1718$respondent_id.x, sep = "")
# amis_study_1819$unique_identifier <- paste(amis_study_1819$year.x, amis_study_1819$respondent_id.x, sep = "")
# amis_study_141516$unique_identifier <- paste(amis_study_141516$year.y, amis_study_141516$respondent_id.y, sep = "")
# 

# 
# remove_obs<- c(amis_study_1415$unique_identifier, amis_study_1516$unique_identifier, amis_study_1617$unique_identifier, amis_study_1718$unique_identifier, amis_study_1819$unique_identifier)
# 
# amis_1yr_unique1 <- amis_study[! amis_study$unique_identifier %in% (c(amis_study_1415$unique_identifier)),]
# amis_1yr_unique2 <- amis_1yr_unique1[! amis_1yr_unique1$unique_identifier %in% (c(amis_study_1516$unique_identifier)),]
# amis_1yr_unique3 <- amis_1yr_unique2[! amis_1yr_unique2$unique_identifier %in% (c(amis_study_1617$unique_identifier)),]
# amis_1yr_unique4 <- amis_1yr_unique3[! amis_1yr_unique3$unique_identifier %in% (c(amis_study_1718$unique_identifier)),]
# amis_1yr_unique <- amis_1yr_unique4[! amis_1yr_unique4$unique_identifier %in% (c(amis_study_1819$unique_identifier)),]

amis_study$unique_identifier <- paste(amis_study$year, amis_study$respondent_id, sep = "")



#obtain unique identifiers for each individual observation for later match and removal, and remove the dups
amis_study_1415$unique_id_14 <- paste(amis_study_1415$year.x, amis_study_1415$respondent_id.x, sep = "") 
amis_study_1415$unique_id_15 <- paste(amis_study_1415$year.y, amis_study_1415$respondent_id.y, sep = "")
amis_study_1516$unique_id_15 <- paste(amis_study_1516$year.x, amis_study_1516$respondent_id.x, sep = "")
amis_study_1516$unique_id_16 <- paste(amis_study_1516$year.y, amis_study_1516$respondent_id.y, sep = "")
amis_study_1617$unique_id_16 <- paste(amis_study_1617$year.x, amis_study_1617$respondent_id.x, sep = "")
amis_study_1617$unique_id_17 <- paste(amis_study_1617$year.y, amis_study_1617$respondent_id.y, sep = "")
amis_study_1718$unique_id_17 <- paste(amis_study_1718$year.x, amis_study_1718$respondent_id.x, sep = "")
amis_study_1718$unique_id_18 <- paste(amis_study_1718$year.y, amis_study_1718$respondent_id.y, sep = "")
amis_study_1819$unique_id_18 <- paste(amis_study_1819$year.x, amis_study_1819$respondent_id.x, sep = "")
amis_study_1819$unique_id_19 <- paste(amis_study_1819$year.y, amis_study_1819$respondent_id.y, sep = "")
amis_study_141516$unique_id_16 <- paste("2016", amis_study_141516$lastid2015, sep = "")
amis_study_14151617$unique_id_17 <- "201723209"
amis_study_151617$unique_id_17 <- paste("2017", amis_study_151617$lastid2016, sep = "")
amis_study_161718$unique_id_18 <- paste("2018", amis_study_161718$lastid2017, sep = "")

remove_obs <- c(amis_study_1415$unique_id_14,
                amis_study_1415$unique_id_15,
                amis_study_1516$unique_id_15,
                amis_study_1516$unique_id_16,
                amis_study_1617$unique_id_16,
                amis_study_1617$unique_id_17,
                amis_study_1718$unique_id_17,
                amis_study_1718$unique_id_18,
                amis_study_1819$unique_id_18,
                amis_study_1819$unique_id_19,
                amis_study_141516$unique_id_16,
                amis_study_14151617$unique_id_17,
                amis_study_151617$unique_id_17,
                amis_study_161718$unique_id_18)

amis_study <- subset(amis_study, !(lastid2015 %in% c("381", "1508", "25273", "40457")) & !(lastid2016 %in% c("8675", "11445", "25555")) & !(lastid2017 %in% c("231793", "268854", "276218", "276619", "291244")) & !(lastid2018 %in% c("40245", "43269", "47262", "47958", "75913")))


amis_study_unique <- subset(amis_study, !(unique_identifier %in% remove_obs))

# uncomment when ready to begin Table S2
# write.csv(amis_study_unique, "amis_1418_new.csv")
# write.csv(amis_study_2_years, "amis_2_year_new.csv")

# Of all not HIV-pos MSM in 2019....

# 1: had no AI in the last year?
neg19_prep <- subset(neg19, sf1_group == "02_PrEPcur")
neg19_noprep <- subset(neg19, sf1_group == "01_HNNP")
neg19_unkprep <- subset(neg19, sf1_group == "04_NegUNK")

tableNA(neg19_prep$ai_12)
tableNA(neg19_noprep$ai_12)
tableNA(neg19_unkprep$ai_12)

neg19_prep_ai <- subset(neg19_prep, ai_12 == 1)
neg19_noprep_ai <- subset(neg19_noprep, ai_12 == 1)
neg19_unkprep_ai <- subset(neg19_unkprep, ai_12 == 1)

# 2: used condoms for 100% of AI?
tableNA(neg19_prep_ai$cai_12_new)
tableNA(neg19_noprep_ai$cai_12_new)
tableNA(neg19_unkprep_ai$cai_12_new)

# 3: used condoms for >0%, <100% of AI?
neg19_prep_cai <- subset(neg19_prep, cai_12_new == 1)
neg19_noprep_cai <- subset(neg19_noprep, cai_12_new == 1)
neg19_unkprep_cai <- subset(neg19_unkprep, cai_12_new == 1)



# 4: never used condoms for AI?
neg19_prep_cai$m_mp12anum[neg19_prep_cai$m_mp12oanum == 1] <- 1
neg19_noprep_cai$m_mp12anum[neg19_noprep_cai$m_mp12oanum == 1] <- 1
neg19_unkprep_cai$m_mp12anum[neg19_unkprep_cai$m_mp12oanum == 1] <- 1

neg19_prep_cai$N_ai_sps_12 <- ifelse(!is.na(neg19_prep_cai$m_mp12manum), neg19_prep_cai$m_mp12manum, neg19_prep_cai$m_mp12anum)
neg19_noprep_cai$N_ai_sps_12 <- ifelse(!is.na(neg19_noprep_cai$m_mp12manum), neg19_noprep_cai$m_mp12manum, neg19_noprep_cai$m_mp12anum)
neg19_unkprep_cai$N_ai_sps_12 <- ifelse(!is.na(neg19_unkprep_cai$m_mp12manum), neg19_unkprep_cai$m_mp12manum, neg19_unkprep_cai$m_mp12anum)

round(prop.table(tableNA(neg19_prep_cai$N_ai_sps_12))*100,0)
round(prop.table(tableNA(neg19_noprep_cai$N_ai_sps_12))*100,0)
round(prop.table(tableNA(neg19_unkprep_cai$N_ai_sps_12))*100,0)

neg19_prep_cai$condom_use_cat <- NA
neg19_prep_cai$condom_use_cat[is.na(neg19_prep_cai$N_ai_sps_12)] <- "unk"
neg19_prep_cai$condom_use_cat[is.na(neg19_prep_cai$N_cai_sps_12)] <- "unk"
neg19_prep_cai$condom_use_cat[neg19_prep_cai$N_ai_sps_12 > neg19_prep_cai$N_cai_sps_12] <- "some"
neg19_prep_cai$condom_use_cat[neg19_prep_cai$N_ai_sps_12 == neg19_prep_cai$N_cai_sps_12] <- "all"

neg19_noprep_cai$condom_use_cat <- NA
neg19_noprep_cai$condom_use_cat[is.na(neg19_noprep_cai$N_ai_sps_12)] <- "unk"
neg19_noprep_cai$condom_use_cat[is.na(neg19_noprep_cai$N_cai_sps_12)] <- "unk"
neg19_noprep_cai$condom_use_cat[neg19_noprep_cai$N_ai_sps_12 > neg19_noprep_cai$N_cai_sps_12] <- "some"
neg19_noprep_cai$condom_use_cat[neg19_noprep_cai$N_ai_sps_12 == neg19_noprep_cai$N_cai_sps_12] <- "all"

neg19_unkprep_cai$condom_use_cat <- NA
neg19_unkprep_cai$condom_use_cat[is.na(neg19_unkprep_cai$N_ai_sps_12)] <- "unk"
neg19_unkprep_cai$condom_use_cat[is.na(neg19_unkprep_cai$N_cai_sps_12)] <- "unk"
neg19_unkprep_cai$condom_use_cat[neg19_unkprep_cai$N_ai_sps_12 > neg19_unkprep_cai$N_cai_sps_12] <- "some"
neg19_unkprep_cai$condom_use_cat[neg19_unkprep_cai$N_ai_sps_12 == neg19_unkprep_cai$N_cai_sps_12] <- "all"

tableNA(neg19_prep_cai$condom_use_cat)
tableNA(neg19_noprep_cai$condom_use_cat)
tableNA(neg19_unkprep_cai$condom_use_cat)


# Table S4: -----
# Measures of the distribution of number of condomless anal sex partners among respondents who stayed off 
# pre-exposure prophylaxis (PrEP) for two surveys, and reported at least one condomless anal sex 
# partner in the last 12 months at each survey, American Men’s Internet Survey 2014-2019 (N=836)

s4 <- subset(amis, N_cai_sps_12 >= 1
             & biomed_4 == "03_HNNP")

#### Match up ID & last ID
s4$match_id_1415 <- NA
s4$match_id_1516 <- NA
s4$match_id_1617 <- NA
s4$match_id_1718 <- NA
s4$match_id_1819 <- NA

s4_14 <- subset(s4, year == 2014)
s4_15 <- subset(s4, year == 2015)
s4_16 <- subset(s4, year == 2016)
s4_17 <- subset(s4, year == 2017)
s4_18 <- subset(s4, year == 2018)
s4_19 <- subset(s4, year == 2019)

### match_id_x variable match-up

s4_14$match_id_1415 <- s4_14$respondent_id
s4_15$match_id_1415 <- s4_15$lastid2014
tableNA(s4_15$lastid2014) #check

s4_15$match_id_1516 <- s4_15$respondent_id
s4_16$match_id_1516 <- s4_16$lastid2015
tableNA(s4_16$lastid2015) #check

s4_16$match_id_1617 <- s4_16$respondent_id
s4_17$match_id_1617 <- s4_17$lastid2016
tableNA(s4_17$lastid2016) #check

s4_17$match_id_1718 <- s4_17$respondent_id
s4_18$match_id_1718 <- s4_18$lastid2017
tableNA(s4_18$lastid2017) #check

s4_18$match_id_1819 <- s4_18$respondent_id
s4_19$match_id_1819 <- s4_19$lastid2018
tableNA(s4_19$lastid2018) #check

# * Match datasets on "match_id_x" using inner_join -----
s4_1415 <- inner_join(s4_14,
                         s4_15,
                         by = "match_id_1415",
                         copy = F,
                         suffix = c(".14", ".15"),
                         keep = T)

s4_1516 <- inner_join(s4_15,
                         s4_16,
                         by = "match_id_1516",
                         copy = F,
                         suffix = c(".15", ".16"),
                         keep = T)

s4_1617 <- inner_join(s4_16,
                         s4_17,
                         by = "match_id_1617",
                         copy = F,
                         suffix = c(".16", ".17"),
                         keep = T)

s4_1718 <- inner_join(s4_17,
                         s4_18,
                         by = "match_id_1718",
                         copy = F,
                         suffix = c(".17", ".18"),
                         keep = T)

s4_1819 <- inner_join(s4_18,
                         s4_19,
                         by = "match_id_1819",
                         copy = F,
                         suffix = c(".18", ".19"),
                         keep = T)

### Merge all years for combined analyses -----
s4_1415_match <- inner_join(s4_14,
                               s4_15,
                               by = "match_id_1415",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

s4_1516_match <- inner_join(s4_15,
                               s4_16,
                               by = "match_id_1516",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

s4_1617_match <- inner_join(s4_16,
                               s4_17,
                               by = "match_id_1617",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

s4_1718_match <- inner_join(s4_17,
                               s4_18,
                               by = "match_id_1718",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

s4_1819_match <- inner_join(s4_18,
                               s4_19,
                               by = "match_id_1819",
                               copy = F,
                               suffix = c(".1", ".2"),
                               keep = T)

s4__two_match <- rbind(s4_1415_match,
                          s4_1516_match,
                          s4_1617_match,
                          s4_1718_match,
                          s4_1819_match)
nrow(s4__two_match)

tableNA(s4__two_match$N_cai_sps_12.1)
tableNA(s4__two_match$N_cai_sps_12.2)
tableNA(s4__two_match$cai_12_new.1)

s4_final <- s4__two_match[,colSums(is.na(s4__two_match))<nrow(s4__two_match)]
s4_data <- s4_final

# * Col (1) - Get Ns ---------
nrow(s4_data)

nrow(s4_data[s4_data$age_25.1 == 0,])
nrow(s4_data[s4_data$age_25.1 == 1,])

nrow(s4_data[!is.na(s4_data$race_cat.1),])
nrow(s4_data[s4_data$race_cat.1 =="NH_Black",])
nrow(s4_data[s4_data$race_cat.1 =="Hispanic",])
nrow(s4_data[s4_data$race_cat.1 =="NH_White",])
nrow(s4_data[s4_data$race_cat.1 =="NH_Other",])

nrow(s4_data[!is.na(s4_data$income_cat.1),])
nrow(s4_data[s4_data$income_cat.1 == "1_<20k",])
nrow(s4_data[s4_data$income_cat.1 == "2_<40k",])
nrow(s4_data[s4_data$income_cat.1 == "3_<75k",])
nrow(s4_data[s4_data$income_cat.1 == "4_75k+",])

nrow(s4_data[!is.na(s4_data$region4.1),])
nrow(s4_data[s4_data$region4.1 == "1_NorE",])
nrow(s4_data[s4_data$region4.1 == "2_MidW",])
nrow(s4_data[s4_data$region4.1 == "3_South",])
nrow(s4_data[s4_data$region4.1 == "4_West",])

nrow(s4_data[!is.na(s4_data$nchs4.1),])
nrow(s4_data[s4_data$nchs4.1 == "1_LgCnt",])
nrow(s4_data[s4_data$nchs4.1 == "2_LgFrg",])
nrow(s4_data[s4_data$nchs4.1 == "3_Metro",])
nrow(s4_data[s4_data$nchs4.1 == "4_Rural",])

# Col (2) Wilcoxon tests and p-values ------
s4_test_all.1 <- c((s4_data$N_cai_sps_12.1))
s4_test_all.2 <- c((s4_data$N_cai_sps_12.2))

wilcox.test(s4_test_all.2, s4_test_all.1, paired=TRUE, 
            alternative = "greater", exact=F)

#age
s4_test_u25.1 <- c((s4_data[s4_data$age_25.1 == 0,]$N_cai_sps_12.1))
s4_test_u25.2 <- c((s4_data[s4_data$age_25.1 == 0,]$N_cai_sps_12.2))

s4_test_o25.1 <- c((s4_data[s4_data$age_25.1 == 1,]$N_cai_sps_12.1))
s4_test_o25.2 <- c((s4_data[s4_data$age_25.1 == 1,]$N_cai_sps_12.2))

wilcox.test(s4_test_u25.2, s4_test_u25.1, paired=TRUE, 
            alternative = "greater", exact=F)
wilcox.test(s4_test_o25.2, s4_test_o25.1, paired=TRUE, 
            alternative = "greater", exact=F)

#race
s4_test_b.1 <- c((s4_data[s4_data$race_cat.1 == "NH_Black",]$N_cai_sps_12.1))
s4_test_b.2 <- c((s4_data[s4_data$race_cat.1 == "NH_Black",]$N_cai_sps_12.2))
wilcox.test(s4_test_b.2, s4_test_b.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_h.1 <- c((s4_data[s4_data$race_cat.1 == "Hispanic",]$N_cai_sps_12.1))
s4_test_h.2 <- c((s4_data[s4_data$race_cat.1 == "Hispanic",]$N_cai_sps_12.2))
wilcox.test(s4_test_h.2, s4_test_h.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_w.1 <- c((s4_data[s4_data$race_cat.1 == "NH_White",]$N_cai_sps_12.1))
s4_test_w.2 <- c((s4_data[s4_data$race_cat.1 == "NH_White",]$N_cai_sps_12.2))
wilcox.test(s4_test_w.2, s4_test_w.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_o.1 <- c((s4_data[s4_data$race_cat.1 == "NH_Other",]$N_cai_sps_12.1))
s4_test_o.2 <- c((s4_data[s4_data$race_cat.1 == "NH_Other",]$N_cai_sps_12.2))
wilcox.test(s4_test_o.2, s4_test_o.1, paired=TRUE, 
            alternative = "greater", exact=F)

# income
s4_test_inc1.1 <- c((s4_data[s4_data$income_cat.1 == "1_<20k",]$N_cai_sps_12.1))
s4_test_inc1.2 <- c((s4_data[s4_data$income_cat.1 == "1_<20k",]$N_cai_sps_12.2))
wilcox.test(s4_test_inc1.2, s4_test_inc1.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_inc2.1 <- c((s4_data[s4_data$income_cat.1 == "2_<40k",]$N_cai_sps_12.1))
s4_test_inc2.2 <- c((s4_data[s4_data$income_cat.1 == "2_<40k",]$N_cai_sps_12.2))
wilcox.test(s4_test_inc2.2, s4_test_inc2.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_inc3.1 <- c((s4_data[s4_data$income_cat.1 == "3_<75k",]$N_cai_sps_12.1))
s4_test_inc3.2 <- c((s4_data[s4_data$income_cat.1 == "3_<75k",]$N_cai_sps_12.2))
wilcox.test(s4_test_inc3.2, s4_test_inc3.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_inc4.1 <- c((s4_data[s4_data$income_cat.1 == "4_75k+",]$N_cai_sps_12.1))
s4_test_inc4.2 <- c((s4_data[s4_data$income_cat.1 == "4_75k+",]$N_cai_sps_12.2))
wilcox.test(s4_test_inc4.2, s4_test_inc4.1, paired=TRUE, 
            alternative = "greater", exact=F)

# education
s4_test_ed1.1 <- c((s4_data[s4_data$educ3.1 == "1_HS-or-less",]$N_cai_sps_12.1))
s4_test_ed1.2 <- c((s4_data[s4_data$educ3.1 == "1_HS-or-less",]$N_cai_sps_12.2))
wilcox.test(s4_test_ed1.2, s4_test_ed1.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_ed2.1 <- c((s4_data[s4_data$educ3.1 == "2_ltCol",]$N_cai_sps_12.1))
s4_test_ed2.2 <- c((s4_data[s4_data$educ3.1 == "2_ltCol",]$N_cai_sps_12.2))
wilcox.test(s4_test_ed2.2, s4_test_ed2.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_ed3.1 <- c((s4_data[s4_data$educ3.1 == "3_Col+",]$N_cai_sps_12.1))
s4_test_ed3.2 <- c((s4_data[s4_data$educ3.1 == "3_Col+",]$N_cai_sps_12.2))
wilcox.test(s4_test_ed3.2, s4_test_ed3.1, paired=TRUE, 
            alternative = "greater", exact=F)

# region
s4_test_reg1.1 <- c((s4_data[s4_data$region4.1 == "1_NorE",]$N_cai_sps_12.1))
s4_test_reg1.2 <- c((s4_data[s4_data$region4.1 == "1_NorE",]$N_cai_sps_12.2))
wilcox.test(s4_test_reg1.2, s4_test_reg1.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_reg2.1 <- c((s4_data[s4_data$region4.1 == "2_MidW",]$N_cai_sps_12.1))
s4_test_reg2.2 <- c((s4_data[s4_data$region4.1 == "2_MidW",]$N_cai_sps_12.2))
wilcox.test(s4_test_reg2.2, s4_test_reg2.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_reg3.1 <- c((s4_data[s4_data$region4.1 == "3_South",]$N_cai_sps_12.1))
s4_test_reg3.2 <- c((s4_data[s4_data$region4.1 == "3_South",]$N_cai_sps_12.2))
wilcox.test(s4_test_reg3.2, s4_test_reg3.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_reg4.1 <- c((s4_data[s4_data$region4.1 == "4_West",]$N_cai_sps_12.1))
s4_test_reg4.2 <- c((s4_data[s4_data$region4.1 == "4_West",]$N_cai_sps_12.2))
wilcox.test(s4_test_reg4.2, s4_test_reg4.1, paired=TRUE, 
            alternative = "greater", exact=F)

# region
s4_test_den1.1 <- c((s4_data[s4_data$nchs4.1 == "1_LgCnt",]$N_cai_sps_12.1))
s4_test_den1.2 <- c((s4_data[s4_data$nchs4.1 == "1_LgCnt",]$N_cai_sps_12.2))
wilcox.test(s4_test_den1.2, s4_test_den1.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_den2.1 <- c((s4_data[s4_data$nchs4.1 == "2_LgFrg",]$N_cai_sps_12.1))
s4_test_den2.2 <- c((s4_data[s4_data$nchs4.1 == "2_LgFrg",]$N_cai_sps_12.2))
wilcox.test(s4_test_den2.2, s4_test_den2.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_den3.1 <- c((s4_data[s4_data$nchs4.1 == "3_Metro",]$N_cai_sps_12.1))
s4_test_den3.2 <- c((s4_data[s4_data$nchs4.1 == "3_Metro",]$N_cai_sps_12.2))
wilcox.test(s4_test_den3.2, s4_test_den3.1, paired=TRUE, 
            alternative = "greater", exact=F)

s4_test_den4.1 <- c((s4_data[s4_data$nchs4.1 == "4_Rural",]$N_cai_sps_12.1))
s4_test_den4.2 <- c((s4_data[s4_data$nchs4.1 == "4_Rural",]$N_cai_sps_12.2))
wilcox.test(s4_test_den4.2, s4_test_den4.1, paired=TRUE, 
            alternative = "greater", exact=F)

# Col (3:8) --- Median N CAS SPs
round(quantile(s4_data$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# age
round(quantile(s4_data[s4_data$age_25.1 == 0,]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$age_25.1 == 0,]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$age_25.1 == 1,]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$age_25.1 == 1,]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# race
round(quantile(s4_data[s4_data$race_cat.1 == "NH_Black",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$race_cat.1 == "NH_Black",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$race_cat.1 == "Hispanic",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$race_cat.1 == "Hispanic",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$race_cat.1 == "NH_White",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$race_cat.1 == "NH_White",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$race_cat.1 == "NH_Other",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$race_cat.1 == "NH_Other",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# income
round(quantile(s4_data[s4_data$income_cat.1 == "1_<20k",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$income_cat.1 == "1_<20k",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$income_cat.1 == "2_<40k",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$income_cat.1 == "2_<40k",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$income_cat.1 == "3_<75k",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$income_cat.1 == "3_<75k",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$income_cat.1 == "4_75k+",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$income_cat.1 == "4_75k+",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# education
round(quantile(s4_data[s4_data$educ3.1 == "1_HS-or-less",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$educ3.1 == "1_HS-or-less",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$educ3.1 == "2_ltCol",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$educ3.1 == "2_ltCol",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$educ3.1 == "3_Col+",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$educ3.1 == "3_Col+",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# region
round(quantile(s4_data[s4_data$region4.1 == "1_NorE",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$region4.1 == "1_NorE",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$region4.1 == "2_MidW",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$region4.1 == "2_MidW",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$region4.1 == "3_South",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$region4.1 == "3_South",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$region4.1 == "4_West",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$region4.1 == "4_West",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

# density
round(quantile(s4_data[s4_data$nchs4.1 == "1_LgCnt",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$nchs4.1 == "1_LgCnt",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$nchs4.1 == "2_LgFrg",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$nchs4.1 == "2_LgFrg",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$nchs4.1 == "3_Metro",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$nchs4.1 == "3_Metro",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

round(quantile(s4_data[s4_data$nchs4.1 == "4_Rural",]$N_cai_sps_12.1, c(.75, .9, .95), na.rm = T))
round(quantile(s4_data[s4_data$nchs4.1 == "4_Rural",]$N_cai_sps_12.2, c(.75, .9, .95), na.rm = T))

### Figure 2 ------
# Percent reporting condomless anal sex in the last twelve months among HIV-negative 
# respondents who have not used pre-exposure prophylaxis (PrEP) in the last twelve
# months, American Men’s Internet Survey, 2014-2019

nrow(amis)

f2 <- subset(amis, biomed_4 == "03_HNNP" & !is.na(cai_12_new) & year >=2014)


round(prop.table(tableNA(f2$year, f2$cai_12_new),margin = 1)*100,2)[,2]

round(prop.table(tableNA(f2[f2$age_25 == 0,]$year, f2[f2$age_25 == 0,]$cai_12_new),margin = 1)*100,2)[,2]
round(prop.table(tableNA(f2[f2$age_25 == 1,]$year, f2[f2$age_25 == 1,]$cai_12_new),margin = 1)*100,2)[,2]

round(prop.table(tableNA(f2[f2$race_cat == "NH_Black",]$year, f2[f2$race_cat == "NH_Black",]$cai_12_new),margin = 1)*100,2)[,2]
round(prop.table(tableNA(f2[f2$race_cat == "Hispanic",]$year, f2[f2$race_cat == "Hispanic",]$cai_12_new),margin = 1)*100,2)[,2]
round(prop.table(tableNA(f2[f2$race_cat == "NH_White",]$year, f2[f2$race_cat == "NH_White",]$cai_12_new),margin = 1)*100,2)[,2]
round(prop.table(tableNA(f2[f2$race_cat == "NH_Other",]$year, f2[f2$race_cat == "NH_Other",]$cai_12_new),margin = 1)*100,2)[,2]
