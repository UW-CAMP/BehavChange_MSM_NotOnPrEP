####################################################################
# Master script for the paper: Goodreau SM, Barry MP, Hamilton DT,
#     Williams AM, Wang L, Sanchez TH, Katz DA, Delaney KP,
#     Behavior Change Among HIV-Negative Men Who Have Sex with Men 
#     Not Using PrEP in the United States. AIDS and Behavior 28, 
#     1766–1780 (2024).
#
#  Be sure to read the README file first for instructions on 
#     obtaining data

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

### Analyses for A&B initial submission
source("01_Initial_submission_data_prep_and_exploratory_analyses.R") # This script prepares data and runs initial analyses
source("02_Initial_submission_final_analyses.R") # This script runs final analyses

### October 2023 R&R Analyses
source("03_R&R_data_prep.R") # This script prepares "comparison data sets" in response to R&R request
source("04_R&R_analysis.R") # This script runs analyses corresponding to the R+R comparison efforts