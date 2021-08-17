##This script is based on LoadCleanExplore, using the data objects loaded from that script. 
##The purpose of this script is to set up feature engineering in PNC for machine learning algorithms

library(tidymodels) # for modeling
library(tidyverse) # for general data wrangling
library(purrr)  # for iteration using map() and walk() variants
library(kableExtra) # for displaying formatted tables w/ kbl()
library(janitor)  # for clean_names(), tabyl()
library(skimr) # for skim()

library(cowplot) # for plot_grid(), theme_half_open()
theme_set(theme_half_open())
source("fun_modeling.R")
##quick skim of data
Merged_PNC %>%
  glimpse()

Merged_PNC %>%
  select(ends_with("DX"),contains("PGS"), Psychosis ) %>%
  skim_some()
####split data into training and test####
set.seed(123456)
splits_pnc <- Merged_PNC %>%
  initial_split(prop = 3/4, strata = "DEP_DX")


##if using machine learning, then need to split the traning/test sample by each disorder, and also need to test each PGS separately, in each cohort. 
##Too many models