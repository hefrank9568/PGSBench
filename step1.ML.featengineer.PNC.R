##This script is based on LoadCleanExplore, using the data objects loaded from that script. 
##The purpose of this script is to set up feature engineering in PNC for machine learning algorithms

library(tidymodels) # for modeling
library(tidyverse) # for general data wrangling
library(purrr)  # for iteration using map() and walk() variants
library(kableExtra) # for displaying formatted tables w/ kbl()
library(janitor)  # for clean_names(), tabyl()
library(skimr) # for skim()
library(doParallel)

library(cowplot) # for plot_grid(), theme_half_open()
theme_set(theme_half_open())
source("fun_modeling.R")

###parallel processing
set.seed(123456)
cl <- makeCluster(5, setup_timeout = 0.5)
registerDoParallel(cl)

##initial split into training and test
splits <- Merged_PNC %>% 
  select(ODD_DX,Race,SUBJID,Med_Rate,all_of(PC),Sex,GSEM_ADHDPGS,PGS_Trad_ADHD) %>%
  rsample::initial_split(prop = 0.75, strata = "ODD_DX")

data_trn <- analysis(splits)
data_test <- assessment(splits)

##quick skim of data
data_trn %>%
  skim_some()

####set up recipes ####
rec <- recipe(ODD_DX ~ ., data = data_trn) %>%
  step_rm(SUBJID, GSEM_ADHDPGS, Med_Rate) %>%
  step_num2factor(ODD_DX, levels = c("No", "Yes"),
                  transform = function(x) x+1) %>%
  step_medianimpute(all_numeric()) %>%
  step_normalize(all_predictors())

feat_trn <- rec %>% 
  make_features(data_trn)

feat_test <- rec %>% 
  make_features(data_test, glimpse_it = FALSE)

####resampling set up####
splits_boot <- data_trn %>% 
  bootstraps(times = 100, strata = "ODD_DX")  
##set up grid to include penalty
grid_penalty <- expand_grid(penalty = exp(seq(-4, 4, length.out = 500)))

##LASSO
fits_lasso <-
  logistic_reg(penalty = tune(), 
             mixture = 1) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification") %>%
  tune_grid(preprocessor = rec, 
            resamples = splits_boot, grid = grid_penalty, 
            metrics = metric_set(roc_auc))

plot_hyperparameters(fits_lasso, hp1 = "penalty", metric = "roc_auc")

metrics_lasso <- collect_metrics(fits_lasso, summarize = FALSE)
metrics_lasso %>% print_kbl()
##glmnet
grid_glmnet <- expand_grid(penalty = exp(seq(-8, 3, length.out = 200)),
                           mixture = seq(0, 1, length.out = 6))
fits_glmnet <-
  logistic_reg(penalty = tune(), 
               mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification") %>% 
  tune_grid(preprocessor = rec, 
            resamples = splits_boot, grid = grid_glmnet, 
            metrics = metric_set(roc_auc))

plot_hyperparameters(fits_glmnet, hp1 = "penalty", hp2 = "mixture", metric = "roc_auc", log_hp1 = TRUE)

metrics_glmnet <- collect_metrics(fits_glmnet, summarize = FALSE)
metrics_glmnet %>% show_best()


###fit the best model in full training data : assume that the best is glmnet, and evaluate accuracy in test data
fit_glmnet <- 
  logistic_reg(penalty = select_best(fits_glmnet)$penalty,
               mixture = select_best(fits_glmnet)$mixture) %>%
  set_engine("glmnet") %>%
  fit(ODD_DX ~., data = feat_trn)

##get parameter estimates
fit_glmnet %>%
  tidy() %>%
  print()

accuracy_vec(feat_test$ODD_DX, predict(fit_glmnet, feat_test)$.pred_class)

roc_auc_vec(feat_test$ODD_DX, ifelse(predict(fit_glmnet, feat_test)$.pred_class=="No",0,1))

####next: get compact model (use feature ablation), compare with full model; 
#####tune for hyperparameters 
#####fit the tuned hyperparameters in full training dataset
#####evaluate fitted model in test dataset

####look into linear regression as well (compare models using rsq and rmse?)

