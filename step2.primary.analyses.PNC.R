
library(pROC)
####----------PNC------------####
####Descriptives####
table(Merged_PNC$Race)
##AA: 2719; EA: 4789

####Diagnoses as outcome####
Merged_PNC %>%
  select(ends_with("_DX"), EAT_ANDX, Psychosis, starts_with("SEP") & ends_with("DX")) %>%
  names()
##use coalesce function to create a new SEP_DX variable that represents having any SEP diagnosis ever
Merged_PNC <- Merged_PNC %>%
  mutate(SEP_DX = coalesce(SEP_CDX,SEP_ALDX,SEP_ATDX))

##create Psychosis Diagnosis variable
Merged_PNC <- Merged_PNC %>%
  mutate(PSY_DX = ifelse(Psychosis >=1, 1, 0))

Merged_PNC$PGS_Trad_ADHD
Merged_PNC$GSEM_ADHDPGS

###set up data names###
PGS_PNC <- c("PGS_Trad_ADHD","GSEM_ADHDPGS")
Disorder_PNC <- Merged_PNC %>%
  select(ends_with("_DX"), EAT_ANDX, PSY_DX) %>%
  names()
dataName_pnc <- expand.grid(PGS_PNC, Disorder_PNC)
####All subjects, adding interaction with Race and PC1, PC2####
all_cov_pnc <- lapply(1:nrow(dataName_pnc), function(x) glm(as.formula(paste0(dataName_pnc[x,"Var2"], "~",
                                                                              "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                              sep = "")), 
                                                            family = "binomial",
                                                            data = Merged_PNC))
all_pnc <- lapply(1:nrow(dataName_pnc), function(x) glm(as.formula(paste0(dataName_pnc[x,"Var2"], "~", 
                                                                         paste0(dataName_pnc[x,"Var1"], "*Race+"), 
                                                                         paste0(dataName_pnc[x,"Var1"], "*PC1+"), 
                                                                         paste0(dataName_pnc[x,"Var1"], "*PC2"), 
                                                                         "+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", sep = "")), 
                                                        family = "binomial",
                                                        data = Merged_PNC))
R2_all_pnc = lapply(all_pnc, performance::r2)
R2_cov_pnc = lapply(all_cov_pnc, performance::r2)
R2_diff_pnc = unname(unlist(R2_all_pnc)) - unname(unlist(R2_cov_pnc))
plot(R2_diff_pnc)

summaries_pncall <- lapply(all_pnc, summary)


summaries_pncall[1]
table(Merged_PNC$ADD_DX)
####in EA####
EA_cov_pnc <- lapply(1:nrow(dataName_pnc), function(x) glm(as.formula(paste0(dataName_pnc[x,"Var2"], "~",
                                                                            "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                            sep = "")), 
                                                           family = "binomial",
                                                           data = Merged_PNC[Merged_PNC$Race==1,]))
EA_pnc <- lapply(1:nrow(dataName_pnc), function(x) glm(as.formula(paste0(dataName_pnc[x,"Var2"], "~", 
                                                                         dataName_pnc[x,"Var1"],
                                                                         "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                        sep = "")), 
                                                       family = "binomial",
                                                       data = Merged_PNC[Merged_PNC$Race==1,]))

R2_EA_pnc = lapply(EA_pnc, performance::r2)
R2_EA_cov_pnc = lapply(EA_cov_pnc, performance::r2)
R2_diff_EA_pnc = unname(unlist(R2_EA_pnc)) - unname(unlist(R2_EA_cov_pnc))
plot(R2_diff_EA_pnc)

summaries_pncEA <- lapply(EA_pnc, summary)
####in AA####
AA_cov_pnc <- lapply(1:nrow(dataName_pnc), function(x) glm(as.formula(paste0(dataName_pnc[x,"Var2"], "~",
                                                                            "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                            sep = "")), 
                                                           family = "binomial",
                                                           data = Merged_PNC[Merged_PNC$Race==0,]))
AA_pnc <- lapply(1:nrow(dataName_pnc), function(x) glm(as.formula(paste0(dataName_pnc[x,"Var2"], "~", 
                                                                        dataName_pnc[x,"Var1"],
                                                                        "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                        sep = "")), 
                                                       family = "binomial",
                                                       data = Merged_PNC[Merged_PNC$Race==0,]))
R2_AA_pnc = lapply(AA_pnc, performance::r2)
R2_AA_cov_pnc = lapply(AA_cov_pnc, performance::r2)
R2_diff_AA_pnc = unname(unlist(R2_AA_pnc)) - unname(unlist(R2_AA_cov_pnc))
plot(R2_diff_AA_pnc)

summaries_pncAA <- lapply(AA_pnc, summary)

##run ROC and AUC for each outcome 

Merged_PNC %>%
  roc("ODD_DX","PGS_Trad_ADHD")




