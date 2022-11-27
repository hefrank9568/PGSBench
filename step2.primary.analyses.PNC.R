
library(pROC)
####----------PNC------------####
rm(list = c("PNCFullSample","PNCfactorScore"))
####Descriptives####
table(Merged_PNC$Race)
##AA: 2719; EA: 4789
##scale PGSs by two racial groups

Merged_PNC <- Merged_PNC %>% 
  group_by(Race) %>%
  mutate(pgs_bip_c = scale(pgs_bip),
         pgs_mdd_c = scale(pgs_mdd),
         pgs_anx_c = scale(pgs_anx),
         pgs_an_c = scale(pgs_an),
         #pgs_alchfull_c = scale(pgs_alchfull),
         #pgs_audit_c = scale(pgs_audit),
         #pgs_cudeur_c = scale(pgs_cudeur),
         pgs_ocd_c = scale(pgs_ocd), 
         pgs_scz_c = scale(pgs_scz), 
         #pgs_ptsdafr_c = scale(pgs_ptsdafr), 
         pgs_ptsdgsem_c = scale(pgs_ptsd), 
         #pgs_ptsdeur_c = scale(pgs_ptsdeur), 
         pgs_adhd_c = scale(pgs_adhd),
         #pgs_adhdrepl_c = scale(pgs_adhdrepl),
         pgs_ts_c = scale(pgs_ts),
         pgs_asd_c = scale(pgs_asd),
         #pgs_alchafr_c = scale(pgs_alchafr),
         #pgs_alcheur_c = scale(pgs_alcheur),
         pgs_extcons_c = scale(pgs_extcons),
         pgs_sub_gsem_c = scale(pgs_sub_gsem),
         pgs_neuro_gsem_c = scale(pgs_neuro_gsem),
         pgs_comp_gsem_c = scale(pgs_comp_gsem),
         pgs_psy_gsem_c = scale(pgs_psy_gsem),
         pgs_int_gsem_c = scale(pgs_int_gsem),
         pgs_ext_irwin_c = scale(pgs_ext_irwin),
         pgs_int_irwin_c = scale(pgs_int_irwin),
         pgs_ndd_irwin_c = scale(pgs_ndd_irwin),
         pgs_tp_irwin_c = scale(pgs_tp_irwin)) %>%
  ungroup()

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

###set up data names###
Merged_PNC %>%
  select(starts_with("pgs") & ends_with("_c"), -"Race") %>%
  names()

PGS_PNC <- c("pgs_adhd_c","pgs_ts_c","pgs_asd_c",
             "pgs_ocd_c","pgs_bip_c","pgs_scz_c",
             "pgs_ptsdgsem_c",
             "pgs_mdd_c","pgs_anx_c","pgs_an_c","pgs_extcons_c","pgs_sub_gsem_c",
             "pgs_neuro_gsem_c","pgs_comp_gsem_c","pgs_psy_gsem_c","pgs_int_gsem_c","pgs_ext_irwin_c",
             "pgs_int_irwin_c","pgs_ndd_irwin_c","pgs_tp_irwin_c")

Merged_PNC %>%
  select(ends_with("_SX"), ends_with("_TSX"),Psychosis,PTD_Int,EAT_TotSX,G,INT,EXT) %>%
  names()

Disorder_PNC <- c("ODD_SX","CDD_SX","ADD_TSX",
                  "OCD_TSX","MAN_TSX","Psychosis",
                  "PTD_Int",
                  "DEP_TSX","GAD_TSX","SEP_SX","SOC_SX","PHB_SX","PAN_SX","AGR_SX","EAT_TotSX",
                  "G","INT","EXT","THOUGHT")

##scale the outcomes 
Merged_PNC <- Merged_PNC %>%
  mutate(ODD_SX = scale(ODD_SX),
         CDD_SX = scale(CDD_SX), 
         ADD_TSX = scale(ADD_TSX),
         OCD_TSX = scale(OCD_TSX),
         MAN_TSX = scale(MAN_TSX),
         Psychosis = scale(Psychosis),
         PTS_Int = scale(PTD_Int),
         DEP_TSX = scale(DEP_TSX),
         GAD_TSX = scale(GAD_TSX),
         SEP_SX = scale(SEP_SX),
         SOC_SX = scale(SOC_SX),
         PHB_SX = scale(PHB_SX),
         PAN_SX = scale(PAN_SX),
         AGR_SX = scale(AGR_SX),
         EAT_TotSX = scale(EAT_TotSX),
         G = scale(G),
         INT = scale(INT),
         EXT = scale(EXT),
         THOUGHT = scale(THOUGHT))

##all traits and pgs pairs
dataName_pnc <- expand.grid(Disorder_PNC, PGS_PNC)

##select a number of PGSs and traits for analyses
EXTrelatedPGS <- c("pgs_adhd_c","pgs_extcons_c",
                   "pgs_ext_irwin_c","pgs_sub_gsem_c")
ADHDrelatedPGS <- c("pgs_adhd_c","pgs_extcons_c","pgs_neuro_gsem_c","pgs_ext_irwin_c","pgs_sub_gsem_c")
PTSDrelatedPGS <- c("pgs_ptsdgsem_c","pgs_int_gsem_c","pgs_neuro_gsem_c","pgs_ndd_irwin_c")
INTrelatedPGS <- c("pgs_ptsdgsem_c",
                   "pgs_mdd_c","pgs_anx_c","pgs_an_c","pgs_int_gsem_c","pgs_int_irwin_c")
THTrelatedPGS <- c("pgs_ocd_c","pgs_bip_c","pgs_scz_c","pgs_comp_gsem_c","pgs_psy_gsem_c","pgs_tp_irwin_c")
ANXrelatedPGS <- c("pgs_anx_c","pgs_int_gsem_c","pgs_int_irwin_c")
#ANXrelatedDisorder <- c("GAD_TSX","SEP_SX","SOC_SX","PHB_SX","PAN_SX","AGR_SX")

####grid for select number of pgs and traits
selectdataNames_pnc <- tibble(Var1 = c(rep("ODD_SX", length(EXTrelatedPGS)),
                                       rep("CDD_SX", length(EXTrelatedPGS)),
                                       rep("ADD_TSX", length(ADHDrelatedPGS)),
                                       "OCD_TSX","OCD_TSX","OCD_TSX",
                                       "MAN_TSX","MAN_TSX","MAN_TSX",
                                       "Psychosis","Psychosis","Psychosis",
                                       rep("PTD_Int", length(PTSDrelatedPGS)),
                                       rep("GAD_TSX",length(ANXrelatedPGS)),
                                       rep("SEP_SX",length(ANXrelatedPGS)),
                                       rep("SOC_SX",length(ANXrelatedPGS)),
                                       rep("PHB_SX",length(ANXrelatedPGS)),
                                       rep("PAN_SX",length(ANXrelatedPGS)),
                                       rep("AGR_SX",length(ANXrelatedPGS)),
                                       "DEP_TSX","DEP_TSX","DEP_TSX",
                                       "EAT_TotSX","EAT_TotSX",
                                       rep("G", length(PGS_PNC)),
                                       rep("INT",length(INTrelatedPGS)),
                                       rep("EXT", length(EXTrelatedPGS))),
                              
                              Var2 = c(EXTrelatedPGS,
                                       EXTrelatedPGS,
                                       ADHDrelatedPGS,
                                       "pgs_ocd_c","pgs_comp_gsem_c","pgs_tp_irwin_c",
                                       "pgs_bip_c","pgs_psy_gsem_c","pgs_tp_irwin_c",
                                       "pgs_scz_c","pgs_psy_gsem_c","pgs_tp_irwin_c",
                                       PTSDrelatedPGS,
                                       ANXrelatedPGS,
                                       ANXrelatedPGS,
                                       ANXrelatedPGS,
                                       ANXrelatedPGS,
                                       ANXrelatedPGS,
                                       ANXrelatedPGS,
                                       "pgs_mdd_c","pgs_int_gsem_c","pgs_int_irwin_c",
                                       "pgs_int_gsem_c","pgs_int_irwin_c",
                                       PGS_PNC,
                                       INTrelatedPGS,
                                       EXTrelatedPGS))

####Running models with a select number of disorder - PGS pairs####
####All subjects, adding interaction with Race and PC1, PC2####
all_select_cov_pnc <- lapply(1:nrow(selectdataNames_pnc), function(x) lm(as.formula(paste0(selectdataNames_pnc[x,"Var1"], "~",
                                                                                            "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                                            sep = "")),
                                                                          data = Merged_PNC))

all_select_pnc <- lapply(1:nrow(selectdataNames_pnc), function(x) lm(as.formula(paste0(selectdataNames_pnc[x,"Var1"], "~", 
                                                                                        paste0(selectdataNames_pnc[x,"Var2"], "*Race+"), 
                                                                                        paste0(selectdataNames_pnc[x,"Var2"], "*PC1+"), 
                                                                                        paste0(selectdataNames_pnc[x,"Var2"], "*PC2"), 
                                                                                        "+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", sep = "")),
                                                                      data = Merged_PNC))

R2select_all_pnc = lapply(all_select_pnc, performance::r2)
R2select_cov_pnc = lapply(all_select_cov_pnc, performance::r2)

R2select_diff_pnc = unname(unlist(R2select_all_pnc)) - unname(unlist(R2select_cov_pnc))
plot(R2select_diff_pnc)

rmseselect_all_pnc = lapply(all_select_pnc, performance::performance_rmse) 
rmseselect_cov_all_pnc = lapply(all_select_cov_pnc, performance::performance_rmse) 
rmseselect_diff_all_pnc = unname(unlist(rmseselect_all_pnc)) - unname(unlist(rmseselect_cov_all_pnc))

#nagelkerkeselect_all_pnc = lapply(all_select_pnc, performance::r2_nagelkerke) 
#nagelkerkeselect_all_cov_pnc = lapply(all_select_cov_pnc, performance::r2_nagelkerke) 
#nagelkerkeselect_all_diff_pnc = unname(unlist(nagelkerkeselect_all_pnc)) - unname(unlist(nagelkerkeselect_all_cov_pnc))
####Model in EA####
EAselect_cov_pnc <- lapply(1:nrow(selectdataNames_pnc), function(x) lm(as.formula(paste0(selectdataNames_pnc[x,"Var1"], "~",
                                                                                          "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                                          sep = "")), 
                                                                        data = Merged_PNC[Merged_PNC$Race==1,]))


EAselect_pnc <- lapply(1:nrow(selectdataNames_pnc), function(x) lm(as.formula(paste0(selectdataNames_pnc[x,"Var1"], "~", 
                                                                                      selectdataNames_pnc[x,"Var2"],
                                                                                      "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                                      sep = "")), 
                                                                    data = Merged_PNC[Merged_PNC$Race==1,]))


R2select_EA_pnc = lapply(EAselect_pnc, performance::r2)
R2select_EA_cov_pnc = lapply(EAselect_cov_pnc, performance::r2)
R2select_diff_EA_pnc = unname(unlist(R2select_EA_pnc)) - unname(unlist(R2select_EA_cov_pnc))
plot(R2select_diff_EA_pnc)

rmseselect_EA_pnc = lapply(EAselect_pnc, performance::performance_rmse) 
rmseselect_cov_EA_pnc = lapply(EAselect_cov_pnc, performance::performance_rmse) 
rmseselect_diff_EA_pnc = unname(unlist(rmseselect_EA_pnc)) - unname(unlist(rmseselect_cov_EA_pnc))

plot(rmseselect_diff_EA_pnc)
#nagelkerkeselect_EA_pnc = lapply(EAselect_pnc, performance::r2_nagelkerke) 
#nagelkerkeselect_EA_cov_pnc = lapply(EAselect_cov_pnc, performance::r2_nagelkerke) 
#nagelkerkeselect_EA_diff_pnc = unname(unlist(nagelkerkeselect_EA_pnc)) - unname(unlist(nagelkerkeselect_EA_cov_pnc))
#plot(nagelkerkeselect_EA_diff_pnc)

####Model in AA####
AAselect_cov_pnc <- lapply(1:nrow(selectdataNames_pnc), function(x) lm(as.formula(paste0(selectdataNames_pnc[x,"Var1"], "~",
                                                                                          "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                                          sep = "")), 
                                                                        data = Merged_PNC[Merged_PNC$Race==0,]))
AAselect_pnc <- lapply(1:nrow(selectdataNames_pnc), function(x) lm(as.formula(paste0(selectdataNames_pnc[x,"Var1"], "~", 
                                                                                      selectdataNames_pnc[x,"Var2"],
                                                                                      "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                                      sep = "")), 
                                                                    data = Merged_PNC[Merged_PNC$Race==0,]))


R2select_AA_pnc = lapply(AAselect_pnc, performance::r2)
R2select_AA_cov_pnc = lapply(AAselect_cov_pnc, performance::r2)
R2select_diff_AA_pnc = unname(unlist(R2select_AA_pnc)) - unname(unlist(R2select_AA_cov_pnc))
plot(R2select_diff_AA_pnc)

rmseselect_AA_pnc = lapply(AAselect_pnc, performance::performance_rmse) 
rmseselect_cov_AA_pnc = lapply(AAselect_cov_pnc, performance::performance_rmse) 
rmseselect_diff_AA_pnc = unname(unlist(rmseselect_AA_pnc)) - unname(unlist(rmseselect_cov_AA_pnc))
plot(rmseselect_diff_AA_pnc)

#nagelkerkeselect_AA_pnc = lapply(AAselect_pnc, performance::r2_nagelkerke) 
#nagelkerkeselect_AA_cov_pnc = lapply(AAselect_cov_pnc, performance::r2_nagelkerke) 
#nagelkerkeselect_AA_diff_pnc = unname(unlist(nagelkerkeselect_AA_pnc)) - unname(unlist(nagelkerkeselect_AA_cov_pnc))
#plot(nagelkerkeselect_AA_diff_pnc)


####All subjects and all disorder-PGS pairs, adding interaction with Race and PC1, PC2####
all_cov_pnc <- lapply(1:nrow(dataName_pnc), function(x) lm(as.formula(paste0(dataName_pnc[x,"Var1"], "~",
                                                                              "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                              sep = "")),
                                                            data = Merged_PNC))
all_pnc <- lapply(1:nrow(dataName_pnc), function(x) lm(as.formula(paste0(dataName_pnc[x,"Var1"], "~", 
                                                                         paste0(dataName_pnc[x,"Var2"], "*Race+"), 
                                                                         paste0(dataName_pnc[x,"Var2"], "*PC1+"), 
                                                                         paste0(dataName_pnc[x,"Var2"], "*PC2"), 
                                                                         "+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", sep = "")),
                                                        data = Merged_PNC))
R2_all_pnc = lapply(all_pnc, performance::r2)
R2_cov_pnc = lapply(all_cov_pnc, performance::r2)

R2_diff_pnc = unname(unlist(R2_all_pnc)) - unname(unlist(R2_cov_pnc))
plot(R2_diff_pnc)

####Model in EA####
EA_cov_pnc <- lapply(1:nrow(dataName_pnc), function(x) lm(as.formula(paste0(dataName_pnc[x,"Var1"], "~",
                                                                            "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                            sep = "")),
                                                           data = Merged_PNC[Merged_PNC$Race==1,]))
EA_pnc <- lapply(1:nrow(dataName_pnc), function(x) lm(as.formula(paste0(dataName_pnc[x,"Var1"], "~", 
                                                                         dataName_pnc[x,"Var2"],
                                                                         "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                        sep = "")), 
                                                       data = Merged_PNC[Merged_PNC$Race==1,]))

R2_EA_pnc = lapply(EA_pnc, performance::r2)
R2_EA_cov_pnc = lapply(EA_cov_pnc, performance::r2)
R2_diff_EA_pnc = unname(unlist(R2_EA_pnc)) - unname(unlist(R2_EA_cov_pnc))
plot(R2_diff_EA_pnc)

####Model in AA####
AA_cov_pnc <- lapply(1:nrow(dataName_pnc), function(x) lm(as.formula(paste0(dataName_pnc[x,"Var1"], "~",
                                                                            "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                            sep = "")), 
                                                           data = Merged_PNC[Merged_PNC$Race==0,]))
AA_pnc <- lapply(1:nrow(dataName_pnc), function(x) glm(as.formula(paste0(dataName_pnc[x,"Var1"], "~", 
                                                                        dataName_pnc[x,"Var2"],
                                                                        "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+Sex + age_at_cnb", 
                                                                        sep = "")), 
                                                       data = Merged_PNC[Merged_PNC$Race==0,]))

R2_AA_pnc = lapply(AA_pnc, performance::r2)
R2_AA_cov_pnc = lapply(AA_cov_pnc, performance::r2)
R2_diff_AA_pnc = unname(unlist(R2_AA_pnc)) - unname(unlist(R2_AA_cov_pnc))
plot(R2_diff_AA_pnc)

##run ROC and AUC for each outcome 

Merged_PNC %>%
  roc("ODD_DX","PGS_Trad_ADHD")


####for the rest of the analyses, only produce the association for matching pairs of PGS-disorder
####for visualization, save only the R2s from all PGS-disorder pairs, then make a bar graph that shows the R2 for all disorders. 
####but also just run the PGS-disorder associations for all combinations, and save the results as supplemental


