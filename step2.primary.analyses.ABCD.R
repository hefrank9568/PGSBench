options(scipen=999)
####run pgs bivariate correlations
abcd_merged <- merge(abcdPGSCombined, abcd_pdemo, by = "subjectkey")
abcd_PCs <- abcd_PCs %>%
  rename(subjectkey = IID)
abcd_PCs$`#FID` = NULL
abcd_merged <- merge(abcd_merged, abcd_PCs, by = "subjectkey")
##create race variable (1=white, 2 = AA, 3 = hispanic, 4 = other)
abcd_merged <- abcd_merged %>%
  mutate(hispanic = ifelse(demo_ethn_v2==1 & demo_ethn_v2!=777 & demo_ethn_v2 !=999,1,0))

abcd_merged <- abcd_merged %>%
  mutate(Race = ifelse(demo_race_a_p___10==1 & hispanic == 0, 1, 
                       ifelse(demo_race_a_p___11==1 & hispanic == 0, 2, 
                              ifelse(hispanic==1, 3, 4))))
###standardize within each group
abcd_merged <- abcd_merged %>% 
  group_by(Race) %>%
  mutate(pgs_adhd_c = scale(pgs_adhd),
         pgs_bip_c = scale(pgs_bip),
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

RawPGStoRemove <- abcd_merged %>%
  select(starts_with("pgs") & !ends_with("_c")) %>%
  names()

abcd_merged[,all_of(RawPGStoRemove)] = NULL

PGS_abcd <- abcd_merged %>%
  select(starts_with("pgs") & ends_with("_c")) %>%
  names() #remove subjectkey

##recode sex 
abcd_merged$demo_sex_v2[which(abcd_merged$demo_sex_v2==3)] = NA
abcd_merged$demo_sex_v2 <- ifelse(abcd_merged$demo_sex_v2==1, -.5, .5)
##clean income variable
table(abcd_merged$demo_comb_income_v2)
abcd_merged <- abcd_merged %>%
  mutate(demo_comb_income_v2 = na_if(demo_comb_income_v2,"999"))
abcd_merged <- abcd_merged %>%
  mutate(demo_comb_income_v2 = na_if(demo_comb_income_v2,"777"))
abcd_merged$demo_comb_income_v2 <- as.numeric(abcd_merged$demo_comb_income_v2)
##interview age
abcd_merged$interview_age <- as.numeric(abcd_merged$interview_age)/12
###put parental education as covariate
abcd_merged <- abcd_merged %>%
  mutate(prnt_ed = na_if(demo_prnt_ed_v2,"777"))
abcd_merged$prnt_ed<- scale(as.numeric(abcd_merged$prnt_ed))
hist(abcd_merged$prnt_ed)

skimr::skim(abcd_merged)

EXTrelatedPGS <- c("pgs_adhd_c","pgs_extcons_c",
                   "pgs_ext_irwin_c","pgs_sub_gsem_c")
ADHDrelatedPGS <- c("pgs_adhd_c","pgs_extcons_c","pgs_neuro_gsem_c","pgs_sub_gsem_c","pgs_ext_irwin_c","pgs_ndd_irwin_c")
PTSDrelatedPGS <- c("pgs_ptsdgsem_c","pgs_int_gsem_c","pgs_ndd_irwin_c","pgs_neuro_gsem_c")
INTrelatedPGS <- c("pgs_ptsdgsem_c",
                   "pgs_mdd_c","pgs_anx_c","pgs_an_c","pgs_int_gsem_c","pgs_int_irwin_c")
THTrelatedPGS <- c("pgs_bip_c","pgs_scz_c","pgs_tp_irwin_c","pgs_psy_gsem_c")

####correlations in EA
abcdpgsCorr_ea <- abcd_merged %>%
  filter(Race ==1) %>%
  select(starts_with("pgs") & ends_with("_c")) %>%
  corr.test()

corrplot::corrplot(abcdpgsCorr_ea$r, diag = FALSE,type = "lower",method = "number", order = "hclust",tl.srt = 45,
                   p.mat = abcdpgsCorr_ea$p, sig.level = 0.05/length(all_of(PGS_abcd)), insig = "label_sig",
                   pch = "*",pch.cex = 1,pch.col = "grey",number.cex = 0.9)

####correlations in AA
abcdpgsCorr_aa <- abcd_merged %>%
  filter(Race ==2) %>%
  select(starts_with("pgs") & ends_with("_c")) %>%
  corr.test()

corrplot::corrplot(abcdpgsCorr_aa$r, diag = FALSE,type = "lower",method = "number",order = "hclust", tl.srt = 45,
                   p.mat = abcdpgsCorr_aa$p, sig.level = 0.05/length(all_of(PGS_abcd)), insig = "label_sig",
                   pch = "*",pch.cex = 1,pch.col = "grey", number.cex = 0.9)
####correlations in hispanic
abcdpgsCorr_ot <- abcd_merged %>%
  filter(Race ==3) %>%
  select(starts_with("pgs") & ends_with("_c")) %>%
  corr.test()

corrplot::corrplot(abcdpgsCorr_ot$r, diag = FALSE,type = "lower",method = "number", tl.srt = 45,
                   p.mat = abcdpgsCorr_ot$p, sig.level = 0.05/length(all_of(PGS_abcd)), insig = "label_sig",
                   pch = "*",pch.cex = 2,pch.col = "grey")



###should generate the files below from step2.hitop.ABCD
###abcd_cbclFScore
##read cbcl and ksads
cbcl_sumscore <- read.csv("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/cbcl_summedscores_allgroups.csv")
#cbcl_fs <- read.csv("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/abcd_twoGroupCBCL_fscores.csv")
ksads <- read.csv("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/PGSBench/abcd_ksadsCurrent.csv")
##only keep the factor score and subjectkey
#cbcl_fs <- cbcl_fs[,c(2,(ncol(cbcl_fs)-5):ncol(cbcl_fs))]
abcd_merged <- merge(cbcl_sumscore[,-1], abcd_merged, by = "subjectkey")
abcd_merged <- merge(abcd_merged, ksads[,-c(1)], by = "subjectkey")

skimr::skim(abcd_merged)

abcd_merged <- abcd_merged %>%
  group_by(Race) %>%
  mutate(EXT = log2(EXT+1),
         Neuro = log2(Neuro+1),
         INT = log2(INT+1),
         Soma = log2(Soma+1),
         Detach = log10(Detach+1),
         P = log2(P+1)) %>%
  ungroup()


sd(abcd_merged$EXT[abcd_merged$Race==1], na.rm = TRUE)
sd(abcd_merged$EXT[abcd_merged$Race==2], na.rm = TRUE)
sd(abcd_merged$EXT[abcd_merged$Race==3], na.rm = TRUE)
sd(abcd_merged$INT[abcd_merged$Race==1], na.rm = TRUE)
sd(abcd_merged$INT[abcd_merged$Race==2], na.rm = TRUE)
sd(abcd_merged$INT[abcd_merged$Race==3], na.rm = TRUE)
sd(abcd_merged$Neuro[abcd_merged$Race==1], na.rm = TRUE)
sd(abcd_merged$Neuro[abcd_merged$Race==2], na.rm = TRUE)
sd(abcd_merged$Neuro[abcd_merged$Race==3], na.rm = TRUE)
sd(abcd_merged$Soma[abcd_merged$Race==1], na.rm = TRUE)
sd(abcd_merged$Soma[abcd_merged$Race==2], na.rm = TRUE)
sd(abcd_merged$Soma[abcd_merged$Race==3], na.rm = TRUE)
sd(abcd_merged$P[abcd_merged$Race==1], na.rm = TRUE)
sd(abcd_merged$P[abcd_merged$Race==2], na.rm = TRUE)
sd(abcd_merged$P[abcd_merged$Race==3], na.rm = TRUE)


skimr::skim(abcd_merged)


####cbcl
cbcl_Factornames <- c("EXT","Neuro","INT","Soma","Detach")
cbclNames <- tibble(Var1 = c(rep("EXT",length(EXTrelatedPGS)),
                             "Neuro","Neuro","Neuro",
                             rep("INT",length(INTrelatedPGS)),
                             rep("Soma",length(INTrelatedPGS)),
                             "Detach","Detach","Detach",
                             rep("P",length(PGS_abcd))),
                    Var2 = c(EXTrelatedPGS,
                             "pgs_adhd_c","pgs_neuro_gsem_c","pgs_ndd_irwin_c",
                             INTrelatedPGS,
                             INTrelatedPGS,
                             "pgs_int_gsem_c","pgs_int_irwin_c","pgs_tp_irwin_c",
                             PGS_abcd))


####ksads
ksadsNames_diagnoses <- tibble(Var1 = c(rep("ksads_odd_crt",length(EXTrelatedPGS)),
                                        rep("ksads_cd_crt_child",length(EXTrelatedPGS)),
                              rep("ksads_adhd_crt",length(ADHDrelatedPGS)),
                              rep("ksads_pho_crt",length(INTrelatedPGS)),
                              "ksads_asspsy_crt","ksads_asspsy_crt","ksads_asspsy_crt",
                              "ksads_ocd_crt","ksads_ocd_crt"),
                    Var2 = c(EXTrelatedPGS,
                             EXTrelatedPGS,
                             ADHDrelatedPGS,
                             INTrelatedPGS,
                             "pgs_psy_gsem_c","pgs_tp_irwin_c","pgs_scz_c",
                             "pgs_ocd_c","pgs_comp_gsem_c"))



####CBCL models in EA####
cbcl_cov_abcd <- lapply(1:nrow(cbclNames), function(x) lm(as.formula(paste0(cbclNames[x,"Var1"], "~",
                                                                                            "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age+prnt_ed", 
                                                                                            sep = "")), 
                                                                          data = abcd_merged[abcd_merged$Race==1,]))

cbcl_abcd_mod <- lapply(1:nrow(cbclNames), function(x) lm(as.formula(paste0(cbclNames[x,"Var1"], "~", 
                                                                        cbclNames[x,"Var2"],
                                                                                      "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age+prnt_ed", 
                                                                                      sep = "")),
                                                        data = abcd_merged[abcd_merged$Race==1,]))


summariescbcl_abcd <- lapply(cbcl_abcd_mod, summary)


abcd_para_cbcl <- lapply(1:4, function(x)
  lapply(1:length(cbcl_abcd_mod), function(y) summariescbcl_abcd[[y]]$coefficients[2,x]))

parameters_abcd_cbcl <- data.frame(PGS = cbclNames$Var2,
                                        Trait = cbclNames$Var1,
                                        coef = unlist(abcd_para_cbcl[1]),
                                        se = unlist(abcd_para_cbcl[2]),
                                        t = unlist(abcd_para_cbcl[3]),
                                        p = unlist(abcd_para_cbcl[4]))

parameters_abcd_cbcl$p.adjust = p.adjust(parameters_abcd_cbcl$p, "fdr")

##add R2 values

R2_EA_abcd = lapply(cbcl_abcd_mod, performance::r2)
R2_EA_cov_abcd = lapply(cbcl_cov_abcd, performance::r2)
R2_diff_EA_abcd = unname(unlist(R2_EA_abcd)) - unname(unlist(R2_EA_cov_abcd))

parameters_abcd_cbcl$r2 = R2_diff_EA_abcd[seq(from = 2, to = length(R2_diff_EA_abcd), by = 2)]

for (i in seq(from = 1, to = length(R2_EA_cov_abcd), by = 1)) {
  parameters_abcd_cbcl$r2_cov[i] = R2_EA_cov_abcd[[i]]$R2_adjusted
}

rm(list = c("cbcl_abcd_mod","cbcl_cov_abcd","summariescbcl_abcd"))
write.csv(parameters_abcd_cbcl,"/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_ea.csv")


####CBCL models in AA####
cbcl_cov_abcd_aa <- lapply(1:nrow(cbclNames), function(x) lm(as.formula(paste0(cbclNames[x,"Var1"], "~",
                                                                            "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age+prnt_ed", 
                                                                            sep = "")), 
                                                          data = abcd_merged[abcd_merged$Race==2,]))

cbcl_abcd_mod_aa <- lapply(1:nrow(cbclNames), function(x) lm(as.formula(paste0(cbclNames[x,"Var1"], "~", 
                                                                            cbclNames[x,"Var2"],
                                                                            "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age+prnt_ed", 
                                                                            sep = "")),
                                                          data = abcd_merged[abcd_merged$Race==2,]))


summariescbcl_abcd_aa <- lapply(cbcl_abcd_mod_aa, summary)

abcd_para_cbcl_aa <- lapply(1:4, function(x)
  lapply(1:length(cbcl_abcd_mod_aa), function(y) summariescbcl_abcd_aa[[y]]$coefficients[2,x]))

parameters_abcd_cbcl_aa <- data.frame(PGS = cbclNames$Var2,
                                   Trait = cbclNames$Var1,
                                   coef = unlist(abcd_para_cbcl_aa[1]),
                                   se = unlist(abcd_para_cbcl_aa[2]),
                                   t = unlist(abcd_para_cbcl_aa[3]),
                                   p = unlist(abcd_para_cbcl_aa[4]))

parameters_abcd_cbcl_aa$p.adjust = p.adjust(parameters_abcd_cbcl_aa$p, "fdr")
##add R2 values
R2_AA_abcd = lapply(cbcl_abcd_mod_aa, performance::r2)
R2_AA_cov_abcd = lapply(cbcl_cov_abcd_aa, performance::r2)
R2_diff_AA_abcd = unname(unlist(R2_AA_abcd)) - unname(unlist(R2_AA_cov_abcd))

parameters_abcd_cbcl_aa$r2 = R2_diff_AA_abcd[seq(from = 2, to = length(R2_diff_AA_abcd), by = 2)]

for (i in seq(from = 1, to = length(R2_AA_cov_abcd), by = 1)) {
  parameters_abcd_cbcl_aa$r2_cov[i] = R2_AA_cov_abcd[[i]]$R2_adjusted
}
rm(list = c("cbcl_abcd_mod_aa","cbcl_cov_abcd_aa","summariescbcl_abcd_aa"))
write.csv(parameters_abcd_cbcl_aa,"/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_aa.csv")


####CBCL models in Hispanic####
cbcl_cov_abcd_hisp <- lapply(1:nrow(cbclNames), function(x) lm(as.formula(paste0(cbclNames[x,"Var1"], "~",
                                                                                 "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age+prnt_ed", 
                                                                                 sep = "")), 
                                                               data = abcd_merged[abcd_merged$Race==3,]))

cbcl_abcd_mod_hisp <- lapply(1:nrow(cbclNames), function(x) lm(as.formula(paste0(cbclNames[x,"Var1"], "~", 
                                                                                 cbclNames[x,"Var2"],
                                                                                 "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age+prnt_ed", 
                                                                                 sep = "")),
                                                               data = abcd_merged[abcd_merged$Race==3,]))


summariescbcl_abcd_hisp <- lapply(cbcl_abcd_mod_hisp, summary)

abcd_para_cbcl_hisp <- lapply(1:4, function(x)
  lapply(1:length(cbcl_abcd_mod_hisp), function(y) summariescbcl_abcd_hisp[[y]]$coefficients[2,x]))

parameters_abcd_cbcl_hisp <- data.frame(PGS = cbclNames$Var2,
                                   Trait = cbclNames$Var1,
                                   coef = unlist(abcd_para_cbcl_hisp[1]),
                                   se = unlist(abcd_para_cbcl_hisp[2]),
                                   t = unlist(abcd_para_cbcl_hisp[3]),
                                   p = unlist(abcd_para_cbcl_hisp[4]))

parameters_abcd_cbcl_hisp$p.adjust = p.adjust(parameters_abcd_cbcl_hisp$p, "fdr")

##add R2 values

R2_hisp_abcd = lapply(cbcl_abcd_mod_hisp, performance::r2)
R2_hisp_cov_abcd = lapply(cbcl_cov_abcd_hisp, performance::r2)
R2_diff_hisp_abcd = unname(unlist(R2_hisp_abcd)) - unname(unlist(R2_hisp_cov_abcd))

parameters_abcd_cbcl_hisp$r2 = R2_diff_hisp_abcd[seq(from = 2, to = length(R2_diff_hisp_abcd), by = 2)]
for (i in seq(from = 1, to = length(R2_hisp_cov_abcd), by = 1)) {
  parameters_abcd_cbcl_hisp$r2_cov[i] = R2_hisp_cov_abcd[[i]]$R2_adjusted
}
rm(list = c("cbcl_abcd_mod_hisp","cbcl_cov_abcd_hisp","summariescbcl_abcd_hisp"))
write.csv(parameters_abcd_cbcl_hisp,"/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_hisp.csv")


####KSADS diagnoses prevalence in EA and AA####
colnames(ksads)[2:10]
##in EA
freq_ksads_ea <- abcd_merged[abcd_merged$Race==1,] %>%
  select(colnames(ksads)[2:10]) %>%
  lapply(table)
8241*.01
##in AA
freq_ksads_aa <- abcd_merged[abcd_merged$Race==2,] %>%
  select(colnames(ksads)[2:10]) %>%
  lapply(table)
1860*.01

table(abcd_merged$Race)

####KSADS models in EA: diagnoses####


ksads_cov_abcd <- lapply(1:nrow(ksadsNames_diagnoses), function(x) glm(as.formula(paste0(ksadsNames_diagnoses[x,"Var1"], "~",
                                                                               "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age+prnt_ed", 
                                                                               sep = "")), 
                                                             family = "binomial",
                                                             data = abcd_merged[abcd_merged$Race==1,]))

ksads_abcd <- lapply(1:nrow(ksadsNames_diagnoses), function(x) glm(as.formula(paste0(ksadsNames_diagnoses[x,"Var1"], "~", 
                                                                                     ksadsNames_diagnoses[x,"Var2"],
                                                                           "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age+prnt_ed", 
                                                                           sep = "")),
                                                         family = "binomial",
                                                         data = abcd_merged[abcd_merged$Race==1,]))
summariescbcl_ksads<- lapply(ksads_abcd, summary)
abcd_para_ksads <- lapply(1:4, function(x)
  lapply(1:length(ksads_abcd), function(y) summariescbcl_ksads[[y]]$coefficients[2,x]))
parameters_abcd_ksads <- data.frame(PGS = ksadsNames_diagnoses$Var2,
                                   Trait = ksadsNames_diagnoses$Var1,
                                   coef = unlist(abcd_para_ksads[1]),
                                   se = unlist(abcd_para_ksads[2]),
                                   t = unlist(abcd_para_ksads[3]),
                                   p = unlist(abcd_para_ksads[4]))
parameters_abcd_ksads$p.adjust = p.adjust(parameters_abcd_ksads$p, "fdr")

##add R2 values
R2_EA_ksads_abcd = lapply(ksads_abcd, performance::r2)
R2_EA_ksads_cov_abcd = lapply(ksads_cov_abcd, performance::r2)
R2_diff_EA_ksads_abcd = unname(unlist(R2_EA_ksads_abcd)) - unname(unlist(R2_EA_ksads_cov_abcd))

parameters_abcd_ksads$r2 = R2_diff_EA_ksads_abcd

for (i in seq(from = 1, to = length(R2_EA_ksads_cov_abcd), by = 1)) {
  parameters_abcd_ksads$r2_cov[i] = R2_EA_ksads_cov_abcd[[i]]$R2_Tjur
}

rm(list = c("ksads_abcd","ksads_cov_abcd","summariescbcl_ksads"))
write.csv(parameters_abcd_ksads,"/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_ksads_ea.csv")

####KSADS models in AA: diagnoses####
ksadsNames_diagnoses <- tibble(Var1 = c(
                                        rep("ksads_adhd_crt",length(ADHDrelatedPGS)),
                                        "ksads_pho_crt","ksads_pho_crt","ksads_pho_crt",
                                        "ksads_asspsy_crt","ksads_asspsy_crt","ksads_asspsy_crt"),
                               Var2 = c(
                                        ADHDrelatedPGS,
                                        "pgs_anx_c","pgs_int_gsem_c","pgs_int_irwin_c",
                                        "pgs_scz_c","pgs_psy_gsem_c","pgs_tp_irwin_c")
)

ksads_cov_abcd_aa <- lapply(1:nrow(ksadsNames_diagnoses), function(x) glm(as.formula(paste0(ksadsNames_diagnoses[x,"Var1"], "~",
                                                                                         "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age+prnt_ed", 
                                                                                         sep = "")), 
                                                                       family = "binomial",
                                                                       data = abcd_merged[abcd_merged$Race==2,]))

ksads_abcd_aa <- lapply(1:nrow(ksadsNames_diagnoses), function(x) glm(as.formula(paste0(ksadsNames_diagnoses[x,"Var1"], "~", 
                                                                                     ksadsNames_diagnoses[x,"Var2"],
                                                                                     "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age+prnt_ed", 
                                                                                     sep = "")),
                                                                   family = "binomial",
                                                                   data = abcd_merged[abcd_merged$Race==2,]))
summariescbcl_ksads_aa <- lapply(ksads_abcd_aa, summary)
abcd_para_ksads_aa <- lapply(1:4, function(x)
  lapply(1:length(ksads_abcd_aa), function(y) summariescbcl_ksads_aa[[y]]$coefficients[2,x]))
parameters_abcd_ksads_aa <- data.frame(PGS = ksadsNames_diagnoses$Var2,
                                    Trait = ksadsNames_diagnoses$Var1,
                                    coef = unlist(abcd_para_ksads_aa[1]),
                                    se = unlist(abcd_para_ksads_aa[2]),
                                    t = unlist(abcd_para_ksads_aa[3]),
                                    p = unlist(abcd_para_ksads_aa[4]))
parameters_abcd_ksads_aa$p.adjust = p.adjust(parameters_abcd_ksads_aa$p, "fdr")
##add R2 values
R2_AA_ksads_abcd = lapply(ksads_abcd_aa, performance::r2)
R2_AA_ksads_cov_abcd = lapply(ksads_cov_abcd_aa, performance::r2)
R2_diff_AA_ksads_abcd = unname(unlist(R2_AA_ksads_abcd)) - unname(unlist(R2_AA_ksads_cov_abcd))

parameters_abcd_ksads_aa$r2 = R2_diff_AA_ksads_abcd
for (i in seq(from = 1, to = length(R2_AA_ksads_cov_abcd), by = 1)) {
  parameters_abcd_ksads_aa$r2_cov[i] = R2_AA_ksads_cov_abcd[[i]]$R2_Tjur
}
write.csv(parameters_abcd_ksads_aa,"/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_ksads_aa.csv")

rm(list = c("summariescbcl_ksads_aa","ksads_abcd_aa","ksads_cov_abcd_aa"))
gc()

####KSADS models in Hispanic: diagnoses####
ksadsNames_diagnoses <- tibble(Var1 = c(
  rep("ksads_adhd_crt",length(ADHDrelatedPGS)),
  "ksads_pho_crt","ksads_pho_crt","ksads_pho_crt",
  "ksads_asspsy_crt","ksads_asspsy_crt","ksads_asspsy_crt"),
  Var2 = c(
    ADHDrelatedPGS,
    "pgs_anx_c","pgs_int_gsem_c","pgs_int_irwin_c",
    "pgs_scz_c","pgs_psy_gsem_c","pgs_tp_irwin_c")
)

ksads_cov_abcd_hisp <- lapply(1:nrow(ksadsNames_diagnoses), function(x) glm(as.formula(paste0(ksadsNames_diagnoses[x,"Var1"], "~",
                                                                                            "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age", 
                                                                                            sep = "")), 
                                                                          family = "binomial",
                                                                          data = abcd_merged[abcd_merged$Race==3,]))

ksads_abcd_hisp <- lapply(1:nrow(ksadsNames_diagnoses), function(x) glm(as.formula(paste0(ksadsNames_diagnoses[x,"Var1"], "~", 
                                                                                        ksadsNames_diagnoses[x,"Var2"],
                                                                                        "+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+demo_sex_v2 + demo_comb_income_v2 + interview_age", 
                                                                                        sep = "")),
                                                                      family = "binomial",
                                                                      data = abcd_merged[abcd_merged$Race==3,]))
summariescbcl_ksads_hisp <- lapply(ksads_abcd_hisp, summary)
abcd_para_ksads_hisp <- lapply(1:4, function(x)
  lapply(1:length(ksads_abcd_hisp), function(y) summariescbcl_ksads_hisp[[y]]$coefficients[2,x]))
parameters_abcd_ksads_hisp <- data.frame(PGS = ksadsNames_diagnoses$Var2,
                                       Trait = ksadsNames_diagnoses$Var1,
                                       coef = unlist(abcd_para_ksads_hisp[1]),
                                       se = unlist(abcd_para_ksads_hisp[2]),
                                       t = unlist(abcd_para_ksads_hisp[3]),
                                       p = unlist(abcd_para_ksads_hisp[4]))
parameters_abcd_ksads_hisp$p.adjust = p.adjust(parameters_abcd_ksads_hisp$p, "fdr")
##add R2 values
R2_Hisp_ksads_abcd = lapply(ksads_abcd_hisp, performance::r2)
R2_Hisp_ksads_cov_abcd = lapply(ksads_cov_abcd_hisp, performance::r2)
R2_diff_Hisp_ksads_abcd = unname(unlist(R2_Hisp_ksads_abcd)) - unname(unlist(R2_Hisp_ksads_cov_abcd))

parameters_abcd_ksads_hisp$r2 = R2_diff_Hisp_ksads_abcd
for (i in seq(from = 1, to = length(R2_Hisp_ksads_cov_abcd), by = 1)) {
  parameters_abcd_ksads_hisp$r2_cov[i] = R2_Hisp_ksads_cov_abcd[[i]]$R2_Tjur
}
write.csv(parameters_abcd_ksads_hisp,"/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_ksads_hisp.csv")

rm(list = c("summariescbcl_ksads_hisp","ksads_abcd_hisp","ksads_cov_abcd_hisp"))
