######Load packages#####
library(lmSupport)
library(psych)
library(dplyr)
library(ggplot2)
library(doParallel)
library(performance)
library(data.table)
library(foreign)
library(tidymodels)
library(tidyverse)
library(pROC)

####------------PNC-------------####

###note, this version of the data only includes AA and EA; mixed race individuals were not included
PNCFullSample = readxl::read_xlsx("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/PNC projects/Coding and analysis/Diagnosis result file/Processed data file/PNC Diagnoses_withAge.xlsx", na = "-999")

PC = paste("PC", seq(1,10, by = 1), sep = "")
##Load PGSs###
PNC_EAADHDPGSGSEM = read.delim("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/CHGPM Grant Study (ASD_ADHD)/GSEM Specific Association Results/GSEM_PGS_0516_noSUB/PNC/PNC_EA_GSEMADHD_05162021_noSUB.all_score",sep="",header = TRUE) %>%
  rename(GSEM_ADHDPGS = Pt_1)
PNC_AA_ADHDPGSGSEM = read.delim("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/PNC PGS/PNC_AA_GSEMADHD_noSUB.all_score",sep="",header = TRUE) %>%
  rename(GSEM_ADHDPGS = Pt_1)

###Combine GSEM PGSs of two groups into one file
PNCGSEMcombined <- rbind(PNC_EAADHDPGSGSEM,PNC_AA_ADHDPGSGSEM)
PNCGSEMcombined$SUBJID = sapply(strsplit(PNCGSEMcombined$IID, split = "_", fixed = TRUE), function(x) (x[2]))

#add in traditional PGS from a previous file
PNCPGSMerged = read.csv("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/R21 Resubmission 2021/PNC/PNC_AllPGS_AfterZihang_replicate.csv") %>%
  select(SUBJID, PGS_Trad_ADHD, all_of(PC), ASD_EVER)
##there are duplicates -> probably because of parent report included in SUBJID as well. But PGSs are the same for the duplicate SUBJIDs; 
##remove all duplicates 
PNCPGSMerged <- PNCPGSMerged[-which(duplicated(PNCPGSMerged$SUBJID)),]

###final data object with phenotype and genetic data
Merged_PNC <- merge(PNCFullSample, PNCGSEMcombined, by = "SUBJID")
Merged_PNC <- merge(Merged_PNC, PNCPGSMerged, by = "SUBJID")
table(Merged_PNC$Race)
Merged_PNC$age_at_cnb <- as.numeric(Merged_PNC$age_at_cnb)
Merged_PNC$Med_Rate <- as.numeric(Merged_PNC$Med_Rate)
##variance of ADHD TSX
var(Merged_PNC$ADD_TSX)
var(Merged_PNC$ADD_TSX[Merged_PNC$Race==0])
var(Merged_PNC$ADD_TSX[Merged_PNC$Race==1])

varAncestry <- glm(ADD_TSX ~ Race + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10 +Sex + age_at_cnb, 
    family = "poisson", data = Merged_PNC)
summary(varAncestry)

##scatter plots##
plot(Merged_PNC$PGS_Trad_ADHD, Merged_PNC$ADD_TSX)
plot(Merged_PNC$GSEM_ADHDPGS, Merged_PNC$ADD_TSX)
##Basic Descriptive##
###Age and Sex by Race
##Sex: Male = 0, Female = 1, Other combinations = 2
##Race: AA = 0, EA = 1

table(Merged_PNC$Sex, Merged_PNC$Race)
SexR <- lm(Sex ~ Race,data = Merged_PNC)
anova(SexR)

##linear regression
Merged_PNC$PGS_Trad_ADHD
m1 <- lm(ADD_TSX ~ PGS_Trad_ADHD*Race + PGS_Trad_ADHD*PC1 + PGS_Trad_ADHD*PC2 + PGS_Trad_ADHD*PC3 + PGS_Trad_ADHD*PC4 + PGS_Trad_ADHD*PC5 
         + PGS_Trad_ADHD*PC6 +PGS_Trad_ADHD*PC7 + PGS_Trad_ADHD*PC8 + PGS_Trad_ADHD*PC9 + PGS_Trad_ADHD*PC10 +Sex*PGS_Trad_ADHD + Med_Rate*PGS_Trad_ADHD + age_at_cnb, data = Merged_PNC)
summary(m1)
performance::r2(m1)

m1.AA <- lm(ADD_TSX ~ PGS_Trad_ADHD + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10 + Sex*PGS_Trad_ADHD + Med_Rate + age_at_cnb, data = Merged_PNC[Merged_PNC$Race==0,])
summary(m1.AA)
performance::r2(m1.AA)
performance::r2(lm(ADD_TSX ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10 + Sex + Med_Rate + age_at_cnb, data = Merged_PNC[Merged_PNC$Race==0,]))


m1.EA <- lm(ADD_TSX ~ PGS_Trad_ADHD + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10 + Sex*PGS_Trad_ADHD +  Med_Rate + age_at_cnb, data = Merged_PNC[Merged_PNC$Race==1,])
summary(m1.EA)
performance::r2(m1.EA)
performance::r2(lm(ADD_TSX ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10 + Sex+  Med_Rate + age_at_cnb, data = Merged_PNC[Merged_PNC$Race==1,]))

table(PNCFullSample$Med_Rate)

##diagnosis of ADHD

m2 <- glm(ADD_DX ~ PGS_Trad_ADHD*Race + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10 +Sex + age_at_cnb, 
          family = "binomial",data = Merged_PNC)
summary(m2)
performance::r2(m2)

m2.AA <- glm(ADD_DX ~ PGS_Trad_ADHD + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10 +Sex + age_at_cnb, 
             family = "binomial", data = Merged_PNC[Merged_PNC$Race==0,])
summary(m2.AA)
performance::r2(m2.AA)

m2.EA <- glm(ADD_DX ~ PGS_Trad_ADHD + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +PC7 + PC8 + PC9 + PC10 +Sex + age_at_cnb, 
             family = "binomial", data = Merged_PNC[Merged_PNC$Race==1,])

summary(m2.EA)
performance::r2(m2.EA)


roc.m2.EA <- 
  


####------------------Add Health----------------####
AddHealthAll = fread("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/FYP/Analysis/Datafile/w1-w4Raw.csv")
AddHealthFull <- read.spss("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Add Health related/reorganize.july2021/PrimaryData.processed.7.26.2021.sav",
                           to.data.frame = TRUE)
AddHealthselected <- c("asbsub1",PC, names(AddHealthFull[1:8]), "CALCAGE1", "ADHD_totalscore","inattention","hyperactivity","COVSUM1","OVTSUM1","PSUSCID","REGION")
AddHealthFull <- AddHealthFull[,AddHealthselected]
##add Depression score at Wave 1 
MDD1 = paste("H1FS",c(6,1,15,16,17,3,7,4,5,11), sep ="")
AddHealthFull <- AddHealthAll %>%
  select(all_of(MDD1),"AID") %>%
  merge(AddHealthFull, by = "AID")
##Add PGSs computed by Q
AddHealthPGS <- read.delim("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Genetic processing/Q lab/AddHealth_set1_traits_other.txt")

selectPGSAddHealth <- data.frame(AddHealthPGS[,c("FID","IID","D0004","D0008","D0009","D0011","D0013","D0020","D0021","D0024","D0025","D0028",
                                "D0029","D0030","D0036","D0039","D0040","D0041","D0044","D0046","D0048","D0051","D0054","D0077",
                                "D0094","D0096","D0105")])

##Link data file
Link = read.table("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/FYP/Analysis/GSEM/My replication/AddHealth dbGap ID Link File.dat",
                  header = TRUE)
tempAddHealthPGSLinked <- merge(Link[,c("AID", "GID")], selectPGSAddHealth, by.x = "GID", by.y = "IID")
###merge PGS with Phenotypes
AddHealthFull <- merge(AddHealthFull,tempAddHealthPGSLinked)

rm(list = c("AddHealthAll","AddHealthPGS","AddHealthASB"))

##rename the PGSs
AddHealthFull <- AddHealthFull %>%
  rename(SCZ_PGS2014 = "D0004",
         PTSD_DuncanEA = "D0008",
         OCD_PGC2017 = "D0009",
         MDD_PGC2018 = "D0011",
         Insom_Hammerschlag_all = "D0013",
         BIP_PGC = "D0020",
         ASD_PGC17EA = "D0021",
         ANX_ANGST_FS = "D0024",
         AN_PGC2017 = "D0025",
         ASD_PGC2019 = "D0028",
         BIP_PGC2018 = "D0029",
         TS_Yu2018 = "D0030",
         SCZ_Pardinas = "D0036",
         ANX_iPSYCH1 = "D0039",
         ANX_iPSYCH2 = "D0040",
         ANX_iPSYCH3 = "D0041",
         CROSS_iPSYCH2019 = "D0044",
         AN_PGC2019 = "D0046",
         PTSD_PGC2019_all = "D0048",
         PTSD_PGC2019_EUR = "D0051",
         PTSD_PGC2019_AA = "D0054",
         MDD_Howard = "D0077",
         Insom_Jansen = "D0094",
         ASD_Matoba2020EUR = "D0096",
         COVID = "D0105")
names(AddHealthFull)

####--------------------ABCD--------------------####
##load data
abcdPath <- "/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/ABCD/"
mergeCol <- c("subjectkey","src_subject_id","interview_date","interview_age","sex","collection_id")

###CBCL
abcd_cbcl <- fread(paste0(abcdPath, "abcd_cbcls01.txt")) %>%
  slice(-1) %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(-c("eventname","dataset_id","study_cohort_name","collection_title"))  ##---> event name is the same for all subject;
########################################dataset id has the same name but different number for each data frame 
########################################so it should not be used as a unique id. 
########################################I think it is the id for the measure as a whole
###KSADS 
abcd_pksads <- fread(paste0(abcdPath, "abcd_ksad01.txt")) %>%
  slice(-1) %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(-c("eventname","dataset_id","study_cohort_name","collection_title"))

####from KSADS, extract only the diagnoses
####then rename to short names for the disorders
######short name -> long name#####
#mdd - major depressive disorder; dysth - dysthymia (persistent depressive disorder); bip1, bip2 (bipolar I and bipolar II); 
#
#####ksads diagnosis extract####
####888 -> not administered due to branching logic from screening items
####555 -> not administered at this wave
abcd_pksadsDiag <- abcd_pksads %>%
  select(c("abcd_ksad01_id","subjectkey","src_subject_id","interview_date","interview_age","sex","collection_id",
           "ksads_1_840_p","ksads_1_841_p","ksads_1_842_p","ksads_1_843_p","ksads_1_844_p","ksads_1_845_p", 
           "ksads_2_830_p","ksads_2_831_p","ksads_2_832_p","ksads_2_833_p","ksads_2_834_p","ksads_2_835_p","ksads_2_836_p","ksads_2_837_p",
           "ksads_3_848_p","ksads_4_826_p","ksads_4_827_p","ksads_4_828_p","ksads_4_829_p","ksads_4_849_p","ksads_4_850_p",
           "ksads_5_857_p","ksads_5_858_p",	"ksads_6_859_p","ksads_6_860_p","ksads_7_861_p","ksads_7_862_p","ksads_8_863_p","ksads_8_864_p",
           "ksads_9_867_p","ksads_9_868_p","ksads_10_869_p","ksads_10_870_p","ksads_11_917_p","ksads_11_918_p",
           "ksads_12_925_p","ksads_12_926_p","ksads_12_927_p","ksads_12_928_p",
           "ksads_13_930_p","ksads_13_931_p","ksads_13_932_p","ksads_13_933_p","ksads_13_934_p","ksads_13_935_p",
           "ksads_13_936_p","ksads_13_937_p","ksads_13_938_p","ksads_13_939_p","ksads_13_940_p",
           "ksads_14_853_p","ksads_14_854_p","ksads_14_855_p","ksads_14_856_p",
           "ksads_15_901_p","ksads_15_902_p","ksads_16_897_p","ksads_16_898_p","ksads_16_899_p","ksads_16_900_p",
           "ksads_17_904_p","ksads_17_905_p", "ksads_18_903_p",
           "ksads_19_891_p","ksads_19_892_p","ksads_19_895_p","ksads_19_896_p","ksads_20_889_p","ksads_20_890_p",
           "ksads_21_921_p","ksads_21_922_p","ksads_22_970_p","ksads_22_969_p",
           "ksads_23_945_p","ksads_23_946_p","ksads_23_954_p","ksads_23_956_p","ksads_23_957_p","ksads_23_965_p",
           "ksads_24_967_p","ksads_24_968_p","ksads_25_865_p","ksads_25_866_p")) %>%
  rename(ksads_mdd_crt = ksads_1_840_p, ksads_mdd_rmi = ksads_1_841_p, ksads_mdd_pst = ksads_1_842_p,
         ksads_dysth_crt = ksads_1_843_p, ksads_dysth_rmi = ksads_1_844_p, ksads_dysth_pst = ksads_1_845_p,
         ksads_bip1man_crt = ksads_2_830_p, ksads_bip1dep_crt =  ksads_2_831_p, ksads_bip1hypoman_crt = ksads_2_832_p, 
         ksads_bip1man_pst = ksads_2_833_p, ksads_bip1dep_pst = ksads_2_834_p, ksads_bip2hypoman_crt = ksads_2_835_p, ksads_bip2dep_crt = ksads_2_836_p, ksads_bip2hypoman_pst = ksads_2_837_p,
         ksads_dmdd_crt = ksads_3_848_p,
         ksads_hallu_crt = ksads_4_826_p, ksads_hallu_pst = ksads_4_827_p,
         ksads_delu_crt = ksads_4_828_p, ksads_delu_pst = ksads_4_829_p,
         ksads_asspsy_crt = ksads_4_849_p, ksads_asspsy_pst = ksads_4_850_p, 
         ksads_pan_crt = ksads_5_857_p, ksads_pan_pst = ksads_5_858_p,
         ksads_agr_crt = ksads_6_859_p, ksads_agr_pst = ksads_6_860_p,
         ksads_sep_crt = ksads_7_861_p, ksads_sep_pst = ksads_7_862_p, 
         ksads_soc_crt = ksads_8_863_p, ksads_soc_pst = ksads_8_864_p, 
         ksads_pho_crt = ksads_9_867_p, ksads_pho_pst = ksads_9_868_p,
         ksads_gad_crt = ksads_10_869_p, ksads_gad_pst = ksads_10_870_p,
         ksads_ocd_crt = ksads_11_917_p, ksads_ocd_pst = ksads_11_918_p,
         ksads_enure_crt = ksads_12_925_p, ksads_enure_pst = ksads_12_926_p, 
         ksads_encop_crt = ksads_12_927_p, ksads_encop_pst = ksads_12_928_p, 
         ksads_an_binpur_rmi = ksads_13_930_p, ksads_an_binpur_pst = ksads_13_931_p, 
         ksads_an_rest_crt = ksads_13_932_p, ksads_an_rest_rmi = ksads_13_933_p, ksads_an_rest_pst = ksads_13_934_p, 
         ksads_bulim_crt = ksads_13_935_p, ksads_bulim_pst = ksads_13_936_p, ksads_bulim_rmi = ksads_13_937_p,
         ksads_binge_crt = ksads_13_938_p, ksads_binge_rmi = ksads_13_939_p, ksads_binge_pst = ksads_13_940_p, 
         ksads_adhd_crt = ksads_14_853_p, ksads_adhd_pst = ksads_14_854_p, ksads_adhd_rmi = ksads_14_855_p, ksads_adhd_unspe = ksads_14_856_p, 
         ksads_odd_crt = ksads_15_901_p, ksads_odd_pst = ksads_15_902_p, 
         ksads_cd_crt_child = ksads_16_897_p, ksads_cd_crt_adol = ksads_16_898_p, ksads_cd_pst_child = ksads_16_899_p, ksads_cd_pst_adol = ksads_16_900_p, 
         ksads_tic_unspe_crt = ksads_17_904_p, ksads_tic_unspe_pst = ksads_17_905_p, 
         ksads_asd_unspe = ksads_18_903_p,
         ksads_aud_crt = ksads_19_891_p, ksads_aud_pst = ksads_19_892_p, ksads_alc_unspe_crt = ksads_19_895_p, ksads_alc_unspe_pst = ksads_19_896_p,
         ksads_sud_crt = ksads_20_889_p, ksads_sud_pst = ksads_20_890_p,
         ksads_ptsd_crt = ksads_21_921_p, ksads_ptsd_pst = ksads_21_922_p,
         ksads_slp_crt = ksads_22_969_p, ksads_slp_pst = ksads_22_970_p, 
         ksads_sinjbeh_crt = ksads_23_945_p, ksads_sipass_crt = ksads_23_946_p, ksads_satt_crt = ksads_23_954_p,
         ksads_sinjbeh_pst = ksads_23_956_p, ksads_sipass_pst = ksads_23_957_p, ksads_satt_pst = ksads_23_965_p,
         ksads_hom_crt = ksads_24_967_p, ksads_hom_pst = ksads_24_968_p, 
         ksads_selmutism_crt = ksads_25_865_p, ksads_selmutism_pst = ksads_25_866_p) 

#replace 555 with NA
abcd_pksadsDiag <- abcd_pksadsDiag %>%
  na_if(555)


#replace 888 with NA  -> but see Barch et al., pg. 10, 888 means the questions were not asked due to branching logic, based on screening items; so these 888 should be coded as "absent"
abcd_pksadsDiag <- abcd_pksadsDiag %>%
  mutate(across(starts_with("ksads"), ~recode(., '888' = '0')
                ))


###ABCLS -> only available for 2 year follow up
abcd_abcls <- fread(paste0(abcdPath, "abcd_abcls01.txt")) %>%
  slice(-1) %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  select(-c("eventname","dataset_id","study_cohort_name","collection_title"))

###parent demographic
abcd_pdemo <- fread(paste0(abcdPath, "pdem02.txt")) %>%
  slice(-1) %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(-c("eventname","dataset_id","study_cohort_name","collection_title")) %>%
  select(all_of(mergeCol),"demo_prim","demo_brthdat_v2","demo_ed_v2","demo_sex_v2",
         starts_with("demo_race_a_p"),"demo_ethn_v2","demo_ethn2_v2",
         "demo_prnt_age_v2","demo_prnt_gender_id_v2","demo_prnt_marital_v2","demo_prnt_ed_v2",
         "demo_prnt_empl_v2","demo_prnt_empl_time","demo_prnt_income_v2","demo_prnt_prtnr_v2",
         "demo_prnt_prtnr_bio","demo_prtnr_ed_v2","demo_prtnr_empl_v2","demo_prtnr_empl_time",
         "demo_prtnr_income_v2","demo_comb_income_v2",starts_with("demo_fam_exp"))
         

###medical history and service utilization --> only at 1 year follow up and 2 year follow up
abcd_medServ <- fread(paste0(abcdPath, "abcd_lpmh01.txt")) %>%
  slice(-1) %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  select(-c("eventname","dataset_id","study_cohort_name","collection_title"))

##anthropometric measure
abcd_anthro <- fread(paste0(abcdPath, "abcd_ant01.txt")) %>%
  slice(-1) %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  select(-c("eventname","dataset_id","study_cohort_name","collection_title"))

rm(list = c("abcd_pksads"))

##merge files into one dataframe
abcd_merged <- abcd_cbcl %>%
  full_join(abcd_pksadsDiag, by = mergeCol) %>%
  full_join(abcd_anthro,by = mergeCol) %>%
  full_join(abcd_pdemo,by = mergeCol)

abcd_merged %>%
  glimpse()

##make numeric data into numeric
abcd_merged[,c(6, 8:230)] <- abcd_merged[,c(6, 8:230)] %>%
  lapply(as.numeric) %>%
  as.data.frame()

##recode sex into 0 (M), and 1 (F) 
abcd_merged$sex <- recode(abcd_merged$sex, 'M' = 0, 'F' = 1)

##explore the merged data; some descriptives
abcd_merged %>%
  select(starts_with("ksads")) %>%
  map(~table(.x))


##how to get factor scores from bass ackward method for EFA

bassAckward(Thurstone,4,main="Thurstone data set")
f.labels <- list(level1=cs(Approach,Avoid),level2=cs(PosAffect,NegAffect,Constraint), 
                 level3 = cs(Extraversion,Agreeableness,Neuroticism,Conscientiousness,Openness))

ba <- bassAckward(psychTools::bfi[1:25],nfactor = c(2,3,5),
                  main="bfi data set from psychTools")

print(ba,short=FALSE)

factor.scores(psychTools::bfi[1:25], ba$fa[[1]], method = "tenBerge")




