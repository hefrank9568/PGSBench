options(scipen = 999) ##turn off scientific notation
library(ggplot2)
####Violin plot for both groups####
pnc_rsq <- readxl::read_xlsx(path = "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/violinPlot_bygroup.xlsx",
                             sheet = "pnc")
pnc_rsq$`HiTOP/disorder` <- as.factor(pnc_rsq$`HiTOP/disorder`)
pnc_rsq$Race<- as.factor(pnc_rsq$Race)


violin_pnc <- ggplot(pnc_rsq, aes(x=`HiTOP/disorder`, y=r2, fill=Race)) + 
  geom_violin(trim=TRUE)+
  stat_summary(fun=mean, geom="point", size=2, position = position_dodge(.9), color = "yellow") + 
  scale_x_discrete(
    limits = c("HiTOP","Disorder","HiTOP PGS","Disorder PGS"),
    labels = c(
    "Disorder" = "Disorder ~ Disorder PGS",
    "Disorder PGS" = "HiTOP ~ Disorder PGS",
    "HiTOP" = "HiTOP ~ HiTOP PGS",
    "HiTOP PGS" = "Disorder ~ HiTOP PGS"
    
  )) + 
  geom_point(position = position_dodge(.9)) + 
  labs(title="Plot of rsq by disorder and race",x=" ", y = "R-squared") + 
  theme_classic()
violin_pnc

violin_pnc_cov <- ggplot(pnc_rsq, aes(x=`HiTOP/disorder`, y=r2_cov, fill=Race)) + 
  geom_violin(trim=TRUE)+
  stat_summary(fun=mean, geom="point", size=2, position = position_dodge(.9), color = "yellow") + 
  scale_x_discrete(
    limits = c("HiTOP","Disorder"),
    labels = c(
      "Disorder" = "Disorder ~ Disorder PGS",
      "HiTOP" = "HiTOP ~ HiTOP PGS"
    )) + 
  geom_point(position = position_dodge(.9)) + 
  labs(title="Plot of rsq explained by covariates by disorder and race",x=" ", y = "R-squared") + 
  theme_classic()
violin_pnc_cov


rm(list = ls())
####Model Parameters for matched PGS-disorder pairs####
##model parameters in full sample
##remove individual PGS files
envList <- ls(globalenv())
rm( list = c(envList[startsWith(envList,"pnc_")],"TradADHDPGS"))
rm(list = c(envList[startsWith(envList,"R2_")]))

summariesselect_pncall <- lapply(all_select_pnc, summary)

pnc_all_para_select <- lapply(1:4, function(x)
  lapply(1:length(all_select_pnc), function(y) summariesselect_pncall[[y]]$coefficients[2,x]))
parameters_pgsselect_all <- data.frame(PGS = selectdataNames_pnc$Var2,
                                  Trait = selectdataNames_pnc$Var1,
                                  coef = unlist(pnc_all_para_select[1]),
                                  se = unlist(pnc_all_para_select[2]),
                                  t = unlist(pnc_all_para_select[3]),
                                  p = unlist(pnc_all_para_select[4]))

parameters_pgsselect_all$p.adjust = p.adjust(parameters_pgsselect_all$p, "fdr")
parameters_pgsselect_all$r2 = R2select_diff_pnc[seq(from = 2, to=length(R2select_diff_pnc), by = 2)]

for (i in seq(from = 1, to = length(R2select_cov_pnc), by = 1)) {
  parameters_pgsselect_all$r2_cov[i] = R2select_cov_pnc[[i]]$R2_adjusted
}

write.csv(parameters_pgsselect_all, file = "/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_pnc_select_all.csv")
rm(list = c("summariesselect_pncall"))

#model parameters in EA 
summariesselect_pncEA <- lapply(EAselect_pnc, summary)
pnc_easelect_para <- lapply(1:4, function(x)
  lapply(1:length(EAselect_pnc), function(y) summariesselect_pncEA[[y]]$coefficients[2,x]))

parameters_pgsselect_ea <- data.frame(PGS = selectdataNames_pnc$Var2,
                                 Trait = selectdataNames_pnc$Var1,
                                 coef = unlist(pnc_easelect_para[1]),
                                 se = unlist(pnc_easelect_para[2]),
                                 t = unlist(pnc_easelect_para[3]),
                                 p = unlist(pnc_easelect_para[4]))
parameters_pgsselect_ea$p.adjust = p.adjust(parameters_pgsselect_ea$p, "fdr")
parameters_pgsselect_ea$r2 = R2select_diff_EA_pnc[seq(from = 2, to=length(R2select_diff_EA_pnc), by = 2)]


for (i in seq(from = 1, to = length(R2select_EA_cov_pnc), by = 1)) {
  parameters_pgsselect_ea$r2_cov[i] = R2select_EA_cov_pnc[[i]]$R2_adjusted
}

write.csv(parameters_pgsselect_ea, file = "/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_pnc_select_ea.csv")
rm(list = c("summariesselect_pncEA"))

#model parameters in AA
summariesselect_pncAA <- lapply(AAselect_pnc, summary)
pnc_aaselect_para <- lapply(1:4, function(x)
  lapply(1:length(AAselect_pnc), function(y) summariesselect_pncAA[[y]]$coefficients[2,x]))

parameters_pgsselect_aa <- data.frame(PGS = selectdataNames_pnc$Var2,
                                 Trait = selectdataNames_pnc$Var1,
                                 coef = unlist(pnc_aaselect_para[1]),
                                 se = unlist(pnc_aaselect_para[2]),
                                 t = unlist(pnc_aaselect_para[3]),
                                 p = unlist(pnc_aaselect_para[4]))

parameters_pgsselect_aa$p.adjust = p.adjust(parameters_pgsselect_aa$p, "fdr")
parameters_pgsselect_aa$r2 = R2select_diff_AA_pnc[seq(from = 2, to=length(R2select_diff_AA_pnc), by = 2)]
for (i in seq(from = 1, to = length(R2select_AA_cov_pnc), by = 1)) {
  parameters_pgsselect_aa$r2_cov[i] = R2select_AA_cov_pnc[[i]]$R2_adjusted
}

parameters_pgsselect_aa$r2/parameters_pgsselect_aa$r2_cov
write.csv(parameters_pgsselect_aa, file = "/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_pncselect_aa.csv")
rm(list = c("summariesselect_pncAA"))

####parameters forest plot####
library(forestplot)
##https://cran.r-project.org/web/packages/forestplot/vignettes/forestplot.html#summary-lines
##better organize disorder trait object 

eabase_data <- tibble(mean = parameters_pgsselect_ea$coef,
                      lower = parameters_pgsselect_ea$coef - 1.96 * parameters_pgsselect_ea$se,
                      upper = parameters_pgsselect_ea$coef + 1.96 * parameters_pgsselect_ea$se,
                      disorder = parameters_pgsselect_ea$Trait,
                      pgs = parameters_pgsselect_ea$PGS, 
                      beta = format(round(parameters_pgsselect_ea$coef, digits = 3)),
                      p = format(round(parameters_pgsselect_ea$p.adjust, digits = 3)))

###order the beta's by ascending order
eabase_data[eabase_data$disorder=="ODD_SX",] <- eabase_data[order(eabase_data[eabase_data$disorder=="ODD_SX",]$mean),]
eabase_data[eabase_data$disorder=="CDD_SX",] <- eabase_data[which(eabase_data$disorder=="CDD_SX")[order(eabase_data[eabase_data$disorder=="CDD_SX",]$mean)],]
eabase_data[eabase_data$disorder=="ADD_TSX",] <- eabase_data[which(eabase_data$disorder=="ADD_TSX")[order(eabase_data[eabase_data$disorder=="ADD_TSX",]$mean)],]
eabase_data[eabase_data$disorder=="PTD_Int",] <- eabase_data[which(eabase_data$disorder=="PTD_Int")[order(eabase_data[eabase_data$disorder=="PTD_Int",]$mean)],]
eabase_data[eabase_data$disorder=="OCD_TSX",] <- eabase_data[which(eabase_data$disorder=="OCD_TSX")[order(eabase_data[eabase_data$disorder=="OCD_TSX",]$mean)],]
eabase_data[eabase_data$disorder=="MAN_TSX",] <- eabase_data[which(eabase_data$disorder=="MAN_TSX")[order(eabase_data[eabase_data$disorder=="MAN_TSX",]$mean)],]
eabase_data[eabase_data$disorder=="Psychosis",] <- eabase_data[which(eabase_data$disorder=="Psychosis")[order(eabase_data[eabase_data$disorder=="Psychosis",]$mean)],]

eabase_data[eabase_data$disorder=="GAD_TSX",] <- eabase_data[which(eabase_data$disorder=="GAD_TSX")[order(eabase_data[eabase_data$disorder=="GAD_TSX",]$mean)],]
eabase_data[eabase_data$disorder=="SEP_SX",] <- eabase_data[which(eabase_data$disorder=="SEP_SX")[order(eabase_data[eabase_data$disorder=="SEP_SX",]$mean)],]
eabase_data[eabase_data$disorder=="SOC_SX",] <- eabase_data[which(eabase_data$disorder=="SOC_SX")[order(eabase_data[eabase_data$disorder=="SOC_SX",]$mean)],]
eabase_data[eabase_data$disorder=="PHB_SX",] <- eabase_data[which(eabase_data$disorder=="PHB_SX")[order(eabase_data[eabase_data$disorder=="PHB_SX",]$mean)],]
eabase_data[eabase_data$disorder=="PAN_SX",] <- eabase_data[which(eabase_data$disorder=="PAN_SX")[order(eabase_data[eabase_data$disorder=="PAN_SX",]$mean)],]
eabase_data[eabase_data$disorder=="AGR_SX",] <- eabase_data[which(eabase_data$disorder=="AGR_SX")[order(eabase_data[eabase_data$disorder=="AGR_SX",]$mean)],]

eabase_data[eabase_data$disorder=="DEP_TSX",] <- eabase_data[which(eabase_data$disorder=="DEP_TSX")[order(eabase_data[eabase_data$disorder=="DEP_TSX",]$mean)],]
eabase_data[eabase_data$disorder=="EAT_TotSX",] <- eabase_data[which(eabase_data$disorder=="EAT_TotSX")[order(eabase_data[eabase_data$disorder=="EAT_TotSX",]$mean)],]

eabase_data[eabase_data$disorder=="G",] <- eabase_data[which(eabase_data$disorder=="G")[order(eabase_data[eabase_data$disorder=="G",]$mean)],]
eabase_data[eabase_data$disorder=="INT",] <- eabase_data[which(eabase_data$disorder=="INT")[order(eabase_data[eabase_data$disorder=="INT",]$mean)],]
eabase_data[eabase_data$disorder=="EXT",] <- eabase_data[which(eabase_data$disorder=="EXT")[order(eabase_data[eabase_data$disorder=="EXT",]$mean)],]
eabase_data[eabase_data$disorder=="THOUGHT",] <- eabase_data[which(eabase_data$disorder=="THOUGHT")[order(eabase_data[eabase_data$disorder=="THOUGHT",]$mean)],]

##only keep the first occurrence of the trait
eabase_data$disorder[which(eabase_data$disorder == "ODD_SX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "CDD_SX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "ADD_TSX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "PTD_Int")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "OCD_TSX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "MAN_TSX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "Psychosis")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "GAD_TSX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "SEP_SX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "SOC_SX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "PHB_SX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "PAN_SX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "AGR_SX")[-1]] = " "

eabase_data$disorder[which(eabase_data$disorder == "DEP_TSX")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "EAT_TotSX")[-1]] = " "

eabase_data$disorder[which(eabase_data$disorder == "G")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "INT")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "EXT")[-1]] = " "
eabase_data$disorder[which(eabase_data$disorder == "THOUGHT")[-1]] = " "

easummary <- tibble(mean = mean(eabase_data$mean),
                    lower = mean(eabase_data$lower),
                    upper = mean(eabase_data$upper),
                    disorder = "Summary",
                    pgs = "",
                    beta = c(format(mean(eabase_data$mean), digits = 3)),
                    summary = TRUE)

eaheader <- tibble(disorder = c("","Disorder"), 
                   pgs = c("","PGS"),
                   beta = c("","beta"),
                   p = c("","p"),
                   summary = TRUE)

eaempty_row <- tibble(mean = NA_real_)

ea_selectpara_df <- bind_rows(eaheader,
                              eabase_data,
                              eaempty_row,
                              easummary)

ea_selectpara_df %>% 
  forestplot(labeltext = c(disorder, pgs, beta, p), 
             is.summary = summary,
             clip = c(-0.15, 0.35), 
             xlog = FALSE, 
             col = fpColors(box = "royalblue",
                            line = "darkblue",
                            summary = "royalblue"),
             txt_gp = fpTxtGp(cex = 0.7))
####AA
aabase_data <- tibble(mean = parameters_pgsselect_aa$coef,
                      lower = parameters_pgsselect_aa$coef - 1.96 * parameters_pgsselect_aa$se,
                      upper = parameters_pgsselect_aa$coef + 1.96 * parameters_pgsselect_aa$se,
                      disorder = parameters_pgsselect_aa$Trait,
                      pgs = parameters_pgsselect_aa$PGS, 
                      beta = format(round(parameters_pgsselect_aa$coef, digits = 3)),
                      p = format(round(parameters_pgsselect_aa$p.adjust, digits = 3)))

###order the beta's by ascending order
aabase_data[aabase_data$disorder=="ODD_SX",] <- aabase_data[order(aabase_data[aabase_data$disorder=="ODD_SX",]$mean),]
aabase_data[aabase_data$disorder=="CDD_SX",] <- aabase_data[which(aabase_data$disorder=="CDD_SX")[order(aabase_data[aabase_data$disorder=="CDD_SX",]$mean)],]
aabase_data[aabase_data$disorder=="ADD_TSX",] <- aabase_data[which(aabase_data$disorder=="ADD_TSX")[order(aabase_data[aabase_data$disorder=="ADD_TSX",]$mean)],]
aabase_data[aabase_data$disorder=="PTD_Int",] <- aabase_data[which(aabase_data$disorder=="PTD_Int")[order(aabase_data[aabase_data$disorder=="PTD_Int",]$mean)],]
aabase_data[aabase_data$disorder=="OCD_TSX",] <- aabase_data[which(aabase_data$disorder=="OCD_TSX")[order(aabase_data[aabase_data$disorder=="OCD_TSX",]$mean)],]
aabase_data[aabase_data$disorder=="MAN_TSX",] <- aabase_data[which(aabase_data$disorder=="MAN_TSX")[order(aabase_data[aabase_data$disorder=="MAN_TSX",]$mean)],]
aabase_data[aabase_data$disorder=="Psychosis",] <- aabase_data[which(aabase_data$disorder=="Psychosis")[order(aabase_data[aabase_data$disorder=="Psychosis",]$mean)],]

aabase_data[aabase_data$disorder=="GAD_TSX",] <- aabase_data[which(aabase_data$disorder=="GAD_TSX")[order(aabase_data[aabase_data$disorder=="GAD_TSX",]$mean)],]
aabase_data[aabase_data$disorder=="SEP_SX",] <- aabase_data[which(aabase_data$disorder=="SEP_SX")[order(aabase_data[aabase_data$disorder=="SEP_SX",]$mean)],]
aabase_data[aabase_data$disorder=="SOC_SX",] <- aabase_data[which(aabase_data$disorder=="SOC_SX")[order(aabase_data[aabase_data$disorder=="SOC_SX",]$mean)],]
aabase_data[aabase_data$disorder=="PHB_SX",] <- aabase_data[which(aabase_data$disorder=="PHB_SX")[order(aabase_data[aabase_data$disorder=="PHB_SX",]$mean)],]
aabase_data[aabase_data$disorder=="PAN_SX",] <- aabase_data[which(aabase_data$disorder=="PAN_SX")[order(aabase_data[aabase_data$disorder=="PAN_SX",]$mean)],]
aabase_data[aabase_data$disorder=="AGR_SX",] <- aabase_data[which(aabase_data$disorder=="AGR_SX")[order(aabase_data[aabase_data$disorder=="AGR_SX",]$mean)],]

aabase_data[aabase_data$disorder=="DEP_TSX",] <- aabase_data[which(aabase_data$disorder=="DEP_TSX")[order(aabase_data[aabase_data$disorder=="DEP_TSX",]$mean)],]
aabase_data[aabase_data$disorder=="EAT_TotSX",] <- aabase_data[which(aabase_data$disorder=="EAT_TotSX")[order(aabase_data[aabase_data$disorder=="EAT_TotSX",]$mean)],]

aabase_data[aabase_data$disorder=="G",] <- aabase_data[which(aabase_data$disorder=="G")[order(aabase_data[aabase_data$disorder=="G",]$mean)],]
aabase_data[aabase_data$disorder=="INT",] <- aabase_data[which(aabase_data$disorder=="INT")[order(aabase_data[aabase_data$disorder=="INT",]$mean)],]
aabase_data[aabase_data$disorder=="EXT",] <- aabase_data[which(aabase_data$disorder=="EXT")[order(aabase_data[aabase_data$disorder=="EXT",]$mean)],]
aabase_data[aabase_data$disorder=="THOUGHT",] <- aabase_data[which(aabase_data$disorder=="THOUGHT")[order(aabase_data[aabase_data$disorder=="THOUGHT",]$mean)],]

##only keep the first occurrent of the trait
aabase_data$disorder[which(aabase_data$disorder == "ODD_SX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "CDD_SX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "ADD_TSX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "PTD_Int")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "OCD_TSX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "MAN_TSX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "Psychosis")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "GAD_TSX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "SEP_SX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "SOC_SX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "PHB_SX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "PAN_SX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "AGR_SX")[-1]] = " "

aabase_data$disorder[which(aabase_data$disorder == "DEP_TSX")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "EAT_TotSX")[-1]] = " "

aabase_data$disorder[which(aabase_data$disorder == "G")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "INT")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "EXT")[-1]] = " "
aabase_data$disorder[which(aabase_data$disorder == "THOUGHT")[-1]] = " "


aasummary <- tibble(mean = mean(aabase_data$mean),
                    lower = mean(aabase_data$lower),
                    upper = mean(aabase_data$upper),
                    disorder = "Summary",
                    pgs = "",
                    beta = c(format(mean(aabase_data$mean), digits = 2)),
                    summary = TRUE)

aaheader <- tibble(disorder = c("","Disorder"), 
                   pgs = c("","PGS"),
                   beta = c("","beta"),
                   p = c("","p"),
                   summary = TRUE)

aaempty_row <- tibble(mean = NA_real_)

aa_selectpara_df <- bind_rows(aaheader,
                              aabase_data,
                              aaempty_row,
                              aasummary)

aa_selectpara_df%>% 
  forestplot(labeltext = c(disorder, pgs, beta,p), 
             is.summary = summary,
             clip = c(-0.10, 0.20), 
             xlog = FALSE, 
             col = fpColors(box = "royalblue",
                            line = "darkblue",
                            summary = "royalblue"),
             txt_gp = fpTxtGp(cex = 0.7))


####PGS bivariate associations####
PGS_PNC_cor <- c("pgs_adhd_c","pgs_ts_c","pgs_asd_c",
             "pgs_ocd_c","pgs_bip_c","pgs_scz_c",
             "pgs_ptsdgsem_c","pgs_ptsdafr_c",
             "pgs_mdd_c","pgs_anx_c","pgs_an_c","pgs_extcons_c",
             "pgs_int_irwin_c","pgs_ext_irwin_c","pgs_ndd_irwin_c","pgs_tp_irwin_c",
             "pgs_neuro_gsem_c","pgs_sub_gsem_c","pgs_int_gsem_c","pgs_tht_gsem_c")
##for all 
all_pgsCorr <- Merged_PNC %>% 
  select(all_of(PGS_PNC_cor)) %>%
 corr.test()

corrplot::corrplot(all_pgsCorr$r,diag = FALSE,type = "lower",method = "number", order = "hclust", tl.srt = 45,
                   p.mat = all_pgsCorr$p, sig.level = 0.05/length(all_of(PGS_PNC_cor)), insig = "label_sig",pch = "*",pch.cex = 2,pch.col = "grey",number.cex = 1.2
                   )

##for EA 
EA_pgsCorr <- Merged_PNC %>% 
  filter(Race == 1) %>%
  select(all_of(PGS_PNC_cor)) %>%
  corr.test()

corrplot::corrplot(EA_pgsCorr$r,diag = FALSE,type = "lower",method = "number", order = "hclust", tl.srt = 40,
                   p.mat = EA_pgsCorr$p, sig.level = 0.05/length(all_of(PGS_PNC_cor)), insig = "label_sig",pch = "*",pch.cex = 1,pch.col = "grey",number.cex = 0.9)


##for AA
AA_pgsCorr <- Merged_PNC %>% 
  filter(Race == 0) %>%
  select(all_of(PGS_PNC_cor)) %>%
  corr.test()

corrplot::corrplot(AA_pgsCorr$r,diag = FALSE,type = "lower",method = "number", order = "hclust", tl.srt = 45,
                   p.mat = AA_pgsCorr$p, sig.level = 0.05/length(all_of(PGS_PNC_cor)), insig = "label_sig",pch = "*",pch.cex = 1,pch.col = "grey",number.cex = 0.9)


####R-squared figure####
##plot R-2 in AA and EA 

##load parameters from existing csv files; do not run the parameters summary again because they take up a lot of storage/memory
parameters_pgsselect_ea <- read.csv("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_pnc_select_ea.csv")
parameters_pgsselect_aa <- read.csv("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_pncselect_aa.csv")
parameters_pgsselect_all <- read.csv("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_pnc_select_all.csv")

EA_namedR2_select <- cbind(selectdataNames_pnc, R2select_diff_EA_pnc)
EA_namedR2_select <- cbind(EA_namedR2_select, parameters_pgsselect_ea$p.adjust)

AA_namedR2_select <- cbind(selectdataNames_pnc, R2select_diff_AA_pnc)
AA_namedR2_select <- cbind(AA_namedR2_select, parameters_pgsselect_aa$p.adjust)

nagelkerkeselect_diff_bygroup_pnc <- R2select_diff_EA_pnc - R2select_diff_AA_pnc
bygroupdiff_namednagelkerkeselect <- cbind(selectdataNames_pnc, nagelkerkeselect_diff_bygroup_pnc)

###in EA
##color coding: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
####see Hexadecimal color code chart
library(ggsci)
ggplot(data = EA_namedR2_select, aes(x=Var1, y = nagelkerkeselect_EA_diff_pnc, fill = Var2,
                                     label = ifelse(`parameters_pgsselect_ea$p.adjust` <.05, "*", " "))) + 
  geom_bar(stat = "identity",
           position = position_dodge(.9)) + 
  geom_text(vjust=-0.1, position = position_dodge(.9), size = 6) +
  scale_x_discrete(limits= c("PAN_SX","AGR_SX","PHB_SX","SOC_SX","SEP_SX","GAD_TSX","DEP_TSX","EAT_TotSX","PTD_Int",
                             "ADD_TSX","ODD_SX","CDD_SX","Psychosis","MAN_TSX","OCD_TSX","G","INT","EXT","THOUGHT"),
                   labels = c("PAN","AGR","PHB","SOC","SEP","GAD","MDD","AN","PTSD","ADHD","ODD","CDD","PSY","MAN","OCD",
                              "G","INT","EXT","THT")) + 
  scale_fill_manual(breaks = c("pgs_adhd_c","pgs_ts_c","pgs_asd_c",
                               "pgs_ocd_c","pgs_bip_c","pgs_scz_c",
                               "pgs_ptsdgsem_c","pgs_ptsdafr_c",
                               "pgs_mdd_c","pgs_anx_c","pgs_an_c","pgs_extcons_c","pgs_comp_gsem_c",
                               "pgs_neuro_gsem_c","pgs_tht_gsem_c","pgs_int_gsem_c"),
                    values = c("#CCFF00","#99FF00","#66FF00",
                               "#00FFCC","#33FFCC","#00FFFF",
                               "#FF9900","#FF6600",
                               "#FFCCFF","#FF99FF","#FF66FF",
                               "#3300CC","#CCCCCC","#666666","#000000","#FFCC00")) +
  xlab("") + 
  scale_y_continuous(breaks = seq(.0, .045, by = 0.005), limits = c(.0, .045)) + 
  ylab("Incremental R2 in EA in PNC") + 
  theme_classic(base_size = 15)

###in AA
ggplot(data = AA_namedR2_select, aes(x=Var1, y = nagelkerkeselect_AA_diff_pnc, fill = Var2,
                                     label = ifelse(`parameters_pgsselect_aa$p.adjust`  <.05, "*", " "))) + 
  geom_bar(stat = "identity",
           position = position_dodge(.9)) +
  geom_text(vjust=-0.1, position = position_dodge(.9), size = 6) +
  scale_x_discrete(limits= c("PAN_SX","AGR_SX","PHB_SX","SOC_SX","SEP_SX","GAD_TSX","DEP_TSX","EAT_TotSX","PTD_Int",
                             "ADD_TSX","ODD_SX","CDD_SX","Psychosis","MAN_TSX","OCD_TSX","G","INT","EXT","THOUGHT"),
                   labels = c("PAN","AGR","PHB","SOC","SEP","GAD","MDD","AN","PTSD","ADHD","ODD","CDD","PSY","MAN","OCD",
                              "G","INT","EXT","THT")) + 
  scale_fill_manual(breaks = c("pgs_adhd_c","pgs_ts_c","pgs_asd_c",
                               "pgs_ocd_c","pgs_bip_c","pgs_scz_c",
                               "pgs_ptsdgsem_c","pgs_ptsdafr_c",
                               "pgs_mdd_c","pgs_anx_c","pgs_an_c","pgs_extcons_c","pgs_comp_gsem_c",
                               "pgs_neuro_gsem_c","pgs_tht_gsem_c","pgs_int_gsem_c"),
                    values = c("#CCFF00","#99FF00","#66FF00",
                               "#00FFCC","#33FFCC","#00FFFF",
                               "#FF9900","#FF6600",
                               "#FFCCFF","#FF99FF","#FF66FF",
                               "#3300CC","#CCCCCC","#666666","#000000","#FFCC00")) +
  xlab("") + 
  scale_y_continuous(breaks = seq(.0, .045, by = 0.005), limits = c(.0, .045)) +
  ylab("Incremental R2 in AA in PNC") + 
  theme_classic(base_size = 15)


