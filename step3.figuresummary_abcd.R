####Violin plot for both groups####
library(ggplot2)
abcd_rsq <- readxl::read_xlsx(path = "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/violinPlot_bygroup.xlsx",
                              sheet = "abcd")
abcd_rsq$`HiTOP/disorder` <- as.factor(abcd_rsq$`HiTOP/disorder`)
abcd_rsq$Race<- as.factor(abcd_rsq$Race)


violin_abcd <- ggplot(abcd_rsq, aes(x=`HiTOP/disorder`, y=r2, fill=Race)) + 
  geom_violin(trim=TRUE)+
  stat_summary(fun=mean, geom="point", size=2, position = position_dodge(.9), color = "yellow") + 
  scale_x_discrete(
    limits = c("HiTOP","Disorder"),
    labels = c(
      "Disorder" = "Disorder ~ Disorder PGS",
      "HiTOP" = "HiTOP ~ HiTOP PGS"
      
    )) + 
  geom_point(position = position_dodge(.9)) + 
  labs(title="Plot of rsq by disorder and race",x=" ", y = "R-squared") + 
  theme_classic()
violin_abcd

violin_abcd_cov <- ggplot(abcd_rsq, aes(x=`HiTOP/disorder`, y=r2_cov, fill=Race)) + 
  geom_violin(trim=TRUE)+
  stat_summary(fun=mean, geom="point", size=2, position = position_dodge(.9), color = "yellow") + 
  scale_x_discrete(
    limits = c("HiTOP","Disorder"),
    labels = c(
      "Disorder" = "Disorder ~ Disorder PGS",
      "HiTOP" = "HiTOP ~ HiTOP PGS"
    )) + 
  geom_point(position = position_dodge(.9)) + 
  labs(title="Plot of rsq by disorder and race",x=" ", y = "R-squared") + 
  theme_classic()
violin_abcd_cov


mean(abcd_rsq$r2[abcd_rsq$Race=="European American" & abcd_rsq$`HiTOP/disorder` =="Disorder"])
mean(abcd_rsq$r2[abcd_rsq$Race=="African American" & abcd_rsq$`HiTOP/disorder` =="Disorder"])
mean(abcd_rsq$r2[abcd_rsq$Race=="European American" & abcd_rsq$`HiTOP/disorder` =="HiTOP"])
mean(abcd_rsq$r2[abcd_rsq$Race=="African American" & abcd_rsq$`HiTOP/disorder` =="HiTOP"])


####load parameter files for cbcl and ksads####
parameters_abcd_cbcl_ea <- read.csv("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_ea.csv")
parameters_abcd_cbcl_aa <- read.csv("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_aa.csv")
parameters_abcd_cbcl_hisp <- read.csv("/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_hisp.csv")

####abcd parameter forest plot####
####parameters forest plot####
library(forestplot)
library(dplyr)
##https://cran.r-project.org/web/packages/forestplot/vignettes/forestplot.html#summary-lines
##better organize disorder trait object 

eabase_abcd_cbcl_data <- tibble(mean = parameters_abcd_cbcl_ea$coef,
                      lower = parameters_abcd_cbcl_ea$coef - 1.96 * parameters_abcd_cbcl_ea$se,
                      upper = parameters_abcd_cbcl_ea$coef + 1.96 * parameters_abcd_cbcl_ea$se,
                      disorder = parameters_abcd_cbcl_ea$Trait,
                      pgs = parameters_abcd_cbcl_ea$PGS, 
                      beta = format(round(parameters_abcd_cbcl_ea$coef, digits = 3)),
                      p = format(round(parameters_abcd_cbcl_ea$p.adjust, digits = 5)))
table(eabase_abcd_cbcl_data$disorder)
###order the beta's by ascending order
eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="Detach",] <- eabase_abcd_cbcl_data[which(eabase_abcd_cbcl_data$disorder=="Detach")[order(eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="Detach",]$mean)],]
eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="EXT",] <- eabase_abcd_cbcl_data[which(eabase_abcd_cbcl_data$disorder=="EXT")[order(eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="EXT",]$mean)],]
eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="INT",] <- eabase_abcd_cbcl_data[which(eabase_abcd_cbcl_data$disorder=="INT")[order(eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="INT",]$mean)],]
eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="Neuro",] <- eabase_abcd_cbcl_data[which(eabase_abcd_cbcl_data$disorder=="Neuro")[order(eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="Neuro",]$mean)],]
eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="Soma",] <- eabase_abcd_cbcl_data[which(eabase_abcd_cbcl_data$disorder=="Soma")[order(eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="Soma",]$mean)],]
eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="P",] <- eabase_abcd_cbcl_data[which(eabase_abcd_cbcl_data$disorder=="P")[order(eabase_abcd_cbcl_data[eabase_abcd_cbcl_data$disorder=="P",]$mean)],]

##only keep the first occurrence of the trait
eabase_abcd_cbcl_data$disorder[which(eabase_abcd_cbcl_data$disorder == "Detach")[-1]] = " "
eabase_abcd_cbcl_data$disorder[which(eabase_abcd_cbcl_data$disorder == "EXT")[-1]] = " "
eabase_abcd_cbcl_data$disorder[which(eabase_abcd_cbcl_data$disorder == "INT")[-1]] = " "
eabase_abcd_cbcl_data$disorder[which(eabase_abcd_cbcl_data$disorder == "Neuro")[-1]] = " "
eabase_abcd_cbcl_data$disorder[which(eabase_abcd_cbcl_data$disorder == "Soma")[-1]] = " "
eabase_abcd_cbcl_data$disorder[which(eabase_abcd_cbcl_data$disorder == "P")[-1]] = " "

ea_abcd_cbcl_summary <- tibble(mean = mean(eabase_abcd_cbcl_data$mean),
                    lower = mean(eabase_abcd_cbcl_data$lower),
                    upper = mean(eabase_abcd_cbcl_data$upper),
                    disorder = "Summary",
                    pgs = "",
                    beta = c(format(mean(eabase_abcd_cbcl_data$mean), digits = 3)),
                    summary = TRUE)

eaheader <- tibble(disorder = c("","Disorder"), 
                   pgs = c("","PGS"),
                   beta = c("","beta"),
                   p = c("","p"),
                   summary = TRUE)

eaempty_row <- tibble(mean = NA_real_)

ea_abcd_cbcl_para_df <- bind_rows(eaheader,
                                  eabase_abcd_cbcl_data,
                              eaempty_row,
                              ea_abcd_cbcl_summary)

ea_abcd_cbcl_para_df %>% 
  forestplot(labeltext = c(disorder, pgs, beta, p), 
             is.summary = summary,
             clip = c(-0.15, 0.35), 
             xlog = FALSE, 
             col = fpColors(box = "royalblue",
                            line = "darkblue",
                            summary = "royalblue"),
             txt_gp = fpTxtGp(cex = 1.0))
####AA

aabase_abcd_cbcl_data <- tibble(mean = parameters_abcd_cbcl_aa$coef,
                                lower = parameters_abcd_cbcl_aa$coef - 1.96 * parameters_abcd_cbcl_aa$se,
                                upper = parameters_abcd_cbcl_aa$coef + 1.96 * parameters_abcd_cbcl_aa$se,
                                disorder = parameters_abcd_cbcl_aa$Trait,
                                pgs = parameters_abcd_cbcl_aa$PGS, 
                                beta = format(round(parameters_abcd_cbcl_aa$coef, digits = 3)),
                                p = format(round(parameters_abcd_cbcl_aa$p.adjust, digits = 5)))
table(aabase_abcd_cbcl_data$disorder)
###order the beta's by ascending order
aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="Detach",] <- aabase_abcd_cbcl_data[which(aabase_abcd_cbcl_data$disorder=="Detach")[order(aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="Detach",]$mean)],]
aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="EXT",] <- aabase_abcd_cbcl_data[which(aabase_abcd_cbcl_data$disorder=="EXT")[order(aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="EXT",]$mean)],]
aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="INT",] <- aabase_abcd_cbcl_data[which(aabase_abcd_cbcl_data$disorder=="INT")[order(aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="INT",]$mean)],]
aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="Neuro",] <- aabase_abcd_cbcl_data[which(aabase_abcd_cbcl_data$disorder=="Neuro")[order(aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="Neuro",]$mean)],]
aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="Soma",] <- aabase_abcd_cbcl_data[which(aabase_abcd_cbcl_data$disorder=="Soma")[order(aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="Soma",]$mean)],]
aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="P",] <- aabase_abcd_cbcl_data[which(aabase_abcd_cbcl_data$disorder=="P")[order(aabase_abcd_cbcl_data[aabase_abcd_cbcl_data$disorder=="P",]$mean)],]

##only keep the first occurrence of the trait
aabase_abcd_cbcl_data$disorder[which(aabase_abcd_cbcl_data$disorder == "Detach")[-1]] = " "
aabase_abcd_cbcl_data$disorder[which(aabase_abcd_cbcl_data$disorder == "EXT")[-1]] = " "
aabase_abcd_cbcl_data$disorder[which(aabase_abcd_cbcl_data$disorder == "INT")[-1]] = " "
aabase_abcd_cbcl_data$disorder[which(aabase_abcd_cbcl_data$disorder == "Neuro")[-1]] = " "
aabase_abcd_cbcl_data$disorder[which(aabase_abcd_cbcl_data$disorder == "Soma")[-1]] = " "
aabase_abcd_cbcl_data$disorder[which(aabase_abcd_cbcl_data$disorder == "P")[-1]] = " "

aa_abcd_cbcl_summary <- tibble(mean = mean(aabase_abcd_cbcl_data$mean),
                               lower = mean(aabase_abcd_cbcl_data$lower),
                               upper = mean(aabase_abcd_cbcl_data$upper),
                               disorder = "Summary",
                               pgs = "",
                               beta = c(format(mean(aabase_abcd_cbcl_data$mean), digits = 3)),
                               summary = TRUE)

aaheader <- tibble(disorder = c("","Disorder"), 
                   pgs = c("","PGS"),
                   beta = c("","beta"),
                   p = c("","p"),
                   summary = TRUE)

aaempty_row <- tibble(mean = NA_real_)

aa_abcd_cbcl_para_df <- bind_rows(aaheader,
                                  aabase_abcd_cbcl_data,
                                  aaempty_row,
                                  aa_abcd_cbcl_summary)

aa_abcd_cbcl_para_df %>% 
  forestplot(labeltext = c(disorder, pgs, beta, p), 
             is.summary = summary,
             clip = c(-0.15, 0.35), 
             xlog = FALSE, 
             col = fpColors(box = "royalblue",
                            line = "darkblue",
                            summary = "royalblue"),
             txt_gp = fpTxtGp(cex = 1.0))


####Hispanic

hispbase_abcd_cbcl_data <- tibble(mean = parameters_abcd_cbcl_hisp$coef,
                                lower = parameters_abcd_cbcl_hisp$coef - 1.96 * parameters_abcd_cbcl_hisp$se,
                                upper = parameters_abcd_cbcl_hisp$coef + 1.96 * parameters_abcd_cbcl_hisp$se,
                                disorder = parameters_abcd_cbcl_hisp$Trait,
                                pgs = parameters_abcd_cbcl_hisp$PGS, 
                                beta = format(round(parameters_abcd_cbcl_hisp$coef, digits = 3)),
                                p = format(round(parameters_abcd_cbcl_hisp$p.adjust, digits = 5)))
table(hispbase_abcd_cbcl_data$disorder)
###order the beta's by ascending order
hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="Detach",] <- hispbase_abcd_cbcl_data[which(hispbase_abcd_cbcl_data$disorder=="Detach")[order(hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="Detach",]$mean)],]
hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="EXT",] <- hispbase_abcd_cbcl_data[which(hispbase_abcd_cbcl_data$disorder=="EXT")[order(hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="EXT",]$mean)],]
hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="INT",] <- hispbase_abcd_cbcl_data[which(hispbase_abcd_cbcl_data$disorder=="INT")[order(hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="INT",]$mean)],]
hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="Neuro",] <- hispbase_abcd_cbcl_data[which(hispbase_abcd_cbcl_data$disorder=="Neuro")[order(hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="Neuro",]$mean)],]
hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="Soma",] <- hispbase_abcd_cbcl_data[which(hispbase_abcd_cbcl_data$disorder=="Soma")[order(hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="Soma",]$mean)],]
hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="P",] <- hispbase_abcd_cbcl_data[which(hispbase_abcd_cbcl_data$disorder=="P")[order(hispbase_abcd_cbcl_data[hispbase_abcd_cbcl_data$disorder=="P",]$mean)],]

##only keep the first occurrence of the trait
hispbase_abcd_cbcl_data$disorder[which(hispbase_abcd_cbcl_data$disorder == "Detach")[-1]] = " "
hispbase_abcd_cbcl_data$disorder[which(hispbase_abcd_cbcl_data$disorder == "EXT")[-1]] = " "
hispbase_abcd_cbcl_data$disorder[which(hispbase_abcd_cbcl_data$disorder == "INT")[-1]] = " "
hispbase_abcd_cbcl_data$disorder[which(hispbase_abcd_cbcl_data$disorder == "Neuro")[-1]] = " "
hispbase_abcd_cbcl_data$disorder[which(hispbase_abcd_cbcl_data$disorder == "Soma")[-1]] = " "
hispbase_abcd_cbcl_data$disorder[which(hispbase_abcd_cbcl_data$disorder == "P")[-1]] = " "

hisp_abcd_cbcl_summary <- tibble(mean = mean(hispbase_abcd_cbcl_data$mean),
                               lower = mean(hispbase_abcd_cbcl_data$lower),
                               upper = mean(hispbase_abcd_cbcl_data$upper),
                               disorder = "Summary",
                               pgs = "",
                               beta = c(format(mean(hispbase_abcd_cbcl_data$mean), digits = 3)),
                               summary = TRUE)

hispheader <- tibble(disorder = c("","Disorder"), 
                   pgs = c("","PGS"),
                   beta = c("","beta"),
                   p = c("","p"),
                   summary = TRUE)

hispempty_row <- tibble(mean = NA_real_)

hisp_abcd_cbcl_para_df <- bind_rows(hispheader,
                                  hispbase_abcd_cbcl_data,
                                  hispempty_row,
                                  hisp_abcd_cbcl_summary)

hisp_abcd_cbcl_para_df %>% 
  forestplot(labeltext = c(disorder, pgs, beta, p), 
             is.summary = summary,
             clip = c(-0.15, 0.35), 
             xlog = FALSE, 
             col = fpColors(box = "royalblue",
                            line = "darkblue",
                            summary = "royalblue"),
             txt_gp = fpTxtGp(cex = 1.0))

####------------ABCD KSADS-------------#######
parameters_abcd_ksads_ea <- read.csv("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_ksads_ea.csv")
parameters_abcd_ksads_aa <- read.csv("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_ksads_aa.csv")
parameters_abcd_ksads_hisp <- read.csv("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/parameters_abcd_ksads_hisp.csv")

eabase_abcd_ksads_data <- tibble(mean = parameters_abcd_ksads_ea$coef,
                                lower = parameters_abcd_ksads_ea$coef - 1.96 * parameters_abcd_ksads_ea$se,
                                upper = parameters_abcd_ksads_ea$coef + 1.96 * parameters_abcd_ksads_ea$se,
                                disorder = parameters_abcd_ksads_ea$Trait,
                                pgs = parameters_abcd_ksads_ea$PGS, 
                                beta = format(round(parameters_abcd_ksads_ea$coef, digits = 3)),
                                p = format(round(parameters_abcd_ksads_ea$p.adjust, digits = 5)))
table(eabase_abcd_ksads_data$disorder)
###order the beta's by ascending order
eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_adhd_crt",] <- eabase_abcd_ksads_data[which(eabase_abcd_ksads_data$disorder=="ksads_adhd_crt")[order(eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_adhd_crt",]$mean)],]
eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_asspsy_crt",] <- eabase_abcd_ksads_data[which(eabase_abcd_ksads_data$disorder=="ksads_asspsy_crt")[order(eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_asspsy_crt",]$mean)],]
eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_cd_crt_child",] <- eabase_abcd_ksads_data[which(eabase_abcd_ksads_data$disorder=="ksads_cd_crt_child")[order(eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_cd_crt_child",]$mean)],]
eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_ocd_crt",] <- eabase_abcd_ksads_data[which(eabase_abcd_ksads_data$disorder=="ksads_ocd_crt")[order(eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_ocd_crt",]$mean)],]
eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_odd_crt",] <- eabase_abcd_ksads_data[which(eabase_abcd_ksads_data$disorder=="ksads_odd_crt")[order(eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_odd_crt",]$mean)],]
eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_pho_crt",] <- eabase_abcd_ksads_data[which(eabase_abcd_ksads_data$disorder=="ksads_pho_crt")[order(eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_pho_crt",]$mean)],]
eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_slp_crt",] <- eabase_abcd_ksads_data[which(eabase_abcd_ksads_data$disorder=="ksads_slp_crt")[order(eabase_abcd_ksads_data[eabase_abcd_ksads_data$disorder=="ksads_slp_crt",]$mean)],]

##only keep the first occurrence of the trait
eabase_abcd_ksads_data$disorder[which(eabase_abcd_ksads_data$disorder == "ksads_adhd_crt")[-1]] = " "
eabase_abcd_ksads_data$disorder[which(eabase_abcd_ksads_data$disorder == "ksads_asspsy_crt")[-1]] = " "
eabase_abcd_ksads_data$disorder[which(eabase_abcd_ksads_data$disorder == "ksads_cd_crt_child")[-1]] = " "
eabase_abcd_ksads_data$disorder[which(eabase_abcd_ksads_data$disorder == "ksads_ocd_crt")[-1]] = " "
eabase_abcd_ksads_data$disorder[which(eabase_abcd_ksads_data$disorder == "ksads_odd_crt")[-1]] = " "
eabase_abcd_ksads_data$disorder[which(eabase_abcd_ksads_data$disorder == "ksads_pho_crt")[-1]] = " "
eabase_abcd_ksads_data$disorder[which(eabase_abcd_ksads_data$disorder == "ksads_slp_crt")[-1]] = " "

ea_abcd_ksads_summary <- tibble(mean = mean(eabase_abcd_ksads_data$mean),
                               lower = mean(eabase_abcd_ksads_data$lower),
                               upper = mean(eabase_abcd_ksads_data$upper),
                               disorder = "Summary",
                               pgs = "",
                               beta = c(format(mean(eabase_abcd_ksads_data$mean), digits = 3)),
                               summary = TRUE)

eaheader <- tibble(disorder = c("","Disorder"), 
                   pgs = c("","PGS"),
                   beta = c("","beta"),
                   p = c("","p"),
                   summary = TRUE)

eaempty_row <- tibble(mean = NA_real_)

ea_abcd_ksads_para_df <- bind_rows(eaheader,
                                  eabase_abcd_ksads_data,
                                  eaempty_row,
                                  ea_abcd_ksads_summary)

ea_abcd_ksads_para_df %>% 
  forestplot(labeltext = c(disorder, pgs, beta, p), 
             is.summary = summary,
             clip = c(-0.15, 0.45), 
             xlog = FALSE, 
             col = fpColors(box = "royalblue",
                            line = "darkblue",
                            summary = "royalblue"),
             txt_gp = fpTxtGp(cex = 1.0))
####AA

aabase_abcd_ksads_data <- tibble(mean = parameters_abcd_ksads_aa$coef,
                                 lower = parameters_abcd_ksads_aa$coef - 1.96 * parameters_abcd_ksads_aa$se,
                                 upper = parameters_abcd_ksads_aa$coef + 1.96 * parameters_abcd_ksads_aa$se,
                                 disorder = parameters_abcd_ksads_aa$Trait,
                                 pgs = parameters_abcd_ksads_aa$PGS, 
                                 beta = format(round(parameters_abcd_ksads_aa$coef, digits = 3)),
                                 p = format(round(parameters_abcd_ksads_aa$p.adjust, digits = 5)))
table(aabase_abcd_ksads_data$disorder)
###order the beta's by ascending order
aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_odd_crt",] <- aabase_abcd_ksads_data[which(aabase_abcd_ksads_data$disorder=="ksads_odd_crt")[order(aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_odd_crt",]$mean)],]
aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_cd_crt_child",] <- aabase_abcd_ksads_data[which(aabase_abcd_ksads_data$disorder=="ksads_cd_crt_child")[order(aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_cd_crt_child",]$mean)],]

aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_adhd_crt",] <- aabase_abcd_ksads_data[which(aabase_abcd_ksads_data$disorder=="ksads_adhd_crt")[order(aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_adhd_crt",]$mean)],]
aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_ocd_crt",] <- aabase_abcd_ksads_data[which(aabase_abcd_ksads_data$disorder=="ksads_ocd_crt")[order(aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_ocd_crt",]$mean)],]
aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_pho_crt",] <- aabase_abcd_ksads_data[which(aabase_abcd_ksads_data$disorder=="ksads_pho_crt")[order(aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_pho_crt",]$mean)],]
aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_slp_crt",] <- aabase_abcd_ksads_data[which(aabase_abcd_ksads_data$disorder=="ksads_slp_crt")[order(aabase_abcd_ksads_data[aabase_abcd_ksads_data$disorder=="ksads_slp_crt",]$mean)],]

##only keep the first occurrence of the trait
aabase_abcd_ksads_data$disorder[which(aabase_abcd_ksads_data$disorder == "ksads_odd_crt")[-1]] = " "
aabase_abcd_ksads_data$disorder[which(aabase_abcd_ksads_data$disorder == "ksads_cd_crt_child")[-1]] = " "

aabase_abcd_ksads_data$disorder[which(aabase_abcd_ksads_data$disorder == "ksads_adhd_crt")[-1]] = " "
aabase_abcd_ksads_data$disorder[which(aabase_abcd_ksads_data$disorder == "ksads_ocd_crt")[-1]] = " "
aabase_abcd_ksads_data$disorder[which(aabase_abcd_ksads_data$disorder == "ksads_pho_crt")[-1]] = " "
aabase_abcd_ksads_data$disorder[which(aabase_abcd_ksads_data$disorder == "ksads_slp_crt")[-1]] = " "

aa_abcd_ksads_summary <- tibble(mean = mean(aabase_abcd_ksads_data$mean),
                                lower = mean(aabase_abcd_ksads_data$lower),
                                upper = mean(aabase_abcd_ksads_data$upper),
                                disorder = "Summary",
                                pgs = "",
                                beta = c(format(mean(aabase_abcd_ksads_data$mean), digits = 3)),
                                summary = TRUE)

aaheader <- tibble(disorder = c("","Disorder"), 
                   pgs = c("","PGS"),
                   beta = c("","beta"),
                   p = c("","p"),
                   summary = TRUE)

aaempty_row <- tibble(mean = NA_real_)

aa_abcd_ksads_para_df <- bind_rows(aaheader,
                                   aabase_abcd_ksads_data,
                                   aaempty_row,
                                   aa_abcd_ksads_summary)

aa_abcd_ksads_para_df %>% 
  forestplot(labeltext = c(disorder, pgs, beta, p), 
             is.summary = summary,
             clip = c(-0.20, 0.45), 
             xlog = FALSE, 
             col = fpColors(box = "royalblue",
                            line = "darkblue",
                            summary = "royalblue"),
             txt_gp = fpTxtGp(cex = 1.0))


####Hispanic
hispbase_abcd_ksads_data <- tibble(mean = parameters_abcd_ksads_hisp$coef,
                                 lower = parameters_abcd_ksads_hisp$coef - 1.96 * parameters_abcd_ksads_hisp$se,
                                 upper = parameters_abcd_ksads_hisp$coef + 1.96 * parameters_abcd_ksads_hisp$se,
                                 disorder = parameters_abcd_ksads_hisp$Trait,
                                 pgs = parameters_abcd_ksads_hisp$PGS, 
                                 beta = format(round(parameters_abcd_ksads_hisp$coef, digits = 3)),
                                 p = format(round(parameters_abcd_ksads_hisp$p.adjust, digits = 5)))
table(hispbase_abcd_ksads_data$disorder)
###order the beta's by ascending order
hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_odd_crt",] <- hispbase_abcd_ksads_data[which(hispbase_abcd_ksads_data$disorder=="ksads_odd_crt")[order(hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_odd_crt",]$mean)],]
hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_cd_crt_child",] <- hispbase_abcd_ksads_data[which(hispbase_abcd_ksads_data$disorder=="ksads_cd_crt_child")[order(hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_cd_crt_child",]$mean)],]

hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_adhd_crt",] <- hispbase_abcd_ksads_data[which(hispbase_abcd_ksads_data$disorder=="ksads_adhd_crt")[order(hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_adhd_crt",]$mean)],]
hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_ocd_crt",] <- hispbase_abcd_ksads_data[which(hispbase_abcd_ksads_data$disorder=="ksads_ocd_crt")[order(hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_ocd_crt",]$mean)],]
hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_pho_crt",] <- hispbase_abcd_ksads_data[which(hispbase_abcd_ksads_data$disorder=="ksads_pho_crt")[order(hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_pho_crt",]$mean)],]
hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_slp_crt",] <- hispbase_abcd_ksads_data[which(hispbase_abcd_ksads_data$disorder=="ksads_slp_crt")[order(hispbase_abcd_ksads_data[hispbase_abcd_ksads_data$disorder=="ksads_slp_crt",]$mean)],]

##only keep the first occurrence of the trait
hispbase_abcd_ksads_data$disorder[which(hispbase_abcd_ksads_data$disorder == "ksads_odd_crt")[-1]] = " "
hispbase_abcd_ksads_data$disorder[which(hispbase_abcd_ksads_data$disorder == "ksads_cd_crt_child")[-1]] = " "

hispbase_abcd_ksads_data$disorder[which(hispbase_abcd_ksads_data$disorder == "ksads_adhd_crt")[-1]] = " "
hispbase_abcd_ksads_data$disorder[which(hispbase_abcd_ksads_data$disorder == "ksads_ocd_crt")[-1]] = " "
hispbase_abcd_ksads_data$disorder[which(hispbase_abcd_ksads_data$disorder == "ksads_pho_crt")[-1]] = " "
hispbase_abcd_ksads_data$disorder[which(hispbase_abcd_ksads_data$disorder == "ksads_slp_crt")[-1]] = " "

hisp_abcd_ksads_summary <- tibble(mean = mean(hispbase_abcd_ksads_data$mean),
                                lower = mean(hispbase_abcd_ksads_data$lower),
                                upper = mean(hispbase_abcd_ksads_data$upper),
                                disorder = "Summary",
                                pgs = "",
                                beta = c(format(mean(hispbase_abcd_ksads_data$mean), digits = 3)),
                                summary = TRUE)

hispheader <- tibble(disorder = c("","Disorder"), 
                   pgs = c("","PGS"),
                   beta = c("","beta"),
                   p = c("","p"),
                   summary = TRUE)

hispempty_row <- tibble(mean = NA_real_)

hisp_abcd_ksads_para_df <- bind_rows(hispheader,
                                   hispbase_abcd_ksads_data,
                                   hispempty_row,
                                   hisp_abcd_ksads_summary)

hisp_abcd_ksads_para_df %>% 
  forestplot(labeltext = c(disorder, pgs, beta, p), 
             is.summary = summary,
             clip = c(-0.20, 0.45), 
             xlog = FALSE, 
             col = fpColors(box = "royalblue",
                            line = "darkblue",
                            summary = "royalblue"),
             txt_gp = fpTxtGp(cex = 1.0))

