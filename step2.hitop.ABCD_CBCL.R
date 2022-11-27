library(knitr)
##merge with demographic to add Race
abcd_cbcldemo <- merge(abcd_cbcl, abcd_pdemo, by = "subjectkey")
abcd_cbcldemo <- abcd_cbcldemo %>%
  mutate(hispanic = ifelse(demo_ethn_v2==1 & demo_ethn_v2!=777 & demo_ethn_v2 !=999,1,0))

abcd_cbcldemo <- abcd_cbcldemo %>%
  mutate(Race = ifelse(demo_race_a_p___10==1 & hispanic == 0, 1, 
                       ifelse(demo_race_a_p___11==1 & hispanic == 0, 2, 
                              ifelse(hispanic==1, 3, 4))))

table(abcd_cbcldemo$Race)

####CFA using CBCL####
###select low frequency items and remove them, per Michelini methods
freq_cbcl <- abcd_cbcldemo %>%
  select(ends_with("_p")) %>%
  lapply(table)

##save the names of items that have high number of 0s; at least 98% of 0
names <- which(unlist(freq_cbcl) >= nrow(abcd_cbcldemo) * 0.98) %>%
  names()
##extract only the item number from the list of items
cbclItemsRmv <- sapply(strsplit(names, split = "_"),`[`, 2)
##concatenate (add) cbcl and p around the item numbers
cbclItemsRmv <- paste("cbcl_",cbclItemsRmv, "_p", sep="")
##select items that have reasonable frequency of non-zero responses
cbcl_select <- abcd_cbcldemo %>%
  select(subjectkey, Race,starts_with("cbcl_q"), -all_of(cbclItemsRmv))

##check correlation
cbcl_select[,2:ncol(cbcl_select)] <- as.data.frame(lapply(cbcl_select[,2:ncol(cbcl_select)], as.numeric))
corrCBCL <- cbcl_select[,-c(1,2)] %>%
  psych::polychoric()

###overall sample####
###these values have high correlation above .75
corrCBCL$rho[which(corrCBCL$rho >.75 & corrCBCL$rho !=1)]

##integer value when dividing the index number within the correlation matrix by total item number in cbcl_select, plus 1
x <- as.integer(which(corrCBCL$rho >.75 & corrCBCL$rho !=1)/ncol(cbcl_select[,-c(1,2)])) + 1
##the residual after substracting raw index number by the integers times total item number
y <- which(corrCBCL$rho >.75 & corrCBCL$rho !=1) - 
  (as.integer(which(corrCBCL$rho >.75 & corrCBCL$rho !=1)/ncol(cbcl_select[,-c(1,2)])) * ncol(cbcl_select[,-c(1,2)]))
##save the names in the list of the correlation matrix so that I know the items that are correlated with each other
rhoNames <- dimnames(corrCBCL$rho)[[1]]
rhoNames[x]
rhoNames[y]

##items that are correlated with each other:
highCorr1Items <- c("cbcl_q08_p", "cbcl_q10_p","cbcl_q78_p") #attention problems
highCorr2Items <- c("cbcl_q20_p", "cbcl_q21_p") #destroys 
highCorr3Items <- c("cbcl_q22_p", "cbcl_q23_p","cbcl_q28_p") #disobeys rules
highCorr4Items <- c("cbcl_q25_p", "cbcl_q48_p") #peer problem (not liked; doesn't get along)
#highCorr5Items <- c("cbcl_q40_p", "cbcl_q70_p") #hallucination -> later excluded for low prevalence of non zero response
highCorr6Items <- c("cbcl_q53_p", "cbcl_q55_p") #weight and overeat
highCorr7Items <- c("cbcl_q56c_p", "cbcl_q56f_p") #somatic: nausea and stomaches
highCorr8Items <- c("cbcl_q57_p", "cbcl_q97_p","cbcl_q16_p") #attack and threaten, cruel, mean to others
highCorr9Items <- c("cbcl_q81_p", "cbcl_q82_p") #steals

cbcl_select <- as.data.frame(cbcl_select)
##create composite by averaging the correlated items and then taking the integers to preserve trichotomous ratings (0,1,2)
cbcl_select <- cbcl_select %>%
  mutate(cbcl_att = as.integer(rowMeans(cbcl_select[highCorr1Items], na.rm = TRUE)),
         cbcl_destry = as.integer(rowMeans(cbcl_select[highCorr2Items], na.rm = TRUE)),
         cbcl_disrule = as.integer(rowMeans(cbcl_select[highCorr3Items], na.rm = TRUE)),
         cbcl_peer = as.integer(rowMeans(cbcl_select[highCorr4Items], na.rm = TRUE)),
         #cbcl_hall = as.integer(rowMeans(cbcl_select[highCorr5Items], na.rm = TRUE)),
         cbcl_whteat = as.integer(rowMeans(cbcl_select[highCorr6Items], na.rm = TRUE)),
         cbcl_somatic = as.integer(rowMeans(cbcl_select[highCorr7Items], na.rm = TRUE)),
         cbcl_thretattck = as.integer(rowMeans(cbcl_select[highCorr8Items], na.rm = TRUE)),
         cbcl_stl = as.integer(rowMeans(cbcl_select[highCorr9Items], na.rm = TRUE))
  )
###remove the individual items that contributed to the composite scores
cbcl_select[,c(all_of(highCorr1Items),
               all_of(highCorr2Items),
               all_of(highCorr3Items),
               all_of(highCorr4Items),
               #all_of(highCorr5Items),
               all_of(highCorr6Items),
               all_of(highCorr7Items),
               all_of(highCorr8Items),
               all_of(highCorr9Items))] = NULL

corrCBCL <- cbcl_select[,-c(1,2)] %>%
  psych::polychoric()

##find number of factors
library(psych)
library(nFactors)
###first way: 
fa.parallel(cbcl_select)
###second way
ev <- eigen(corrCBCL$rho) # get eigenvalues
ap <- parallel(subject=nrow(cbcl_select),var=ncol(cbcl_select),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

##EFA
one_cbcl <- fa(corrCBCL$rho, nfactors = 1, n.obs = 11748)
two_cbcl <- fa(corrCBCL$rho, nfactors = 2, n.obs = 11748)
three_cbcl <- fa(corrCBCL$rho, nfactors = 3, n.obs = 11748)
four_cbcl <- fa(corrCBCL$rho, nfactors = 4, n.obs = 11748)
five_cbcl <- fa(corrCBCL$rho, nfactors = 5, n.obs = 11748)

fa.diagram(two_cbcl, digits = 2, cut = .15, simple = FALSE)
fa.diagram(three_cbcl, digits = 2, cut = .15, simple=FALSE)
fa.diagram(four_cbcl, digits = 2, cut = .15, simple = FALSE)
fa.diagram(five_cbcl, digits = 2, cut = .15, simple = FALSE)

##bassAckward 
####four factors
four_hiercbcl <- bassAckward(corrCBCL$rho, nfactors = 4, fm="pca")
summary(four_hiercbcl)

####five factors####
five_hiercbcl <- bassAckward(corrCBCL$rho, nfactors = 5, fm="pca")
summary(five_hiercbcl)
bassAckward.diagram(five_hiercbcl)

fivefactor_cbcl_hier <- five_hiercbcl$fa[[5]]
fivefactor_cbcl_hier
fa.diagram(fivefactor_cbcl_hier)


#write.csv(fivefactor_cbcl_hier$loadings,file = "fivefactor_cbcl_hier.csv")
#write.csv(fivefactor_cbcl_hier$Phi,file = "fivefactor_cbcl_hier_factorCorr.csv")

###factor scores for five factor structure####
#abcd_cbclFScore <- factor.scores(cbcl_select[,-1], fivefactor_cbcl_hier)
#write.csv(fivefactor_cbcl_hier$loadings,file = "fivefactor_cbcl_hier.csv")
#write.csv(fivefactor_cbcl_hier$Phi,file = "fivefactor_cbcl_hier_factorCorr.csv")

####CFA in lavaan for five factor model####
###which items load onto each factor
####externalizing

EXT_items <- names(which(fivefactor_cbcl_hier$loadings[,1]>=.30))
####Neurodevelopmental
Neuro_items <- names(which(fivefactor_cbcl_hier$loadings[,2]>=.30))
####Internalizing
INT_items <- names(which(fivefactor_cbcl_hier$loadings[,3]>=.30))
####Somatoform
Soma_items <- names(which(fivefactor_cbcl_hier$loadings[,4]>=.30))
####Detachment
Detach_items <- names(which(fivefactor_cbcl_hier$loadings[,5]>=.30))



###lavaan model syntax
####unit variance identification (first item freely estimated)

##EXT model 
linestart_EXT <- paste("EXT"," =~ NA*", EXT_items[1], sep = "")
linemid_EXT <- ""
for(i in 2:length(EXT_items)){
  linemid_EXT <- paste(linemid_EXT, " + ", EXT_items[i], sep = "")
}
Model_EXT <- paste(linestart_EXT, linemid_EXT, " \n ", sep = "")

##Neuro model 
linestart_Neuro <- paste("Neuro"," =~ NA*", Neuro_items[1], sep = "")
linemid_Neuro <- ""
for(i in 2:length(Neuro_items)){
  linemid_Neuro <- paste(linemid_Neuro, " + ", Neuro_items[i], sep = "")
}
Model_Neuro <- paste(linestart_Neuro, linemid_Neuro, " \n ", sep = "")

##Internalizing
linestart_INT <- paste("INT"," =~ NA*", INT_items[1], sep = "")
linemid_INT <- ""
for(i in 2:length(INT_items)){
  linemid_INT <- paste(linemid_INT, " + ", INT_items[i], sep = "")
}
Model_INT <- paste(linestart_INT, linemid_INT, " \n ", sep = "")

##Soma
linestart_Soma <- paste("Soma"," =~ NA*", Soma_items[1], sep = "")
linemid_Soma <- ""
for(i in 2:length(Soma_items)){
  linemid_Soma <- paste(linemid_Soma, " + ", Soma_items[i], sep = "")
}
Model_Soma <- paste(linestart_Soma, linemid_Soma, " \n ", sep = "")

##Detach
linestart_Detach <- paste("Detach"," =~ NA*", Detach_items[1], sep = "")
linemid_Detach <- ""
for(i in 2:length(Detach_items)){
  linemid_Detach <- paste(linemid_Detach, " + ", Detach_items[i], sep = "")
}
Model_Detach <- paste(linestart_Detach, linemid_Detach, " \n ", sep = "")

##full model
Model_full <- paste(Model_EXT, Model_Neuro, Model_INT, Model_Soma, Model_Detach, 
                    "EXT ~~ 1*EXT", " \n ",
                    "Neuro ~~ 1*Neuro", " \n ",
                    "INT ~~ 1*INT", " \n ",
                    "Soma ~~ 1*Soma", " \n ",
                    "Detach ~~ 1*Detach", sep = "")

cat(Model_full)

####fit model ####
model_fivefactor <- cfa(Model_full, data = cbcl_select)
summary(model_fivefactor, fit.measures = TRUE)
modi_model <- modindices(model_fivefactor)
modi_model <- modi_model[order(-modi_model$mi),]
modi_model

##full model modified; add the top 150 items suggested by modification indices

modi_items <- ""
for (j in 1:200) {
  modi_items = paste(modi_items, modi_model$lhs[j]," ", modi_model$op[j]," ", modi_model$rhs[j], " \n ", sep = "")
}
cat(modi_items)
Model_full_mod <- ""
Model_full_mod <- paste(Model_EXT, Model_Neuro, Model_INT, Model_Soma, Model_Detach, 
                        "EXT ~~ 1*EXT", " \n ",
                        "Neuro ~~ 1*Neuro", " \n ",
                        "INT ~~ 1*INT", " \n ",
                        "Soma ~~ 1*Soma", " \n ",
                        "Detach ~~ 1*Detach"," \n ", modi_items,
                        sep = "")

cat(Model_full_mod)
model_fivefactor_mod <- cfa(Model_full_mod, data = cbcl_select)

summary(model_fivefactor_mod,
        fit.measures = TRUE,
        standardized = TRUE,
        rsquare = TRUE)
fitmeasures(model_fivefactor_mod)
standardizedsolution(model_fivefactor_mod)
#write.csv(standardizedsolution(model_fivefactor_mod), file = "abcd_cbcl_fiveFactor_cfasolution.csv")

###Add traditional bifactor####
##General
P_items = unique(c(EXT_items,Neuro_items,INT_items,Soma_items,Detach_items))

linestart_P <- paste("P"," =~ NA*", P_items[1], sep = "")
linemid_P <- ""
for(i in 2:length(P_items)){
  linemid_P <- paste(linemid_P, " + ", P_items[i], sep = "")
}
Model_P <- paste(linestart_P, linemid_P, " \n ", sep = "")
###model syntax for bifactor model
Model_fullBi <- ""
Model_fullBi <- paste(Model_EXT, Model_Neuro, Model_INT, Model_Soma, Model_Detach, 
                          Model_P,
                        "EXT ~~ 1*EXT", " \n ",
                        "Neuro ~~ 1*Neuro", " \n ",
                        "INT ~~ 1*INT", " \n ",
                        "Soma ~~ 1*Soma", " \n ",
                        "Detach ~~ 1*Detach"," \n ", 
                        "P ~~ 1*P", " \n ",
                        "P ~~ 0*EXT"," \n ",
                        "P ~~ 0*Neuro", " \n ",
                        "P ~~ 0*INT", " \n ",
                        "P ~~ 0*Soma"," \n ",
                        "P ~~ 0*Detach", " \n ",
                        sep = "")

cat(Model_fullBi)
Model_fullBi_fit <- cfa(Model_fullBi, data = cbcl_select)

summary(Model_fullBi_fit,
        fit.measures = TRUE,
        standardized = TRUE)
fitmeasures(Model_fullBi_fit,c("cfi","tli","rmsea","srmr","chisq","df"))

###modify the bifactor model
modi_model_bifactor <- modindices(Model_fullBi_fit)
modi_model_bifactor <- modi_model_bifactor[order(-modi_model_bifactor$mi),]
modi_model_bifactor

modi_bifactor_items <- ""
for (j in 1:200) {
  modi_bifactor_items = paste(modi_bifactor_items, modi_model_bifactor$lhs[j]," ", modi_model_bifactor$op[j]," ", modi_model_bifactor$rhs[j], " \n ", sep = "")
}
cat(modi_bifactor_items)

####modified bifactor model####
Model_fullBi_mod <- ""
Model_fullBi_mod <- paste(Model_EXT, Model_Neuro, Model_INT, Model_Soma, Model_Detach, 
                      Model_P,
                      "EXT ~~ 1*EXT", " \n ",
                      "Neuro ~~ 1*Neuro", " \n ",
                      "INT ~~ 1*INT", " \n ",
                      "Soma ~~ 1*Soma", " \n ",
                      "Detach ~~ 1*Detach"," \n ", 
                      "P ~~ 1*P", " \n ",
                      "P ~~ 0*EXT"," \n ",
                      "P ~~ 0*Neuro", " \n ",
                      "P ~~ 0*INT", " \n ",
                      "P ~~ 0*Soma"," \n ",
                      "P ~~ 0*Detach", " \n ",modi_bifactor_items,
                      sep = "")

cat(Model_fullBi_mod)
Model_fullBi_mod_fit <- cfa(Model_fullBi_mod, data = cbcl_select)

summary(Model_fullBi_mod_fit,
        fit.measures = TRUE,
        standardized = TRUE)
fitmeasures(Model_fullBi_mod_fit,c("cfi","tli","rmsea","srmr","chisq","df"))


####using lavaan to test invariance####

####Measurement invariance testing for correlated factors####

library(equaltestMI)

threeGroup <- cbcl_select[cbcl_select$Race==1 | cbcl_select$Race==2| cbcl_select$Race==3,]
rm(list = c("abcd_cbcl","abcd_cbcldemo","abcd_cbcls","abcd_pdemo","cbcl_select"))
gc()
obs <- numeric()
for(i in 1:ncol(threeGroup)) {
  obs <- which(is.na(threeGroup[,i]))
}
obs #observations with missing data
threeGroup <- threeGroup[-c(obs),]##these observations had all NAs. remove them

##model fit indices
fits <- matrix(NA, nrow = 12, ncol = 7)
fits_ea_hi <- matrix(NA, nrow = 12, ncol = 7)
fits_aa_hi <- matrix(NA, nrow = 12, ncol = 7)
colnames(fits) <- c("Model","cfi","tli","rmsea","srmr","x","df")
colnames(fits_ea_hi) <- c("Model","cfi","tli","rmsea","srmr","x","df")
colnames(fits_aa_hi) <- c("Model","cfi","tli","rmsea","srmr","x","df")

EA_AA_compare <- which(threeGroup$Race==1 | threeGroup$Race==2)
EA_HI_compare <- which(threeGroup$Race==1 | threeGroup$Race==3)
AA_HI_compare <- which(threeGroup$Race==2 | threeGroup$Race == 3)
####compare EA and AA####
multi_mod <- equaltestMI::eqMI.main(model = Model_full_mod, 
                       data = threeGroup[EA_AA_compare,],
                       group = "Race",
                       meanstructure = TRUE,
                       output = "both",
                       equivalence.test = FALSE,
                       adjRMSEA = FALSE,
                       projection = TRUE,
                       bootstrap = FALSE)

###Measurement invariance testing for bifactor model###
multi_bifactor_mod <- equaltestMI::eqMI.main(model = Model_fullBi_mod, 
                                    data = threeGroup[EA_AA_compare,], 
                                    group = "Race",
                                    meanstructure = TRUE,
                                    output = "both",
                                    equivalence.test = FALSE,
                                    adjRMSEA = FALSE,
                                    projection = TRUE,
                                    bootstrap = FALSE)
####compare EA and HI####
multi_mod_eahi <- equaltestMI::eqMI.main(model = Model_full_mod, 
                                    data = threeGroup[EA_HI_compare,],
                                    group = "Race",
                                    meanstructure = TRUE,
                                    output = "both",
                                    equivalence.test = FALSE,
                                    adjRMSEA = FALSE,
                                    projection = TRUE,
                                    bootstrap = FALSE)

###Measurement invariance testing for bifactor model###
multi_bifactor_mod_eahi <- equaltestMI::eqMI.main(model = Model_fullBi_mod, 
                                             data = threeGroup[EA_HI_compare,], 
                                             group = "Race",
                                             meanstructure = TRUE,
                                             output = "both",
                                             equivalence.test = FALSE,
                                             adjRMSEA = FALSE,
                                             projection = TRUE,
                                             bootstrap = FALSE)

####compare AA and HI ####
multi_mod_aahi <- equaltestMI::eqMI.main(model = Model_full_mod, 
                                         data = threeGroup[AA_HI_compare,],
                                         group = "Race",
                                         meanstructure = TRUE,
                                         output = "both",
                                         equivalence.test = FALSE,
                                         adjRMSEA = FALSE,
                                         projection = TRUE,
                                         bootstrap = FALSE)

###Measurement invariance testing for bifactor model###
multi_bifactor_mod_aahi <- equaltestMI::eqMI.main(model = Model_fullBi_mod, 
                                                  data = threeGroup[AA_HI_compare,], 
                                                  group = "Race",
                                                  meanstructure = TRUE,
                                                  output = "both",
                                                  equivalence.test = FALSE,
                                                  adjRMSEA = FALSE,
                                                  projection = TRUE,
                                                  bootstrap = FALSE)

#saveRDS(multi_bifactor_mod, file = "abcd_cbcl_bifactorInvariance.Rdata")
#saveRDS(multi_mod, file = "abcd_cbcl_CorrFactorInvariance.Rdata")
#saveRDS(multi_bifactor_mod_eahi, file = "abcd_cbcl_bifactorInvariance_eahi.Rdata")
#saveRDS(multi_mod_eahi, file = "abcd_cbcl_CorrFactorInvariance_eahi.Rdata")
#saveRDS(multi_bifactor_mod_aahi, file = "abcd_cbcl_bifactorInvariance_aahi.Rdata")
#saveRDS(multi_mod_aahi, file = "abcd_cbcl_CorrFactorInvariance_aahi.Rdata")


###adjust model to improve strict invariance???
##model fit statistics for correlated and bifactor models
##EA 
fits[1,] <- c("EA Model - Correlated",round(fitmeasures(multi_mod$convention.sem$LavaanOut$fit.configural.g1,
                                                        c("cfi","tli","rmsea","srmr","chisq","df")),3))
##AA
fits[2,] <- c("AA Model - Correlated",round(fitmeasures(multi_mod$convention.sem$LavaanOut$fit.configural.g2,
                                                        c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Configural
fits[3,] <- c("Configural Model - Correlated",round(fitmeasures(multi_mod$convention.sem$LavaanOut$fit.combine.groups,
                                                                c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Metric
fits[4,] <- c("Metric Model - Correlated",round(fitmeasures(multi_mod$convention.sem$LavaanOut$fit.metric,
                                                            c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Scalar
fits[5,] <- c("Scalar Model - Correlated",round(fitmeasures(multi_mod$convention.sem$LavaanOut$fit.scalar,
                                                            c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Strict(residual) invariance
fits[6,] <- c("Strict Model - Correlated",round(fitmeasures(multi_mod$convention.sem$LavaanOut$fit.strict.residuals,
                                                            c("cfi","tli","rmsea","srmr","chisq","df")),3))

##EA 
fits[7,] <- c("EA Model - Bifactor",round(fitmeasures(multi_bifactor_mod$convention.sem$LavaanOut$fit.configural.g1,
                                           c("cfi","tli","rmsea","srmr","chisq","df")),3))
##AA
fits[8,] <- c("AA Model - Bifactor",round(fitmeasures(multi_bifactor_mod$convention.sem$LavaanOut$fit.configural.g2,
                                           c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Configural
fits[9,] <- c("Configural Model - Bifactor",round(fitmeasures(multi_bifactor_mod$convention.sem$LavaanOut$fit.combine.groups,
                                                   c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Metric
fits[10,] <- c("Metric Model - Bifactor",round(fitmeasures(multi_bifactor_mod$convention.sem$LavaanOut$fit.metric,
                                               c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Scalar
fits[11,] <- c("Scalar Model - Bifactor",round(fitmeasures(multi_bifactor_mod$convention.sem$LavaanOut$fit.scalar,
                                               c("cfi","tli","rmsea","srmr","chisq","df")),3))

##Strict(residual) invariance
fits[12,] <- c("Strict Model - Bifactor",round(fitmeasures(multi_bifactor_mod$convention.sem$LavaanOut$fit.strict.residuals,
                                               c("cfi","tli","rmsea","srmr","chisq","df")),3))
kable(fits)
write.csv(fits, file = "abcd_cbclFits_invariance_ea_aa.csv")



###EA and Hispanic comparison model fits
##EA 
fits_ea_hi[1,] <- c("EA Model - Correlated",round(fitmeasures(multi_mod_eahi$convention.sem$LavaanOut$fit.configural.g1,
                                                        c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Hispanic
fits_ea_hi[2,] <- c("Hispanic Model - Correlated",round(fitmeasures(multi_mod_eahi$convention.sem$LavaanOut$fit.configural.g2,
                                                        c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Configural
fits_ea_hi[3,] <- c("Configural Model - Correlated",round(fitmeasures(multi_mod_eahi$convention.sem$LavaanOut$fit.combine.groups,
                                                                c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Metric
fits_ea_hi[4,] <- c("Metric Model - Correlated",round(fitmeasures(multi_mod_eahi$convention.sem$LavaanOut$fit.metric,
                                                            c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Scalar
fits_ea_hi[5,] <- c("Scalar Model - Correlated",round(fitmeasures(multi_mod_eahi$convention.sem$LavaanOut$fit.scalar,
                                                            c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Strict(residual) invariance
fits_ea_hi[6,] <- c("Strict Model - Correlated",round(fitmeasures(multi_mod_eahi$convention.sem$LavaanOut$fit.strict.residuals,
                                                            c("cfi","tli","rmsea","srmr","chisq","df")),3))

##EA 
fits_ea_hi[7,] <- c("EA Model - Bifactor",round(fitmeasures(multi_bifactor_mod_eahi$convention.sem$LavaanOut$fit.configural.g1,
                                                      c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Hispanic
fits_ea_hi[8,] <- c("Hispanic Model - Bifactor",round(fitmeasures(multi_bifactor_mod_eahi$convention.sem$LavaanOut$fit.configural.g2,
                                                      c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Configural
fits_ea_hi[9,] <- c("Configural Model - Bifactor",round(fitmeasures(multi_bifactor_mod_eahi$convention.sem$LavaanOut$fit.combine.groups,
                                                              c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Metric
fits_ea_hi[10,] <- c("Metric Model - Bifactor",round(fitmeasures(multi_bifactor_mod_eahi$convention.sem$LavaanOut$fit.metric,
                                                           c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Scalar
fits_ea_hi[11,] <- c("Scalar Model - Bifactor",round(fitmeasures(multi_bifactor_mod_eahi$convention.sem$LavaanOut$fit.scalar,
                                                           c("cfi","tli","rmsea","srmr","chisq","df")),3))

##Strict(residual) invariance
fits_ea_hi[12,] <- c("Strict Model - Bifactor",round(fitmeasures(multi_bifactor_mod_eahi$convention.sem$LavaanOut$fit.strict.residuals,
                                                           c("cfi","tli","rmsea","srmr","chisq","df")),3))
kable(fits_ea_hi)
write.csv(fits_ea_hi, file = "abcd_cbclFits_invariance_ea_hi.csv")


####AA and Hispanic
###AA and Hispanic comparison model fits
##AA 
fits_aa_hi[1,] <- c("AA Model - Correlated",round(fitmeasures(multi_mod_aahi$convention.sem$LavaanOut$fit.configural.g1,
                                                              c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Hispanic
fits_aa_hi[2,] <- c("Hispanic Model - Correlated",round(fitmeasures(multi_mod_aahi$convention.sem$LavaanOut$fit.configural.g2,
                                                                    c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Configural
fits_aa_hi[3,] <- c("Configural Model - Correlated",round(fitmeasures(multi_mod_aahi$convention.sem$LavaanOut$fit.combine.groups,
                                                                      c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Metric
fits_aa_hi[4,] <- c("Metric Model - Correlated",round(fitmeasures(multi_mod_aahi$convention.sem$LavaanOut$fit.metric,
                                                                  c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Scalar
fits_aa_hi[5,] <- c("Scalar Model - Correlated",round(fitmeasures(multi_mod_aahi$convention.sem$LavaanOut$fit.scalar,
                                                                  c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Strict(residual) invariance
fits_aa_hi[6,] <- c("Strict Model - Correlated",round(fitmeasures(multi_mod_aahi$convention.sem$LavaanOut$fit.strict.residuals,
                                                                  c("cfi","tli","rmsea","srmr","chisq","df")),3))

##AA
fits_aa_hi[7,] <- c("AA Model - Bifactor",round(fitmeasures(multi_bifactor_mod_aahi$convention.sem$LavaanOut$fit.configural.g1,
                                                            c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Hispanic
fits_aa_hi[8,] <- c("Hispanic Model - Bifactor",round(fitmeasures(multi_bifactor_mod_aahi$convention.sem$LavaanOut$fit.configural.g2,
                                                                  c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Configural
fits_aa_hi[9,] <- c("Configural Model - Bifactor",round(fitmeasures(multi_bifactor_mod_aahi$convention.sem$LavaanOut$fit.combine.groups,
                                                                    c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Metric
fits_aa_hi[10,] <- c("Metric Model - Bifactor",round(fitmeasures(multi_bifactor_mod_aahi$convention.sem$LavaanOut$fit.metric,
                                                                 c("cfi","tli","rmsea","srmr","chisq","df")),3))
##Scalar
fits_aa_hi[11,] <- c("Scalar Model - Bifactor",round(fitmeasures(multi_bifactor_mod_aahi$convention.sem$LavaanOut$fit.scalar,
                                                                 c("cfi","tli","rmsea","srmr","chisq","df")),3))

##Strict(residual) invariance
fits_aa_hi[12,] <- c("Strict Model - Bifactor",round(fitmeasures(multi_bifactor_mod_aahi$convention.sem$LavaanOut$fit.strict.residuals,
                                                                 c("cfi","tli","rmsea","srmr","chisq","df")),3))
kable(fits_aa_hi)
write.csv(fits_aa_hi, file = "abcd_cbclFits_invariance_aa_hi.csv")


####save factor scores####
FS_bi <- lavPredict(multi_bifactor_mod$convention.sem$LavaanOut$fit.scalar, type = "lv",append.data = TRUE, assemble = TRUE)

####Final merged factor scores with individual items####
threeGroup[EA_AA_compare,] <- cbind(threeGroup[EA_AA_compare,], FS_bi[,-7])

#write.csv(twoGroup, file = "abcd_twoGroupCBCL_fscores.csv")
###another approach
idx <- lavInspect(multi_bifactor_mod$convention.sem$LavaanOut$fit.scalar, "case.idx") # list: 1 vector per group
fscores <- lavPredict(multi_bifactor_mod$convention.sem$LavaanOut$fit.scalar, newdata = threeGroup[EA_AA_compare,])         # list: 1 matrix per group
twogroup <- threeGroup[EA_AA_compare,]
## loop over groups and factors
for (g in seq_along(fscores)) {
  for (fs in colnames(fscores[[g]])) {
    twogroup[ idx[[g]], fs] <- fscores[[g]][ , fs]
  }
}
twogroup

