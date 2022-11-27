####based on abcd_pksadsDiag
#first need to convert the string data to numeric
abcd_pksadsDiag <- as.data.frame(abcd_pksadsDiag)
abcd_pksadsDiag[,-c(2,3,4,6)] <- abcd_pksadsDiag[,-c(2,3,4,6)] %>%
  lapply(as.numeric) %>%
  as.data.frame()

##recode sex to numeric
abcd_pksadsDiag$sex <- recode(abcd_pksadsDiag$sex, 'M' = 0, 'F' = 1)

abcd_pksadsDiag <- merge(abcd_pksadsDiag, abcd_pdemo[,-c(2:5)], by = "subjectkey")
abcd_pksadsDiag <- abcd_pksadsDiag %>%
  mutate(hispanic = ifelse(demo_ethn_v2==1 & demo_ethn_v2!=777 & demo_ethn_v2 !=999,1,0))
abcd_pksadsDiag <- abcd_pksadsDiag %>%
  mutate(Race = ifelse(demo_race_a_p___10==1 & hispanic == 0, 1, 
                       ifelse(demo_race_a_p___11==1 & hispanic == 0, 2, 
                              ifelse(hispanic==1, 3, 4))))
table(abcd_pksadsDiag$Race)
table(abcd_pksadsDiag$hispanic)
##select current diagnoses; removing four diagnoses that have all NAs (not assessed?), also removing suicidal ideation/behavior because irrelevant
abcd_pksadsDiagCurrent <- abcd_pksadsDiag %>%
  select("subjectkey","Race",ends_with("_crt"),c("ksads_cd_crt_child","ksads_cd_crt_adol","ksads_tic_unspe_pst","ksads_asd_unspe"),
         -c("ksads_tic_unspe_crt","ksads_enure_crt","ksads_encop_crt","ksads_selmutism_crt"),
         -c("ksads_sipass_crt","ksads_sinjbeh_crt","ksads_satt_crt","ksads_hom_crt"))
#write.csv(abcd_pksadsDiagCurrent,file = "abcd_ksadsCurrent.csv")

##select past diagnoses; current diagnoses prevalences are too low for most disorders, including MDD, gad, soc
abcd_pksadsDiagPast <- abcd_pksadsDiag %>%
  select(ends_with("_pst"),c("ksads_cd_pst_child","ksads_cd_pst_adol","ksads_tic_unspe_pst","ksads_asd_unspe"),
         -c("ksads_tic_unspe_pst","ksads_enure_pst","ksads_encop_pst","ksads_selmutism_pst"),
         -c("ksads_sipass_pst","ksads_sinjbeh_pst","ksads_satt_pst","ksads_hom_pst"))

#prevalence of current and past disorders
abcd_pksadsDiagCurrent %>%
  lapply(table)

abcd_pksadsDiagPast %>%
  lapply(table)

####select a number of current diagnoses####
abcd_pksadsDiagCurrentSelect <- abcd_pksadsDiagCurrent %>%
  select(contains(c("delu","asspsy","pho","gad","ocd","adhd","odd","slp_crt","cd_crt_child")))

names(abcd_pksadsDiagCurrentSelect)

####select a number of past diagnoses####
#abcd_pksadsDiagPastSelect <- abcd_pksadsDiagPast %>%
#  select(contains(c("_mdd","bip1man","delu","asspsy","sep","soc","pho","gad","ocd","adhd","odd","ptsd_pst","slp_pst","cd_pst_child")))
####EFA for current diagnoses####
##using promax to allow for correlated factors; the default was varimax (orthogonal)
cormat_current <- corr.test(abcd_pksadsDiagCurrentSelect)

####how many factors####
library(nFactors)
ev <- eigen(cormat_current$r) # get eigenvalues
ap <- parallel(subject=nrow(abcd_pksadsDiagCurrentSelect),var=ncol(abcd_pksadsDiagCurrentSelect),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

##using the psych package fa function
one_crt <- fa(cormat_current$r,nfactors = 1,n.obs = 11748)
two_crt <- fa(cormat_current$r,nfactors = 2,n.obs = 11748)
three_crt <- fa(cormat_current$r,nfactors = 3,n.obs = 11748)
four_crt <- fa(cormat_current$r,nfactors = 4,n.obs = 11748)
five_crt <- fa(cormat_current$r,nfactors = 5,n.obs = 11748)

##showing diagrams of the EFA results
fa.diagram(two_crt, digits = 2, cut = .15, simple = FALSE)
fa.diagram(three_crt, digits = 2, cut = .15, simple=FALSE)
fa.diagram(four_crt, digits = 2, cut = .15, simple = FALSE)
fa.diagram(five_crt, digits = 2, cut = .2, simple = FALSE)

require(Matrix)
####CFA for current diagnoses####
##model fit indices 
fits_crt <- tibble(model = character(),
                   chisq = numeric(),
                   df=numeric(),
                   rmsea = numeric(),
                   cfi = numeric(),
                   aic = numeric(),
                   bic = numeric())

##One factor
modone_crt <- structure.sem(one_crt, cut = .15)
semone_crt <- sem::sem(modone_crt, cormat_current$r, 11748, standardized = TRUE)

fit_onecrt <- summary(semone_crt)
##add the fit statistics
fits_crt <- bind_rows(fits_crt,
                      tibble(model = "one factor",
                             chisq = fit_onecrt$chisq,
                             df = fit_onecrt$df,
                             rmsea = fit_onecrt$RMSEA[1],
                             cfi = fit_onecrt$CFI,
                             aic = fit_onecrt$AIC,
                             bic = fit_onecrt$BIC))

##two factors
modtwo_crt <- structure.sem(two_crt, cut = .15)
semtwo_crt <- sem::sem(modtwo_crt, cormat_current$r, 11748, standardized = TRUE)
opt <- options(fit.indices = c("CFI","TLI","BIC","AIC","RMSEA","SRMR"))
fit_twocrt <- summary(semtwo_crt)
##add the fit statistics
fits_crt <- bind_rows(fits_crt,
                      tibble(model = "two factor",
                      chisq = fit_twocrt$chisq,
                      df = fit_twocrt$df,
                      rmsea = fit_twocrt$RMSEA[1],
                      cfi = fit_twocrt$CFI,
                      aic = fit_twocrt$AIC,
                      bic = fit_twocrt$BIC))

####path diagrams
sem::pathDiagram(semtwo_crt,edge.labels = "values", digits = 2)
####get factor scores for the factors
sem::fscores(semtwo_crt,abcd_pksadsDiagCurrentSelect, center = TRUE, scale = TRUE)

##three factor 
modthree_crt <- structure.sem(three_crt, cut = .15)
semthree_crt <- sem::sem(modthree_crt, cormat_current$r, 11748)
fit_threecrt <- summary(semthree_crt)

fits_crt <- bind_rows(fits_crt,
                      tibble(model = "three factor",
                             chisq = fit_threecrt$chisq,
                             df = fit_threecrt$df,
                             rmsea = fit_threecrt$RMSEA[1],
                             cfi = fit_threecrt$CFI,
                             aic = fit_threecrt$AIC,
                             bic = fit_threecrt$BIC))
####path diagram 
sem::pathDiagram(semthree_crt,edge.labels = "values", digits = 2)

##four factor -> does not fit after excluding MDD and SOC because of low prevalence
modfour_crt <- structure.sem(four_crt, cut = .15)
semfour_crt <- sem::sem(modfour_crt, cormat_current$r, 11748)
fit_fourcrt <- summary(semfour_crt)

fits_crt <- bind_rows(fits_crt,
                      tibble(model = "four factor",
                             chisq = fit_fourcrt$chisq,
                             df = fit_fourcrt$df,
                             rmsea = fit_fourcrt$RMSEA[1],
                             cfi = fit_fourcrt$CFI,
                             aic = fit_fourcrt$AIC,
                             bic = fit_fourcrt$BIC))
sem::pathDiagram(semfour_crt,edge.labels = "values", digits = 2)

##five factor -> underidentified; soc loaded onto its own factor. 
modfive_crt <- structure.sem(five_crt, cut = .3)
semfive_crt <- sem::sem(modfive_crt, cormat_current$r, 11748)
fit_fivecrt <- summary(semfive_crt)

sem::pathDiagram(semfive_crt,edge.labels = "values", digits = 2)

####bifactor modeling####
##one way to do bifactor modeling\
threebi_crt <- omega(cormat_current$r, nfactors = 3, n.obs = 11748)
summary(sem::sem(threebi_crt$model$sem, cormat_current$r, 11878))

fourbi_crt <- omega(cormat_current$r, nfactors = 4, n.obs = 11748)
summary(sem::sem(fourbi_crt$model$sem, cormat_current$r, 11878))

summary(omegaSem(cormat_current$r, nfactors = 3, n.obs = 11748, cut = .15))
summary(omegaSem(cormat_current$r, nfactors = 4, n.obs = 11748, cut = .15))


####add general factor on best fitting correlated factor model####
library(lavaan)
##bifactor two factor model not identified
twobi_crt_mod <- "G =~ ksads_slp_crt + ksads_ocd_crt + ksads_gad_crt + ksads_pho_crt + ksads_adhd_crt + ksads_cd_crt_child + ksads_odd_crt + ksads_delu_crt + ksads_asspsy_crt
               thought =~  ksads_delu_crt + ksads_asspsy_crt 
               distress =~ ksads_pho_crt + ksads_slp_crt + ksads_ocd_crt + ksads_gad_crt + ksads_adhd_crt + ksads_cd_crt_child + ksads_odd_crt 
               G ~~ 0*thought
               G ~~ 0*distress"

twobi_crt <- cfa(twobi_crt_mod, data = abcd_pksadsDiagCurrentSelect)
summary(twobi_crt, fit.measures = T)


threebi_crt_mod <- "G =~ ksads_delu_crt + ksads_asspsy_crt + ksads_gad_crt + ksads_ocd_crt + ksads_slp_crt + ksads_pho_crt + ksads_adhd_crt + ksads_cd_crt_child + ksads_odd_crt
               thought   =~ ksads_delu_crt + ksads_asspsy_crt
               distress =~ ksads_gad_crt + ksads_ocd_crt + ksads_slp_crt + ksads_pho_crt + ksads_adhd_crt
               externalizing   =~ ksads_cd_crt_child + ksads_adhd_crt + ksads_odd_crt 
               G ~~ 0*thought
               G ~~ 0*distress
               G ~~ 0*externalizing"
threebi_crt <- cfa(threebi_crt_mod, data = abcd_pksadsDiagCurrentSelect)
summary(threebi_crt, fit.measures = T)
standardizedsolution(threebi_crt)

semPlot::semPaths(threebi_crt, layout = "tree2", bifactor = "G", what = "std", edge.label.cex = 0.8, residuals = FALSE, intercepts = FALSE)

####add the factor score to the data frame and add subjectkey
abcd_pksadsDiagCurrentSelect$subjectkey <- abcd_pksadsDiag$subjectkey
idx <- lavInspect(threebi_crt, "case.idx")
fscores <- lavPredict(threebi_crt)
## loop over factors
for (fs in colnames(fscores)) {
  abcd_pksadsDiagCurrentSelect[idx, fs] <- fscores[ , fs]
}

#write.csv(abcd_pksadsDiagCurrentSelect, file = "abcd_ksadsCurrent_fs.csv")




####EFA for current diagnoses in EA (n=8715)####

##using promax to allow for correlated factors; the default was varimax (orthogonal)

abcd_pksadsDiagCurrent %>%
  filter(Race == 1) %>%
  lapply(table)
lavTables(abcd_pksadsDiagCurrentSelect_ea, categorical = colnames(abcd_pksadsDiagCurrentSelect_ea))
##compared to total sample, removed delusion because of low prevalence (<1%)
abcd_pksadsDiagCurrentSelect_ea <- abcd_pksadsDiagCurrent %>%
  filter(Race ==1) %>%
  select(contains(c("pho","gad","ocd","adhd","odd","slp_crt","cd_crt_child","asspsy")))

cormat_current_ea <- corr.test(abcd_pksadsDiagCurrentSelect_ea)

####how many factors####
library(nFactors)
ev <- eigen(cormat_current_ea$r) # get eigenvalues
ap <- parallel(subject=nrow(abcd_pksadsDiagCurrentSelect_ea),var=ncol(abcd_pksadsDiagCurrentSelect_ea),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

##using the psych package fa function
one_crt_ea <- fa(cormat_current_ea$r,nfactors = 1,n.obs = 11748)
two_crt_ea <- fa(cormat_current_ea$r,nfactors = 2,n.obs = 11748)
three_crt_ea <- fa(cormat_current_ea$r,nfactors = 3,n.obs = 11748)
four_crt_ea <- fa(cormat_current_ea$r,nfactors = 4,n.obs = 11748)
five_crt_ea <- fa(cormat_current_ea$r,nfactors = 5,n.obs = 11748)

##showing diagrams of the EFA results
fa.diagram(two_crt_ea, digits = 2, cut = .15, simple = FALSE)
fa.diagram(three_crt_ea, digits = 2, cut = .15, simple=FALSE)
fa.diagram(four_crt_ea, digits = 2, cut = .15, simple = FALSE)
fa.diagram(five_crt_ea, digits = 2, cut = .2, simple = FALSE)

require(Matrix)
####CFA for current diagnoses in EA####
##model fit indices 
fits_crt_ea <- tibble(model = character(),
                   chisq = numeric(),
                   df=numeric(),
                   rmsea = numeric(),
                   cfi = numeric(),
                   aic = numeric(),
                   bic = numeric())
opt <- options(fit.indices = c("CFI","TLI","BIC","AIC","RMSEA","SRMR"))
##One factor
modone_crt_ea <- structure.sem(one_crt_ea, cut = .15)
semone_crt_ea <- sem::sem(modone_crt_ea, cormat_current_ea$r, 11748, standardized = TRUE)

fit_onecrt_ea <- summary(semone_crt_ea)

##add the fit statistics
fits_crt_ea <- bind_rows(fits_crt_ea,
                      tibble(model = "one factor",
                             chisq = fit_onecrt_ea$chisq,
                             df = fit_onecrt_ea$df,
                             rmsea = fit_onecrt_ea$RMSEA[1],
                             cfi = fit_onecrt_ea$CFI,
                             aic = fit_onecrt_ea$AIC,
                             bic = fit_onecrt_ea$BIC))

##two factors
modtwo_crt_ea <- structure.sem(two_crt_ea, cut = .15)
semtwo_crt_ea <- sem::sem(modtwo_crt_ea, cormat_current_ea$r, 11748, standardized = TRUE)
fit_twocrt_ea <- summary(semtwo_crt_ea)
##add the fit statistics
fits_crt_ea <- bind_rows(fits_crt_ea,
                      tibble(model = "two factor",
                             chisq = fit_twocrt_ea$chisq,
                             df = fit_twocrt_ea$df,
                             rmsea = fit_twocrt_ea$RMSEA[1],
                             cfi = fit_twocrt_ea$CFI,
                             aic = fit_twocrt_ea$AIC,
                             bic = fit_twocrt_ea$BIC))

####path diagrams
sem::pathDiagram(semtwo_crt_ea,edge.labels = "values", digits = 2)
####factor scores
#two_ea_fscores <- sem::fscores(semtwo_crt_ea, abcd_pksadsDiagCurrentSelect_ea)

##three factor -> cannot be identified. no more factors can be extracted and fitted
modthree_crt_ea <- structure.sem(three_crt_ea, cut = .15)
semthree_crt_ea <- sem::sem(modthree_crt_ea, cormat_current_ea$r, 11748)
fit_threecrt_ea <- summary(semthree_crt_ea)

####add general factor on best fitting correlated factor model in EA####
library(lavaan)
##bifactor two factor -> cannot fit if specify Ordered/categorical estimator
twobi_crt_ea_mod <- "G =~ ksads_gad_crt + ksads_adhd_crt + ksads_slp_crt + ksads_ocd_crt + ksads_pho_crt + ksads_cd_crt_child + ksads_odd_crt
               externalizing =~ ksads_cd_crt_child + ksads_odd_crt + ksads_adhd_crt
               distress =~ ksads_pho_crt  + ksads_slp_crt + ksads_ocd_crt + ksads_gad_crt
               G ~~ 0*externalizing
               G ~~ 0*distress"

twobi_crt_ea <- cfa(twobi_crt_ea_mod, data = abcd_pksadsDiagCurrentSelect_ea, ordered = TRUE )
summary(twobi_crt_ea, fit.measures = T)
standardizedsolution(twobi_crt_ea)

##bifactor s-1 (thought/DISTRESS as reference)
twobis1_crt_ea_mod <-  "G =~ ksads_gad_crt + ksads_adhd_crt + ksads_slp_crt + ksads_ocd_crt + ksads_pho_crt + ksads_cd_crt_child + ksads_odd_crt + ksads_asspsy_crt
               externalizing =~ ksads_cd_crt_child + ksads_odd_crt + ksads_adhd_crt
               G ~~ 0*externalizing
               ksads_cd_crt_child ~~  ksads_asspsy_crt"

twobis1_crt_ea <- cfa(twobis1_crt_ea_mod, data = abcd_pksadsDiagCurrentSelect_ea, estimator = "WLSMV")
summary(twobis1_crt_ea, fit.measures = T)
modindices(twobis1_crt_ea)
standardizedsolution(twobis1_crt_aa)

##bifactor s-1 (Externalizing as reference)
twobis1_crt_ea_mod_EXTreference <-  "G =~ ksads_adhd_crt + ksads_gad_crt + ksads_slp_crt + ksads_ocd_crt + ksads_pho_crt + ksads_cd_crt_child + ksads_odd_crt + ksads_asspsy_crt
               thought =~ ksads_pho_crt  + ksads_slp_crt + ksads_ocd_crt + ksads_gad_crt + ksads_asspsy_crt
               G ~~ 0*thought"

twobis1_crt_ea_EXTreference <- cfa(twobis1_crt_ea_mod_EXTreference, data = abcd_pksadsDiagCurrentSelect_ea, estimator = "WLSMV")
summary(twobis1_crt_ea_EXTreference, fit.measures = T)

standardizedsolution(twobis1_crt_ea_EXTreference)

####add the factor score to the data frame and add subjectkey
#abcd_pksadsDiagCurrentSelect_ea$subjectkey <- abcd_pksadsDiag[abcd_pksadsDiag$Race==1,]$subjectkey

#idx <- lavInspect(twobi_crt_ea, "case.idx")
#fscores <- lavPredict(twobi_crt_ea, abcd_pksadsDiagCurrentSelect_ea)
## loop over factors
#for (fs in colnames(fscores)) {
#  abcd_pksadsDiagCurrentSelect_ea[idx, fs] <- fscores[ , fs]
#}

####EFA for current diagnoses in AA (n=1991)####

##using promax to allow for correlated factors; the default was varimax (orthogonal)

abcd_pksadsDiagCurrent %>%
  filter(Race == 2) %>%
  lapply(table)

##compared to total sample, although SOC had higher than 1% prevalence, the models that include it had either very low (<.15) loading or uninterpretable patterns
##remove soc
abcd_pksadsDiagCurrentSelect_aa <- abcd_pksadsDiagCurrent %>%
  filter(Race ==2) %>%
  select(contains(c("delu","asspsy","pho","ocd","adhd","odd","slp_crt","cd_crt_child","gad")))

cormat_current_aa <- corr.test(abcd_pksadsDiagCurrentSelect_aa)

####how many factors####
library(nFactors)
ev <- eigen(cormat_current_aa$r) # get eigenvalues
ap <- parallel(subject=nrow(abcd_pksadsDiagCurrentSelect_aa),var=ncol(abcd_pksadsDiagCurrentSelect_aa),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

##using the psych package fa function
one_crt_aa <- fa(cormat_current_aa$r,nfactors = 1,n.obs = 11748)
two_crt_aa <- fa(cormat_current_aa$r,nfactors = 2,n.obs = 11748)
three_crt_aa <- fa(cormat_current_aa$r,nfactors = 3,n.obs = 11748)
four_crt_aa <- fa(cormat_current_aa$r,nfactors = 4,n.obs = 11748)
five_crt_aa <- fa(cormat_current_aa$r,nfactors = 5,n.obs = 11748)

##showing diagrams of the EFA results
fa.diagram(two_crt_aa, digits = 2, cut = .15, simple = FALSE)
fa.diagram(three_crt_aa, digits = 2, cut = .15, simple=FALSE)
fa.diagram(four_crt_aa, digits = 2, cut = .15, simple = FALSE)
fa.diagram(five_crt_aa, digits = 2, cut = .2, simple = FALSE)

require(Matrix)
####CFA for current diagnoses in EA####
##model fit indices 
fits_crt_aa <- tibble(model = character(),
                      chisq = numeric(),
                      df=numeric(),
                      rmsea = numeric(),
                      cfi = numeric(),
                      aic = numeric(),
                      bic = numeric())

##One factor
modone_crt_aa <- structure.sem(one_crt_aa, cut = .15)
semone_crt_aa <- sem::sem(modone_crt_aa, cormat_current_aa$r, 11748, standardized = TRUE)

fit_onecrt_aa <- summary(semone_crt_aa)

##add the fit statistics
fits_crt_aa <- bind_rows(fits_crt_aa,
                         tibble(model = "one factor",
                                chisq = fit_onecrt_aa$chisq,
                                df = fit_onecrt_aa$df,
                                rmsea = fit_onecrt_aa$RMSEA[1],
                                cfi = fit_onecrt_aa$CFI,
                                aic = fit_onecrt_aa$AIC,
                                bic = fit_onecrt_aa$BIC))

##two factors
modtwo_crt_aa <- structure.sem(two_crt_aa, cut = .15)
semtwo_crt_aa <- sem::sem(modtwo_crt_aa, cormat_current_aa$r, 11748, standardized = TRUE)
fit_twocrt_aa <- summary(semtwo_crt_aa)
##add the fit statistics
fits_crt_aa <- bind_rows(fits_crt_aa,
                         tibble(model = "two factor",
                                chisq = fit_twocrt_aa$chisq,
                                df = fit_twocrt_aa$df,
                                rmsea = fit_twocrt_aa$RMSEA[1],
                                cfi = fit_twocrt_aa$CFI,
                                aic = fit_twocrt_aa$AIC,
                                bic = fit_twocrt_aa$BIC))

####path diagrams
sem::pathDiagram(semtwo_crt_aa,edge.labels = "values", digits = 2)
####factor scores
#two_ea_fscores <- sem::fscores(semtwo_crt_ea, abcd_pksadsDiagCurrentSelect_ea)

##three factor 
modthree_crt_aa <- structure.sem(three_crt_aa, cut = .15)
semthree_crt_aa <- sem::sem(modthree_crt_aa, cormat_current_aa$r, 11748)
fit_threecrt_aa <- summary(semthree_crt_aa)
##add the fit statistics
fits_crt_aa <- bind_rows(fits_crt_aa,
                         tibble(model = "three factor",
                                chisq = fit_threecrt_aa$chisq,
                                df = fit_threecrt_aa$df,
                                rmsea = fit_threecrt_aa$RMSEA[1],
                                cfi = fit_threecrt_aa$CFI,
                                aic = fit_threecrt_aa$AIC,
                                bic = fit_threecrt_aa$BIC))
####path diagrams
sem::pathDiagram(semthree_crt_aa,edge.labels = "values", digits = 2)

####four factor model cannot be identified
modfour_crt_aa <- structure.sem(four_crt_aa, cut = .15)
semfour_crt_aa <- sem::sem(modfour_crt_aa, cormat_current_aa$r, 11748)
fit_fourcrt_aa <- summary(semfour_crt_aa)

####add general factor on best fitting correlated factor model in AA####
library(lavaan)
##bifactor two factor 
twobi_crt_aa_mod <- "G =~ ksads_adhd_crt + ksads_cd_crt_child + ksads_odd_crt + ksads_pho_crt + ksads_ocd_crt + ksads_slp_crt +ksads_ocd_crt + ksads_delu_crt + ksads_asspsy_crt
               distress =~ ksads_cd_crt_child + ksads_odd_crt + ksads_adhd_crt + ksads_pho_crt + ksads_ocd_crt + ksads_slp_crt + ksads_ocd_crt
               thought =~ ksads_ocd_crt + ksads_delu_crt + ksads_asspsy_crt
               G ~~ 0*thought
               G ~~ 0*distress"

twobi_crt_aa <- cfa(twobi_crt_aa_mod, data = abcd_pksadsDiagCurrentSelect_aa, estimator = "WLSMV")
summary(twobi_crt_aa, fit.measures = T)
standardizedsolution(twobi_crt_aa)

###bifactor s-1 (distress/externalizing as reference)
twobis1_crt_aa_mod <- "G =~ ksads_adhd_crt + ksads_cd_crt_child + ksads_odd_crt + ksads_pho_crt + ksads_ocd_crt + ksads_slp_crt + ksads_delu_crt + ksads_asspsy_crt
               thought =~ ksads_ocd_crt + ksads_delu_crt + ksads_asspsy_crt
               G ~~ 0*thought"
twobis1_crt_aa <- cfa(twobis1_crt_aa_mod, data = abcd_pksadsDiagCurrentSelect_aa, estimator = "WLSMV")
summary(twobis1_crt_aa, fit.measures = T)

standardizedsolution(twobis1_crt_aa)

###bifactor s-1 (thought as reference)
twobis1distress_crt_aa_mod <- "G =~ ksads_adhd_crt + ksads_cd_crt_child + ksads_odd_crt + ksads_pho_crt + ksads_ocd_crt + ksads_slp_crt + ksads_delu_crt + ksads_asspsy_crt
              distress =~ ksads_cd_crt_child + ksads_odd_crt + ksads_adhd_crt + ksads_pho_crt + ksads_ocd_crt + ksads_slp_crt + ksads_ocd_crt
               G ~~ 0*distress"
twobis1distress_crt_aa <- cfa(twobis1distress_crt_aa_mod, data = abcd_pksadsDiagCurrentSelect_aa, estimator = "WLSMV")
summary(twobis1distress_crt_aa, fit.measures = T)

standardizedsolution(twobis1_crt_aa)


####would the EA model work in aa?
twobieamodel_crt_aa <- cfa(twobi_crt_ea_mod, data = abcd_pksadsDiagCurrentSelect_aa, estimator = "WLSMV")
summary(twobieamodel_crt_aa, fit.measures = T)

###bifactor s-1 using EA model, thought/distress as reference
twobis1_crt_ea_mod <- "G =~ ksads_gad_crt + ksads_adhd_crt + ksads_slp_crt + ksads_ocd_crt + ksads_pho_crt + ksads_cd_crt_child + ksads_odd_crt + ksads_asspsy_crt
               externalizing =~ ksads_cd_crt_child + ksads_odd_crt + ksads_adhd_crt
               G ~~ 0*externalizing"

twobis1eamodel_crt_aa <- cfa(twobis1_crt_ea_mod, data = abcd_pksadsDiagCurrentSelect_aa, estimator = "WLSMV")
summary(twobis1eamodel_crt_aa, fit.measures = T)
standardizedsolution(twobis1eamodel_crt_aa)

###bifactor s-1 using EA model, externalizing as reference
twobis1_crt_ea_mod_EXTreference <-  "G =~ ksads_adhd_crt + ksads_gad_crt + ksads_slp_crt + ksads_ocd_crt + ksads_pho_crt + ksads_cd_crt_child + ksads_odd_crt + ksads_asspsy_crt
               thought =~ ksads_pho_crt  + ksads_slp_crt + ksads_ocd_crt + ksads_gad_crt + ksads_asspsy_crt
               G ~~ 0*thought"

twobis1eamodel_crt_aa_extReference <- cfa(twobis1_crt_ea_mod_EXTreference, data = abcd_pksadsDiagCurrentSelect_aa, estimator = "WLSMV")
summary(twobis1eamodel_crt_aa_extReference, fit.measures = T)
standardizedsolution(twobis1eamodel_crt_aa)

##bifactor three factor -> could not be fit after playing with it in different specifications
threebi_crt_aa_mod <- "G =~ ksads_pho_crt + ksads_odd_crt + ksads_adhd_crt + ksads_cd_crt_child  + ksads_ocd_crt + ksads_slp_crt + ksads_delu_crt + ksads_asspsy_crt
               externalizing =~ ksads_adhd_crt + ksads_odd_crt + ksads_cd_crt_child + ksads_ocd_crt
               thought =~ ksads_delu_crt + ksads_asspsy_crt + ksads_ocd_crt
               fear =~  ksads_pho_crt + ksads_slp_crt  + ksads_adhd_crt
               G ~~ 0*thought
               G ~~ 0*externalizing
               G ~~ 0*fear"

threebi_crt_aa <- cfa(threebi_crt_aa_mod, data = abcd_pksadsDiagCurrentSelect_aa, estimator = "WLSMV")
summary(threebi_crt_aa, fit.measures = T)

##bifactor three factor s-1 (externalizing as reference led to negative lv variances; )
threebis1_crt_aa_mod <- "G =~ ksads_pho_crt + ksads_odd_crt + ksads_adhd_crt + ksads_cd_crt_child  + ksads_ocd_crt + ksads_slp_crt + ksads_delu_crt + ksads_asspsy_crt
               thought =~ ksads_delu_crt + ksads_asspsy_crt + ksads_ocd_crt
               fear =~  ksads_pho_crt + ksads_slp_crt  + ksads_adhd_crt
               G ~~ 0*thought
               G ~~ 0*fear"

threebis1_crt_aa <- cfa(threebis1_crt_aa_mod, data = abcd_pksadsDiagCurrentSelect_aa, estimator = "WLSMV")


##bifactor three factor s-1 (thought as reference)
threebis1_thought_crt_aa_mod <- "G =~ ksads_pho_crt + ksads_odd_crt + ksads_adhd_crt + ksads_cd_crt_child  + ksads_ocd_crt + ksads_slp_crt + ksads_delu_crt + ksads_asspsy_crt
               externalizing =~ ksads_adhd_crt + ksads_odd_crt + ksads_cd_crt_child + ksads_ocd_crt
               fear =~  ksads_pho_crt + ksads_slp_crt  + ksads_adhd_crt
               G ~~ 0*externalizing
               G ~~ 0*fear"

threebis1_crt_aa <- cfa(threebis1_thought_crt_aa_mod, data = abcd_pksadsDiagCurrentSelect_aa, estimator = "WLSMV")
summary(threebis1_crt_aa, fit.measures = T)

##bifactor three factor s-1 (fear as reference)
threebis1_fear_crt_aa_mod <- "G =~ ksads_pho_crt + ksads_odd_crt + ksads_adhd_crt + ksads_cd_crt_child  + ksads_ocd_crt + ksads_slp_crt + ksads_delu_crt + ksads_asspsy_crt
               externalizing =~ ksads_adhd_crt + ksads_odd_crt + ksads_cd_crt_child + ksads_ocd_crt
               thought =~ ksads_delu_crt + ksads_asspsy_crt + ksads_ocd_crt
               G ~~ 0*externalizing
               G ~~ 0*thought"

threebis1fear_crt_aa <- cfa(threebis1_fear_crt_aa_mod, data = abcd_pksadsDiagCurrentSelect_aa, estimator = "WLSMV")
summary(threebis1fear_crt_aa, fit.measures = T)
####add the factor score to the data frame and add subjectkey



abcd_pksadsDiagCurrentSelect_aa$subjectkey <- abcd_pksadsDiag[abcd_pksadsDiag$Race==2,]$subjectkey

idx <- lavInspect(twobi_crt_aa, "case.idx")
fscores <- lavPredict(twobi_crt_aa, abcd_pksadsDiagCurrentSelect_aa)
## loop over factors
for (fs in colnames(fscores)) {
  abcd_pksadsDiagCurrentSelect_aa[idx, fs] <- fscores[ , fs]
}

##merge factor scores for AA and EA
abcd_pksadsDiagCurrentSelect_merged <- full_join(abcd_pksadsDiagCurrentSelect_ea[,c("subjectkey","G","externalizing","distress")],
                                                 abcd_pksadsDiagCurrentSelect_aa[,c("subjectkey","G","distress","thought")])



