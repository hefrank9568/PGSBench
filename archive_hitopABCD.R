####KSADS past diagnoses
####EFA for past diagnoses####
cormat_pst <- corr.test(abcd_pksadsDiagPastSelect)
ev <- eigen(cormat_pst$r) # get eigenvalues
ap <- parallel(subject=nrow(abcd_pksadsDiagPastSelect),var=ncol(abcd_pksadsDiagPastSelect),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

one_pst <- fa(cormat_pst$r, nfactors = 1, n.obs = 11748)
two_pst <- fa(cormat_pst$r, nfactors = 2, n.obs = 11748)
three_pst <- fa(cormat_pst$r, nfactors = 3, n.obs = 11748)
four_pst <- fa(cormat_pst$r, nfactors = 4, n.obs = 11748)
five_pst <- fa(cormat_pst$r, nfactors = 5, n.obs = 11748)

fa.diagram(two_pst, digits = 2, cut = .12, simple = FALSE)
fa.diagram(three_pst, digits = 2, cut = .12, simple=FALSE)
fa.diagram(four_pst, digits = 2, cut = .12, simple = FALSE)
fa.diagram(five_pst, digits = 2, cut = .12, simple = FALSE)

require(Matrix)
####CFA for past diagnoses####
##model fit indices 
fits_pst <- tibble(model = character(),
                   chisq = numeric(),
                   df=numeric(),
                   rmsea = numeric(),
                   cfi = numeric(),
                   aic = numeric(),
                   bic = numeric())

##One factor
modone_pst <- structure.sem(one_pst, cut = .15)
semone_pst <- sem::sem(modone_pst, cormat_pst$r, 11748, standardized = TRUE)

fit_onepst <- summary(semone_pst)
##add the fit statistics
fits_pst <- bind_rows(fits_pst,
                      tibble(model = "one factor",
                             chisq = fit_onepst$chisq,
                             df = fit_onepst$df,
                             rmsea = fit_onepst$RMSEA[1],
                             cfi = fit_onepst$CFI,
                             aic = fit_onepst$AIC,
                             bic = fit_onepst$BIC))
sem::pathDiagram(semone_pst,edge.labels = "values", digits = 2)


##two factor
modtwo_pst <- structure.sem(two_pst, cut = .15)
semtwo_pst <- sem::sem(modtwo_pst, cormat_pst$r, 11748, standardized = TRUE)

fit_twopst <- summary(semtwo_pst)
##add the fit statistics
fits_pst <- bind_rows(fits_pst,
                      tibble(model = "two factor",
                             chisq = fit_twopst$chisq,
                             df = fit_twopst$df,
                             rmsea = fit_twopst$RMSEA[1],
                             cfi = fit_twopst$CFI,
                             aic = fit_twopst$AIC,
                             bic = fit_twopst$BIC))
sem::pathDiagram(semtwo_pst,edge.labels = "values", digits = 2)

##three factor
modthree_pst <- structure.sem(three_pst, cut = .15)
semthree_pst <- sem::sem(modthree_pst, cormat_pst$r, 11748, standardized = TRUE)

fit_threepst <- summary(semthree_pst)
##add the fit statistics
fits_pst <- bind_rows(fits_pst,
                      tibble(model = "three factor",
                             chisq = fit_threepst$chisq,
                             df = fit_threepst$df,
                             rmsea = fit_threepst$RMSEA[1],
                             cfi = fit_threepst$CFI,
                             aic = fit_threepst$AIC,
                             bic = fit_threepst$BIC))
sem::pathDiagram(semthree_pst,edge.labels = "values", digits = 2)

##four factor -> cannot be identified
modfour_pst <- structure.sem(four_pst, cut = .15)
semfour_pst <- sem::sem(modfour_pst, cormat_pst$r, 11748, standardized = TRUE)

fit_fourpst <- summary(semfour_pst)
##add the fit statistics
fits_pst <- bind_rows(fits_pst,
                      tibble(model = "four factor",
                             chisq = fit_fourpst$chisq,
                             df = fit_fourpst$df,
                             rmsea = fit_fourpst$RMSEA[1],
                             cfi = fit_fourpst$CFI,
                             aic = fit_fourpst$AIC,
                             bic = fit_fourpst$BIC))

####bifactor modeling for past diagnosis####
##bifactor two factor
twobi_pst_mod <- "G =~ ksads_bip1man_pst + ksads_delu_pst + ksads_asspsy_pst + ksads_sep_pst + ksads_bip1man_pst  + ksads_soc_pst + ksads_ptsd_pst + 
                      ksads_gad_pst + ksads_ocd_pst + ksads_slp_pst + ksads_pho_pst + ksads_adhd_pst + ksads_odd_pst + ksads_cd_pst_child + ksads_mdd_pst
               thought =~  ksads_delu_pst + ksads_asspsy_pst + ksads_bip1man_pst
               distress =~ ksads_sep_pst + ksads_bip1man_pst  + ksads_soc_pst + ksads_ptsd_pst + ksads_gad_pst + ksads_ocd_pst + ksads_slp_pst + ksads_pho_pst + 
                      ksads_adhd_pst + ksads_odd_pst + ksads_cd_pst_child + ksads_mdd_pst
               G ~~ 0*thought
               G ~~ 0*distress"

twobi_pst <- cfa(twobi_pst_mod, data = abcd_pksadsDiagPastSelect)
summary(twobi_pst)
standardizedsolution(twobi_pst)
semPlot::semPaths(twobi_pst)

##bifactor three factor -> only works if removing bipolar from externalizing (low correlation with most traits; low loadings; similar to cd_pst_child; but cd_pst_child also very low prevalence (just reaching 1%))
threebi_pst_mod <- "G =~ ksads_delu_pst + ksads_asspsy_pst + ksads_sep_pst + ksads_soc_pst + ksads_ptsd_pst + 
                      ksads_gad_pst + ksads_ocd_pst + ksads_slp_pst + ksads_pho_pst + ksads_adhd_pst + ksads_odd_pst + ksads_mdd_pst + ksads_bip1man_pst
               thought =~  ksads_asspsy_pst + ksads_delu_pst
               distress =~ ksads_sep_pst  + ksads_soc_pst + ksads_ptsd_pst + ksads_gad_pst + ksads_ocd_pst + ksads_slp_pst + ksads_pho_pst + ksads_mdd_pst
               externalizing =~  ksads_adhd_pst + ksads_odd_pst
               G ~~ 0*thought
               G ~~ 0*distress
               G ~~ 0*externalizing"

threebi_pst <- cfa(threebi_pst_mod, data = abcd_pksadsDiagPastSelect)

summary(threebi_pst)
standardizedsolution(threebi_pst)

semPlot::semPaths(threebi_pst, bifactor = "G", layout = "tree2", what = "std")

