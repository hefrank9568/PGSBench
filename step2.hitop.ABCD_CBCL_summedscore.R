####produce summed scores for the five dimensions from CBCL. after running part of step2.hitop.ABCD_CBCL.R####
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

####generate score and put in cbcl_select 
cbcl_select$EXT <- rowSums(cbcl_select[,all_of(EXT_items)])
cbcl_select$Neuro <- rowSums(cbcl_select[,all_of(Neuro_items)])
cbcl_select$INT <- rowSums(cbcl_select[,all_of(INT_items)])
cbcl_select$Soma <- rowSums(cbcl_select[,all_of(Soma_items)])
cbcl_select$Detach <- rowSums(cbcl_select[,all_of(Detach_items)])
##P factor
Pitems <- cbcl_select %>%
  select(starts_with("cbcl_")) %>%
  names()
cbcl_select$P <- rowSums(cbcl_select[,all_of(Pitems)])

write.csv(cbcl_select[,c("EXT","Neuro","INT","Soma","Detach","P","subjectkey")], file = "cbcl_summedscores_allgroups.csv")
