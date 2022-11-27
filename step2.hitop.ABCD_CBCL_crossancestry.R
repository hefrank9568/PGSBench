##based on abcd_cbcldemo loaded in step2.hitop.ABCD_CBCL.R
###select low frequency items and remove them, per Michelini methods

##non Hispanic EA (n=7166)
freq_cbcl_ea <- abcd_cbcldemo %>%
  filter(Race == 1) %>%
  select(ends_with("_p")) %>%
  lapply(table)
##AA (n=1862)
freq_cbcl_aa <- abcd_cbcldemo %>%
  filter(Race == 2) %>%
  select(ends_with("_p")) %>%
  lapply(table)
##Hispanic (n=2411)
freq_cbcl_hispanic <- abcd_cbcldemo %>%
  filter(Race == 3) %>%
  select(ends_with("_p")) %>%
  lapply(table)

##low frequency response items in the three groups -> very similar; so not going to exclude different items by group
which(unlist(freq_cbcl_ea) > 7166*.95)
which(unlist(freq_cbcl_aa) > 1862*.95)
which(unlist(freq_cbcl_hispanic) > 2411*.95)

##save the names of items that have high number of 0s; at least 95% of 0
names <- which(unlist(freq_cbcl) >= nrow(abcd_cbcldemo) * 0.95) %>%
  names()
##extract only the item number from the list of items
cbclItemsRmv <- sapply(strsplit(names, split = "_"),`[`, 2)
##concatenate (add) cbcl and p around the item numbers
cbclItemsRmv <- paste("cbcl_",cbclItemsRmv, "_p", sep="")
##select items that have reasonable frequency of non-zero responses
cbcl_select <- abcd_cbcldemo %>%
  select(Race,starts_with("cbcl_q"), -all_of(cbclItemsRmv))

##check correlation
cbcl_select <- as.data.frame(lapply(cbcl_select, as.numeric))

##check correlation across groups
corrCBCL_ea <- cbcl_select %>%
  filter(Race ==1) %>%
  psych::polychoric()
corrCBCL_aa <- cbcl_select %>%
  filter(Race ==2) %>%
  psych::polychoric()
corrCBCL_other <- cbcl_select %>%
  filter(Race ==3) %>%
  psych::polychoric()

##select highly correlated items
corrCBCL_ea$rho[which(corrCBCL_ea$rho >.75 & corrCBCL_ea$rho !=1)]
corrCBCL_aa$rho[which(corrCBCL_aa$rho >.75 & corrCBCL_aa$rho !=1)]

##integer value when dividing the index number within the correlation matrix by total item number in cbcl_select, plus 1
x.ea <- as.integer(which(corrCBCL_ea$rho >.75 & corrCBCL_ea$rho !=1)/ncol(cbcl_select[cbcl_select$Race==1,-1])) + 1
##the residual after substracting raw index number by the integers times total item number
y.ea <- which(corrCBCL_ea$rho >.75 & corrCBCL_ea$rho !=1) - 
  (as.integer(which(corrCBCL_ea$rho >.75 & corrCBCL_ea$rho !=1)/ncol(cbcl_select[cbcl_select$Race==1,-1])) * ncol(cbcl_select[cbcl_select$Race==1,-1]))
##save the names in the list of the correlation matrix so that I know the items that are correlated with each other
rhoNames.ea <- dimnames(corrCBCL_ea$rho)[[1]]
rhoNames.ea[x.ea]
rhoNames.ea[y.ea]



##check correlation across groups
corrCBCL_ea <- cbcl_select %>%
  filter(Race ==1) %>%
  psych::polychoric()
corrCBCL_aa <- cbcl_select %>%
  filter(Race ==2) %>%
  psych::polychoric()
corrCBCL_other <- cbcl_select %>%
  filter(Race ==3) %>%
  psych::polychoric()
