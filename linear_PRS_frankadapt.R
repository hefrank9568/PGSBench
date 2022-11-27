##this script was adapted from yuchang; the original scripts was used to compute PGS in Q's lab
setwd("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing")
library(tidyverse)
library(data.table)
options(stringsAsFactors = F)
rm(list = ls())
source("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/Q lab PRS pipeline/f.pgs.frankadapt.R")

###Read GWAS summary and paths
gwas_summary <- readxl::read_xlsx(path = "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/GWAS_pgscolumns.xlsx")

###regenerate PGSs using EUR and then AFR reference panel for cross ancestry PGS benchmark project
setwd("~/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing")
base = "/Volumes/Frank/Genetic_processing/GWASs/"
alt = "/Volumes/Frank/Genetic_processing/GSEM/"
##EUR and AFR reference
EUR_ref <- c("/Volumes/Frank/Genetic_processing/1kgCEU_maf0.01_rs_ref")
AFR_ref <- c("/Volumes/Frank/Genetic_processing/1000G_AFR_QC_rsID")

##sumstats and target paths for individual disorders

sumstat <- c(paste0(base,"daner_adhd_meta_filtered_NA_iPSYCH23_PGC11_sigPCs_woSEX_2ell6sd_EUR_Neff_70.meta.gz"),
             paste0(base,"PGC3_SCZ_wave3_public.v2.tsv.gz"),
             paste0(alt,"ANX_gsem_clean.txt.gz"),
             paste0(base,"iPSYCH-PGC_ASD_Nov2017.gz"),
             paste0(base,"pgc-bip2021-all.vcf.tsv"),
             paste0(base,"PGC_UKB_depression_genome-wide.txt"),
             paste0(alt,"PTSD_gsem_clean.txt.gz"),
             paste0(base,"TS_Oct2018.gz"),
             paste0(base,"ocd_aug2017.gz"),
             paste0(base,"pgsAN2.2019-07.reordered.vcf.tsv"),
             paste0(base,"CUD_EUR_casecontrol_public_11.14.2020.gz"),
             paste0(alt,"ALCH_gsem_clean.txt.gz"),
             paste0(alt,"SmokingInitiation.txt"))

target_EUR <- c("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/PNC/EUR_merged_imputed_qced_No_Axiom_EUR",
                "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/ABCD/ABCD_release_3.0_QCed") ##PNC EA; ABCD
target_AFR <- c("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/PNC/AA_merged_imputed_qced",
                "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/ABCD/ABCD_release_3.0_QCed") ##PNC AA; ABCD

##generate in EUR, using EUR reference
pgs(sumstat = sumstat, target = target_EUR, 
    snp = gwas_summary$snp, a1=gwas_summary$a1, a2=gwas_summary$a2,
    stat = gwas_summary$stat, pvalue = gwas_summary$pvalue,
    binary = gwas_summary$binary,betaOR = gwas_summary$betaOR,ref = EUR_ref)

##generate using AFR reference
pgs(sumstat = sumstat, target = target_AFR, 
    snp = gwas_summary$snp, a1=gwas_summary$a1, a2=gwas_summary$a2,
    stat = gwas_summary$stat, pvalue = gwas_summary$pvalue,
    binary = gwas_summary$binary,betaOR = gwas_summary$betaOR,ref = AFR_ref)

 ####need to regenerate GSEM PGS as well for AA and EA in PNC and ABCD, using appropriate reference panel


#####running GSEM PGS
### summay statistics:
sumstat_gsem <- c("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/GSEM_wsub_fivefactor_benchmarkproject_02222022/GSEM_INT_gwas_wsub.txt.gz",
             "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/GSEM_wsub_fivefactor_benchmarkproject_02222022/GSEM_SUB_gwas_wsub.txt.gz",
             "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/GSEM_wsub_fivefactor_benchmarkproject_02222022/GSEM_NDD_gwas_wsub.txt.gz",
             "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/GSEM_wsub_fivefactor_benchmarkproject_02222022/GSEM_COMP_gwas_wsub.txt.gz",
             "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/GSEM_wsub_fivefactor_benchmarkproject_02222022/GSEM_PSY_gwas_wsub.txt.gz")

### compute PGS for EUR
pgs(sumstat = sumstat_gsem, target = target_EUR, ref = EUR_ref)
### compute PGS for AA
pgs(sumstat = sumstat_gsem, target = target_AFR, ref = AFR_ref)


#####running irwin GSEM PGS
### summay statistics:
setwd("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/regenerateirwinGSEM")
sumstat_irwin <- 
           c("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/Irwin_worldpsych_mvGWAS/ext_mvGWAS_finaledited.txt",
             "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/Irwin_worldpsych_mvGWAS/int_mvGWAS_finalEdited.txt",
             "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/Irwin_worldpsych_mvGWAS/ndd_mvGWAS_finalEdited.txt",
             "/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/Irwin_worldpsych_mvGWAS/tp_mvGWAS_finalEdited.txt")

### compute PGS for EUR
pgs(sumstat = sumstat_irwin, target = target_EUR, ref = EUR_ref,
    snp = rep("rsID",length(sumstat_irwin)),
    stat = rep("Beta",length(sumstat_irwin)), pvalue = rep("P",length(sumstat_irwin)), binary = rep(FALSE,length(sumstat_irwin)), 
    betaOR = rep("beta",length(sumstat_irwin)))

### compute PGS for AA
pgs(sumstat = sumstat_irwin, target = target_AFR, ref = AFR_ref,
    snp = rep("rsID",length(sumstat_irwin)),
    stat = rep("Beta",length(sumstat_irwin)), pvalue = rep("P",length(sumstat_irwin)), binary = rep(FALSE,length(sumstat_irwin)), 
    betaOR = rep("beta",length(sumstat_irwin)))



#####Externalizing GSEM PGS
setwd("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing")
sumstat_EXTcons <- 
  c("/Users/quanfahe/Library/CloudStorage/OneDrive-UW-Madison/GradSchool/Research/Genetic_processing/GWASs/Externalizing Consortium/GSEM.GWAS.EXTERNALIZING.SHARE.v20191014.txt.gz")

### compute PGS for EUR
pgs(sumstat = sumstat_EXTcons, target = target_EUR, ref = EUR_ref,
    snp = rep("SNP",length(sumstat_EXTcons)),
    stat = rep("BETA.A1",length(sumstat_EXTcons)), pvalue = rep("PVAL",length(sumstat_EXTcons)), binary = rep(FALSE,length(sumstat_EXTcons)), 
    betaOR = rep("beta",length(sumstat_EXTcons)))

### compute PGS for AA
pgs(sumstat = sumstat_EXTcons, target = target_AFR, ref = AFR_ref,
    snp = rep("SNP",length(sumstat_EXTcons)),
    stat = rep("BETA.A1",length(sumstat_EXTcons)), pvalue = rep("PVAL",length(sumstat_EXTcons)), binary = rep(FALSE,length(sumstat_EXTcons)), 
    betaOR = rep("beta",length(sumstat_EXTcons)))

