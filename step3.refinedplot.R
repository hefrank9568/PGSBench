library(ggplot2)
####PNC ####
##load a manually created csv file that contain beta, se, EA/AA grouping and traditional/dimensional grouping
plotfile_pnc_full <- readxl::read_xlsx(path = "/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/isrcap_pnc_effectplot.xlsx", "Raw")

ggplot(data = plotfile_pnc_full, mapping = aes(x=traditional, y = beta, group = group, colour = group)) +
  scale_x_discrete(name ="", 
                   limits=c("DSM Disorder","HiTOP")) +
  geom_point() +
  stat_summary(fun = "mean", geom = "line", size = 3) + 
  theme_classic()

ggplot(data = plotfile_pnc_full, mapping = aes(x=traditional, y = 100*r2, group = group, colour = group)) +
  scale_x_discrete(name ="", 
                   limits=c("DSM Disorder","HiTOP")) +
  ylab("Incremental R2 %") + 
  geom_point() +
  stat_summary(fun = "mean", geom = "line", size = 3) + 
  theme_classic()

####ABCD ####
##load a manually created csv file that contain beta, se, EA/AA grouping and traditional/dimensional grouping
plotfile_abcd_full <- readxl::read_xlsx(path = "/Users/quanfahe/OneDrive - UW-Madison/GradSchool/Research/Cross Race PGS Benchmark/Results/abcd_effectplot_parameters.xlsx", "Raw ABCD")

ggplot(data = plotfile_abcd_full, mapping = aes(x=traditional, y = beta, group = group, colour = group)) +
  scale_x_discrete(name ="", 
                   limits=c("DSM Disorder","HiTOP")) +
  geom_point() +
  stat_summary(fun = "mean", geom = "line", size = 3) + 
  theme_classic()

ggplot(data = plotfile_abcd_full, mapping = aes(x=traditional, y = 100*r2, group = group, colour = group)) +
  scale_x_discrete(name ="", 
                   limits=c("DSM Disorder","HiTOP")) +
  ylab("Incremental R2 %") + 
  geom_point() +
  stat_summary(fun = "mean", geom = "line", size = 3) + 
  theme_classic()


