

library(haven)#for reading dta file
library(readr)#for reading csv file
library(readxl)
library(openxlsx)
library(dplyr)
library(plyr) # 不等长行合并 rbind.fill
library(tidyverse)
library(reshape2)
library(lubridate)#for deal with time information
library(base)#for dealing with string 
library(car)#for diagnose reg model  
library(ggplot2)
# BiocManager::install("ggtree")
# library(ggtree)
library(ggrepel)
library(RColorBrewer)
library(ggpubr)# for add p value and signif
library(ggsignif) # 添加显著性线段
library(compositions) #clr
# library(rgr)
library(tableone) #for table 1: statistical description
library(data.table)
library(lightgbm)
library(pROC)
library(R.matlab)
library(mlogit)
library(pheatmap)
library(Hmisc)
library(vegan)
library(aplot)

# color pallet
library(colorspace)
library(MetBrewer) 


library(factoextra)
library(cluster)


Manet <- MetBrewer::met.brewer("Manet")
Benedictus <- MetBrewer::met.brewer("Benedictus")
Archambault <- MetBrewer::met.brewer("Archambault")
OKeeffe1 <- MetBrewer::met.brewer("OKeeffe1")
OKeeffe2 <- MetBrewer::met.brewer("OKeeffe2")
Signac <- MetBrewer::met.brewer("Signac")
Morgenstern <- MetBrewer::met.brewer("Morgenstern")
VanGogh3 <- MetBrewer::met.brewer("VanGogh3")





