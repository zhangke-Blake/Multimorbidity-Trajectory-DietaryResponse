




#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part0
#' @:Aging-CGMtraits [Aging-induced disruption in glycemic metabolism]
#' 
#' @:-----------------------------------------------------------------------------------------




#' @:Aging-Traits [pSC analysis]
#' @:Bar
##-----------------------------------------------------------
# data --------------------------------------- 
setwd(paste0(workpath, "/part1_aging"))
database <- openxlsx::read.xlsx("stat_age_CGMtraits_corr.xlsx", sheet = "stat") 
database$group[grep("RG", database$feat2)] <- "RG"
database$group[grep("WG", database$feat2)] <- "WG"
database$group[grep("_mean_day", database$feat2)] <- "Daytime"
database$group[grep("_mean_night", database$feat2)] <- "Nighttime"

database$feat2 <- gsub("_mean_day","", database$feat2)
database$feat2 <- gsub("_mean_night","", database$feat2)
database$feat2[database$feat2=="RG_acc"] <- "IR"
database$feat2[database$feat2=="WG_acc"] <- "IR"
database$feat2[database$feat2=="RG_peak"] <- "MPG"
database$feat2[database$feat2=="WG_peak"] <- "MPG"
database$feat2[database$feat2=="RG_ppge"] <- "iMPG"
database$feat2[database$feat2=="WG_ppge"] <- "iMPG"
database$feat2[database$feat2=="RG_iAUC"] <- "iAUC"
database$feat2[database$feat2=="WG_iAUC"] <- "iAUC"

database <- database[order(database$pSC, decreasing = T),]
database <- database[order(database$group, decreasing = F),]


# plot ----------------------------------------
input_data <- database
input_data$fill <- ifelse(input_data$qval<0.05, input_data$group, "Insignificant")
input_data$feat2 <- factor(input_data$feat2, levels = unique(input_data$feat2))
input_data$group <- factor(input_data$group, levels = c("PGS","Daytime","Nighttime","RG","WG"))

plot <- ggplot(data=input_data, aes(x=pSC, y=feat2))+    
  geom_bar(aes(fill = group), stat="identity", position=position_dodge(), width=0.7, color="black")+  
  geom_text(aes(label=sig_p), vjust=0.5, hjust=-1, size=7, color="black")+
  scale_fill_manual(values = c("#006769","#8FC0A9","#C8D5B9","#FAF3DD","grey80"))+
  
  scale_color_discrete_divergingx(palette = "Geyser")+ 
  scale_x_continuous(expand = c(0,0), limits = c(-0.1,0.16))+
  
  labs(tag="A", fill=NULL, color="Daily traits", title = NULL,
       x="Partial Spearman correlation coefficient", 
       y=NULL)+ 
  facet_grid(group~., scales = "free", space = "free")+
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot



##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple1_age_CGMtraits_corr_bar.pdf", width = 8, height=12); plot; dev.off()




#' @:Aging-PGS [Spearman correlation]
#' @:dot
##------------------------------------------------------
# data --------------------------
setwd(paste0(workpath, "/part1_aging"))
database <- openxlsx::read.xlsx("stat_disease_uniqueness_corr.xlsx", sheet = "data")   
stat <- rcorr(database$age, database$PGS, type = "spearman")
tag <- paste0("Spearman rho = ", round(stat$r[1,2],2), " , P = ", round(stat$P[1,2],3))

a <- rev(quantile(database$age, seq(0,1,1/3)))
database$group <- 3
for(i in 2:(length(a)-1)){
  database$group[database$age<=a[i]] <- (3-(i-1))
}
table(database$group)


# plot -------------------------- 
input_data <- database[,c("age","PGS")] 
input_data <- na.omit(input_data)

plot <- ggplot(data=input_data, aes(x=age, y=log2(PGS)))+     
  geom_point(shape=21, color="white", fill="#8FC0A9", alpha=0.5, size=6)+
  geom_smooth(method = "lm", se = T, linewidth=1, fill="grey70", color="#006769")+ 
  # "#002766","#003A8C","#0050B3","#096DD6","#1890FF","#69C0FF","#BAE7FF"
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  labs(tag="b.", fill=NULL, color="Daily traits", 
       title = paste0("Personalized glycemic sensitivity\n",tag),
       x="Chronological age (years)", 
       y="PGS")+  
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot



##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_age_PGS_corr_dot.pdf", width = 5.5, height=6); plot; dev.off()




#' @:Aging-Uniqueness [Spearman correlation]
#' @:Bar
##-----------------------------------------------------------
# data --------------------------------------- 
setwd(paste0(workpath, "/part1_aging"))
database <- openxlsx::read.xlsx("stat_age_uniqueness_corr.xlsx", sheet = "stat_corr")
database <- database[database$Var2 %in% c("unique_day","unique_night","unique_RG","unique_WG","PGS"),]
database <- database[order(database$r, decreasing = F),] 
database$tag_p <- ifelse(database$pval<0.001, format(database$pval, scientific = T, digits = 2), round(database$pval,2))
database$tag_p[database$pval>0.05] <- NA


# plot ----------------------------------------
input_data <- database
input_data$Var2 <- factor(input_data$Var2, 
                          levels = c("unique_WG","unique_RG","unique_night","unique_day","PGS"), 
                          labels = rev(c("PGS","Daytime (Uniqueness)","Nighttime (Uniqueness)","RG (Uniqueness)", "WG (Uniqueness)")))


plot <- ggplot(data=input_data, aes(x=r, y=Var2))+ 
  geom_vline(xintercept = 0, linetype="dashed", linewidth=0.5, color="lightgrey")+
  geom_bar(aes(fill=Var2), stat="identity", position=position_dodge(), width=0.6, color="grey50")+  
  geom_text(aes(label=tag_p), vjust=0, hjust=0.5, size=7, color="black")+
  
  scale_x_continuous(limits = c(-0.01,0.1), breaks = seq(-0.01,0.1,0.01))+
  scale_color_manual(values = rev(c("#006769","#8FC0A9","#C8D5B9","#FAF3DD","grey80")))+ 
  scale_fill_manual(values = rev(c("#006769","#8FC0A9","#C8D5B9","#FAF3DD","grey80")))+  
  
  labs(tag="c.", fill=NULL, color="Daily traits", 
       title = "Associations of aging and personalized glycemic patterns\n(Uniqueness & PGS)",
       y=NULL, 
       x="Spearman rho")+  
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_age_uniqueness_corr_bar.pdf", height = 4, width=7); plot; dev.off()




#' @:Disease-Uniqueness [Spearman analysis]
#' @:Scatter
##-----------------------------------------------------------
# data --------------------------------------- 
setwd(paste0(workpath, "/part1_aging"))
database <- openxlsx::read.xlsx("stat_disease_uniqueness_corr.xlsx", sheet = "data")   
stat_result <- openxlsx::read.xlsx("stat_disease_uniqueness_corr.xlsx", sheet = "stat_corr")  


# plot (Daytime) ----------------------------------------
input_data <- database[,c("unique_disease","unique_day")] 
input_data <- na.omit(input_data)
names(input_data)[2] <- "uniqueness"  

tag <- paste0("Spearman rho = ", round(stat_result$r[stat_result$Var2=="unique_day"],2), 
              "\nP = ", format(stat_result$pval[stat_result$Var2=="unique_day"],digits = 2, scientific = T))

plot.day <- ggplot(data=input_data, aes(x=unique_disease, y=log2(uniqueness)))+     
  geom_point(shape=21, color="white", fill="#003A8C", alpha=0.5, size=6)+
  geom_smooth(method = "lm", se = T, linewidth=1, fill="grey70", color="#002766")+ 
  # "#002766","#003A8C","#0050B3","#096DD6","#1890FF","#69C0FF","#BAE7FF"
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  labs(tag="a.", fill=NULL, color="Daily traits", 
       title = paste0("Daytime glycemic pattern\n",tag),
       x="Multimorbidity trajectory\n(Uniqueness)", 
       y="Daytime (Uniqueness)")+  
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot.day



# plot (Nighttime) ----------------------------------------
input_data <- database[,c("unique_disease","unique_night")] 
input_data <- na.omit(input_data)
names(input_data)[2] <- "uniqueness"  

tag <- paste0("Spearman rho = ", round(stat_result$r[stat_result$Var2=="unique_night"],2), 
              "\nP = ", format(stat_result$pval[stat_result$Var2=="unique_night"],digits = 2, scientific = T))

plot.night <- ggplot(data=input_data, aes(x=unique_disease, y=log2(uniqueness)))+     
  geom_point(shape=21, color="white", fill="#096DD6", alpha=0.5, size=6)+
  geom_smooth(method = "lm", se = T, linewidth=1, fill="grey70", color="#002766")+ 
  # "#002766","#003A8C","#0050B3","#096DD6","#1890FF","#69C0FF","#BAE7FF"
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  labs(tag="b.", fill=NULL, color="Daily traits", 
       title = paste0("Nighttime glycemic pattern\n",tag),
       x="Multimorbidity trajectory\n(Uniqueness)", 
       y="Nighttime (Uniqueness)")+  
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot.night


# plot (RG) ----------------------------------------
input_data <- database[,c("unique_disease","unique_RG")] 
input_data <- na.omit(input_data)
names(input_data)[2] <- "uniqueness"  

tag <- paste0("Spearman rho = ", round(stat_result$r[stat_result$Var2=="unique_RG"],2), 
              "\nP = ", format(stat_result$pval[stat_result$Var2=="unique_RG"],digits = 2, scientific = T))

plot.RG <- ggplot(data=input_data, aes(x=unique_disease, y=log2(uniqueness)))+     
  geom_point(shape=21, color="white", fill="#1890FF", alpha=0.5, size=6)+
  geom_smooth(method = "lm", se = T, linewidth=1, fill="grey70", color="#002766")+ 
  # "#002766","#003A8C","#0050B3","#096DD6","#1890FF","#69C0FF","#BAE7FF"
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  labs(tag="c.", fill=NULL, color="Daily traits", 
       title = paste0("Postprandial responses to RG\n",tag),
       x="Multimorbidity trajectory\n(Uniqueness)", 
       y="RG (Uniqueness)")+  
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot.RG


# plot (WG) ----------------------------------------
input_data <- database[,c("unique_disease","unique_WG")] 
input_data <- na.omit(input_data)
names(input_data)[2] <- "uniqueness"  

tag <- paste0("Spearman rho = ", round(stat_result$r[stat_result$Var2=="unique_WG"],2), 
              "\nP = ", format(stat_result$pval[stat_result$Var2=="unique_WG"],digits = 2, scientific = T))

plot.WG <- ggplot(data=input_data, aes(x=unique_disease, y=log2(uniqueness)))+     
  geom_point(shape=21, color="white", fill="#69C0FF", alpha=0.5, size=6)+
  geom_smooth(method = "lm", se = T, linewidth=1, fill="grey70", color="#002766")+ 
  # "#002766","#003A8C","#0050B3","#096DD6","#1890FF","#69C0FF","#BAE7FF"
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  labs(tag="d.", fill=NULL, color="Daily traits", 
       title = paste0("Postprandial responses to WG\n",tag),
       x="Multimorbidity trajectory\n(Uniqueness)", 
       y="WG (Uniqueness)")+  
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot.WG


# plot (PGS) ----------------------------------------
input_data <- database[,c("unique_disease","PGS")] 
input_data <- na.omit(input_data)  

stat <- rcorr(input_data$unique_disease, input_data$PGS, type = "spearman")
tag <- paste0("Spearman rho = ", round(stat$r[1,2],2), 
              "\nP = ", format(stat$P[1,2],digits = 2, scientific = T))

plot.PGS <- ggplot(data=input_data, aes(x=unique_disease, y=log2(PGS)))+
  geom_point(shape=21, color="white", fill=OKeeffe2[5], alpha=0.5, size=6)+
  geom_smooth(method = "lm", se = T, linewidth=1, fill="grey70", color="#002766")+

  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  labs(tag="e.", fill=NULL, color="Daily traits", 
       title = paste0("Personalized glycemic sensitivity\n",tag),
       x="Multimorbidity trajectory\n(Uniqueness)", 
       y="PGS")+  
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot.PGS


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_Disease_uniqueness_corr_scatter.pdf", width = 5.5, height=6); plot.day; plot.night; plot.RG; plot.WG; plot.PGS; dev.off()

















#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part1
#' @:LADE-dailyTraits [longitudinal accumulation of disease exposure]
#' 
#' @:-----------------------------------------------------------------------------------------




#' @:Aging-Dailys [Slicing Aging & inter-variability, discovery]
#' @:scatter-line
##-----------------------------------------------------------
# data --------------------------------------- 
#' @:GNHS
setwd(paste0(workpath, "/part1_disease"))
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD","MPT","HPT","LPT")
list <- c(paste0(a,"_mean_day"),paste0(a,"_mean_night"))

daily_corr <- openxlsx::read.xlsx("stat_Daily_CV_age_corr.xlsx", sheet = "CV_GNHS")
daily_corr <- daily_corr[daily_corr$window_size==18,c("age_mean",list)]
daily_corr[,list] <- apply(daily_corr[,list],2,scale)
daily_corr$age_mean <- as.character(daily_corr$age_mean)

daily_corr <- reshape2::melt(daily_corr)
daily_corr$age_mean <- as.numeric(daily_corr$age_mean)
daily_corr$variable <- as.character(daily_corr$variable) 
daily_corr$trait <- sapply(daily_corr$variable, function(x){strsplit(x, split = "_")[[1]][1]}) 
daily_corr$trait[daily_corr$trait=="J"] <- "J-index"
daily_corr$time <- ifelse(grepl("_all", daily_corr$variable),"Whole-day time","Daytime")
daily_corr$time[grepl("_night",daily_corr$variable)] <- "Nighttime"


# plot ----------------------------------------
input_data <- daily_corr
input_data$time <- factor(input_data$time, levels = c("Whole-day time","Daytime","Nighttime"))

plot1 <- ggplot(data=input_data, aes(x=age_mean, y=value))+    
  geom_smooth(aes(color=trait), method = "loess", se = F, linewidth=0.5, fill="grey95")+  
  geom_point(aes(color=trait), alpha=1, size=3)+
  
  scale_color_discrete_divergingx(palette = "Geyser")+ 
  scale_x_continuous(expand = c(0.01,0.01))+
  scale_y_continuous(expand = c(0.01,0.01))+  
  labs(tag="A", fill=NULL, color="Daily traits", title = NULL,
       x="Mean chronological age", 
       y="Inter-individual variations (CV, %)")+ 
  facet_grid(.~time, scales = "free")+
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot1


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_Daily_aging_CV_line.pdf", height = 5, width=8); plot1; dev.off()




#' @:Aging-Dailys [discovery & validation]
#' @:bar-point
##-----------------------------------------------------------
# data --------------------------------------- 
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list <- c(paste0(a,"_mean_all"),paste0(a,"_mean_day"),paste0(a,"_mean_night"),
          paste0(b,"_mean_all"),paste0(b,"_mean_day"),paste0(b,"_mean_night"))

setwd(paste0(workpath, "/part1_disease"))
stat_corr <- openxlsx::read.xlsx("stat_Daily_CV_age_corr.xlsx", sheet = "stat_corr")
stat_corr <- stat_corr[stat_corr$window_size==18 & stat_corr$Var2 %in% list,]
stat_corr$trait <- sapply(stat_corr$Var2, function(x){strsplit(x, split = "_")[[1]][1]}) 
stat_corr$trait[stat_corr$trait=="J"] <- "J-index"
stat_corr$time <- ifelse(grepl("_all", stat_corr$Var2),"Whole-day time","Daytime")
stat_corr$time[grepl("_night",stat_corr$Var2)] <- "Nighttime" 


list <- c(paste0(a,"_allday"),paste0(a,"_day"),paste0(a,"_night"),
          paste0(b,"_allday"),paste0(b,"_day"),paste0(b,"_night"))
valid_corr <- openxlsx::read.xlsx("valid_Daily_CV_age_corr.xlsx", sheet = "stat_corr")
valid_corr <- valid_corr[valid_corr$window_size==18 & valid_corr$Var2 %in% list,]
valid_corr$trait <- sapply(valid_corr$Var2, function(x){strsplit(x, split = "_")[[1]][1]}) 
valid_corr$trait[valid_corr$trait=="J"] <- "J-index"
valid_corr$time <- ifelse(grepl("_all", valid_corr$Var2),"Whole-day time","Daytime")
valid_corr$time[grepl("_night",valid_corr$Var2)] <- "Nighttime" 


#' @:merge
database <- merge(stat_corr[,c("trait","time","r","qval")], valid_corr[,c("trait","time","r","pval")], by=c("trait","time"), all = F)
database$valid <- ifelse(database$r.x*database$r.y>0 & database$qval<0.05 & database$pval<0.05,"Valid","")
list <- paste(database$trait[database$valid=="Valid"], database$time[database$valid=="Valid"], sep = "_")


# plot1 ----------------------
input_data <- data.frame(time=database$time, trait=database$trait, Discovery=database$r.x, Validation=database$r.y) 
input_data <- reshape2::melt(input_data) 
input_data$variable <- as.character(input_data$variable)
input_data$color <- input_data$variable
input_data$color <- ifelse(paste(input_data$trait, input_data$time, sep = "_") %in% list, input_data$color, "Inconsistent")

input_data <- input_data[order(input_data$value, decreasing = T),]
input_data$trait <- factor(input_data$trait, levels = unique(input_data$trait)) 
input_data$time <- factor(input_data$time, levels = c("Whole-day time","Daytime","Nighttime"))
input_data$color <- factor(input_data$color, levels = c("Discovery","Validation","Inconsistent"))
input_data <- input_data[input_data$time!="Whole-day time",]


#' @:plot
plot1 <- ggplot(data=input_data, aes(x=trait, y=value))+  
  geom_hline(yintercept = 0, linetype="dashed", linewidth=0.5, color="grey80")+
  geom_point(aes(fill=color, shape=variable), color="black", alpha=0.7, size=6.5)+ 
  geom_line(aes(group=trait), color="Black", linewidth=0.5)+
  geom_point(color="black", alpha=1, size=1.5)+ 
  
  scale_fill_manual(values = VanGogh3[c(6,2,1)])+
  # scale_fill_discrete_divergingx(palette = "Geyser", rev=T)+ 
  
  scale_shape_manual(values = c(22,24))+
  scale_y_continuous(expand = c(0.1,0.1))+
  labs(tag="a.", title="Associations of aging and inter-individual variability\n(Daily glycemic traits)", 
       fill=NULL, color=NULL, shape=NULL, x=NULL, y="Spearman rho")+
  facet_grid(time~.)+
  
  theme_bw()+ 
  theme( 
    # facet
    strip.text.x = element_text(size = 15, angle = 0),
    strip.text.y = element_text(size = 15, angle = -90),
    strip.background = element_rect(color = "transparent", fill="transparent"),
    panel.spacing = unit(0,"cm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "top",
    legend.title = element_text(face = "plain", size = 15),
    legend.text = element_text(face = "plain", size=15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black",size=20),
    axis.title.y = element_text(face = "plain", color = "black",size=20),  
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=0.5, size=15),   
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15),  
    axis.text = element_text(color="black")
  ) 
plot1


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_Daily_aging_cv_line_comb.pdf", width = 9, height=8); plot1; dev.off()




#' @:Comorbidity-distribution [with aging]
#' @:heatmap 
##------------------------------------------------------
# data --------------------------
setwd(paste(root,"/1_data", sep = ""))
database <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = "disease")  
database <- database[grep("NL", database$id),]

setwd(paste0(workpath, "/part1_disease"))
a <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "data")
idlist <- a$id

database <- database[database$id %in% idlist,]
database <- database[,c("id","time","age","otc_coMorbidity")] 
database$duration <- (database$time-1)*3


#' @:Median-Follow-up-year
a <- database[database$time==1,]
b <- database[database$time!=1,]
b <- b[order(b$time, decreasing=T),]
b <- b[!duplicated(b$id),]
c <- merge(a[,c("id","age")], b[,c("id","age")], by="id", all=F)
c$duration <- c$age.y-c$age.x
mean <- round(median(c$duration[c$duration>0]),1)
sd <- round(sd(c$duration[c$duration>0]),1)
print(paste0("median follow-up year: ", mean, " (", sd,") year"))


data <- data.frame(table(database$otc_coMorbidity, database$duration))  
for(i in 1:ncol(data)){data[,i] <- as.numeric(data[,i])} 
sum <- aggregate(data$Freq, by=list(data$Var2), sum); names(sum) <- c("Var2","sum")
data <- merge(data, sum, by=c("Var2"), all.x=T)
data$perct <- data$Freq*100/data$sum


mean(database$age[database$duration==12])
table(database$otc_coMorbidity[database$duration==12])
summary(database$otc_coMorbidity[database$duration==12])


# plot1: Heatmap ------------------------- 
input_data <- data
input_data$disease <- input_data$Var1-1
input_data$duration <- factor(input_data$Var2, label=seq(0,12,3))


#' @:plot
plot1 <- ggplot(input_data, aes(y=disease, x=duration))+ 
  geom_tile(aes(fill=Freq), width=0.95, height=0.95)+  
  
  scale_fill_continuous_sequential(palette = "YlGnBu", begin=0.1, end=0.9, rev = T)+
  # scale_fill_gradientn(colours = OKeeffe2)+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,12,2))+ 
  labs(tag="C", title=NULL, x="Follow-up years", y="Number of co-occurrence diseases", fill="Num. of\n participants")+  
  
  theme_bw()+
  theme(  
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15, hjust = 0.5, vjust = 1),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(11,"mm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),   
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  )
plot1


##-----------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_comorbidity_distribution_longitudinal_heatmap.pdf", width = 4, height=8); plot1; dev.off()




#' @:Comorbidity-distribution
#' @:barplot [Distance]
##------------------------------------------------------
# data --------------------------
setwd(paste(root,"/1_data", sep = ""))
database <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = "disease")  
database <- database[grep("NL", database$id),]

setwd(paste0(workpath, "/part1_disease"))
a <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "data")
idlist <- a$id
database <- database[database$id %in% idlist,]
list_disease <- colnames(database)[grep("otc_", colnames(database))]
list_disease <- setdiff(list_disease, colnames(database)[grep("otc_coMorb", colnames(database))])


set.seed(1234)     
dist <- as.matrix(vegdist(database[,list_disease], method='eu'))  
mds.stuff <- cmdscale(dist, eig=TRUE, x.ret=TRUE, k=2) # 计算各个成分解释度，并指定提取成分的个数
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.values <- mds.stuff$points

mds.data <- data.frame(time=database$time, X=mds.values[,1], Y=mds.values[,2]) 
mds.data$time <- factor(mds.data$time)
names(mds.data)[-1] <- c("pcoa1","pcoa2")


# plot (Bar) -------------------------------
input_data <- data.frame(dist)
names(input_data) <- database$sampleid
rownames(input_data) <- database$sampleid

dist_y0 <- input_data[grep("^F0", colnames(input_data)), grep("^F0", rownames(input_data))]
dist_y3 <- input_data[grep("^F1", colnames(input_data)), grep("^F1", rownames(input_data))]
dist_y6 <- input_data[grep("^F2", colnames(input_data)), grep("^F2", rownames(input_data))]
dist_y9 <- input_data[grep("^F3", colnames(input_data)), grep("^F3", rownames(input_data))]
dist_y12 <- input_data[grep("^F4", colnames(input_data)), grep("^F4", rownames(input_data))]

dist_y0 <- reshape2::melt(dist_y0); dist_y0 <- dist_y0[dist_y0$value!=0,] 
data_y0 <- data.frame(duration="y0",mean=mean(dist_y0$value),sd=sd(dist_y0$value),n=length(unique(dist_y0$variable)))
dist_y3 <- reshape2::melt(dist_y3); dist_y3 <- dist_y3[dist_y3$value!=0,] 
data_y3 <- data.frame(duration="y3",mean=mean(dist_y3$value),sd=sd(dist_y3$value),n=length(unique(dist_y3$variable)))
dist_y6 <- reshape2::melt(dist_y6); dist_y6 <- dist_y6[dist_y6$value!=0,] 
data_y6 <- data.frame(duration="y6",mean=mean(dist_y6$value),sd=sd(dist_y6$value),n=length(unique(dist_y6$variable)))
dist_y9 <- reshape2::melt(dist_y9); dist_y9 <- dist_y9[dist_y9$value!=0,] 
data_y9 <- data.frame(duration="y9",mean=mean(dist_y9$value),sd=sd(dist_y9$value),n=length(unique(dist_y9$variable)))
dist_y12 <- reshape2::melt(dist_y12); dist_y12 <- dist_y12[dist_y12$value!=0,] 
data_y12 <- data.frame(duration="y12",mean=mean(dist_y12$value),sd=sd(dist_y12$value),n=length(unique(dist_y12$variable)))


#' @:group-comparison
stat <- t.test(dist_y0$value, dist_y3$value, paired = F)
p1 <- ifelse(stat$p.value==0, "P<2.2e-16", round(stat$p.value,3))
stat <- t.test(dist_y3$value, dist_y6$value, paired = F)
p2 <- ifelse(stat$p.value==0, "P<2.2e-16", round(stat$p.value,3))
stat <- t.test(dist_y6$value, dist_y9$value, paired = F)
p3 <- ifelse(stat$p.value==0, "P<2.2e-16", round(stat$p.value,3))
stat <- t.test(dist_y9$value, dist_y12$value, paired = F)
p4 <- ifelse(stat$p.value==0, "P<2.2e-16", round(stat$p.value,3))


data_dist <- rbind(data_y0, data_y3, data_y6, data_y9, data_y12)
data_dist$sem <- data_dist$sd/sqrt(data_dist$n)
data_dist$lci <- data_dist$mean-data_dist$sem
data_dist$uci <- data_dist$mean+data_dist$sem


input_data <- data_dist
input_data$duration <- gsub("y","",input_data$duration)
input_data$duration <- factor(input_data$duration, levels = unique(input_data$duration))


#' @:bar-plot
plot <- ggplot(data=input_data, aes(x=duration, y=mean, fill=duration))+
  geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position=position_dodge(.6), width=.3)+
  scale_color_manual(values = c("grey90","#FAF3DD","#C8D5B9","#8FC0A9","#006769"))+
  scale_fill_manual(values = c("grey90","#FAF3DD","#C8D5B9","#8FC0A9","#006769"))+

  geom_signif(annotations = c(p1,p2,p3,p4),
              y_position = seq(2.5,4,0.5),
              textsize = 6,
              xmin = c(1,2,3,4), xmax = c(2,3,4,5),
              tip_length = c(0.1,0.1,  0.1,0.1,  0.1,0.1,  0.1,0.1))+
  
  scale_y_continuous(expand = c(0,0), limits = c(0,4.5))+
  labs(fill="", x="Follow-up year", y="Inter-individual dissimilarity")+
  
  theme_classic()+ 
  theme( 
    # facet
    strip.text.x = element_text(size = 15, angle = 0),
    strip.text.y = element_text(size = 15, angle = -90),
    strip.background = element_rect(color = "transparent", fill="transparent"),
    # panel.spacing = unit(0,"cm"), 
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "none",
    legend.title = element_text(face = "plain", size = 15),
    legend.text = element_text(face = "plain", size=15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black",size=20),
    axis.title.y = element_text(face = "plain", color = "black",size=20),  
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1,size=15), 
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5,size=15),  
    axis.text = element_text(color="black")
  ) 
plot

##-----------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_comorbidity_dissimilarity.pdf", width = 6, height=8); plot; dev.off()




#' @:LADE-Daily-triats [Longitudinal, all]
#' @:Chord-Diagram
library(circlize)
library(viridis)
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_LADE_daily_chord_all.pdf", width = 14, height=12)
##------------------------------------------------------
# data -----------------------
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list <- c(paste0(a,"_mean_all"),paste0(a,"_mean_day"),paste0(a,"_mean_night"),
          paste0(b,"_mean_all.asin"),paste0(b,"_mean_day.asin"),paste0(b,"_mean_night.asin"))

setwd(paste0(workpath, "/part1_disease"))
stat_disc <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_all")
stat_disc <- stat_disc[stat_disc$time!="Whole-day time",]
stat_disc <- stat_disc[stat_disc$outcome %in% list,]

stat_disc$time <- NA 
stat_disc$time[grep("_day", stat_disc$outcome)] <- "Daytime"
stat_disc$time[grep("_night", stat_disc$outcome)] <- "Nighttime" 

stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]}) 
stat_disc$trait[stat_disc$trait=="J"] <- "J-index"

stat_disc$exposure <- gsub("otc_","",stat_disc$exposure)
stat_disc$exposure <- gsub("_"," & ",stat_disc$exposure)
stat_disc$exposure[stat_disc$exposure=="ObesOverwe"] <- "Obese & Overweight" 


# plot -------------------------
set.seed(1)
df <- stat_disc[stat_disc$qval<0.05 & stat_disc$pval<0.05,]
matrix <- with(df, table(exposure, trait))  


circos.clear()
circos.par(start.degree = 180)

description_colors <- setNames(OKeeffe2, rownames(matrix))
col_names_color <- setNames(rep("lightgrey", length(colnames(matrix))), colnames(matrix))
all_colors <- c(description_colors, col_names_color)

# Generate the chord diagram with specified colors
chordDiagram(matrix, transparency = 0.5,  
             annotationTrack = "grid", 
             annotationTrackHeight = c(0.05, 0.1),
             preAllocateTracks = list(track.height = 0.1), # Reduced track height for genes
             grid.col = all_colors, # Apply colors to both descriptions and genes
             
             directional = 1,    
             direction.type = c("arrows", "diffHeight"),
             
             link.sort = TRUE,         
             link.arr.type = "big.arrow", # Add arrowheads to links
             link.arr.width = 0.05, # Adjust arrowhead width
             
             big.gap = 30, small.gap = 1 
             )  # Adjust the highlight sector height here

# Text labels for the sectors
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(CELL_META$xcenter, ylim[1] + cm_h(2), sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)


##------------------------------------------------------
dev.off()




#' @:LADE-Daily-triats [Longitudinal, noT2D]
#' @:Chord-Diagram
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_LADE_daily_chord_noT2D.pdf", width = 14, height=12)
##------------------------------------------------------
# data -----------------------
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list <- c(paste0(a,"_mean_all"),paste0(a,"_mean_day"),paste0(a,"_mean_night"),
          paste0(b,"_mean_all.asin"),paste0(b,"_mean_day.asin"),paste0(b,"_mean_night.asin"))

setwd(paste0(workpath, "/part1_disease"))
stat_disc <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_noT2D")
stat_disc <- stat_disc[stat_disc$time!="Whole-day time",]
stat_disc <- stat_disc[stat_disc$outcome %in% list,]

stat_disc$time <- NA 
stat_disc$time[grep("_day", stat_disc$outcome)] <- "Daytime"
stat_disc$time[grep("_night", stat_disc$outcome)] <- "Nighttime" 

stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]}) 
stat_disc$trait[stat_disc$trait=="J"] <- "J-index"

stat_disc$exposure <- gsub("otc_","",stat_disc$exposure)
stat_disc$exposure <- gsub("_"," & ",stat_disc$exposure)
stat_disc$exposure[stat_disc$exposure=="ObesOverwe"] <- "Obese & Overweight" 


# plot -------------------------
set.seed(1)
df <- stat_disc[stat_disc$qval<0.05 & stat_disc$pval<0.05,]
matrix <- with(df, table(exposure, trait))  

circos.clear()
circos.par(start.degree = 180)

description_colors <- setNames(c("#002766","#003A8C","#0050B3","#096DD6","#1890FF","#69C0FF","#BAE7FF"), rownames(matrix))
col_names_color <- setNames(rep("lightgrey", length(colnames(matrix))), colnames(matrix))
all_colors <- c(description_colors, col_names_color)

# Generate the chord diagram with specified colors
chordDiagram(matrix, transparency = 0.5, 
             annotationTrack = "grid", 
             annotationTrackHeight = c(0.03),
             preAllocateTracks = list(track.height = 0.05), # Reduced track height for genes
             grid.col = all_colors, # Apply colors to both descriptions and genes
             
             directional = 1,    
             direction.type = c("arrows", "diffHeight"),
             
             link.sort = TRUE,         
             link.arr.type = "big.arrow", # Add arrowheads to links
             link.arr.width = 0.05, # Adjust arrowhead width
             
             big.gap = 30, small.gap = 1)  # Adjust the highlight sector height here

# Text labels for the sectors
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(CELL_META$xcenter, ylim[1] + cm_h(2), sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)


##------------------------------------------------------
dev.off()




#' @:Disease-Network [Cross-sec, all]
#' @:ggraph 
library(ggraph)
library(igraph)
##------------------------------------------------------
# data -----------------------
setwd(paste0(workpath, "/part1_disease")) 
age <- openxlsx::read.xlsx("stat_DiseaeseNetwork_logistic_all.xlsx", sheet = "data_onsetAge") 
age$disease <- gsub("otc_","",age$disease)
age$disease <- gsub("_"," & ",age$disease)
age$disease[age$disease=="ObesOverwe"] <- "Obese & Overweight" 


data <- openxlsx::read.xlsx("stat_DiseaeseNetwork_logistic_all.xlsx", sheet = "stat_all")
data <- merge(data, age, by.x = "exposure", by.y = "disease", all.x=T)
names(data)[grep("meanAge", colnames(data))] <- "MeanAge_exp"
data <- merge(data, age, by.x = "outcome", by.y = "disease", all.x=T)
names(data)[grep("meanAge", colnames(data))] <- "MeanAge_otc"
data$MeanAge_exp <- round(data$MeanAge_exp,1)
data$MeanAge_otc <- round(data$MeanAge_otc,1)

data$exposure <- gsub("otc_","",data$exposure)
data$exposure <- gsub("_"," & ",data$exposure)
data$exposure[data$exposure=="ObesOverwe"] <- "Obese & Overweight" 
# data$exposure <- paste0(data$exposure,"\n (",data$MeanAge_exp,")")

data$outcome <- gsub("otc_","",data$outcome)
data$outcome <- gsub("_"," & ",data$outcome)
data$outcome[data$outcome=="ObesOverwe"] <- "Obese & Overweight" 
# data$outcome <- paste0(data$outcome,"\n (",data$MeanAge_otc,")")


#' @:nodes-file                             
num_nodes1 <- data.frame(table(data$exposure[data$pval<0.05 & data$qval<0.05]))
num_nodes2 <- data.frame(table(data$outcome[data$pval<0.05 & data$qval<0.05]))
num_nodes <- rbind(num_nodes1, num_nodes2)
input <- data[data$pval<0.05 & data$qval<0.05,]

nodes <- data.frame(unique(c(unique(input$exposure), unique(input$outcome))))
names(nodes) <- "node"  
nodes <- merge(nodes, num_nodes, by.x = "node", by.y = "Var1", all.x=T)
nodes <- aggregate(nodes$Freq, by=list(nodes$node), sum)
names(nodes) <- c("node","Freq")
nodes <- merge(nodes, age, by.x = "node", by.y = "disease", all.x=T)
nodes$meanAge <- round(nodes$meanAge,1)
nodes$meanAge <- paste(nodes$meanAge,"y")
nodes <- nodes[order(nodes$meanAge, decreasing = F),]

nodes$seq <- seq(0,nrow(nodes)-1) 
nodes$angle <- 90 - 360 * (nodes$seq+0.5) / nrow(nodes)
nodes$angle <- ifelse(nodes$angle < (-90), nodes$angle+180, nodes$angle)
nodes$hjust <- ifelse(nodes$seq < round(nrow(nodes)/2), 0, 1) 

#' @:edges
edges <- data.frame(from=input$exposure, to=input$outcome, corr=input$OR, qval=input$qval)
edges$class <- ifelse(edges$corr>1, "Positive correlation", "Negative correlation")
edges$class[edges$qval>0.05] <- "Insignificant"

#' @:merge
g <- tidygraph::tbl_graph(nodes=nodes, edges=edges)

#' @:transitivity
graph <- graph_from_data_frame(edges, vertices = data.frame(name = nodes$node), directed = TRUE)
global_transitivity <- mean(transitivity(graph, type = "barrat"))


# plot ----------------------
set.seed(1) # gem fr  
layout <- create_layout(g, layout = "circle", circular=F)   
canvas_size <- 2

plot <- ggraph(layout)+  
  geom_edge_arc(
    aes(edge_width = abs(corr)),
    edge_color = "grey50",
    alpha = 1, strength = 0,  # 控制边的弯曲程度
    arrow = grid::arrow(length = grid::unit(4, "mm"),type = "closed", ends = "last")) +
  geom_node_point(aes(size=Freq, color=node), alpha=0.5, size=20)+
  geom_node_label(aes(label=node, fill=node, hjust=0.5, vjust=1, x=x*1.1, y=y*1.1),color="white", size=7)+
  geom_node_text(aes(label=meanAge, hjust=0.5, vjust=4), angle=0, color="black", size=5)+
  
  # scale_size_continuous(range = c(10,10))+ # point size
  scale_fill_manual(values = OKeeffe2)+ 
  scale_color_manual(values = OKeeffe2)+ 
  scale_edge_width_continuous(range = c(0.5,2))+ # line width
  # scale_edge_color_manual(values = c("#4394C4","#EA8A8A"))+
  
  labs(tag="f", title=paste0("All, n=1398, Transitivity = ",round(global_transitivity,2)), 
       color="Disease", edge_width="OR (FDR-P<0.05)", size="Density")+
  coord_cartesian(xlim = c(-1*canvas_size, canvas_size), ylim = c(-1*canvas_size, canvas_size))+
  
  theme_graph(base_family = 'Helvetica') +
  theme_minimal()+
  theme(
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "right",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    # legend.key.width = unit(3,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank()
  )
plot


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_Disease_network_all.pdf", width = 14, height=10); plot; dev.off()




#' @:Disease-Network [All, mean age of the onset]
#' @:bar-plot
##------------------------------------------------------
# data ----------------------------
setwd(paste0(workpath, "/part1_disease"))
data <- openxlsx::read.xlsx("stat_DiseaeseNetwork_logistic_all.xlsx", sheet = "data_onsetAge")
data <- data[order(data$meanAge, decreasing=F),]
data$meanAge <- data$meanAge-50

data$disease <- gsub("otc_","",data$disease)
data$disease <- gsub("_"," & ",data$disease)
data$disease[data$disease=="ObesOverwe"] <- "Obese & Overweight" 


# plot ----------------------------
input_data <- data
input_data$disease <- factor(input_data$disease, levels = input_data$disease)

plot <- ggplot(data=input_data, aes(x=meanAge, y=disease, fill=disease))+ 
  geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black")+ 
  scale_fill_manual(values = c(OKeeffe2[c(2,6,4,5,3,7,1)])) + 
  
  scale_x_continuous(expand = c(0,0), limits = c(0,20), labels = seq(0,20,5)+50)+
  labs(fill="", x="Mean age at diagnosis", y=NULL)+
  
  theme_classic()+ 
  theme( 
    # facet
    strip.text.x = element_text(size = 15, angle = 0),
    strip.text.y = element_text(size = 15, angle = -90),
    strip.background = element_rect(color = "transparent", fill="transparent"),
    # panel.spacing = unit(0,"cm"), 
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "none",
    legend.title = element_text(face = "plain", size = 15),
    legend.text = element_text(face = "plain", size=15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black",size=20),
    axis.title.y = element_text(face = "plain", color = "black",size=20),  
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1,size=15), 
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5,size=15),  
    axis.text = element_text(color="black")
  ) 
plot

##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_Disease_network_all_meanAge.pdf", width = 6, height=4.5); plot; dev.off()




#' @:Disease-Network [Cross-sectional, noT2D]
#' @:ggraph 
library(ggraph)
##------------------------------------------------------
# data -----------------------
setwd(paste0(workpath, "/part1_disease")) 
age <- openxlsx::read.xlsx("stat_DiseaeseNetwork_logistic_noT2D.xlsx", sheet = "data_onsetAge") 
age$disease <- gsub("otc_","",age$disease)
age$disease <- gsub("_"," & ",age$disease)
age$disease[age$disease=="ObesOverwe"] <- "Obese & Overweight" 


data <- openxlsx::read.xlsx("stat_DiseaeseNetwork_logistic_noT2D.xlsx", sheet = "stat_all")
data <- merge(data, age, by.x = "exposure", by.y = "disease", all.x=T)
names(data)[grep("meanAge", colnames(data))] <- "MeanAge_exp"
data <- merge(data, age, by.x = "outcome", by.y = "disease", all.x=T)
names(data)[grep("meanAge", colnames(data))] <- "MeanAge_otc"
data$MeanAge_exp <- round(data$MeanAge_exp,1)
data$MeanAge_otc <- round(data$MeanAge_otc,1)

data$exposure <- gsub("otc_","",data$exposure)
data$exposure <- gsub("_"," & ",data$exposure)
data$exposure[data$exposure=="ObesOverwe"] <- "Obese & Overweight" 
# data$exposure <- paste0(data$exposure,"\n (",data$MeanAge_exp,")")

data$outcome <- gsub("otc_","",data$outcome)
data$outcome <- gsub("_"," & ",data$outcome)
data$outcome[data$outcome=="ObesOverwe"] <- "Obese & Overweight" 
# data$outcome <- paste0(data$outcome,"\n (",data$MeanAge_otc,")")


#' @:nodes-file                             
num_nodes1 <- data.frame(table(data$exposure[data$pval<0.05 & data$qval<0.05]))
num_nodes2 <- data.frame(table(data$outcome[data$pval<0.05 & data$qval<0.05]))
num_nodes <- rbind(num_nodes1, num_nodes2)
input <- data[data$pval<0.05 & data$qval<0.05,] 

nodes <- data.frame(unique(c(unique(input$exposure), unique(input$outcome))))
names(nodes) <- "node"  
nodes <- rbind(nodes,"KSD")
nodes <- merge(nodes, num_nodes, by.x = "node", by.y = "Var1", all.x=T)
nodes <- aggregate(nodes$Freq, by=list(nodes$node), sum)
names(nodes) <- c("node","Freq")
nodes <- merge(nodes, age, by.x = "node", by.y = "disease", all.x=T)
nodes$meanAge <- round(nodes$meanAge,1)
nodes$meanAge <- paste(nodes$meanAge,"y")
nodes <- nodes[order(nodes$meanAge, decreasing = F),]

nodes$seq <- seq(0,nrow(nodes)-1) 
nodes$angle <- 90 - 360 * (nodes$seq+0.5) / nrow(nodes)
nodes$angle <- ifelse(nodes$angle < (-90), nodes$angle+180, nodes$angle)
nodes$hjust <- ifelse(nodes$seq < round(nrow(nodes)/2), 0, 1) 

#' @:edges
edges <- data.frame(from=input$exposure, to=input$outcome, corr=input$OR, qval=input$qval)
edges$class <- ifelse(edges$corr>1, "Positive correlation", "Negative correlation")
edges$class[edges$qval>0.05] <- "Insignificant"

#' @:merge
g <- tidygraph::tbl_graph(nodes=nodes, edges=edges)

#' @:transitivity
graph <- graph_from_data_frame(edges, vertices = data.frame(name = nodes$node), directed = TRUE)
global_transitivity <- transitivity(graph, type = "barrat")
global_transitivity[is.na(global_transitivity)] <- 0
global_transitivity <- mean(global_transitivity)


# plot ----------------------
set.seed(1) # gem fr  
layout <- create_layout(g, layout = "circle", circular=F)   
canvas_size <- 2

plot <- ggraph(layout)+  
  geom_edge_arc(
    aes(edge_width = abs(corr)),
    edge_color = "grey50",
    alpha = 1, strength = 0,  # 控制边的弯曲程度
    arrow = grid::arrow(length = grid::unit(4, "mm"),type = "closed", ends = "last")) +
  geom_node_point(aes(size=Freq, color=node), alpha=0.5, size=20)+
  geom_node_label(aes(label=node, fill=node, hjust=0.5, vjust=1, x=x*1.1, y=y*1.1),color="white", size=7)+
  geom_node_text(aes(label=meanAge, hjust=0.5, vjust=4), angle=0, color="black", size=5)+
  
  # scale_size_continuous(range = c(10,10))+ # point size
  scale_fill_manual(values = c("#002766","#003A8C","#0050B3","#096DD6","#1890FF","#69C0FF","#BAE7FF"))+ 
  scale_color_manual(values = c("#002766","#003A8C","#0050B3","#096DD6","#1890FF","#69C0FF","#BAE7FF"))+ 
  scale_edge_width_continuous(range = c(0.5,2))+ # line width
  # scale_edge_color_manual(values = c("#4394C4","#EA8A8A"))+
  
  labs(tag="h", title=paste0("Non-diabetic subgroup, n=994, Transitivity = ",round(global_transitivity,2)), 
       color="Disease", edge_width="OR (FDR-P<0.05)", size="Density")+
  coord_cartesian(xlim = c(-1*canvas_size, canvas_size), ylim = c(-1*canvas_size, canvas_size))+
  
  theme_graph(base_family = 'Helvetica') +
  theme_minimal()+
  theme(
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "right",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    # legend.key.width = unit(3,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank()
  )
plot


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_Disease_network_noT2D.pdf", width = 14, height=10); plot; dev.off()




#' @:Disease-Network [noT2D, mean age of the onset]
#' @:bar-plot
##------------------------------------------------------
# data ----------------------------
setwd(paste0(workpath, "/part1_disease"))
data <- openxlsx::read.xlsx("stat_DiseaeseNetwork_logistic_noT2D.xlsx", sheet = "data_onsetAge")
data <- data[order(data$meanAge, decreasing=F),]
data$meanAge <- data$meanAge-50

data$disease <- gsub("otc_","",data$disease)
data$disease <- gsub("_"," & ",data$disease)
data$disease[data$disease=="ObesOverwe"] <- "Obese & Overweight" 


# plot ----------------------------
input_data <- data
input_data$disease <- factor(input_data$disease, levels = input_data$disease)

plot <- ggplot(data=input_data, aes(x=meanAge, y=disease, fill=disease))+ 
  geom_bar(stat="identity", position=position_dodge(), width=0.7, color="black")+ 
  scale_fill_manual(values = c("#002766","#003A8C","#0050B3","#096DD6","#1890FF","#69C0FF","#BAE7FF")) + 
  
  scale_x_continuous(expand = c(0,0), limits = c(0,20), labels = seq(0,20,5)+50)+
  labs(fill="", x="Mean age of disease diagnosis", y=NULL)+
  
  theme_classic()+ 
  theme( 
    # facet
    strip.text.x = element_text(size = 15, angle = 0),
    strip.text.y = element_text(size = 15, angle = -90),
    strip.background = element_rect(color = "transparent", fill="transparent"),
    # panel.spacing = unit(0,"cm"), 
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "none",
    legend.title = element_text(face = "plain", size = 15),
    legend.text = element_text(face = "plain", size=15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black",size=20),
    axis.title.y = element_text(face = "plain", color = "black",size=20),  
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1,size=15), 
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5,size=15),  
    axis.text = element_text(color="black")
  ) 
plot

##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_Disease_network_noT2D_meanAge.pdf", width = 6, height=4.5); plot; dev.off()




#' @:Comorbidity-trajectory [all sample, Dyslipidemia case vs. non-Case]
#' @:trajectory-lineplot 
##------------------------------------------------------
# data ---------------------------
setwd(paste0(workpath, "/part1_disease"))
stat_mixed <- openxlsx::read.xlsx("stat_Dyslipid_subgroup_comparison.xlsx", sheet = "stat_mixed")
label <- paste("P for group*time = ", round(stat_mixed$pval[nrow(stat_mixed)],2))

stat_wilcox <- openxlsx::read.xlsx("stat_Dyslipid_subgroup_comparison.xlsx", sheet = "stat_wilcox")
stat_wilcox[,c(4:7)] <- apply(stat_wilcox[,c(4:7)],2,as.numeric)
stat_wilcox$label <- ifelse(stat_wilcox$pval<0.001, format(stat_wilcox$pval, scientific = T, digits = 2), round(stat_wilcox$pval, 2))

a <- stat_wilcox[,c("time","pval","case_mean","case_sd")]; names(a) <- c("time","pval","mean","sd")
b <- stat_wilcox[,c("time","pval","non_mean","non_sd")]; names(b) <- c("time","pval","mean","sd")
input <- rbind(data.frame(group="case", a), data.frame(group="non", b)) 

input$upper <- input$mean+input$sd
input$lower <- input$mean-input$sd
input$lower[input$lower<0] <- 0

input$year <- (input$time-1)*3
input$group <- ifelse(input$group=="case","Case","Non-case")


# plot ---------------------------
input_data <- input

#' @:bar-plot
plot <- ggplot(data=input_data, aes(x=year, y=mean))+  
  annotate("text", label=label, x=2, y=3.7, size=5, color="black")+
  
  geom_line(aes(group=group, color=group), position=position_dodge(.6))+
  geom_point(aes(y=mean, color=group), size=5, position=position_dodge(.6))+
  geom_errorbar(aes(ymin=lower, ymax=upper, color=group), position=position_dodge(.6), width=.3)+   
  geom_text(data=stat_wilcox, aes(label=label, x=(time-1)*3, y=(case_mean+case_sd+0.2)), size=5, color="black")+
  
  scale_color_manual(values = c("#006769","#8FC0A9"))+ 
  scale_x_continuous(breaks = seq(0,12,3))+
  scale_y_continuous(expand = c(0,0), limits = c(0,4))+
  
  # scale_y_continuous(expand = c(0,0), limits = c(0,6))+
  labs(tag="i", color="Dyslipidemia diagnosed at the baseline", fill=NULL, x="Follow-up year", y="Comorbid condition\nof glucose-related diseases")+
  
  theme_classic()+ 
  theme( 
    # facet
    strip.text.x = element_text(size = 15, angle = 0),
    strip.text.y = element_text(size = 15, angle = -90),
    strip.background = element_rect(color = "transparent", fill="transparent"),
    # panel.spacing = unit(0,"cm"), 
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "top",
    legend.title = element_text(face = "plain", size = 15, hjust=0.5),
    legend.text = element_text(face = "plain", size=15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black",size=20),
    axis.title.y = element_text(face = "plain", color = "black",size=20),  
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1,size=15), 
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5,size=15),  
    axis.text = element_text(color="black")
  ) 
plot


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_DiseaseTrajectory_dyslipidemia_comp.pdf", height = 5, width=6); plot; dev.off()




#' @:Comorbidity-trajectory [External validation, Dyslipidemia case vs. non-Case]
#' @:cross-sectional
##------------------------------------------------------
# data ---------------------------
setwd(paste0(root,"/1_data"))
data_CGM <- openxlsx::read.xlsx("CHNS_CGMdata_clean.xlsx", sheet = "daily_mean")  
list_id <- unique(data_CGM$tag)


data_disease <- openxlsx::read.xlsx("CHNS_disease_all.xlsx", sheet = "disease")
data_disease <- data_disease[data_disease$tag %in% list_id,]

list_disease <- c("otc_T2D","otc_Dyslipidemia","otc_ObesOverwe","otc_Hypertension")
data_disease$otc_coMorbidity.glucose <- apply(data_disease[,list_disease],1,sum, na.rm=T) 
data_disease$otc_coMorbidity.glucose2 <- apply(data_disease[,setdiff(list_disease,"otc_Dyslipidemia")],1,sum, na.rm=T) 
table(data_disease$otc_coMorbidity.glucose)

data_disease$otc_Dyslipidemia[is.na(data_disease$otc_Dyslipidemia)] <- 0
data_disease$group <- ifelse(data_disease$otc_Dyslipidemia==0,"non","case")
table(data_disease$group, data_disease$otc_coMorbidity.glucose2)

stat <- wilcox.test(x = data_disease$otc_coMorbidity.glucose2, group=data_disease$group)
label <- paste0("P = ", format(stat$p.value, scientific = T, digits = 2))


# plot ---------------------------
input_data <- data_disease

#' @:bar-plot
plot <- ggplot(input_data, aes(x = otc_coMorbidity.glucose2, fill=group, color=group)) + 
  annotate("text", x=2.2, y=0.5, label=label, size=5, color="black")+
  geom_density(alpha = 0.3, adjust=2) + 
  
  scale_color_manual(values = c("#006769","#8FC0A9"))+   
  scale_fill_manual(values = c("#006769","#8FC0A9"))+  
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  labs(tag="j", color="Dyslipidemia diagnosis", fill="Dyslipidemia diagnosis", y="Density", x="Comorbid condition\nof glucose-related diseases")+
  
  theme_classic()+ 
  theme( 
    # facet
    strip.text.x = element_text(size = 15, angle = 0),
    strip.text.y = element_text(size = 15, angle = -90),
    strip.background = element_rect(color = "transparent", fill="transparent"),
    # panel.spacing = unit(0,"cm"), 
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "top",
    legend.title = element_text(face = "plain", size = 15, hjust=0.5),
    legend.text = element_text(face = "plain", size=15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black",size=20),
    axis.title.y = element_text(face = "plain", color = "black",size=20),  
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1,size=15), 
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5,size=15),  
    axis.text = element_text(color="black")
  ) 
plot


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig1_DiseaseTrajectory_dyslipidemia_comp_validate.pdf", width = 5, height=5); plot; dev.off()
















#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part2
#' @:Multimorbidity-DailyTraits [8 glucose-related diseases]
#' 
#' @:-----------------------------------------------------------------------------------------




#' @:Comorbidity-CGM [dose-responses, all samples & non-diabetic]
#' @:error-bar
##------------------------------------------------------
# data --------------------
setwd(paste0(workpath, "/part2_multimorbid"))
database <- openxlsx::read.xlsx("stat_multimorbidity_DailyTraits_linear_quartile.xlsx", sheet = "stat")
database$group[database$group==1] <- "Single"
database$group[database$group==2] <- "Mild-comorbid (2-3)"
database$group[database$group==3] <- "Severe-comorbid (>=4)"
database <- database[database$time!="Whole-day time",]

data.all <- database[database$group2=="all",]
data.noT2D <- database[database$group2=="noT2D",]


# plot (all) -------------------- 
input_data <- data.all 
input_data$fill <- input_data$group
input_data$fill[input_data$pval>0.05] <- "Insignificant"
table(input_data$fill)

input_data$group <- factor(input_data$group, levels = rev(c("Single","Mild-comorbid (2-3)","Severe-comorbid (>=4)")))
input_data$fill <- factor(input_data$fill, levels = rev(c("Mild-comorbid (2-3)","Severe-comorbid (>=4)","Insignificant"))) 
input_data$trait <- factor(input_data$trait, levels = rev(c("J-index","MAGE","MODD","CV","eA1C","HBGI","LBGI","LPT","MPT","HPT")))

plot <- ggplot(input_data, aes(x=beta, y=trait, color=group)) + 
  annotate("rect", ymin=seq(0.5,8.5,2), ymax=seq(1.5,10,2), xmin=rep(-0.9,5), xmax=rep(1.2,5), color="white", fill="grey90")+
  
  geom_vline(xintercept = 0, linewidth=0.5, linetype="dashed", color="black")+ 
  geom_errorbar(aes(xmin = beta-1.96*se, xmax = beta+1.96*se, color=group), width = 0.3, position = position_dodge(0.5)) + # 误差线图层
  geom_point(aes(color=group, fill=fill), shape=21, alpha=1, size=4, position = position_dodge(0.5)) + # 点图层 
  
  scale_color_manual(values = rev(c(Benedictus[c(10,12,13)])))+ 
  scale_fill_manual(values = rev(c(Benedictus[c(12,13)],"white")))+ 
  scale_x_continuous(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0.5))+ 
  
  ggtitle(label = "Associations of MMI-system and daily glycemic traits")+ 
  labs(tag="A", color=NULL, fill=NULL, y=NULL, x="Standardized beta coefficient")+
  facet_wrap(.~time, strip.position = "top")+
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(size=15, angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(size=10, angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black")
  )
plot



# plot (noT2D) -------------------- 
input_data <- data.noT2D
input_data$fill <- input_data$group
input_data$fill[input_data$pval>0.05] <- "Insignificant"
table(input_data$fill)

input_data$group <- factor(input_data$group, levels = rev(c("Single","Mild-comorbid (2-3)","Severe-comorbid (>=4)")))
input_data$fill <- factor(input_data$fill, levels = rev(c("Mild-comorbid (2-3)","Severe-comorbid (>=4)","Insignificant"))) 
input_data$trait <- factor(input_data$trait, levels = rev(c("J-index","MAGE","MODD","CV","eA1C","HBGI","LBGI","LPT","MPT","HPT")))

plot2 <- ggplot(input_data, aes(x=beta, y=trait, color=group)) + 
  annotate("rect", ymin=seq(0.5,8.5,2), ymax=seq(1.5,10,2), xmin=rep(-0.7,5), xmax=rep(0.7,5), color="white", fill="grey90")+
  
  geom_vline(xintercept = 0, linewidth=0.5, linetype="dashed", color="black")+ 
  geom_errorbar(aes(xmin = beta-1.96*se, xmax = beta+1.96*se, color=group), width = 0.3, position = position_dodge(0.5)) + # 误差线图层
  geom_point(aes(color=group, fill=fill), shape=21, alpha=1, size=4, position = position_dodge(0.5)) + # 点图层 
  
  scale_color_manual(values = rev(c(Benedictus[c(5,3,1)])))+ 
  scale_fill_manual(values = rev(c(Benedictus[c(5,3,1)],"white")))+ 
  scale_x_continuous(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0.5))+ 
  
  ggtitle(label = "Associations of MMI-system and daily glycemic traits (Non-diabetic)")+ 
  labs(tag="A", color=NULL, fill=NULL, y=NULL, x="Standardized beta coefficient")+
  facet_wrap(.~time, strip.position = "top")+
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(size=15, angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(size=10, angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black")
  )
plot2


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig2_comorbidity_daily_class_forest.pdf", width = 7, height=11); plot; plot2; dev.off()




#' @:Comorbidity-Daily-Traits [MMI-system, conti, all] 
#' @:scatter-plot [Validation in CHNS]
##------------------------------------------------------
# data -------------------------- 
setwd(paste0(workpath,"/part2_multimorbid"))
valid_time <- openxlsx::read.xlsx("valid_multimorbidity_DailyTraits_linear.xlsx", sheet = "sort") 


# plot ---------------------- 
input_data <- valid_time
input_data$group <- factor(input_data$group, levels = seq(1,3,1), labels = c("Single","Mild-comorbid","Severe-comorbid"))
input_data$check <- ifelse(input_data$beta_disc*input_data$beta_valid>0
                           & input_data$qval_disc<0.05 & input_data$pval_valid<0.05, input_data$group, "Invalid")
input_data$label <- ifelse(input_data$check=="Invalid",NA, input_data$trait)
input_data$check <- factor(input_data$check, levels = c(3,2,"Invalid"), labels = c("Severe-comorbid","Mild-comorbid","Invalid"))
rcorr(input_data$beta_disc, input_data$beta_valid)


#' @:plot
plot <- ggplot(input_data, aes(x = -log10(qval_disc)*beta_disc, y=-log10(pval_valid)*beta_valid)) + 
  
  geom_hline(yintercept = 0, linewidth=0.5, linetype="dashed", color="grey80")+
  geom_vline(xintercept = 0, linewidth=0.5, linetype="dashed", color="grey80")+
  
  geom_label_repel(aes(label=label), size=4.5, fill = "grey98", alpha=1, xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))+ 
  geom_point(aes(color=check, shape=time), size=5, alpha=1) + 
  
  scale_color_manual(values = c(Benedictus[c(13,12)], "grey80"))+
  ggtitle(label = "External validation (All)")+
  labs(tag="D", shape="Time", color="Condition", size="-log10 (Adjusted-P)", fill=NULL, 
       y="Significance-weighted effect of MMI-system\n(Validation, CHNS)", 
       x="Significance-weighted effect of MMI-system\n(Discovery, GNHS)")+ 
  
  theme_bw()+
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = c(0.8,0.2),
    legend.title = element_text(face = "plain", size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.key.height = unit(8,"mm"),
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(angle=0, face = "plain", hjust=0.5, vjust=1, color = "black"), 
    axis.text = element_text(size=15)
  )
plot


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs")) 
pdf("fig2_comorbidity_daily_class_scatter_all.pdf", width = 8.5, height=8); plot; dev.off()




#' @:Comorbidity-Daily-Traits [MMI-system, conti, noT2D] 
#' @:scatter-plot [Validation in CHNS]
##------------------------------------------------------
# data -------------------------- 
setwd(paste0(workpath,"/part2_multimorbid"))
valid_time <- openxlsx::read.xlsx("valid_multimorbidity_DailyTraits_linear_noT2D.xlsx", sheet = "sort") 


# plot ---------------------- 
input_data <- valid_time
input_data$group <- factor(input_data$group, levels = seq(1,3,1), labels = c("Single","Mild-comorbid","Severe-comorbid"))
input_data$check <- ifelse(input_data$beta_disc*input_data$beta_valid>0
                           & input_data$qval_disc<0.05 & input_data$pval_valid<0.05, input_data$group, "Invalid")
input_data$label <- ifelse(input_data$check=="Invalid",NA, input_data$trait)
input_data$check <- factor(input_data$check, levels = c(3,2,"Invalid"), labels = c("Severe-comorbid","Mild-comorbid","Invalid"))
rcorr(input_data$beta_disc, input_data$beta_valid)


#' @:plot
plot <- ggplot(input_data, aes(x = -log10(qval_disc)*beta_disc, y=-log10(pval_valid)*beta_valid)) + 
  
  geom_hline(yintercept = 0, linewidth=0.5, linetype="dashed", color="grey80")+
  geom_vline(xintercept = 0, linewidth=0.5, linetype="dashed", color="grey80")+
  
  geom_label_repel(aes(label=label), size=4.5, fill = "grey98", alpha=1, xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))+ 
  geom_point(aes(color=check, shape=time), size=5, alpha=1) + 
  
  scale_color_manual(values = c(Benedictus[c(1,3)], "grey80"))+
  ggtitle(label = "External validation (Non-diabetic subgroup)")+
  labs(tag="D", shape="Time", color="Condition", size="-log10 (Adjusted-P)", fill=NULL, 
       y="Significance-weighted effect of MMI-system\n(Validation, CHNS)", 
       x="Significance-weighted effect of MMI-system\n(Discovery, GNHS)")+ 
  
  theme_bw()+
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = c(0.8,0.2),
    legend.title = element_text(face = "plain", size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.key.height = unit(8,"mm"),
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(angle=0, face = "plain", hjust=0.5, vjust=1, color = "black"), 
    axis.text = element_text(size=15)
  )
plot


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs")) 
pdf("fig2_comorbidity_daily_class_scatter_noT2D.pdf", width = 8.5, height=8); plot; dev.off()










#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part3
#' @:DRs-PGS [Postprandial dietary responses]
#' 
#' @:-----------------------------------------------------------------------------------------




#' @:Multimorbidity-Meals [RG & WG]
#' @:boxplot 
setwd(paste0(workpath, "/part3_PGS"))
##-----------------------------------------------------------
# data --------------------------------------- 
database <- openxlsx::read.xlsx("stat_DRs_multimorbidity_wilcox.xlsx", sheet = "data") 
# database[,c("iAUC","peak","ppge","acc")] <- apply(database[,c("iAUC","peak","ppge","acc")],2,scale)


#' @:sorting
mean <- data.frame(t(aggregate(database[,c("iAUC","peak","ppge","acc")], by=list(paste(database$meal, database$group,sep = "_")), mean, na.rm=T)))
names(mean) <- mean[1,]; mean <- mean[-1,]
mean <- data.frame(trait=rownames(mean),mean)
mean[c(2:ncol(mean))] <- apply(mean[,c(2:ncol(mean))],2,as.numeric)
mean <- reshape2::melt(mean); names(mean)[3] <- "mean" 

sd <- data.frame(t(aggregate(database[,c("iAUC","peak","ppge","acc")], by=list(paste(database$meal, database$group,sep = "_")), sd, na.rm=T)))
names(sd) <- sd[1,]; sd <- sd[-1,]
sd <- data.frame(trait=rownames(sd),sd)
sd[c(2:ncol(sd))] <- apply(sd[,c(2:ncol(sd))],2,as.numeric) 
sd <- reshape2::melt(sd); names(sd)[3] <- "sd"

data.mean <- merge(mean, sd, by=c("trait","variable"), all=F)
data.mean$meal <- substr(data.mean$variable,1,2)
data.mean$group <- substr(data.mean$variable,4,4)
data.mean$variable <- data.mean$trait


# plot ----------------------------------------
input_data <- reshape2::melt(database[,c(1:2,4:7)])  
input_data <- merge(input_data, database[,c("id","group")], by="id", all.x=T)
input_data$variable <- factor(input_data$variable, levels = c("iAUC","peak","ppge","acc"), labels = c("iAUC","MPG","iMPG","IR"))
input_data$group <- factor(input_data$group, levels = seq(0,3,1), labels = c("Healthy (n=0)","Single disease (n=1)","Mild multimorbid (2-3)","Severe multimorbid (≥4)"))

data.mean$variable <- factor(data.mean$variable, levels = c("iAUC","peak","ppge","acc"), labels = c("iAUC","MPG","iMPG","IR"))
data.mean$group <- factor(data.mean$group, levels = seq(0,3,1), labels = c("Healthy (n=0)","Single disease (n=1)","Mild multimorbid (2-3)","Severe multimorbid (≥4)"))


plot1 <- ggplot(input_data, aes(x=group, y=value))+   
  geom_violin(aes(fill=group), alpha=1, trim = F) +
  # geom_point(data=input_data, aes(x=group, y=value, fill=group), shape=21, color="transparent", size=2, alpha=0.01, position=position_jitter(width = 0.3))+
  # geom_errorbar(data= data.mean, aes(x=group, ymin = mean-sd, ymax = mean+sd, color=group), width = 0.2, position = position_dodge(0.3)) + # 误差线图层
  geom_boxplot(color="black", width=0.1, fill="white", position = position_dodge(0.4), alpha=0.9, outlier.alpha = 0)+
  # geom_point(data=data.mean, aes(x=group, y=mean, color=group), size=5, alpha=1)+
  # geom_point(aes(fill=group), shape=21, color="transparent", alpha=0.5, size=2, position=position_jitterdodge(jitter.width=0.6, dodge.width=0.6))+
  
  stat_compare_means( 
    comparisons = list(c(2,3), c(3,4), c(1,2), c(1,3), c(1,4)),  
    method = "wilcox.test", paired = FALSE,
    label="p.format", #p.format p.signif 表示*
    # ref.group = "LPS",
    size = 4,
    hide.ns = T
  )+
  
  scale_x_discrete(expand = c(0,0.5))+  
  scale_y_continuous(expand = c(0.1,0))+ 
  scale_color_manual(values = c(OKeeffe1[c(9)], OKeeffe2[c(1,3,5)]))+
  scale_fill_manual(values = c(OKeeffe1[c(8)], OKeeffe2[c(1,2,4)]))+
  labs(fill=NULL, x=NULL, y="Postprandial dietary responses", title=NULL)+ 
  facet_grid(meal~variable, scales = "free")+ 
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    # axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=15, color="black"),
    axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot1


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig3_comorbidity_DRs_wilcox.pdf", width = 12, height=7); plot1; dev.off()




#' @:Comorbidity-DRs [All vs. non-diabetic]
#' @:bubble-plot
##------------------------------------------------------
# data -----------------------
setwd(paste0(workpath, "/part3_PGS"))
stat_all <- openxlsx::read.xlsx("stat_DRs_multimorbidity_linear.xlsx", sheet = "stat_all")
stat_noT2D <- openxlsx::read.xlsx("stat_DRs_multimorbidity_linear.xlsx", sheet = "stat_noT2D")
database <- rbind(data.frame(group="all", stat_all),data.frame(group="noT2D", stat_noT2D))


# plot ----------------------- 
input_data <- database
input_data$group <- factor(input_data$group, levels = c("all","noT2D"), labels = c("All","Non-diabetic"))
input_data$meal <- factor(input_data$meal, levels = c("RG","WG"))
input_data$outcome <- factor(input_data$outcome, levels = rev(c("iAUC","peak","ppge","acc")), labels = rev(c("iAUC","MPG","AGE","IR")))


plot <- ggplot(data=input_data, aes(x=meal, y=outcome))+   
  geom_point(aes(fill=beta, size=-log10(qval)), shape=21, color="transparent", alpha=1)+
  geom_text(aes(x=meal, y=outcome, label=sig_q), size=5, color="black", vjust=-1)+
  
  # scale_x_discrete(expand = c(0,0.2))+
  scale_y_discrete(expand = c(0,0.3))+
  scale_size_continuous(range = c(4,7))+
  scale_fill_gradientn(colours = c(OKeeffe1[rev(11:7)]))+
  labs(tag="a.", title = "Associations of Multimorbidity and\npostprandial glycemic responses",
       fill="Beta\ncoefficient", x=NULL, y=NULL, size="-log10(FDR)")+  
  facet_grid(.~group)+
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = "lightgrey"), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NULL),
    plot.background = element_rect(fill = "transparent",color = NULL),
    
    # legend
    legend.position = "right",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.height = unit(10,"mm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=15, color="black"), 
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  )
plot


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig3_comorbidity_DRs_bubble.pdf", width = 5, height=6); plot; dev.off()




#' @:Diseases-DRs [All vs. non-diabetic]
#' @:heatmap
##------------------------------------------------------
# data -----------------------
setwd(paste0(workpath, "/part3_PGS"))
stat_all <- openxlsx::read.xlsx("stat_DRs_LADE_linear.xlsx", sheet = "stat_all")
stat_noT2D <- openxlsx::read.xlsx("stat_DRs_LADE_linear.xlsx", sheet = "stat_noT2D")

database <- rbind(data.frame(group="all", stat_all),data.frame(group="noT2D", stat_noT2D))
database <- database[database$exposure!="otc_T2D",]
database$exposure <- gsub("otc_","", database$exposure)
database$exposure[database$exposure=="ObesOverwe"] <- "Obese & overweight"


# plot ----------------------- 
input_data <- database
input_data$group <- factor(input_data$group, levels = c("all","noT2D"), labels = c("All","Non-diabetic"))
input_data$meal <- factor(input_data$meal, levels = c("RG","WG"))
input_data$outcome <- factor(input_data$outcome, levels = rev(c("iAUC","peak","ppge","acc")), labels = rev(c("iAUC","MPG","AGE","IR")))


plot <- ggplot(data=input_data, aes(x=meal, y=outcome))+    
  geom_tile(aes(fill=beta), width=0.9, height=0.9)+
  geom_text(aes(label=sig_q), size=6, color="black", vjust=0.5)+
   
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  scale_size_continuous(range = c(4,7))+
  scale_fill_gradientn(colours = c(OKeeffe1[c(10:1)]))+
  labs(tag="b.", title = NULL, 
       fill="Beta\ncoefficient", x=NULL, y=NULL, size="-log10(FDR)")+  
  facet_grid(group~exposure, space = "free", scales = "free")+
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = "transparent"), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NULL),
    plot.background = element_rect(fill = "transparent",color = NULL),
    
    # legend
    legend.position = "right",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.height = unit(10,"mm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=15, color="black"), 
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  )
plot


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig3_disease_DRs_heatmap.pdf", width = 12, height=7); plot; dev.off()




#' @:PGS-diseases [WPN1 vs GNHS]
#' @:Group-comparison [Box plot] 
##------------------------------------------------------
# data ------------------------------ 
setwd(paste0(root, "/1_data"))
data_PGS <- openxlsx::read.xlsx("PGS_construction_GNHS.xlsx", sheet = 1)    
data_PGS.WPN1 <- openxlsx::read.xlsx("PGS_construction_WPN1.xlsx", sheet = 1)    

setwd(paste0(workpath, "/part3_PGS"))
data_result <- openxlsx::read.xlsx("stat_PGS_Disease_wilcox.xlsx", sheet = "data")
stat_result <- openxlsx::read.xlsx("stat_PGS_Disease_wilcox.xlsx", sheet = "stat_all")
stat_result$direction <- ifelse(stat_result$g1_mean > stat_result$g2_mean, "increase","decrease")
table(stat_result$direction, stat_result$sig_q)

stat_result.noT2D <- openxlsx::read.xlsx("stat_PGS_Disease_wilcox.xlsx", sheet = "stat_noT2D")
stat_result.noT2D$direction <- ifelse(stat_result.noT2D$g1_mean > stat_result.noT2D$g2_mean, "increase","decrease")
table(stat_result.noT2D$direction, stat_result.noT2D$sig_q)


#' @:sort
sort_sig <- data.frame(table(stat_result$disease, stat_result$sig_q)) 
list_disease <- unique(stat_result$disease[!is.na(stat_result$sig_q) & stat_result$pair=="case_non"])
list_disease <- setdiff(c(list_disease,"otc_CKD","otc_Gout"),"otc_coMorbidity.glucose")


# plot (Single disease) ------------------------------

plot_box <- function(data, target){
  title <- gsub("otc_","",target)
  title <- gsub("_"," & ",title)
  
  if(title=="coMorbidity"){title <- "Systemic health"} 
  if(title=="ObesOverwe"){title <- "Obese & Overweight"}
  
  submatr <- data[,c(target,"PGS")]
  submatr$group <- ifelse(submatr[,target]>0,"case","none")
  a <- data.frame(PGS=data_PGS.WPN1$PGS, group="ref")
  submatr <- rbind.fill(a, submatr)
  submatr$group <- factor(submatr$group, levels = c("ref","none","case"), labels = c("Young-healthy","Elderly-none","Elderly-case"))
  
  
  plot1 <- ggplot(data=submatr, aes(x=group, y=PGS, fill=group))+  
    geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
    geom_violin()+
    # stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
    geom_boxplot(width=0.1, position = position_dodge(0.4), fill="white", color="black", alpha=1, outlier.alpha = 0)+
    # geom_point(aes(fill=group), shape=21, color="transparent", alpha=0.5, size=2, position=position_jitterdodge(jitter.width=0.4, dodge.width=0.4))+
    
    stat_compare_means( 
      comparisons = list(c(1,2), c(1,3), c(2,3)), #
      method = "wilcox.test", paired = FALSE,
      label="p.format", #p.format p.signif 表示*
      label.y = c(0.25,0.3,0.35),
      # ref.group = "LPS",
      size = 5,
      hide.ns = T
    )+
    
    ggtitle(label = title)+ 
    scale_y_continuous(expand = c(0,0), limits = c(0,0.45))+
    scale_fill_manual(values = c(OKeeffe1[c(9,7,4)]))+
    labs(fill=NULL, x=NULL, y=NULL)+  
    
    theme_classic()+ 
    theme( 
      # for facet
      strip.text.y = element_text(size = 15, angle = 0), 
      strip.background = element_rect(color = "transparent", fill= "transparent"),
      panel.spacing = unit(0,"mm"),
      
      # for ggtitle
      plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
      
      panel.grid.major = element_line(color = NA), 
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent",color = NA),
      plot.background = element_rect(fill = "transparent",color = NA),
      
      # legend
      legend.position = "none",
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.background = element_rect(fill = "transparent", color="transparent"),
      legend.key.width = unit(2,"cm"),
      legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
      legend.box.background = element_rect(fill = "transparent", color = "transparent"),
      
      # for axis
      axis.ticks.length = unit(3,"mm"),
      axis.title.x = element_text(color = "black",size=20),
      axis.title.y = element_text(color = "black",size=20),
      # axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=20, color="black"),
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
    ) 
  return(plot1)
}

plot1 <- plot_box(data = data_result, target = "otc_T2D"); plot1
plot2 <- plot_box(data = data_result, target = "otc_ObesOverwe"); plot2
plot3 <- plot_box(data = data_result, target = "otc_MASLD"); plot3
plot4 <- plot_box(data = data_result, target = "otc_Dyslipidemia"); plot4
plot5 <- plot_box(data = data_result, target = "otc_Hypertension"); plot5
plot6 <- plot_box(data = data_result, target = "otc_KSD"); plot6
plot7 <- plot_box(data = data_result, target = "otc_CKD"); plot7 
plot8 <- plot_box(data = data_result, target = "otc_Gout"); plot8

#' @:merge 
plot_merge <- cowplot::plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8,
                                 align = "v", ncol = 4, rel_widths = c(rep(1,8))); plot_merge


# plot (Systemic healthy, all) ----------------------
submatr <- data_result[,c("otc_coMorbidity.glucose","PGS")]
submatr$group <- ifelse(submatr[,"otc_coMorbidity.glucose"]>0,"mild","none") 
submatr$group[submatr$otc_coMorbidity.glucose==1] <- "single"
submatr$group[submatr$otc_coMorbidity.glucose>3] <- "severe"
table(submatr$group)

a <- data.frame(PGS=data_PGS.WPN1$PGS, otc_coMorbidity.glucose=0, group="ref")
submatr <- rbind.fill(a, submatr)
submatr$group <- factor(submatr$group, levels = c("ref","none","single","mild","severe"), 
                        labels = c("Young-Healthy","Elder-Healthy (n=0)","Elder-Single (n=1)","Elder-Mild (2-3)","Elder-Severe (n>=4)"))


plot1 <- ggplot(data=submatr, aes(x=group, y=PGS, fill=group))+  
  geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  geom_violin()+
  # stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  geom_boxplot(width=0.1, position = position_dodge(0.4), fill="white", color="black", alpha=1, outlier.alpha = 0)+
  # geom_point(aes(fill=group), shape=21, color="black", alpha=0.5, size=2, position=position_jitterdodge(jitter.width=0.4, dodge.width=0.4))+
  
  stat_compare_means( 
    comparisons = list(c(1,2), c(2,3), c(2,4), c(2,5), c(3,4), c(4,5)), #
    method = "wilcox.test", paired = FALSE,
    label="p.format", #p.format p.signif 表示*
    label.y = c(seq(0.2,0.5,0.05)),
    # ref.group = "LPS",
    size = 5,
    hide.ns = T
  )+
  
  ggtitle(label = "Systemic multimorbidity (All)")+ 
  # scale_y_continuous(expand = c(0,0), limits = c(0,0.45))+
  scale_fill_manual(values = c(OKeeffe1[c(10,7,4,3,1)]))+
  # scale_fill_manual(values = c("#4D869C","#CDE8E5","#EA907A"))+
  labs(fill=NULL, x=NULL, y="PGS value")+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=20, color="black"),
    # axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot1


# plot (Systemic healthy, noT2D) ----------------------
submatr <- data_result[data_result$otc_T2D==0,c("otc_coMorbidity.glucose","PGS")]
submatr$group <- ifelse(submatr[,"otc_coMorbidity.glucose"]>0,"mild","none") 
submatr$group[submatr$otc_coMorbidity.glucose==1] <- "single"
submatr$group[submatr$otc_coMorbidity.glucose>3] <- "severe"
table(submatr$group)

a <- data.frame(PGS=data_PGS.WPN1$PGS, otc_coMorbidity.glucose=0, group="ref")
submatr <- rbind.fill(a, submatr)
submatr$group <- factor(submatr$group, levels = c("ref","none","single","mild","severe"), 
                        labels = c("Young-Healthy","Elder-Healthy (n=0)","Elder-Single (n=1)","Elder-Mild (2-3)","Elder-Severe (n>=4)"))


plot2 <- ggplot(data=submatr, aes(x=group, y=PGS, fill=group))+  
  geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  geom_violin()+
  # stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  geom_boxplot(width=0.1, position = position_dodge(0.4), fill="white", color="black", alpha=1, outlier.alpha = 0)+
  # geom_point(aes(fill=group), shape=21, color="black", alpha=0.5, size=2, position=position_jitterdodge(jitter.width=0.4, dodge.width=0.4))+
  
  stat_compare_means( 
    comparisons = list(c(1,2), c(2,3), c(2,4), c(2,5), c(3,4), c(4,5)), #
    method = "wilcox.test", paired = FALSE,
    label="p.format", #p.format p.signif 表示*
    label.y = c(seq(0.2,0.5,0.05)),
    # ref.group = "LPS",
    size = 5,
    hide.ns = T
  )+
  
  ggtitle(label = "Systemic multimorbidity (Non-diabetic)")+ 
  # scale_y_continuous(expand = c(0,0), limits = c(0,0.45))+
  scale_fill_manual(values = c(OKeeffe1[c(9,7)], OKeeffe2))+
  # scale_fill_manual(values = c("#4D869C","#CDE8E5","#EA907A"))+
  labs(fill=NULL, x=NULL, y="PGS value")+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=20, color="black"),
    # axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot2


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig3_PGS_disease_merge.pdf", width = 13, height=10); plot_merge; dev.off() 
pdf("fig3_PGS_healthy_box.pdf", width = 5, height=7); plot1; plot2; dev.off() 




#' @:PGS-diseases [all samples & non-diabetic, beta coefficient]
library(forestplot)  
library(forestploter)
##------------------------------------------------------
# data -------------------
setwd(paste0(workpath, "/part3_PGS")) 
stat_result1 <- openxlsx::read.xlsx("stat_PGS_LADE_linear.xlsx", sheet="stat_all")
stat_result2 <- openxlsx::read.xlsx("stat_PGS_LADE_linear.xlsx", sheet="stat_noT2D")

stat_result <- rbind.fill(data.frame(group="all", stat_result1), data.frame(group="noT2D", stat_result2))
stat_result <- stat_result[stat_result$exposure!="otc_T2D",]
stat_result <- stat_result[order(stat_result$pval, decreasing=T),] 
stat_result$lci <- stat_result$beta-stat_result$se*1.96
stat_result$uci <- stat_result$beta+stat_result$se*1.96

stat_result$exposure <- gsub("otc_","",stat_result$exposure) 
stat_result$exposure <- gsub("_"," & ",stat_result$exposure)
stat_result$exposure[stat_result$exposure=="ObesOverwe"] <- "Obese & Overweight"

stat_result$beta <- round(stat_result$beta,2)
stat_result$lci <- round(stat_result$lci,2)
stat_result$uci <- round(stat_result$uci,2)
stat_result$qval <- ifelse(stat_result$qval<0.01, format(stat_result$qval, scientific = T, digits = 2), round(stat_result$qval,2))


# sort ------------------
input_data <- stat_result
all <- input_data[input_data$group=="all",]; names(all) <- paste("all_", colnames(all), sep = "")
T2D <- input_data[input_data$group=="noT2D",]; names(T2D) <- paste("T2D_", colnames(T2D), sep = "")
data <- merge(all, T2D, by.x="all_exposure", by.y="T2D_exposure", all.x=T)
data <- data[order(data$all_beta, decreasing = T),]
data$all_ci <- paste0(data$all_beta, " [",data$all_lci,",",data$all_uci,"]")
data$T2D_ci <- paste0(data$T2D_beta, " [",data$T2D_lci,",",data$T2D_uci,"]")


#' @:label-text
labeltext <- data.frame(data$all_exposure); names(labeltext) <- "exposure"
labeltext$all <- data$all_ci
labeltext$qval_all <- data$all_qval
labeltext$noT2D <- data$T2D_ci
labeltext$qval_T2D <- data$T2D_qval

line1 <- data.frame(t(c(NA,"Beta coefficient [95% CI]",NA,NA,NA))); names(line1) <- colnames(labeltext)
line2 <- data.frame(t(c("Longitudinal accumulation \nof disease exposure (LADE)",
                        "All","FDR-P (All)","Non-diabetic","FDR-P (Non-diabetic)"))); names(line2) <- colnames(labeltext)

labeltext <- as.matrix(rbind(line1, line2, labeltext))


coef <- with(data, cbind(all_beta, T2D_beta))
coef <- rbind(c(NA,NA),c(NA,NA),coef)
low <- with(data, cbind(all_lci, T2D_lci))
low <- rbind(c(NA,NA),c(NA,NA),low)
high <- with(data, cbind(all_uci, T2D_uci))
high <- rbind(c(NA,NA),c(NA,NA),high)


list1 = list2 = list(gpar(fontface="plain", cex=1.10), gpar(fontface="plain", cex=1.10), gpar(fontface="plain", cex=1.10), gpar(fontface="plain", cex=1.10),gpar(fontface="plain", cex=1.10))
list3 = list4 = list5 = list6 = list7 = list8 = list9 = list(gpar(cex=1.10), gpar(cex=1.10), gpar(cex=1.10), gpar(cex=1.10),gpar(cex=1.10))

list_zzz = list(list1,list2,list3,list4,list5,list6,list7,list8,list9)


## visualization------------------------------------------------ 
plot <- forestplot(labeltext = labeltext,
                   mean =coef,
                   lower = low,
                   upper =high,
                   zero = 0, lwd.zero = 1, # 参考线
                   graph.pos = 2,
                   boxsize = 0.2,
                   lineheight = unit(13,'mm'),
                   colgap = unit(5,'mm'),
                   align = "c",
                   xlog = F,  
                   clip=c(0.8,1.2),
                   fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
                   xticks = c(seq(-0.2, 0.5, 0.1)),
                   graphwidth = unit(80,'mm'),
                   hrzl_lines = list("2" = gpar(lty=1, columns=c(3:6), col = "black"),
                                     "3" = gpar(lty=1, columns=c(1:6), col = "black")),
                   txt_gp = fpTxtGp(label =list_zzz,
                                    #label=gpar(cex=1.10),
                                    ticks=gpar(cex=1), xlab=gpar(cex = 1),
                                    title=gpar(cex = 1), legend=gpar(cex = 0.85),
                                    legend.title=gpar(cex = 0.1)),
                   col=fpColors(box=c(OKeeffe2[3], OKeeffe1[c(8)]),lines=c(OKeeffe2[3], OKeeffe1[c(8)])), 
                   
                   legend=c("All","Non-diabetic"),
                   legend_args = fpLegend (pos = list("topright"),
                                           #pos = list(x =-0.15, y=0.13),
                                           r = unit(NA, "snpc")),
                   gp = gpar(col="#CCCCCC", lwd=1)
) |> 
  fp_add_lines(h_4 = gpar(lty = 2),
               h_5 = gpar(lty = 2),
               h_6 = gpar(lty = 2),
               h_7 = gpar(lty = 2),
               h_8 = gpar(lty = 2),
               h_9 = gpar(lty = 2)) |> 
  fp_set_zebra_style("#EEE")

plot

##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig3_PGS_LADE_forest_merge.pdf", height = 6, width = 14); plot; dev.off()




#' @:PGS-Multimorbidity [all samples & non-diabetic, beta coefficient]
##------------------------------------------------------
# data -------------------
setwd(paste0(workpath, "/part3_PGS")) 
comorb_result <- openxlsx::read.xlsx("stat_PGS_multimorbidity_linear.xlsx", sheet="stat_ter") 
names(comorb_result)[grep("group.1", colnames(comorb_result))] <- "subgroup" 
comorb_result$pval <- ifelse(comorb_result$pval<0.01, format(comorb_result$pval,scientific = T, digits = 2), round(comorb_result$pval,2))


# plot --------------------
input_data <- comorb_result
input_data$group <- factor(input_data$group, levels = unique(input_data$group), labels = c("All", "Non-diabetic"))
input_data$subgroup <- factor(input_data$subgroup, levels = unique(input_data$subgroup), labels = c("Severe vs. Healthy","Mild vs. Healthy","Single vs. Healthy"))


plot <- ggplot(input_data, aes(x = beta, y=subgroup, color= subgroup)) +  
  geom_vline(xintercept = 0, linewidth=0.5, color="lightgrey")+
  geom_errorbar(aes(xmin = beta-1.96*se, xmax = beta+1.96*se, color=subgroup), width = 0.3, position = position_dodge(0.3)) + # 误差线图层
  geom_point(aes(x = beta, y = subgroup), alpha=1, size=4, position = position_dodge(0.3)) + # 点图层
  geom_text(aes(label=pval, x=beta+1.96*se+0.1), size=5, color="black")+
   
  scale_color_manual(values = c(OKeeffe2[c(7,5,3)]))+ 
  scale_fill_manual(values = c(OKeeffe2[c(7,5,3)]))+  
  scale_y_discrete(expand = c(0,0.7))+
  scale_x_continuous(expand = c(0,0.1), limits = c(-0.4,1.1))+
  
  labs(tag="d.", color=NULL, fill=NULL, y=NULL, x="Beta coefficient")+
  facet_grid(group~., scales = "free")+ 
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.text.y = element_text(size = 15, face = "plain", angle = -90, hjust=0.5), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(size=12, angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(size=15, angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black")
  )
plot



##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig3_PGS_multimorbidity_bar.pdf", height = 5, width = 7); plot; dev.off()










#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part4
#' @:Prediction [Inidividual glycemic patterns]
#' 
#' @:-----------------------------------------------------------------------------------------




#' @:Daily-traits
setwd(paste0(workpath,"/part4_predict"))
##------------------------------------------------------
# data ------------------------- 
data_daily_all.base <- openxlsx::read.xlsx("stat_Comorbidity_Daily_predict_all.xlsx", sheet = "perf_base")
data_daily_all.comb <- openxlsx::read.xlsx("stat_Comorbidity_Daily_predict_all.xlsx", sheet = "perf_comb")
data_daily_all <- rbind(data_daily_all.base, data_daily_all.comb)
data_daily_all$trait <- sapply(data_daily_all$outcome, function(x){strsplit(x, split = "_")[[1]][1]}) 
data_daily_all$trait[data_daily_all$trait=="J"] <- "J-index"

data_daily_all$time <- ifelse(grepl("_mean_all", data_daily_all$outcome), "Whole-day time", "Daytime")
data_daily_all$time[grep("_mean_night", data_daily_all$outcome)] <- "Nighttime"
data_daily_all <- data_daily_all[data_daily_all$time!="Whole-day time",]
data_daily_all$predictor <- gsub("T2D","Glycemia",data_daily_all$predictor)


#' @:Delta
data_daily_all.delta <- openxlsx::read.xlsx("stat_Comorbidity_Daily_predict_all.xlsx", sheet = "perf_merge")
data_daily_all.delta$trait <- sapply(data_daily_all.delta$outcome, function(x){strsplit(x, split = "_")[[1]][1]}) 
data_daily_all.delta$trait[data_daily_all.delta$trait=="J"] <- "J-index"

data_daily_all.delta$time <- ifelse(grepl("_mean_all", data_daily_all.delta$outcome), "Whole-day time", "Daytime")
data_daily_all.delta$time[grep("_mean_night", data_daily_all.delta$outcome)] <- "Nighttime"
data_daily_all.delta <- data_daily_all.delta[data_daily_all.delta$time!="Whole-day time",]


#' @:Percentage 
data_daily_all.delta$spearman_r_Perc <- data_daily_all.delta$spearman_r_Delta/data_daily_all.delta$spearman_r_Base
data_daily_all.delta$pearson_r_Perc <- data_daily_all.delta$pearson_r_Delta/data_daily_all.delta$pearson_r_Base


#' @:stat
stat <- wilcox.test(data_daily_all.delta$spearman_r_Base[data_daily_all.delta$time=="Daytime"], data_daily_all.delta$spearman_r_Comb[data_daily_all.delta$time=="Daytime"], paired = T)
label1 <- paste0("P = ", format(stat$p.value, scientific = T, digits = 2))
stat <- wilcox.test(data_daily_all.delta$spearman_r_Base[data_daily_all.delta$time=="Nighttime"], data_daily_all.delta$spearman_r_Comb[data_daily_all.delta$time=="Nighttime"], paired = T)
label2 <- paste0("P = ", format(stat$p.value, scientific = T, digits = 2))
stat_merge <- data.frame(group="Daily traits", time=c("Daytime","Nighttime"), tag=c(label1,label2))


# plot (bar) -------------------------
input_data <- data_daily_all
input_data$trait <- factor(input_data$trait, levels = c("J-index","MODD","MAGE","CV","eA1C","HPT","HBGI","LBGI","LPT","MPT"))

plot1 <- ggplot(input_data, aes(x = trait, y=spearman_r)) +  
  geom_hline(yintercept = seq(0,0.6,0.2), linewidth=0.5, color="lightgrey", linetype="dashed")+
  geom_bar(aes(fill=predictor), color="white", stat = "identity", width = 0.7, position = position_dodge(0.7), alpha=1)+  
   
  scale_fill_manual(values = c(OKeeffe1[c(7,10)]))+ 
  scale_y_continuous(expand = c(0,0), limits = c(0,0.8))+
  
  labs(tag="a.", title = "Prediction of daily glycemic traits (All)", color=NULL, fill=NULL, y="Prediction performance (Spearman r)", x=NULL)+
  facet_grid(time~., scales = "free")+
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.text.y = element_text(size = 15, face = "plain", angle = -90, hjust=0.5), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = c(0.9,0.9),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(size=12, angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(size=15, angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black")
  )
plot1


# plot (scatter) ----------------------------------
input_data <- data_daily_all.delta
input_data$group <- "Daily traits"


plot2 <- ggplot(input_data, aes(x=group, y=spearman_r_Perc*100)) +  
  geom_hline(yintercept = seq(10,40,10), linewidth=0.3, color="lightgrey", linetype="dashed")+ 
  geom_hline(yintercept = 0, linewidth=0.3, color="darkred")+ 
  geom_point(aes(fill=trait), shape=21, color="transparent", alpha=1, size=5)+
  geom_label_repel(aes(label=trait), size=4.5, fill = "grey90", alpha=1, xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))+ 
  geom_text(data=stat_merge, aes(label=tag), y=38, size=5)+
  # annotate("text", label=stat_merge$tag, x=1, y=0.12, color="black", size=5)+
  
  scale_fill_manual(values = c(OKeeffe1[-c(6)]))+
  scale_color_manual(values = c(OKeeffe1[-c(6)]))+
  # scale_y_continuous(expand = c(0,0), limits = c(-0.03,0.13))+
  
  labs(tag="b.", title = "Improvement of prediction performance", color=NULL, fill=NULL, y="% Percentage\n(Delta Spearman rho / Basic rho)", x=NULL)+
  facet_grid(.~time, scales = "free")+
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.text.y = element_text(size = 15, face = "plain", angle = -90, hjust=0.5), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(size=12, angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(size=15, angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black")
  )
plot2

  
##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig4_predict_daily_all.pdf", height = 6, width = 10); plot1; dev.off()
pdf("fig4_predict_daily_all_delta.pdf", height = 6, width = 5); plot2; dev.off()




#' @:DRs-traits
setwd(paste0(workpath,"/part4_predict"))
##------------------------------------------------------
# data ------------------------- 
data_DRs_all.base <- openxlsx::read.xlsx("stat_Comorbidity_DRs_predict_all.xlsx", sheet = "perf_base")
data_DRs_all.comb <- openxlsx::read.xlsx("stat_Comorbidity_DRs_predict_all.xlsx", sheet = "perf_comb")
data_DRs_all <- rbind(data_DRs_all.base, data_DRs_all.comb)
data_DRs_all$predictor <- gsub("T2D","Glycemia",data_DRs_all$predictor)
data_DRs_all$meal <- sapply(data_DRs_all$outcome, function(x){strsplit(x, split = "_")[[1]][1]}) 
data_DRs_all$response <- sapply(data_DRs_all$outcome, function(x){strsplit(x, split = "_")[[1]][2]}) 
data_DRs_all$response[data_DRs_all$response=="acc"] <- "IR"
data_DRs_all$response[data_DRs_all$response=="ppge"] <- "iMPG"
data_DRs_all$response[data_DRs_all$response=="peak"] <- "MPG"


#' @:Delta
data_DRs_all.delta <- openxlsx::read.xlsx("stat_Comorbidity_DRs_predict_all.xlsx", sheet = "perf_merge")
data_DRs_all.delta$meal <- sapply(data_DRs_all.delta$outcome, function(x){strsplit(x, split = "_")[[1]][1]}) 
data_DRs_all.delta$response <- sapply(data_DRs_all.delta$outcome, function(x){strsplit(x, split = "_")[[1]][2]}) 
data_DRs_all.delta$response[data_DRs_all.delta$response=="acc"] <- "IR"
data_DRs_all.delta$response[data_DRs_all.delta$response=="ppge"] <- "iMPG"
data_DRs_all.delta$response[data_DRs_all.delta$response=="peak"] <- "MPG" 


#' @:Percentage 
data_DRs_all.delta$spearman_r_Perc <- data_DRs_all.delta$spearman_r_Delta/data_DRs_all.delta$spearman_r_Base
data_DRs_all.delta$pearson_r_Perc <- data_DRs_all.delta$pearson_r_Delta/data_DRs_all.delta$pearson_r_Base


#' @:stat
stat <- wilcox.test(data_DRs_all.delta$spearman_r_Base[data_DRs_all.delta$meal=="RG"], data_DRs_all.delta$spearman_r_Comb[data_DRs_all.delta$meal=="RG"], paired = T)
label1 <- paste0("P = ", round(stat$p.value,2))
stat <- wilcox.test(data_DRs_all.delta$spearman_r_Base[data_DRs_all.delta$meal=="WG"], data_DRs_all.delta$spearman_r_Comb[data_DRs_all.delta$meal=="WG"], paired = T)
label2 <- paste0("P = ", round(stat$p.value,2))
stat_merge <- data.frame(group="Response", time=c("RG","WG"), tag=c(label1,label2))


# plot (bar) -------------------------
input_data <- data_DRs_all
input_data$response <- factor(input_data$response, levels = c("MPG","iMPG","iAUC","IR"))

plot1 <- ggplot(input_data, aes(x = response, y=spearman_r)) +  
  geom_hline(yintercept = seq(0,0.6,0.2), linewidth=0.5, color="lightgrey", linetype="dashed")+
  geom_bar(aes(fill=predictor), color="white", stat = "identity", width = 0.7, position = position_dodge(0.7), alpha=1)+  
  
  scale_fill_manual(values = c(OKeeffe1[c(5,2)]))+ 
  scale_y_continuous(expand = c(0,0), limits = c(0,0.8))+
  
  labs(tag="c.", title = "Prediction of postprandial glycemic responses (All)", color=NULL, fill=NULL, y="Prediction performance (Spearman r)", x=NULL)+
  facet_grid(meal~., scales = "free")+
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.text.y = element_text(size = 15, face = "plain", angle = -90, hjust=0.5), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = c(0.8,0.9),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(size=12, angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(size=15, angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black")
  )
plot1


# plot (scatter) ----------------------------------
input_data <- data_DRs_all.delta
input_data$group <- "Response"


plot2 <- ggplot(input_data, aes(x=group, y=spearman_r_Perc*100)) +  
  geom_hline(yintercept = seq(0,60,20), linewidth=0.5, color="lightgrey", linetype="dashed")+ 
  geom_hline(yintercept = 0, linewidth=0.5, color="darkred")+ 
  geom_point(aes(fill=response), shape=21, color="transparent", alpha=1, size=5)+
  geom_label_repel(aes(label=response), size=4.5, fill = "grey90", alpha=1, xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))+ 
  geom_text(data=stat_merge, aes(label=tag), y=0.10, size=5)+ 
  
  scale_fill_manual(values = c(OKeeffe1[-c(6)]))+
  scale_color_manual(values = c(OKeeffe1[-c(6)]))+  
  
  labs(tag="d.", title = "Improvement of prediction performance", color=NULL, fill=NULL, y="Delta of Spearman r", x=NULL)+
  facet_grid(.~meal, scales = "free")+
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.text.y = element_text(size = 15, face = "plain", angle = -90, hjust=0.5), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(size=12, angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(size=15, angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black")
  )
plot2


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig4_predict_DRs_all.pdf", height = 6, width = 6); plot1; dev.off()
pdf("fig4_predict_DRs_all_delta.pdf", height = 6, width = 5); plot2; dev.off()




#' @:PGS
setwd(paste0(workpath,"/part4_predict"))
##------------------------------------------------------
# data ------------------------- 
data_PGS_all.base <- openxlsx::read.xlsx("stat_Comorbidity_PGS_predict_all.xlsx", sheet = "perf_base")
data_PGS_all.comb <- openxlsx::read.xlsx("stat_Comorbidity_PGS_predict_all.xlsx", sheet = "perf_comb")
data_PGS_all <- rbind(data_PGS_all.base, data_PGS_all.comb)


# plot ----------------------------------
input_data <- data_PGS_all
input_data$group <- "PGS"

plot1 <- ggplot(input_data, aes(x = group, y=spearman_r)) +  
  geom_hline(yintercept = seq(0,0.6,0.2), linewidth=0.5, color="lightgrey", linetype="dashed")+
  geom_bar(aes(fill=predictor), color="white", stat = "identity", width = 0.7, position = position_dodge(0.7), alpha=1)+  
  
  scale_fill_manual(values = c("#8FC0A9","#006769"))+ 
  scale_y_continuous(expand = c(0,0), limits = c(0,0.6))+
  
  labs(tag="e.", title = "Prediction of PGS (All)", color=NULL, fill=NULL, y="Prediction performance (Spearman r)", x=NULL)+ 
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.text.y = element_text(size = 15, face = "plain", angle = -90, hjust=0.5), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = c(0.8,0.9),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(size=12, angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(size=15, angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black")
  )
plot1


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig4_predict_PGS_all.pdf", height = 6, width = 3.5); plot1; dev.off()













#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part5
#' @:Proteomics
#' 
#' @:-----------------------------------------------------------------------------------------




#' @:CRP-screening [Comorbidity-related proteins, with validation]
#' @:Scatter
##-------------------------------------------------------------
# data ----------------------------------
setwd(paste0(workpath,"/part5_proteomics")) 
stat_all <- openxlsx::read.xlsx("valid_Comorbidity_proteomics_linear.xlsx", sheet = "stat") 
stat_all$valid[is.na(stat_all$valid)] <- "Invalid"
stat_all$label <- ifelse(stat_all$valid=="valid", stat_all$name,NA)


# plot ---------------------------
input_data <- stat_all 
input_data$valid <- factor(input_data$valid, levels = c("valid","Invalid"), labels = c("Validated", "Unvalidated"))
input_data$alpha <- ifelse(input_data$valid=="Validated",1,0.7)


plot1 <- ggplot(data=input_data, aes(x=beta_disc, y=beta, fill=valid))+    
  geom_hline(yintercept = 0, color="grey80", linetype="dashed", linewidth=0.5)+ 
  geom_vline(xintercept = 0, color="grey80", linetype="dashed", linewidth=0.5)+ 
  geom_point(aes(alpha=alpha), size=4, shape=21, color="black")+   
  geom_label_repel(aes(label=label), size=5, fill = "grey98", alpha=1, xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))+
  
  # scale_x_continuous(expand = c(0,0), limits = c(-1,0.1))+
  # scale_y_continuous(expand = c(0,0), limits = c(-0.15,0.15))+
  scale_size_continuous(range = c(1,10))+
  scale_fill_manual(values = c(Benedictus[11],"grey70")) +
  labs(title="Longitudinal multimorbid proteomic signatures", 
       x="Standardized beta coefficient for discovery (GNHS)", 
       y="Standardized beta coefficient for validation (CHNS)", fill="External validation", size="Size")+  
  # facet_grid(time~., scales = "free", space = "free")+
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = c(0.15,0.8),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15, face = "plain"),
    axis.title.y = element_text(color = "black",size=15, face = "plain"),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black", face = "plain"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black", face = "plain") 
  ) 
plot1


##-------------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig5_CRP_screening.pdf", width = 9.5, height = 9); plot1; dev.off()




#' @:CRP-protein-network [Topological features of proteomic network] 
#' @:dot-line
##-------------------------------------------------------------
# data ---------------------------
setwd(paste0(workpath,"/part5_proteomics"))
database <- openxlsx::read.xlsx("stat_Comorbidity_proteomics_network.xlsx", sheet = "stat")  
database$group <- factor(database$group, levels = c("healthy","single","mild","severe"), labels = c("Healthy","Single","Mild-comorbid","Severe-comorbid"))
database$time <- 3*(database$time-1)

# function --------------------------
plot_dot <- function(data, trait, y_label, legend="none"){
  
  plot1 <- ggplot(data, aes(x = time, y=data[,trait], color= group)) + 
    geom_line(aes(group=group), color="grey80", linewidth=0.5)+    
    geom_point(aes(color=group), size=5)+
     
    scale_x_continuous(expand = c(0,0.3), breaks = seq(0,6,3))+
    scale_color_manual(values = c(Benedictus[c(12:10,3)]))+ 
    scale_fill_manual(values = c(OKeeffe2))+   
    
    labs(title=y_label, color=NULL, fill=NULL, y="Value", x="Follow-up years", caption = NULL)+ 
    
    theme_classic()+
    theme(
      # for facet
      strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
      strip.text.y = element_text(size = 15, face = "plain", angle = -90, hjust=0.5), 
      strip.background = element_rect(color = "transparent", fill= "transparent"),
      panel.spacing = unit(0,"mm"),
      
      # for ggtitle
      plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
      
      panel.grid.major = element_line(color = NA),
      panel.background = element_rect(fill = "transparent",color = NA),
      plot.background = element_rect(fill = "transparent",color = NA),
      panel.grid.minor = element_blank(),
      
      # legend
      legend.position = as.character(legend),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 15),
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
      legend.box.background = element_rect(fill = "transparent", color = "transparent"),
      
      # for axis
      axis.ticks.length = unit(3,"mm"),
      axis.title.x = element_text(face = "plain", color = "black"),
      axis.title.y = element_text(face = "plain", color = "black"),
      axis.title = element_text(size=15),
      
      axis.text.y = element_text(size=12, angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
      axis.text.x = element_text(size=15, angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black")
    )
  return(plot1)
}


# plot -------------------------
plot_node <- plot_dot(data = database, trait = "num.nodes", y_label = "Number of effective nodes"); plot_node
plot_edge <- plot_dot(data = database, trait = "num.edges", y_label = "Number of effective edges"); plot_edge
plot_edge.pos <- plot_dot(data = database, trait = "num.pos.edges", y_label = "Number of positive edges"); plot_edge.pos
plot_edge.neg <- plot_dot(data = database, trait = "num.neg.edges", y_label = "Number of negative edges"); plot_edge.neg
plot_degree <- plot_dot(data = database, trait = "global.degree", y_label = "Global degree"); plot_degree
plot_density <- plot_dot(data = database, trait = "global.density", y_label = "Global density"); plot_density
plot_path <- plot_dot(data = database, trait = "global.average.path", y_label = "Global average path length"); plot_path
plot_clossness <- plot_dot(data = database, trait = "global.clossness", y_label = "Global clossness"); plot_clossness
plot_betweeness <- plot_dot(data = database, trait = "global.betweeness", y_label = "Global betweeness"); plot_betweeness
plot_centality <- plot_dot(data = database, trait = "global.eigen.centrality", y_label = "Global eigenvector centrality"); plot_centality
plot_transitivity <- plot_dot(data = database, trait = "global_transitivity", y_label = "Global transitivity", legend = c(0.05,0)); plot_transitivity

#' @:merge
plot_merge <- cowplot::plot_grid(plot_edge.pos, plot_edge.neg, plot_degree, plot_betweeness, plot_centality, plot_transitivity,
                                 align = "v", ncol = 3, rel_widths = c(rep(1,3))); plot_merge


##-------------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig5_Proteomics_network.pdf", height = 8, width = 10); plot_merge; dev.off()




#' @:CRP-enrichment
setwd(paste0(workpath,"/part5_proteomics")) 
##-------------------------------------------------------------
# data ------------------------- 
data_enrich <- openxlsx::read.xlsx("stat_Comorbidity_proteomics_GOenrich.xlsx", sheet = "stat")
data_enrich <- data_enrich[order(data_enrich$qvalue, decreasing = F),]
data_enrich <- data_enrich[c(1:10),]
data_enrich <- data_enrich[order(data_enrich$GeneRatio2, decreasing = F),]

data_enrich$GeneRatio2 <- data_enrich$GeneRatio2*100
data_enrich$Description <- paste0(toupper(substr(data_enrich$Description,1,1)), substr(data_enrich$Description,2,str_length(data_enrich$Description)))


# plot ------------------------
input_sort <- data_enrich
input_sort$Description <- factor(input_sort$Description, levels = input_sort$Description)


#' @:plot
plot1 <- ggplot(input_sort, aes(x=GeneRatio2, y=Description)) +  
  geom_bar(stat="identity", position = position_dodge(0.75), width=0.02, color="transparent", fill="lightgrey") +  
  geom_point(aes(size=-log10(qvalue), color=GeneRatio2))+
  
  scale_size_continuous(range = c(4,10))+
  scale_color_gradientn(colors = Benedictus[c(5,12,13)]) +    
  scale_x_continuous(expand = c(0,0), limits = c(0,30))+
  
  ggtitle(label = NULL)+ 
  labs(fill="Time", color="Gene ratio", x="Gene ratio (%)", y=NULL, size="FDR-P", title=NULL)+  
  
  theme_classic2()+ 
  theme( 
    # facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill="transparent"),
    panel.spacing.x = unit(10,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend 
    legend.position = c(0.8,0.35),
    legend.title = element_text(face = "plain", size = 15),
    legend.text = element_text(face = "plain", size=15),
    legend.background = element_rect(fill = "transparent", color = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black",size=20),
    axis.title.y = element_text(face = "plain", color = "black",size=20, vjust = 0.5, hjust = 0.5),  
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5,size=15), 
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1,size=15),  
    axis.text = element_text(color="black")
  )
plot1


##-------------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig5_CRPprotein_enrichment_bar.pdf", width = 8, height = 6); plot1; dev.off()




#' @:CRP_protein-PGS [linear]
#' @:forest
setwd(paste0(workpath,"/part5_proteomics")) 
##-------------------------------------------------------------
# data ------------------------- 
data_score_PGS <- openxlsx::read.xlsx("stat_Proteins_PGS_linear.xlsx", sheet = "stat_all")
stat <- data_score_PGS[!grepl("score_", data_score_PGS$exposure),]  
stat <- stat[stat$pval<0.05 & stat$qval<0.05,]

sensi_score_PGS <- openxlsx::read.xlsx("stat_Proteins_PGS_linear.xlsx", sheet = "stat_noT2D")
stat_noT2D <- sensi_score_PGS[!grepl("score_", sensi_score_PGS$exposure),]   
stat_noT2D <- stat_noT2D[stat_noT2D$exposure %in% stat$exposure,]
list_prot <- stat_noT2D$exposure[stat_noT2D$pval<0.05]


input_data <- rbind(data.frame(group="all", stat), data.frame(group="noT2D", stat_noT2D))
input_data <- input_data[input_data$exposure %in% list_prot,]
input_data <- input_data[order(input_data$beta, decreasing = F),]

input_data$SYMBOL <- factor(input_data$SYMBOL, levels = unique(input_data$SYMBOL))
input_data$color <- ifelse(input_data$beta>0, "Positive", "Negative")
input_data$color[input_data$qval>0.05] <- "Insignificant (Adjusted P > 0.05)"
input_data$group <- factor(input_data$group, levels = c("all", "noT2D"), labels = c("All", "Non-diabetic"))


# plot -------------------------

plot <- ggplot(input_data, aes(y = beta, x=SYMBOL, color=group)) +
  annotate("rect", xmin = 0, xmax=15.5, ymin = -0.22, ymax=0.22, fill=Benedictus[c(8)])+
  annotate("rect", xmin = 15.5, xmax=18.5, ymin = -0.22, ymax=0.22, fill=Benedictus[c(6)])+
  geom_hline(yintercept = 0, linewidth=0.5, linetype="dashed", color="darkred")+ 
  
  geom_errorbar(aes(ymin = beta-1.96*se, ymax = beta+1.96*se, color=group), width = 0.6, position = position_dodge(0.5)) + # 误差线图层
  geom_point(aes(fill=group, color=group), shape=22, alpha=1, size=5, position = position_dodge(0.5)) + # 点图层 
  
  scale_color_manual(values = c(Archambault[c(2:1)]))+
  scale_fill_manual(values = c(Archambault[c(2:1)]))+
  scale_y_continuous(expand = c(0,0), limits = c(-0.22,0.22))+
  
  ggtitle(label = "Associations of multimorbidity-related proteins and PGS")+ 
  labs(color=NULL, fill=NULL, x=NULL, y="Standardized beta coefficient for PGS")+ 
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = c(0.3,0.85),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(angle=0, face = "plain", hjust=1, vjust=0.5, color = "black", size=15), 
    axis.text.x = element_text(angle=90, face = "plain", hjust=1, vjust=1, color = "black", size=12)
  )
plot


##-------------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig5_CRPprotein_PGS_forest.pdf", height = 6, width = 10); plot; dev.off()




#' @:PGS-Prediction [prediction performance]
#' @:scatter
setwd(paste0(workpath,"/part5_proteomics")) 
##-------------------------------------------------------------
# data ------------------------- 
stat <- openxlsx::read.xlsx("stat_Proteomics_PGS_predict.xlsx", sheet = "perf")
r <- round(stat$spearman_r,2)
p <- format(stat$pearson_p, scientific = T, digits = 2)
label <- paste("Spearman r = ", r, "\nP = ",p)

database <- openxlsx::read.xlsx("stat_Proteomics_PGS_predict.xlsx", sheet = "pred")
names(database)[3:4] <- c("true_PGS","pred_PGS")
input_data <- database

# plot -------------------------

plot <- ggplot(input_data, aes(x = true_PGS, y= pred_PGS)) +    
  annotate("text", label=label, color="black", size=6, x=0.3, y=0.15)+
  geom_point(color=Benedictus[11], alpha=0.3, size=4) + # 点图层
  geom_smooth(method="lm", fill="lightgrey", color=Benedictus[1])+

  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0.05, 0.2))+ 
  
  labs(color=NULL, fill=NULL, y="Predicted-PGS", x="Measured-PGS", caption = NULL)+
  
  
  theme_classic()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0), 
    strip.text.y = element_text(size = 15, face = "plain", angle = -90, hjust=0.5), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(size=15, angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"), 
    axis.text.x = element_text(size=15, angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black")
  )
plot



##-------------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig5_PGS_predict_scatter.pdf", height = 5.5, width = 6); plot; dev.off()




#' @:PGS-Prediction [prediction SHAP ranking]
setwd(paste0(workpath,"/part5_proteomics")) 
##-------------------------------------------------------------
# data ------------------------- 
stat <- openxlsx::read.xlsx("stat_Proteomics_PGS_predict.xlsx", sheet = "SHAP")
stat <- stat[order(stat$mean_absolute_shap, decreasing = T),]

input_data <- stat[c(1:15),]
input_data$mean_absolute_shap <- input_data$mean_absolute_shap*1000
input_data$SYMBOL <- factor(input_data$SYMBOL, levels = input_data$SYMBOL)


# plot ------------------------ 

#' @:plot
plot1 <- ggplot(input_data, aes(x=SYMBOL, y=mean_absolute_shap)) +  
  geom_bar(stat="identity", position = position_dodge(0.75), width=0.02, color="transparent", fill="lightgrey") +  
  geom_point(aes(color=mean_absolute_shap), size=5)+
  
  # scale_size_continuous(range = c(4,10))+
  scale_color_gradientn(colors = Benedictus[c(5,12,13)]) +
  scale_y_continuous(expand = c(0,0), limits = c(0,2.5))+
  
  labs(fill=NULL, color=NULL, y="Mean absolute SHAP value (x10^-3)", x=NULL, title="Top15 protein predictors of PGS")+  
  
  theme_classic2()+ 
  theme( 
    # facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill="transparent"),
    panel.spacing.x = unit(10,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, face = "plain", hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    
    # legend 
    legend.position = "none",
    legend.title = element_text(face = "plain", size = 15),
    legend.text = element_text(face = "plain", size=15),
    legend.background = element_rect(fill = "transparent", color = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black",size=20),
    axis.title.y = element_text(face = "plain", color = "black",size=20, vjust = 0.5, hjust = 0.5),  
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5,size=15), 
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5,size=15),  
    axis.text = element_text(color="black")
  )
plot1


##-------------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("fig5_PGS_predict_SHAP_bar.pdf", width = 9, height = 6); plot1; dev.off()













#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part6
#' @:Extended-Figures [Supplementary plots]
#' 
#' @:-----------------------------------------------------------------------------------------




#' @:Aging-Breakfasts [RG & WG]
#' @:boxplot  
setwd(paste0(root,"/1_data"))
##-----------------------------------------------------------
# data --------------------------------------- 
#' @:GNHS
data_pheno.GNHS <- openxlsx::read.xlsx("GNHS_pheno_all.xlsx", sheet = 1)
data_CGM_meal.GNHS <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "meal")
data_CGM_meal.GNHS <- data_CGM_meal.GNHS[data_CGM_meal.GNHS$break_type %in% c("refine","whole"),] 
data_CGM_meal.GNHS <- data_CGM_meal.GNHS[data_CGM_meal.GNHS$cohort=="NL",]
data_GNHS <- aggregate(data_CGM_meal.GNHS[,c("ppgr","ppge","peak","glu_acc")], by=list(data_CGM_meal.GNHS$id, data_CGM_meal.GNHS$break_type), mean, na.rm=T)
names(data_GNHS)[c(1:2)] <- c("sampleid","diet")  
data_GNHS <- merge(data_pheno.GNHS[,c("sampleid","age")], data_GNHS, by="sampleid", all.y=T)
data_GNHS$age[is.na(data_GNHS$age)] <- mean(data_GNHS$age, na.rm = T)


#' @:WPN1
data_pheno.WPN1 <- openxlsx::read.xlsx("WPN1_pheno_traits_all.xlsx", sheet = 1)
data_pheno.WPN1 <- data_pheno.WPN1[data_pheno.WPN1$time=="v1",]
data_CGM_meal.WPN1 <- openxlsx::read.xlsx("WPN1_CGMdata_clean.xlsx", sheet = "meal")
data_CGM_meal.WPN1$diet <- ifelse(data_CGM_meal.WPN1$diet=="RG","refine","whole")
data_WPN1 <- aggregate(data_CGM_meal.WPN1[,c("ppgr","ppge","peak","glu_acc")], by=list(data_CGM_meal.WPN1$id, data_CGM_meal.WPN1$diet), mean, na.rm=T)
names(data_WPN1)[c(1:2)] <- c("sampleid","diet")  
data_WPN1 <- merge(data_WPN1, data_pheno.WPN1[,c("id","age")], by.y="id", by.x = "sampleid", all.x=T) 


#' @:merge
database <- rbind(data.frame(cohort="GNHS", data_GNHS), data.frame(cohort="WePrecision",data_WPN1))
a <- quantile(database$age[database$cohort=="GNHS"], seq(0,1,1/3)) 
database$group <- paste0("Group",(length(a)-1))
for(i in 1:(length(a)-1)){
  database$group[database$age>=a[i] & database$age<a[i+1]] <- paste0("Group",i)
}
database$group[database$cohort=="WePrecision"] <- "Group0"
aggregate(database$age[database$diet=="refine"], by=list(database$group[database$diet=="refine"]), mean)
aggregate(database$age[database$diet=="whole"], by=list(database$group[database$diet=="whole"]), mean)

database <- database[,-which(colnames(database) %in% c("age"))]
table(database$group)


# plot1 ----------------------------------------
input_data <- reshape2::melt(database[database$diet=="refine",])
input_data$cohort <- factor(input_data$cohort, levels = c("WePrecision","GNHS"))
input_data$variable <- factor(input_data$variable, levels = c("ppgr","peak","ppge","glu_acc"), labels = c("iAUC","MPG","iMPG","IR"))
input_data$group <- factor(input_data$group, levels = c(paste0("Group",seq(0,3,1))), labels = c("WePrecision\n(27.1y)","GNHS-Q1\n(63.5y)","GNHS-Q2\n(68.5y)","GNHS-Q3\n(74.2y)"))


plot1 <- ggplot(data=input_data, aes(x=group, y=value, fill=group))+  
  geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, position = position_dodge(0.4), fill="white", color="black", alpha=1, outlier.alpha = 0)+
  # 
  # stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  # geom_boxplot(width=0.6, position = position_dodge(0.4), color="black", alpha=1, outlier.alpha = 0)+
  # geom_point(aes(fill=group), shape=21, color="transparent", alpha=0.5, size=2, position=position_jitterdodge(jitter.width=0.6, dodge.width=0.6))+
  # 
  stat_compare_means( 
    comparisons = list(c(2,3), c(2,4), c(3,4), c(1,2), c(1,3), c(1,4)), 
    method = "wilcox.test", paired = FALSE,
    label="p.format", #p.format p.signif 表示* 
    size = 5,
    hide.ns = T
  )+
  
  scale_y_continuous(expand = c(0.1,0.1))+
  ggtitle(label = "RG")+ 
  scale_fill_manual(values = c(OKeeffe1[c(4,7:10)]))+
  labs(fill=NULL, x=NULL, y="Postprandial dietary responses")+ 
  facet_grid(variable~., scales = "free")+ 
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot1


# plot2 ----------------------------------------
input_data <- reshape2::melt(database[database$diet=="whole",])
input_data$cohort <- factor(input_data$cohort, levels = c("WePrecision","GNHS"))
input_data$variable <- factor(input_data$variable, levels = c("ppgr","peak","ppge","glu_acc"), labels = c("iAUC","MPG","iMPG","IR"))
input_data$group <- factor(input_data$group, levels = c(paste0("Group",seq(0,3,1))), labels = c("WePrecision\n(27.1y)","GNHS-Q1\n(63.5y)","GNHS-Q2\n(68.5y)","GNHS-Q3\n(74.2y)"))

plot2 <- ggplot(data=input_data, aes(x=group, y=value, fill=group))+  
  geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, position = position_dodge(0.4), fill="white", color="black", alpha=1, outlier.alpha = 0)+
  # 
  # stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  # geom_boxplot(width=0.6, position = position_dodge(0.4), color="black", alpha=1, outlier.alpha = 0)+
  # geom_point(aes(fill=group), shape=21, color="transparent", alpha=0.5, size=2, position=position_jitterdodge(jitter.width=0.6, dodge.width=0.6))+
  
  stat_compare_means(
    comparisons = list(c(2,3), c(2,4), c(3,4), c(1,2), c(1,3), c(1,4)), 
    method = "t.test", paired = FALSE,
    label="p.format", #p.signif 表示* 
    size = 5,
    hide.ns = T
  )+
  
  scale_y_continuous(expand = c(0.1,0.1))+
  ggtitle(label = "WG")+ 
  scale_fill_manual(values = c(OKeeffe1[c(4,7:10)]))+
  labs(fill=NULL, x=NULL, y="Postprandial dietary responses")+ 
  facet_grid(variable~., scales = "free")+ 
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot2


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple1_DRs_aging_cv_box.pdf", width = 5, height=15); plot1; plot2; dev.off()




#' @:Aging-DRs [CV, slicing windows]
#' @:scatter-line
##-----------------------------------------------------------
# data ---------------------------------------
#' @:WePrecision
setwd(paste0(root,"/1_data"))
data_CGM_meal.WPN1 <- openxlsx::read.xlsx("WPN1_CGMdata_clean.xlsx", sheet = "meal")
input_data <- aggregate(data_CGM_meal.WPN1[,c("ppgr","ppge","peak","glu_acc")], by=list(data_CGM_meal.WPN1$id, data_CGM_meal.WPN1$diet), mean, na.rm=T)
names(input_data)[c(1:2)] <- c("id","diet")
input_data <- merge(data_pheno.WPN1[,c("id","age")], input_data, by="id", all.y=T)

a <- apply(input_data[input_data$diet=="RG", c("ppgr","ppge","peak","glu_acc")], 2, function(x){sd(x)*100/mean(x)})
b <- apply(input_data[input_data$diet=="WG", c("ppgr","ppge","peak","glu_acc")], 2, function(x){sd(x)*100/mean(x)})
data_WPN1 <- data.frame(diet=c("refine","whole"), rbind(a,b), age_mean=mean(input_data$age[input_data$diet=="RG"]))
data_WPN1 <- data.frame(t(data_WPN1))
names(data_WPN1) <- data_WPN1[1,]; data_WPN1 <- data_WPN1[-1,]
data_WPN1 <- data.frame(trait=rownames(data_WPN1),data_WPN1)
data_WPN1 <- data_WPN1[-nrow(data_WPN1),]


#' @:GNHS
setwd(paste0(workpath, "/part3_PGS"))
RG <- openxlsx::read.xlsx("stat_DRs_CV_age_corr.xlsx", sheet = "corr_GNHS_refine")
WG <- openxlsx::read.xlsx("stat_DRs_CV_age_corr.xlsx", sheet = "corr_GNHS_whole")
data_GNHS.corr <- rbind(data.frame(meal="RG",RG), data.frame(meal="WG",WG))
data_GNHS.corr <- data_GNHS.corr[data_GNHS.corr$window_size==6,]

database <- openxlsx::read.xlsx("stat_DRs_CV_age_corr.xlsx", sheet = "CV_GNHS")
database <- database[database$window_size==6,]
database$label <- paste0("round",seq(1,nrow(database),1))


# plot1 ----------------------------------------
input_data <- reshape2::melt(database[database$diet=="refine", c("label","ppgr","peak","ppge","glu_acc")])
input_data <- merge(input_data, database[,c("label","age_mean")], by="label", all.x = T)
input_data$variable <- factor(input_data$variable, levels = c("ppgr","peak","ppge","glu_acc"), labels = c("iAUC","MPG","iMPG","IR"))

input_data2 <- data.frame(variable=data_WPN1$trait, value=data_WPN1$refine)
input_data2$variable <- factor(input_data2$variable, levels = c("ppgr","peak","ppge","glu_acc"), labels = c("iAUC","MPG","iMPG","IR"))
input_data2$value <- as.numeric(input_data2$value)

max <- data.frame(aggregate(input_data$value, by=list(input_data$variable), max)); names(max) <- c("variable", "max")
input_data3 <- data_GNHS.corr[data_GNHS.corr$meal=="RG",]
input_data3$variable <- factor(input_data3$Var2, levels = c("ppgr","peak","ppge","glu_acc"), labels = c("iAUC","MPG","iMPG","IR"))
input_data3 <- merge(input_data3, max, by="variable", all.x=T)
input_data3$pos_y <- floor(input_data3$max)
input_data3$label <- paste("Spearman r = ", round(input_data3$r,3),
                           "\n FDR = ", ifelse(input_data3$qval<0.001, format(input_data3$qval, scientific = T, digits = 2), round(input_data3$qval,3)))


plot1 <- ggplot(data=input_data, aes(x=age_mean, y=value))+
  geom_hline(data=input_data2, aes(yintercept=value), color="#973131", linetype="dashed")+
  geom_point(shape=21, color="white", fill=OKeeffe2[2], alpha=1, size=5)+
  geom_smooth(method = "lm", se = T, linewidth=1, color=OKeeffe1[9], fill=OKeeffe1[7])+
  geom_text(data = input_data3, aes(x = 57, y = pos_y-3, label = label), color = "black", vjust=0, hjust=0, size=5)+
  geom_text(data = input_data2, aes(x = 75, y = value-3, label = "WePrecision line"), color = "#973131")+

  ggtitle(label = "RG")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0.2,0.2))+

  labs(fill=NULL, x="Mean age in slicing windows", y="% CV in slicing windows")+
  facet_grid(variable~., scales = "free")+

  theme_bw()+
  theme(
    # for facet
    strip.text.y = element_text(size = 15, angle = 0),
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),

    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),

    panel.grid.major = element_line(color = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),

    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),

    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black")
  )
plot1


# plot2 ----------------------------------------
input_data <- reshape2::melt(database[database$diet=="whole", c("label","ppgr","peak","ppge","glu_acc")])
input_data <- merge(input_data, database[,c("label","age_mean")], by="label", all.x = T)
input_data$variable <- factor(input_data$variable, levels = c("ppgr","peak","ppge","glu_acc"), labels = c("iAUC","MPG","iMPG","IR"))

input_data2 <- data.frame(variable=data_WPN1$trait, value=data_WPN1$refine)
input_data2$variable <- factor(input_data2$variable, levels = c("ppgr","peak","ppge","glu_acc"), labels = c("iAUC","MPG","iMPG","IR"))
input_data2$value <- as.numeric(input_data2$value)

max <- data.frame(aggregate(input_data$value, by=list(input_data$variable), max)); names(max) <- c("variable", "max")
input_data3 <- data_GNHS.corr[data_GNHS.corr$meal=="WG",]
input_data3$variable <- factor(input_data3$Var2, levels = c("ppgr","peak","ppge","glu_acc"), labels = c("iAUC","MPG","iMPG","IR"))
input_data3 <- merge(input_data3, max, by="variable", all.x=T)
input_data3$pos_y <- floor(input_data3$max)
input_data3$label <- paste("Spearman r = ", round(input_data3$r,3),
                           "\n FDR = ", ifelse(input_data3$qval<0.001, format(input_data3$qval, scientific = T, digits = 2), round(input_data3$qval,3)))


plot2 <- ggplot(data=input_data, aes(x=age_mean, y=value))+
  geom_hline(data=input_data2, aes(yintercept=value), color="#973131", linetype="dashed")+
  geom_point(shape=21, color="white", fill=OKeeffe2[2], alpha=1, size=5)+
  geom_smooth(method = "lm", se = T, linewidth=1, color=OKeeffe1[9], fill=OKeeffe1[7])+
  geom_text(data = input_data3, aes(x = 57, y = pos_y-3, label = label), color = "black", vjust=0, hjust=0, size=5)+
  geom_text(data = input_data2, aes(x = 75, y = value-3, label = "WePrecision line"), color = "#973131")+

  ggtitle(label = "WG")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0.2,0.2))+
  labs(fill=NULL, x="Mean age in slicing windows", y="% CV in slicing windows")+
  facet_grid(variable~., scales = "free")+

  theme_bw()+
  theme(
    # for facet
    strip.text.y = element_text(size = 15, angle = 0),
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),

    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),

    panel.grid.major = element_line(color = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),

    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),

    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black")
  )
plot2


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple1_DRs_aging_line.pdf", width = 5, height=14); plot1; plot2; dev.off()




#' @:LADE-Daily-triats [Longitudinal, all, GNHS]
#' @:heatmap-plot
##------------------------------------------------------
# data -----------------------
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list <- c(paste0(a,"_mean_day"),paste0(a,"_mean_night"),
          paste0(b,"_mean_day.asin"),paste0(b,"_mean_night.asin"))

setwd(paste0(workpath, "/part1_disease"))
stat_disc <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_all")
stat_disc <- stat_disc[stat_disc$outcome %in% list,] 

stat_disc$time <- NA 
stat_disc$time[grep("_day", stat_disc$outcome)] <- "Daytime"
stat_disc$time[grep("_night", stat_disc$outcome)] <- "Nighttime"

stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]})
stat_disc$trait[stat_disc$trait=="J"] <- "J-index"
stat_disc$trait[stat_disc$trait=="LPT"] <- "TBR"
stat_disc$trait[stat_disc$trait=="MPT"] <- "TIR"
stat_disc$trait[stat_disc$trait=="HPT"] <- "TAR"

stat_disc$exposure <- gsub("otc_","",stat_disc$exposure)
stat_disc$exposure <- gsub("_"," & ",stat_disc$exposure)
stat_disc$exposure[stat_disc$exposure=="ObesOverwe"] <- "Obese & Overweight"


#' @:clustering
a <- reshape2::dcast(stat_disc[stat_disc$time=="Daytime",], exposure~trait, value.var = "beta")
order_exp <- a$exposure[hclust(dist(a[,-1]), method = "ward.D2")$order]
b <- reshape2::dcast(stat_disc[stat_disc$time=="Daytime",], trait~exposure, value.var = "beta")
order_otc <- b$trait[hclust(dist(b[,-1]), method = "ward.D2")$order]


#' @:sort
input_data <- stat_disc[order(stat_disc$beta, decreasing=F),]
input_data$exposure <- factor(input_data$exposure, levels = rev(order_exp))
input_data$trait <- factor(input_data$trait, levels = c(setdiff(order_otc, c("TAR","TIR","TBR")),"TAR","TIR","TBR"))


# plot ---------------------------
plot1 <- ggplot(input_data, aes(x=trait, y=exposure))+
  geom_tile(aes(fill=beta), height=0.9, width=0.9)+
  geom_text(aes(label=sig_q), size=6)+
  scale_fill_continuous_diverging(palette = "Blue-Red 3", rev=F)+
  # scale_fill_continuous_sequential(palette = "Blue-Red 3", begin=0, end=1, rev = T)+
  # ArmyRose, Earth, Fall, Geyser, TealRose, Temps, Tropic, PuOr, RdBu, RdGy, PiYG, PRGn, BrBG, RdYlBu, RdYlGn, Spectral, Zissou 1, Cividis, Roma

  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  labs(title="LADE and daily glycemic traits\n(GNHS, all samples)", x=NULL, y=NULL, fill="Standardized beta coefficient")+
  facet_grid(.~time, scales = "free", space = "free")+

  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, angle = 0),
    strip.text.y = element_text(size = 15, angle = -90, hjust=0.5, vjust=0),
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing.y = unit(0,"mm"),
    panel.spacing.x = unit(5,"mm"),

    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),

    panel.grid.major = element_line(color = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),

    # legend
    legend.position = "right",
    legend.title.position = "right",
    legend.title = element_text(size = 15, angle = -90, hjust = 0.5),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.height = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),

    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black")
  )
plot1


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple2_LADE_daily_heat.pdf", width = 10, height=10); plot1; dev.off()




#' @:LADE-Daily-triats [Longitudinal, non-diabetes, GNHS]
#' @:heatmap-plot
##------------------------------------------------------
# data -----------------------
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list <- c(paste0(a,"_mean_day"),paste0(a,"_mean_night"),
          paste0(b,"_mean_day.asin"),paste0(b,"_mean_night.asin"))

setwd(paste0(workpath, "/part1_disease"))
stat_disc <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_noT2D")
stat_disc <- stat_disc[stat_disc$outcome %in% list,] 

stat_disc$time <- NA 
stat_disc$time[grep("_day", stat_disc$outcome)] <- "Daytime"
stat_disc$time[grep("_night", stat_disc$outcome)] <- "Nighttime"

stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]})
stat_disc$trait[stat_disc$trait=="J"] <- "J-index"
stat_disc$trait[stat_disc$trait=="LPT"] <- "TBR"
stat_disc$trait[stat_disc$trait=="MPT"] <- "TIR"
stat_disc$trait[stat_disc$trait=="HPT"] <- "TAR"

stat_disc$exposure <- gsub("otc_","",stat_disc$exposure)
stat_disc$exposure <- gsub("_"," & ",stat_disc$exposure)
stat_disc$exposure[stat_disc$exposure=="ObesOverwe"] <- "Obese & Overweight"


#' @:clustering
a <- reshape2::dcast(stat_disc[stat_disc$time=="Daytime",], exposure~trait, value.var = "beta")
order_exp <- a$exposure[hclust(dist(a[,-1]), method = "ward.D2")$order]
b <- reshape2::dcast(stat_disc[stat_disc$time=="Daytime",], trait~exposure, value.var = "beta")
order_otc <- b$trait[hclust(dist(b[,-1]), method = "ward.D2")$order]


#' @:sort
input_data <- stat_disc[order(stat_disc$beta, decreasing=F),]
input_data$exposure <- factor(input_data$exposure, levels = rev(order_exp))
input_data$trait <- factor(input_data$trait, levels = c(setdiff(order_otc, c("TAR","TIR","TBR")),"TAR","TIR","TBR"))


# plot ---------------------------
plot1 <- ggplot(input_data, aes(x=trait, y=exposure))+
  geom_tile(aes(fill=beta), height=0.9, width=0.9)+
  geom_text(aes(label=sig_q), size=6)+
  scale_fill_continuous_diverging(palette = "Vik", rev=F)+
  # Blue-Red, Blue-Red 2, Blue-Red 3, Red-Green, Purple-Green, Purple-Brown, Green-Brown, Blue-Yellow 2, Blue-Yellow 3, Green-Orange, 
  # Cyan-Magenta, Tropic, Broc, Cork, Vik, Berlin, Lisbon, Tofino
  
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  labs(title="LADE and daily glycemic traits\n(GNHS, Non-diabetic subgroup)", x=NULL, y=NULL, fill="Standardized beta coefficient")+
  facet_grid(.~time, scales = "free", space = "free")+

  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, angle = 0),
    strip.text.y = element_text(size = 15, angle = -90, hjust=0.5, vjust=0),
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing.y = unit(0,"mm"),
    panel.spacing.x = unit(5,"mm"),

    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),

    panel.grid.major = element_line(color = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),

    # legend
    legend.position = "right",
    legend.title.position = "right",
    legend.title = element_text(size = 15, angle = -90, hjust = 0.5),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.height = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),

    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black")
  )
plot1


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple2_LADE_daily_noT2D_heat.pdf", width = 10, height=10); plot1; dev.off()




#' @:Cumulative-disease-burden [all sample, discovery & validation]
#' @:heatmap-plot
setwd(paste0(workpath, "/part1_disease"))
##------------------------------------------------------
# data -----------------------
#' @:Discovery
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list_daily <- c(paste0(a, "_mean_day"),paste0(a, "_mean_night"),
                paste0(b, "_mean_day.asin"),paste0(b, "_mean_night.asin"))

stat_disc <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_all")
stat_disc <- stat_disc[stat_disc$outcome %in% list_daily,] 
stat_disc$time <- NA
stat_disc$time[grep("day", stat_disc$outcome)] <- "Daytime"
stat_disc$time[grep("night", stat_disc$outcome)] <- "Nighttime"

stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]})
stat_disc$trait[stat_disc$trait=="J"] <- "J-index"

stat_disc$exposure <- gsub("otc_","",stat_disc$exposure)
stat_disc$exposure <- gsub("_"," & ",stat_disc$exposure)
stat_disc$exposure[stat_disc$exposure=="ObesOverwe"] <- "Obese & Overweight"
list_disease <- unique(stat_disc$exposure)


#' @:Validation
stat_valid <- openxlsx::read.xlsx("valid_Daily_LADE_linear.xlsx", sheet = "stat_all")
stat_valid$exposure <- gsub("otc_","",stat_valid$exposure)
stat_valid$exposure <- gsub("_"," & ",stat_valid$exposure)
stat_valid$exposure[stat_valid$exposure=="ObesOverwe"] <- "Obese & Overweight"

list <- c(paste0(a,"_day"),paste0(a,"_night"),
          paste0(b,"_day.asin"),paste0(b,"_night.asin"))
stat_valid <- stat_valid[stat_valid$outcome %in% list,] 
stat_valid$time <- NA
stat_valid$time[grep("day", stat_valid$outcome)] <- "Daytime"
stat_valid$time[grep("night", stat_valid$outcome)] <- "Nighttime"

stat_valid$trait <- sapply(stat_valid$outcome, function(x){strsplit(x, split = "_")[[1]][1]})
stat_valid$trait[stat_valid$trait=="J"] <- "J-index"
list_disease.valid <- unique(stat_valid$exposure)


#' @:merge
stat_sort <- openxlsx::read.xlsx("valid_Daily_LADE_linear.xlsx", sheet = "sort")
stat_sort <- stat_sort[stat_sort$outcome %in% list,]

list_merge <- intersect(list_disease, list_disease.valid)
database <- rbind(data.frame(group="Discovery", stat_disc[stat_disc$exposure %in% list_merge,]),
                  data.frame(group="Validation", stat_valid[stat_valid$exposure %in% list_merge,]))
database <- database[database$exposure!="CHD & MI",]
database$trait[database$trait=="LPT"] <- "TBR"
database$trait[database$trait=="MPT"] <- "TIR"
database$trait[database$trait=="HPT"] <- "TAR"


# plot ----------------------------
plot_forest <- function(data, target, colors){
  input_data <- data[data$exposure==target,]
  input_data <- input_data[order(input_data$beta, decreasing=F),]
  input_data <- input_data[order(input_data$time, decreasing=F),]
  
  input_data$fill <- ifelse(input_data$pval<0.05, input_data$time, "Insig")
  input_data$trait <- factor(input_data$trait, levels = unique(input_data$trait))
  input_data$time <- factor(input_data$time, levels = rev(c("Nighttime","Daytime")))
  input_data$fill <- factor(input_data$fill, levels = rev(c("Nighttime","Daytime","Insig")))
  

 plot <- ggplot(input_data, aes(x = beta, y=trait)) +
    geom_vline(xintercept = 0, linewidth=0.5, linetype="dashed", color="grey80")+
    geom_errorbar(aes(xmin = beta-1.96*se, xmax = beta+1.96*se, color=time), width = 0.6, position = position_dodge(0.5)) + # 误差线图层
    geom_point(aes(color=time, fill=fill), shape=21, alpha=1, size=4, position = position_dodge(0.5)) + # 点图层

    scale_color_manual(values = colors)+
    scale_fill_manual(values = c("white",colors))+
    # scale_x_continuous(limits = c(-1,13),breaks = seq(0,12,3))+

    ggtitle(label = target)+
    labs(color=NULL, fill=NULL, y=NULL, x="Standardized beta coefficient")+
    facet_wrap(.~group, ncol=7, scales = "free_x")+

    theme_bw()+
    theme(
      # for facet
      strip.text.x = element_text(size = 15, face = "plain", angle = 0),
      strip.background = element_rect(color = "transparent", fill= "transparent"),
      panel.spacing = unit(0,"mm"),

      # for ggtitle
      plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),

      panel.grid.major = element_line(color = NA),
      panel.background = element_rect(fill = "transparent",color = NA),
      plot.background = element_rect(fill = "transparent",color = NA),
      panel.grid.minor = element_blank(),

      # legend
      legend.position = c(0.15,0.9),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 15),
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
      legend.box.background = element_rect(fill = "transparent", color = "transparent"),

      # for axis
      axis.ticks.length = unit(3,"mm"),
      axis.title.x = element_text(face = "plain", color = "black"),
      axis.title.y = element_text(face = "plain", color = "black"),
      axis.title = element_text(size=15),

      axis.text.y = element_text(angle=0, face = "plain", hjust=1, vjust=0.5, color = "black"),
      axis.text.x = element_text(angle=0, face = "plain", hjust=0.5, vjust=0.5, color = "black"),
      axis.text = element_text(size=10)
    )
  return(plot)
}

plot_T2D <- plot_forest(data = database, target = "T2D", colors=Archambault[c(1,2)]); plot_T2D
plot_dys <- plot_forest(data = database, target = "Dyslipidemia", colors=Archambault[c(3,4)]); plot_dys
plot_obes <- plot_forest(data = database, target = "Obese & Overweight", colors=Archambault[c(5,6)]); plot_obes
plot_hyperten <- plot_forest(data = database, target = "Hypertension", colors=Archambault[c(7,8)]); plot_hyperten

plot_merge <- aplot::insert_right(plot_T2D, plot_dys); plot_merge
plot_merge <- aplot::insert_right(plot_merge, plot_obes); plot_merge
plot_merge <- aplot::insert_right(plot_merge, plot_hyperten); plot_merge

##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple2_LADE_daily_valid_forest.pdf", width = 18, height=10); plot_merge; dev.off()




#' @:Protein-CGMdaily [Associations of MMI-proteins and CGM daily mapping]
#' @:heatmap-plot
setwd(paste0(workpath, "/part5_proteomics"))
##------------------------------------------------------
# data -----------------------
#' @:Discovery
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list_daily <- c(paste0(a, "_mean_day"),paste0(a, "_mean_night"),
                paste0(b, "_mean_day.asin"),paste0(b, "_mean_night.asin"))

stat_disc <- openxlsx::read.xlsx("stat_Proteomics_CGMdaily_linear.xlsx", sheet = "stat_all")
stat_disc <- stat_disc[stat_disc$outcome %in% list_daily,] 
stat_disc$time <- NA
stat_disc$time[grep("day", stat_disc$outcome)] <- "Daytime"
stat_disc$time[grep("night", stat_disc$outcome)] <- "Nighttime"

stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]})
stat_disc$trait[stat_disc$trait=="J"] <- "J-index"


#' @:sorting
a <- reshape2::dcast(stat_disc[stat_disc$time=="Daytime",], formula = "SYMBOL~trait", value.var = "beta")
rownames(a) <- a$SYMBOL
a <- a[,-1]
order_prot <- rownames(a)[hclust(dist(as.matrix(a)), method = "complete")$order]

b <- reshape2::dcast(stat_disc[stat_disc$time=="Daytime",], formula = "trait~SYMBOL", value.var = "beta")
rownames(b) <- b$trait
b <- b[,-1]
order_trait <- rownames(b)[hclust(dist(as.matrix(b)))$order]



# plot ----------------------------
input_data <- stat_disc
input_data$label <- ifelse(input_data$qval<0.05, input_data$sig_q, "")
input_data$SYMBOL <- factor(input_data$SYMBOL, levels=order_prot)
input_data$trait <- factor(input_data$trait, levels=order_trait)


plot1 <- ggplot(input_data, aes(x=SYMBOL, y=trait))+
  geom_tile(aes(fill=beta), height=0.9, width=0.9)+
  geom_text(aes(label=sig_q), size=6)+
  scale_fill_continuous_divergingx(palette = "RdBu", rev=T)+
  # ArmyRose, Earth, Fall, Geyser, TealRose, Temps, Tropic, PuOr, RdBu, RdGy, PiYG, PRGn, BrBG, RdYlBu, RdYlGn, Spectral, Zissou 1, Cividis, Roma
  
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  labs(title="Associations of MMI-related proteins and CGM-measured daily glycemic traits", x=NULL, y=NULL, fill="Standardized beta coefficient")+
  facet_grid(time~., scales = "free", space = "free")+
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, angle = 0),
    strip.text.y = element_text(size = 15, angle = -90, hjust=0.5, vjust=0),
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing.y = unit(5,"mm"),
    panel.spacing.x = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "right",
    legend.title.position = "right",
    legend.title = element_text(size = 15, angle = -90, hjust = 0.5),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.height = unit(3,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black")
  )
plot1


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple4_protein_CGMdaily_heat.pdf", width = 35, height=7); plot1; dev.off()




#' @:Protein-CGMmeal [Associations of MMI-proteins and CGM meal traits mapping]
#' @:heatmap-plot
setwd(paste0(workpath, "/part5_proteomics"))
##------------------------------------------------------
# data -----------------------
#' @:Discovery 
stat_disc <- openxlsx::read.xlsx("stat_Proteomics_Mealtraits_linear.xlsx", sheet = "stat_all") 
stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]}) 


#' @:sorting
a <- reshape2::dcast(stat_disc[stat_disc$meal=="RG",], formula = "SYMBOL~trait", value.var = "beta")
rownames(a) <- a$SYMBOL
a <- a[,-1]
order_prot <- rownames(a)[hclust(dist(as.matrix(a)), method = "complete")$order]

b <- reshape2::dcast(stat_disc[stat_disc$meal=="RG",], formula = "trait~SYMBOL", value.var = "beta")
rownames(b) <- b$trait
b <- b[,-1]
order_trait <- rownames(b)[hclust(dist(as.matrix(b)))$order]



# plot ----------------------------
input_data <- stat_disc
input_data$label <- ifelse(input_data$qval<0.05, input_data$sig_q, "")
input_data$SYMBOL <- factor(input_data$SYMBOL, levels=order_prot)
input_data$trait <- factor(input_data$trait, levels=order_trait, labels = c("MPG","IR","iAUC","iPMG"))


plot1 <- ggplot(input_data, aes(x=SYMBOL, y=trait))+
  geom_tile(aes(fill=beta), height=0.9, width=0.9)+
  geom_text(aes(label=sig_q), size=6)+
  scale_fill_continuous_divergingx(palette = "RdGy", rev=T)+
  # ArmyRose, Earth, Fall, Geyser, TealRose, Temps, Tropic, PuOr, RdBu, RdGy, PiYG, PRGn, BrBG, RdYlBu, RdYlGn, Spectral, Zissou 1, Cividis, Roma
  
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  labs(title="Associations of MMI-related proteins and CGM-measured daily glycemic traits", x=NULL, y=NULL, fill="Standardized beta coefficient")+
  facet_grid(meal~., scales = "free", space = "free")+
  
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, angle = 0),
    strip.text.y = element_text(size = 15, angle = -90, hjust=0.5, vjust=0),
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing.y = unit(5,"mm"),
    panel.spacing.x = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "right",
    legend.title.position = "right",
    legend.title = element_text(size = 15, angle = -90, hjust = 0.5),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.height = unit(3,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black")
  )
plot1


##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple4_protein_CGMmeal_heat.pdf", width = 35, height=5.5); plot1; dev.off()




#' @:Protein-CGM-merge [Associations of MMI-proteins and CGM all traits mapping]
#' @:Circle-heatmap-plot
library(circlize)
library(wesanderson)
library(ComplexHeatmap)
##------------------------------------------------------
# data -----------------------
#' @:Daily
setwd(paste0(workpath, "/part5_proteomics"))
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list_daily <- c(paste0(a, "_mean_day"),paste0(a, "_mean_night"),
                paste0(b, "_mean_day.asin"),paste0(b, "_mean_night.asin"))

stat_disc <- openxlsx::read.xlsx("stat_Proteomics_CGMdaily_linear.xlsx", sheet = "stat_all")
stat_disc <- stat_disc[stat_disc$outcome %in% list_daily,] 
stat_disc$time <- NA
stat_disc$time[grep("day", stat_disc$outcome)] <- "Daytime"
stat_disc$time[grep("night", stat_disc$outcome)] <- "Nighttime"

stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]})
stat_disc$trait[stat_disc$trait=="J"] <- "J-index"
stat_disc.daily <- stat_disc


#' @:Daily-sorting
a <- reshape2::dcast(stat_disc[stat_disc$time=="Daytime",], formula = "SYMBOL~trait", value.var = "beta")
rownames(a) <- a$SYMBOL
a <- a[,-1]
order_prot <- rownames(a)[hclust(dist(as.matrix(a)), method = "complete")$order]

b <- reshape2::dcast(stat_disc[stat_disc$time=="Daytime",], formula = "trait~SYMBOL", value.var = "beta")
rownames(b) <- b$trait
b <- b[,-1]
order_trait.daily <- rownames(b)[hclust(dist(as.matrix(b)))$order]


#' @:Meal 
stat_disc <- openxlsx::read.xlsx("stat_Proteomics_Mealtraits_linear.xlsx", sheet = "stat_all") 
stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]}) 
stat_disc.meal <- stat_disc
stat_disc.meal$trait[stat_disc.meal$trait=="acc"] <- "IR"
stat_disc.meal$trait[stat_disc.meal$trait=="ppge"] <- "iMPG"
stat_disc.meal$trait[stat_disc.meal$trait=="peak"] <- "MPG"


# plot (Daytime) ----------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple4_protein_CGMdaily_Day_circle.pdf", width = 10, height=10)

#' @:Daytime
day <- reshape2::dcast(stat_disc.daily[stat_disc.daily$time=="Daytime",], formula = "SYMBOL~trait", value.var = "beta")
night <- reshape2::dcast(stat_disc.daily[stat_disc.daily$time=="Nighttime",], formula = "SYMBOL~trait", value.var = "beta")

#' @:beta
input_data <- day
input_data2 <- data.frame()
for(i in order_prot){input_data2 <- rbind.fill(input_data2, input_data[input_data$SYMBOL==i,])}  
rownames(input_data2) <- input_data2$SYMBOL
input_data2 <- as.matrix(input_data2[,-1])  

#' @:significance
sig <- reshape2::dcast(stat_disc.daily[stat_disc.daily$time=="Daytime",], formula = "SYMBOL~trait", value.var = "sig_q")
sig[is.na(sig)] <- "" 
sig[sig=="**"] <- "#"
sig2 <- data.frame()
for(i in order_prot){sig2 <- rbind.fill(sig2, sig[sig$SYMBOL==i,])}  
rownames(sig2) <- sig2$SYMBOL
sig2 <- as.matrix(sig2[,-1])  
order_trait <- colnames(sig2)


#' @:Painting
circos.clear()  
col_fun1 = colorRamp2(c(seq(-0.25,0,0.25/6), seq(0,0.25,0.25/6)[-1]), c(Benedictus))##设置热图颜色 
# split <- p_g_all$group
# table(split)

circos.par(start.degree = -45, gap.degree = 10, gap.after = c(90))##为添加列名留出空间
circos.heatmap(input_data2, col= col_fun1,
               cluster = F,
               rownames.side = "outside",##行名在圈外
               bg.border= "transparent", # 背景边框的颜色, 
               # rownames.cex = 0.4, 
               track.height = 0.25, cell_width = 0.01, rownames.cex = 0.8, show.sector.labels = F)##行名的大小


# add legend
lgd = Legend(title = "Daytime Traits\nBeta coefficient", col_fun = col_fun1)
grid.draw(lgd)

# set up group track names
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
  if(CELL_META$sector.numeric.index == 1) { # the last sector
    
    order_trait <- rev(order_trait)
    for(i in 1:length(order_trait)){
      circos.text(0, i-0.5, order_trait[i], facing = "clockwise", cex = 1)
      
    }
     
    # add significance after FDR
    for (i in 1:nrow(sig2)){
      for(j in 1:ncol(sig2)){
        circos.text(i-0.5, ncol(sig2)-j+0.5, sig2[i,j], facing = "clockwise", cex = 0.6)
      } 
    }
  }
})
circos.clear()
dev.off()


# plot (Nighttime) ----------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple4_protein_CGMdaily_Night_circle.pdf", width = 10, height=10)

#' @:Daytime
day <- reshape2::dcast(stat_disc.daily[stat_disc.daily$time=="Daytime",], formula = "SYMBOL~trait", value.var = "beta")
night <- reshape2::dcast(stat_disc.daily[stat_disc.daily$time=="Nighttime",], formula = "SYMBOL~trait", value.var = "beta")

#' @:beta
input_data <- night
input_data2 <- data.frame()
for(i in order_prot){input_data2 <- rbind.fill(input_data2, input_data[input_data$SYMBOL==i,])}  
rownames(input_data2) <- input_data2$SYMBOL
input_data2 <- as.matrix(input_data2[,-1])  

#' @:significance
sig <- reshape2::dcast(stat_disc.daily[stat_disc.daily$time=="Nighttime",], formula = "SYMBOL~trait", value.var = "sig_q")
sig[is.na(sig)] <- "" 
sig[sig=="**"] <- "#"
sig2 <- data.frame()
for(i in order_prot){sig2 <- rbind.fill(sig2, sig[sig$SYMBOL==i,])}  
rownames(sig2) <- sig2$SYMBOL
sig2 <- as.matrix(sig2[,-1])  
order_trait <- colnames(sig2)


#' @:Painting
circos.clear()  
col_fun1 = colorRamp2(c(seq(-0.25,0,0.25/6), seq(0,0.25,0.25/6)[-1]), c(Benedictus))##设置热图颜色 
# split <- p_g_all$group
# table(split)

circos.par(start.degree = -45, gap.degree = 10, gap.after = c(90))##为添加列名留出空间
circos.heatmap(input_data2, col= col_fun1,
               cluster = F,
               rownames.side = "outside",##行名在圈外
               bg.border= "transparent", # 背景边框的颜色, 
               # rownames.cex = 0.4, 
               track.height = 0.25, cell_width = 0.01, rownames.cex = 0.8, show.sector.labels = F)##行名的大小


# add legend
lgd = Legend(title = "Nighttime Traits\nBeta coefficient", col_fun = col_fun1)
grid.draw(lgd)

# set up group track names
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
  if(CELL_META$sector.numeric.index == 1) { # the last sector
    
    order_trait <- rev(order_trait)
    for(i in 1:length(order_trait)){
      circos.text(0, i-0.5, order_trait[i], facing = "clockwise", cex = 1)
      
    }
    
    # add significance after FDR
    for (i in 1:nrow(sig2)){
      for(j in 1:ncol(sig2)){
        circos.text(i-0.5, ncol(sig2)-j+0.5, sig2[i,j], facing = "clockwise", cex = 0.6)
      } 
    }
  }
})
circos.clear()
dev.off()


# plot (Meal-RG) ----------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple4_protein_CGMmeal_RG_circle.pdf", width = 10, height=10)

#' @:Daytime
RG <- reshape2::dcast(stat_disc.meal[stat_disc.meal$meal=="RG",], formula = "SYMBOL~trait", value.var = "beta")
WG <- reshape2::dcast(stat_disc.meal[stat_disc.meal$meal=="WG",], formula = "SYMBOL~trait", value.var = "beta")

#' @:beta
input_data <- RG
input_data2 <- data.frame()
for(i in order_prot){input_data2 <- rbind.fill(input_data2, input_data[input_data$SYMBOL==i,])}  
rownames(input_data2) <- input_data2$SYMBOL
input_data2 <- as.matrix(input_data2[,-1])  

#' @:significance
sig <- reshape2::dcast(stat_disc.meal[stat_disc.meal$meal=="RG",], formula = "SYMBOL~trait", value.var = "sig_q")
sig[is.na(sig)] <- "" 
sig[sig=="**"] <- "#"
sig2 <- data.frame()
for(i in order_prot){sig2 <- rbind.fill(sig2, sig[sig$SYMBOL==i,])}  
rownames(sig2) <- sig2$SYMBOL
sig2 <- as.matrix(sig2[,-1])  
order_trait <- colnames(sig2)


#' @:Painting
circos.clear()  
col_fun1 = colorRamp2(c(seq(-0.25,0,0.25/6), seq(0,0.25,0.25/6)[-1]), c(Benedictus))##设置热图颜色 
# split <- p_g_all$group
# table(split)

circos.par(start.degree = -45, gap.degree = 10, gap.after = c(90))##为添加列名留出空间
circos.heatmap(input_data2, col= col_fun1,
               cluster = F,
               rownames.side = "outside",##行名在圈外
               bg.border= "transparent", # 背景边框的颜色, 
               # rownames.cex = 0.4, 
               track.height = 0.15, cell_width = 0.01, rownames.cex = 0.8, show.sector.labels = F)##行名的大小


# add legend
lgd = Legend(title = "RG postprandial traits\nBeta coefficient", col_fun = col_fun1)
grid.draw(lgd)

# set up group track names
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
  if(CELL_META$sector.numeric.index == 1) { # the last sector
    
    order_trait <- rev(order_trait)
    for(i in 1:length(order_trait)){
      circos.text(0, i-0.5, order_trait[i], facing = "clockwise", cex = 1)
      
    }
    
    # add significance after FDR
    for (i in 1:nrow(sig2)){
      for(j in 1:ncol(sig2)){
        circos.text(i-0.5, ncol(sig2)-j+0.5, sig2[i,j], facing = "clockwise", cex = 0.6)
      } 
    }
  }
})
circos.clear()
dev.off()


# plot (Meal-WG) ----------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple4_protein_CGMmeal_WG_circle.pdf", width = 10, height=10)

#' @:Daytime
RG <- reshape2::dcast(stat_disc.meal[stat_disc.meal$meal=="RG",], formula = "SYMBOL~trait", value.var = "beta")
WG <- reshape2::dcast(stat_disc.meal[stat_disc.meal$meal=="WG",], formula = "SYMBOL~trait", value.var = "beta")

#' @:beta
input_data <- WG
input_data2 <- data.frame()
for(i in order_prot){input_data2 <- rbind.fill(input_data2, input_data[input_data$SYMBOL==i,])}  
rownames(input_data2) <- input_data2$SYMBOL
input_data2 <- as.matrix(input_data2[,-1])  

#' @:significance
sig <- reshape2::dcast(stat_disc.meal[stat_disc.meal$meal=="WG",], formula = "SYMBOL~trait", value.var = "sig_q")
sig[is.na(sig)] <- "" 
sig[sig=="**"] <- "#"
sig2 <- data.frame()
for(i in order_prot){sig2 <- rbind.fill(sig2, sig[sig$SYMBOL==i,])}  
rownames(sig2) <- sig2$SYMBOL
sig2 <- as.matrix(sig2[,-1])  
order_trait <- colnames(sig2)


#' @:Painting
circos.clear()  
col_fun1 = colorRamp2(c(seq(-0.25,0,0.25/6), seq(0,0.25,0.25/6)[-1]), c(Benedictus))##设置热图颜色 
# split <- p_g_all$group
# table(split)

circos.par(start.degree = -45, gap.degree = 10, gap.after = c(90))##为添加列名留出空间
circos.heatmap(input_data2, col= col_fun1,
               cluster = F,
               rownames.side = "outside",##行名在圈外
               bg.border= "transparent", # 背景边框的颜色, 
               # rownames.cex = 0.4, 
               track.height = 0.15, cell_width = 0.01, rownames.cex = 0.8, show.sector.labels = F)##行名的大小


# add legend
lgd = Legend(title = "RG postprandial traits\nBeta coefficient", col_fun = col_fun1)
grid.draw(lgd)

# set up group track names
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
  if(CELL_META$sector.numeric.index == 1) { # the last sector
    
    order_trait <- rev(order_trait)
    for(i in 1:length(order_trait)){
      circos.text(0, i-0.5, order_trait[i], facing = "clockwise", cex = 1)
      
    }
    
    # add significance after FDR
    for (i in 1:nrow(sig2)){
      for(j in 1:ncol(sig2)){
        circos.text(i-0.5, ncol(sig2)-j+0.5, sig2[i,j], facing = "clockwise", cex = 0.6)
      } 
    }
  }
})
circos.clear()
dev.off()


##------------------------------------------------------




#' @:Protein-CGM-merge [Associations of MMI-proteins and CGM all traits mapping]
#' @:Scatter 
##------------------------------------------------------
# data -----------------------
#' @:Daily
setwd(paste0(workpath, "/part5_proteomics"))
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list_daily <- c(paste0(a, "_mean_day"),paste0(a, "_mean_night"),
                paste0(b, "_mean_day.asin"),paste0(b, "_mean_night.asin"))

stat_disc <- openxlsx::read.xlsx("stat_Proteomics_CGMdaily_linear.xlsx", sheet = "stat_all")
stat_disc <- stat_disc[stat_disc$outcome %in% list_daily,] 
stat_disc$time <- NA
stat_disc$time[grep("day", stat_disc$outcome)] <- "Daytime"
stat_disc$time[grep("night", stat_disc$outcome)] <- "Nighttime"

stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]})
stat_disc$trait[stat_disc$trait=="J"] <- "J-index"
stat_disc.daily <- stat_disc

#' @:Daily-correlation
list <- unique(stat_disc.daily$trait)
output <- data.frame()
for(i in list){
  day <- input_data[stat_disc.daily$trait==i & stat_disc.daily$time=="Daytime","beta"]
  night <- input_data[stat_disc.daily$trait==i & stat_disc.daily$time=="Nighttime","beta"]
  r <- rcorr(day, night, type = "spearman")$r[2,1]
  p <- rcorr(day, night, type = "spearman")$P[2,1]
  result <- data.frame(trait=i, r=r, pval=p)
  output <- rbind.fill(output, result)
}
output.daily <- data.frame(group="daily", output)
output.daily$qval <- p.adjust(output.daily$pval, method = "BH")


#' @:Meal 
stat_disc <- openxlsx::read.xlsx("stat_Proteomics_Mealtraits_linear.xlsx", sheet = "stat_all") 
stat_disc$trait <- sapply(stat_disc$outcome, function(x){strsplit(x, split = "_")[[1]][1]}) 
stat_disc.meal <- stat_disc

#' @:Daily-correlation
list <- unique(stat_disc.meal$trait)
output <- data.frame()
for(i in list){
  rg <- input_data[stat_disc.meal$trait==i & stat_disc.meal$meal=="RG","beta"]
  wg <- input_data[stat_disc.meal$trait==i & stat_disc.meal$meal=="WG","beta"]
  r <- rcorr(rg, wg, type = "spearman")$r[2,1]
  p <- rcorr(rg, wg, type = "spearman")$P[2,1]
  result <- data.frame(trait=i, r=r, pval=p)
  output <- rbind.fill(output, result)
}
output.meal <- data.frame(group="meal", output)
output.meal$qval <- p.adjust(output.meal$pval, method = "BH")


# stat ------------------------------
input_data <- rbind(output.daily, output.meal)
input_data$trait[input_data$trait=="LPT"] <- "TBR"
input_data$trait[input_data$trait=="MPT"] <- "TIR"
input_data$trait[input_data$trait=="HPT"] <- "TAR"
input_data$trait[input_data$trait=="acc"] <- "IR"
input_data$trait[input_data$trait=="ppge"] <- "iMPG"
input_data$trait[input_data$trait=="peak"] <- "MPG"
input_data <- input_data[order(input_data$r, decreasing = F),]
input_data <- input_data[order(input_data$group, decreasing = F),]

input_data$trait <- factor(input_data$trait, levels = input_data$trait)
input_data$tag <- ifelse(input_data$qval<0.05,"**",NA)
input_data$position <- ifelse(input_data$r>0, input_data$r+0.1, input_data$r-0.1)
input_data$group <- factor(input_data$group, levels = c("daily","meal"), labels = 
                             c("Daily glycemic traits\n(Daytime & Nighttime)", 
                               "Postprandial dietary responses\n(RG & WG)"))


plot1 <- ggplot(data=input_data, aes(x=trait, y=r))+
  geom_hline(yintercept = 0, linewidth=0.5, color="grey80")+
  geom_bar(aes(fill=trait), color="grey80", stat = "identity", width = 0.7, position = position_dodge(0.7), alpha=0.5)+
  geom_text(aes(x=trait, y=position, label=tag), size=6)+
  
  # scale_x_discrete(expand = c(0,0))+
  # scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c(Benedictus,"grey"))+
  labs(fill=NULL, title="Consistency of MMI-protein-associated patterns", x=NULL, y="Spearman rho")+
  facet_grid(.~group, scales = "free", space = "free")+
  
  theme_bw()+
  theme(
    # for facet
    strip.text = element_text(size = 15, angle = 0),
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(5,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black")
  )
plot1



##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple4_protein_CGMmerge_corr_bar.pdf", width = 12, height=5.5); plot1; dev.off()




#' @:Proteins-CrossOrgan [Enrichment analysis]
#' @:boxplot  
setwd(paste0(workpath, "/part5_proteomics"))
##-----------------------------------------------------------
# data --------------------------------------- 
#' @:GNHS
database <- openxlsx::read.xlsx("stat_Proteomics_CrossOrgan.xlsx", sheet = "data")
database$MMI <- factor(database$MMI, levels = c("other","MMI"), labels = c("Other","MMI"))


# plot1 ----------------------------------------

plot1 <- ggplot(data=database, aes(x=MMI, y=Gobal.label.score..GLS., fill=MMI))+  
  geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  geom_violin(trim = FALSE) +
  geom_boxplot(width=0.1, position = position_dodge(0.4), fill="white", color="black", alpha=0.5, outlier.alpha = 0)+
  
  stat_compare_means( 
    comparisons = list(c(1,2)), 
    method = "wilcox.test", paired = FALSE,
    label="p.format", #p.format p.signif 表示* 
    size = 6,
    hide.ns = T
  )+
  
  # scale_y_continuous(expand = c(0.1,0.1))+ 
  scale_fill_manual(values = c(OKeeffe1[c(7,10)]))+
  labs(title=NULL, fill=NULL, x=NULL, y="Global label score (GLS)")+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),    
    axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot1

 
##------------------------------------------------------
setwd(paste0(workpath,"/part0_figs"))
pdf("supple4_MMIprotein_CrossOrgan_enrich_box.pdf", width = 5, height=7); plot1; dev.off()








