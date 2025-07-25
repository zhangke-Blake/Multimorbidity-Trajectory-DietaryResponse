

stat_mix_linear <- function(data, list_otc, list_covar, name_cluster, name_time, n_cluster, cluster_ref){ 
  library(lmerTest) 
  
  data[,name_cluster] <- as.factor(data[,name_cluster])
  data[,name_cluster] <- relevel(data[,name_cluster], ref = cluster_ref) 
  output_mix <- data.frame()
  
  for(d in list_otc){ 
    submatr <- data[!is.na(data[,d]),] 
    submatr$otc <- ifelse(submatr[,d]>0,1,0)
    
    if(sum(submatr$otc, na.rm=T)>0 & length(unique(submatr$duration))>1){ 
      formula1 <- paste0(d," ~ ", name_time,"*", name_cluster, " + ", name_cluster, " + ", name_time)
      formula2 <- paste(list_covar, collapse = " + ")
      formula <- paste(formula1, formula2,"(1|id)", sep = " + ")
      
      stat <- summary(lmerTest::lmer(as.formula(formula), data=submatr))
      result <- data.frame(stat$coefficients)[grep("duration:",rownames(stat$coefficients)),-4]; names(result) <- c("beta","se","df","pval")
      result1 <- data.frame(outcome=d, exposure=rownames(result), result, n=nrow(submatr), method="mixed-linear", ref=cluster_ref)
      output_mix <- rbind.fill(output_mix, result1)
    }  
  }
  
  output_mix$sig_p <- ifelse(output_mix$pval<0.05, "*", NA)
  output_mix$sig_p[output_mix$pval<0.01] <- "**"
  output_mix$sig_p[output_mix$pval<0.001] <- "***"
  
  output_mix <- output_mix[order(output_mix$pval, decreasing = F),]
  output_mix <- output_mix[order(output_mix$outcome, decreasing = F),]
  return(output_mix)
  
} 