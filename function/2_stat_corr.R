

stat_corr <- function(data, list1, list2, method){
  
  list <- unique(c(list1,list2))
  stat <- rcorr(as.matrix(data[,list]), type = method) 
  
  r <- data.frame(reshape2::melt(stat$r)); names(r)[3] <- "r"
  p <- data.frame(reshape2::melt(stat$P)); names(p)[3] <- "pval"
  n <- data.frame(reshape2::melt(stat$n)); names(n)[3] <- "n"
  
  output <- merge(r,p, by=c("Var1","Var2"), all=F) 
  output <- merge(output, n, by=c("Var1","Var2"), all=F) 
  
  #' @:filtering
  output <- output[output$Var1 %in% list1,]
  output <- output[output$Var2 %in% list2,]
  output$method <- method
  output$Var1 <- as.character(output$Var1)
  output$Var2 <- as.character(output$Var2)
  
  output$sig_p <- ifelse(output$pval<0.05,"*",NA)
  output$sig_p[output$pval<0.01] <- "**"
  output$sig_p[output$pval<0.001] <- "***"
  table(output$Var1, output$sig_p)
  
  #' @:FDR-correction
  output$qval <- NA
  for(trait in list1){
    output$qval[output$Var1==trait] <- p.adjust(output$pval[output$Var1==trait], method = "BH")
  }
  output$sig_q <- ifelse(output$qval<0.25 & output$pval<0.05,"*",NA)
  output$sig_q[output$qval<0.1 & output$pval<0.05] <- "**"
  output$sig_q[output$qval<0.05 & output$pval<0.05] <- "***"
  table(output$Var1, output$sig_q)
  
  output <- output[order(output$pval, decreasing = F),]
  output <- output[order(output$Var1, decreasing = F),]
  
  return(output)
} 