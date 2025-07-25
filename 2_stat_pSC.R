
stat_pSC <- function(data, list1, list2, method){ 
  library(ppcor)
  
  data <- data[,c(list1, list2)]
  data <- na.omit(data)
  output <- data.frame()
  for(i in 1:length(list1)){ 
    
    result2 <- data.frame()
    list.rest <- setdiff(list1, list1[i])
    for(j in 1:length(list.rest)){
      
      feat1 <- data[,list1[i]]
      feat2 <- data[,list.rest[j]]
      covar <- data[,list2] 
      stat <- ppcor::pcor.test(x=feat1, y=feat2, z=covar, method = method)  # partial correlation 
      names(stat)[1:2] <- c("pSC","pval")
      result <- data.frame(feat1=list1[i], feat2=list.rest[j], stat, method=method, covar=paste(list2, collapse = ","))
      result2 <- rbind.fill(result2, result)
    } 
    output <- rbind.fill(output, result2)
  }
  
  output$sig_p <- ifelse(output$pval<0.05,"*",NA)
  output$sig_p[output$pval<0.01] <- "**"
  output$sig_p[output$pval<0.001] <- "***" 
  
  #' @:FDR-correction
  output$qval <- NA
  for(trait in list1){
    output$qval[output$feat1==trait] <- p.adjust(output$pval[output$feat1==trait], method = "BH")
  }
  output$sig_q <- ifelse(output$qval<0.25 & output$pval<0.05,"*",NA)
  output$sig_q[output$qval<0.1] <- "**"
  output$sig_q[output$qval<0.05] <- "***"
  table(output$feat1, output$sig_q)
  
  output <- output[order(output$pval, decreasing = F),]
  output <- output[order(output$feat1, decreasing = F),]
  
  return(output)
}