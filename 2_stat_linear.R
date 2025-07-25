


stat_linear <- function(data, list_exp, list_otc, list_covar, timepoint){
  
  output_linear <- data.frame()
  for(exp in list_exp){
    for(otc in list_otc){
      
      formula1 <- paste(otc,"~",exp)
      formula2 <- paste(list_covar, collapse=" + ")
      formula <- paste(formula1, formula2, sep=" + ")
      result <- summary(lm(as.formula(formula), data=data))  
      
      result1 <- data.frame(result$coefficients); names(result1) <- c("beta","se","t","pval")
      result1 <- result1[2,]
      result1 <- data.frame(outcome=otc, exposure=exp, timepoint="endpoint", rsquare_adj=result$adj.r.squared, result1, n=nrow(data), method="linear")
      
      #' @:Sorting 
      result_all <- result1
      result_all$sig_p <- ifelse(result_all$pval<0.05,"*",NA)
      result_all$sig_p[result_all$pval<0.01] <- "**"
      result_all$sig_p[result_all$pval<0.001] <- "***"
      
      output_linear <- rbind.fill(output_linear, result_all)
      
    } 
  }
  output_linear <- output_linear[order(output_linear$pval, decreasing = F),]
  output_linear <- output_linear[order(output_linear$outcome, decreasing = F),]
  return(output_linear) 
}  