

sort_validation <- function(data, list_exp, list_otc){ 
  data$validation <- NA
  
  for(otc in list_otc){
    for(exp in list_exp){ 
      submatr <- data[data$exposure==exp & data$outcome==otc,]
      
      if(submatr$beta[1]*submatr$beta[2]>0 & sum(!is.na(submatr$sig_p))==2){
        data$validation[data$exposure==exp & data$outcome==otc] <- "valid"
      }
    }  
  }
  
  data <- data[order(data$group, decreasing = F),]
  data <- data[order(data$outcome, decreasing = F),]
  data <- data[order(data$validation, decreasing = F),]
  
  return(data)
}