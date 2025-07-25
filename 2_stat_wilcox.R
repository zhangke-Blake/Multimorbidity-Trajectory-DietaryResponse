

stat_wilcox <- function(data, group, trait){
  stat <- wilcox.test(data[,trait]~data[,group])
  
  mean <- aggregate(data[,trait], by=list(data[,group]), mean, na.rm=T); mean <- data.frame(t(mean))
  names(mean) <- paste0(mean[1,],"_mean"); mean <- mean[-1,] 
  sd <- aggregate(data[,trait], by=list(data[,group]), sd, na.rm=T); sd <- data.frame(t(sd))
  names(sd) <- paste0(sd[1,],"_sd"); sd <- sd[-1,] 
  
  result <- data.frame(trait=trait, pval=stat$p.value, mean, sd, method=stat$method)
  return(result)
} 
