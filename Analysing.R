




#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part0
#' @:Aging-CGMtraits [Aging-induced disruption in glycemic metabolism]
#' 
#' @:-----------------------------------------------------------------------------------------




if(!file.exists(paste0(workpath,"/part1_aging"))){
  dir.create(paste0(workpath,"/part1_aging")); print("Folder is built")
} else { print("Folder exists") }


a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list_daily <- c(paste0(a, "_mean_day"),paste0(a, "_mean_night"),
                paste0(b, "_mean_day.asin"),paste0(b, "_mean_night.asin"))
list_daily2 <- c(paste0(c(a,b), "_mean_day"),paste0(c(a,b), "_mean_night"))




#' @:Aging [Spearman correlation]
##-----------------------------------------------------------
# data ---------------------------------------  
setwd(paste0(root,"/1_data")) 
data_pheno.GNHS <- openxlsx::read.xlsx("GNHS_pheno_all.xlsx", sheet = 1)
data_pheno.GNHS <- data_pheno.GNHS[data_pheno.GNHS$visit=="F4",]

#' @:daily
data_CGM.daily <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "daily_mean")
data_CGM.daily <- data_CGM.daily[grep("NL",data_CGM.daily$id),] 
#' @:Meal-RG
data_CGM.RG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "RG_mean")
data_CGM.RG <- data_CGM.RG[grep("NL",data_CGM.RG$SampleID),]
list_RG <- setdiff(colnames(data_CGM.RG),"SampleID")
#' @:Meal-WG
data_CGM.WG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "WG_mean")
data_CGM.WG <- data_CGM.WG[grep("NL",data_CGM.WG$SampleID),]
list_WG <- setdiff(colnames(data_CGM.WG),"SampleID")
#' @:PGS
data_CGM.PGS <- openxlsx::read.xlsx("PGS_construction_GNHS.xlsx", sheet = 1)
data_CGM.PGS <- data_CGM.PGS[grep("NL",data_CGM.PGS$id),]

#' @:merge
database <- merge(data_CGM.PGS[,c("id","PGS")], data_CGM.RG, by.x = "id", by.y = "SampleID", all=T)
database <- merge(database, data_CGM.WG, by.x = "id", by.y = "SampleID", all=T)
database <- merge(database, data_CGM.daily, by.x = "id", by.y = "id", all=T) 
database <- merge(data_pheno.GNHS[,c("sampleid", "age","sex","BMI")], database, by.x = "sampleid", by.y = "id", all.y = T)

database$age[is.na(database$age)] <- mean(database$age, na.rm = T)
database$BMI[is.na(database$BMI)] <- mean(database$BMI, na.rm = T)
database$sex[is.na(database$sex)] <- median(database$sex, na.rm = T)
sum(is.na(database[,c("age","sex","BMI")]))

list_trait <- c(list_daily2, list_RG, list_WG, "PGS")


# stat ---------------------------------------  
#' @:spearman-correlation
result1 <- stat_corr(data = database, list1 = "age", list2 = list_trait, method = "spearman")
result1$group <- "PGS"
result1$group[result1$Var2 %in% list_daily2] <- "daily"
result1$group[result1$Var2 %in% c(list_RG, list_WG)] <- "meal"
table(result1$sig_p, result1$group) 


#' @:pSC
result2 <- stat_pSC(data = database, list1 = c("age", list_trait), list2 = c("sex"), method = "spearman")
result2 <- result2[result2$feat1=="age",]
result2$group <- "PGS"
result2$group[result2$feat2 %in% list_daily2] <- "daily"
result2$group[result2$feat2 %in% c(list_RG, list_WG)] <- "meal"
table(result2$sig_p, result2$group) 


##-----------------------------------------------------------
setwd(paste0(workpath, "/part1_aging"))
openxlsx::write.xlsx(list("stat"=result2, "data"=database),"stat_age_CGMtraits_corr.xlsx")




#' @:Aging-&-DailyTraits [CV, Slicing window]
##-----------------------------------------------------------
# data ---------------------------------------  
setwd(paste0(root,"/1_data"))
data_disease.GNHS <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = 1)

data_pheno.GNHS <- openxlsx::read.xlsx("GNHS_pheno_all.xlsx", sheet = 1)
data_pheno.GNHS <- data_pheno.GNHS[data_pheno.GNHS$visit=="F4",]

data_CGM <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "daily_mean")
data_CGM <- data_CGM[grep("NL",data_CGM$id),]


# stat (CV, all) ---------------------------------------
input_data <- merge(data_pheno.GNHS[,c("sampleid","age")], data_CGM, by.x="sampleid", by.y = "id", all.y=T)
input_data <- na.omit(input_data)


#' @:Slicing-Age 
list2 <- list_daily2
list_size <- c(seq(4,28,2))
step <- 1

output_CV <- data.frame()
for(size in list_size){
  
  window_size=size
  origin=min(input_data$age)
  n=floor(((max(input_data$age)-window_size)-origin)/step)
  
  output <- data.frame() 
  for(round in 0:n){
    
    start <- (origin+round*step)
    end <- (start+window_size)
    submatr <- input_data[input_data$age>=start & input_data$age<end,]
    
    age_mean <- mean(submatr$age)
    cv <- data.frame(t(apply(submatr[,list2],2, function(x){sd(x)*100/mean(x)})))
    result <- data.frame(step_width=step, window_size=size, cv, age_mean, start, end, number=nrow(submatr))
    output <- rbind.fill(output, result)
  }
  output_CV <- rbind.fill(output_CV, output)
}
output_CV.all <- output_CV


#' @:stat [pearson correlation]
eval_daily.GNHS <- data.frame()  
for(size in list_size){
  submatr <- output_CV[output_CV$window_size==size,]
  result <- stat_corr(data = submatr, list1 = "age_mean", list2 = list2, method = "pearson")
  
  eval_daily.GNHS <- rbind.fill(eval_daily.GNHS, data.frame(result, window_size=size)) 
}
eval_daily.GNHS <- eval_daily.GNHS[order(eval_daily.GNHS$Var2),]
eval_daily.GNHS.all <- eval_daily.GNHS


# stat (CV, noT2D) ---------------------------------------
input_data <- merge(data_pheno.GNHS[,c("sampleid","age")], data_CGM, by.x="sampleid", by.y = "id", all.y=T)
input_data <- merge(input_data, data_disease.GNHS[,c("sampleid","otc_T2D")], by="sampleid", all=F)
input_data <- input_data[input_data$otc_T2D==0,]
input_data <- na.omit(input_data)


#' @:Slicing-Age 
list2 <- list_daily2
list_size <- c(seq(4,28,2))
step <- 1

output_CV <- data.frame()
for(size in list_size){
  
  window_size=size
  origin=min(input_data$age)
  n=floor(((max(input_data$age)-window_size)-origin)/step)
  
  output <- data.frame() 
  for(round in 0:n){
    
    start <- (origin+round*step)
    end <- (start+window_size)
    submatr <- input_data[input_data$age>=start & input_data$age<end,]
    
    age_mean <- mean(submatr$age)
    cv <- data.frame(t(apply(submatr[,list2],2, function(x){sd(x)*100/mean(x)})))
    result <- data.frame(step_width=step, window_size=size, cv, age_mean, start, end, number=nrow(submatr))
    output <- rbind.fill(output, result)
  }
  output_CV <- rbind.fill(output_CV, output)
}
output_CV.noT2D <- output_CV


#' @:stat [pearson correlation]
eval_daily.GNHS <- data.frame()  
for(size in list_size){
  submatr <- output_CV[output_CV$window_size==size,]
  result <- stat_corr(data = submatr, list1 = "age_mean", list2 = list2, method = "pearson")
  
  eval_daily.GNHS <- rbind.fill(eval_daily.GNHS, data.frame(result, window_size=size)) 
}
eval_daily.GNHS <- eval_daily.GNHS[order(eval_daily.GNHS$Var2),] 
eval_daily.GNHS.noT2D <- eval_daily.GNHS
table(eval_daily.GNHS.noT2D$sig_q, eval_daily.GNHS.noT2D$window_size)


##-----------------------------------------------------------
setwd(paste0(workpath, "/part1_aging"))
openxlsx::write.xlsx(list("CV_GNHS"=output_CV.all, "stat_corr"=eval_daily.GNHS.all,
                          "CV_GNHS_noT2D"=output_CV.noT2D, "stat_corr_noT2D"=eval_daily.GNHS.noT2D),"stat_Daily_CV_age_corr.xlsx")




#' @:Aging-&-DRs [CV, Slicing window]
##-----------------------------------------------------------
# data --------------------------------------- 
#' @:GNHS
setwd(paste0(root,"/1_data"))
data_pheno.GNHS <- openxlsx::read.xlsx("GNHS_pheno_all.xlsx", sheet = 1)
data_CGM_meal.GNHS <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "meal")
data_CGM_meal.GNHS <- data_CGM_meal.GNHS[data_CGM_meal.GNHS$break_type %in% c("refine","whole"),] 
data_CGM_meal.GNHS <- data_CGM_meal.GNHS[data_CGM_meal.GNHS$cohort=="NL",]


data_CGM_meal.GNHS.diff <- data.frame()
refine <- data_CGM_meal.GNHS[data_CGM_meal.GNHS$break_type=="refine",c("id","peak","ppge","ppgr","glu_acc")]
refine_mean <- data.frame(aggregate(refine[,c("peak","ppge","ppgr","glu_acc")], by=list(refine$id), mean, na.rm=T))
names(refine_mean) <- c("id", paste0(colnames(refine_mean)[-1],"_refine"))

whole <- data_CGM_meal.GNHS[data_CGM_meal.GNHS$break_type=="whole",c("id","peak","ppge","ppgr","glu_acc")]
whole_mean <- data.frame(aggregate(whole[,c("peak","ppge","ppgr","glu_acc")], by=list(whole$id), mean, na.rm=T))
names(whole_mean) <- c("id", paste0(colnames(whole_mean)[-1],"_whole"))

data_CGM_meal.GNHS.diff <- merge(refine_mean, whole_mean, by="id", all=F)
for(i in c("peak","ppge","ppgr","glu_acc")){ 
  submatr <- data.frame("diff"= data_CGM_meal.GNHS.diff[,paste0(i,"_refine")]-data_CGM_meal.GNHS.diff[,paste0(i,"_whole")])
  names(submatr) <- paste0(i,"_diff") 
  data_CGM_meal.GNHS.diff <- cbind(data_CGM_meal.GNHS.diff, submatr)
}
apply(data_CGM_meal.GNHS.diff[,-1], 2, sd)*100/apply(data_CGM_meal.GNHS.diff[,-1], 2, mean)


# stat (CV) ---------------------------------------
input_data <- aggregate(data_CGM_meal.GNHS[,c("ppgr","ppge","peak","glu_acc")], by=list(data_CGM_meal.GNHS$id, data_CGM_meal.GNHS$break_type), mean, na.rm=T)
names(input_data)[c(1:2)] <- c("sampleid","diet")  
input_data <- merge(data_pheno.GNHS[,c("sampleid","age")], input_data, by="sampleid", all.y=T)
input_data$age[is.na(input_data$age)] <- mean(input_data$age, na.rm = T)


#' @:Slicing-Age
list1 <- unique(input_data$diet)
list2 <- c("ppgr","ppge","peak","glu_acc")
list_size <- c(seq(4,28,2))
step <- 1

output_CV <- data.frame()
for(size in list_size){
  
  window_size=size
  origin=min(input_data$age)
  n=floor(((max(input_data$age)-window_size)-origin)/step)
  
  output <- data.frame() 
  for(diet in list1){ 
    for(round in 0:n){
      
      start <- (origin+round*step)
      end <- (start+window_size)
      submatr <- input_data[input_data$age>=start & input_data$age<end & input_data$diet==diet,]
      
      age_mean <- mean(submatr$age)
      cv <- data.frame(t(apply(submatr[,list2],2, function(x){sd(x)*100/mean(x)})))
      result <- data.frame(diet=diet, step_width=step, window_size=size, cv, age_mean, start, end, number=nrow(submatr))
      output <- rbind.fill(output, result)
    }
  }
  output_CV <- rbind.fill(output_CV, output)
}


#' @:stat [Pearson correlation]
eval_RG.GNHS <- data.frame() 
eval_WG.GNHS <- data.frame()
for(size in list_size){
  submatr <- output_CV[output_CV$window_size==size,]
  RG_corr <- stat_corr(data = submatr[submatr$diet=="refine",], list1 = "age_mean", list2 = c("ppgr","ppge","peak","glu_acc"), method = "pearson")
  WG_corr <- stat_corr(data = submatr[submatr$diet=="whole",], list1 = "age_mean", list2 = c("ppgr","ppge","peak","glu_acc"), method = "pearson")
  
  eval_RG.GNHS <- rbind.fill(eval_RG.GNHS, data.frame(RG_corr, window_size=size))
  eval_WG.GNHS <- rbind.fill(eval_WG.GNHS, data.frame(WG_corr, window_size=size))
}
eval_WG.GNHS <- eval_WG.GNHS[order(eval_WG.GNHS$Var2),]
eval_RG.GNHS <- eval_RG.GNHS[order(eval_RG.GNHS$Var2),]


##-----------------------------------------------------------
setwd(paste0(workpath, "/part1_aging"))
openxlsx::write.xlsx(list("CV_GNHS"=output_CV, "corr_GNHS_refine"=eval_RG.GNHS, "corr_GNHS_whole"=eval_WG.GNHS),"stat_DRs_CV_age_corr.xlsx")




#' @:Aging-&-PGS [CV, Slicing window]
##-----------------------------------------------------------
# data --------------------------------------- 
#' @:GNHS
setwd(paste0(workpath, "/part1_aging"))
data_PGS <- openxlsx::read.xlsx("stat_age_CGMtraits_corr.xlsx", sheet = "data")
data_PGS <- data_PGS[!is.na(data_PGS$PGS), c("sampleid","age","PGS")]


# stat (CV) ---------------------------------------
input_data <- data_PGS

#' @:Slicing-Age  
list_size <- c(seq(4,28,2))
step <- 1

output_CV <- data.frame()
for(size in list_size){
  
  window_size=size
  origin=min(input_data$age)
  n=floor(((max(input_data$age)-window_size)-origin)/step)
  
  output <- data.frame() 
  for(round in 0:n){
    
    start <- (origin+round*step)
    end <- (start+window_size)
    submatr <- input_data[input_data$age>=start & input_data$age<end,]
    
    age_mean <- mean(submatr$age)
    cv <- sd(submatr[,"PGS"])*100/mean(submatr[,"PGS"])
    result <- data.frame(diet="PGS", step_width=step, window_size=size, cv, age_mean, start, end, number=nrow(submatr))
    output <- rbind.fill(output, result)
  }
  output_CV <- rbind.fill(output_CV, output)
}


#' @:stat [Pearson correlation]
eval_PGS.GNHS <- data.frame()  
for(size in list_size){
  submatr <- output_CV[output_CV$window_size==size,]
  corr <- stat_corr(data = submatr, list1 = "age_mean", list2 = c("cv"), method = "pearson")
 
  eval_PGS.GNHS <- rbind.fill(eval_PGS.GNHS, data.frame(corr, window_size=size)) 
}
eval_PGS.GNHS <- eval_PGS.GNHS[order(eval_PGS.GNHS$Var2),] 
median(eval_PGS.GNHS$r)


##-----------------------------------------------------------
setwd(paste0(workpath, "/part1_aging"))
openxlsx::write.xlsx(list("CV_GNHS"=output_CV, "corr_GNHS_PGS"=eval_PGS.GNHS),"stat_PGS_CV_age_corr.xlsx")




#' @:Uniqueness-Aging [Spearman correlation] 
##-----------------------------------------------------------
# data --------------------

setwd(paste0(root,"/1_data")) 
#' @:daily
data_CGM.daily <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "daily_mean")
data_CGM.daily <- data_CGM.daily[grep("NL",data_CGM.daily$id),] 
data_CGM.daily$LBGI_mean_all[is.na(data_CGM.daily$LBGI_mean_all)] <- 0
data_CGM.daily$LBGI_mean_day[is.na(data_CGM.daily$LBGI_mean_day)] <- 0
data_CGM.daily$LBGI_mean_night[is.na(data_CGM.daily$LBGI_mean_night)] <- 0

data_CGM.daily$HBGI_mean_all[is.na(data_CGM.daily$HBGI_mean_all)] <- 0
data_CGM.daily$HBGI_mean_day[is.na(data_CGM.daily$HBGI_mean_day)] <- 0
data_CGM.daily$HBGI_mean_night[is.na(data_CGM.daily$HBGI_mean_night)] <- 0
data_CGM.daily$MODD_mean_night[is.na(data_CGM.daily$MODD_mean_night)] <- mean(data_CGM.daily$MODD_mean_night, na.rm = T)


#' @:Meal-RG
data_CGM.RG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "RG_mean")
data_CGM.RG <- data_CGM.RG[grep("NL",data_CGM.RG$SampleID),]
list_RG <- setdiff(colnames(data_CGM.RG),"SampleID")
#' @:Meal-WG
data_CGM.WG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "WG_mean")
data_CGM.WG <- data_CGM.WG[grep("NL",data_CGM.WG$SampleID),]
list_WG <- setdiff(colnames(data_CGM.WG),"SampleID")
#' @:PGS
data_CGM.PGS <- openxlsx::read.xlsx("PGS_construction_GNHS.xlsx", sheet = 1)
data_CGM.PGS <- data_CGM.PGS[grep("NL",data_CGM.PGS$id),]


setwd(paste0(workpath, "/part1_aging"))
data_pheno.GNHS <- openxlsx::read.xlsx("stat_age_CGMtraits_corr.xlsx", sheet = "data") 


# funtion ------------------------
stat_uniqueness <- function(input_data){
  input_data[,-1] <- apply(input_data[,-1],2,scale)
  data_dist <- data.frame(as.matrix(vegdist(input_data[,-1], method = "eu")))
  rownames(data_dist) <- input_data$id; colnames(data_dist) <- input_data$id
  
  data_dist <- data.frame(id=input_data$id, data_dist)
  data_dist <- reshape2::melt(data_dist)
  data_dist <- data_dist[data_dist$id!=data_dist$variable,]
  
  idlist <- unique(data_dist$id)
  data_uniqueness <- data.frame()
  for(i in 1:length(idlist)){
    submatr <- data_dist[data_dist$id==idlist[i],]
    uniqueness <- submatr[which.min(submatr$value),]
    names(uniqueness) <- c("id","target","uniqueness")
    data_uniqueness <- rbind.fill(data_uniqueness, uniqueness)
  }
  return(data_uniqueness)
}


# stat ---------------------------
#' @:uniqueness [Daily-Allday time] 
a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
input_data <- data_CGM.daily[,c("id",paste0(c(a,b),"_mean_all"))]
data_uniqueness.allday <- stat_uniqueness(input_data) 

#' @:uniqueness [Daily-Daytime]
input_data <- data_CGM.daily[,c("id",list_daily2[grep("day$",list_daily2)])]
data_uniqueness.day <- stat_uniqueness(input_data) 

#' @:uniqueness [Daily-Nighttime]
input_data <- data_CGM.daily[,c("id",list_daily2[grep("night$",list_daily2)])] 
data_uniqueness.night <- stat_uniqueness(input_data) 

#' @:uniqueness [Meal-RG]
input_data <- data_CGM.RG; names(input_data)[1] <- "id"
data_uniqueness.RG <- stat_uniqueness(input_data) 

#' @:uniqueness [Meal-WG]
input_data <- data_CGM.WG; names(input_data)[1] <- "id"
data_uniqueness.WG <- stat_uniqueness(input_data) 

#' @:uniqueness [Meal-RG + WG]
input_data <- merge(data_CGM.RG, data_CGM.WG, by="SampleID", all=F) 
names(input_data)[1] <- "id"
data_uniqueness.meal <- stat_uniqueness(input_data) 


#' @:merge
database <- merge(data_uniqueness.day[,c("id","uniqueness")], data_uniqueness.night[,c("id","uniqueness")], by="id", all=T) 
names(database) <- c("id","unique_day","unique_night")
database <- merge(database, data_uniqueness.allday[,c("id","uniqueness")], by="id", all=T) 
names(database)[ncol(database)] <- "unique_allday"
database <- merge(database, data_uniqueness.RG[,c("id","uniqueness")], by="id", all=T) 
names(database)[ncol(database)] <- "unique_RG"
database <- merge(database, data_uniqueness.WG[,c("id","uniqueness")], by="id", all=T) 
names(database)[ncol(database)] <- "unique_WG"
database <- merge(database, data_uniqueness.meal[,c("id","uniqueness")], by="id", all=T) 
names(database)[ncol(database)] <- "unique_meal"

database <- merge(database, data_CGM.PGS[,c("id","PGS")], by="id", all=T)  
database <- merge(database, data_pheno.GNHS[,c("sampleid","age","sex","BMI")], by.x="id", by.y = "sampleid", all.x = T)  


#' @:stat
list_unique <- colnames(database)[grep("unique_", colnames(database))]
result1 <- stat_corr(data = database, list1 = c("age"), list2 = c(list_unique,"PGS"), method = "spearman")
result2 <- stat_pSC(data = database, list1 = c(list_unique,"PGS","age"), list2 = c("sex"), method = "spearman")


##-----------------------------------------------------------
setwd(paste0(workpath, "/part1_aging"))
openxlsx::write.xlsx(list("data"=database, "stat_corr"=result1, "stat_pSC"=result2), "stat_age_uniqueness_corr.xlsx")




#' @:Uniqueness-Disease [Correlation between disease trajectory and dietary patterns]  
##-----------------------------------------------------------
# data --------------------------------------- 
setwd(paste0(workpath, "/part1_aging"))
data_unique <- openxlsx::read.xlsx("stat_age_uniqueness_corr.xlsx", sheet = "data")

#' @:GNHS 
setwd(paste(root,"/1_data", sep = ""))
database <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = "disease") 
# database <- openxlsx::read.xlsx("stat_DRs_DTs_comp_dynlm_whole.xlsx", sheet = "data")

list_DTs.all <- colnames(database)[grep("otc_", colnames(database))]  
list_DTs.comorb <- colnames(database)[grep("otc_coMorbidity", colnames(database))] 
list_DTs.single <- list_DTs.all[-which(list_DTs.all %in% c(list_DTs.comorb))] 


#' @:Reshape-database
data_t1 <- database[database$time==1,c("id", list_DTs.single)]; names(data_t1)[-1] <- paste0(colnames(data_t1)[-1],"_t1")
data_t2 <- database[database$time==2,c("id", list_DTs.single)]; names(data_t2)[-1] <- paste0(colnames(data_t2)[-1],"_t2")
data_t3 <- database[database$time==3,c("id", list_DTs.single)]; names(data_t3)[-1] <- paste0(colnames(data_t3)[-1],"_t3")
data_t4 <- database[database$time==4,c("id", list_DTs.single)]; names(data_t4)[-1] <- paste0(colnames(data_t4)[-1],"_t4")
data_t5 <- database[database$time==5,c("id", list_DTs.single)]; names(data_t5)[-1] <- paste0(colnames(data_t5)[-1],"_t5")
database2 <- merge(data_t1, data_t2, by="id", all=F)
database2 <- merge(database2, data_t3, by="id", all=F)
database2 <- merge(database2, data_t4, by="id", all=F)
database2 <- merge(database2, data_t5, by="id", all=F) 
sum(is.na(database2)) 



#' @:reorder
database3 <- data.frame(id=database2$id)
list <- list_DTs.single
for(i in list){
  submatr <- database2[,c("id",colnames(database2)[grep(i, colnames(database2))])]
  database3 <- merge(database3, submatr, by="id", all=F)
}
database3 <- cbind(database3, database2[,grep("^otc_coMorbidity_t", colnames(database2))])


#' @:uniqueness
list_disease <- colnames(database3)[grep("otc_", colnames(database3))]
data_dist <- data.frame(as.matrix(vegdist(database3[,list_disease], method = "eu")))
rownames(data_dist) <- database3$id; colnames(data_dist) <- database3$id

data_dist <- data.frame(id=rownames(data_dist), data_dist)
data_dist <- reshape2::melt(data_dist)
data_dist <- data_dist[data_dist$id!=data_dist$variable,]

idlist <- unique(data_dist$id)
data_uniqueness <- data.frame()
for(i in 1:length(idlist)){
  submatr <- data_dist[data_dist$id==idlist[i],]
  uniqueness <- submatr[which.min(submatr$value),]
  names(uniqueness) <- c("id","target","unique_disease")
  data_uniqueness <- rbind.fill(data_uniqueness, uniqueness)
}
data_uniqueness$id <- paste0("F4", data_uniqueness$id)
database <- merge(data_unique, data_uniqueness[,c("id","unique_disease")], by="id", all.x=T)


# stat ---------------------------
list2 <- setdiff(colnames(database[grep("unique_", colnames(database))]), "unique_disease")
result1 <- stat_corr(data = database, list1 = "unique_disease", list2 = c(list2), method = "spearman")
result2 <- stat_pSC(data = database, list1 = c("unique_disease",list2,"age"), list2 = c("sex"), method = "spearman")


##-----------------------------------------------------------
setwd(paste0(workpath, "/part1_aging")) 
openxlsx::write.xlsx(list("data"=database,"stat_corr"=result1, "stat_pSC"=result2), "stat_disease_uniqueness_corr.xlsx")














#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part1
#' @:LADE-dailyTraits [longitudinal accumulation of disease exposure]
#' 
#' @:-----------------------------------------------------------------------------------------




if(!file.exists(paste0(workpath,"/part1_disease"))){
  dir.create(paste0(workpath,"/part1_disease")); print("Folder is built")
} else { print("Folder exists") }




#' @:Pre-setting
#' @:Daily-traits [CGM-based glycemic triats]

a <- c("eA1C","J_index","MAGE","CV","HBGI","LBGI","MODD")
b <- c("MPT","HPT","LPT")
list_daily <- c(paste0(a, "_mean_all"),paste0(a, "_mean_day"),paste0(a, "_mean_night"),
                paste0(b, "_mean_all.asin"),paste0(b, "_mean_day.asin"),paste0(b, "_mean_night.asin"))
list_daily2 <- c(paste0(c(a,b), "_mean_all"),paste0(c(a,b), "_mean_day"),paste0(c(a,b), "_mean_night"))




#' @:LADE-daily [Daily glycemic traits, all samples & non-diabetic]
#' @:Linear [Cross-sectional, one point]
##------------------------------------------------------
# data ------------------------------  
#' @:CGM-traits
setwd(paste0(root,"/1_data"))
data_pheno.GNHS <- openxlsx::read.xlsx("GNHS_pheno_all.xlsx", sheet = 1)
data_pheno.GNHS <- data_pheno.GNHS[data_pheno.GNHS$visit=="F4",]
data_CGM <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "daily_mean")
data_CGM <- data_CGM[grep("NL",data_CGM$id),]  

database <- merge(data_pheno.GNHS[,c("sampleid","age","sex","BMI")], data_CGM[,c("id",list_daily)], by.x = "sampleid", by.y = "id", all.y=T)
database <- data.frame(id=substr(database$sampleid,3,8), database)
database[,list_daily] <- apply(database[,list_daily],2,scale)


#' @:LADE
setwd(paste(root,"/1_data", sep = ""))
data_onset <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = "onset_duration") 
list_disease <- colnames(data_onset)[grep("otc_", colnames(data_onset))] 

database <- merge(database, data_onset, by = "id", all = F)  
print(paste0("N of sample size: ", length(unique(database$id))))


#' @:Setting
list_exp <- list_disease
list_otc <- list_daily
list_covar <- c("age","sex") 


# stat (all) ---------------------------- 
input_data <- database 
list <- c("all","day","night")


out_linear <- data.frame()
for(time in list){
  list_otc2 <- list_otc[grep(paste0("_",time),list_otc)]  
  result <- stat_linear(data = input_data, list_exp = list_exp, list_otc = list_otc2, list_covar = list_covar) 
  
  #' @:FDR-correction
  result$qval <- p.adjust(result$pval, method = "BH")
  result$sig_q <- ifelse(result$pval<0.05 & result$qval<0.05,"**",NA)
  result$sig_q[result$pval<0.05 & result$qval>=0.05] <- "*"
  out_linear <- rbind(out_linear, result)
}
out_linear.all <- out_linear
table(out_linear.all$exposure, out_linear.all$sig_q)
unique(out_linear.all$exposure[out_linear.all$sig_q=="**"])


# stat (non-diabetic) ---------------------------- 
input_data <- database[database$otc_T2D==0,]
list <- c("all","day","night")

out_linear <- data.frame()
for(time in list){
  list_otc2 <- list_otc[grep(paste0("_",time),list_otc)]  
  result <- stat_linear(data = input_data, list_exp = setdiff(list_exp,"otc_T2D"), list_otc = list_otc2, list_covar = list_covar) 
  
  #' @:FDR-correction
  result$qval <- p.adjust(result$pval, method = "BH")
  result$sig_q <- ifelse(result$pval<0.05 & result$qval<0.05,"**",NA)
  result$sig_q[result$pval<0.05 & result$qval>=0.05] <- "*"
  out_linear <- rbind(out_linear, result)
}
out_linear.noT2D <- out_linear
table(out_linear.noT2D$exposure, out_linear.noT2D$sig_q)
unique(out_linear.noT2D$exposure[out_linear.noT2D$sig_q=="**"])


##------------------------------------------------------
setwd(paste0(workpath, "/part1_disease"))
openxlsx::write.xlsx(list("stat_all"=out_linear.all, "stat_noT2D"=out_linear.noT2D, "data"=database), "stat_Daily_LADE_linear.xlsx")




#' @:Disease-network [Associations of each disease pair, all]
#' @:Logistic-reg [cross-sectional]
##------------------------------------------------------
# data ---------------------------
setwd(paste0(workpath, "/part1_disease"))
data <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "data")
list_id <- unique(data$id)
stat <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_all")
list_disease <- unique(stat$exposure[stat$sig_q=="**" & !is.na(stat$sig_q)])


setwd(paste(root,"/1_data", sep = ""))
data_disease <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = "disease")   
data_disease <- data_disease[data_disease$id %in% list_id, c("sampleid","id","time","age","sex","BMI",list_disease)]
sum(is.na(data_disease))


#' @:onset_age
data_onsetAge <- data.frame(id = list_id)
for(d in list_disease){
  submatr <- data_disease[,c("id","time","age",d)]
  submatr <- submatr[order(submatr$time, decreasing = F),]
  submatr <- submatr[order(submatr$id, decreasing = F),]
  
  submatr2 <- data.frame()
  for(id in list_id){ 
    onset_Age <- ifelse(sum(submatr[submatr$id==id,d])==0, NA, min(submatr[submatr$id==id & submatr[,d]==1,"age"]))
    onset_Age <- data.frame(id=id, age=onset_Age)
    submatr2 <- rbind(submatr2, onset_Age)
  }
  names(submatr2)[2] <- d
  data_onsetAge <- merge(data_onsetAge, submatr2, by='id', all=F)
}
meanAge_onset <- data.frame(meanAge=apply(data_onsetAge[,-1],2,mean, na.rm=T))
meanAge_onset <- data.frame(disease=rownames(meanAge_onset),meanAge_onset)

data_disease.sub <- data_disease[data_disease$id %in% list_id,]
data_disease.sub <- data_disease.sub[order(data_disease.sub$time, decreasing = T),]
data_disease.sub <- data_disease.sub[!duplicated(data_disease.sub$id),]; table(data_disease.sub$time)


# stat ------------------------
input_data <- data_disease.sub 
output_logist <- data.frame()

for(d1 in list_disease){
  list_disease2 <- setdiff(list_disease, d1)
  
  output <- data.frame()
  for(d2 in list_disease2){
    age1 <- meanAge_onset$meanAge[meanAge_onset$disease==d1]
    age2 <- meanAge_onset$meanAge[meanAge_onset$disease==d2]
    
    exp <- ifelse(age1<age2, d1, d2)
    otc <- setdiff(c(d1,d2), exp) 
    stat <- glm(input_data[,otc] ~ input_data[,exp] + age + sex, data = input_data, family = "binomial")
    result <- data.frame(t(summary(stat)$coefficient[2,]))
    names(result) <- c("beta","se","z","pval")
    
    or <- exp(coef(stat))[2]
    ci <- exp(confint(stat))[2,] 
    
    result <- data.frame(exposure=exp, outcome=otc, result, OR=or, CI_lower=ci[1], CI_upper=ci[2]) 
    output <- rbind.fill(output, result)
  }
  output_logist <- rbind.fill(output_logist, output)
}
output_logist$tag <- paste(output_logist$exposure, output_logist$outcome, sep = "_")
output_logist <- output_logist[!duplicated(output_logist$tag),]

output_logist$qval <- p.adjust(output_logist$pval, method = "BH")
output_logist$sig_q <- ifelse(output_logist$pval<0.05,"*",NA)
output_logist$sig_q[output_logist$qval<0.05 & output_logist$pval<0.05] <- "**"
table(output_logist$sig_q) 


##------------------------------------------------------
setwd(paste0(workpath, "/part1_disease"))
openxlsx::write.xlsx(list("stat_all"=output_logist, "data"=data_disease.sub, "data_onsetAge"=meanAge_onset), "stat_DiseaeseNetwork_logistic_all.xlsx")




#' @:Disease-network [Associations of each disease pair, noT2D]
#' @:Logistic-reg [cross-sectional]
##------------------------------------------------------
# data ---------------------------
setwd(paste0(workpath, "/part1_disease"))
data <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "data")
data <- data[data$otc_T2D==0,]
list_id <- unique(data$id)
stat <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_noT2D")
list_disease <- unique(stat$exposure[stat$sig_q=="**" & !is.na(stat$sig_q)])


setwd(paste(root,"/1_data", sep = ""))
data_disease <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = "disease")   
data_disease <- data_disease[data_disease$id %in% list_id, c("sampleid","id","time","age","sex","BMI",list_disease)]
sum(is.na(data_disease))


#' @:onset_age
data_onsetAge <- data.frame(id = list_id)
for(d in list_disease){
  submatr <- data_disease[,c("id","time","age",d)]
  submatr <- submatr[order(submatr$time, decreasing = F),]
  submatr <- submatr[order(submatr$id, decreasing = F),]
  
  submatr2 <- data.frame()
  for(id in list_id){ 
    onset_Age <- ifelse(sum(submatr[submatr$id==id,d])==0, NA, min(submatr[submatr$id==id & submatr[,d]==1,"age"]))
    onset_Age <- data.frame(id=id, age=onset_Age)
    submatr2 <- rbind(submatr2, onset_Age)
  }
  names(submatr2)[2] <- d
  data_onsetAge <- merge(data_onsetAge, submatr2, by='id', all=F)
}
meanAge_onset <- data.frame(meanAge=apply(data_onsetAge[,-1],2,mean, na.rm=T))
meanAge_onset <- data.frame(disease=rownames(meanAge_onset),meanAge_onset)

data_disease.sub <- data_disease[data_disease$id %in% list_id,]
data_disease.sub <- data_disease.sub[order(data_disease.sub$time, decreasing = T),]
data_disease.sub <- data_disease.sub[!duplicated(data_disease.sub$id),]; table(data_disease.sub$time)


# stat ------------------------
input_data <- data_disease.sub 
output_logist <- data.frame()

for(d1 in list_disease){
  list_disease2 <- setdiff(list_disease, d1)
  
  output <- data.frame()
  for(d2 in list_disease2){
    age1 <- meanAge_onset$meanAge[meanAge_onset$disease==d1]
    age2 <- meanAge_onset$meanAge[meanAge_onset$disease==d2]
    
    exp <- ifelse(age1<age2, d1, d2)
    otc <- setdiff(c(d1,d2), exp) 
    stat <- glm(input_data[,otc] ~ input_data[,exp] + age + sex, data = input_data, family = "binomial")
    result <- data.frame(t(summary(stat)$coefficient[2,]))
    names(result) <- c("beta","se","z","pval")
    
    or <- exp(coef(stat))[2]
    ci <- exp(confint(stat))[2,] 
    
    result <- data.frame(exposure=exp, outcome=otc, result, OR=or, CI_lower=ci[1], CI_upper=ci[2]) 
    output <- rbind.fill(output, result)
  }
  output_logist <- rbind.fill(output_logist, output)
}
output_logist$tag <- paste(output_logist$exposure, output_logist$outcome, sep = "_")
output_logist <- output_logist[!duplicated(output_logist$tag),]

output_logist$qval <- p.adjust(output_logist$pval, method = "BH")
output_logist$sig_q <- ifelse(output_logist$pval<0.05,"*",NA)
output_logist$sig_q[output_logist$qval<0.05 & output_logist$pval<0.05] <- "**"
table(output_logist$sig_q) 


##------------------------------------------------------
setwd(paste0(workpath, "/part1_disease"))
openxlsx::write.xlsx(list("stat_all"=output_logist, "data"=data_disease.sub, "data_onsetAge"=meanAge_onset), "stat_DiseaeseNetwork_logistic_noT2D.xlsx")




#' @:Multimorbidity.Glucose-Dyslipidemia [Trajectory comparison, conti]
#' @:Interaction
##------------------------------------------------------
# data -----------------------
setwd(paste0(workpath, "/part1_disease"))
data <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "data")
list_id <- unique(data$id)
stat1 <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_all")
stat2 <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_noT2D")
a <- unique(stat1$exposure[stat1$sig_q=="**" & !is.na(stat1$sig_q)])
b <- unique(stat2$exposure[stat2$sig_q=="**" & !is.na(stat2$sig_q)])
list_disease <- unique(c(a,b))


setwd(paste(root,"/1_data", sep = ""))
data_disease <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = "disease")   
data_disease <- data_disease[data_disease$id %in% list_id, c("sampleid","id","time","age","sex","BMI",list_disease)]
sum(is.na(data_disease))

data_disease.sub <- data_disease[data_disease$id %in% list_id,]
data_disease.sub <- data_disease.sub[order(data_disease.sub$time, decreasing = T),]
table(data_disease.sub$otc_Dyslipidemia, data_disease.sub$time)
data_disease.sub$otc_coMorbidity.glucose <- apply(data_disease.sub[,list_disease],1,sum, na.rm=T)
data_disease.sub$otc_coMorbidity.glucose2 <- data_disease.sub$otc_coMorbidity.glucose-data_disease.sub$otc_Dyslipidemia
table(data_disease.sub$otc_coMorbidity.glucose2, data_disease.sub$time)


#' @:group
a <- data_disease.sub[data_disease.sub$time==1, c("id","otc_Dyslipidemia")]
a$group <- ifelse(a$otc_Dyslipidemia==0,-1,1)
database <- merge(a[,c("id","group")], data_disease.sub, by="id", all=F)
table(database$otc_coMorbidity.glucose, database$time, database$group)


# stat --------------------------
#' @:Mixed-linear [P-interaction]
input_data <- database 
stat <- lmerTest::lmer(otc_coMorbidity.glucose2 ~ time * group + age + sex + (1 + time | id), data = input_data) 
result <- data.frame(summary(stat)$coefficient)
names(result) <- c("beta","se","df","t","pval")
result.mixed <- result


#' @:Wilcox-test [Point-by-point]
input_data <- database
input_data$group <- ifelse(input_data$group==-1,"non","case")
result_wilcox <- data.frame()

list <- unique(input_data$time)
for(t in list){
  submatr <- input_data[input_data$time==t,]
  stat <- stat_wilcox(data = submatr, group = "group", trait = "otc_coMorbidity.glucose2")
  stat <- data.frame(time=t, stat)
  result_wilcox <- rbind.fill(result_wilcox, stat)
}
result_wilcox$sig_p <- ifelse(result_wilcox$pval<0.05, "*",NA)
result_wilcox <- result_wilcox[order(result_wilcox$time, decreasing=F),]


##------------------------------------------------------
setwd(paste0(workpath, "/part1_disease"))
openxlsx::write.xlsx(list("stat_mixed"=result.mixed, "stat_wilcox"=result_wilcox, "data"=database), "stat_Dyslipid_subgroup_comparison.xlsx")













#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part2
#' @:Daily-traits-&-Multimorbidity [8 glucose-related diseases]
#' 
#' @:-----------------------------------------------------------------------------------------



if(!file.exists(paste0(workpath,"/part2_multimorbid"))){
  dir.create(paste0(workpath,"/part2_multimorbid")); print("Folder is built")
} else { print("Folder exists") }




#' @:Multimorbidity.Glucose [Multimorbidity Comparison, GNHS, quartile (P-stratified) *** ]
#' @:linear-test [with T2D-stratified analysis, dose-response, the same reference]
#' @:Group: [0: No disease, 1: One single disease, 2: mild-comorbid with 2-3 diseases, 3: severe-comorbid with 4 and more diseases]
##------------------------------------------------------
# data ----------------------- 
#' @:disease
setwd(paste0(workpath, "/part1_disease"))
data <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "data")
list_id <- unique(data$id)
stat1 <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_all")
stat2 <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_noT2D")
a <- unique(stat1$exposure[stat1$sig_q=="**" & !is.na(stat1$sig_q)])
b <- unique(stat2$exposure[stat2$sig_q=="**" & !is.na(stat2$sig_q)])
list_disease <- unique(c(a,b))


setwd(paste(root,"/1_data", sep = ""))
data_disease <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = "disease")  
data_disease <- data_disease[,c("sampleid","id","age","sex","BMI","time", list_disease)]
data_disease <- data_disease[order(data_disease$time, decreasing = T),]
data_disease <- data_disease[data_disease$id %in% list_id,]
data_disease <- data_disease[!duplicated(data_disease$id),]

data_disease$otc_coMorbidity.glucose <- apply(data_disease[,list_disease],1,sum, na.rm=T) 
table(data_disease$otc_coMorbidity.glucose)
median(data_disease$otc_coMorbidity.glucose)


#' @:CGM-data
setwd(paste(root,"/1_data", sep = ""))
data_CGM <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "daily_mean")
data_CGM <- data_CGM[grep("NL",data_CGM$id),]
data_CGM$id <- gsub("F4","",data_CGM$id)  
database <- merge(data_disease, data_CGM[,c("id", list_daily)], by="id", all = F) 


#' @:Normalization-Standardization
a <- c("eA1C","J_index","MAGE","CV","MODD")
list <- c(paste0(a, "_mean_day"),paste0(a, "_mean_night")) 
database[,list] <- apply(database[,list],2,log2)
database[,list_daily] <- apply(database[,list_daily],2,scale)


#' @:quartile  
database$group <- ifelse(database$otc_coMorbidity.glucose==0, 0,NA)
database$group[database$otc_coMorbidity.glucose==1] <- 1
database$group[database$otc_coMorbidity.glucose>1 & database$otc_coMorbidity.glucose<4] <- 2
database$group[database$otc_coMorbidity.glucose>=4] <- 3
table(database$group)


# function -----------------------------
stat_linear_factor <- function(data, list_exp, list_otc, list_covar, timepoint){
  
  output_linear <- data.frame()
  for(exp in list_exp){
    for(otc in list_otc){
      
      formula1 <- paste(otc,"~ as.factor(",exp,")")
      formula2 <- paste(list_covar, collapse=" + ")
      formula <- paste(formula1, formula2, sep=" + ")
      result <- summary(lm(as.formula(formula), data=data))  
      
      result1 <- data.frame(result$coefficients); names(result1) <- c("beta","se","t","pval")
      result1 <- result1[grep("as.factor", rownames(result1)),]
      result1 <- data.frame(group=seq(1,nrow(result1),1),result1)
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


# stat -----------------------

result_all <- stat_linear_factor(data = database, list_exp = "group", list_otc = list_daily, list_covar = c("age","sex")) 
result_noT2D <- stat_linear_factor(data = database[database$otc_T2D==0,], list_exp = "group", list_otc = list_daily, list_covar = c("age","sex")) 
result_T2D <- stat_linear_factor(data = database[database$otc_T2D==1,], list_exp = "group", list_otc = list_daily, list_covar = c("age","sex")) 

#' @:Sorting
output_linear <- rbind(data.frame(group2="all", result_all), data.frame(group2="noT2D", result_noT2D), data.frame(group2="T2D", result_T2D))
output_linear <- data.frame(time=NA,output_linear)
output_linear$time[grep("all",output_linear$outcome)] <- "Whole-day time"
output_linear$time[grep("day",output_linear$outcome)] <- "Daytime"
output_linear$time[grep("night",output_linear$outcome)] <- "Nighttime" 

output_linear$trait <- sapply(output_linear$outcome, function(x){strsplit(x, split = "_")[[1]][1]}) 
output_linear$trait[output_linear$trait=="J"] <- "J-index"
table(output_linear$group, output_linear$sig_p)


#' @:FDR
output_linear2 <- data.frame()
list_time <- unique(output_linear$time)
list_exp <- unique(output_linear$exposure)
list_group1 <- unique(output_linear$group)
list_group2 <- unique(output_linear$group2)

for(a in list_group1){
  for(i in list_group2){ 
    for(j in list_time){
      for(k in list_exp){
        submatr <- output_linear[output_linear$group==a & output_linear$group2==i & output_linear$time==j & output_linear$exposure==k,]
        submatr$qval <- p.adjust(submatr$pval, method = "BH")
        submatr$sig_q <- ifelse(submatr$pval<0.05,"*",NA)
        submatr$sig_q[submatr$qval<0.05 & submatr$pval<0.05] <- "**"
        output_linear2 <- rbind.fill(output_linear2, submatr) 
      } 
    } 
  }
}

table(output_linear2$group2, output_linear2$sig_q)
table(output_linear2$time, output_linear2$group2, output_linear2$sig_q)


##------------------------------------------------------
setwd(paste0(workpath, "/part2_multimorbid"))
openxlsx::write.xlsx(list("stat"=output_linear2, "data"=database), "stat_multimorbidity_DailyTraits_linear_quartile.xlsx")













#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part3
#' @:PGS-Multimorbidity [With standard meal tests]
#' 
#' @:-----------------------------------------------------------------------------------------



if(!file.exists(paste0(workpath,"/part3_PGS"))){
  dir.create(paste0(workpath,"/part3_PGS")); print("Folder is built")
} else { print("Folder exists") }




#' @:Multimorbidity-DRs [Group comparison]
##------------------------------------------------------
# data ------------------------------ 
#' @:Dietary-responses
setwd(paste0(root,"/1_data")) 
data_CGM.RG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "RG_mean")
names(data_CGM.RG) <- gsub("RG_","",colnames(data_CGM.RG))
data_CGM.WG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "WG_mean")
names(data_CGM.WG) <- gsub("WG_","",colnames(data_CGM.WG))

data_CGM <- rbind(data.frame(meal="RG", data_CGM.RG),data.frame(meal="WG", data_CGM.WG))
data_CGM <- data_CGM[grep("NL",data_CGM$SampleID),]
names(data_CGM)[2] <- "sampleid"
data_CGM$id <- substr(data_CGM$sampleid,3,8)


#' @:LADE
setwd(paste0(workpath, "/part2_multimorbid"))
data_disease <- openxlsx::read.xlsx("stat_multimorbidity_DailyTraits_linear_quartile.xlsx", sheet = "data") 
list_comorb <- colnames(data_disease)[grep("otc_coMorbidity", colnames(data_disease))]
list_disease <- setdiff(colnames(data_disease)[grep("otc_", colnames(data_disease))],list_comorb)

database <- merge(data_CGM, data_disease[,c("id","age","sex","BMI",list_comorb,"otc_T2D")], by="id", all=F)   
print(paste0("N of sample size: ", length(unique(database$id))))
# 
# for(meal in c("RG","WG")){
#   database[database$meal==meal, c("ppge","iAUC","acc","peak")] <- apply(database[database$meal==meal, c("ppge","iAUC","acc","peak")],2,scale)
# }


#' @:Group
database$group <- ifelse(database$otc_coMorbidity.glucose==0,0,1)
database$group[database$otc_coMorbidity.glucose>=2 & database$otc_coMorbidity.glucose<=3] <- 2
database$group[database$otc_coMorbidity.glucose>=4] <- 3
table(database$group, database$meal)


# stat (all) ---------------------------- 
input_data <- database
list_meal <- c("RG","WG")
list_group <- c(1,2,3)
list_trait <- c("peak","iAUC","ppge","acc")

output_wilcox <- data.frame()
for(meal in list_meal){
  
  output <- data.frame()
  for(g in list_group){
    
    submatr <- input_data[input_data$meal==meal & input_data$group %in% c(0,g),]
    n_case <- sum(submatr$group)
    submatr$group <- ifelse(submatr$group==0,"ref","case")
    
    result <- data.frame() 
    for(t in list_trait){
      stat1 <- stat_wilcox(data = submatr, group = "group", trait = t)
      stat2 <- data.frame(meal=meal, group=g, stat1, n_case=n_case)
      result <- rbind.fill(result, stat2)
    } 
    output <- rbind(output, result)
  }
  #' @:FDR-correction
  output$qval <- p.adjust(output$pval, method = "BH")
  output$sig_q <- ifelse(output$pval<0.05 & output$qval<0.05,"**",NA)
  output$sig_q[output$pval<0.05 & output$qval>=0.05] <- "*" 
  output_wilcox <- rbind(output_wilcox, output)  
}
table(output_wilcox$meal, output_wilcox$group, output_wilcox$sig_q)
output_wilcox.all <- output_wilcox


# stat (noT2D) ---------------------------- 
input_data <- database[database$otc_T2D==0,]
list_meal <- c("RG","WG")
list_group <- c(1,2,3)
list_trait <- c("peak","iAUC","ppge","acc")

output_wilcox <- data.frame()
for(meal in list_meal){
  
  output <- data.frame()
  for(g in list_group){
    
    submatr <- input_data[input_data$meal==meal & input_data$group %in% c(0,g),]
    n_case <- sum(submatr$group)
    submatr$group <- ifelse(submatr$group==0,"ref","case")
    
    result <- data.frame() 
    for(t in list_trait){
      stat1 <- stat_wilcox(data = submatr, group = "group", trait = t)
      stat2 <- data.frame(meal=meal, group=g, stat1, n_case=n_case)
      result <- rbind.fill(result, stat2)
    } 
    output <- rbind(output, result)
  }
  #' @:FDR-correction
  output$qval <- p.adjust(output$pval, method = "BH")
  output$sig_q <- ifelse(output$pval<0.05 & output$qval<0.05,"**",NA)
  output$sig_q[output$pval<0.05 & output$qval>=0.05] <- "*" 
  output_wilcox <- rbind(output_wilcox, output)  
}
table(output_wilcox$meal, output_wilcox$group, output_wilcox$sig_q)
output_wilcox.noT2D <- output_wilcox


##------------------------------------------------------
setwd(paste0(workpath, "/part3_PGS"))
openxlsx::write.xlsx(list("stat_all"=output_wilcox.all, "stat_noT2D"=output_wilcox.noT2D, "data"=database), "stat_DRs_multimorbidity_wilcox.xlsx")




#' @:Multimorbidity-DRs  
##------------------------------------------------------
# data ------------------------------ 
#' @:Dietary-responses
setwd(paste0(root,"/1_data")) 
data_CGM.RG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "RG_mean")
names(data_CGM.RG) <- gsub("RG_","",colnames(data_CGM.RG))
data_CGM.WG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "WG_mean")
names(data_CGM.WG) <- gsub("WG_","",colnames(data_CGM.WG))

data_CGM <- rbind(data.frame(meal="RG", data_CGM.RG),data.frame(meal="WG", data_CGM.WG))
data_CGM <- data_CGM[grep("NL",data_CGM$SampleID),]
names(data_CGM)[2] <- "sampleid"
data_CGM$id <- substr(data_CGM$sampleid,3,8)


#' @:LADE
setwd(paste0(workpath, "/part2_multimorbid"))
data_disease <- openxlsx::read.xlsx("stat_multimorbidity_DailyTraits_linear_quartile.xlsx", sheet = "data") 
list_comorb <- colnames(data_disease)[grep("otc_coMorbidity", colnames(data_disease))]
list_disease <- setdiff(colnames(data_disease)[grep("otc_", colnames(data_disease))],list_comorb)

database <- merge(data_CGM, data_disease[,c("id","age","sex","BMI",list_comorb,"otc_T2D")], by="id", all=F)   
print(paste0("N of sample size: ", length(unique(database$id))))

for(meal in c("RG","WG")){
  database[database$meal==meal, c("ppge","iAUC","acc","peak")] <- apply(database[database$meal==meal, c("ppge","iAUC","acc","peak")],2,scale)
}


#' @:Setting
list_exp <- list_comorb
list_otc <- c("ppge","iAUC","acc","peak")
list_covar <- c("age","sex") 


# stat (all) ---------------------------- 
input_data <- database
list_meal <- c("RG","WG")

out_linear <- data.frame()
for(meal in list_meal){
  result <- stat_linear(data = input_data[input_data$meal==meal,], list_exp = list_exp, list_otc = list_otc, list_covar = list_covar, timepoint = meal)
  result <- data.frame(meal=meal,result)
  
  #' @:FDR-correction
  result$qval <- p.adjust(result$pval, method = "BH")
  result$sig_q <- ifelse(result$pval<0.05 & result$qval<0.05,"**",NA)
  result$sig_q[result$pval<0.05 & result$qval>=0.05] <- "*"
  out_linear <- rbind(out_linear, result)
}
out_linear.all <- out_linear
table(out_linear.all$meal, out_linear.all$outcome, out_linear.all$sig_q)


# stat (noT2D) ---------------------------- 
input_data <- database[database$otc_T2D==0,]
list_meal <- c("RG","WG")
list_exp <- setdiff(list_exp, "otc_T2D")

out_linear <- data.frame()
for(meal in list_meal){
  result <- stat_linear(data = input_data[input_data$meal==meal,], list_exp = list_exp, list_otc = list_otc, list_covar = list_covar, timepoint = meal)
  result <- data.frame(meal=meal,result)
  
  #' @:FDR-correction
  result$qval <- p.adjust(result$pval, method = "BH")
  result$sig_q <- ifelse(result$pval<0.05 & result$qval<0.05,"**",NA)
  result$sig_q[result$pval<0.05 & result$qval>=0.05] <- "*"
  out_linear <- rbind(out_linear, result)
}
out_linear.noT2D <- out_linear
table(out_linear.noT2D$meal, out_linear.noT2D$outcome, out_linear.noT2D$sig_q)


##------------------------------------------------------
setwd(paste0(workpath, "/part3_PGS"))
openxlsx::write.xlsx(list("stat_all"=out_linear.all, "stat_noT2D"=out_linear.noT2D, "data"=database), "stat_DRs_multimorbidity_linear.xlsx")




#' @:LADE-DRs [8 glucose-related diseases] 
##------------------------------------------------------
# data ------------------------------ 
setwd(paste0(workpath, "/part3_PGS"))
data_CGM <- openxlsx::read.xlsx("stat_multimorbidity_DRs_linear.xlsx", sheet = "data") 
data_CGM <- data_CGM[,-grep("otc_T2D", colnames(data_CGM))]


#' @:LADE
setwd(paste0(workpath, "/part2_multimorbid"))
data_disease <- openxlsx::read.xlsx("stat_multimorbidity_DailyTraits_linear_quartile.xlsx", sheet = "data")  
list_disease <- setdiff(colnames(data_disease)[grep("otc_", colnames(data_disease))],list_comorb)

setwd(paste(root,"/1_data", sep = ""))
data_onset <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = "onset_duration")  
data_onset <- data_onset[,c("id",list_disease)]

database <- merge(data_CGM, data_onset, by="id", all=F)   
print(paste0("N of sample size: ", length(unique(database$id))))


#' @:Setting
list_exp <- list_disease
list_otc <- c("ppge","iAUC","acc","peak")
list_covar <- c("age","sex") 


# stat (all) ---------------------------- 
input_data <- database
list_meal <- c("RG","WG")


#' @:all-samples
out_linear <- data.frame()
for(meal in list_meal){
  result <- stat_linear(data = input_data[input_data$meal==meal,], list_exp = list_exp, list_otc = list_otc, list_covar = list_covar, timepoint = meal)
  result <- data.frame(meal=meal,result)
  
  #' @:FDR-correction
  result$qval <- p.adjust(result$pval, method = "BH")
  result$sig_q <- ifelse(result$pval<0.05 & result$qval<0.05,"**",NA)
  result$sig_q[result$pval<0.05 & result$qval>=0.05] <- "*"
  out_linear <- rbind(out_linear, result)
}
out_linear.all <- out_linear
table(out_linear.all$meal, out_linear.all$outcome, out_linear.all$sig_q)


# stat (noT2D) ---------------------------- 
input_data <- database[database$otc_T2D==0,]
list_meal <- c("RG","WG")
list_exp <- setdiff(list_exp, "otc_T2D")

#' @:all-samples
out_linear <- data.frame()
for(meal in list_meal){
  result <- stat_linear(data = input_data[input_data$meal==meal,], list_exp = list_exp, list_otc = list_otc, list_covar = list_covar, timepoint = meal)
  result <- data.frame(meal=meal,result)
  
  #' @:FDR-correction
  result$qval <- p.adjust(result$pval, method = "BH")
  result$sig_q <- ifelse(result$pval<0.05 & result$qval<0.05,"**",NA)
  result$sig_q[result$pval<0.05 & result$qval>=0.05] <- "*"
  out_linear <- rbind(out_linear, result)
}
out_linear.noT2D <- out_linear
table(out_linear.noT2D$meal, out_linear.noT2D$outcome, out_linear.noT2D$sig_q)



##------------------------------------------------------
setwd(paste0(workpath, "/part3_PGS"))
openxlsx::write.xlsx(list("stat_all"=out_linear.all, "stat_noT2D"=out_linear.noT2D, "data"=database), "stat_DRs_LADE_linear.xlsx")




#' @:PGS-disease [Group comparison, 8 glucose-related diseases & comorbidity] 
#' @:wilcox [Cross-sectional, one point]
##------------------------------------------------------
# data ------------------------------ 
setwd(paste0(root, "/1_data"))
data_PGS.WPN1 <- openxlsx::read.xlsx("PGS_construction_WPN1.xlsx", sheet = 1)    
data_PGS <- openxlsx::read.xlsx("PGS_construction_GNHS.xlsx", sheet = 1)  
data_PGS$id <- gsub("F4","",data_PGS$id) 
data_disease <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = 1) 
data_disease <- data_disease[order(data_disease$time, decreasing = T),]
data_disease <- data_disease[!duplicated(data_disease$id),]


setwd(paste0(workpath, "/part3_PGS"))
a <- openxlsx::read.xlsx("stat_DRs_LADE_linear.xlsx", sheet = "data")   
list_disease <- colnames(a)[grep("otc_", colnames(a))] 
list_disease <- setdiff(list_disease, "otc_coMorbidity.glucose")

database <- merge(data_PGS[,c("id","PGS")], data_disease[,c("id",list_disease)], by="id", all=F) 
database <- database[grep("^NL", database$id),]
database$otc_coMorbidity.glucose <- apply(database[,list_disease],1,sum)
print(paste0("N of sample size: ", length(unique(database$id))))

list_disease <- c(list_disease,"otc_coMorbidity.glucose")


# stat (all) ---------------------------- 
input_data <- database 
output <- data.frame()

for(i in list_disease){
  submatr <- input_data[,c("PGS",i)]
  submatr$group <- ifelse(submatr[,i]==0, "non", "case")
  a <- data.frame(PGS=data_PGS.WPN1$PGS, group="ref")
  submatr <- rbind.fill(a, submatr)
  
  #' @:stat
  result1 <- stat_wilcox(data = submatr[submatr$group!="case",], group = "group", trait = "PGS")
  result1 <- data.frame(pair="non_ref",result1)
  names(result1) <- c("pair","trait","pval","g1_mean","g2_mean","g1_sd","g2_sd","method")
  
  result2 <- stat_wilcox(data = submatr[submatr$group!="ref",], group = "group", trait = "PGS")
  result2 <- data.frame(pair="case_non",result2)
  names(result2) <- c("pair","trait","pval","g1_mean","g2_mean","g1_sd","g2_sd","method")
  
  result3 <- stat_wilcox(data = submatr[submatr$group!="non",], group = "group", trait = "PGS")
  result3 <- data.frame(pair="case_ref",result3)
  names(result3) <- c("pair","trait","pval","g1_mean","g2_mean","g1_sd","g2_sd","method")
  
  result <- rbind(result1, result2, result3)
  a <- data.frame(table(submatr$group))
  result <- data.frame(disease=i, case_num=a$Freq[a$Var1=="case"], prevelance=100*a$Freq[a$Var1=="case"]/sum(a$Freq[a$Var1!="ref"]), result)
  output <- rbind.fill(output, result)
}
output <- output[order(output$pval, decreasing = F),]
output$qval <- p.adjust(output$pval, method = "BH")

output$sig_q <- ifelse(output$pval<0.05,"*",NA)
output$sig_q[output$qval<0.05 & output$pval<0.05] <- "**"
table(output$sig_q)
output.all <- output


# stat (noT2D) ---------------------------- 
input_data <- database[database$otc_T2D==0,]
list_disease <- setdiff(list_disease,"otc_T2D")
output <- data.frame()

for(i in list_disease){
  submatr <- input_data[,c("PGS",i)]
  submatr$group <- ifelse(submatr[,i]==0, "non", "case")
  a <- data.frame(PGS=data_PGS.WPN1$PGS, group="ref")
  submatr <- rbind.fill(a, submatr)
  
  #' @:stat
  result1 <- stat_wilcox(data = submatr[submatr$group!="case",], group = "group", trait = "PGS")
  result1 <- data.frame(pair="non_ref",result1)
  names(result1) <- c("pair","trait","pval","g1_mean","g2_mean","g1_sd","g2_sd","method")
  
  result2 <- stat_wilcox(data = submatr[submatr$group!="ref",], group = "group", trait = "PGS")
  result2 <- data.frame(pair="case_non",result2)
  names(result2) <- c("pair","trait","pval","g1_mean","g2_mean","g1_sd","g2_sd","method")
  
  result3 <- stat_wilcox(data = submatr[submatr$group!="non",], group = "group", trait = "PGS")
  result3 <- data.frame(pair="case_ref",result3)
  names(result3) <- c("pair","trait","pval","g1_mean","g2_mean","g1_sd","g2_sd","method")
  
  result <- rbind(result1, result2, result3)
  a <- data.frame(table(submatr$group))
  result <- data.frame(disease=i, case_num=a$Freq[a$Var1=="case"], prevelance=100*a$Freq[a$Var1=="case"]/sum(a$Freq[a$Var1!="ref"]), result)
  output <- rbind.fill(output, result)
}
output <- output[order(output$pval, decreasing = F),]
output$qval <- p.adjust(output$pval, method = "BH")

output$sig_q <- ifelse(output$pval<0.05,"*",NA)
output$sig_q[output$qval<0.05 & output$pval<0.05] <- "**"
table(output$sig_q)
output.noT2D <- output



##------------------------------------------------------
setwd(paste0(workpath, "/part3_PGS"))
openxlsx::write.xlsx(list("stat_all"=output.all, "stat_noT2D"=output.noT2D, "data"=database), "stat_PGS_Disease_wilcox.xlsx")




#' @:PGS-comorbidity
#' @:linear [Cross-sectional, one point] 
##------------------------------------------------------
# data ------------------------------ 
setwd(paste0(root, "/1_data")) 
data_disease <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = 1) 
data_disease <- data_disease[order(data_disease$time, decreasing = T),]
data_disease <- data_disease[!duplicated(data_disease$id),]

setwd(paste0(workpath, "/part3_PGS"))
database <- openxlsx::read.xlsx("stat_PGS_Disease_wilcox.xlsx", sheet = "data")
database <- merge(database, data_disease[,c("id","age","sex","BMI")], by="id", all.x = T)
print(paste0("N of sample size: ", length(unique(database$id))))
table(database$otc_coMorbidity.glucose)


#' @:tertile
database$otc_coMorbidity.glucose2 <- ifelse(database$otc_coMorbidity.glucose>0,1,0)
database$otc_coMorbidity.glucose2[database$otc_coMorbidity.glucose>1] <- 2
database$otc_coMorbidity.glucose2[database$otc_coMorbidity.glucose>3] <- 3
table(database$otc_coMorbidity.glucose2)


#' @:Setting
list_otc <- "PGS"
list_covar <- c("age","sex") 


# function -----------------------------
stat_linear_factor <- function(data, list_exp, list_otc, list_covar, timepoint){
  
  output_linear <- data.frame()
  for(exp in list_exp){
    for(otc in list_otc){
      
      formula1 <- paste(otc,"~ as.factor(",exp,")")
      formula2 <- paste(list_covar, collapse=" + ")
      formula <- paste(formula1, formula2, sep=" + ")
      result <- summary(lm(as.formula(formula), data=data))  
      
      result1 <- data.frame(result$coefficients); names(result1) <- c("beta","se","t","pval")
      result1 <- result1[grep("as.factor", rownames(result1)),]
      result1 <- data.frame(group=seq(1,nrow(result1),1),result1)
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


# stat (conti & tertile) ---------------------------- 
#' @:continuous
list_exp <- c("otc_coMorbidity.glucose")

input_data <- database 
input_data$PGS <- scale(input_data$PGS)
out_linear.all <- stat_linear(data = input_data, list_exp = list_exp, list_otc = list_otc, list_covar = list_covar)

input_data <- database[database$otc_T2D==0,]
input_data$PGS <- scale(input_data$PGS)
list_exp <- setdiff(list_exp,"otc_T2D") 
out_linear.noT2D <- stat_linear(data = input_data, list_exp = list_exp, list_otc = list_otc, list_covar = list_covar)

output_conti <- rbind(data.frame(group="all", out_linear.all), data.frame(group="noT2D", out_linear.noT2D))


#' @:tertile
list_exp <- c("otc_coMorbidity.glucose2")

input_data <- database 
input_data$PGS <- scale(input_data$PGS)
out_linear.all <- stat_linear_factor(data = input_data, list_exp = list_exp, list_otc = list_otc, list_covar = list_covar)

input_data <- database[database$otc_T2D==0,]
input_data$PGS <- scale(input_data$PGS)
list_exp <- setdiff(list_exp,"otc_T2D") 
out_linear.noT2D <- stat_linear_factor(data = input_data, list_exp = list_exp, list_otc = list_otc, list_covar = list_covar)

output_tertile <- rbind(data.frame(group="all", out_linear.all), data.frame(group="noT2D", out_linear.noT2D))


##------------------------------------------------------
setwd(paste0(workpath, "/part3_PGS"))
openxlsx::write.xlsx(list("stat_conti"=output_conti, "stat_ter"=output_tertile, "data"=database), "stat_PGS_multimorbidity_linear.xlsx")




#' @:PGS-LADE [8 glucose-related diseases] 
#' @:linear
##------------------------------------------------------
# data ------------------------------ 
setwd(paste0(root, "/1_data")) 
data_disease <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = 1) 
data_disease <- data_disease[order(data_disease$time, decreasing = T),]
data_disease <- data_disease[!duplicated(data_disease$id),]

setwd(paste0(workpath, "/part3_PGS"))
database <- openxlsx::read.xlsx("stat_PGS_Disease_wilcox.xlsx", sheet = "data")
database <- merge(database, data_disease[,c("id","age","sex","BMI")], by="id", all.x = T)
print(paste0("N of sample size: ", length(unique(database$id))))


#' @:Setting
list_exp <- setdiff(colnames(database)[grep("otc_", colnames(database))],"otc_coMorbidity.glucose")
list_otc <- "PGS"
list_covar <- c("age","sex") 


# stat (all) ---------------------------- 
input_data <- database 
input_data$PGS <- scale(input_data$PGS)

#' @:all-samples
out_linear.all <- stat_linear(data = input_data, list_exp = list_exp, list_otc = list_otc, list_covar = list_covar)
out_linear.all <- data.frame(out_linear.all, qval=NA, sig_q=NA)
for(i in list_otc){
  out_linear.all$qval[out_linear.all$outcome==i] <- p.adjust(out_linear.all$pval[out_linear.all$outcome==i], method = "BH")
  out_linear.all$sig_q <- ifelse(out_linear.all$pval<0.05 & out_linear.all$qval<0.05,"**",NA)
  out_linear.all$sig_q[out_linear.all$pval<0.05 & out_linear.all$qval>=0.05] <- "*"
}
table(out_linear.all$outcome, out_linear.all$sig_q)


# stat (noT2D) ---------------------------- 
input_data <- database[database$otc_T2D==0,]
input_data$PGS <- scale(input_data$PGS)
list_exp <- setdiff(list_exp,"otc_T2D")


#' @:all-samples
out_linear.T2D <- stat_linear(data = input_data, list_exp = list_exp, list_otc = list_otc, list_covar = list_covar)
out_linear.T2D <- data.frame(out_linear.T2D, qval=NA, sig_q=NA)
for(i in list_otc){
  out_linear.T2D$qval[out_linear.T2D$outcome==i] <- p.adjust(out_linear.T2D$pval[out_linear.T2D$outcome==i], method = "BH")
  out_linear.T2D$sig_q <- ifelse(out_linear.T2D$pval<0.05 & out_linear.T2D$qval<0.05,"**",NA)
  out_linear.T2D$sig_q[out_linear.T2D$pval<0.05 & out_linear.T2D$qval>=0.05] <- "*"
}
table(out_linear.T2D$outcome, out_linear.T2D$sig_q)


##------------------------------------------------------
setwd(paste0(workpath, "/part3_PGS"))
openxlsx::write.xlsx(list("stat_all"=out_linear.all, "stat_noT2D"=out_linear.T2D,"data"=database), "stat_PGS_LADE_linear.xlsx")













#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part4
#' @:Prediction [Prediction for daily traits and postprandial glycemic responses]
#' 
#' @:-----------------------------------------------------------------------------------------



if(!file.exists(paste0(workpath,"/part4_predict"))){
  dir.create(paste0(workpath,"/part4_predict")); print("Folder is built")
} else { print("Folder exists") }




#' @:Function
##------------------------------------------------------
# continuous ---------------------
model_rf_conti <- function(input_data, trait, list_x, formula, nfold, seed){
  library(caret)
  library(randomForest)
  
  set.seed(seed)
  
  kfold <- sample(rep(1:nfold,ceiling(nrow(input_data)/nfold))[1:nrow(input_data)])
  input_data <- data.frame(kfold=kfold, input_data)
  input_data <- input_data[order(input_data$kfold, decreasing = F),]
  
  output_pred <- data.frame(id=input_data$id, kfold=input_data$kfold, true=input_data[,trait], pred=NA)
  for(fold in 1:nfold){ 
    # print(paste("#-------- Round: ", fold,"/",nfold))
    
    train <- input_data[input_data$kfold!=fold,]
    test <- input_data[input_data$kfold==fold,]
    id <- test$id
    
    model <- randomForest(as.formula(formula), data = train)
    pred <- predict(model, test[,c(list_x)])
    output_pred$pred[output_pred$kfold==fold] <- pred
  }
  return(output_pred)
  
}

model_eval_conti <- function(data){ 
  
  data$true[data$true==0] <- min(data$true)*10^-15 
  r_spearman <- rcorr(data$pred, data$true, type = "spearman")
  r_pearson <- rcorr(data$pred, data$true, type = "pearson")
  MAPE <- (sum(abs((data$pred-data$true)/data$true))*100)/nrow(data)
  RMSE <- sqrt(sum((data$pred-data$true)^2/nrow(data)))
  
  SSE <- sum((data$pred-data$true)^2) # sum of squared errors, SSE
  SSR <- sum((data$pred-mean(data$true))^2) # sum of squares for regression, SSR
  SST <- SSE + SSR 
  Coefficient_of_determination <- 1 - (SSE / SST) 
  
  result_eval <- data.frame(spearman_r=r_spearman$r[2,1], spearman_p=r_spearman$P[2,1],
                            pearson_r=r_pearson$r[2,1], pearson_p=r_pearson$P[2,1],
                            MAPE=MAPE, RMSE=RMSE, Coefficient_of_determination = Coefficient_of_determination
  )
  return(result_eval)
}


# seed list ---------------------
set.seed(123456)
list_seed <- sample(1:1000,100, replace = F)


##------------------------------------------------------




#' @:Prediction1-1 [CGM-measured daily traits, all samples] 
##------------------------------------------------------
# data ------------------------
#' @:Disease-traits
setwd(paste0(workpath, "/part2_multimorbid"))
data_disease <- openxlsx::read.xlsx("stat_multimorbidity_DailyTraits_linear_quartile.xlsx", sheet = "data")
list_disease <- colnames(data_disease)[grep("otc_", colnames(data_disease))] 

setwd(paste(root,"/1_data", sep = ""))
data_traits <- openxlsx::read.xlsx("GNHS_traits_all.xlsx", sheet = "trait")
list_score <- colnames(data_traits)[grep("score", colnames(data_traits))] 

database <- merge(data_traits, data_disease[,c("sampleid", list_disease)], by="sampleid", all=F)
database <- database[grep("NL", database$id),] 
database <- database[,apply(!is.na(database),2,sum)>0]


#' @:CGM-daily
setwd(paste(root,"/1_data", sep = ""))
data_CGM <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "daily_mean")
data_CGM <- data_CGM[grep("NL",data_CGM$id),] 
data_CGM$id <- gsub("F4","",data_CGM$id)
 
database <- merge(database, data_CGM[,c("id",list_daily2)], by = "id", all = F)


# sort ----------------------
list_base <- c("otc_T2D","HbA1C","glucose","insulin","homa_ir","homa_beta") 

list_comorb <- c("otc_MASLD","ALT","AST","ALP","ratio_AST_ALT",
                 "otc_ObesOverwe","waist","hip","WHR","ratio_AG","PFAT_trunck","PFAT_total","PFAT_android","PFAT_gynoid","LAP","VAI",
                 "otc_Hypertension", "SBP","DBP","HR",
                 "otc_Dyslipidemia", "TC", "TG", "HDL", "LDL","ratio_TyG","ratio_TG_HDL",
                 "otc_KSD",
                 "otc_CKD","blood_Crea","urine_Crea",
                 "otc_coMorbidity.glucose"
) 
list_merge <- list(list_comorb)
list_name <- c("MMI-system")


# prediction (combination) ------------------
list_y <- list_daily2
input_data <- database

output_perf <- data.frame()
for(s in list_seed){
  print(paste0("N of Bootstraps: ", grep(s, list_seed), " / ", length(list_seed)))
  
  for(y in list_y){
    for(x in 1:length(list_merge)){
      
      print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
      list_x <- unique(c(list_base, list_merge[[x]]))
      
      #' @:imputation
      submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
      for(i in 2:ncol(submatr)){
        submatr[,i] <- as.numeric(submatr[,i])
        submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
      }
      
      #' @:modeling
      a <- paste(list_x, collapse = " + ")
      formula <- paste0(y," ~ ", a)
      result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10, seed = s))
      result <- data.frame(seed=s, outcome=y, predictor=paste0("T2D & ", list_name[x]), result)
      output_perf <- rbind.fill(output_perf, result)
    }  
  }
}

output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.comb <- output_perf


# prediction (base-T2D) ---------------
list_y <- list_daily2
input_data <- database

output_perf <- data.frame()
for(y in list_y){
  
  print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
  list_x <- list_base
  
  #' @:imputation
  submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
  for(i in 2:ncol(submatr)){
    submatr[,i] <- as.numeric(submatr[,i])
    submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
  }
  
  #' @:modeling
  a <- paste(list_x, collapse = " + ")
  formula <- paste0(y," ~ ", a)
  result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
  result <- data.frame(outcome=y, predictor="T2D", result)
  output_perf <- rbind.fill(output_perf, result)
  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.base <- output_perf


# merge ---------------------------
a <- output_perf.base
names(a)[2:ncol(a)] <- paste0(colnames(a)[2:ncol(a)], "_Base")
b <- output_perf.comb
names(b)[2:ncol(b)] <- paste0(colnames(b)[2:ncol(b)], "_Comb")

output_merge <- merge(a, b, by="outcome", all.y=T)
output_merge$spearman_r_Delta <- output_merge$spearman_r_Comb - output_merge$spearman_r_Base
output_merge$pearson_r_Delta <- output_merge$pearson_r_Comb - output_merge$pearson_r_Base
output_merge$MAPE_Delta <- output_merge$MAPE_Comb - output_merge$MAPE_Base
output_merge$RMSE_Delta <- output_merge$RMSE_Comb - output_merge$RMSE_Base
output_merge$Coefficient_of_determination_Delta <- output_merge$Coefficient_of_determination_Comb - output_merge$Coefficient_of_determination_Base

output_merge$spearman_r_Binary <- ifelse(output_merge$spearman_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)

output_merge$pearson_r_Binary <- ifelse(output_merge$pearson_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$pearson_r_Binary)

output_merge$MAPE_Binary <- ifelse(output_merge$MAPE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$MAPE_Binary)

output_merge$RMSE_Binary <- ifelse(output_merge$RMSE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$RMSE_Binary)

output_merge$Coefficient_of_determination_Binary <- ifelse(output_merge$Coefficient_of_determination_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)


##------------------------------------------------------
setwd(paste0(workpath, "/part4_predict"))
openxlsx::write.xlsx(list("perf_base"=output_perf.base, "perf_comb"=output_perf.comb, "perf_merge"=output_merge, "data"=database), "stat_Comorbidity_Daily_predict_all.xlsx")




#' @:Prediction1-2 [CGM-measured daily traits, non-diabetic samples] 
##------------------------------------------------------
# data ------------------------
#' @:Disease-traits
setwd(paste0(workpath, "/part2_multimorbid"))
data_disease <- openxlsx::read.xlsx("stat_multimorbidity_DailyTraits_linear_quartile.xlsx", sheet = "data")
list_disease <- colnames(data_disease)[grep("otc_", colnames(data_disease))] 

setwd(paste(root,"/1_data", sep = ""))
data_traits <- openxlsx::read.xlsx("GNHS_traits_all.xlsx", sheet = "trait")
list_score <- colnames(data_traits)[grep("score", colnames(data_traits))] 

database <- merge(data_traits, data_disease[,c("sampleid", list_disease)], by="sampleid", all=F)
database <- database[grep("NL", database$id),] 
database <- database[,apply(!is.na(database),2,sum)>0]


#' @:CGM-daily
setwd(paste(root,"/1_data", sep = ""))
data_CGM <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "daily_mean")
data_CGM <- data_CGM[grep("NL",data_CGM$id),] 
data_CGM$id <- gsub("F4","",data_CGM$id)

database <- merge(database, data_CGM[,c("id",list_daily2)], by = "id", all = F)
database <- database[database$otc_T2D==0,]

# sort ----------------------
list_base <- c("HbA1C","glucose","insulin","homa_ir","homa_beta") 

list_comorb <- c("otc_MASLD","ALT","AST","ALP","ratio_AST_ALT",
                 "otc_ObesOverwe","waist","hip","WHR","ratio_AG","PFAT_trunck","PFAT_total","PFAT_android","PFAT_gynoid","LAP","VAI",
                 "otc_Hypertension", "SBP","DBP","HR",
                 "otc_Dyslipidemia", "TC", "TG", "HDL", "LDL","ratio_TyG","ratio_TG_HDL",
                 "otc_KSD",
                 "otc_CKD","blood_Crea","urine_Crea",
                 "otc_coMorbidity.glucose"
) 
list_merge <- list(list_comorb)
list_name <- c("MMI-system")


# prediction (combination) ------------------
list_y <- list_daily2
input_data <- database

output_perf <- data.frame()
for(y in list_y){
  for(x in 1:length(list_merge)){
    
    print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
    list_x <- unique(c(list_base, list_merge[[x]]))
    
    #' @:imputation
    submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
    for(i in 2:ncol(submatr)){
      submatr[,i] <- as.numeric(submatr[,i])
      submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
    }
    
    #' @:modeling
    a <- paste(list_x, collapse = " + ")
    formula <- paste0(y," ~ ", a)
    result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
    result <- data.frame(outcome=y, predictor=paste0("T2D & ", list_name[x]), result)
    output_perf <- rbind.fill(output_perf, result)
  }  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.comb <- output_perf


# prediction (base-T2D) ---------------
list_y <- list_daily2
input_data <- database

output_perf <- data.frame()
for(y in list_y){
  
  print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
  list_x <- list_base
  
  #' @:imputation
  submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
  for(i in 2:ncol(submatr)){
    submatr[,i] <- as.numeric(submatr[,i])
    submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
  }
  
  #' @:modeling
  a <- paste(list_x, collapse = " + ")
  formula <- paste0(y," ~ ", a)
  result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
  result <- data.frame(outcome=y, predictor="T2D", result)
  output_perf <- rbind.fill(output_perf, result)
  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.base <- output_perf


# merge ---------------------------
a <- output_perf.base
names(a)[2:ncol(a)] <- paste0(colnames(a)[2:ncol(a)], "_Base")
b <- output_perf.comb
names(b)[2:ncol(b)] <- paste0(colnames(b)[2:ncol(b)], "_Comb")

output_merge <- merge(a, b, by="outcome", all.y=T)
output_merge$spearman_r_Delta <- output_merge$spearman_r_Comb - output_merge$spearman_r_Base
output_merge$pearson_r_Delta <- output_merge$pearson_r_Comb - output_merge$pearson_r_Base
output_merge$MAPE_Delta <- output_merge$MAPE_Comb - output_merge$MAPE_Base
output_merge$RMSE_Delta <- output_merge$RMSE_Comb - output_merge$RMSE_Base
output_merge$Coefficient_of_determination_Delta <- output_merge$Coefficient_of_determination_Comb - output_merge$Coefficient_of_determination_Base

output_merge$spearman_r_Binary <- ifelse(output_merge$spearman_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)

output_merge$pearson_r_Binary <- ifelse(output_merge$pearson_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$pearson_r_Binary)

output_merge$MAPE_Binary <- ifelse(output_merge$MAPE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$MAPE_Binary)

output_merge$RMSE_Binary <- ifelse(output_merge$RMSE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$RMSE_Binary)

output_merge$Coefficient_of_determination_Binary <- ifelse(output_merge$Coefficient_of_determination_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)


##------------------------------------------------------
setwd(paste0(workpath, "/part4_predict"))
openxlsx::write.xlsx(list("perf_base"=output_perf.base, "perf_comb"=output_perf.comb, "perf_merge"=output_merge, "data"=database), "stat_Comorbidity_Daily_predict_noT2D.xlsx")




#' @:Prediction2-1 [Postprandial responses, all samples] 
##------------------------------------------------------
# data ------------------------
#' @:Disease-traits
setwd(paste0(workpath, "/part2_multimorbid"))
data_disease <- openxlsx::read.xlsx("stat_multimorbidity_DailyTraits_linear_quartile.xlsx", sheet = "data")
list_disease <- colnames(data_disease)[grep("otc_", colnames(data_disease))] 

setwd(paste(root,"/1_data", sep = ""))
data_traits <- openxlsx::read.xlsx("GNHS_traits_all.xlsx", sheet = "trait")
list_score <- colnames(data_traits)[grep("score", colnames(data_traits))] 

database <- merge(data_traits, data_disease[,c("sampleid", list_disease)], by="sampleid", all=F)
database <- database[grep("NL", database$id),] 
database <- database[,apply(!is.na(database),2,sum)>0]


#' @:Dietary-responses
setwd(paste0(root,"/1_data")) 
data_CGM.RG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "RG_mean") 
data_CGM.RG$SampleID <- substr(data_CGM.RG$SampleID,3,8)
data_CGM.WG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "WG_mean") 
data_CGM.WG$SampleID <- substr(data_CGM.WG$SampleID,3,8)


#' @:merge
database <- merge(data_CGM.RG, database, by.x="SampleID", by.y = "id", all.y = T)
database <- merge(data_CGM.WG, database, by.x="SampleID", all.y = T)
names(database)[1] <- "id"

# sort ----------------------
list_base <- c("otc_T2D","HbA1C","glucose","insulin","homa_ir","homa_beta") 

list_comorb <- c("otc_MASLD","ALT","AST","ALP","ratio_AST_ALT",
                 "otc_ObesOverwe","waist","hip","WHR","ratio_AG","PFAT_trunck","PFAT_total","PFAT_android","PFAT_gynoid","LAP","VAI",
                 "otc_Hypertension", "SBP","DBP","HR",
                 "otc_Dyslipidemia", "TC", "TG", "HDL", "LDL","ratio_TyG","ratio_TG_HDL",
                 "otc_KSD",
                 "otc_CKD","blood_Crea","urine_Crea",
                 "otc_coMorbidity.glucose"
) 
list_merge <- list(list_comorb)
list_name <- c("MMI-system")
list_trait <- colnames(database)[c(grep("^RG_", colnames(database)), grep("^WG_", colnames(database)))]


# prediction (combination) ------------------
list_y <- list_trait
input_data <- database

output_perf <- data.frame()
for(y in list_y){ 
  for(x in 1:length(list_merge)){
    
    print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
    list_x <- unique(c(list_base, list_merge[[x]]))
    
    #' @:imputation
    submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
    for(i in 2:ncol(submatr)){
      submatr[,i] <- as.numeric(submatr[,i])
      submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
    }
    
    #' @:modeling
    a <- paste(list_x, collapse = " + ")
    formula <- paste0(y," ~ ", a)
    result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
    result <- data.frame(outcome=y, predictor=paste0("T2D & ", list_name[x]), result)
    output_perf <- rbind.fill(output_perf, result)
  }  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.comb <- output_perf


# prediction (base-T2D) ---------------
list_y <- list_trait
input_data <- database

output_perf <- data.frame()
for(y in list_y){
  
  print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
  list_x <- list_base
  
  #' @:imputation
  submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
  for(i in 2:ncol(submatr)){
    submatr[,i] <- as.numeric(submatr[,i])
    submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
  }
  
  #' @:modeling
  a <- paste(list_x, collapse = " + ")
  formula <- paste0(y," ~ ", a)
  result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
  result <- data.frame(outcome=y, predictor="T2D", result)
  output_perf <- rbind.fill(output_perf, result)
  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.base <- output_perf


# merge ---------------------------
a <- output_perf.base
names(a)[2:ncol(a)] <- paste0(colnames(a)[2:ncol(a)], "_Base")
b <- output_perf.comb
names(b)[2:ncol(b)] <- paste0(colnames(b)[2:ncol(b)], "_Comb")

output_merge <- merge(a, b, by="outcome", all.y=T)
output_merge$spearman_r_Delta <- output_merge$spearman_r_Comb - output_merge$spearman_r_Base
output_merge$pearson_r_Delta <- output_merge$pearson_r_Comb - output_merge$pearson_r_Base
output_merge$MAPE_Delta <- output_merge$MAPE_Comb - output_merge$MAPE_Base
output_merge$RMSE_Delta <- output_merge$RMSE_Comb - output_merge$RMSE_Base
output_merge$Coefficient_of_determination_Delta <- output_merge$Coefficient_of_determination_Comb - output_merge$Coefficient_of_determination_Base

output_merge$spearman_r_Binary <- ifelse(output_merge$spearman_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)

output_merge$pearson_r_Binary <- ifelse(output_merge$pearson_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$pearson_r_Binary)

output_merge$MAPE_Binary <- ifelse(output_merge$MAPE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$MAPE_Binary)

output_merge$RMSE_Binary <- ifelse(output_merge$RMSE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$RMSE_Binary)

output_merge$Coefficient_of_determination_Binary <- ifelse(output_merge$Coefficient_of_determination_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)


##------------------------------------------------------
setwd(paste0(workpath, "/part4_predict"))
openxlsx::write.xlsx(list("perf_base"=output_perf.base, "perf_comb"=output_perf.comb, "perf_merge"=output_merge, "data"=database), "stat_Comorbidity_DRs_predict_all.xlsx")




#' @:Prediction2-2 [Postprandial responses, non-diabetic samples] 
##------------------------------------------------------
# data ------------------------
#' @:Disease-traits
setwd(paste0(workpath, "/part2_multimorbid"))
data_disease <- openxlsx::read.xlsx("stat_multimorbidity_DailyTraits_linear_quartile.xlsx", sheet = "data")
list_disease <- colnames(data_disease)[grep("otc_", colnames(data_disease))] 

setwd(paste(root,"/1_data", sep = ""))
data_traits <- openxlsx::read.xlsx("GNHS_traits_all.xlsx", sheet = "trait")
list_score <- colnames(data_traits)[grep("score", colnames(data_traits))] 

database <- merge(data_traits, data_disease[,c("sampleid", list_disease)], by="sampleid", all=F)
database <- database[grep("NL", database$id),] 
database <- database[,apply(!is.na(database),2,sum)>0]


#' @:Dietary-responses
setwd(paste0(root,"/1_data")) 
data_CGM.RG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "RG_mean") 
data_CGM.RG$SampleID <- substr(data_CGM.RG$SampleID,3,8)
data_CGM.WG <- openxlsx::read.xlsx("GNHS_CGMdata_clean.xlsx", sheet = "WG_mean") 
data_CGM.WG$SampleID <- substr(data_CGM.WG$SampleID,3,8)


#' @:merge
database <- merge(data_CGM.RG, database, by.x="SampleID", by.y = "id", all.y = T)
database <- merge(data_CGM.WG, database, by.x="SampleID", all.y = T)
database <- database[database$otc_T2D==0,]
names(database)[1] <- "id"

# sort ----------------------
list_base <- c("HbA1C","glucose","insulin","homa_ir","homa_beta") 

list_comorb <- c("otc_MASLD","ALT","AST","ALP","ratio_AST_ALT",
                 "otc_ObesOverwe","waist","hip","WHR","ratio_AG","PFAT_trunck","PFAT_total","PFAT_android","PFAT_gynoid","LAP","VAI",
                 "otc_Hypertension", "SBP","DBP","HR",
                 "otc_Dyslipidemia", "TC", "TG", "HDL", "LDL","ratio_TyG","ratio_TG_HDL",
                 "otc_KSD",
                 "otc_CKD","blood_Crea","urine_Crea",
                 "otc_coMorbidity.glucose"
) 
list_merge <- list(list_comorb)
list_name <- c("MMI-system")
list_trait <- colnames(database)[c(grep("^RG_", colnames(database)), grep("^WG_", colnames(database)))]


# prediction (combination) ------------------
list_y <- list_trait
input_data <- database

output_perf <- data.frame()
for(y in list_y){ 
  for(x in 1:length(list_merge)){
    
    print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
    list_x <- unique(c(list_base, list_merge[[x]]))
    
    #' @:imputation
    submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
    for(i in 2:ncol(submatr)){
      submatr[,i] <- as.numeric(submatr[,i])
      submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
    }
    
    #' @:modeling
    a <- paste(list_x, collapse = " + ")
    formula <- paste0(y," ~ ", a)
    result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
    result <- data.frame(outcome=y, predictor=paste0("T2D & ", list_name[x]), result)
    output_perf <- rbind.fill(output_perf, result)
  }  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.comb <- output_perf


# prediction (base-T2D) ---------------
list_y <- list_trait
input_data <- database

output_perf <- data.frame()
for(y in list_y){
  
  print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
  list_x <- list_base
  
  #' @:imputation
  submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
  for(i in 2:ncol(submatr)){
    submatr[,i] <- as.numeric(submatr[,i])
    submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
  }
  
  #' @:modeling
  a <- paste(list_x, collapse = " + ")
  formula <- paste0(y," ~ ", a)
  result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
  result <- data.frame(outcome=y, predictor="T2D", result)
  output_perf <- rbind.fill(output_perf, result)
  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.base <- output_perf


# merge ---------------------------
a <- output_perf.base
names(a)[2:ncol(a)] <- paste0(colnames(a)[2:ncol(a)], "_Base")
b <- output_perf.comb
names(b)[2:ncol(b)] <- paste0(colnames(b)[2:ncol(b)], "_Comb")

output_merge <- merge(a, b, by="outcome", all.y=T)
output_merge$spearman_r_Delta <- output_merge$spearman_r_Comb - output_merge$spearman_r_Base
output_merge$pearson_r_Delta <- output_merge$pearson_r_Comb - output_merge$pearson_r_Base
output_merge$MAPE_Delta <- output_merge$MAPE_Comb - output_merge$MAPE_Base
output_merge$RMSE_Delta <- output_merge$RMSE_Comb - output_merge$RMSE_Base
output_merge$Coefficient_of_determination_Delta <- output_merge$Coefficient_of_determination_Comb - output_merge$Coefficient_of_determination_Base

output_merge$spearman_r_Binary <- ifelse(output_merge$spearman_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)

output_merge$pearson_r_Binary <- ifelse(output_merge$pearson_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$pearson_r_Binary)

output_merge$MAPE_Binary <- ifelse(output_merge$MAPE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$MAPE_Binary)

output_merge$RMSE_Binary <- ifelse(output_merge$RMSE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$RMSE_Binary)

output_merge$Coefficient_of_determination_Binary <- ifelse(output_merge$Coefficient_of_determination_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)


##------------------------------------------------------
setwd(paste0(workpath, "/part4_predict"))
openxlsx::write.xlsx(list("perf_base"=output_perf.base, "perf_comb"=output_perf.comb, "perf_merge"=output_merge, "data"=database), "stat_Comorbidity_DRs_predict_noT2D.xlsx")




#' @:Prediction3-1 [PGS, all samples] 
##------------------------------------------------------
# data ------------------------
#' @:Disease-traits
setwd(paste0(workpath, "/part2_multimorbid"))
data_disease <- openxlsx::read.xlsx("stat_multimorbidity_DailyTraits_linear_quartile.xlsx", sheet = "data")
list_disease <- colnames(data_disease)[grep("otc_", colnames(data_disease))] 

setwd(paste(root,"/1_data", sep = ""))
data_traits <- openxlsx::read.xlsx("GNHS_traits_all.xlsx", sheet = "trait")
list_score <- colnames(data_traits)[grep("score", colnames(data_traits))] 

database <- merge(data_traits, data_disease[,c("sampleid", list_disease)], by="sampleid", all=F)
database <- database[grep("NL", database$id),] 
database <- database[,apply(!is.na(database),2,sum)>0]


#' @:PGS
setwd(paste0(root, "/1_data")) 
data_PGS <- openxlsx::read.xlsx("PGS_construction_GNHS.xlsx", sheet = 1)  
data_PGS$id <- gsub("F4","",data_PGS$id) 


#' @:merge
database <- merge(data_PGS[,c("id","PGS")], database, by = "id", all.y = T)


# sort ----------------------
list_base <- c("otc_T2D","HbA1C","glucose","insulin","homa_ir","homa_beta") 
list_comorb <- c("otc_MASLD","ALT","AST","ALP","ratio_AST_ALT",
                 "otc_ObesOverwe","waist","hip","WHR","ratio_AG","PFAT_trunck","PFAT_total","PFAT_android","PFAT_gynoid","LAP","VAI",
                 "otc_Hypertension", "SBP","DBP","HR",
                 "otc_Dyslipidemia", "TC", "TG", "HDL", "LDL","ratio_TyG","ratio_TG_HDL",
                 "otc_KSD",
                 "otc_CKD","blood_Crea","urine_Crea",
                 "otc_coMorbidity.glucose"
) 
list_merge <- list(list_comorb)
list_name <- c("MMI-system")
list_trait <- "PGS"


# prediction (combination) ------------------
list_y <- list_trait
input_data <- database

output_perf <- data.frame()
for(y in list_y){ 
  for(x in 1:length(list_merge)){
    
    print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
    list_x <- unique(c(list_base, list_merge[[x]]))
    
    #' @:imputation
    submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
    for(i in 2:ncol(submatr)){
      submatr[,i] <- as.numeric(submatr[,i])
      submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
    }
    
    #' @:modeling
    a <- paste(list_x, collapse = " + ")
    formula <- paste0(y," ~ ", a)
    result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
    result <- data.frame(outcome=y, predictor=paste0("T2D & ", list_name[x]), result)
    output_perf <- rbind.fill(output_perf, result)
  }  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.comb <- output_perf


# prediction (base-T2D) ---------------
list_y <- list_trait
input_data <- database

output_perf <- data.frame()
for(y in list_y){
  
  print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
  list_x <- list_base
  
  #' @:imputation
  submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
  for(i in 2:ncol(submatr)){
    submatr[,i] <- as.numeric(submatr[,i])
    submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
  }
  
  #' @:modeling
  a <- paste(list_x, collapse = " + ")
  formula <- paste0(y," ~ ", a)
  result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
  result <- data.frame(outcome=y, predictor="T2D", result)
  output_perf <- rbind.fill(output_perf, result)
  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.base <- output_perf


# merge ---------------------------
a <- output_perf.base
names(a)[2:ncol(a)] <- paste0(colnames(a)[2:ncol(a)], "_Base")
b <- output_perf.comb
names(b)[2:ncol(b)] <- paste0(colnames(b)[2:ncol(b)], "_Comb")

output_merge <- merge(a, b, by="outcome", all.y=T)
output_merge$spearman_r_Delta <- output_merge$spearman_r_Comb - output_merge$spearman_r_Base
output_merge$pearson_r_Delta <- output_merge$pearson_r_Comb - output_merge$pearson_r_Base
output_merge$MAPE_Delta <- output_merge$MAPE_Comb - output_merge$MAPE_Base
output_merge$RMSE_Delta <- output_merge$RMSE_Comb - output_merge$RMSE_Base
output_merge$Coefficient_of_determination_Delta <- output_merge$Coefficient_of_determination_Comb - output_merge$Coefficient_of_determination_Base

output_merge$spearman_r_Binary <- ifelse(output_merge$spearman_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)

output_merge$pearson_r_Binary <- ifelse(output_merge$pearson_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$pearson_r_Binary)

output_merge$MAPE_Binary <- ifelse(output_merge$MAPE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$MAPE_Binary)

output_merge$RMSE_Binary <- ifelse(output_merge$RMSE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$RMSE_Binary)

output_merge$Coefficient_of_determination_Binary <- ifelse(output_merge$Coefficient_of_determination_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)


##------------------------------------------------------
setwd(paste0(workpath, "/part4_predict"))
openxlsx::write.xlsx(list("perf_base"=output_perf.base, "perf_comb"=output_perf.comb, "perf_merge"=output_merge, "data"=database), "stat_Comorbidity_PGS_predict_all.xlsx")




#' @:Prediction3-2 [PGS, non-diabetic samples] 
##------------------------------------------------------
# data ------------------------
#' @:Disease-traits
setwd(paste0(workpath, "/part2_multimorbid"))
data_disease <- openxlsx::read.xlsx("stat_multimorbidity_DailyTraits_linear_quartile.xlsx", sheet = "data")
list_disease <- colnames(data_disease)[grep("otc_", colnames(data_disease))] 

setwd(paste(root,"/1_data", sep = ""))
data_traits <- openxlsx::read.xlsx("GNHS_traits_all.xlsx", sheet = "trait")
list_score <- colnames(data_traits)[grep("score", colnames(data_traits))] 

database <- merge(data_traits, data_disease[,c("sampleid", list_disease)], by="sampleid", all=F)
database <- database[grep("NL", database$id),] 
database <- database[,apply(!is.na(database),2,sum)>0]


#' @:PGS
setwd(paste0(root, "/1_data")) 
data_PGS <- openxlsx::read.xlsx("PGS_construction_GNHS.xlsx", sheet = 1)  
data_PGS$id <- gsub("F4","",data_PGS$id) 


#' @:merge
database <- merge(data_PGS[,c("id","PGS")], database, by = "id", all.y = T)
database <- database[database$otc_T2D==0,]


# sort ----------------------
list_base <- c("HbA1C","glucose","insulin","homa_ir","homa_beta") 
list_comorb <- c("otc_MASLD","ALT","AST","ALP","ratio_AST_ALT",
                 "otc_ObesOverwe","waist","hip","WHR","ratio_AG","PFAT_trunck","PFAT_total","PFAT_android","PFAT_gynoid","LAP","VAI",
                 "otc_Hypertension", "SBP","DBP","HR",
                 "otc_Dyslipidemia", "TC", "TG", "HDL", "LDL","ratio_TyG","ratio_TG_HDL",
                 "otc_KSD",
                 "otc_CKD","blood_Crea","urine_Crea",
                 "otc_coMorbidity.glucose"
) 
list_merge <- list(list_comorb)
list_name <- c("MMI-system")
list_trait <- "PGS"


# prediction (combination) ------------------
list_y <- list_trait
input_data <- database

output_perf <- data.frame()
for(y in list_y){ 
  for(x in 1:length(list_merge)){
    
    print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
    list_x <- unique(c(list_base, list_merge[[x]]))
    
    #' @:imputation
    submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
    for(i in 2:ncol(submatr)){
      submatr[,i] <- as.numeric(submatr[,i])
      submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
    }
    
    #' @:modeling
    a <- paste(list_x, collapse = " + ")
    formula <- paste0(y," ~ ", a)
    result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
    result <- data.frame(outcome=y, predictor=paste0("T2D & ", list_name[x]), result)
    output_perf <- rbind.fill(output_perf, result)
  }  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.comb <- output_perf


# prediction (base-T2D) ---------------
list_y <- list_trait
input_data <- database

output_perf <- data.frame()
for(y in list_y){
  
  print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
  list_x <- list_base
  
  #' @:imputation
  submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
  for(i in 2:ncol(submatr)){
    submatr[,i] <- as.numeric(submatr[,i])
    submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
  }
  
  #' @:modeling
  a <- paste(list_x, collapse = " + ")
  formula <- paste0(y," ~ ", a)
  result <- model_eval_conti(model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10))
  result <- data.frame(outcome=y, predictor="T2D", result)
  output_perf <- rbind.fill(output_perf, result)
  
}
output_perf <- output_perf[order(output_perf$spearman_r, decreasing = T),]
output_perf <- output_perf[order(output_perf$predictor, decreasing = T),]
output_perf.base <- output_perf


# merge ---------------------------
a <- output_perf.base
names(a)[2:ncol(a)] <- paste0(colnames(a)[2:ncol(a)], "_Base")
b <- output_perf.comb
names(b)[2:ncol(b)] <- paste0(colnames(b)[2:ncol(b)], "_Comb")

output_merge <- merge(a, b, by="outcome", all.y=T)
output_merge$spearman_r_Delta <- output_merge$spearman_r_Comb - output_merge$spearman_r_Base
output_merge$pearson_r_Delta <- output_merge$pearson_r_Comb - output_merge$pearson_r_Base
output_merge$MAPE_Delta <- output_merge$MAPE_Comb - output_merge$MAPE_Base
output_merge$RMSE_Delta <- output_merge$RMSE_Comb - output_merge$RMSE_Base
output_merge$Coefficient_of_determination_Delta <- output_merge$Coefficient_of_determination_Comb - output_merge$Coefficient_of_determination_Base

output_merge$spearman_r_Binary <- ifelse(output_merge$spearman_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)

output_merge$pearson_r_Binary <- ifelse(output_merge$pearson_r_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$pearson_r_Binary)

output_merge$MAPE_Binary <- ifelse(output_merge$MAPE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$MAPE_Binary)

output_merge$RMSE_Binary <- ifelse(output_merge$RMSE_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$RMSE_Binary)

output_merge$Coefficient_of_determination_Binary <- ifelse(output_merge$Coefficient_of_determination_Delta>0,1,0)
table(output_merge$predictor_Comb, output_merge$spearman_r_Binary)


##------------------------------------------------------
setwd(paste0(workpath, "/part4_predict"))
openxlsx::write.xlsx(list("perf_base"=output_perf.base, "perf_comb"=output_perf.comb, "perf_merge"=output_merge, "data"=database), "stat_Comorbidity_PGS_predict_noT2D.xlsx")














#' @:-----------------------------------------------------------------------------------------
#' 
#' @:Part5
#' @:Comorbidity-Proteomics
#' 
#' @:-----------------------------------------------------------------------------------------


if(!file.exists(paste0(workpath,"/part5_proteomics"))){
  dir.create(paste0(workpath,"/part5_proteomics")); print("Folder is built")
} else { print("Folder exists") }




#' @:Comorbidity-Proteomics [comorbidity-related protein, CRPs]
#' @:mix-linear 
library(lmerTest)  
##-----------------------------------------------------------
# data ----------------------------
#' @:CGM-list_id
setwd(paste0(workpath, "/part1_disease"))
data_CGM <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "data")
list_id <- unique(data_CGM$id)


#' @:Proteomics
setwd(paste(root,"/1_data", sep = ""))
data.protein <- openxlsx::read.xlsx("GNHS_omics_proteomics.xlsx", sheet = "data") 
data.protein$followup[data.protein$followup=="baseline"] <- "F0"
data.protein$sampleid <- paste0(data.protein$followup, data.protein$sampleid) 
list_prot <- setdiff(colnames(data.protein), c("sampleid","age","sex","BMI","phase","time","followup"))
code_prot <- openxlsx::read.xlsx("GNHS_omics_proteomics.xlsx", sheet = "code")


#' @:comorbidity-merge
setwd(paste0(workpath, "/part1_disease"))
data <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "data") 
stat1 <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_all")
stat2 <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "stat_noT2D")
a <- unique(stat1$exposure[stat1$sig_q=="**" & !is.na(stat1$sig_q)])
b <- unique(stat2$exposure[stat2$sig_q=="**" & !is.na(stat2$sig_q)])
list_disease <- unique(c(a,b))

setwd(paste(root,"/1_data", sep = ""))
data_disease <- openxlsx::read.xlsx("GNHS_disease_all.xlsx", sheet = "disease")  
data_disease <- data_disease[,c("sampleid","id","age","sex","BMI","time", list_disease)] 
data_disease$otc_coMorbidity.glucose <- apply(data_disease[,list_disease],1,sum, na.rm=T) 

list_comorb <- "otc_coMorbidity.glucose"
database <- merge(data_disease[,c("sampleid","id","time","age","sex","BMI", list_comorb)], 
                  data.protein[,c("sampleid","phase",list_prot)], by="sampleid", all=F)
database <- database[database$id %in% list_id,]
for(i in list_prot){database[,i] <- scale(log2(database[,i]))}
table(database$time, database$phase)

list_time <- unique(database$time) 
list_covar <- c("age","sex","phase")
 

#' @:Sorting [Repeated measures]
sort_time <- data.frame()
list_id <- unique(database$id)
for(i in list_id){
  a <- data.frame(id=i, freq=sum(grepl(i, database$id)))
  sort_time <- rbind.fill(sort_time, a)
}
table(sort_time$freq)


# stat ---------------------------- 
list_x <- list_comorb
list_y <- list_prot

input_data <- database
output_mix <- data.frame() 

for(y in list_y){ 
  for(x in list_x){
    submatr <- input_data[!is.na(input_data[,y]),]   
    if(length(unique(submatr$phase))==1){
      list_covar2 <- setdiff(list_covar, "phase")
    }else{
      list_covar2 <- list_covar
    }
    
    formula1 <- paste0(y," ~ ", x)
    formula2 <- paste(list_covar2, collapse = " + ")
    formula <- paste(formula1, formula2,"(1|id)", sep = " + ")
    
    stat <- summary(lmerTest::lmer(as.formula(formula), data=submatr))
    result <- data.frame(stat$coefficients)[grep(x,rownames(stat$coefficients)),-4]; names(result) <- c("beta","se","df","pval")
    result1 <- data.frame(outcome=y, exposure=rownames(result), result, n=nrow(submatr), method="mixed-linear")
    output_mix <- rbind.fill(output_mix, result1)
  } 
}

output_mix$sig_p <- ifelse(output_mix$pval<0.05, "*", NA)
output_mix$sig_p[output_mix$pval<0.01] <- "**"
output_mix$sig_p[output_mix$pval<0.001] <- "***"


#' @:FDR
output_mix$qval <- NA
for(i in list_x){
  output_mix$qval[output_mix$exposure==i] <- p.adjust(output_mix$pval[output_mix$exposure==i], method = "BH")
}
output_mix$sig_q <- ifelse(output_mix$pval<0.05 & output_mix$qval<0.05, "**",NA)
output_mix$sig_q[output_mix$pval<0.05 & output_mix$qval>=0.05] <- "*"
output_mix <- merge(output_mix, code_prot, by.x = "outcome", by.y = "Uniprot", all.x=T)

output_mix <- output_mix[order(output_mix$pval, decreasing = F),]
output_mix <- output_mix[order(output_mix$outcome, decreasing = F),]
table(output_mix$exposure, output_mix$sig_q)
output_mix1 <- output_mix

##-----------------------------------------------------------
setwd(paste0(workpath, "/part5_proteomics"))
openxlsx::write.xlsx(list("stat"=output_mix1, "data_zlog2"=database), "stat_Comorbidity_proteomics_mixed.xlsx")




#' @:Protein-network [Dynamic changes: healthy, single, mild, severe]
#' @:pSC-correlation
##-----------------------------------------------------------
# data ---------------------------
setwd(paste0(workpath, "/part5_proteomics"))
database <- openxlsx::read.xlsx("stat_Comorbidity_proteomics_mixed.xlsx", sheet = "data_zlog2") 
valid_prot <- openxlsx::read.xlsx("valid_Comorbidity_proteomics_linear.xlsx", sheet = "stat")
list_prot <- valid_prot$outcome[!is.na(valid_prot$valid)]
for(i in list_prot){database[,i] <- as.numeric(database[,i])}

database$group <- ifelse(database$otc_coMorbidity.glucose==0,"healthy",NA)
database$group[database$otc_coMorbidity.glucose==1] <- "single"
database$group[database$otc_coMorbidity.glucose>1 & database$otc_coMorbidity.glucose<=3] <- "mild"
database$group[database$otc_coMorbidity.glucose>3] <- "severe"
table(database$time, database$group)
length(unique(database$id))


#' @:sort-visit-time
list_id <- unique(database$id)
database2 <- data.frame()

for(i in list_id){
  submatr <- database[database$id==i,]
  submatr <- submatr[order(submatr$time, decreasing = F),]
  submatr$time <- seq(1,nrow(submatr),1)
  database2 <- rbind.fill(database2, submatr)
}
table(database2$time, database2$group)


# function -----------------------
stat_network <- function(data){
  
  library(igraph)
  submatr <- data[data$pval<0.05 & abs(data$pSC)>0.2,]
  # submatr$pSC[submatr$pval>0.05 & abs(submatr$pSC)<=0.2] <- 0
  
  pSC_matrix <- submatr[,c("feat1","feat2","pSC")]
  input_data <- reshape2::dcast(pSC_matrix, feat1~feat2); nrow(input_data); ncol(input_data)
  rownames(input_data) <- input_data$feat1; input_data <- input_data[,-1]  
  input_data[is.na(input_data)] <- 0
  for(i in 1:nrow(input_data)){input_data[i,i] <- 1}
  
  
  #' @:transitivity------------------- 
  #' @:nodes-file                             
  num_nodes1 <- data.frame(table(submatr$feat1))
  num_nodes2 <- data.frame(table(submatr$feat2))
  num_nodes <- rbind(num_nodes1, num_nodes2) 
  
  nodes <- data.frame(node=unique(c(submatr$feat1, submatr$feat2))) 
  nodes <- merge(nodes, num_nodes, by.x = "node", by.y = "Var1", all.x=T)
  nodes <- aggregate(nodes$Freq, by=list(nodes$node), sum)
  names(nodes) <- c("node","Freq")
  nodes <- nodes[order(nodes$Freq, decreasing = T),]
  nodes <- rbind(nodes, data.frame(node="KSD", Freq=1))
  
  nodes$seq <- seq(0,nrow(nodes)-1) 
  nodes$angle <- 90 - 360 * (nodes$seq+0.5) / nrow(nodes)
  nodes$angle <- ifelse(nodes$angle < (-90), nodes$angle+180, nodes$angle)
  nodes$hjust <- ifelse(nodes$seq < round(nrow(nodes)/2), 0, 1) 
  
  #' @:edges
  edges <- data.frame(from=submatr$feat1, to=submatr$feat2, corr=submatr$pSC, qval=submatr$qval)
  edges$class <- ifelse(edges$corr>0, "Positive correlation", "Negative correlation")
  edges$class[edges$qval>0.05] <- "Insignificant"
  
  #' @:merge
  g <- tidygraph::tbl_graph(nodes=nodes, edges=edges)
  
  graph <- graph_from_data_frame(edges, vertices = data.frame(name = nodes$node), directed = TRUE)
  global_transitivity <- transitivity(graph, type = "barrat")
  global_transitivity[is.na(global_transitivity)] <- 0
  global_transitivity <- mean(global_transitivity)
  
  
  #' @:Global-toplogical-features------------------
  g1 <- graph.adjacency(as.matrix(input_data), weighted=TRUE, mode="undirected")
  g1 <- simplify(g1)
  node.label <- V(g1)$name
  
  print("Extract global toplogical features!")
  num.nodes <- vcount(g1) # node number
  num.edges <- ecount(g1) # edge number
  num.pos.edges <- sum(E(g1)$weight>0) # number of positive correlation edges
  num.neg.edges <- sum(E(g1)$weight<0) # number of negative correlation edges
  global.degree <- mean(degree(g1)) # global average degree
  global.density <- edge_density(g1, loops=FALSE) # Density
  global.diameter <- diameter(g1,directed = F,weights = NA) # Overall path
  global.edge.connecivity <- edge_connectivity(g1)
  global.cluster.coef <- transitivity(g1, type = "average") # Average clustering coefficient
  
  E(g1)$weight = abs(E(g1)$weight)
  global.average.path <- mean_distance(g1) # Average path length
  g2 <- g1
  global.clossness <- mean(closeness(g1)) # Closeness centrality
  global.betweeness <- mean(betweenness(g1)) # Betweeness centrality
  global.eigen.centrality <- mean(evcent(g1)$vector) # eigen_centrality
  
  
  #' @:output----------------------
  result <- data.frame(num.nodes=num.nodes,
                       num.edges=num.edges,
                       num.pos.edges=num.pos.edges,
                       num.neg.edges=num.neg.edges,
                       global.degree=global.degree,
                       global.density=global.density,
                       global.diameter=global.diameter,
                       global.edge.connecivity=global.edge.connecivity,
                       global.cluster.coef=global.cluster.coef,
                       global.average.path=global.average.path,
                       global.clossness=global.clossness,
                       global.betweeness=global.betweeness,
                       global.eigen.centrality=global.eigen.centrality,
                       global_transitivity=global_transitivity)
  return(result)
}


# stat ---------------------------
input_data <- database2
output_network <- data.frame()

list_time <- unique(input_data$time)
list_group <- unique(input_data$group)
list_covar <- c("age","sex")


for(t in list_time){
  for(g in list_group){
    print(paste("Time: ",t," | Group: ", g))
    submatr <- input_data[input_data$time==t & input_data$group==g, c(list_prot, list_covar)]
    stat <- stat_pSC(data = submatr, list1 = list_prot, list2 = list_covar, method = "spearman")
    result_network <- stat_network(data = stat)
    result_network <- data.frame(time=t, group=g, result_network)
    output_network <- rbind.fill(output_network, result_network)
  }
}


##-----------------------------------------------------------
setwd(paste0(workpath, "/part5_proteomics"))
openxlsx::write.xlsx(list("stat"=output_network, "data"=database2), "stat_Comorbidity_proteomics_network.xlsx")




#' @:GO-enrichment [validated protein list]
##-----------------------------------------------------------
# function ----------------------
stat_GOenrich <- function(gene_list, code=code_prot){ 
  
  library("clusterProfiler")
  library("topGO")
  library("Rgraphviz") 
  library("pathview")
  library("org.Hs.eg.db")
  
  # entrez_ids <- clusterProfiler::bitr(gene_list, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)
  go_enrich <- enrichGO(gene = gene_list,
                        OrgDb = org.Hs.eg.db,
                        keyType = "ENTREZID",
                        ont = "BP",  # 选择 "BP", "MF", 或 "CC"
                        pAdjustMethod = "BH",
                        pvalueCutoff = 0.05,
                        qvalueCutoff = 0.05)
  
  output_GO <- as.data.frame(go_enrich)
  output_GO$GeneRatio2 <- sapply(output_GO$GeneRatio, function(x){
    x2 <- strsplit(x,split = "/")
    ratio <- as.numeric(x2[[1]][1])/as.numeric(x2[[1]][2])
    return(ratio)
  })
  
  
  #' @:rename-GeneID
  a <- strsplit(output_GO$geneID, split = "/")
  output_GO$geneID.Uniprot <- NA
  output_GO$geneID.SYMBOL <- NA
  
  for(i in 1:length(a)){
    b <- a[[i]]
    c <- paste0(code$Uniprot[code$ENTREZID %in% b], collapse = ",")
    d <- paste0(code$SYMBOL[code$ENTREZID %in% b], collapse = ",")
    output_GO$geneID.Uniprot[i] <- c
    output_GO$geneID.SYMBOL[i] <- d
  }
  
  output_GO <- output_GO[order(output_GO$GeneRatio2, decreasing = T),]
  return(output_GO) 
}

# data --------------------------
setwd(paste0(workpath, "/part5_proteomics")) 
valid_prot <- openxlsx::read.xlsx("valid_Comorbidity_proteomics_linear.xlsx", sheet = "stat")
list_prot <- valid_prot$outcome[!is.na(valid_prot$valid)]  

setwd(paste0(root, "/1_data")) 
code_prot <- openxlsx::read.xlsx("GNHS_omics_proteomics.xlsx", sheet = "code")

#' @:stat
list <- code_prot$ENTREZID[code_prot$Uniprot %in% list_prot]
output_GO <- stat_GOenrich(gene_list = list, code = code_prot)


#' @:Organ-type
setwd(paste0(root, "/1_data")) 
ref_organ <- openxlsx::read.xlsx("ref_OrganType_Ptoteomis_CELL2025.xlsx")
list <- toupper(code_prot$Uniprot[code_prot$Uniprot %in% list_prot])
data_organ <- data.frame(ref_organ, `MMI`=ifelse(ref_organ$uniprot %in% list, "MMI", "other"))
t.test(data_organ$Gobal.label.score..GLS. ~ data_organ$MMI)
table(data_organ$Global.label[data_organ$MMI=="MMI"])


##-----------------------------------------------------------
setwd(paste0(workpath, "/part5_proteomics"))
openxlsx::write.xlsx(list("stat"=output_GO), "stat_Comorbidity_proteomics_GOenrich.xlsx")




#' @:Protein-Dailys [Associations between proteins and CGM-daily traits]
##----------------------------------------------------------- 
# data --------------------------
setwd(paste0(root, "/1_data")) 
code_prot <- openxlsx::read.xlsx("GNHS_omics_proteomics.xlsx", sheet = "code")

setwd(paste0(workpath, "/part5_proteomics")) 
valid_prot <- openxlsx::read.xlsx("valid_Comorbidity_proteomics_linear.xlsx", sheet = "stat")
list_prot <- valid_prot$outcome[!is.na(valid_prot$valid)]  

#' @:Proteomics
data_prot <- openxlsx::read.xlsx("stat_Comorbidity_proteomics_mixed.xlsx", sheet = "data_zlog2") 
data_prot <- data_prot[order(data_prot$time, decreasing = T),]
data_prot <- data_prot[!duplicated(data_prot$id),]
for(i in list_prot){data_prot[,i] <- as.numeric(data_prot[,i])}

#' @:CGM-Daily
setwd(paste0(workpath, "/part1_disease"))
data_CGM <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "data")

#' @:merge
database <- merge(data_CGM[,c("id","otc_T2D", list_daily)], data_prot, by="id", all=F)
database.noT2D <- database[database$otc_T2D==0,]


list_x <- list_prot
list_y <- list_daily
list_covar <- c("age","sex", "phase")


# stat -------------------------- 
#' @:all-samples
result <- stat_linear(data = database, list_exp = list_x, list_otc = list_y, list_covar = list_covar)
output_linear.all <- data.frame(result, qval=NA)
for(i in list_y){
  output_linear.all$qval[output_linear.all$outcome==i] <- p.adjust(output_linear.all$pval, method = "BH")
}
output_linear.all$sig_q <- ifelse(output_linear.all$pval<0.05,"*",NA)
output_linear.all$sig_q[output_linear.all$qval<0.05 & output_linear.all$pval<0.05] <- "**"
table(output_linear.all$outcome, output_linear.all$sig_q)
output_linear.all <- merge(output_linear.all, code_prot, by.x="exposure", by.y="Uniprot", all.x = T)


#' @:noT2D
result <- stat_linear(data = database.noT2D, list_exp = list_x, list_otc = list_y, list_covar = list_covar)
output_linear.noT2D <- data.frame(result, qval=NA)
for(i in list_y){
  output_linear.noT2D$qval[output_linear.noT2D$outcome==i] <- p.adjust(output_linear.noT2D$pval, method = "BH")
}
output_linear.noT2D$sig_q <- ifelse(output_linear.noT2D$pval<0.05,"*",NA)
output_linear.noT2D$sig_q[output_linear.noT2D$qval<0.05 & output_linear.noT2D$pval<0.05] <- "**"
table(output_linear.noT2D$outcome, output_linear.noT2D$sig_q)
output_linear.noT2D <- merge(output_linear.noT2D, code_prot, by.x="exposure", by.y="Uniprot", all.x = T)


#' @:sorting
a <- merge(output_linear.all[,c("exposure","outcome","beta")], output_linear.noT2D[,c("exposure","outcome","beta","SYMBOL")], by=c("exposure","outcome"), all = F)
rcorr(a$beta.x, a$beta.y)


##-----------------------------------------------------------
setwd(paste0(workpath, "/part5_proteomics"))
openxlsx::write.xlsx(list("stat_all"=output_linear.all, "stat_noT2D"=output_linear.noT2D, "data"=database), "stat_Proteomics_CGMdaily_linear.xlsx")




#' @:Protein-Meals [Associations between proteins and Meal traits]
##----------------------------------------------------------- 
# data --------------------------
setwd(paste0(root, "/1_data")) 
code_prot <- openxlsx::read.xlsx("GNHS_omics_proteomics.xlsx", sheet = "code")

setwd(paste0(workpath, "/part5_proteomics")) 
valid_prot <- openxlsx::read.xlsx("valid_Comorbidity_proteomics_linear.xlsx", sheet = "stat")
list_prot <- valid_prot$outcome[!is.na(valid_prot$valid)]  

#' @:Proteomics
data_prot <- openxlsx::read.xlsx("stat_Comorbidity_proteomics_mixed.xlsx", sheet = "data_zlog2") 
data_prot <- data_prot[order(data_prot$time, decreasing = T),]
data_prot <- data_prot[!duplicated(data_prot$id),]
for(i in list_prot){data_prot[,i] <- as.numeric(data_prot[,i])}

#' @:CGM-Meals
setwd(paste0(workpath, "/part3_PGS"))
data_CGM <- openxlsx::read.xlsx("stat_DRs_multimorbidity_linear.xlsx", sheet = "data")
list_trait <- c("peak","iAUC","ppge","acc")

#' @:merge
database <- merge(data_CGM[,c("id","otc_T2D","meal", list_trait)], data_prot, by="id", all=F)
database.noT2D <- database[database$otc_T2D==0,]


list_x <- list_prot
list_y <- list_trait
list_covar <- c("age","sex", "phase")


# stat -------------------------- 
#' @:all-samples
output_linear.all <- data.frame()
for(meal in c("RG","WG")){
  result <- stat_linear(data = database[database$meal==meal,], list_exp = list_x, list_otc = list_y, list_covar = list_covar)
  result2 <- data.frame(meal=meal, result, qval=NA)
  for(i in list_y){
    result2$qval[result2$outcome==i] <- p.adjust(result2$pval, method = "BH")
  }
  
  result2$sig_q <- ifelse(result2$pval<0.05,"*",NA)
  result2$sig_q[result2$qval<0.05 & result2$pval<0.05] <- "**"  
  output_linear.all <- rbind.fill(output_linear.all, result2)
}
table(output_linear.all$meal, output_linear.all$outcome, output_linear.all$sig_q)
output_linear.all <- merge(output_linear.all, code_prot, by.x="exposure", by.y="Uniprot", all.x = T)


#' @:noT2D
output_linear.noT2D <- data.frame()
for(meal in c("RG","WG")){
  result <- stat_linear(data = database.noT2D[database.noT2D$meal==meal,], list_exp = list_x, list_otc = list_y, list_covar = list_covar)
  result2 <- data.frame(meal=meal, result, qval=NA)
  for(i in list_y){
    result2$qval[result2$outcome==i] <- p.adjust(result2$pval, method = "BH")
  }
  
  result2$sig_q <- ifelse(result2$pval<0.05,"*",NA)
  result2$sig_q[result2$qval<0.05 & result2$pval<0.05] <- "**"  
  output_linear.noT2D <- rbind.fill(output_linear.noT2D, result2)
}
table(output_linear.noT2D$meal, output_linear.noT2D$outcome, output_linear.noT2D$sig_q)
output_linear.noT2D <- merge(output_linear.noT2D, code_prot, by.x="exposure", by.y="Uniprot", all.x = T)


#' @:sorting
a <- merge(output_linear.all[,c("exposure","outcome","meal","beta")], output_linear.noT2D[,c("exposure","outcome","meal","beta","SYMBOL")], by=c("exposure","outcome","meal"), all = F)
rcorr(a$beta.x, a$beta.y)


##-----------------------------------------------------------
setwd(paste0(workpath, "/part5_proteomics"))
openxlsx::write.xlsx(list("stat_all"=output_linear.all, "stat_noT2D"=output_linear.noT2D, "data"=database), "stat_Proteomics_Mealtraits_linear.xlsx")




#' @:CRPs-PGS [All & non-diabetic]
#' @:linear
##-----------------------------------------------------------
# data ------------------------ 
#' @:Protein
setwd(paste0(workpath, "/part5_proteomics"))
valid_prot <- openxlsx::read.xlsx("valid_Comorbidity_proteomics_linear.xlsx", sheet = "stat")
list_prot <- valid_prot$outcome[!is.na(valid_prot$valid)]

data_prot <- openxlsx::read.xlsx("stat_Comorbidity_proteomics_mixed.xlsx", sheet = "data_zlog2")
data_prot <- data_prot[,c("sampleid","id","time","age","sex","BMI", list_prot)]
data_prot <- data_prot[order(data_prot$time, decreasing = T),]
data_prot <- data_prot[!duplicated(data_prot$id),]
for(i in list_prot){data_prot[,i] <- as.numeric(data_prot[,i])}


#' @:PGS
setwd(paste0(root, "/1_data"))
data_PGS <- openxlsx::read.xlsx("PGS_construction_GNHS.xlsx", sheet = 1)   
data_PGS$id <- gsub("F4","",data_PGS$id) 
code_prot <- openxlsx::read.xlsx("GNHS_omics_proteomics.xlsx", sheet = "code")


#' @:merge
database <- merge(data_PGS[,c("id","PGS")], data_prot[,c("id","age","sex","BMI", list_prot)], by="id", all=F)
database$PGS <- scale(database$PGS)
hist(database$PGS)


# stat (all) ------------------------
input_data <- database 

list_x <- c(list_prot)
list_y <- "PGS"
list_covar <- c("age","sex")


#' @:stat
output_linear <- data.frame()
output_linear <- stat_linear(data = input_data, list_exp = list_x, list_otc = list_y, list_covar = list_covar)

output_linear$qval <- p.adjust(output_linear$pval, method = "BH")
output_linear$sig_q <- ifelse(output_linear$pval<0.05 &output_linear$qval<0.05, "**",NA)
output_linear$sig_q[output_linear$pval<0.05 & output_linear$qval>=0.05] <- "*"
table(output_linear$sig_q)

output_linear <- merge(output_linear, code_prot, by.x="exposure", by.y="Uniprot", all.x=T)
output_linear <- output_linear[order(output_linear$pval, decreasing = F),]
output_linear.all <- output_linear
 

# stat (noT2D) -------------------------- 
setwd(paste0(workpath, "/part1_disease"))
data_CGM <- openxlsx::read.xlsx("stat_Daily_LADE_linear.xlsx", sheet = "data")
database.noT2D <- merge(data_CGM[,c("id","otc_T2D")], database, by="id", all.y=T)
database.noT2D <- database.noT2D[database.noT2D$otc_T2D==0,]
input_data <- database.noT2D 

list_x <- c(list_prot)
list_y <- "PGS"
list_covar <- c("age","sex")

#' @:stat
output_linear <- data.frame()
output_linear <- stat_linear(data = input_data, list_exp = list_x, list_otc = list_y, list_covar = list_covar)

output_linear$qval <- p.adjust(output_linear$pval, method = "BH")
output_linear$sig_q <- ifelse(output_linear$pval<0.05 &output_linear$qval<0.05, "**",NA)
output_linear$sig_q[output_linear$pval<0.05 & output_linear$qval>=0.05] <- "*"
table(output_linear$sig_q)

output_linear <- merge(output_linear, code_prot, by.x="exposure", by.y="Uniprot", all.x=T)
output_linear <- output_linear[order(output_linear$pval, decreasing = F),]
output_linear.noT2D <- output_linear

a <- merge(output_linear.all[,c("exposure","beta")], output_linear.noT2D[,c("exposure","beta")], by="exposure", all=F)
plot(a$beta.x, a$beta.y)

##-----------------------------------------------------------
setwd(paste0(workpath, "/part5_proteomics"))
openxlsx::write.xlsx(list("stat_all"=output_linear.all, "stat_noT2D"=output_linear.noT2D, "data"=database), "stat_Proteins_PGS_linear.xlsx")




#' @:Proteins [Cross organ evaluations]
##-----------------------------------------------------------
# data --------------------------
setwd(paste0(root, "/1_data")) 
code_prot <- openxlsx::read.xlsx("GNHS_omics_proteomics.xlsx", sheet = "code")

setwd(paste0(workpath, "/part5_proteomics")) 
valid_prot <- openxlsx::read.xlsx("valid_Comorbidity_proteomics_linear.xlsx", sheet = "stat")
list_prot <- valid_prot$outcome[!is.na(valid_prot$valid)]  

PGS_prot <- openxlsx::read.xlsx("stat_Proteins_PGS_linear.xlsx", sheet = "stat_all")
list_prot.PGS <- unique(PGS_prot$exposure[PGS_prot$qval<0.05 & PGS_prot$pval<0.05])


#' @:Organ-type
setwd(paste0(root, "/1_data")) 
ref_organ <- openxlsx::read.xlsx("ref_OrganType_Ptoteomis_CELL2025.xlsx")
list <- toupper(code_prot$Uniprot[code_prot$Uniprot %in% list_prot])
list_PGS <- toupper(code_prot$Uniprot[code_prot$Uniprot %in% list_prot.PGS])

data_organ <- data.frame(ref_organ, `MMI`=ifelse(ref_organ$uniprot %in% list, "MMI", "other"))
data_organ$MMI_PGS <- data_organ$MMI
data_organ$MMI_PGS[data_organ$uniprot %in% list_PGS] <- "PGS"
table(data_organ$MMI_PGS)


# stat -------------------------
#' @:comparison
stat1 <- stat_wilcox(data = data_organ, group = "MMI", trait = "Gobal.label.score..GLS.") 
stat2 <- stat_wilcox(data = data_organ[data_organ$MMI_PGS!="MMI",], group = "MMI_PGS", trait = "Gobal.label.score..GLS.") 
stat3 <- stat_wilcox(data = data_organ[data_organ$MMI_PGS!="other",], group = "MMI_PGS", trait = "Gobal.label.score..GLS.") 


#' @:difference
a1 <- data.frame(table(data_organ$Global.label[data_organ$MMI=="MMI"]))
names(a1) <- c("Global_label","freq_MMI") 
a2 <- data.frame(table(data_organ$Global.label[data_organ$MMI_PGS=="PGS"]))
names(a2) <- c("Global_label","freq_PGS") 
data_sort <- merge(a1, a2, by="Global_label", all=T)
data_sort$freq_PGS[is.na(data_sort$freq_PGS)] <- 0
data_sort$organ <- sapply(data_sort$Global_label, function(x){str_split(x, "\\.")[[1]][1]})

data_sort <- data_sort[order(data_sort$freq_MMI, decreasing = T),]
data_sort <- data_sort[order(data_sort$freq_PGS, decreasing = T),]
length(unique(data_sort$Global_label[data_sort$freq_PGS>0]))


##-----------------------------------------------------------
setwd(paste0(workpath, "/part5_proteomics"))
openxlsx::write.xlsx(list("sort"=data_sort, "data"=data_organ), "stat_Proteomics_CrossOrgan.xlsx")




#' @:CRPs-score-PGS
#' @:mediation-analysis
library(mediation)
##-----------------------------------------------------------
# data ---------------------------
setwd(paste0(root, "/1_data")) 
code_prot <- openxlsx::read.xlsx("GNHS_omics_proteomics.xlsx", sheet = "code")


#' @:database
setwd(paste0(workpath, "/part5_proteomics"))
data_comorb <- openxlsx::read.xlsx("stat_Comorbidity_proteomics_mixed.xlsx", sheet = "data_zlog2")
data_comorb <- data_comorb[order(data_comorb$time, decreasing = T),]
data_comorb <- data_comorb[!duplicated(data_comorb$id),]
data_comorb <- data_comorb[,c("id","time","phase","otc_coMorbidity.glucose")]

setwd(paste0(workpath, "/part5_proteomics"))
database <- openxlsx::read.xlsx("stat_Proteins_PGS_linear.xlsx", sheet = "data")
database <- merge(data_comorb, database, by="id", all=F)
stat <- openxlsx::read.xlsx("stat_Proteins_PGS_linear.xlsx", sheet = "stat_all")
list_prot <- stat$exposure[stat$qval<0.05]


#' @:CRP-score
stat_prot <- openxlsx::read.xlsx("stat_Comorbidity_proteomics_mixed.xlsx", sheet = "stat") 
submatr <- database[,c("id",list_prot)]
for(i in list_prot){ 
  a <- submatr[,i]
  b <- stat_prot$beta[stat_prot$outcome==i]
  submatr[,i] <- a*b 
}
submatr$score_CRP <- apply(submatr[,list_prot],1,sum)
database <- merge(submatr[,c("id","score_CRP")], database, by="id", all=F)
plot(database$age, database$score_CRP)


# mediation -----------------------------
list_mediate <- c(list_prot,"score_CRP")
table.mediation <- data.frame(x="otc_coMorbidity.glucose", m=list_mediate, y="PGS")
input_data <- database
output_mediate <- data.frame()

for(i in 1:nrow(table.mediation)){
  print(paste0("Round: ",i," / ", nrow(table.mediation)))
  submatr <- data.frame(age=input_data$age,
                        sex=input_data$sex,
                        BMI=input_data$BMI,
                        x=input_data[,table.mediation$x[i]],
                        m=input_data[,table.mediation$m[i]],
                        y=input_data[,table.mediation$y[i]])
  
  # m <- scale(log2(submatr$m))
  mod.mediate <- lm(m ~ x + age + sex + BMI, data=submatr)
  summary(mod.mediate)
  
  if(table.mediation$y[i]=="mean_HPT.IRN"){
    y <- submatr$y
  }else{
    y <- scale(log2(submatr$y))
  }
  mod.outcome <- lm(y ~ m + x + age + sex + BMI, data=submatr)
  summary(mod.outcome)
  
  stat<- mediate(mod.mediate, mod.outcome, sims = 2000, treat = "x", mediator = "m", data=submatr, robustSE = TRUE)
  result <- summary(stat)
  
  ACME <- data.frame(result$d1.p, result$d1, t(result$d1.ci)); names(ACME) <- c("pval","beta","lci","uci")
  ADE <- data.frame(result$z1.p, result$z1, t(result$z1.ci)); names(ADE) <- c("pval","beta","lci","uci")
  propMedia <- data.frame(result$n1.p, result$n1, t(result$n1.ci)); names(propMedia) <- c("pval","beta","lci","uci")
  tEffect <- data.frame(result$tau.p, result$tau.coef, t(result$tau.ci)); names(tEffect) <- c("pval","beta","lci","uci")
  output <- rbind(ACME,ADE,propMedia, tEffect)
  output <- data.frame(x=table.mediation$x[i],m=table.mediation$m[i], y=table.mediation$y[i], 
                       effect=c("ACME","ADE","propMedia","tEffect"),output)
  
  output$sig_p <- ifelse(output$pval<0.05,"*",NA)
  output$sig_p[output$pval<0.01] <- "**"
  output$sig_p[output$pval<0.001] <- "***"
  
  output_mediate <- rbind(output_mediate, output)
}


output_mediate <- merge(output_mediate, code_prot, by.x="m", by.y="Uniprot", all.x=T)
output_mediate <- output_mediate[order(output_mediate$pval, decreasing = F),]
output_mediate <- output_mediate[order(output_mediate$m, decreasing = F),]

output_mediate$qval <- NA
for(i in c("ADE","tEffect","ACME","propMedia")){
  output_mediate$qval[output_mediate$effect==i] <- p.adjust(output_mediate$pval[output_mediate$effect==i], method = "BH")
}
output_mediate$sig_q <- ifelse(output_mediate$pval<0.05,"*",NA)
output_mediate$sig_q[output_mediate$qval<0.05 & output_mediate$pval<0.05] <- "**"
table(output_mediate$sig_q)
output_mediate.prot <- output_mediate


##-----------------------------------------------------------
setwd(paste0(workpath, "/part5_proteomics"))
openxlsx::write.xlsx(list("stat"=output_mediate.prot, "data"=database), "stat_Comorbidity_proteomics_PGS_interact.xlsx")




#' @:CRPscore-PGS 
setwd(paste0(workpath, "/part5_proteomics")) 
##-----------------------------------------------------------
# data ----------------------
database <- openxlsx::read.xlsx("stat_Comorbidity_proteomics_PGS_interact.xlsx", sheet = "data")
database$PGS <- scale(database$PGS)

setwd(paste0(root, "/1_data")) 
code_prot <- openxlsx::read.xlsx("GNHS_omics_proteomics.xlsx", sheet = "code")


# stat ----------------------
result1 <- stat_linear(data = database, list_exp = "otc_coMorbidity.glucose", list_otc = "score_CRP", list_covar = c("age","sex"))
result2 <- stat_linear(data = database, list_exp = "score_CRP", list_otc = "PGS", list_covar = c("age","sex"))
output <- rbind(result1, result2)

##-----------------------------------------------------------
setwd(paste0(workpath, "/part5_proteomics"))
openxlsx::write.xlsx(list("stat"=output, "data"=database), "stat_Comorbidity_score_PGS_linear.xlsx")




#' @:Function
##------------------------------------------------------
# continuous ---------------------

#' @:RandomForest-SHAPexplainer
model_rf_conti <- function(input_data, trait, list_x, formula, nfold){
  library(caret)
  library(randomForest)
  library(iml)
  # install.packages("iml")
  
  set.seed(123456)
  
  kfold <- sample(rep(1:nfold,ceiling(nrow(input_data)/nfold))[1:nrow(input_data)])
  input_data <- data.frame(kfold=kfold, input_data)
  input_data <- input_data[order(input_data$kfold, decreasing = F),]
  
  output_pred <- data.frame(id=input_data$id, kfold=input_data$kfold, true=input_data[,trait], pred=NA)
  shap_values_all <- data.frame()
  mean_absolute_shap <- data.frame()
  for(fold in 1:nfold){ 
    print(paste("#-------- Round: ", fold,"/",nfold))
    
    train <- input_data[input_data$kfold!=fold,]
    test <- input_data[input_data$kfold==fold,]
    id <- test$id
    
    model <- randomForest(as.formula(formula), data = train, importance=T)
    pred <- predict(model, test[,c(list_x)])
    output_pred$pred[output_pred$kfold==fold] <- pred
    
    #' @:SHAP-explainer
    predictor <- iml::Predictor$new(model, data = train[,list_x], y = train[,trait])
    shap_values <- data.frame()
    for(i in 1:nrow(test)){
      print(paste("#--- SHAP: ", i,"/",nrow(test)))
      shap <- iml::Shapley$new(predictor, x.interest=test[i,list_x])
      shap_value <- shap$results[,c("feature","phi")]
      shap_value <- data.frame(t(shap_value)); names(shap_value) <- shap_value[1,]; shap_value <- shap_value[-1,]
      shap_value <- data.frame(id=test$id[i],shap_value)
      shap_values <- rbind.fill(shap_values, shap_value)
    }
    shap_values_all <- rbind.fill(shap_values_all, shap_values) 
  }
  shap_values_all[,-1] <- apply(shap_values_all[,-1],2,as.numeric)
  mean_absolute_shap <- apply(shap_values_all[,-1], 2, function(x){mean(abs(x))})
  mean_absolute_shap <- data.frame(feature=colnames(shap_values_all[,-1]), mean_absolute_shap=mean_absolute_shap)
  mean_absolute_shap <- mean_absolute_shap[order(mean_absolute_shap$mean_absolute_shap, decreasing = T),]
  
  output <- list(output_pred, shap_values_all, mean_absolute_shap)
  return(output) 
}

model_eval_conti <- function(data){ 
  
  data$true[data$true==0] <- min(data$true)*10^-15 
  r_spearman <- rcorr(data$pred, data$true, type = "spearman")
  r_pearson <- rcorr(data$pred, data$true, type = "pearson")
  MAPE <- (sum(abs((data$pred-data$true)/data$true))*100)/nrow(data)
  RMSE <- sqrt(sum((data$pred-data$true)^2/nrow(data)))
  
  SSE <- sum((data$pred-data$true)^2) # sum of squared errors, SSE
  SSR <- sum((data$pred-mean(data$true))^2) # sum of squares for regression, SSR
  SST <- SSE + SSR 
  Coefficient_of_determination <- 1 - (SSE / SST) 
  
  result_eval <- data.frame(spearman_r=r_spearman$r[2,1], spearman_p=r_spearman$P[2,1],
                            pearson_r=r_pearson$r[2,1], pearson_p=r_pearson$P[2,1],
                            MAPE=MAPE, RMSE=RMSE, Coefficient_of_determination = Coefficient_of_determination
  )
  return(result_eval)
}

##------------------------------------------------------




#' @:CRPs-predict-PGS
##------------------------------------------------------
# data ------------------------
setwd(paste0(root, "/1_data")) 
code_prot <- openxlsx::read.xlsx("GNHS_omics_proteomics.xlsx", sheet = "code")

#' @:Proteomics
setwd(paste0(workpath, "/part5_proteomics"))
stat <- openxlsx::read.xlsx("stat_Proteins_PGS_linear.xlsx", sheet = "stat_all")
list_prot <- stat$exposure[stat$qval<0.05]

data_prot <- openxlsx::read.xlsx("stat_Comorbidity_proteomics_mixed.xlsx", sheet = "data_zlog2")
data_prot <- data_prot[order(data_prot$time, decreasing = F),]
data_prot <- data_prot[!duplicated(data_prot$id),]
for(i in list_prot){data_prot[,i] <- as.numeric(data_prot[,i])}
table(data_prot$time)


#' @:PGS
setwd(paste0(root, "/1_data")) 
data_PGS <- openxlsx::read.xlsx("PGS_construction_GNHS.xlsx", sheet = 1)  
data_PGS$id <- gsub("F4","",data_PGS$id) 


#' @:merge
database <- merge(data_PGS[,c("id","PGS")], data_prot, by = "id", all = F)


list_merge <- list(list_prot)
list_name <- c("CRPs")
list_trait <- "PGS"


# prediction ------------------
list_y <- list_trait
input_data <- database

output_perf <- data.frame()
for(y in list_y){ 
  for(x in 1:length(list_merge)){
    
    print(paste("Prediction of trait", which(list_y==y),"/",length(list_y)))
    list_x <- unique(c(list_merge[[x]]))
    
    #' @:imputation
    submatr <- input_data[!is.na(input_data[,y]),c("id", y, list_x)] 
    for(i in 2:ncol(submatr)){
      submatr[,i] <- as.numeric(submatr[,i])
      submatr[is.na(submatr[,i]),i] <- mean(submatr[,i], na.rm = T)
    }
    
    #' @:modeling
    a <- paste(list_x, collapse = " + ")
    formula <- paste0(y," ~ ", a)
    model <- model_rf_conti(submatr, trait = y, list_x = list_x, formula=formula, nfold = 10)  
    
    
    #' @:output
    output_pred <- model[[1]]
    output_SHAP <- model[[3]] # inversely ranked by SHAP values, top ot down
    output_SHAP <- merge(output_SHAP, code_prot, by.x="feature", by.y="Uniprot", all.x=T)
    output_SHAP <- output_SHAP[order(output_SHAP$mean_absolute_shap, decreasing = T),]
    
    result <- model_eval_conti(output_pred)
    output <- data.frame(outcome="PGS", predictor="CRPs", result) 
    output_perf <- rbind.fill(output_perf, output) 
  }  
} 

  
##------------------------------------------------------
setwd(paste0(workpath, "/part5_proteomics"))
openxlsx::write.xlsx(list("pred"=output_pred, "perf"=output_perf,"SHAP"=output_SHAP, "data"=database), "stat_Proteomics_PGS_predict.xlsx")







