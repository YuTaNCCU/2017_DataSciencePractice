#cd ~/Desktop/hw2-YuTaNCCU
#setwd("~/Desktop/hw2-YuTaNCCU")
#Rscript hw2_106356013.R --target male --inputs examples/method1.csv examples/method2.csv --output examples/output1.csv
#Rscript hw2_106356013.R --target male --inputs examples/method1.csv examples/method3.csv examples/method5.csv --output examples/output2.csv
#Rscript hw2_106356013.R --target female --inputs examples/method2.csv examples/method4.csv examples/method6.csv --output examples/output3.csv

#install.packages('dplyr') #安裝dplyr套件。
library('dplyr') 
#install.packages('ROCR') #安裝ROCR套件。
library('ROCR') 

# read parameters
args = commandArgs(trailingOnly=TRUE)
#args = c('--target', 'male', '--inputs', 'examples/method1.csv',  'examples/method2.csv', '--output' , 'examples/output1.csv')
if (length(args)==0) {
  stop("USAGE: Rscript hw2_yourID.R --target male|female --inputs file1 file2 ... filen --output out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--inputs"){
    j<-grep("-", c(args[(i+1):length(args)], "-")  )[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

#print input
print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
cat(paste("files      :",files,'\n'))

# read files
#a<- read.csv(files[1])
#a1=select(a,prediction,reference)
#a2<-table(a1)

#自訂函數
query_func<-function(query_m, file1)
{
  file2<- read.csv(file1)
  
  file3<- select(file2,prediction,reference)
  file4<- table(file3)   #to compute comflix matrix
  
  file_roc <- file2[2:4]
  file_roc_2 <- mutate(file_roc, predictions=pred.score , labels=reference)
  file_roc_3 <- select(file_roc_2,predictions,labels)   #to compute roc and auc 
  
  if(query_m == "male"){
    
    TP <- file4[2,2]
    TN <- file4[1,1]
    FP <- file4[2,1]
    FN <- file4[1,2]
    
    file_roc_3$labels<-ifelse(file_roc_3$labels=='male',1,0)
    pred <- prediction(file_roc_3$predictions, file_roc_3$labels)
    AUC_value <- performance(pred, "auc")@y.values
    AUC_value <- round(AUC_value[[1]],digits =2)
    
    query0 <- list(TP,TN,FP,FN,AUC_value)
    return(query0)
    
  }
  else if (query_m == "female") {

    TP <- file4[1,1]
    TN <- file4[2,2]
    FP <- file4[1,2]
    FN <- file4[2,1]
    
    file_roc_3$labels<-ifelse(file_roc_3$labels=='female',1,0)
    pred <- prediction(1-file_roc_3$predictions, file_roc_3$labels)
    AUC_value <- performance(pred, "auc")@y.values
    AUC_value <- round(AUC_value[[1]],digits =2)
    
    query0 <- list(TP,TN,FP,FN,AUC_value)
    return(query0)
    
    
  } 
  else {
    stop(paste("ERROR: unknown query function", query_m))
  }
}

#prepare vector
method<-c()
sensitivity<-c()
specificity<-c()
F1<-c()
AUC<-c()

#prepare datafame
for(file in files)
{
  #name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  
  query1 <-  query_func(query_m, file)
  #print(query1)
  TP<- query1[[1]]
  TN<- query1[[2]]
  FP<- query1[[3]]
  FN<- query1[[4]]
  AUC_value<- query1[[5]]
  
  method <- c(method,   name<-gsub(".csv", "", basename(file))   )#basename() remove the path of the file ,gsub() replace '.csv' with ''
  sensitivity<-c(sensitivity, round( TP/(TP+FN) , digits =2)  )
  specificity<-c(specificity, round( TN/(TN+FP) , digits =2)  )
  ppp <- TP/(TP+FP)
  rrr <- TP/(FN+TP)
  F1<-c(F1, round( 2*(ppp*rrr)/(ppp+rrr)   , digits =2 )  )
  AUC<-c(AUC,AUC_value)
}
out_data<-data.frame(method=method,sensitivity=sensitivity, specificity=specificity, F1=F1, AUC=AUC, stringsAsFactors = F)




#find the highest
tmp<-method[sapply(out_data[2:5],which.max)]
highest <- data.frame(method='highest',sensitivity=tmp[1], specificity=tmp[2], F1=tmp[3], AUC=tmp[4], stringsAsFactors = F)
out_data<-rbind(out_data,highest)

# output file
print(out_data)
write.csv(out_data, file=out_f, row.names = F, quote = F)

















