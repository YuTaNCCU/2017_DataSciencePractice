#setwd("~/Desktop/hw4-YuTaNCCU")
#system("Rscript hw4_106356013.R -fold 5 –out performance.csv")

################
### 讀取指令 ###
print('(1/7)讀取指令')
################

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw4_106356013.R -fold 5 –out performance.csv", call.=FALSE)
}
i<-1 
while(i < length(args)){
  if(args[i] == "-fold"){
    fold<-as.numeric(args[i+1])
    i<-i+1
  }else if(args[i] == "–out"){
    files<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}


################
### 讀取檔案 ###
print('(2/7)讀取檔案')
################
d <- read.csv("Archaeal_tfpssm.csv", header = F)

##################
### 分割資料 #####
print('(3/7)分割資料')
##################
#install.packages('caret')
require(caret)

#分割成Ｋ個區塊
flds <- createFolds( y=d$V1 , k = fold, list = TRUE, returnTrain = FALSE)

#呼叫函式以選擇‘要第幾組fold‘
nfcv<-function(i){
  d_test<-d[flds[[i]],]
  if(i+1<=fold){
    d_calib<-d[flds[[i+1]],]
  }else{
    d_calib<-d[flds[[i+1-fold]],]
  }
  if(i+2<=fold){
    d_train<-d[flds[[i+2]],]
  }else{
    d_train<-d[flds[[i+2-fold]],]
  }
  for (j in  1:(fold-3) ){
    if(i+3<=fold){
      d_train<-rbind(d_train,d[flds[[i+3]],])
    }else{
      d_train<-rbind(d_train,d[flds[[i+3-fold]],])
    }
  }
  return( list(d_test,d_calib,d_train) )
}


############
### PCA ####
print('(4/7)PCA')
############
require(caret)
trans = preProcess(d[, 3:5602], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans, d[, 3:5602])
PC_label<-colnames(PC[0,])  
PC_label1<-sub('PC','',PC_label)
PC_label1 <- as.numeric(PC_label1) + 2

#################
### 跑n-fold ###
print('(5/7)跑n-fold')
#################
trainningAccuracy <-c()
calibrationAccuracy <-c()
testAccuracy <-c()
for (i in  1:fold ){
  data<-nfcv(i)
  d_test<-data[[1]]
  d_calib<-data[[2]]
  d_train<-data[[3]]
  
  ########################
  ### 使用不同k值的KNN ###
  if(i==1)print('(6/7)使用不同k值的KNN')
  ########################
  ValidationValue<-c()
  for(i_knn in 2:15){
    library(class)
    #train
    y_pred = knn(train = d_train[, PC_label1 ],
                 test = d_train[, PC_label1 ],
                 cl = d_train[, 2],
                 k = i_knn,
                 prob = TRUE)
    cm = table(d_train[, 2], y_pred)
    trainningAccuracy <- c(trainningAccuracy,  (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4])/sum(cm) )
    
    #validate
    y_pred = knn(train = d_train[, PC_label1 ],
                 test = d_calib[, PC_label1 ],
                 cl = d_train[, 2],
                 k = i_knn,
                 prob = TRUE)
    cm = table(d_calib[, 2], y_pred)
    calibrationAccuracy <- c(calibrationAccuracy, (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4])/sum(cm) )
    ValidationValue <- c(ValidationValue, (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4])/sum(cm) )
  }
  i_knn <- which.max(ValidationValue)
  
  #test
  y_pred = knn(train = d_train[, PC_label1 ],
               test = d_test[, PC_label1 ],
               cl = d_train[, 2],
               k = i_knn+1,
               prob = TRUE)
  cm = table(d_test[, 2], y_pred)
  testAccuracy  <- c(testAccuracy, (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4])/sum(cm) )
  
}

##################
### print+匯出 ###
print('(7/7)print+匯出')
##################
print('set,accuracy', quote = FALSE)
print(paste('trainning,', round(mean(trainningAccuracy),2 ), sep=''), quote = FALSE )
print(paste('calibration,', round(mean(calibrationAccuracy),2 ), sep=''), quote = FALSE)
print(paste('test,', round(mean(testAccuracy),2 ), sep=''), quote = FALSE)

out_data <- data.frame(set=c('trainning', 'calibration', 'test'), 
                       accuracy=c(round( mean(trainningAccuracy), 2 ),
                                  round( mean(calibrationAccuracy), 2 ),
                                  round( mean(testAccuracy), 2 ) 
                                  )
                       )
write.csv(out_data, file=files, row.names = F, quote = F)














