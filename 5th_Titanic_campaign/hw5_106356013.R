#setwd("~/Desktop/hw5-YuTaNCCU")
#system("Rscript hw5_106356013.R -fold 5 –out performance.csv")

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
library(dplyr)
d <- read.csv("Titanic_Data/train.csv", header = T) %>%
  select(Survived,	Pclass, Sex,	Age,	SibSp,	Parch,Fare )
d$Age = ifelse(is.na(d$Age),
                    ave(d$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                    d$Age)
d$Sex = ifelse(d$Sex == 'male', 1, 0)
d$Age = scale(d$Age) # Feature Scaling
d$Fare = scale(d$Fare) # Feature Scaling

test <- read.csv("Titanic_Data/test.csv", header = T) %>%
  select(PassengerId,	Pclass, Sex,	Age,	SibSp,	Parch, Fare )
test$Age = ifelse(is.na(test$Age),
               ave(test$Age, FUN = function(x) mean(x, na.rm = TRUE)),
               test$Age)
test$Sex = ifelse(test$Sex == 'male', 1, 0)
test$Age = scale(test$Age) # Feature Scaling
test$Fare = scale(test$Fare) # Feature Scaling
test$Fare = ifelse(is.na(test$Fare),
                   ave(test$Fare, FUN = function(x) mean(x, na.rm = TRUE)),
                   test$Fare)
##################
### 分割資料 #####
print('(3/7)分割資料')
##################
#install.packages('caret')
require(caret) 

#fold=5
#分割成Ｋ個區塊
flds <- createFolds( y=d$Survived, k = fold, list = TRUE, returnTrain = FALSE)


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

ModelFit <- function(fold_i){

  
  

}


#################
### 跑n-fold ###
print('(4/7)跑n-fold')
#################
trainningAccuracy <-c()
calibrationAccuracy <-c()
testAccuracy <-c()
for (fold_i in  1:fold ){

  
  data<-nfcv(fold_i)
  d_test<-data[[1]]
  d_calib<-data[[2]]
  d_train<-data[[3]]
  
  ########################
  ### 使用不同k值的KNN ###
  if(fold_i==1)print('(5/7)使用不同k值的KNN')
  ########################
  ValidationValue<-c()
  ValidationValue<-c()
  
  for(i_knn in 1:10){
    #train
    library(class)
    y_pred = knn(train = d_train[,2:7],
                 test = d_train[,2:7],
                 cl = d_train[, 1],
                 k = i_knn,
                 prob = TRUE)
    
    cm = table(d_train[, 1], y_pred)
    trainningAccuracy <- c(trainningAccuracy,   (cm[1,1]+cm[2,2])/sum(cm) )
    #validate
    y_pred = knn(train = d_train[,2:7],
                 test = d_calib[,2:7],
                 cl = d_train[, 1],
                 k = i_knn)
    cm = table(d_calib[, 1], y_pred)
    calibrationAccuracy <- c(calibrationAccuracy,  (cm[1,1]+cm[2,2])/sum(cm) )
    ValidationValue <- c(ValidationValue, (cm[1,1]+cm[2,2])/sum(cm) )
    
  }
  
  
  ########################
  ### 使用Ramdom forests ###
  if(fold_i==1)print('(6/7)Ramdom forests')
  ########################
  #train
  library(randomForest)
  set.seed(123)
  fmodel <- randomForest(x=d_train[,2:7], y=d_train[,1], ntree=100, nodesize=7, importance=T)
  y_pred <- ifelse(predict(fmodel, newdata=d_train[,2:7]) >0.5,1,0)
  cm = table(d_train[, 1], y_pred) 
  trainningAccuracy <- c(trainningAccuracy,   (cm[1,1]+cm[2,2])/sum(cm) )
  #validate
  y_pred <- ifelse(predict(fmodel, newdata=d_calib[,2:7]) >0.5,1,0)
  cm = table(d_calib[, 1],y_pred) 
  calibrationAccuracy <- c(calibrationAccuracy,  (cm[1,1]+cm[2,2])/sum(cm) )

  #choose best valid model
   if(max(ValidationValue) > (cm[1,1]+cm[2,2])/sum(cm) ){
     #test
     y_pred = knn(train = d_train[,2:7],
                  test = d_test[,2:7],
                  cl = d_train[, 1],
                  k = which.max(ValidationValue),
                  prob = TRUE)
     cm = table(d_test[,1], y_pred)
     testAccuracy  <- c(testAccuracy, (cm[1,1]+cm[2,2])/sum(cm) )
     
     #kaggle
     y_pred_kaggle <- knn(train = d[,2:7],
                  test = test[,2:7],
                  cl = d[, 1],
                  k = which.max(ValidationValue),
                  prob = TRUE)
     
   }else{
     y_pred <- ifelse(predict(fmodel, newdata=d_test[,2:7]) >0.5,1,0)
     cm = table(d_test[, 1], y_pred) 
     testAccuracy <- c(trainningAccuracy,   (cm[1,1]+cm[2,2])/sum(cm) )
     
     #kaggle  
     y_pred_kaggle  <- ifelse(predict(fmodel, newdata=test[,2:7]) > 0.5, 1, 0)
   }
  
  
  write.csv(test,'1.csv')


  
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

#kaggle
out_data_kaggle <- data.frame(PassengerId=test[,1],
                              Survived= y_pred_kaggle
                       )

write.csv(out_data_kaggle, file='yuta_ds.csv', row.names = F, quote = F)














