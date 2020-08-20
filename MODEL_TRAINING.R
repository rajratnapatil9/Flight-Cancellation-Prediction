#FINAL
flight<-read.csv("flight.csv")
#Removing NAs and 2 rows where distance is 0 and negative
flight<-flight[-c(1193,2966,504,315,3064,674,3746),]
colSums(is.na(flight))
dim(flight)
#4993    8

cor(flight[-4],)
#Distance and SchedElapsedTime are highly correlated and ArrDelay and DepDelay are
#highly correlated so we will remove one of them while building model

#Training set
smp_size <- floor(0.99 * nrow(flight))
train_ind <- sample(seq_len(nrow(flight)), size = smp_size)
train <- flight[train_ind, ]
test <- flight[-train_ind, ]
canceled.test <- test[,1]
canceled.train<- train[,1]

#Logistics Regression
glm_fits=glm(Canceled~.-ArrDelay-SchedElapsedTime, data = train, family = binomial)
summary(glm_fits)
test_size<- nrow(flight)-smp_size
glm_probs_test=predict(glm_fits,test, type = "response")
threshold<-0.2
glm_pred_test=rep(0,test_size)
glm_pred_test[glm_probs_test>threshold]=1
table(glm_pred_test, canceled.test)
mean(glm_pred_test == canceled.test)

#For LDA
library(MASS)
lda_fits=lda(Canceled~.-ArrDelay-SchedElapsedTime-UniqueCarrier, data = train)
pred_lda_test <- (predict(lda_fits, test)$posterior)[,'1']
#pred.lda_test <- predict(lda_fits, test)
#table(pred.lda_test$class, canceled.test)
#mean(pred.lda_test$class == canceled.test)

#For QDA
qda_fits=qda(Canceled~.-ArrDelay-SchedElapsedTime-UniqueCarrier, data = train)
pred_qda_test <- (predict(qda_fits, test)$posterior)[,'1']
#pred_qda_test <- predict(qda_fits, test)
#table(pred_qda_test$class, canceled.test)
#mean(pred_qda_test$class == canceled.test)

source("ROC_func.R")
df = data.frame(true.class = canceled.test, LR = glm_probs_test, LDA = pred_lda_test, QDA = pred_qda_test)
ROC_func(df, 1, 2, add_on = F)
ROC_func(df, 1, 3, add_on = T, color = "blue")
ROC_func(df, 1, 4, add_on = T, color = "red")

#KNN
library(class)
trainknn<- train[,-1]
trainknn<- trainknn[,-3]
testknn<- test[,-1]
testknn<- testknn[,-3]
trainY<-train[,1]
testY<-test[,1]
set.seed(1)
pred.knn <- knn(trainknn, testknn, trainY, k = 10)
table(pred.knn,testY)
mean(pred.knn==testY)

#threshold vs accuracy analysis to calcualte best threshold value for qda
thresh <- seq(0.7, 1, 0.005)
nt <- length(thresh)
threshold <- numeric(nt)
accuracy <- numeric(nt)
k = 1
for(t in thresh){
  qda_lable <- ifelse(pred_qda_test > t, 1, 0)
  threshold[k] <- t
  accuracy[k] <- mean(qda_lable == canceled.test) 
  k <- k+1
}
#plot(threshold, accuracy, type='b')

