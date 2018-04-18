library(data.table)
require(e1071)
data<-fread("E:\\AFINAL_PROJECT\\R\\creditcard.csv\\creditcard.csv")
data<-as.data.frame(data)
data$Class<-as.factor(data$Class)

#ROSE
library(ROSE)
data1<-ROSE(Class ~ ., data = data, seed = 1)
#dividing test and train
index<-sample(nrow(data1$data),nrow(data1$data)*.7)
train<-data1$data[index,]
test<-data1$data[-index,]
#naive bayes
library(class)
data1naive <- naiveBayes(Class~V17+V12+V14+V10+V11, data = train)
data1naivepred <- predict(data1naive,test)
A(data1naivepred,test$Class)
#naive bayes
data1naive2 <- naiveBayes(Class~V17+V12+V14+V10+V11+V16+V18+V9+V4+V7, data = train)
data1naive2pred <- predict(data1naive2,test)
A(data1naive2pred,test$Class)
#naive bayes
data1naive3 <- naiveBayes(Class~., data = train)
data1naive3pred <- predict(data1naive3,test)
A(data1naive3pred,test$Class)

A<-function(data,ref)
{
  C<-0
  C1<-0
  C2<-0
  C3<-0
  for(i in 1:length(data)){
    if(data[i]==ref[i]&ref[i]==0)
    {C=C+1
    }else if(data[i]==ref[i]&ref[i]==1){
      C1<-C1+1
    }else if(data[i]!=ref[i]&ref[i]==0){
      C2<-C2+1
    }else{C3<-C3+1}
  }
  M1<-matrix(c(C,C3,C2,C1),byrow = T,ncol = 2,nrow = 2)
  rownames(M1)<-c("0","1")
  colnames(M1)<-c("0","1")
  cat("Pred Ref \n")
  print(M1)
  cat("\n")
  sensitivity<-C/(C+C2)
  specificity<-C1/(C1+C3)
  accuracy<-(C1+C)/(C1+C2+C3+C)
  balenced_accuracy<-(sensitivity+specificity)/2
  cat("sensitivity = ", sensitivity, "\n")
  cat("specificity = ", specificity , "\n")
  cat("accuracy = ", accuracy, "\n")
  cat("balenced_accuracy = ", balenced_accuracy, "\n")
  
}