library(data.table)
data<-fread("E:\\AFINAL_PROJECT\\R\\creditcard.csv\\creditcard.csv")
data<-as.data.frame(data)
data$Class<-as.factor(data$Class)

#ROSE
library(ROSE)
data1<-ROSE(Class ~ ., data = data, seed = 1)

#divide into test and train
index<-sample(nrow(data1$data),nrow(data1$data)*.7)
train<-data1$data[index,]
test<-data1$data[-index,]

#decision tree model
library(rpart)
data1decision<-rpart(Class~V17+V12+V14+V10+V11,data = train)
data1decisionpred<-predict(data1decision,test,type = "class")
A(data1decisionpred,test$Class)

#decision tree model
data1decision2<-rpart(Class~V17+V12+V14+V10+V11+V16+V18+V9+V4+V7,data = train)
data1decision2pred<-predict(data1decision2,test,type = "class")
A(data1decision2pred,test$Class)

#decision tree model
data1decision3<-rpart(Class~.,data = train)
data1decision3pred<-predict(data1decision3,test,type = "class")
A(data1decision3pred,test$Class)

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

