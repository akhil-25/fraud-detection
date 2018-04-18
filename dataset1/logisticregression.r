#read data
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

#logistic regression model
data1logistic<-glm(formula = Class~V17+V12+V14+V10+V11,family = binomial(link="logit"),data = train)
data1logisticpred<-predict(logistic[[1]],test,type = "response")
pr<-ifelse(pred>0.5,1,0)
data1logisticpred<-ifelse(data1logisticpred>0.4,1,0)
A(pr,test$Class)
A(data1logisticpred,test$Class)

#
data1logistic2<-glm(formula = Class~V17+V12+V14+V10+V11+V16+V18+V9+V4+V7,family = binomial(link="logit"),data = train)
data1logistic2pred<-predict(logistic[[2]],test,type = "response")
pr10<-ifelse(pred10>0.5,1,0)
data1logistic2pred<-ifelse(data1logistic2pred>0.4,1,0)
A(pr10,test$Class)
A(pr410,test$Class)

#
data1logistic3<-glm(formula = Class~.,family = binomial(link="logit"),data = train)
data1logistic3pred<-predict(logistic[[3]],test,type = "response")
pra<-ifelse(preda>0.5,1,0)
data1logistic3pred<-ifelse(data1logistic3pred>0.4,1,0)
A(pra,test$Class)
A(pr4a,test$Class)

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
