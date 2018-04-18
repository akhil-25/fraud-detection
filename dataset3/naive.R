library(data.table)
data<-fread("E:\\AFINAL_PROJECT\\R\\creditcard.csv\\newpaysimdata2.csv")
data<-as.data.frame(data)
data<-data[,-4]
data<-data[,-5]
data<-data[,-1]
data<-data[,-3]
View(data)
data$age<-as.factor(data$age)
data$gender<-as.factor(data$gender)
data$category<-as.factor(data$category)
data$fraud<-as.factor(data$fraud)
index<-sample(nrow(data),nrow(data)*.7,replace = TRUE)
train<-data[index,]
test<-data[-index,]

library(class)
library(e1071)
data3naive<-naiveBayes(formula = fraud~.,data = train)
data3naivepred<-predict(data3naive,test)


A(data3naivepred,test$fraud)

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


