#read data
library(data.table)
data<-fread("E:\\AFINAL_PROJECT\\R\\creditcard.csv\\creditcard.csv")
data<-as.data.frame(data)
data$Class<-as.factor(data$Class)

library(ROSE)
data1<-ROSE(Class ~ ., data = data, seed = 1)

data1$Class<-as.factor(data1$Class)
index<-sample(nrow(data1$data),nrow(data1$data)*.7)
train<-data1$data[index,]
test<-data1$data[-index,]

library(h2o)
h2o.init(nthreads = 2)
htrain<-as.h2o(train)
htest<-as.h2o(test)
x<-2:29
y<-31

model<-h2o.deeplearning(x,y,training_frame = htrain,
                        activation = "Rectifier",hidden = c(200),
                        epochs = 10,distribution = "bernoulli" )
pr<-predict(model,htest)
pr1<-as.vector(pr[,1])
A(pr1,test$Class)

x<-c(17,12,14,10,11)
y<-31

model1<-h2o.deeplearning(x,y,training_frame = htrain,
                        activation = "Rectifier",hidden = c(100),
                        epochs = 10,distribution = "bernoulli" )
pr1<-predict(model1,htest)
pr2<-as.vector(pr1[,1])
A(pr2,test$Class)

x<-c(17,12,14,10,11,16,18,9,4,7)
y<-31

model2<-h2o.deeplearning(x,y,training_frame = htrain,
                        activation = "Rectifier",hidden = c(100),
                        epochs = 10,distribution = "bernoulli" )
pr2<-predict(model2,htest)
pr3<-as.vector(pr2[,1])
A(pr3,test$Class)


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


