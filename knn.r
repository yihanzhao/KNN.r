#load the data and use dist() to get a distance matrix
data3<-read.csv(file="/Users/zhaoyihanzhao/Desktop/data mining/homework/homework3/hw03_q3.csv",header=TRUE)
x1_x2<-cbind(data3$x1,data3$x2)
x1_x2
dis<-dist(x1_x2)
dis.matrix<-as.matrix(dis)
dis.matrix
dis.matrix[dis.matrix==0] <- NA #change all 0's to NA
#use the first data point as the testing data and the rest as the training set. Implement KNN. 
library(class)
x<-data3[,1:2]
y<-data3[,3]
dis.x<-dist(x)
dis.matrix<-as.matrix(dis.x)
dis.matrix[dis.matrix==0] <- NA 
test_x<-data3[1,1:2]
test_y<-data3[1,3]
train_x<-data3[-1,1:2]
train_y<-data3[-1,3]
dis.train<-dist(train_x)
dis.train.matrix<-as.matrix(dis.train)
dis.train.matrix[dis.train.matrix==0] <- NA 
dim(dis.train.matrix)

#for training data 2:20#
order.train<-vector()
for (i in 1:19){
  order.train.one<-(order(dis.train.matrix[i,]))+1
  order.train<-rbind(order.train,order.train.one)
}
rownames(order.train)<-paste("data point" , 1:ncol(order.train)+1, sep="")
order.train
#allocate all points' nearest neighbours; row: different points; col:sequence of nearest neigh
#find all predicted y for 19 data points based on k=1 to 10 
MSE_train<-vector()
for (i in 1:10){
  predict_y_one<-apply((matrix(y[ order.train[,1:i]],nrow=i,ncol=19,byrow=TRUE)),2,mean)
  MSE_dif_k<-(sum((predict_y_one-y[-1])^2))/19
  MSE_train<-cbind(MSE_train,MSE_dif_k)
}
colnames(MSE_train)<-paste("k=" , 1:ncol(MSE_train), sep="")
MSE_train
class(MSE_train)
plot(c(MSE_train))
#####For test data########
order.dist<-vector()
for (i in 1:20){
  order.one<-order(dis.matrix[i,])
  order.dist<-rbind(order.dist,order.one)
}
rownames(order.dist)<-paste("data point" , 1:ncol(order.dist), sep="")
order.dist
MSE_test<-vector()
for (i in 1:10){
  predict_test_y_one<-apply((matrix(y[order.dist[,1:i]],nrow=i,ncol=20,byrow=TRUE)),2,mean)
  MSE_test_dif_k<-(predict_test_y_one-test_y)^2
  MSE_test<-cbind(MSE_test,MSE_test_dif_k)
}
colnames(MSE_test)<-paste("k=" , 1:ncol(MSE_test), sep="")
MSE_test[1,]
plot(c(MSE_test[1,]))
#perform leave one out cross validation 
dis.matrix.train<-vector()
for (i in 1:20){#20 kinds of testing data; each has 19 training data, 19*19 distance matrix
  train_data_x<-data3[-i,1:2]
  train_data_y<-data3[-i,3]
  test_data_x<-data3[i,1:2]
  test_data_y<-data3[i,3]
  dis.train.c<-dist(train_data_x)
  dis.train<-as.matrix(dis.train.c)
  dis.train[dis.train==0] <- NA 
  order.matrix<-vector()
  for (j in 1:19){#for each distance matrix, we have 19 rows, order each row, have 19 ordered row
    order.dis<-order(dis.train[j,])
    order.colname<-ifelse(order.dis<i,order.dis,order.dis+1) 
    order.matrix<-rbind(order.matrix,order.colname)
  }
  dis.matrix.train<-cbind(dis.matrix.train,order.matrix)
}
dim(dis.matrix.train)
#split the distance matrix into 20 sub matrix according to different test x
msplit<-lapply(1:20,function(col){
  dis.matrix.train[,((col-1)*19+1):((col-1)*19+19)]
})
#MSE for training data based on different test data from k from 1 to 10 
MSE_dif_train<-vector()
for (j in 1:20){
  MSE_k<-vector()
  for (i in 1:10){
    predict_y_c<-apply(matrix(y[msplit[[j]][,1:i]],nrow=i,ncol=19,byrow=TRUE),2,mean)
    MSE_k_one<-(sum((predict_y_c-y[-j])^2))/19
    MSE_k<-cbind(MSE_k,MSE_k_one)
  }
  MSE_dif_train<-rbind(MSE_dif_train,MSE_k)
}
colnames(MSE_dif_train)<-paste("k=" , 1:ncol(MSE_dif_train), sep="")
rownames(MSE_dif_train)<-paste("test data=" , 1:nrow(MSE_dif_train), sep="")
MSE_dif_train
MSE_train_k<-apply(MSE_dif_train,2,mean)
plot(MSE_train_k)
MSE_train_k
#####For test data########
order.dist<-vector()
for (i in 1:20){
  order.one<-order(dis.matrix[i,])
  order.dist<-rbind(order.dist,order.one)
}
rownames(order.dist)<-paste("data point" , 1:ncol(order.dist), sep="")
dim(order.dist)
MSE_k_test<-vector()
for (i in 1:10){
  predict_y_test_c<-apply((matrix(y[order.dist[,1:i]],nrow=i,ncol=20,byrow=TRUE)),2,mean)
  MSE_k_test_one<-(predict_y_test_c-y)^2
  MSE_k_test<-cbind(MSE_k_test,MSE_k_test_one)
}

colnames(MSE_k_test)<-paste("k=" , 1:ncol(MSE_k_test), sep="")
rownames(MSE_k_test)<-paste("test data=" , 1:nrow(MSE_k_test), sep="")
MSE_k_test
a<-apply(MSE_k_test,2,mean)
plot(a)
