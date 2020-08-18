setwd("D:/ernie/self-study/GTxMicroMasters/Introduction to Analytics Modeling/week 1")
credit <- read.table("credit_card_data-headers.txt" , header = T)
head(credit)
library(kernlab)
library(kknn)
library(ggplot2)
library(dplyr)
library(caTools)


#2-2.1
#creating first model
#(linear,"vanilladot")
set.seed(101)
model.1 <- ksvm(x = as.matrix(credit[,1:10]),
                y = as.factor(credit[,11]),
                type = "C-svc" ,
                scaled = TRUE , 
                kernel = "vanilladot" ,
                C = 100)
model.1
a <- colSums(model.1@xmatrix[[1]]*model.1@coef[[1]])
a0 <- model.1@b

#result
pred1 <- predict(model.1,credit[,1:10])
res1 <- sum(pred1 == credit [,11]) / nrow(credit)

#choosing best model : c =1 ~100
test.c <- list(1:100)
acc <- data.frame(matrix(ncol = 2, nrow = 100))
names(acc) <- c("c","accuracy")
for (i in test.c){
  model <- ksvm(x = as.matrix(credit[,1:10]),
                y = as.factor(credit[,11]),
                type = "C-svc" ,
                scaled = TRUE , 
                kernel = "vanilladot" ,
                C = i)
  pred <- predict(model,credit[,1:10])
  res.0 <- sum(pred1 == credit [,11]) / nrow(credit)
  acc[i,1] <- i
  acc[i,2] <- res.0
}

svm.plt <-ggplot(acc, aes(x = c , y = accuracy)) + geom_point() + geom_line(lty = "dotted" , color = "red")
svm.plt
#2.2.2
#Using other non-linear models
#Radial Basis kernel "Gaussian"
model.2 <- ksvm(x = as.matrix(credit[,1:10]),
                y = as.factor(credit[,11]),
                type = "C-svc" ,
                scaled = TRUE , 
                kernel = "rbfdot" ,
                C = 100)
model.2
b <- colSums(model.2@xmatrix[[1]]*model.2@coef[[1]])
b0 <- model.1@b
#result
pred2 <- predict(model.2,credit[,1:10])
res2 <- sum(pred2 == credit [,11]) / nrow(credit)
#Polynomial kernel
model.3 <- ksvm(x = as.matrix(credit[,1:10]),
                y = as.factor(credit[,11]),
                type = "C-svc" ,
                scaled = TRUE , 
                kernel = "polydot" ,
                C = 100)
model.3
c <- colSums(model.3@xmatrix[[1]]*model.3@coef[[1]])
c0 <- model.3@b
#result
pred3 <- predict(model.3,credit[,1:10])
res3 <- sum(pred3 == credit [,11]) / nrow(credit)
#" Hyperbolic tangent kernel"
model.4 <- ksvm(x = as.matrix(credit[,1:10]),
                y = as.factor(credit[,11]),
                type = "C-svc" ,
                scaled = TRUE , 
                kernel = "tanhdot" ,
                C = 100)
model.4
d <- colSums(model.4@xmatrix[[1]]*model.4@coef[[1]])
d0 <- model.4@b
#result
pred4 <- predict(model.4,credit[,1:10])
res4 <- sum(pred4 == credit [,11]) / nrow(credit)
pred.list <- c(res1,res2,res3,res4)
kernel.list <- c("Linear","Radial Basis" ,"Polynomial" ,"Hyperbolic tangent")
result.df <- data.frame(kernel.list ,pred.list)
#best model
result.df
result.df[2,]

#2.2.3
R1 <-credit[,11]
pred5<- rep(0,(nrow(credit))) 

for (i in 1:nrow(credit)){
  #making sure that i won't use it self
  knn.model=kknn(R1~., credit[-i,],credit[i,],k=1, scale = T) 
  pred5[i]<- as.integer(fitted(knn.model)+0.5)
}

res5 = sum(pred5 == R1) / nrow(credit)
res5
knn.df <- data.frame(matrix(nrow = 30, ncol = 2))
colnames(knn.df) <- c("k" , "accuracy")
for(n in 1:30){
  for (i in 1:nrow(credit)){
    #ensure it doesn't use i itself 
    knn_model=kknn(R1~., credit[-i,],credit[i,],k=n, scale = T) 
    #pred5 <-  predict(knn_model ,credit[,1:10])
    pred5[i] <- as.integer(fitted(knn_model)+0.5) #for rounding
    res.00 <- sum(pred5 == R1) / nrow(credit)
    knn.df[n,1]<- n
    knn.df[n,2]<- res.00
  }
}
knn.plt <-ggplot(knn.df, aes(x = k , y = accuracy)) + geom_point() + geom_line(lty = "dotted" , color = "red")
knn.plt

#3.1
#a
#k-fold Cross validation
acc2 <- data.frame(matrix(nrow = 30 ,ncol = 2))
names(acc2) <- c("k" , "accuracy")
for (i in 1:30){
  knn_model2 <- cv.kknn(R1~ ., credit , kcv = 10 , k = i, scale = T)
  pred6 <- round(knn_model2[[1]][,2])
  res6 <- sum(pred6 == credit [,11]) / nrow(credit)
  acc2[i,1] <- i
  acc2[i,2] <- res6
}
acc2
knn.plt2 <-ggplot(acc2, aes(x = k , y = accuracy)) + geom_point() + geom_line(lty = "dotted" , color = "red")
knn.plt2
#best model
acc2[11,]

#b
#splitting data
train.index <- sample(nrow(credit),nrow(credit) * 0.7)
train.data <-  credit[train.index,]
remaining_data <- credit[-train.index,]
vad.index <- sample(nrow(remaining_data),nrow(remaining_data) * 0.5)
vad.data <- remaining_data[vad.index,]
test.data <- remaining_data[-vad.index,]
nrow(test.data) + nrow(vad.data) + nrow(train.data) == nrow(credit)
acc3 <- data.frame(matrix(nrow = 30 ,ncol = 2))
names(acc3) <- c("k" , "accuracy")
pred7<- rep(0,(nrow(vad.data)))
for(n in 1:30){
    knn_model3=kknn(R1~., train = train.data,test = vad.data,k=n, scale = T) 
    res7 <- sum(knn_model3$fitted.values == vad.data$R1) / nrow(vad.data)
    acc3[n,1]<- n
    acc3[n,2]<- res7
  }
acc3
knn.plt3 <-ggplot(acc3, aes(x = k , y = accuracy)) + geom_point() + geom_line(lty = "dotted" , color = "red")
knn.plt3
knn_model4=kknn(R1~., train = train.data,test = test.data,k=1, scale = T) 
sum(knn_model4$fitted.values == test.data$R1) / nrow(test.data)
#------------------------------failed-------------------------------------
# for (k in 1:10){
#   k.index <- sample(nrow(train_vad.data),52 , replace = F)
#   k.group <- list (data.frame(train_vad.data[k.index,]))
#   train_vad.lis <- c(train_vad.lis ,k.group)
#   train_vad.data <- train_vad.data[-k.index,]
# }
#for (n in 1:30){}  
# for (k in 1:10){
#   #train.9df <- do.call(rbind, train_vad.lis[-k])
#   v#ad.1df <-data.frame(train_vad.lis[k])
#   # knn.lis<- list()
#   train.9df <- do.call(rbind, train_vad.lis[-1])
#   vad.1df <-data.frame(train_vad.lis[1])
#   for (i in 1:nrow(train.9df)){
#     #knn.df2<- data.frame(matrix(nrow = 468 , ncol = 2))
#     pred6<- rep(0,(nrow(vad.1df)))
#     knn.model2=kknn(R1~., train.9df,vad.1df,k=12, scale = T)
#     #pred6 <- predict(knn_model,vad.1df)
#     pred6[i] <- as.integer(fitted(knn.model2)+0.5)
#     res6 <- sum(pred6 == train.9df ) / nrow(train.9df)
#     #knn.df2[n,1]<- n
#     #nn.df2[n,2]<- res.00
#     #knn.lis <- c(knn.lis,knn.df2)
#     print(res6)
#   }
#   
# }
# knn.lis
# pred4

