#Question 9.1
setwd("D:/ernie/self-study/GTxMicroMasters/Introduction to Analytics Modeling/week4/homework")
library(tidyverse)
set.seed(101)
crime <- read.table("uscrime.txt",header = T) %>% 
  data.frame()
#original model from 8.2 with 9 variables
model <- lm (data = crime , Crime ~ Ed + Ineq +
               LF + M + M.F +Po1 + Pop + Prob  + Time )
summary(model)

pca <- prcomp(formula =  ~  So + Ed + Ineq + LF + M + M.F +Po1 +
                Pop + Prob  + Time  + Po2 + NW + U1 +U2 + Wealth ,
              data = crime , scale = T ,center = T)
pca

#Plotting Scree plot
# Kaiser eigenvalue-greater-than-one rule
Scree <- plot(pca,         
              type="line", 
              main="Scree Plot for crime factors")%>%
              abline(h=1, col="blue")

# Calculate  and Plot the variances and proportion of variances 

var <- pca$sdev^2
propvar <- var/sum(var)

#plotting
por.explained <- plot(propvar, xlab = "Principle component" , ylab = "Porpotion explained" , type = "s")
acc.por.explained <- plot(cumsum(propvar) , xlab = "Principle component" , ylab = " AccamulatedPorpotion explained" , type = "o")

#choose 4 new variables(variance > 1)
pca.chosen <- pca$x[, 1:4]
pca.chosen


#Combining PCAs  with crime 
new.crime <- cbind(pca.chosen, crime[,16]) %>%
  data.frame()
colnames(new.crime)[5] <- "Crime"
new.model <- lm ( Crime ~ PC1 + PC2 + PC3 + PC4 ,data = new.crime )
summary(new.model)

#Transforming new data back to original variable
PCAs <- new.model$coefficients[2:5]
intercept <- new.model$coefficients[1]
original <- pca$rotation[,1:4] %*% PCAs
original
PCAs
# un-scaling data

origi.var <- original/sapply(crime[,1:15],sd)
origi.inter <-  intercept - sum(original*sapply(crime[,1:15],mean)/sapply(crime[,1:15],sd))

origi.var

#Trying with 5 PCAs
#choose 4 new variables(variance > 1)
pca.chosen2 <- pca$x[, 1:5]
pca.chosen2


#Combining PCAs  with crime 
new.crime2 <- cbind(pca.chosen2, crime[,16]) %>%
  data.frame()
colnames(new.crime2)[6] <- "Crime"
new.model2 <- lm ( Crime ~ PC1 + PC2 + PC3 + PC4 + PC5 ,data = new.crime2 )
summary(new.model2)


#Question 10.1
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(modelr)
#CART
# set up train and testing split
train <- createDataPartition(crime$Crime, p = .85, list = F)
# set up test and train datasets
crime.train <- crime[train,]
crime.test <- crime[-train,]
# check splits
dim(crime.train); dim(crime.test)

crime.tree <- train(
  Crime ~ .,
  data = crime.train,
  method = 'rpart',
  trControl = trainControl(method = 'boot_all', number = 10),
  metric = 'RMSE'
)
crime.tree$finalModel
prp(crime.tree$finalModel)
#Random Forest
crime.forest <- train(
  Crime ~ .,
  data = crime.train,
  method = 'rf',
  trControl = trainControl(method = 'boot_all', number = 10),
  metric = 'RMSE')
crime.forest$finalModel
#Testing
crime.res1 <- crime.test %>%
  add_predictions(., crime.tree) %>%
  select('observations' = Crime, pred) %>%
  as.data.frame()
crime.res2 <- crime.test %>%
  add_predictions(., crime.forest) %>%
  select('observations' = Crime, pred) %>%
  as.data.frame()
crime.res1
crime.res2
postResample(obs = crime.res1$observations, pred = crime.res1$pred)
postResample(obs = crime.res2$observations, pred = crime.res2$pred)
# tree <- rpart (Crime ~., method = "class" , data = crime)
# printcp(tree)
# summary(tree)
# prp(tree)


#10.3
set.seed(101)
credit <- read.table("germancredit.txt", header = FALSE)
str(credit)
credit$V21[credit$V21==1] <- 0
credit$V21[credit$V21==2] <- 1
#Dividing data
credit.part <- createDataPartition(credit$V21, times = 1, p = 0.7, list=FALSE)
head(credit.part)
credit.train <- credit[credit.part,] 
credit.valid <- credit[-credit.part,]
credit.log <- glm(V21 ~ ., data = credit.train, family=binomial(link="logit"))
summary(credit.log)
creditPredict <- predict(credit.log, newdata=credit.valid[,-21], type="response")
Confusion.mat <- table(credit.valid$V21, round(creditPredict))
Sensitivity <- Confusion.mat[1,1]/sum(Confusion.mat[1,])
Sensitivity
Specfitity <-Confusion.mat[2,2]/sum(Confusion.mat[2,])
Specfitity
#setting second thresh hold
threshold <- 0.7
thres <- as.matrix(table(round(creditPredict > threshold), credit.valid$V21))
names(dimnames(thres)) <- c("Predicted", "Observed")
thres
Sensitivity2 <- thres[1,1]/sum(thres[1,])
Sensitivity2
Specfitity2 <-thres[2,2]/sum(thres[2,])
Specfitity2
