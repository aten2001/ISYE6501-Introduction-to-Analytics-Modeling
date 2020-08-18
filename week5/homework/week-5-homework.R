# Question 11.1
# Using the crime data set uscrime.txt from Questions 8.2, 9.1, and 10.1, build a regression model using:
# 1. Stepwise regression
setwd("D:/ernie/self-study/GTxMicroMasters/Introduction to Analytics Modeling/week5/homework")
#loading library
library(tidyverse)
library(caret)
library(egg)
library(stargazer)
library(modelr)
library(glmnet)
library(foreach)
library(FrF2)
#loading data
crime <- read.table("uscrime.txt" , header = T)
head(crime)
#dividing into training and testing data sets
set.seed(101)
train.index <- createDataPartition(crime$Crime , p = 0.8 ,times = 1, list = F)
train <- crime[train.index,]
test <- crime[-train.index,]

#Forward stepwise
null = lm (Crime ~1, data = train) #setting upper bound
full = lm(Crime ~., data = train) #setting lower bound

#step function

forward.sel <- step(null,
                    scope = list(lower = null , upper = full),
                    direction = "forward")
summary(forward.sel)

pred.for <- predict(forward.sel,test)
actual <- test$Crime
comparison.for <- data.frame(pred.for,actual)
forward.p <- ggplot(data = comparison.for , 
                    aes(x = actual, y = pred.for))+
              geom_point (color = "blue") +
              geom_line()
forward.p
#Testing
res.forw <- test %>%
  add_predictions(.,forward.sel) %>%
  select('observations' = Crime, pred) %>%
  as.data.frame()
forward <- postResample(obs = res.forw$observations, pred = res.forw$pred)

#Backwards stepwise
backward.sel <- step( full,
  scope = list(upper = full , lower = null),
  direction = "backward")
summary(backward.sel)

pred.back <- predict(backward.sel,test)
comparison.back <- data.frame(pred.for,actual)
backward.p <- ggplot(data = comparison.back ,
                     aes(x = actual, y = pred.back))+
                      geom_point (color = "blue") +
                      geom_line()
#Testing
res.back <- test %>%
  add_predictions(.,backward.sel) %>%
  select('observations' = Crime, pred) %>%
  as.data.frame()
backward <- postResample(obs = res.back$observations, pred = res.back$pred)
#ggarrange(forward.p,backward.p)

# Stepwise regression
step.both.1 <- step(null, scope = list(upper = full) , direction = "both")
step.both.2 <- step(full, scope = list(upper = full), direction = "both")
summary(step.both.1)
summary(step.both.2)
#Testing
res.both.1 <- test %>%
  add_predictions(.,step.both.1) %>%
  select('observations' = Crime, pred) %>%
  as.data.frame()
res.both.2 <- test %>%
  add_predictions(.,step.both.2) %>%
  select('observations' = Crime, pred) %>%
  as.data.frame()
#Prediction 
stepwise.fromNull <- postResample(obs = res.both.1$observations, pred = res.both.1$pred)
stepwise.fromFull <- postResample(obs = res.both.2$observations, pred = res.both.2$pred)




# 2. Lasso
#divide data
set.seed(101)
train.index <- createDataPartition(crime$Crime , p = 0.8 ,times = 1, list = F)
train <- crime[train.index,]
test <- crime[-train.index,]



#modeling
lasso <- glmnet(x = scale(as.matrix(train[,-16])), 
                y =scale(as.matrix(train[,16]))  ,
                family = "gaussian" , 
                alpha = 1)


summary(lasso$beta)

#finding suitable lambda value
cv.lasso <- cv.glmnet(x = scale(as.matrix(train[,-16])), 
                   y =scale(as.matrix(train[,16]))  ,
                   family = "gaussian" , 
                   alpha = 1)

best.lambda = cv.lasso$lambda.min
best.lambda
plot(lasso, xvar='lambda', main="Lasso")
abline(v=log(best.lambda), col="blue", lty=5.5 )

#choosing coefficients
coef(cv.lasso, s = "lambda.min")
select.ind = which(coef(cv.lasso, s = "lambda.min") != 0)
select.ind = select.ind[-1]-1 # remove Intercept
select.ind  # which one is important

important <- colnames(train[select.ind])
#Regression model
lasso.reg <- lm(Crime~., data = train[,c(important,"Crime")])
summary(lasso.reg)
predict(lasso.reg,test)
res.lasso <- test %>%
  add_predictions(.,lasso.reg) %>%
  select('observations' = Crime, pred) %>%
  as.data.frame()
Lasso.regression <- postResample(obs = res.lasso$observations, pred = res.lasso$pred)
Lasso.regression
#stargazer(res.lasso, type = "text"  )
# 3. Elastic net

#divide data
set.seed(101)
train.index <- createDataPartition(crime$Crime , p = 0.8 ,times = 1, list = F)
train <- crime[train.index,]
test <- crime[-train.index,]



#Finding suitable aplpha
a <- seq(0.05, 0.95, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
cv.elastic <- cv.glmnet(x = scale(as.matrix(train[,-16])), 
                  y =scale(as.matrix(train[,16]))  ,
                  family = "gaussian" , 
                  nfold = 10, 
                  type.measure = "deviance",
                  paralle = TRUE, 
                  alpha = i)
  data.frame(cvm = cv.elastic$cvm[cv.elastic$lambda == cv.elastic$lambda.1se],
             lambda.1se = cv.elastic$lambda.1se,
             alpha = i)
}
search
cv <- search[search$cvm == min(search$cvm), ]
cv
#modeling
elastic <- glmnet(x = scale(as.matrix(train[,-16])), 
                y =scale(as.matrix(train[,16]))  ,
                family = "gaussian" , 
                alpha = 0.05,
                lambda = 0.346184)
#choosing coefficients
coef(elastic, s = "lambda.min")
select.ind2 = which(coef(elastic, s = "lambda.min") != 0)
select.ind2 = select.ind[-1]-1 # remove Intercept
select.ind2  # which one is important
important2 <- colnames(train[select.ind2])
#Regression model
elastic.reg <- lm(Crime~., data = train[,c(important,"Crime")])
summary(elastic.reg)
predict(elastic.reg,test)
res.elastic <- test %>%
  add_predictions(.,elastic.reg) %>%
  select('observations' = Crime, pred) %>%
  as.data.frame()
Elastic.regression <- postResample(obs = res.lasso$observations, pred = res.lasso$pred)
Elastic.regression

#Results
Regression.summary <- stargazer(step.both.1,step.both.2 ,backward.sel,forward.sel, lasso.reg,elastic.reg, type = "text")
Regression.summary
Final.results <- data.frame(forward,backward,stepwise.fromFull,stepwise.fromNull, Lasso.regression, Elastic.regression)
Final.results

# Question 12.2
# To determine the value of 10 different yes/no features to the market value of a house (large yard, solar roof, etc.), 
# a real estate agent plans to survey 50 potential buyers, 
# showing a fictitious house with different combinations of features. 
# To reduce the survey size, the agent wants to show just 16 fictitious houses.
FrF2(16,nfactors = 10, 
     factor.names =  c("Large yard" , "solar roof" , "double restrooms" , "Garage" , "pool" ,"lawn",
                       "security system" , "Smart house system" ,"Elevator","Walk-in closet") )
