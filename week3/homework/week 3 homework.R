setwd("D:/ernie/self-study/GTxMicroMasters/Introduction to Analytics Modeling/week3/homework")
# Question 7.2
# Using the 20 years of daily high temperature data for Atlanta 
# (July through October) from Question 6.2 (file temps.txt), 
# build and use an exponential smoothing model to help make a judgment 
# of whether the unofficial end of summer has gotten later over the 20 years.
library(magrittr)
library(tidyverse)
library(lubridate)
library(corrplot)
library(leaps)
weather <- data.frame(read.table("temps.txt" , header = T))%>%
  select(.,- DAY) %>%
  unlist()%>%
  as.vector()%>%
  ts(start = 1996 , end = 2015 , frequency = 100)
  # gather(year,temp,-DAY)%>%
  # mutate(year = as.factor(year),
  #        date = paste(DAY,year,sep = "-"))%>%
  # mutate(date = dmy(date),
  #        month = month(date),
  #        day = day(date))%>%
plot(weather)
HoltWinters(weather)

#Qustion 8.2
crime <- read.table("uscrime.txt" , header = TRUE)%>%
  data.frame()
pl1 <- corrplot(cor(crime))
pl1
# remove Po2 due to correlation with Po1
cor_relation <- abs(cor(crime$Crime , crime[,1:15]))%>%
  data.frame()
cor_relation <- cor_relation%>%
  gather(predictor, correlation)
cor_relation
pl2 <- ggplot(data = cor_relation, aes( x  = predictor , y = correlation , fill = correlation )) +
  geom_col()
pl2
#remove NW,U1,So due to low correlation

model <- lm (data = crime , Crime ~ Ed + Ineq + LF + M + M.F +Po1 + Pop + Prob  + Time )
summary(model)
test <- data.frame(   M = 14.0,
                      So = 0,
                      Ed = 10.0,
                      Po1 = 12.0,
                      Po2 = 15.5,
                      LF = 0.640,
                      M.F = 94.0 ,
                      Pop = 150,
                      NW = 1.1,
                      U1 = 0.120,
                      U2 = 3.6,
                      Wealth = 3200,
                      Ineq = 20.1,
                      Prob = 0.04,
                      Time = 39.0
                      )
predict(model,test)

