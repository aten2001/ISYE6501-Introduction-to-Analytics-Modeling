setwd("D:/ernie/self-study/GTxMicroMasters/Introduction to Analytics Modeling/week2/homework")
iris <- read.table("iris.txt")
library(tidyverse)
library(factoextra)
library(cluster)
#4.2
# The iris data set iris.txt contains 150 data points, 
# each with four predictor variables and one categorical response. 
# The predictors are the width and length of the sepal and 
# petal of flowers and the response is the type of flower.
# Use the R function kmeans to cluster the points as well as possible. 
# Report the best combination of predictors, 
# your suggested value of k, and how well your best clustering predicts flower type.

#Visualization

pl <- ggplot(iris, aes(Petal.Length ,Petal.Width, color = Species))+
  geom_point(size = 4)
print(pl)
#model
set.seed(101)
print(head(iris))
irisCluster <- kmeans(iris[,1:4], 3, nstart = 20)
comparison <-table(irisCluster$cluster, iris$Species)
comparison
clusplot(iris, irisCluster$cluster , color = T, shade = T, labels = 0,lines = 0)

#testing numbers of k
test_mat<- data.frame(matrix(nrow = 15, ncol = 2,0))
for (k in 1:15){
  irisCluster_test <- kmeans(iris[,1:4], k, nstart = 20)
  j <- irisCluster_test$tot.withinss
  test_mat[k,1] <- k
  test_mat[k,2] <- j
}
colnames(test_mat) <- c("k" , "Total")# Total within-cluster sum of squares
test_mat
ggplot(test_mat , aes(x = k , y = Total ))+
  geom_point()+
  geom_line()

#5.1
# Using crime data from the file uscrime.txt, 
# test to see whether there are any outliers in the last column 
# (number of crimes per 100,000 people). 
# Use the grubbs.test function in the outliers package in R.
library(outliers)
crime <- read.table("uscrime.txt" , header = T)
crimePerHundred <-crime$Crime
summary(crimePerHundred)
grubbs.test(crimePerHundred)
ggplot(crime, aes(y = Crime)) + 
  geom_boxplot(outlier.color = "red" ,width = 5)

#6.2
# 1. Using July through October daily-high-temperature data 
# for Atlanta for 1996 through 2015, 
# use a CUSUM approach to identify when unofficial summer ends 
# (i.e., when the weather starts cooling off) each year.
library(qcc)
library(dplyr)
library(tidyr)
library(lubridate)
#loading and tidyiing data
weather <- data.frame(read.table("temps.txt" , header = T))%>%
  gather(year,temp,-DAY)%>%
  mutate(year = as.factor(year),
         date = paste(DAY,year,sep = "-"))%>%
  mutate(date = dmy(date),
         month = month(date),
         day = day(date))
summary(weather)
#average temperture per year
avg_by_year <-weather %>%
  group_by(year)%>%
  summarise(avg_year = mean(temp))
avg_by_year 
 pl2 <-ggplot(weather, aes(x = date, y = temp , color = year))+
   geom_boxplot()
 pl2
#average temperture per month
avg_by_month <-weather %>%
  group_by(month)%>%
  summarise(avg_month = mean(temp))
avg_by_month
pl3 <-ggplot(weather, aes(x = month, y = temp,group = month))+
  geom_boxplot()
pl3

#cusum
weather_2 <- data.frame(read.table("temps.txt" , header = T)) 
day_mean <- rowMeans(weather_2[,-1])
summary(day_mean)
# mean - Xi (want to find decrease)
total_mean<- mean(day_mean)
diff <- total_mean - day_mean %>%
  data.frame()
#setting C
thresh <- 85
C <- 4 #half way between median and 1st quarter
diff <- diff - C
diff<- mutate(diff, date = weather_2$DAY)
colnames(diff)<- c("Xi-m-C" , "date")
diff
S <- matrix(nrow = 124,0)

for (i in 1:123){
  if (i == 1){
    S[i+1,1] <- max(0,S[1,1]+diff[i,1])
  }else{
    S[i+1,1] <- max(0,S[i,1]+diff[i,1])
  }
  
}
S<- data.frame(S)
diff <- diff%>%
  mutate(S = S[2:124,1])
diff
pl4 <- ggplot(diff, aes(x= seq_along(diff[,2]), y = diff[,3] ))+
        geom_point()+
        geom_line()
pl4
#threshold set at 10
S2 <-subset(diff,S>10)
S2

#2.Use a CUSUM approach to make a judgment of 
#whether Atlanta’s summer climate has g
#gotten warmer in that time (and if so, when).
ann_data<-weather%>% 
        group_by(year)%>%
         summarise(year_avg = mean(temp))
year_mean<- mean(ann_data$year_avg)
sd(ann_data$year_avg)
year_mean
#setting C
C2 <- 1
thresh2 <- 2#total of C and threshold would be approx. 2 Sd. away from mean

ann_data <-ann_data %>%
  mutate(diff = year_avg - year_mean-C2)%>%
  data.frame()
S2 <-matrix(nrow = 21,0)
for (i in 1:20){
  if (i == 1){
    S2[i+1,1] <- max(0,S2[1,1]+ann_data[i,3])
  }else{
    S2[i+1,1] <- max(0,S2[i,1]+ann_data[i,3])
  }
  
}
S2 <- data.frame(S2)
ann_data <-ann_data %>%
        mutate(S = S2[2:21,])
pl5 <- ggplot(ann_data, aes(x= year, y = S , group = 1))+
  geom_point()+
  geom_line()
pl5
subset(ann_data, S > thresh2)
