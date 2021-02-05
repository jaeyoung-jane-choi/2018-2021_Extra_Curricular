rm(list=ls())
library(faraway)
library(mgcv)
library(data.table)
library(dplyr)
train <- fread("data_final_train.csv")
test <- fread("data_final_test.csv")

train <- as.data.frame(train)
#train[,c(1,2,9,15,16,19,20,21,22)] <- as.factor(train[,c(1,2,9,15,16,19,20,21,22)])
train[,1] <- as.factor(train[,1])
train[,2] <- as.factor(train[,2])
train[,9] <- as.factor(train[,9])
train[,15] <- as.factor(train[,15])
train[,16] <- as.factor(train[,16])
train[,19] <- as.factor(train[,19])
train[,20] <- as.factor(train[,20])
train[,21] <- as.factor(train[,21])
train[,22] <- as.factor(train[,22])

###########################
gam_train <- gam(log(y)~s(rent)+s(wind)+s(humidity)+s(temp)+s(ibh)+ s(dpg)+s(ibt)+s(vis)+s(doy), family=gaussian(link=identity),data=ozone)
summary(gam_train)

m1 <- gam(count ~ s(distance) + status, data=bush_status, family="nb")
m2 <- gam(count ~ s(distance, by=status) + status, data=bush_status, family="nb")

Gam.object <- gam(y~s(x,6)+z,data=gam.data)
###########################
gam_train <- gam(y ~ s(mean_temp, by = rent_date) + s(precip) + s(wind) + s(humid) + s(diff_temp) + s(NO2) + s(O3) + s(SO2) + s(pm10) + s(pm2.5) +  s(hanlyang_index) + s(slave_index) + 
                   rent_date + loc_num + gu + redday + festival + networkgroup + bikeroad + rent_month + day, 
                 family = gaussian(), data = train)

gam_train2 <- gam(y ~ s(mean_temp, by= c(rent_date, loc_num)), family = gaussian(), data = train)
gam.check(gam_train2)

gam_train <- gam(y ~ s(mean_temp) + s(precip) + s(wind) + s(humid) + s(diff_temp) + s(NO2) + s(O3) + s(SO2) + s(pm10) + s(pm2.5) +  s(hanlyang_index) + s(slave_index) + 
                    gu + redday + festival + networkgroup + bikeroad + rent_month + day, 
                 family = gaussian(link=identity), data = train)


#s(rent_date) + s(loc_num) + s(gu), family = gaussian() + s(redday) + s(festival) + s(networkgroup) + s(bikeroad) + s(rent_month) + s(day)

gam.check(gam_train)          

#########################
library(faraway)
library(mgcv)
library(data.table)
library(dplyr)
train <- fread("data_final_train.csv")
test <- fread("data_final_test.csv")

train <- as.data.frame(train)
#train[,c(1,2,9,15,16,19,20,21,22)] <- as.factor(train[,c(1,2,9,15,16,19,20,21,22)])
train[,1] <- as.factor(train[,1])
train[,2] <- as.factor(train[,2])
train[,9] <- as.factor(train[,9])
train[,15] <- as.factor(train[,15])
train[,16] <- as.factor(train[,16])
train[,19] <- as.factor(train[,19])
train[,20] <- as.factor(train[,20])
train[,21] <- as.factor(train[,21])
train[,22] <- as.factor(train[,22])
gam_train <- gam(y ~ s(mean_temp) + s(precip) + s(wind) + s(humid) + s(diff_temp) + s(NO2) + s(O3) + s(SO2) + s(pm10) + s(pm2.5) +  s(hanlyang_index) + s(slave_index) + 
                    gu + redday + festival + networkgroup + bikeroad + rent_month + day, 
                 family = gaussian(link=identity), data = train)

gam.check(gam_train) 
summary(gam_train)

library(ggplot2)
ggplot(train, aes(mean_temp, y)) + geom_point() + geom_smooth(method = "gam", formula = y ~  s(mean_temp) + s(precip) + s(wind) + s(humid) + s(diff_temp) + s(NO2) + s(O3) + s(SO2) + s(pm10) + s(pm2.5) +  s(hanlyang_index) + s(slave_index) + 
                                                                gu + redday + festival + networkgroup + bikeroad + rent_month + day)

par(mfrow = c(3,3))
gam.check(gam_train)

preds=predict(gam_train,newdata= train) 
plot(preds)

#AplotmethodforGAMobjects, which canbeused onGLM andLMobjects aswell. It focuseson terms (main-effects), and produces a suitable plot for terms of different types
plot(gam_train, residuals = TRUE, rugplot = TRUE, ask = TRUE,scale = 10) 
return


CVgam(formula=y ~ s(mean_temp) + s(precip) + s(wind) + s(humid) + s(diff_temp) + s(NO2) + s(O3) + s(SO2) + s(pm10) + s(pm2.5) +  s(hanlyang_index) + s(slave_index) + 
          gu + redday + festival + networkgroup + bikeroad + rent_month + day,
        data = train, nfold = 5, debug.level = 0, method = "GCV.Cp",
        printit = TRUE, cvparts = NULL, gamma = 1, seed = 29)

gam_MSE <- mean(gam_error)


data(train)
library(sp)
library(gamclass)
CVgam(gam_train, train, nfold = 5, debug.level = 0, method = "GCV.Cp",  
      printit = TRUE, cvparts = NULL, gamma = 1, seed = 29)
library(caret)
library(plyr)
model <- train(mpg ~ hp, train, method = "lm", trControl = trainControl(method = "cv", number = 10))

#####################################

k = 5

train$id <- sample(1:k, nrow(train), replace = TRUE)
list <- 1:k


# prediction and test set data frames that we add to with each iteration over
# the folds
prediction <- data.frame()
testsetCopy <- data.frame()
#데이터 프레임 초기화


#아래는 쓸데 없지만 그냥 멋있음을 위해 존재
#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)
#|================================================================================================================| 100%
#progress bar는 위의 모양같이 생겼고 굳이 넣을 필요없는데 비쥬얼을 위해서 넣음



#function for k fold
#i는 1부터 5로 나눈후에 5번을 진행하도록 합니다.

for(i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(train, id %in% list[-i])
  testset <- subset(train, id %in% c(i))
  #데이터를 5등분하고 한번 뽑은 test data가 다시 train 으로 가지 않도록 5등분 합니다.
  
  #run a random forest model
  mymodel <- gam(y ~ s(mean_temp) + s(precip) + s(wind) + s(humid) + s(diff_temp) + s(NO2) + s(O3) + s(SO2) + s(pm10) + s(pm2.5) +  s(hanlyang_index) + s(slave_index) + 
                                gu + redday + festival + networkgroup + bikeroad + rent_month + day, 
                              family = gaussian(link=identity), data = train)
  #랜덤 포레스트 알고리즘으로 꽃받침의 길이를 나머지 데이터로 예측하는 모델을 만듭니다.
  
  #remove response column 1, Sepal.Length
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  
  # append this iteration's predictions to the end of the prediction data frame
  prediction <- rbind(prediction, temp)
  
  # append this iteration's test set to the test set copy data frame
  # keep only the Sepal Length Column
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  
  
}


progress.bar$step()

# add predictions and actual Sepal Length values
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
summary(result)

result$Difference <- abs(result$Actual - result$Predicted)

plot(result)

# As an example use Mean Absolute Error as Evalution
summary(result$Difference)

par(mfrow=c(1,3))
plot(mymodel, se=TRUE, col='blue')
