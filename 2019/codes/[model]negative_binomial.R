

setwd("E:/2019학회/따릉이3")


library(data.table)
library(lubridate)

train_y<-fread('train_y.csv')
test_y<-fread('test_y.csv')


Sys.setlocale("LC_TIME", "english") # for writing 'day' variable as English 


# making 'day' variable for train data 
df_train = data.frame(rent_date=train_y$rent_date, loc_num=train_y$loc_num) 
df_train$day <- weekdays(as.Date(df_train$rent_date))


# making 'day' variable for test data
df_test = data.frame(rent_date=test_y$rent_date, loc_num=test_y$loc_num) 
df_test$day <- weekdays(as.Date(df_test$rent_date))




########################################################
###### Interpretation Modeling _ Negative Binomial #####
########################################################



#process data#

train<-fread('data_new_index.csv')
train<-train[,-1]  # erase 'loc_num' column for reducing dimension of factor loc_num

train<-as.data.frame(train)


#making variables as factor#

for ( i in c(7,17,19,20,14,18,13) ) #gu/ networkgroup/ month/ day/holiday/bikeroad / festival
  
{train[,i]<-as.factor(train[,i])}

str(train)

#check VIF for multicollinearity in train data

#install.packages('usdm')
library(usdm)
df <- train[,2:20]   
vif(df) 



# modeling except mean_temp variable due to multicollinearity #

library(MASS)

fit_negbin <- glm.nb(y~.-mean_temp, data = train)
summary(fit_negbin)


# train <- within(train, rent_month <- relevel(rent_month, ref = 4)) # for comparing variables of factors with different reference level 


# caluculating 5-fold CV error for training data #

cost <- function(obs, hat) { MAPE(obs,hat) } # function for MAPE 

cv.negbin<-cv.glm(data=train , glmfit=fit_negbin, cost=cost(train$y,tr.yhat) , K=5)
cv.negbin$delta[1]









