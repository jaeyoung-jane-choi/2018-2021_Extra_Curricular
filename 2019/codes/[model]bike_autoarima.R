library(data.table)
library(dplyr)
library(tibble)
library(devtools)
library(tseries)
library(astsa)
library(itsmr)
library(forecast) ## 패키지 로딩 순서 꼭 지킬것

train=fread("train_y.csv")
## 250대여소로 한번 해볼까?
plot(t250$y)
t250=filter(train, loc_num==250)
t250_y=t250$y

t250_y=ts(t250_y) ## 시계열 data 만들기
test(t250_y) ## iid검사 !white noise -> modeling 진행

arima_250<-auto.arima(t250_y) ## 알아서 차분해주는 autoarima 
practice_forecast<-forecast(arima_250, h=61) 
tsdisplay(residuals(arima_250))  #acf와 pacf okay 

plot(practice_forecast)

as.vector(practice_forecast$mean)

# 모든 대여소에 적용하기 위한 for문
locnum=sort(unique(train$loc_num))


predict_arima=as.data.frame(matrix(0, nrow=7,ncol=443))

for (i in 1:443) 
{
  a<-filter(train, loc_num==locnum[i])
  a<-a$y
  a<-ts(a)
  arima<-auto.arima(a)
  pred <- forecast(arima, h=7)
  
  
  predict_arima[,i]<-  pred[[4]][1:7]
  colnames(predict_arima)[i]<-locnum[i]
  
}

############################### skip
## if 정상성 검사를 따로 한다면

adf.test(t250_y) ## 정상성 불만족

acf2(t250_y) ## 흠... 차분해야겠군

decom_t250=diff(t250_y) ##차분
adf.test(decom_t250) ## 정상성 만족
acf2(decom_t250)

test(decom_t250) ## 화이트노이즈 아님


