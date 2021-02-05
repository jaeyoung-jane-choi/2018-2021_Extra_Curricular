library(prophet)
library(data.table)
library(dplyr)
library(pan)
df <- fread("train_y.csv")
unique(sort(df$loc_num))


loc_num <- rep(unique(sort(df$loc_num)), each= 365)
length(loc_num)
start_date <- as.Date("2017-10-01")
end_date <- as.Date("2018-09-30")
date_set <-seq(as.Date(start_date), as.Date(end_date), by = "day")
rent_date <- rep(date_set, 443)
length(rent_date)
df_1 <- data.frame(loc_num, rent_date)

df_1$merge_key <- paste(df_1$loc_num, df_1$rent_date, sep='-')


df$merge_key <- paste(df$loc_num, df$rent_date, sep='-')


new<-left_join(df_1, df, by = 'merge_key')

#na자리에 0넣는거
new[is.na(new$y),'y'] <- 0
#근데 0넣으면 안됨

#그래서 그냥 na 둔다
new_data <- new[,-c(3,4,5)]
names(new_data) <- c('loc_num', 'rent_date', 'y')

##########################################################################################

as.data.frame(new_data)
new_data <- new_data %>% group_by(loc_num, rent_date) %>% summarize(y)

unique(new_data$loc_num)
###대여소별 prophet을 443번 돌려야하기 때문에 이렇게! 101만 뽑자
#######디플라이어개새끼왜안되냐뒤져라
a <- new_data %>% filter(new_data$loc_num == '101')
a<-a[,-2]
#####뒤져라뒤져라

condition<-c(new_data$loc_num=='101')
a1<-new_data[condition,]
a1 <- a1[,-1]

a1 <- a1[c('rent_date','y')]
colnames(a1)<-c('ds','y')

playoff <- data_frame(
  holiday = 'playoff',
  ds = as.Date(c('2017-10-02', '2017-10-03', '2017-10-04', '2017-10-05', 
                 '2017-10-06', '2017-10-09', '2017-12-25', '2018-01-01',
                 '2018-02-15', '2018-02-16', '2018-02-17', '2018-03-01',
                 '2018-05-05', '2018-05-07', '2018-05-22', '2018-06-06',
                 '2018-06-13', '2018-08-15', '2018-09-23', '2018-09-24',
                 '2018-09-25', '2018-09-26', '2018-10-03', '2018-10-09',
                 '2018-12-25')),
  lower_window = 0,
  upper_window = 1)

m<- prophet(a1, growth = 'linear',seasonality.mode = 'multiplicative', yearly.seasonality=FALSE, weekly.seasonality = TRUE, daily.seasonality=FALSE, holidays = playoff, 
            holidays.prior.scale = 10, changepoint.range = 0.9)
######################################이걸로 month 추가가능하다는데 안됨
m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
############################################
m<- prophet(a1, growth = 'linear', seasonality.mode = 'multiplicative', yearly.seasonality=TRUE, daily.seasonality=TRUE, weekly.seasonality = TRUE,
            holidays = playoff, holidays.prior.scale = 10, changepoint.range = 0.9 )

future<- make_future_dataframe(m, periods = 62)
forecast<- predict(m,future)
tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')])

plot(m, forecast)

a1.cv <- cross_validation(m, initial = 289, period = 12, horizon = 62, unidts = "days" )
##시각화
plot(m, forecast)
prophet_plot_components(m, forecast)
dyplot.prophet(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)

##cv로 mape 확인

head(a1.cv)
a1.p <- performance_metrics(a1.cv)
head(a1.p)
plot_cross_validation_metric(a1.cv,metric = 'mape')
range(a1.p$mape, decreasing=T)
mean(a1.p$mape)

##proophet grid 
prophetGrid <- expand.grid(changepoint_prior_scale = c(0.5, 0.7, 1.0),
                           changepoint_prior_scale = c(0.5, 0.7, 0.9),
                           holidays_prior_scale = c(100, 10, 1),
                           growth = 'linear')


## Search best parameters
for (i in seq_len(nrow(prophetGrid))) {
  parameters <- prophetGrid[i, ]
  if (parameters$growth == 'linear')
    m <- prophet(a, growth = parameters$growth, holidays = playoff,
                 seasonality.prior.scale = parameters$seasonality_prior_scale, 
                 changepoint.prior.scale = parameters$changepoint_prior_scale,
                 holidays.prior.scale = parameters$holidays_prior_scale,
                 seasonality.mode = 'multiplicative',
                 yearly.seasonality=T,daily.seasonality=F,weekly.seasonality = T)
  
  future <- make_future_dataframe(m, periods = 62)
  forecast <- predict(m, future)
  results[i] <- forecast::accuracy(forecast[forecast$ds %in% test$ds, 'yhat'], test$y)[ , 'MAPE']
}

#---------------------------------------------------------------------
m<- prophet(a1,growth = 'linear', seasonality.mode = 'multiplicative',changepoint.range = 0.8,changepoint.prior.scale = 0.7,yearly.seasonality=T,
            daily.seasonality=F,weekly.seasonality = T, holidays = playoff, holidays.prior.scale = 10)
future<- make_future_dataframe(m, periods = 62)
forecast<- predict(m,future)
tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')])

plot(m, forecast)
plot(m, forecast) + add_changepoints_to_plot(m)
prophet_plot_components(m, forecast)
dyplot.prophet(m, forecast)

#-------------------------------------------------------------------
#################for 문
sp_new_data <- split(new_data, new_data$loc_num)

for (k in 1:443) {
  z <- sp_new_data[[k]]
  a <- z[-1,]
  colnames(a) <-c('ds','y')
  playoff <- data_frame(
    holiday = 'playoff',
    ds = as.Date(c('2017-10-02', '2017-10-03', '2017-10-04', '2017-10-05', 
                   '2017-10-06', '2017-10-09', '2017-12-25', '2018-01-01',
                   '2018-02-15', '2018-02-16', '2018-02-17', '2018-03-01',
                   '2018-05-05', '2018-05-07', '2018-05-22', '2018-06-06',
                   '2018-06-13', '2018-08-15', '2018-09-23', '2018-09-24',
                   '2018-09-25', '2018-09-26', '2018-10-03', '2018-10-09',
                   '2018-12-25')),
    lower_window = 0,
    upper_window = 1)

  m<- prophet(a, growth = 'linear', yearly.seasonality=TRUE, daily.seasonality=TRUE, holidays = playoff, holidays.prior.scale = 10)
  future<- make_future_dataframe(m, periods = 62)
  forecast<- predict(m,future)
  tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')])
  
  a.cv <- cross_validation(m, initial = 289, period = 12, horizon = 62, units = "days" )
  a.p <- performance_metrics(a.cv)
  plot_cross_validation_metric(a.cv,metric = 'mape')
  mean(a.p$mape)
}


