######### prepare xgb
library(xgboost)
library(MLmetrics)
library(sdcTable)
library(data.table)
library(dplyr)
library(dummies)
library(caret)
library(Metrics)

set.seed(1)

### train, test 합친다음 factor들 onehot encoding, xgb의 input인 xgb_matrix로 만들기
tr.dat=fread("data_final_train.csv")
te.dat=fread("data_final_test.csv")
colnames(te.dat)

jamci=rbind(tr.dat, te.dat)

jamci$rent_date<-NULL
jamci$loc_num =NULL

gu=as.data.frame(dummy(jamci$gu))
network=as.data.frame(dummy(jamci$networkgroup))
rent_month=as.data.frame(dummy(jamci$rent_month))
day=as.data.frame(dummy(jamci$day))

jamci.onehot=jamci
colnames(jamci.onehot)
jamci.onehot[,c(7,17,19,20)]=NULL
jamci.onehot=cbind(jamci.onehot, gu, network, rent_month, day)



dtrain=jamci.onehot[1:155461,]
dtest=jamci.onehot[155462:179293,]

rm(day, gu, jamci, jamci.onehot, network, rent_month, te.dat,tr.dat)
gc()
glimpse(dtrain)

dtrain=xgb.DMatrix(as.matrix(dtrain[,-1]), label=dtrain$y)
dtest=xgb.DMatrix(as.matrix(dtest[,-1]), label=dtest$y)

#### 우리의 metric인 mape function (xgb eval metric에 넣을거)
mape_xgb_fun<- function(preds, dtrain){
  library(Metrics)
  labels <- getinfo(dtrain, "label")
  my_mape <- Metrics::mape(labels, preds)
  return(list(metric = "mape", value = round(my_mape,3)))
}


max.depth.grid=seq(4,8,2)

###################### parameter tuning
max.depth.grid=seq(8,14,2)
min.child.grid=seq(3,9,2)
col.sample.grid=c(0.6,0.7,0.8)

xg_tune_2=as.data.frame(matrix(nrow=length(max.depth.grid)*length(col.sample.grid),ncol=3,NA))
colnames(xg_tune_2)=c("max_depth","col.sample.bytree","MAPE")

x=1

for (i in max.depth.grid) {
  for (j in col.sample.grid) {
    params_1=list(objective="reg:linear",
                  eval_metric=mape_xgb_fun,
                  eta=0.1,
                  max_depth=i,
                  min_child_weight=5,
                  subsample=0.8, colsample_bytree=j)
    xgb_cv_1 = xgb.cv(params = params_1,
                      data=dtrain,
                      nrounds = 500,
                      nfold = 5,
                      verbose = 0,
                      prediction = T,
                      early_stopping_rounds = 100,
                      nthread=2,
                      maximize = F)
    
    best_mape=xgb_cv_1$evaluation_log[,min(test_mape_mean)]
    xg_tune_2[x,1]=i
    xg_tune_2[x,2]=j
    xg_tune_2[x,3]=best_mape
    x=x+1
  }
  print("AHH")
}

xg_tune_1
xg_tune_2

## parameter grid search plot
xg_tune_1=c(0.3828,0.3806,0.3776,0.3794,0.3762,0.3736,0.3782,0.3766,0.3754,0.3840,0.3818,0.3728)
xg_tune_1=cbind(max.depth.grid, col.sample.grid, xg_tune_1)
xg_tune_1=as.data.frame(xg_tune_1)
xg_tune_1$max.depth.grid=c(8,8,8,10,10,10,12,12,12,14,14,14)
colnames(xg_tune_1)=c("max.depth","col.sample.bytree","MAPE")

max.depth.grid=seq(8,14,2)
min.child.grid=seq(3,9,2)
col.sample.grid=c(0.6,0.7,0.8)


ggplot(xg_tune_1, aes(x=max.depth, y=col.sample.bytree))+geom_tile(aes(fill=MAPE), colour="white")+
  scale_fill_gradient(low="white", high="red")+scale_x_continuous(breaks = max.depth.grid)+
  scale_y_continuous(breaks=col.sample.grid) + ggtitle("GRID SEARCH")




####### final cv error

params_3=list(objective="reg:linear",
              eval_metric=mape_xgb_fun,
              eta=0.01,
              max_depth=14,
              min_child_weight=5,
              subsample=0.8, colsample_bytree=0.8)

xgb_cv_3 = xgb.cv(params = params_3,
                  data=dtrain,
                  nrounds = 10000,
                  nfold = 5,
                  verbose = 0,
                  prediction = T,
                  early_stopping_rounds = 500,
                  nthread=2,
                  maximize = F)

xgb_cv_3$evaluation_log[,min(test_mape_mean)]
as.data.frame(xgb_cv_3$evaluation_log)[172,]


## final xgb
params=list(objective="reg:linear",
              eval_metric=mape_xgb_fun,
              eta=0.01,
              max_depth=14,
              min_child_weight=5,
              subsample=0.8, colsample_bytree=0.8, nrounds=190)

xgb_final=xgb.train(params = params, data = dtrain, nrounds = params$nrounds,  
                      maximize = F, verbose = F)

yhat=predict(xgb_final, newdata = dtest)
MAPE(yhat, te.dat$y)
