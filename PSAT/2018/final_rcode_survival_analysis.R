
setwd('C:/Users/최재영/Desktop/final')
data<-read.csv('final.csv')

library("party")
library("maxstat")
library('rpart')
library("survival")
library("survminer")
library(leaps)


####################################

for ( i in c(2,3,4,5,6,7,9,12,13,15,16,17,18,32,33)){
  data[,i] = as.factor(data[,i])}


colSums(is.na(data))
for(i in 1:nrow(data)){
  ifelse(data$가출유무[i] == 1,data$가출유무[i] <- 1,
         data$가출유무[i] <- 0)}

table(data$가출유무)

Surv(data$age, data$가출유무==1)



########연간소득만 넣었을 때
out <- ctree(Surv(age, 가출유무)~연간소득, data=data)
plot(out)

########한번에 다 넣었을 때
out2 <- ctree(Surv(age, 가출유무) ~. , data=data)
plot(out2)


out3 <- ctree(Surv(age,가출유무) ~ 성별 + 부최종학력 + 연간소득 + damage + 방임 + 학대 + 
                학습 + 가족연락 + 친구연락 +성적만족도 + 가족여행 + 체험활동, data=data)
summary(out3)
plot(out3)

########


colnames(student)
########################
student<-data

for ( i in 1:nrow(student)){
  if(student$가출유무[i] == 2){
    student$가출유무[i] = 0
  }
}

for ( i in 1:nrow(student)){
  if(student$부종사상지위[i] == 0){
    student$부종사상지위[i] = 0
  }
  else if(student$부종사상지위[i] == 1 | student$부종사상지위[i] == 4){
    student$부종사상지위[i] = 1
  }
  else {
    student$부종사상지위[i] = 2 
  }
}

for ( i in 1:nrow(student)){
  if(student$모종사상지위[i] == 0){
    student$모종사상지위[i] = 0
  }
  else if(student$모종사상지위[i] == 1 | student$모종사상지위[i] == 4){
    student$모종사상지위[i] = 1
  }
  else {
    student$모종사상지위[i] = 2 
  }
}

for ( i in 1:nrow(student)){
  if(student$가족구성[i] == 1 | student$가족구성[i]==4 ){
    student$가족구성[i] = 0
  }
  else if(student$가족구성[i] == 2 | student$가족구성[i] == 5){
    student$가족구성[i] = 1
  }
  else {
    student$가족구성[i] = 2 
  }
}

for ( i in 1:nrow(student)){
  if(student$연간소득[i] == 0){
    student$연간소득[i] = 300
  }
}

table(student$연간소득)


for ( i in c(2,3,4,5,6,7,9,12,13,15,16,17,18,32,33)){
  student[,i] = as.factor(student[,i])
}
str(student)
colSums(is.na(student))


####### idp variable direction #######
colnames(student)
library(dplyr)
table(student$휴대전화의존도)
student <- student %>% mutate(학대 = abs(학대- 5))
student <- student %>% mutate(휴대전화의존도 = abs(휴대전화의존도- 5))

colnames(student)

#### modeling - logistic #####


####### survival analysis

runaway3 = student
colnames(runaway3)

runaway3$ID = NULL
runaway3$학교지역 = NULL

library(survival)

coxrg1 <- coxph(Surv(age, 가출유무) ~. , data = runaway3)
summary(coxrg1)

table(runaway3$가출유무)


str(runaway3)
runaway3$가족여행 = runaway3$가족여행 + 1
runaway3$damage = runaway3$damage + 1
runaway3$가출친구 = runaway3$가출친구 + 1
runaway3$비행친구수 = runaway3$비행친구 + 1
runaway3$체험활동 = runaway3$체험활동 + 1


library(survminer)
ggcoxfunctional(Surv(age, 가출유무)~ 연간소득 + log(연간소득) + sqrt(연간소득), data=runaway3)
## log
ggcoxfunctional(Surv(age, 가출유무)~ 키 +log(키) + sqrt(키), data=runaway3)
## delete
ggcoxfunctional(Surv(age, 가출유무)~ 몸무게 +log(몸무게) + sqrt(몸무게), data=runaway3)
## log
ggcoxfunctional(Surv(age, 가출유무)~ 가족여행 +log(가족여행) + sqrt(가족여행), data=runaway3)
## log
ggcoxfunctional(Surv(age, 가출유무)~ damage +log(damage) + sqrt(damage), data=runaway3)
## log
ggcoxfunctional(Surv(age, 가출유무)~ sleep +log(sleep) + sqrt(sleep), data=runaway3)
## delete
ggcoxfunctional(Surv(age, 가출유무)~ 방임 +log(방임) + sqrt(방임), data=runaway3)
ggcoxfunctional(Surv(age, 가출유무)~ 학대 +log(학대) + sqrt(학대), data=runaway3)
ggcoxfunctional(Surv(age, 가출유무)~ 학습 +log(학습) + sqrt(학습), data=runaway3)
ggcoxfunctional(Surv(age, 가출유무)~ 교우 +log(교우) + sqrt(교우), data=runaway3)
## delete
ggcoxfunctional(Surv(age, 가출유무)~ 교사 +log(교사) + sqrt(교사), data=runaway3)
ggcoxfunctional(Surv(age, 가출유무)~ 사회성 +log(사회성) + sqrt(사회성), data=runaway3)
## delete
ggcoxfunctional(Surv(age, 가출유무)~ 가족연락 +log(가족연락) + sqrt(가족연락), data=runaway3)
## log
ggcoxfunctional(Surv(age, 가출유무)~ 친구연락 +log(친구연락) + sqrt(친구연락), data=runaway3)
## delete
ggcoxfunctional(Surv(age, 가출유무)~ 휴대전화의존도 +log(휴대전화의존도) + sqrt(휴대전화의존도), data=runaway3)
## delete
ggcoxfunctional(Surv(age, 가출유무)~ 가출친구 +log(가출친구) + sqrt(가출친구), data=runaway3)
## log
ggcoxfunctional(Surv(age, 가출유무)~ 비행친구수 +log(비행친구수) + sqrt(비행친구수), data=runaway3)
## delete
ggcoxfunctional(Surv(age, 가출유무)~ 체험활동 +log(체험활동) + sqrt(체험활동), data=runaway3)
## log

runaway4 = runaway3
runaway4$연간소득 = log(runaway4$연간소득)
runaway4$키 = NULL
runaway4$몸무게 = log(runaway4$몸무게)
runaway4$가족여행 = log(runaway4$가족여행)
runaway4$damage = log(runaway4$damage)
runaway4$sleep = NULL
runaway4$교우 = NULL
runaway4$사회성 = NULL
runaway4$가족연락 = log(runaway4$가족연락)
runaway4$친구연락 = NULL
runaway4$휴대전화의존도 = NULL
runaway4$가출친구 = log(runaway4$가출친구)
runaway4$비행친구수 = NULL
runaway4$체험활동= log(runaway4$체험활동)


coxrg2 <- coxph(Surv(age, 가출유무) ~ ., data = runaway4)
summary(coxrg2)
cox.zph(coxrg2)


### martingale residuals ###
plot(log(runaway4$연간소득), resid(coxph(Surv(age,가출유무)~1, data = runaway4)))
lines(lowess(log(runaway4$연간소득), resid(coxph(Surv(age,가출유무)~1,
                                             data= runaway4)),iter =0,f= 0.6))


######### LLS ##########

fit3 = survfit(coxph(Surv( age , 가출유무)~strata(성별), data=runaway4,
                     method="breslow"), type="aalen")

plot(fit3, fun= "cumhaz", col=c("blue", "red"), lty=c(1,2))

fit4 = survfit(coxph(Surv( age , 가출유무)~strata(보호자건강), data=runaway4,
                     method="breslow"), type="aalen")
plot(fit4, fun= "cumhaz", col=c("blue","red", "black", "yellow"), lty=c(1,2,1,2))


runaway4$교내동아리 = NULL
coxrg2 <- coxph(Surv(age, 가출유무) ~ ., data = runaway4)
summary(coxrg2)
cox.zph(coxrg2)


library(MASS)
ro.step <- stepAIC(coxrg2, trace=FALSE)

extractAIC(coxrg1)
extractAIC(ro.step)

summary(ro.step)

cox.zph(ro.step)
round(exp(ro.step$coefficients),3)

