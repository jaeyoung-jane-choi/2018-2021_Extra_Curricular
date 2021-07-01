# 2018년 휴일 변수 만들기
library(tibble)
library(dplyr)
d1=as.Date("2018-01-01")
for (i in 1:364) {
  d1 <- c(d1, d1[1]+i)
} 


d2=rep(0,365)
d2[c(6,7)] <-1
for (i in 1:51) {
  u <- 7*i
  d2[c(6+u,7+u)] <- 1
} ## 토, 일요일


heuil_2018=data.frame(date=d1, heuil=d2)
heuil_2018 <- heuil_2018 %>% remove_rownames %>% column_to_rownames(var="date")

heuil_2018[c("2018-01-01","2018-02-15","2018-02-16","2018-03-01","2018-05-07",
             "2018-05-22","2018-06-06","2018-06-13","2018-08-15","2018-09-24",
             "2018-09-25","2018-09-26","2018-10-03","2018-10-09","2018-12-25"),] <- 1 ## 주말 제외 공휴일

write.csv(heuil_2018, file = "heuil_2018.csv", row.names = T)

