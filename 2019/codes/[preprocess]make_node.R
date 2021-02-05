# node data

library(data.table)
library(dplyr)
library(tidyr)

## 전처리팀에서 준 코드로 전처리 
final=fread("realfinal.csv")


glimpse(final2)
range(unique(final$loc_num))

##  mergekey, loc_num,loc_name, rent_num, return_num을 뽑음 (unique쓸거라 mergekey 꼭 필요)   
final2 <- final[,c(1,7,8,10,11)] 
final2 <- unique(final2)
final2$mergekey<-NULL ## mergekey 이제 필요없음
final2$total_num <- final2$rent_num+final2$return_num

unique(final$loc_num) %>% length()


## 이제 일자별로 나누어진 대여소별 출납, 반납 횟수를 대여소기준으로 합침
final3=final2 %>% group_by(loc_num) %>% summarize(rent_sum_tot=sum(rent_num), 
                                                    return_sum_tot=sum(return_num), total_num_tot=sum(total_num))
final3$loc_num <- as.integer(final3$loc_num)
final3=final3 %>% arrange(loc_num)
final3=as.data.frame(final3)


## 마지막으로 대여소 이름 가져오기
jam=final[,c(7,8)]
jam=arrange(jam, loc_num)
final3$loc_name = unique(jam)[,2]

write.csv(final3, file = "node.csv")

