# 성대 사거리 대여소 x변수 채우기

te=fread("data_final_test.csv")
colnames(te)
te$festival

skku=matrix(0,1,22)
colnames(skku)=colnames(te)
skku[1]="2019-05-30"
skku[2]=358
skku[3]=NA
skku[4]=19.6
skku[5]=0
skku[6]=2.3
skku[7]=0.6
skku[8]=9.9
skku[9]="종로구"
skku[10]=0.0300
skku[11]=0.04500
skku[12]=0.00400
skku[13]=30.0
skku[14]=18.0
skku[16]=1
skku[17]=4
skku[18]=3
skku[19]=3
skku[20]=1
skku[21]=5
skku[22]="Friday"
skku

write.csv(skku, file = "skku_crossed_street.csv",row.names = F)


##  아래는 대여소에 관한 x 찾기용
final=fread("realfinal.csv")
te[te$loc_num==358,]

net=fread("refre.csv")
net[net$loc_num==358,]
