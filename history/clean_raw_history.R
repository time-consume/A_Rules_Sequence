#get first occurance of 8 kind of disease for each people 
library("dplyr")
load("raw.rdata")
dis<-dat[,c("HHIDPN","YEAR","HEARTR","HYPERR","STROKER","ARTHRITISR","CANCERR","DIABETESR","COPDR","PSYCHR","ADLSUM","UPPERSUM","LOWERSUM","DEAD")]
dis[is.na(dis)]<-0
dis[dis$ADLSUM>0,]$ADLSUM<-1
dis[dis$UPPERSUM>0,]$UPPERSUM<-1
dis[dis$LOWERSUM>0,]$LOWERSUM<-1


group<-dis%>%group_by(HHIDPN)

group<-group %>%  group_by(HHIDPN,HEARTR) %>% mutate(count1 = c(1:n()))
group<-group %>%  group_by(HHIDPN,HYPERR) %>% mutate(count2 = c(1:n()))
group<-group %>%  group_by(HHIDPN,STROKER) %>% mutate(count3 = c(1:n()))
group<-group %>%  group_by(HHIDPN,ARTHRITISR) %>% mutate(count4 = c(1:n()))
group<-group %>%  group_by(HHIDPN,CANCERR) %>% mutate(count5 = c(1:n()))
group<-group %>%  group_by(HHIDPN,DIABETESR) %>% mutate(count6 = c(1:n()))
group<-group %>%  group_by(HHIDPN,COPDR) %>% mutate(count7 = c(1:n()))
group<-group %>%  group_by(HHIDPN,PSYCHR) %>% mutate(count8 = c(1:n()))
group<-group %>%  group_by(HHIDPN,ADLSUM) %>% mutate(count9 = c(1:n()))
group<-group %>%  group_by(HHIDPN,UPPERSUM) %>% mutate(count10 = c(1:n()))
group<-group %>%  group_by(HHIDPN,LOWERSUM) %>% mutate(count11 = c(1:n()))
group<-group %>%  group_by(HHIDPN,DEAD) %>% mutate(count12 = c(1:n()))


group$flag1<-ifelse(group$HEARTR*group$count1==1,group$flag1<-1,group$flag1<-0)
group$flag2<-ifelse(group$HYPERR*group$count2==1,group$flag2<-1,group$flag2<-0)
group$flag3<-ifelse(group$STROKER*group$count3==1,group$flag3<-1,group$flag3<-0)
group$flag4<-ifelse(group$ARTHRITISR*group$count4==1,group$flag4<-1,group$flag4<-0)
group$flag5<-ifelse(group$CANCERR*group$count5==1,group$flag5<-1,group$flag5<-0)
group$flag6<-ifelse(group$DIABETESR*group$count6==1,group$flag6<-1,group$flag6<-0)
group$flag7<-ifelse(group$COPDR*group$count7==1,group$flag7<-1,group$flag7<-0)
group$flag8<-ifelse(group$PSYCHR*group$count8==1,group$flag8<-1,group$flag8<-0)
group$flag9<-ifelse(group$ADLSUM*group$count9==1,group$flag9<-1,group$flag9<-0)
group$flag10<-ifelse(group$UPPERSUM*group$count10==1,group$flag10<-1,group$flag10<-0)
group$flag11<-ifelse(group$LOWERSUM*group$count11==1,group$flag11<-1,group$flag11<-0)
group$flag12<-ifelse(group$DEAD*group$count12==1,group$flag12<-1,group$flag12<-0)


newdis<-group[,c(1,2,27:38)]
colnames(newdis)<-c("HHIDPN","YEAR","HEARTR","HYPERR","STROKER","ARTHRITISR","CANCERR","DIABETESR","COPDR","PSYCHR","ADLSUM","UPPERSUM","LOWERSUM","DEAD")
mytry<-newdis[,c(3:10)]
mytry$add<-apply(mytry,1,sum)
mytry$HHIDPN<-newdis$HHIDPN
mytry$csum <- ave(mytry$add, mytry$HHIDPN, FUN=cumsum)
mytry<-mytry %>%  group_by(HHIDPN) %>% mutate(health = max(csum))



newdis<-newdis[mytry$health!=0,]

save(newdis,file ="cleaned.rdata" )
#Time difference of 5.803041 secs

