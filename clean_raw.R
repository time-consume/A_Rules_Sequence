library("dplyr")
load("raw.rdata")
#Bio information
bio<-dat[c("HHIDPN","YEAR")]
#Disease
dis<-dat[,c(218:225)]
#Condition
cond<-dat[,c("ADLSUM","UPPERSUM","LOWERSUM","IADLSUM","STRONGSUM")]
#Death
dead<-dat[c("DEAD")]
#Combine the variables we need
mydata<-cbind(bio,dis,cond,dead)
#For Dead=1 record, other variables be missing, replaced by 0 
mydata[is.na(mydata)]<-0
#For sum>=1, replaced by 1
mydata[mydata$ADLSUM>0,]$ADLSUM<-1
mydata[mydata$UPPERSUM>0,]$UPPERSUM<-1
mydata[mydata$LOWERSUM>0,]$LOWERSUM<-1
mydata[mydata$IADLSUM>0,]$IADLSUM<-1
mydata[mydata$STRONGSUM>0,]$STRONGSUM<-1

#Loop
for (i in c(1:13)) {
  mydata[,i+16]<- ave(mydata[,i+2], mydata$HHIDPN, FUN=cumsum)
  mydata[,i+16]<- ave(mydata[,i+16], mydata$HHIDPN, FUN=cumsum)
  mydata[mydata[,i+16]!=1,i+16]<-0
}
colnames(mydata)[17:29]<-names(mydata)[3:15]
#exp
#mydata$try<-mydata$HHIDPN*mydata$`Flag 1`
#pass test
#all first occurance is marked with 1
cook<-mydata[,c(1,2,17:29,16)]
cook$add<-apply(cook[,c(3:10)],1,sum)
cook$max<-ave(cook$add,cook$HHIDPN,FUN = max)
#eliminate 6.2% data
dat<-cook[cook$max!=0,1:16]
save(dat,file ="cleaned.rdata" )
#Time difference of 2.3823 secs

