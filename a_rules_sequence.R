#arulesequemce
library(dplyr)
library(tidyverse)
library(arulesSequences)
#9 variables 
load("cleaned.rdata");dat<-newdis
#get the matrix so exclude HHIPD AND YEAR FIRST
dt<-dat[,-c(1:3)]
#clear missing value for death
dt[is.na(dt)]<-0
#
mydata <- as(as.matrix(dt),"transactions")
mydata<-as(mydata,"data.frame")
dat$items<-mydata$items
#get the final form to work on
focus<-dat[,c(1,2,15)]
#fix the items and size
focus$items <- gsub(',',';', focus$items)
focus$items <- gsub('^.|.$','', focus$items)
focus$items<-as.character(focus$items)
focus[,4]<-focus$items
focus[,3]<-apply(dt, 1, sum)
names(focus) = c("sequenceID", "eventID", "SIZE", "items")
focus <- data.frame(lapply(focus, as.factor))
focus <- focus[order(focus$sequenceID, focus$eventID),]

#
write.table(focus, "medium.txt", sep=";", row.names = FALSE, col.names = FALSE, quote = FALSE)
trans_matrix <- read_baskets("medium.txt", sep = ";", info = c("sequenceID","eventID","SIZE"))


#takes a long time to run
#to search for support
start<-Sys.time()
s1 <- cspade(trans_matrix,parameter = list(support = 0.01,maxgap = 4,maxsize=3),control= list(verbose = TRUE,numpart=10))
print(Sys.time()-start)
s1.df <- as(s1, "data.frame")
summary(s1)


#get the rules
r1 <- as(ruleInduction(s1, confidence = 0.01, control = list(verbose = TRUE)), "data.frame")


# Separate LHS and RHS rules
r1$rulecount <- as.character(r1$rule)
max_col <- max(sapply(strsplit(r1$rulecount,' => '),length))
r_sep <- separate(data = r1, col = rule, into = paste0("Time",1:max_col), sep = " => ")
#clean Time1
r_sep$Time1 <- substring(r_sep$Time1,2,nchar(r_sep$Time1)-1)
#trial<-strsplit(r_sep$Time1,' }, ')
#rm(trial)
max_origin <- max(sapply(strsplit(r_sep$Time1,','),length))
r_sep_new <- separate(data = r_sep, col = Time1, into = paste0("Origin",1:max_origin), sep = ",")
#clean Time2
r_sep$Time2 <- substring(r_sep$Time2,3,nchar(r_sep$Time2)-2)
r_sep_new<-separate(data = r_sep_new,col=Time2,into = paste("OUTCOME",1:2),sep = ",")

write.csv(r_sep_new,"Focus.csv")

