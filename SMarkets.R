
rm(list = ls())

library(RCurl)
library(rjson)
library(rvest)


## Will be used later to get the names into correct format
nameFunc<- function(x){
  x<-strsplit(x, "\t")
  x<- do.call(rbind, x)
  x<- paste(x[,1], x[,2], sep = " ")
}
#########################################################
### Get data ###

#USER file
users<- fromJSON(getURL("https://smarkets.herokuapp.com/users"))
users<- lapply(users, function(x){unlist(x)})
users<- as.data.frame(users)
users<- t(users)
rownames(users)<- NULL
users<- as.data.frame(users)
users$id<- as.numeric(as.character(users$id))
users$affiliate_id<- as.numeric(as.character(users$affiliate_id))
out <- strsplit(as.character(users$created), "\\.")
out<- do.call(rbind, out)
colnames(out)<- c("DateTime", "numFormat")
users<- cbind.data.frame(users, out)
users$name<- sapply(as.character(users$name), nameFunc)

#AFFILIATES file
affiliates<- fromJSON(getURL("https://smarkets.herokuapp.com/affiliates"))
affiliates<- lapply(affiliates, function(x){unlist(x)})
affiliates<- as.data.frame(affiliates)
affiliates<- t(affiliates)
rownames(affiliates)<- NULL
affiliates<- as.data.frame(affiliates)
affiliates$id<- as.numeric(as.character(affiliates$id))
affiliates$name<- sapply(as.character(affiliates$name), nameFunc)
affiliates$website<- as.character(affiliates$website)


#BETS file
# Scrape the data
for(i in c(0:499)){
  bets<- fromJSON(getURL(paste("http://smarkets.herokuapp.com/users/",i,"/bets", sep = "")))
  bets<- lapply(bets, function(x){unlist(x)})
  bets<- as.data.frame(bets)
  bets<- t(bets)
  rownames(bets)<- NULL
  bets<- as.data.frame(bets)
  write.table(bets, "bets.csv",append = TRUE, col.names = FALSE)
}
bets<- read.csv("bets.csv", header = FALSE)
names(bets)<- c("id", "user_id", "amount", "percentage_odds", "created", "result")
out <- strsplit(as.character(bets$created), "\\.")
out<- do.call(rbind, out)
colnames(out)<- c("DateTime", "numFormat")
bets<- cbind.data.frame(bets, out)
bets<- bets[c("id", "user_id", "amount", "percentage_odds", "created", "DateTime", "numFormat", "result")]

#########################################################
### Analysis
library(sqldf)

###############################################################################################
#  Interview Question 1: "Find the affiliate with the maximum number of users."

ans_1<- sqldf("SELECT name
              FROM affiliates
              WHERE id IN (SELECT affiliate_id
              FROM users
              group by affiliate_id
              ORDER BY COUNT(name) DESC LIMIT 1)")
# Ava Bell


###############################################################################################

# Interview Question 2: "Find the amount won by users coming through the
#                        top 3 affiliates - by user_count."

bets$result<-as.character(bets$result)
ans_2<- sqldf("SELECT SUM(amount) FROM bets
               WHERE user_id IN (SELECT id FROM users
                                 WHERE affiliate_id IN (SELECT affiliate_id
                                                        FROM users GROUP BY affiliate_id
                                                        ORDER BY COUNT(name) DESC LIMIT 3)) AND result='TRUE'")
# 4616.454



###############################################################################################

# Interview Question 3: "What is the percentage of users who have won 2 or more bets with low odds - say 25%."


total<- sqldf("SELECT COUNT(DISTINCT name) FROM users")

numUsers<- sqldf("SELECT count(id)
               FROM users
               WHERE id IN
               (SELECT user_id
                FROM bets
                WHERE percentage_odds<=25 AND result='TRUE'
                GROUP BY user_id
                HAVING COUNT(*) > 1
               )
               ORDER BY id")
ans_3<-  paste(round((numUsers/total)*100), "percent")

# 6 percent