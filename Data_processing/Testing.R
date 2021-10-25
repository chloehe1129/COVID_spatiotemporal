## Download testing data
#author: "Jooyoung Lee"
#date: "5/11/2020"

library(lubridate)
library(tidyverse)
library(zoo)

testing <- read.csv("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv")
### Stat-level
testing$date <- ymd(testing$date)
testing <-testing[order(testing$state,testing$date),]
testing <- testing[,c("date", "state", "positiveIncrease", "negativeIncrease", "totalTestResultsIncrease", "positive", "negative", "totalTestResults")]
names(testing)[2] <- "State_abbrev"

testlist <- split(testing, testing$State_abbrev)
for(i in 1:length(testlist)){
  testlist[[i]]$positiveIncrease[1] <- testlist[[i]]$positive[1]
  testlist[[i]]$negativeIncrease[1] <- testlist[[i]]$negative[1]
  testlist[[i]]$totalTestResultsIncrease[1] <- testlist[[i]]$totalTestResults[1]
}

test <- do.call(rbind, testlist)
test <- test[, c("date",  "State_abbrev", "positiveIncrease", "negativeIncrease", "totalTestResultsIncrease")]
names(test) <- c("date",  "State_abbrev", "pos_test", "neg_test", "test")

stateinfo <- read.csv("https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_info.csv")
stateinfo <- stateinfo[, c("state", "fips", "name")]
names(stateinfo) <- c("State_abbrev", "stateFIPS", "state")
test <- merge(stateinfo,test,by="State_abbrev")
test <- test[order(test$state, as.Date(test$date)),]

# Change NA to 0
test[,c(5:7)][is.na(test[,c(5:7)])] <- 0
# Change negaative to positive
test$pos_test = ifelse(test$pos_test<0,-test$pos_test,test$pos_test)
test$neg_test = ifelse(test$neg_test<0,-test$neg_test,test$neg_test)
test$test = test$pos_test + test$neg_test

write.csv(test,"~/Desktop/covid19/Data/daily_testing.csv")
write.csv(stateinfo,"~/Desktop/covid19/Data/stateinfo.csv")

## Weekly testing: start from Wednesday

test$date <- as.Date(test$date)
#tmp <- test[, c("date", "state", "pos_test", "neg_test", "test")] %>% group_by(week = cut(date, "week"), state) 
tmp <- test[, c("date", "state", "pos_test", "neg_test", "test")] %>% group_by(week = week(date), state) 

state_weekly_test <- aggregate(.~week+ state , data=tmp, sum, na.rm=TRUE)
state_weekly_test <- state_weekly_test[, -3]
state_weekly_test$date <- ymd("2020-01-01" ) + weeks(state_weekly_test$week - 1 ) 
state_weekly_test <- state_weekly_test[, c("week", "date", "state", "pos_test", "neg_test", "test")]
state_weekly_test$state = as.character(state_weekly_test$state)
state_weekly_test$state <- ifelse(state_weekly_test$state=="District Of Columbia", "District of Columbia", state_weekly_test$state)


write.csv(state_weekly_test,"~/Desktop/covid19/Data/weekly_testing.csv")
