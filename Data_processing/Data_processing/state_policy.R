## Stay at home order and Mask
#author: "Jooyoung Lee"
#date: "5/22/2020"

library(lubridate)
library(tidyverse)
stayathome <- read.csv("Data/state_stayathome.csv")

tmp <- stayathome
tmp <- tmp[rep(seq_len(nrow(tmp)), each=102),]
tmp$date <- seq(as.Date("2020-3-1"), as.Date("2020-6-10"), by = "day")
tmp$date <- as.Date(tmp$date)
tmp$Start.Date <- as.Date(tmp$Start.Date)
tmp$End.Date <- as.Date(tmp$End.Date)
tmp$stayathome <- 0
mylist <- split(tmp, as.factor(tmp$state))

for(i in 1:length(mylist)){
  mylist[[i]]$stayathome <- ifelse(mylist[[i]]$date < mylist[[i]]$Start.Date, 0, 1)
  mylist[[i]]$stayathome <- ifelse(mylist[[i]]$date > mylist[[i]]$End.Date, 0, mylist[[i]]$stayathome)
}

state_stayathome <- do.call(rbind, mylist)
state_stayathome$stayathome <- ifelse(is.na(state_stayathome $stayathome), 0, state_stayathome$stayathome)


write.csv(state_stayathome, "Data/state_daily_Shome_policy.csv")

state_stayathome <- read.csv("Data/state_daily_Shome_policy.csv")[,-1]

tmp <- state_stayathome %>% group_by(week = week(date), state) 
tmp$state <- as.factor(tmp$state)
tmp$date <- as.Date(tmp$date)

tmp$stayathome <- as.numeric(tmp$stayathome)

tmp <- tmp[, c(2, 7, 8)]
tmp2 <- aggregate(.~week+ state, data=tmp, mean, na.rm=TRUE)

tmp2$date <- ymd("2020-01-01" ) + weeks(tmp2$week - 1 )
tmp2$stayathome <- ifelse(tmp2$stayathome>=0.5, 1, 0)
tmp2 <- tmp2[, c("date", "state", "stayathome")]
state_stayathome <- merge(tmp2, state_stayathome[, c("state", "Start.Date", "End.Date", "date")], by=c("state", "date"))
write.csv(state_stayathome, "Data/state_weekly_Shome_policy.csv")



## Policy data
policies = read.csv("https://raw.githubusercontent.com/chloehe1129/COVID-19/master/Clean_cases/clean_policies.csv")
policies = policies[,-4] # get rid of FIPS
policies_state = policies[!duplicated(policies[,c("State","date","Mask")]),]
Mask_state = policies_state[, c("State", "date", "Mask")]
names(Mask_state)[1] <- "state"
tmp<- Mask_state %>% group_by(week = week(date), state) 
tmp$state <- as.factor(tmp$state)
tmp$date <- as.Date(tmp$date)
tmp2 <- aggregate(.~week+ state, data=tmp, mean, na.rm=TRUE)
tmp2$Mask <- ifelse(tmp2$Mask>=0.5, 1, 0)
tmp2$date <- ymd("2020-01-01" ) + weeks(tmp2$week - 1 )

policy <- merge(state_stayathome, tmp2, by=c("state", "date"), all.x=T)
policy <- policy[, c("state", "date", "stayathome", "Start.Date", "End.Date", "Mask")]
policy$Mask <- ifelse(is.na(policy$Mask), 1, policy$Mask)
write.csv(policy, "Data/state_weekly_policy.csv")
