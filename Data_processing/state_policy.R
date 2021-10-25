## Stay at home order and Mask
#author: "Chloe He"
#date: "5/22/2020"

library(lubridate)
library(tidyverse)
library(zoo)
stayathome <- read.csv("~/Desktop/covid19/Data/state_stayathome.csv")

tmp <- stayathome
#tmp <- tmp[rep(seq_len(nrow(tmp)), each=102),]
tmp <- tmp[rep(seq_len(nrow(tmp)), each=length(seq(as.Date("2020-3-1"), Sys.Date(), by = "day"))),]
tmp$date <- rep(seq(as.Date("2020-3-1"), Sys.Date(), by = "day"),length(unique(stayathome$state)))
tmp$date <- as.Date(tmp$date)
tmp$Start.Date <- as.Date(as.character(tmp$Start.Date), format = "%m/%d/%y")
tmp$End.Date <- as.Date(as.character(tmp$End.Date), format = "%m/%d/%y")
tmp$stayathome <- 0
mylist <- split(tmp, as.factor(tmp$state))

for(i in 1:length(mylist)){
  mylist[[i]]$stayathome <- ifelse(mylist[[i]]$date < mylist[[i]]$Start.Date, 0, 1)
  mylist[[i]]$stayathome <- ifelse(mylist[[i]]$date > mylist[[i]]$End.Date, 0, mylist[[i]]$stayathome)
}

state_stayathome_daily <- do.call(rbind, mylist)
state_stayathome_daily$stayathome <- ifelse(is.na(state_stayathome_daily$stayathome), 0, state_stayathome_daily$stayathome)


write.csv(state_stayathome_daily, "~/Desktop/covid19/Data/state_daily_Shome_policy.csv")

state_stayathome <- read.csv("~/Desktop/covid19/Data/state_daily_Shome_policy.csv")[,-1]

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
write.csv(state_stayathome, "~/Desktop/covid19/Data/state_weekly_Shome_policy.csv")


### Mask policy
state_stayathome$Mask = 0
state_stayathome[which(state_stayathome$date>="2020-04-03"),]$Mask=1 # MASK policy was announced after April 3rd

## Policy data
#policies = read.csv("https://raw.githubusercontent.com/chloehe1129/COVID-19/master/Clean_cases/clean_policies.csv")
#policies = policies[,-4] # get rid of FIPS
#policies_state = policies[!duplicated(policies[,c("State","date","Mask")]),]
#Mask_state = policies_state[, c("State", "date", "Mask")]
#names(Mask_state)[1] <- "state"

Mask_state = state_stayathome[,c("state","date","Mask")]
tmp<- Mask_state %>% group_by(week = week(date), state)
tmp$state <- as.factor(tmp$state)
tmp$date <- as.Date(tmp$date)
tmp2 <- aggregate(.~week+ state, data=tmp, mean, na.rm=TRUE)
tmp2$Mask <- ifelse(tmp2$Mask>=0.5, 1, 0)
tmp2$date <- ymd("2020-01-01") + weeks(tmp2$week - 1 )

policy_daily <- merge(state_stayathome_daily, Mask_state, by=c("state", "date"), all.x=T)
policy_daily <- policy_daily[, c("state", "date", "stayathome", "Start.Date", "End.Date", "Mask")]
policy_daily$Mask <- ifelse(is.na(policy_daily$Mask), 1, policy_daily$Mask)
write.csv(policy_daily, "~/Desktop/covid19/Data/state_daily_policy.csv")

policy <- merge(state_stayathome, tmp2[,c("week","state","date")], by=c("state", "date"), all.x=T)
policy <- policy[, c("state", "date", "stayathome", "Start.Date", "End.Date", "Mask")]
policy$Mask <- ifelse(is.na(policy$Mask), 1, policy$Mask)
write.csv(policy, "~/Desktop/covid19/Data/state_weekly_policy.csv")
