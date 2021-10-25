## Movement data
#author: "Shan He"
#date: "5/11/2020"

library(zoo)

## Movement data
##https://www.google.com/covid19/mobility/ # update manually
movement = read.csv("~/Dropbox (Personal)/COVID/covid19/Data/Global_Mobility_Report.csv")
movement = movement[which(movement$country_region=="United States"),] # only US data
movement = movement[which(movement$sub_region_2==""),]                # only state data
movement = movement[-which(movement$sub_region_1==""),]      
movement = movement[,-c(1,2,4,5,6)]

names(movement) <- c("state","date", "retail_rec", "grocery_pharmacy", "parks", "transit", "work", "residential")
movement$date <- as.Date(movement$date)
write.csv(movement, "~/Dropbox (Personal)/COVID/covid19/Data/us_state_daily_movement.csv")

## Weekly movement: start from Wednesday
library(dplyr)
library(tidyr)
library(lubridate)

tmp <- movement %>% group_by(week = week(date), state) 

movement_week <- aggregate(.~week+ state , data=tmp, mean, na.rm=TRUE)
movement_week <- movement_week[, -3]
movement_week$date <- ymd("2020-01-01" ) + weeks(movement_week$week - 1 )
movement_week <- movement_week[, c("week", "date", "state", "residential", "work", "retail_rec", "grocery_pharmacy", "parks", "transit")]
movement_week$date <- as.Date(movement_week$date)

write.csv(movement_week, "~/Dropbox (Personal)/covid19/Data/us_state_weekly_movement.csv")


## County Level Movement data
movement = read.csv("~/Dropbox (Personal)/COVID/covid19/Data/Global_Mobility_Report.csv")
movement = movement[which(movement$country_region=="United States"),] # only US data
movement = movement[which(movement$sub_region_2!=""),]                # only state data
movement_county = movement[,-c(1,2,4,5)]

names(movement_county) <- c("state","FIPS","date", "retail_rec", "grocery_pharmacy", "parks", "transit", "work", "residential")
movement_county$date <- as.Date(movement_county$date)
write.csv(movement_county, "~/Dropbox (Personal)/COVID/covid19/Data/us_county_daily_movement.csv")

## Weekly movement: start from Wednesday
movement_county$FIPS = as.factor(movement_county$FIPS)
tmp <- movement_county %>% group_by(week = week(date), FIPS) 
tmp$date = as.Date(tmp$date)
movement_week_county <- aggregate(.~week+ state+FIPS , data=tmp, mean, na.rm=TRUE)
movement_week_county$date <- ymd("2020-01-01" ) + weeks(movement_week_county$week - 1 )
movement_week_county <- movement_week_county[, c("week", "state", "FIPS","date","residential", "work", "retail_rec", "grocery_pharmacy", "parks", "transit")]
movement_week_county$date <- as.Date(movement_week_county$date)

write.csv(movement_week_county, "~/Dropbox (Personal)/COVID/covid19/Data/us_county_weekly_movement.csv")


