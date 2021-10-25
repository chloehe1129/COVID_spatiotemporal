## Download daily confirmed COVID-19 cases by state
#author: "Chloe He & Jooyoung Lee"
#date: "5/11/2020"

library(qdapRegex)
library(data.table)
library(lubridate)
library(zoo)

df = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") # extract the most updated cases information

df = df[,-c(1:4,8)]
df = df[,-c(4:6)]
## exclude recent date 
#df = df[, -which(names(df) %in% c("X5.3.20", "X5.4.20", "X5.5.20", "X5.6.20", "X5.7.20", "X5.8.20", "X5.9.20"))]
df =melt(setDT(df), id.vars = c("FIPS","Admin2","Province_State"), variable.name = "Date")
colnames(df) = c("FIPS","Countynames","state","Date","cases")
df$date = as.character(df$Date)
df$year = 0
df$month = 0
df$day = 0
df$year = 2020

df$month = rm_between(df$Date, "X", ".", extract=TRUE)
df$day = rm_between(df$Date, ".", ".", extract=TRUE)
tmp = paste0(df$year,"-",df$month,"-",df$day)
df$date = as.Date(tmp,format = "%Y-%m-%d")
df = df[,c(3,5,6)]
colnames(df) = c("state","case","date")

### State-level
# Aggregate to get state level cases count
state_case <- aggregate(.~date + state, data=df, sum, na.rm=TRUE)
# calculate the daily new cases
mylist <- split(state_case, state_case$state)

for(i in 1:length(mylist)){
  mylist[[i]]$daily_case <- c(mylist[[i]]$case[1], diff(mylist[[i]]$case))
  mylist[[i]]$daily_case <- ifelse(mylist[[i]]$daily_case<0, 0, mylist[[i]]$daily_case)
}

state_case <- do.call(rbind, mylist)
state_case <- state_case[state_case$case>0, ]
## extract cum value greater than 11
#state_case <- state_case[state_case$case>11,]
write.csv(state_case, "~/Desktop/covid19/Data/us_state_case_JHU.csv")

## Weekly case: start from Wednesday

state_case$date <- as.Date(state_case$date)
tmp <- state_case[, c("date", "state", "daily_case")] %>% group_by(week = week(date), state) 
state_weekly_case <- aggregate(.~week+ state , data=tmp, sum, na.rm=TRUE)
state_weekly_case  <- state_weekly_case[, -3]
state_weekly_case$date <- ymd("2020-01-01" ) + weeks(state_weekly_case$week - 1)
names(state_weekly_case) <- c("week", "state", "weekly_case", "date")
state_weekly_case <- state_weekly_case[, c("week", "date", "state", "weekly_case")]

## extract cum value greater than 11????

mylist <- split(state_weekly_case, state_weekly_case$state)
for(i in 1:length(mylist)){
  mylist[[i]]$cum_case <- cumsum(mylist[[i]]$weekly_case)
}

state_weekly_case <- do.call(rbind, mylist)
state_weekly_case <- state_weekly_case[state_weekly_case$cum_case>0, ]
write.csv(state_weekly_case, "~/Desktop/covid19/Data/us_state_weekly_case_JHU.csv")


### state-level death
df = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") # extract the most updated cases information

df = df[,-c(1:4,8)]
df = df[,-c(4:7)]
## exclude recent date 
#df = df[, -which(names(df) %in% c("X5.8.20", "X5.9.20", "X5.10.20", "X5.11.20", "X5.12.20", "X5.13.20"))]
df =melt(setDT(df), id.vars = c("FIPS","Admin2","Province_State"), variable.name = "Date")
colnames(df) = c("FIPS","Countynames","state","Date","cases")
df$date = as.character(df$Date)
df$year = 0
df$month = 0
df$day = 0
df$year = 2020

df$month = rm_between(df$Date, "X", ".", extract=TRUE)
df$day = rm_between(df$Date, ".", ".", extract=TRUE)
tmp = paste0(df$year,"-",df$month,"-",df$day)
df$date = as.Date(tmp,format = "%Y-%m-%d")

df = df[,c(3,5,6)]
colnames(df) = c("state","case","date")

### State-level
## Daily death
# Aggregate to get state level cases count
state_death <- aggregate(.~date + state, data=df, sum, na.rm=TRUE)
# calculate the daily new cases
mylist <- split(state_death, state_death$state)

for(i in 1:length(mylist)){
  mylist[[i]]$death <- c(mylist[[i]]$case[1], diff(mylist[[i]]$case))
}

state_death <- do.call(rbind, mylist)
names(state_death) <- c("date", "state", "cum_death", "daily_death")
## extract cum value greater than 11
state_daily_death <- state_death[state_death$cum_death>0,]
state_daily_death$daily_death = ifelse(state_daily_death$daily_death<0,0,state_daily_death$daily_death)
write.csv(state_daily_death, "~/Desktop/covid19/Data/us_state_daily_death_JHU.csv")

## Weekly death: start from Wednesday

state_death$date <- as.Date(state_death$date)
tmp <- state_death[, c("date", "state", "daily_death")] %>% group_by(week = week(date), state) 
state_weekly_death <- aggregate(.~week+ state , data=tmp, sum, na.rm=TRUE)
state_weekly_death <- state_weekly_death[, -3]
state_weekly_death$date <- ymd("2020-01-01" ) + weeks(state_weekly_death$week - 1 )
names(state_weekly_death) <- c("week", "state", "weekly_death", "date")
state_weekly_death <- state_weekly_death[, c("week", "date", "state", "weekly_death")]

## extract cum value greater than 11

mylist <- split(state_weekly_death, state_weekly_death$state)
for(i in 1:length(mylist)){
  mylist[[i]]$cum_death <- cumsum(mylist[[i]]$weekly_death)
}

state_weekly_death <- do.call(rbind, mylist)
state_weekly_death <- state_weekly_death[state_weekly_death$cum_death>0, ]
write.csv(state_weekly_death, "~/Desktop/covid19/Data/us_state_weekly_death_JHU.csv")


#################################################  COUNTY LEVEL INCIDENCE ####################################################################

df = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") # extract the most updated cases information

df = df[,-c(1:4,8)]
df = df[,-c(4:6)]
## exclude recent date 
#df = df[, -which(names(df) %in% c("X5.3.20", "X5.4.20", "X5.5.20", "X5.6.20", "X5.7.20", "X5.8.20", "X5.9.20"))]
df =melt(setDT(df), id.vars = c("FIPS","Admin2","Province_State"), variable.name = "Date")
colnames(df) = c("FIPS","Countynames","state","Date","cases")
df$date = as.character(df$Date)
df$year = 0
df$month = 0
df$day = 0
df$year = 2020

df$month = rm_between(df$Date, "X", ".", extract=TRUE)
df$day = rm_between(df$Date, ".", ".", extract=TRUE)
tmp = paste0(df$year,"-",df$month,"-",df$day)
df$date = as.Date(tmp,format = "%Y-%m-%d")
df = df[,c(1,3,5,6)]
colnames(df) = c("FIPS","state","case","date")

### County-level
# Aggregate to get county level cases count
county_case <- aggregate(.~date + FIPS+state, data=df, sum, na.rm=TRUE)
# calculate the daily new cases
mylist <- split(county_case, county_case$FIPS)

for(i in 1:length(mylist)){
  mylist[[i]]$daily_case <- c(mylist[[i]]$case[1], diff(mylist[[i]]$case))
  mylist[[i]]$daily_case <- ifelse(mylist[[i]]$daily_case<0, 0, mylist[[i]]$daily_case)
}

county_case <- do.call(rbind, mylist)
county_case <- county_case[county_case$case>0, ]
## extract cum value greater than 11
#county_case <- county_case[county_case$case>11,]
write.csv(county_case, "~/Desktop/covid19/Data/us_county_case_JHU.csv")

## Weekly case: start from Wednesday

county_case$date <- as.Date(county_case$date)
tmp <- county_case[, c("date", "FIPS","state", "daily_case")] %>% group_by(week = week(date), FIPS) 
county_weekly_case <- aggregate(.~week+ state+FIPS , data=tmp, sum, na.rm=TRUE)
county_weekly_case$date <- ymd("2020-01-01" ) + weeks(county_weekly_case$week - 1)
names(county_weekly_case) <- c("week", "state", "FIPS", "date","weekly_case")
county_weekly_case <- county_weekly_case[, c("week", "date", "state", "FIPS","weekly_case")]

mylist <- split(county_weekly_case, county_weekly_case$FIPS)
for(i in 1:length(mylist)){
  mylist[[i]]$cum_case <- cumsum(mylist[[i]]$weekly_case)
}

county_weekly_case <- do.call(rbind, mylist)
county_weekly_case <- county_weekly_case[county_weekly_case$cum_case>0, ]
write.csv(county_weekly_case, "~/Desktop/covid19/Data/us_county_weekly_case_JHU.csv")

