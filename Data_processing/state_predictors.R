## Combine demographic data + temperature + number of bed + health data
#author: "Jooyoung Lee"
#date: "5/11/2020"

library(tidyverse)
library(RCurl)
library(Hmisc)

# Demographic data source: https://www.census.gov/acs/www/data/data-tables-and-tools/data-profiles/: 2014—2018 ACS 5-Year Data Profile

## Density and poverty -> categorical variable
# density

demographic <- read.csv("~/Desktop/covid19/Data/State_demographic/us_state.csv")
demographic$density <- demographic$population/demographic$area
demographic$q_popdensity = 1
quantile_popdensity = quantile(demographic$density,c(0.2,0.4,0.6,0.8))
demographic$q_popdensity[demographic$density<=quantile_popdensity[1]] = 1
demographic$q_popdensity[demographic$density>quantile_popdensity[1] &
                           demographic$density<=quantile_popdensity[2]] = 2
demographic$q_popdensity[demographic$density>quantile_popdensity[2] &
                           demographic$density<=quantile_popdensity[3]] = 3
demographic$q_popdensity[demographic$density>quantile_popdensity[3] &
                           demographic$density<=quantile_popdensity[4]] = 4
demographic$q_popdensity[demographic$density>quantile_popdensity[4]] = 5


# Make porverty Tertiles
demographic$q_poverty = 1
quantile_poverty = quantile(demographic$poverty_perc,probs = seq(0, 1, 1/3))[-1]
demographic$q_poverty[demographic$poverty_perc<=quantile_poverty[1]] = 1
demographic$q_poverty[demographic$poverty_perc>quantile_poverty[1] &
                        demographic$poverty_perc<=quantile_poverty[2]] = 2
demographic$q_poverty[demographic$poverty_perc>quantile_poverty[2]] = 3

## Combine
stateid <- read.csv("~/Desktop/covid19/Data/stateinfo.csv")
temp <- read.csv("~/Desktop/covid19/Data/State_demographic/us_state_temp.csv")
temp <- merge(stateid, temp, by="state")
temp <- temp[, !names(temp) %in% c("X", "stateFIPS")]

mydata <- merge(demographic, temp, by="state")
mydata <- mydata[order(mydata$state), ]

## Hospital bed

hospitals = read.csv(text=getURL("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
hospitals = hospitals[,c("COUNTY", "STATE","COUNTRY","BEDS")]
hospitals = subset(hospitals,hospitals$COUNTRY %in% c("USA", "PRI"))

hospitals$BEDS[hospitals$BEDS < 0] = NA
hospitals_state = aggregate(hospitals$BEDS,by=list(hospitals$STATE),FUN=sum,na.rm=TRUE)
colnames(hospitals_state) = c("State_abbrev","beds")
hospitals_state = merge(hospitals_state, stateid,by="State_abbrev")
hospitals_state <- hospitals_state[, c("state", "beds", "State_abbrev")]

# Health data
#health<-read.csv(text=getURL("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv"),skip = 1)
health<-read.csv("~/Desktop/covid19/Data/State_demographic/analytic_data2020.csv")
health<-health[,c('fipscode','state','v011_rawvalue','v009_rawvalue','v070_rawvalue', 'v060_rawvalue')]
colnames(health) = c("FIPS","state","smoke_perc","obese_perc","inactive_perc", "diabetes_perc")
health = health[-1,]
health_state = aggregate(list(health$smoke_perc,health$obese_perc,health$inactive_perc, health$diabetes_perc),by=list(health$state),mean)
colnames(health_state) = c("State_abbrev","smoke_perc","obese_perc","inactive_perc", "diabetes_perc")
health_state = merge(health_state, stateid, by="State_abbrev")

mydata <- merge(mydata, hospitals_state, by="state")
mydata <- merge(mydata, health_state[,c("smoke_perc","obese_perc","inactive_perc", "diabetes_perc", "state")],by="state")
mydata$bed_rate = mydata$beds/(mydata$population/100000)

mydata <- mydata[, !names(mydata) %in% "X"]
# Write data
write.csv(mydata, "~/Desktop/covid19/Data/us_state_predictors.csv")


################################################   COUNTY LEVEL PREDICTORS   ################################################

demographic <- read.csv("https://raw.githubusercontent.com/chloehe1129/COVID-19/master/Clean_demo/demographic_clean.csv")
demographic$q_popdensity = 1
quantile_popdensity = quantile(demographic$density,c(0.2,0.4,0.6,0.8))
demographic$q_popdensity[demographic$density<=quantile_popdensity[1]] = 1
demographic$q_popdensity[demographic$density>quantile_popdensity[1] &
                           demographic$density<=quantile_popdensity[2]] = 2
demographic$q_popdensity[demographic$density>quantile_popdensity[2] &
                           demographic$density<=quantile_popdensity[3]] = 3
demographic$q_popdensity[demographic$density>quantile_popdensity[3] &
                           demographic$density<=quantile_popdensity[4]] = 4
demographic$q_popdensity[demographic$density>quantile_popdensity[4]] = 5

# Make porverty Tertiles
demographic$q_poverty = 1
quantile_poverty = quantile(demographic$poverty_perc,probs = seq(0, 1, 1/3))[-1]
demographic$q_poverty[demographic$poverty_perc<=quantile_poverty[1]] = 1
demographic$q_poverty[demographic$poverty_perc>quantile_poverty[1] &
                        demographic$poverty_perc<=quantile_poverty[2]] = 2
demographic$q_poverty[demographic$poverty_perc>quantile_poverty[2]] = 3

countyid <- read.csv("https://raw.githubusercontent.com/chloehe1129/COVID-19/master/match_file.csv")[,-1]
mydata = demographic
mydata <- mydata[order(mydata$FIPS), ]

# Health data
#health<-read.csv(text=getURL("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv"),skip = 1)
health<-read.csv("~/Desktop/covid19/Data/State_demographic/analytic_data2020.csv")
health<-health[,c('fipscode','state','v011_rawvalue','v009_rawvalue','v070_rawvalue', 'v060_rawvalue')]
colnames(health) = c("FIPS","state","smoke_perc","obese_perc","inactive_perc", "diabetes_perc")
health_county = aggregate(list(health$smoke_perc,health$obese_perc,health$inactive_perc, health$diabetes_perc),by=list(health$state,health$FIPS),mean)
colnames(health_county) = c("Code","FIPS","smoke_perc","obese_perc","inactive_perc", "diabetes_perc")
health_county = merge(health_county, countyid[,c("Code","FIPS")], by=c("Code","FIPS"))

mydata <- merge(mydata, health_county[,c("smoke_perc","obese_perc","inactive_perc", "diabetes_perc", "FIPS")],by="FIPS")

mydata <- mydata[, !names(mydata) %in% "X"]
mydata$temp = (mydata$summer_tmmx+mydata$winter_tmmx)/2
names(mydata) = c("FIPS","summer_tmmx","summer_rmax","winter_tmmx","winter_rmax","state","Countynames","State_abbrev",
                  "young_perc","median_perc","old_perc","lowerthan_HS", "with_HS","with_BS","Bachelor_orhigher_perc",
                  "poverty_perc","Median_HH_income","population","Black.perc" ,"Hispanic.or.Latino.perc","area","density",
                  "q_popdensity","q_poverty","smoke_perc","obese_perc","inactive_perc","diabetes_perc","temp")
                  
# Write data
write.csv(mydata, "~/Desktop/covid19/Data/us_county_predictors.csv")
