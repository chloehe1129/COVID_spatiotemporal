## Movement data
#author: "Jooyoung Lee"
#date: "5/11/2020"

## Movement data
##https://www.google.com/covid19/mobility/ # update manually
movement = read.csv("/Users/jooyounglee/Dropbox/2020_covid19/covid19/Data/Global_Mobility_Report.csv")
movement = movement[which(movement$country_region=="United States"),] # only US data
movement = movement[which(movement$sub_region_2==""),]                # only state data
movement = movement[-which(movement$sub_region_1==""),]      
movement = movement[,-c(1,2,4)]

names(movement) <- c("state", "date", "retail_rec", "grocery_pharmacy", "parks", "transit", "work", "residential")
movement$date <- as.Date(movement$date)

## Weekly movement: start from Wednesday

tmp <- movement %>% group_by(week = week(date), state) 

movement_week <- aggregate(.~week+ state , data=tmp, mean, na.rm=TRUE)
movement_week <- movement_week[, -3]
movement_week$date <- ymd("2020-01-01" ) + weeks(movement_week$week - 1 )
movement_week <- movement_week[, c("week", "date", "state", "residential", "work", "retail_rec", "grocery_pharmacy", "parks", "transit")]
movement_week$date <- as.Date(movement_week$date)

write.csv(movement_week, "/Users/jooyounglee/Dropbox/2020_covid19/covid19/Data/us_state_weekly_movement.csv")

