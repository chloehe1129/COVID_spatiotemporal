## County Level Movement data
movement = read.csv("~/Dropbox (Personal)/COVID/covid19/Data/Global_Mobility_Report.csv")
movement = movement[which(movement$country_region=="United States"),] # only US data
movement = movement[which(movement$sub_region_2!=""),]                # only state data
movement_county = movement[,-c(1,2,4,5)]

names(movement_county) <- c("state","FIPS","date", "retail_rec", "grocery_pharmacy", "parks", "transit", "work", "residential")
movement_county$date <- as.Date(movement_county$date)
write.csv(movement_county, "~/Dropbox (Personal)/COVID/covid19/Data/us_county_daily_movement.csv")

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)


##################### SA : RAW MAP ###############################################################
library(dplyr)
movement_county = movement_county %>% filter(rowSums(is.na(movement_county[,c(4:9)])) != ncol(movement_county[,c(4:9)]))
mydata3 = merge(mydata,movement_county,by=c("date","FIPS"))
mydata3$weeks_since_first = 0
mydata3$date = as.Date(mydata3$date)

for (i in 1:length(unique(mydata3$FIPS))){
  if(nrow(mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),])<2){mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"weeks_since_first"]=0}else{
    for (k in 2:nrow(mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),])){
      mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"weeks_since_first"][k]=
        as.numeric((mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"date"][k]-mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"date"][1])/7)
    }}
}

library(tigris); library(reshape); library(dplyr); library(INLA); library(spdep); library(splines)
us_co <- counties(cb=TRUE, class="sp") # Download a US Counties shape-file into R
us_co$struct<-1:dim(us_co@data)[1]

us_co1<-st_as_sf(us_co) # Convert foreign object to an sf object
us_co1$FIPS <- as.numeric(paste(us_co1$STATEFP, us_co1$COUNTYFP, sep=""))
final.dat<-merge(us_co1,mydata3, by="FIPS")
final.dat <- final.dat[order(final.dat$struct),]

tmp <- data.frame(FIPS=unique(final.dat$FIPS), IDcounty1=1:length(unique(final.dat$struct)))
final.dat <- left_join(final.dat, tmp, by="FIPS")
final.dat$IDcounty2 <- final.dat$IDcounty1
final.dat$IDcounty3 <- final.dat$IDcounty1

data = final.dat
data$case_pop = data$cum_case/data$population*100000

pdf("~/Dropbox (Personal)/covid19_paper/tables_figures/SA_raw_map.pdf",width=20,height=30)
data%>%
  filter(week%in%c(28))%>%
  mutate(qrr=cut(case_pop, breaks = quantile(case_pop, p=seq(0,1,length.out = 6)), include.lowest = T))%>%
  ggplot()+geom_sf(aes(fill=qrr))+scale_colour_brewer(palette = "RdBu" )+scale_fill_brewer(palette = "RdBu", na.value="grey")+guides(fill=guide_legend(title="COVID-19 cases per 100K Quartile"))+
  ggtitle(label="County-level Cumulative COVID-19 cases per 100K")
dev.off()
#########################################################################################################

movement_component = c("residential","work","retail_rec","grocery_pharmacy","transit","parks")

for (m in (1:length(movement_component))){
## Weekly movement: start from Wednesday
movement_county$FIPS = as.factor(movement_county$FIPS)
tmp <- movement_county %>% group_by(week = week(date), FIPS) 
tmp$date = as.Date(tmp$date)
movement_week <- aggregate(.~week+ state+FIPS , data=tmp[,c("state","FIPS","week",movement_component[m])], mean, na.rm=TRUE) ####### Only average one component at a time
movement_week$date <- ymd("2020-01-01" ) + weeks(movement_week$week - 1 )
movement_week <- movement_week[, c("week", "state", "FIPS","date",movement_component[m])]
movement_week$date <- as.Date(movement_week$date)

movement = movement_week
mydata <-  read.csv("~/Dropbox (Personal)/COVID/covid19/Data/us_county_weekly_case_JHU.csv")[,-1] 
mydata$date <- as.Date(mydata$date)
county_predic <-  read.csv("~/Dropbox (Personal)/COVID/covid19/Data/us_county_predictors.csv")[,-1] 

mydata <- merge(mydata, county_predic, by=c("FIPS"))
mydata$date <- as.Date(mydata$date)
mydata <- mydata[order(mydata$FIPS, as.Date(mydata$date)),]

movement$date = as.Date(movement$date)
lagk = 8
for (kk in (1:lagk)){
  create = paste0(movement_component[m],"_weeks_ago",kk)
  for(i in (1:length(unique(movement$FIPS)))){
    dates_before = movement[which(movement$FIPS == unique(movement$FIPS)[i]),]$date-7*kk
    ind = match(dates_before,movement[which(movement$FIPS == unique(movement$FIPS)[i]),]$date)
    movement[which(movement$FIPS==unique(movement$FIPS)[i]),create]=movement[which(movement$FIPS==unique(movement$FIPS)[i]),][ind,movement_component[m]]
  }
}

mydata2 = merge(mydata,movement[,-c(2,4)],by=c("FIPS","week")) 

mydata2 <- mydata2[mydata2$cum_case>=50, ] 
mydata2 <- mydata2[mydata2$state.x!="Maine",]

mydata3 <- na.omit(mydata2)
mydata3$weeks_since_first = 0
mydata3$date = as.Date(mydata3$date)

for (i in 1:length(unique(mydata3$FIPS))){
  if(nrow(mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),])<2){mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"weeks_since_first"]=0}else{
    for (k in 2:nrow(mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),])){
      mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"weeks_since_first"][k]=
        as.numeric((mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"date"][k]-mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"date"][1])/7)
    }}
}
write.csv(mydata3,paste0("~/Dropbox (Personal)/covid19_paper/weeklydata_",movement_component[m],".csv"))
}

for (m in (1:length(movement_component))){

# INLA Set up
mydata3 = read.csv(paste0("~/Dropbox (Personal)/covid19_paper/weeklydata_",movement_component[m],".csv"))

library(tigris); library(reshape); library(dplyr); library(INLA); library(spdep); library(splines)
us_co <- counties(cb=TRUE, class="sp") # Download a US Counties shape-file into R
us_co$struct<-1:dim(us_co@data)[1]

us_co1<-st_as_sf(us_co) # Convert foreign object to an sf object
us_co1$FIPS <- as.numeric(paste(us_co1$STATEFP, us_co1$COUNTYFP, sep=""))
final.dat<-merge(us_co1,mydata3, by="FIPS")
final.dat <- final.dat[order(final.dat$struct),]

tmp <- data.frame(FIPS=unique(final.dat$FIPS), IDcounty1=1:length(unique(final.dat$struct)))
final.dat <- left_join(final.dat, tmp, by="FIPS")
final.dat$IDcounty2 <- final.dat$IDcounty1
final.dat$IDcounty3 <- final.dat$IDcounty1

# different adjacency matrix for each movement component!

us_co2 <- us_co[us_co$GEOID %in% unique(final.dat$GEOID),]
us.nb <-poly2nb(us_co2, row.names=us_co2$struct) # builds a neighbours list based on regions with contiguous boundaries
us.nb <- knearneigh(coordinates(us_co2), k = 5, longlat = T) # k=5 nearest neighbors
us.nb <- knn2nb(us.nb, row.names = us_co2$struct, sym = T) #force symmetry
mat <- nb2mat(us.nb, style="B",zero.policy=TRUE)
colnames(mat) <- rownames(mat)
mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
nb2INLA(paste0("cl_graph_",movement_component[m]),us.nb) # Output spatial neighbours for INLA
am_adj <-paste0("~/Dropbox (Personal)/covid19_paper/Code/cl_graph_",movement_component[m])

H<-inla.read.graph(filename=paste0("cl_graph_",movement_component[m])) 

prec.prior <- list(theta = list(prior="loggamma", param = c(1, 0.001)))


# Lag-specific weekly model
nk=3
library(Hmisc)
time_RCS = scale(rcspline.eval(final.dat$weeks_since_first, nk=nk, inclx=TRUE, knots.only=FALSE, type="ordinary", norm=2, rpm=NULL))
colnames(time_RCS) <- paste("time_RCS", seq(1:(nk-1)), sep="")
final.dat = cbind(final.dat, time_RCS)

model <- inla(weekly_case ~ f(IDcounty1, model = "bym", graph = H, hyper = prec.prior)
              + f(IDcounty2, time_RCS1, model="iid", hyper = prec.prior) + f(IDcounty3, time_RCS2, model="iid", hyper = prec.prior)
              + time_RCS1 + time_RCS2 + factor(q_popdensity)
              + scale(young_perc) + scale(median_perc) + scale(old_perc)
              + scale(poverty_perc) + scale(Black.perc) + scale(Hispanic.or.Latino.perc)
              + scale(temp)+ scale(Bachelor_orhigher_perc) + scale(obese_perc) + get(movement_component[m]),
              data = final.dat,
              offset=log(population),
              family = "nbinomial",
              control.predictor = list(compute = TRUE, link = 1))
assign(paste0("SA_",movement_component[m],"_0"),model)

lag = c(1,2,3,4,5,6,7,8)
for (l in (1:length(lag))){
  model <- inla(weekly_case ~ f(IDcounty1, model = "bym", graph = H, hyper = prec.prior)
              + f(IDcounty2, time_RCS1, model="iid", hyper = prec.prior) + f(IDcounty3, time_RCS2, model="iid", hyper = prec.prior)
              + time_RCS1 + time_RCS2 + factor(q_popdensity) 
              + scale(young_perc) + scale(median_perc) + scale(old_perc) 
              + scale(poverty_perc) + scale(Black.perc) + scale(Hispanic.or.Latino.perc)
              + scale(temp)+ scale(Bachelor_orhigher_perc) + scale(obese_perc) + get(paste0("grocery_weeks_ago",lag[l])),
              data = final.dat, 
              offset=log(population), 
              family = "nbinomial",
              control.predictor = list(compute = TRUE, link = 1))
  assign(paste0("SA_",movement_component[m],"_",lag[l]),model)
}


# DLM weekly
library(Hmisc)
library(sf)
lagd <- 8
lages <- c(movement_component[m], paste("grocery_weeks_ago", seq(1:lagd), sep=""))
#Movement <- as.matrix(final.dat[, c(lages)])
Movement <- final.dat[, c(lages)]
Movement <- `st_geometry<-`(Movement, NULL)
lagpoint <- matrix(rep(seq(0, lagd), nrow(final.dat)), nrow=nrow(final.dat), byrow=T)

kk <- 4
B1 <- cbind(1, rcspline.eval(seq(0, lagd), knots=seq(0, lagd, length=(kk+2))[-c(1, kk+2)], inclx = T))
knots <- rcspline.eval(seq(0, lagd), knots=seq(0, lagd, length=(kk+2))[-c(1, kk+2)], inclx=T, knots.only=T)
Movement = as.matrix(Movement)
#RCS = scale(Movement%*%B1) #### the new variables = basis * old variable
RCS = Movement%*%B1
colnames(RCS) <- paste("RCS", seq(1:kk), sep="")
mydat = cbind(final.dat,RCS)

RCS_form<- paste(paste("RCS", seq(1:kk), sep=""), collapse="+")
updated.terms <- paste("~.",  RCS_form, sep="+")

x <- seq(0, lagd, length=100)
newB1 <- cbind(1, rcspline.eval(x, knots=knots, inclx = T))
lc <- inla.make.lincombs(RCS1 = newB1[,1] , RCS2 = newB1[,2], RCS3 = newB1[,3])

model1 <- inla(weekly_case ~ f(IDcounty1, model = "bym", graph = H, hyper = prec.prior)
               + f(IDcounty2, time_RCS1, model="iid", hyper = prec.prior) + f(IDcounty3, time_RCS2, model="iid", hyper = prec.prior)
               + time_RCS1 + time_RCS2 +
                 + factor(q_popdensity)
               + scale(young_perc) + scale(median_perc) + scale(old_perc)
               + scale(poverty_perc) + scale(Black.perc) + scale(Hispanic.or.Latino.perc)
               + scale(temp)+ scale(Bachelor_orhigher_perc) + scale(obese_perc) + RCS1 + RCS2 + RCS3 ,
               data = mydat, lincomb=lc,
               offset=log(population),
               family = "nbinomial",
               control.predictor = list(compute = TRUE, link = 1), control.compute = list(dic=T))

assign(paste0("SA_DLM_",movement_component[m]),model)
ef <- function(x) exp(10*x)

pdf(paste0("~/Dropbox (Personal)/covid19_paper/tables_figures/INLA_weekly/wo01_SA_DLM_4_",Sys.Date(),"_",movement_component[m],".pdf"),width=5,height=4)
est <- sapply(1:100, function(i) inla.emarginal(ef, model1$marginals.lincomb.derived[[i]]))
CI <- sapply(1:100, function(i) inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef, model1$marginals.lincomb.derived[[i]]))) #CI
uCI <- CI[2,]  # the upper credible interval
lCI <- CI[1,]  # the lower credible interval,
pltdata = as.data.frame(cbind(seq(from=0,to=8,length=100),est,uCI,lCI))
library(ggplot2)
g= ggplot(data = pltdata,aes(x=V1,y=est))+geom_line()+
  ylab("Incidence Rate Ratio") + xlab("Weeks in the past")+
  theme(legend.title = element_text(color = "black", size = 12),legend.position = "none",plot.title=element_text(size=18,face="bold"))
g+geom_ribbon(data=pltdata,aes(ymin=uCI,ymax=lCI),alpha=0.3,colour=NA)+ggtitle(paste0("grocery and pharmacy"))+geom_hline(yintercept = 1,lty="dashed")
dev.off()

}


#### FOREST PLOT
week = rep(c(0:8),each=6)
ef <- function(x) exp(10*x)
result0 = cbind(rbind(inla.emarginal(ef, get(paste0("SA_residential_0"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_residential_1"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_residential_2"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_residential_3"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_residential_4"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_residential_5"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_residential_6"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_residential_7"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_residential_8"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_work_0"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_work_1"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_work_2"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_work_3"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_work_4"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_work_5"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_work_6"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_work_7"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_work_8"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_retail_rec_0"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_retail_rec_1"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_retail_rec_2"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_retail_rec_3"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_retail_rec_4"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_retail_rec_5"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_retail_rec_6"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_retail_rec_7"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_retail_rec_8"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_grocery_pharmacy_0"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_grocery_pharmacy_1"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_grocery_pharmacy_2"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_grocery_pharmacy_3"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_grocery_pharmacy_4"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_grocery_pharmacy_5"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_grocery_pharmacy_6"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_grocery_pharmacy_7"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_grocery_pharmacy_8"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_transit_0"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_transit_1"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_transit_2"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_transit_3"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_transit_4"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_transit_5"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_transit_6"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_transit_7"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_transit_8"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_transit_0"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_parks_1"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_parks_2"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_parks_3"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_parks_4"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_parks_5"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_parks_6"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_parks_7"))$marginals.fixed[[17]]),
                     inla.emarginal(ef, get(paste0("SA_parks_8"))$marginals.fixed[[17]])),
               rbind(inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_residential_0"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_residential_1"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_residential_2"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_residential_3"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_residential_4"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_residential_5"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_residential_6"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_residential_7"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_residential_8"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_work_0"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_work_1"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_work_2"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_work_3"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_work_4"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_work_5"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_work_6"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_work_7"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_work_8"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_retail_rec_0"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_retail_rec_1"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_retail_rec_2"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_retail_rec_3"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_retail_rec_4"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_retail_rec_5"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_retail_rec_6"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_retail_rec_7"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_retail_rec_8"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_retail_rec_0"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_grocery_pharmacy_1"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_grocery_pharmacy_2"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_grocery_pharmacy_3"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_grocery_pharmacy_4"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_grocery_pharmacy_5"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_grocery_pharmacy_6"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_grocery_pharmacy_7"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_grocery_pharmacy_8"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_transit_0"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_transit_1"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_transit_2"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_transit_3"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_transit_4"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_transit_5"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_transit_6"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_transit_7"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_transit_8"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_transit_0"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_parks_1"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_parks_2"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_parks_3"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_parks_4"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_parks_5"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_parks_6"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_parks_7"))$marginals.fixed[[17]])),
                     inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef,get(paste0("SA_parks_8"))$marginals.fixed[[17]]))))

movement = rep(c("Residential","Work","Grocery and Pharmacy","Retail and Recreation","Transit","Parks"),each=9)

result = cbind(movement,
               week,
               result0)

colnames(result) = c("movement","weeklag","effect","lower","upper")
result = as.data.frame(result)
result$effect = as.numeric(as.character(result$effect))
result$lower = as.numeric(as.character(result$lower))
result$upper = as.numeric(as.character(result$upper))


######### Without Residential
x <- c("Residential", "Work", "Grocery and Pharmacy", "Retail and Recreation", "Transit", "Parks")
result = result[order(match(result$movement,x)),]
result$weeklaag = as.numeric(as.character(result$weeklag))
# result$week = ifelse(result$weeklag<4,0,1)
# result$week = as.factor(result$week)
result = result[-which(result$movement=="Residential"),]
result$color = ifelse(result$lower>1,1,0)
result$color = as.factor(result$color)

tabletext=cbind(c("Movement","Work"," "," "," "," "," "," "," "," ",
                  "Grocery and Pharmacy"," "," "," "," "," "," "," "," ",
                  "Retail and recreation"," "," "," "," "," "," "," "," ",
                  "Transit"," "," "," "," "," "," "," "," ",
                  "Parks"," "," "," "," "," "," "," "," "),
                c("Lag Week",
                  "0","1","2","3","4","5","6","7","8"
                  ,"0","1","2","3","4","5","6","7","8"
                  ,"0","1","2","3","4","5","6","7","8"
                  ,"0","1","2","3","4","5","6","7","8"
                  ,"0","1","2","3","4","5","6","7","8"),
                c("IRR [95% CI]",paste0(format(round(result$effect,2),nsmall=2)," ","[",format(round(result$lower,2),nsmall=2),",",format(round(result$upper,2),nsmall=2),"]")))

data = structure(list(
  mean=c(NA,result$effect),
  lower=c(NA,result$lower),
  upper=c(NA,result$upper),
  week= c(NA,result$color),
  .Names=c("mean","lower","upper","week"),row.names=c(NA,-11L),class="data.frame"))

fn = local({
  i=0
  no_lines=sum(!is.na(data$color))
  b_clrs=c("red","blue")[match(result$color,c("0","1"))]
  l_clrs=c("red","blue")[match(result$color,c("0","1"))]
  
  function(...,clr.line,clr.marker){
    i<<-i+1
    fpDrawNormalCI(...,clr.line=l_clrs[i],clr.marker=b_clrs[i])
  }
})

pdf(paste0("~/Dropbox (Personal)/covid19_paper/tables_figures/INLA_weekly/SA_no_residential_",Sys.Date(),".pdf"),width=20,height=20)
forestplot::forestplot(tabletext,fn.ci_norm=fn,xlim=c(0.5,1.3),
                       mean = data$mean,lower=data$lower,upper=data$upper,
                       hrzl_lines=list("2"=gpar(lwd=3,lty="solid",lineend="butt",col="black"),
                                       "11"=gpar(lwd=10,lineend="butt",col="#99999922"),
                                       "20"=gpar(lwd=10,lineend="butt",col="#99999922"),
                                       "29"=gpar(lwd=10,lineend="butt",col="#99999922"),
                                       "38"=gpar(lwd=10,lineend="butt",col="#99999922"),
                                       "47"=gpar(lwd=10,lineend="butt",col="#99999922")),
                       zero = 1,cex = 2,lineheight = "auto",
                       colour = data$week,
                       boxsize=0.1,
                       graphwidth = unit(230, "mm"),
                       txt_gp = fpTxtGp(ticks=gpar(fontfamily="",cex=2),xlab=gpar(fontfamily="",cex=3),
                                        label=gpar(fontfamily="",cex=2)),
                       lwd.ci=2,
                       grid=T,
                       is.summary=c(TRUE,rep(FALSE,45),TRUE),
                       xlab = "Incidence Rate Ratio",
                       col=fpColors(box="royalblue",line=c("red","blue")[match(result$week,c("0","1"))],summary="lightblue"))
dev.off()

######### Residential Only
x <- c("Residential", "Work", "Grocery and Pharmacy", "Retail and Recreation", "Transit", "Parks")
result = result[order(match(result$movement,x)),]
result$weeklag = as.numeric(as.character(result$weeklag))
# result$week = ifelse(result$weeklag<4,0,1)
# result$week = as.factor(result$week)
result = result[which(result$movement=="Residential"),]
result$color = ifelse(result$upper>=1,1,0)
result$color = as.factor(result$color)

tabletext=cbind(c("Movement","Residential"," "," "," "," "," "," "," "," "),
                c("Lag Week",
                  "0","1","2","3","4","5","6","7","8"),
                c("IRR [95% CI]",paste0(format(round(result$effect,2),nsmall=2)," ","[",format(round(result$lower,2),nsmall=2),",",format(round(result$upper,2),nsmall=2),"]")))

data = structure(list(
  mean=c(NA,result$effect),
  lower=c(NA,result$lower),
  upper=c(NA,result$upper),
  week= c(NA,result$color),
  .Names=c("mean","lower","upper","week"),row.names=c(NA,-11L),class="data.frame"))

fn = local({
  i=0
  no_lines=sum(!is.na(data$color))
  b_clrs=c("red","blue")[match(result$color,c("1","0"))]
  l_clrs=c("red","blue")[match(result$color,c("1","0"))]
  
  function(...,clr.line,clr.marker){
    i<<-i+1
    fpDrawNormalCI(...,clr.line=l_clrs[i],clr.marker=b_clrs[i])
  }
})

pdf(paste0("~/Dropbox (Personal)/covid19_paper/tables_figures/INLA_weekly/SA_residential_",Sys.Date(),".pdf"),width=15,height=3)
forestplot::forestplot(tabletext,fn.ci_norm=fn,xlim=c(0,8),
                       mean = data$mean,lower=data$lower,upper=data$upper,
                       zero = 1,cex = 2,lineheight = "auto",
                       colour = data$week,
                       boxsize=0.2,
                       graphwidth = unit(230, "mm"),
                       txt_gp = fpTxtGp(ticks=gpar(fontfamily="",cex=1),xlab=gpar(fontfamily="",cex=1),
                                        label=gpar(fontfamily="",cex=1)),
                       lwd.ci=2,
                       grid=T,
                       is.summary=c(TRUE,rep(FALSE,42),TRUE),
                       col=fpColors(box="royalblue",line=c("red","blue")[match(result$week,c("0","1"))],summary="lightblue"))
dev.off()


#### Independent variable coefficients - work 6 weeks
ef <- function(x) exp(x)

syoung_perc <- scale(mydat$young_perc)
smedian_perc <- scale(mydat$median_perc)
sold_perc <- scale(mydat$old_perc)
spoverty_perc <- scale(mydat$poverty_perc)
sBlack_perc <- scale(mydat$Black.perc)
sHispanic <- scale(mydat$Hispanic.or.Latino.perc)
stemp <- scale(mydat$temp)
sBachelor <- scale(mydat$Bachelor_orhigher_perc) 
sobese <-  scale(mydat$obese_perc)

write.csv(as.data.frame(cbind(
  rbind(inla.emarginal(exp, SA_work_6$marginals.fixed[[1]]),
        inla.emarginal(function(x) exp(x/attributes(time_RCS)[[5]][1]), SA_work_6$marginals.fixed[[2]]),
        inla.emarginal(function(x) exp(x/attributes(time_RCS)[[5]][2]), SA_work_6$marginals.fixed[[3]]),
          inla.emarginal(exp, SA_work_6$marginals.fixed[[4]]),
        inla.emarginal(exp, SA_work_6$marginals.fixed[[5]]),
        inla.emarginal(exp, SA_work_6$marginals.fixed[[6]]),
        inla.emarginal(exp, SA_work_6$marginals.fixed[[7]]),
        inla.emarginal(function(x) exp(x/attributes(syoung_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[8]]),
        inla.emarginal(function(x) exp(x/attributes(smedian_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[9]]),
        inla.emarginal(function(x) exp(x/attributes(sold_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[10]]),
        inla.emarginal(function(x) exp(x/attributes(spoverty_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[11]]),
        inla.emarginal(function(x) exp(x/attributes(sBlack_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[12]]),
        inla.emarginal(function(x) exp(x/attributes(sHispanic)[[3]]), 0.01*SA_work_6$marginals.fixed[[13]]),
        inla.emarginal(function(x) exp(x/attributes(stemp)[[3]]), SA_work_6$marginals.fixed[[14]]),
        inla.emarginal(function(x) exp(x/attributes(sBachelor)[[3]]), 0.01*SA_work_6$marginals.fixed[[15]]),
        inla.emarginal(function(x) exp(x/attributes(sobese)[[3]]), 0.01*SA_work_6$marginals.fixed[[16]])),
  
  rbind(paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp,SA_work_6$marginals.fixed[[1]]))[1],2),",",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, SA_work_6$marginals.fixed[[1]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(time_RCS)[[5]][1]), SA_work_6$marginals.fixed[[2]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(time_RCS)[[5]][1]), SA_work_6$marginals.fixed[[2]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(time_RCS)[[5]][2]), SA_work_6$marginals.fixed[[3]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(time_RCS)[[5]][2]), SA_work_6$marginals.fixed[[3]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, SA_work_6$marginals.fixed[[4]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, SA_work_6$marginals.fixed[[4]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, SA_work_6$marginals.fixed[[5]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, SA_work_6$marginals.fixed[[1]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.575), inla.tmarginal(exp, SA_work_6$marginals.fixed[[6]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, SA_work_6$marginals.fixed[[6]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, SA_work_6$marginals.fixed[[7]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, SA_work_6$marginals.fixed[[7]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(syoung_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[8]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(syoung_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[8]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(smedian_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[9]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(smedian_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[9]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sold_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[10]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sold_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[10]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(spoverty_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[11]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(spoverty_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[11]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sBlack_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[12]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sBlack_perc)[[3]]), 0.01*SA_work_6$marginals.fixed[[12]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sHispanic)[[3]]), 0.01*SA_work_6$marginals.fixed[[13]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sHispanic)[[3]]), 0.01*SA_work_6$marginals.fixed[[13]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(stemp)[[3]]), SA_work_6$marginals.fixed[[14]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(stemp)[[3]]), SA_work_6$marginals.fixed[[14]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sBachelor)[[3]]), 0.01*SA_work_6$marginals.fixed[[15]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sBachelor)[[3]]), 0.01*SA_work_6$marginals.fixed[[15]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sobese)[[3]]), 0.01*SA_work_6$marginals.fixed[[16]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sobese)[[3]]), SA_work_6$marginals.fixed[[16]]))[2],2),"]")))),
  "~/Dropbox (Personal)/covid19_paper/tables_figures/INLA_weekly/SA_supplement_weekly.csv")


