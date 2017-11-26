#import international flight dataset 
int_flights<-read.csv(file.choose(),header=TRUE)
dim(int_flights)

#passenger to seat ratio
int_flights$pass_to_seat<-int_flights$PASSENGERS/int_flights$SEATS

#grab departures from U.S. cities
us_dep<-subset(int_flights,ORIGIN_COUNTRY=="US")
dim(us_dep)
us_dep1<-na.omit(us_dep)

# best/worst passenger to seat ratios (from the U.S.)
library(dplyr)
pass_seat_ratings<-ddply(us_dep1,.(DEST_CITY_NAME),summarize,med_pass_seat=median(pass_to_seat))

#count us departures to foreign cities
us_to_foreign<-us_dep %>% 
  group_by(DEST_CITY_NAME,DEPARTURES_PERFORMED) %>%
  summarise(n=n())


us_to_foreign$total_dep_city<-us_to_foreign$DEPARTURES_PERFORMED*us_to_foreign$n
us_to_foreign
us_to_foreign_city<-ddply(us_to_foreign,.(DEST_CITY_NAME),summarize,dep_month=sum(total_dep_city))
us_to_foreign_city
us_to_foreign_city1<-merge(us_to_foreign_city,pass_seat_ratings,by="DEST_CITY_NAME")
#foreign destinations with at least 1,000 flights 
us_to_foreign_city2<-subset(us_to_foreign_city1,dep_month>=1000)

#subset destinations 
#cancun 
cancun<-subset(us_dep,DEST_CITY_NAME=="Cancun, Mexico")
#cancun by month
cancun_month<-cancun %>% 
  group_by(MONTH,DEPARTURES_PERFORMED) %>%
  summarise(n=n())
cancun_month

cancun_month$total_dep_month<-cancun_month$DEPARTURES_PERFORMED*cancun_month$n
cancun_month 
cancun_dep_month<-ddply(cancun_month,.(MONTH),summarize,dep_month=sum(total_dep_month))
cancun_dep_month


#departures by month
us_to_for_month<-us_dep %>% 
  group_by(MONTH) %>%
  summarise(n=n())
us_to_for_month


#total departures by month
us_dep1<-us_dep %>% 
  group_by(MONTH,DEPARTURES_PERFORMED) %>%
  summarise(n=n())
us_dep1$total_dep_month<-us_dep1$DEPARTURES_PERFORMED*us_dep1$n
us_dep1

us_dep_month<-ddply(us_dep1,.(MONTH),summarize,dep_month=sum(total_dep_month))
us_dep_month


#2-sample t-test
merge_foreign<-merge(pass_seat_ratings,us_to_foreign,by="DEST_CITY_NAME")
cor.test(merge_foreign$med_pass_seat,merge_foreign$n) #r=0.28 

#pass to seat by distance group
us_dep$DISTANCE_GROUP<-as.factor(us_dep$DISTANCE_GROUP)
ggplot(us_dep,aes(x=DISTANCE_GROUP,y=pass_to_seat))+geom_boxplot()

#pass to seat by month
us_dep$MONTH<-as.factor(us_dep$MONTH)
ggplot(us_dep,aes(x=MONTH,y=pass_to_seat))+geom_boxplot()

#china flights 
china_flights<-subset(us_dep,DEST_COUNTRY_NAME=="China")
china_flights1<-na.omit(china_flights)
dim(china_flights)
dim(china_flights1)

#median passenger to seat ratio
pass_seat_china<-ddply(china_flights1,.(MONTH),summarize,med_pass_seat=mean(pass_to_seat))
pass_seat_china

#japanese flights 
jp_flights<-subset(us_dep,DEST_COUNTRY_NAME=="Japan")
jp_flights1<-na.omit(jp_flights)
dim(jp_flights)
dim(jp_flights1)

#median passenger to seat ratio (japan)
pass_seat_jp<-ddply(jp_flights1,.(MONTH),summarize,med_pass_seat=mean(pass_to_seat))
pass_seat_jp

#combine pass to seat (china and japan)
combine_china_jp<-cbind(pass_seat_china,pass_seat_jp)
summary(combine_china_jp)

#t-test china vs. japan (for international U.S. flights)
t.test(pass_seat_china$med_pass_seat,pass_seat_jp$med_pass_seat)

#count flights by month (departing to china)
us_to_china<-china_flights %>% 
  group_by(MONTH,DEPARTURES_PERFORMED) %>%
  summarise(n=n())
us_to_china$total_dep_month<-us_to_china$DEPARTURES_PERFORMED*us_to_china$n
us_to_china

china_dep_month<-ddply(us_to_china,.(MONTH),summarize,sun_month=sum(total_dep_month))
china_dep_month

#departure performed in china
dep_china<-china_flights %>% 
  group_by(DEPARTURES_PERFORMED) %>%
  summarise(n=n())
dep_china
dep_china$total_n<-dep_china$DEPARTURES_PERFORMED*dep_china$n
sum(dep_china$total_n) #3319 flights 

#china origin 
dep_china<-china_flights %>%
  group_by(ORIGIN) %>%
  summarize(n=n())
dep_china #ancorage international airport

#japan origin 
dep_jp<-jp_flights %>%
  group_by(ORIGIN) %>%
  summarize(n=n())
dep_jp

#departure performed japan
dep_japan<-jp_flights %>% 
  group_by(DEPARTURES_PERFORMED) %>%
  summarise(n=n())
dep_japan$total_n<-dep_japan$DEPARTURES_PERFORMED*dep_japan$n
sum(dep_japan$total_n) #7362 

#histogram (passenger to seat ratio)
qplot(int_flights$pass_to_seat,geom="histogram")+xlab("Passenger to Available Seat Ratio")+
  geom_vline(xintercept = 0.86,colour="red")+geom_vline(xintercept=0.829,colour="blue")+
  geom_vline(xintercept=0.7342857,colour="green")+geom_vline(xintercept=0.6633977,colour="orange")+
  geom_vline(xintercept=0.8785348,colour="purple")+ggtitle("High Booking for Seoul")+
  theme(plot.title = element_text(hjust = 0.5))


# seat ratio countries (facet wrap)
can<-us_to_foreign_city2[grep("Canada",us_to_foreign_city2$DEST_CITY_NAME),]
can$Country<-"Canada"
mex<-us_to_foreign_city2[grep("Mexico",us_to_foreign_city2$DEST_CITY_NAME),]
mex$Country<-"Mexico"
china<-us_to_foreign_city2[grep("China",us_to_foreign_city2$DEST_CITY_NAME),]
china$Country<-"China"


final_pais<-rbind(can,mex,china)
final_pais$id<-as.factor(final_pais$Country)

#histogram by country
hist_pais<-ggplot(final_pais,aes(x=id,y=med_pass_seat,color="Country"))+geom_boxplot()
hist_pais+xlab("Country")+ylab("Passenger to Seat Ratio")+ggtitle("Flyers Flock to Canada")+
  theme(plot.title = element_text(hjust = 0.5))

#departures by us airport 
dep_inter<-us_dep %>% 
  group_by(ORIGIN,DEPARTURES_PERFORMED) %>%
  summarise(n=n())
dep_inter$total_n<-dep_inter$DEPARTURES_PERFORMED*dep_inter$n

dep_inter_final<-ddply(dep_inter,.(ORIGIN),summarize,int_dep=sum(total_n))
dep_inter_final1<-subset(dep_inter_final,int_dep>5000)
ggplot(dep_inter_final1, aes(x = reorder(ORIGIN, -int_dep), y = int_dep)) +
  geom_bar(stat = "identity")+coord_flip()+
  ylab("Origin")+xlab("International Departures")+ggtitle("Miami is America's International Hub")+
  theme(plot.title = element_text(hjust = 0.5))

#country carriers
dim(us_dep)
can_carriers<-us_dep[grep("Canada",us_dep$DEST_CITY_NAME),]

can_carriers_count<-can_carriers %>% 
  group_by(CARRIER_NAME) %>%
  summarise(n=n())
sum(can_carriers_count$n)

mex_carriers<-us_dep[grep("Mexico",us_dep$DEST_CITY_NAME),]

mex_carriers_count<-mex_carriers %>% 
  group_by(CARRIER_NAME) %>%
  summarise(n=n())
sum(mex_carriers_count$n)

#us carriers departures (dense hubs)
us_carriers_count<-us_dep %>% 
  group_by(ORIGIN,CARRIER_NAME) %>%
  summarise(n=n())
sum(mex_carriers_count$n)






















