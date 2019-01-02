library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
#library(tslm)

household_power_consumption <- read.table("file:///C:/Users/User/Desktop/Ubiqum/Task6.1/household_power_consumption/household_power_consumption.txt", sep=";", stringsAsFactors = FALSE, header = TRUE)
View(household_power_consumption)

summary(household_power_consumption)
str(household_power_consumption)

mean(is.na(household_power_consumption))
mean(!is.na(household_power_consumption))
sum(is.na(household_power_consumption))
sum(!is.na(household_power_consumption))

household_power_consumption$Global_active_power<-as.numeric(household_power_consumption$Global_active_power) * (1000/60)
household_power_consumption$Global_reactive_power<-as.numeric(household_power_consumption$Global_reactive_power) * (1000/60)
household_power_consumption$Voltage<-as.numeric(household_power_consumption$Voltage)
household_power_consumption$Global_intensity<-as.numeric(household_power_consumption$Global_intensity)
household_power_consumption$Sub_metering_1<-as.numeric(household_power_consumption$Sub_metering_1)
household_power_consumption$Sub_metering_2<-as.numeric(household_power_consumption$Sub_metering_2)
household_power_consumption$Sub_metering_3<-as.numeric(household_power_consumption$Sub_metering_3)

str(household_power_consumption)

                                                ###########################
################################################## Create a new data frame #######################################
                                                ###########################

energy_consumption<-household_power_consumption
View(energy_consumption)

                                                ######################
################################################# creating Date+Time #################################################
                                                #####################

energy_consumption$DateTime<-paste(energy_consumption$Date, energy_consumption$Time)
View(energy_consumption)

                                              ###########################################
###############################################Converst the string to a Date/Time object###################################
                                              ###########################################

energy_consumption$DateTime<-strptime(energy_consumption$DateTime, "%d/%m/%Y %H:%M:%S", tz="") 
energy_consumption$DateTime<-as.POSIXct(energy_consumption$DateTime, tz="")
View(energy_consumption$DateTime)

                                                ##################################
################################################# Data preprocessing & remove NAs############################################
                                                ##################################

energy_consumption$Date<-NULL
energy_consumption$Time<-NULL

energy_consumption<-na.omit(energy_consumption)
summary(energy_consumption)

energy_consumption<-mutate(energy_consumption, Hours = format(energy_consumption$DateTime, "%H") )
energy_consumption<-mutate(energy_consumption, years=format(energy_consumption$DateTime, "%Y"))
energy_consumption<-mutate(energy_consumption, month=format(energy_consumption$DateTime, "%m"))
energy_consumption<-mutate(energy_consumption, days=format(energy_consumption$DateTime, "%d"))
energy_consumption<-mutate(energy_consumption, Week=format(energy_consumption$DateTime, "%w"))

energy_consumption<-mutate(energy_consumption, Other=round(Global_active_power) - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)
View(energy_consumption)

                                              ##############################
############################################### visualization and ploting #################################################
                                              #############################

#Hourly consumption 
ggplot(data = energy_consumption) +aes(x = Hours, weight = Other) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal() 

# Yearly consumption
ggplot(data = energy_consumption) + aes(x = years, weight = Other) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

#Energy Consumption 
ggplot(energy_consumption, aes(Other)) + geom_line(stat="density",
                                  adjust=.25, colour="Black") #need to revove anything >45

#Monthly energy
ggplot(energy_consumption, aes(month, y=Other)) +
    geom_jitter()

  
#ggplot(data = energy_consumption) +
  #aes(x = month, weight = Other) +
 # geom_bar(fill = "#0c4c8a") +
 # theme_minimal()

                                                          ################
########################################################### TOTAL Energy ##################################################
                                                          ################
energy_consumption<-mutate(energy_consumption, TotalSub_met = 
                             Sub_metering_1 + Sub_metering_2 + Sub_metering_3)

View(energy_consumption)

ggplot(energy_consumption, aes(TotalSub_met)) + geom_line(stat="density", adjust=.25, colour="red") +
  geom_line(stat="density") + geom_line(stat="density", adjust=2, colour="blue")

                                ############################
#################################Energy_consumption Per day##########################################################
                                ############################

Per_day<-group_by(energy_consumption, days) %>%
  summarize(dailySubmet = sum(TotalSub_met), count=n(),
              avg_daily = mean(Global_active_power),
            Kitchen=mean(Sub_metering_1),
            laundry=mean(Sub_metering_2),
            ACs=mean(Sub_metering_3))

View(Per_day)

#GLOBAL ACTIVE POWER
ggplot(data = Per_day) +
  aes(y = avg_daily, x = seq_along(avg_daily)) +
  geom_line(color = "Blue") 

#Daily consumption per compartment
ggplot(Per_day) + geom_area(mapping = aes(as.numeric(days), avg_daily),fill="gray")+
  geom_line(aes(as.numeric(days), ACs,colour="ACs"),size=2)+
  geom_line(aes(as.numeric(days), laundry,colour="laundry"),size=1.5)+
  geom_line(aes(as.numeric(days), Kitchen, colour="Kitchen"),size=1.5)+ xlab("Days")+ ylab("Average energy")


                                      #################################
              ######################### Energy consumption per Hours ###################################
                                      ################################

Hourly_Energy<-group_by(energy_consumption, Hours) %>% summarise(
  Sub_metering_3 = mean(Sub_metering_3),
  Sub_metering_2 = mean(Sub_metering_2),
  Sub_metering_1 = mean(Sub_metering_1),
  TotalPower = mean(Global_active_power)
)

View(Hourly_Energy)

#TOTAL ENERGY = Global_active_power
ggplot(Hourly_Energy, aes(Hours, TotalPower, fill=TotalPower))+
  geom_bar(stat="identity")

#Sub_metering_3 = ACs
ggplot(Hourly_Energy, aes(Hours, Sub_metering_3, fill=Sub_metering_3))+
  geom_bar(stat="identity")

#sub_metering_2 = laundry
ggplot(Hourly_Energy, aes(Hours, Sub_metering_2, fill=Sub_metering_2))+
  geom_bar(stat="identity")

#sub_metering_1 = Kitchen
ggplot(Hourly_Energy, aes(Hours, Sub_metering_1, fill=Sub_metering_1))+
  geom_bar(stat = "identity")


                                        ####################
#########################################YEARLY CONSUMPTION#########################################################
                                        ####################

yearly_energy<-group_by(energy_consumption, years)%>%
    summarise(yearlyEnerg=sum(TotalSub_met),count=n(),
              Kitchen=mean(Sub_metering_1),
              laundry=mean(Sub_metering_2),
              ACs=mean(Sub_metering_3),
              Totalpower=mean(Global_active_power))
View(yearly_energy)

ggplot(yearly_energy, aes(years, ACs, fill=ACs))+geom_bar(stat="identity")                            
ggplot(yearly_energy, aes(years, laundry, fill=laundry))+geom_bar(stat="identity")                            
ggplot(yearly_energy, aes(years, Kitchen, fill=Kitchen))+geom_bar(stat="identity")                            

ggplot(yearly_energy, aes(years, Totalpower))+geom_bar(stat="identity")                       

#AVERAGE ENERGY#
ggplot(yearly_energy, aes(years, avg_submet, color=years,shape=years))+
  geom_point(size=10)+geom_smooth()

#TOTAL ENERGY & AVG ENERGY (If we plot both the sum_of_Energy & AVG_energy, we realize that 2006 has the highest consumption of energy)
ggplot(data = yearly_energy) + aes(x = years, weight = avg_submet) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

                              ######################
#################################MONTHLY CONSUMPTION###############################################################
                              #####################

monthly_energy<-group_by(energy_consumption, month,days) %>%
  summarise(Kitchen=mean(Sub_metering_1),
            laundry=mean(Sub_metering_2),
            ACs=mean(Sub_metering_3),
            Totalpower=mean(Global_active_power))

View(monthly_energy)

#MONTHLY ENERGY
ggplot(monthly_energy, aes(as.numeric(days), Totalpower))+ geom_line(color="blue", size=1.5)+
  facet_wrap(~month)+theme_classic()+labs(x="Day",y="Monthly Energy")

#DAILY ENERGY
ggplot(monthly_energy, aes(as.numeric(month), Totalpower))+geom_line(color="blue",size=1.5)+
  facet_wrap(~days)+theme_gray()+labs(x="Month", y= "Energy")

                    ############YEARS' SELECTION#########
######################FILTERING DIFFERENT YEAR###########################################################################
                    ###################################

year_2k6<- filter(energy_consumption, years=="2006")
year_2k7<-filter(energy_consumption, years=="2007")
year_2k8<-filter(energy_consumption, years=="2008")
year_2k9<-filter(energy_consumption, years=="2009")
year_2k10<-filter(energy_consumption, years=="2010")

                                                      ######################
#######################################################SEASONAL CONSUMPTION######################################################
                                                      ######################

#creating seasonality for years
library(lubridate)
energy_consumption$Quarter<-energy_consumption$DateTime %>%
  quarter(with_year = FALSE)

View(energy_consumption)

#+++++++++++++++++++++++++++++++++++++++++++++++#YEARLY CONSUMPTION PER CONPARTMENTS++++++++++++++++++++++++++++++++++++++++++++++
YearsQuarters<-energy_consumption %>%
  filter(years ==c(2007,2008,2009,2010)) %>%
  group_by(year(DateTime),Quarter, month) %>%
  summarise(Kitchen=sum(Sub_metering_1),
            laundry=sum(Sub_metering_2),
            ACs=sum(Sub_metering_3),
            Totalpower=sum(Global_active_power),)

View(YearsQuarters)

colnames(YearsQuarters)<-c("years","Quarter", "month", "kitchen","Laundry","ACs","Totalpower")
#ggplot(YearsQuarters) + geom_area(mapping = aes(x=Quarter ,y=Totalpower)) +
  #geom_line(aes(Quarter, Kitchen, colour="kitchen"), size=1.5)+
  #geom_line(aes(Quarter, laundry, colour="laundry"),size=1.5)+
  #geom_line(aes(Quarter, ACs, colour="ACs"),size=1.5) + facet_wrap(~year)+ labs(x="Years")

ggplot(YearsQuarters) + geom_area(mapping = aes(x=as.numeric(month) ,y=Totalpower)) +
  geom_line(aes(as.numeric(month), kitchen, colour="kitchen"), size=1.5)+
  geom_line(aes(as.numeric(month), Laundry, colour="laundry"),size=1.5)+
  geom_line(aes(as.numeric(month), ACs, colour="ACs"),size=1.5) + facet_wrap(~years)+ labs(x="Years")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++#WINTER ENERGY CONSUMPTION IN 2009++++++++++++++++++++++++++++++++++
Winter2k9<-year_2k9 %>%
  filter(quarter(DateTime)==1) %>%
  group_by(hour(DateTime)) %>%
  summarise(Kitchen=sum(Sub_metering_1),
            laundry=sum(Sub_metering_2),
            ACs=sum(Sub_metering_3),
            Totalpower=sum(Global_active_power),)

View(Winter2k9)

names(Winter2k9)[names(Winter2k9)=="hour(DateTime)"]<-"Hours"

# SUM OF WINTER TOTAL ENERGY VS c(ACs&HEAT, KITCHEN, LAUNDRY)
ggplot(Winter2k9) + geom_area(mapping = aes(Hours,Totalpower)) + 
  geom_line(aes(x=Hours, y=Kitchen, colour="Kitchen"),size=1.5)+
  geom_line(aes(x=Hours, y=laundry, colour="laundry"),size=1.5)+
  geom_line(aes(x=Hours, y=ACs, colour="ACs"),size=1.5) +
  ggtitle("Winter")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++YEAR 2008+++++++++++++++++++++++++++++++++++++++++++++++
#SUM OF 2008 ENERGY
year2k8<- energy_consumption %>%
  filter(years==2008) %>%
  group_by(Quarter, month) %>%
  summarise(Kitchen=sum(Sub_metering_1),
            laundry=sum(Sub_metering_2),
            ACs=sum(Sub_metering_3),
            Totalpower=sum(Global_active_power),)
View(year2k8)

ggplot(year2k8) + geom_area(mapping = aes(as.numeric(month), Totalpower))+
  geom_line(aes(x=as.numeric(month), y=Kitchen, colour="kitchen"),size=1)+
  geom_line(aes(x=as.numeric(month), y=laundry,colour="Laundry"),size=1)+ 
  geom_line(aes(x=as.numeric(month), y= ACs,colour="AC"),size=1 )+
  facet_wrap(~Quarter) + labs(x="Years 2008")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++YEAR 2007++++++++++++++++++++++++++++++++++++++++++++

year2k7<-energy_consumption %>%
  filter(years==2007) %>%
  group_by( Quarter, month) %>%
  summarise(Kitchen=sum(Sub_metering_1),
            laundry=sum(Sub_metering_2),
            ACs=sum(Sub_metering_3),
            Totalpower=sum(Global_active_power),)
View(year2k7)  

names(year2k7)[names(year2k9)=="year(DateTime)"]<-"Year"

ggplot(year2k7) + geom_area(mapping = aes(as.numeric(month), Totalpower))+
  geom_line(aes(x=as.numeric(month), y=Kitchen, colour="kitchen"),size=1)+
  geom_line(aes(x=as.numeric(month), y=laundry,colour="Laundry"),size=1)+ 
  geom_line(aes(x=as.numeric(month), y= ACs,colour="AC"),size=1 )+
  facet_wrap(~Quarter) + labs(x="Years 2007")

                                                        ######################                                                     
#########################################################  TIME SERIES USES ##################################################
#AVERAGE USER OF ENERGY                                 ######################
energy_ts<-energy_consumption %>% 
  filter(years != 2006) %>% group_by(years, month) %>%
  summarise(kitchen = mean(Sub_metering_1),
            laundry = mean(Sub_metering_2),
            ACs = mean(Sub_metering_3),
            Totalpower = mean(Global_active_power))

summary(energy_ts)

Totalpower_ts<-ts(energy_ts$Totalpower, frequency = 12, start = 2007)
kitchen_ts<-ts(energy_ts$kitchen, frequency=12, start = 2007)
laundry_ts<-ts(energy_ts$laundry, frequency = 12, start=2007)
AC_ts<-ts(energy_ts$ACs, frequency = 12, start = 2007)


All_energy<-c(Totalpower_ts,kitchen_ts,laundry_ts,AC_ts)
summary(All_energy)
#+++++++++++++++++++++++++++++++++++++++++++PLOTTING TOTALPOWER+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot.ts(Totalpower_ts) # We can see from this ts that there seems to be seasonal variation in the energy consumption

#mean
abline(reg=lm(Totalpower_ts~time(Totalpower_ts))) #We see that the points are different from each other included our mean

#trend
plot(aggregate(Totalpower_ts, FUN=mean)) # our values are downward, decreasing according to the time

#Boxplot that analyse a particular moment we experience max energy consumption
#check the seasonality
boxplot(Totalpower_ts~cycle(Totalpower_ts))

#log function
plot(log(Totalpower_ts)) #Our seasonal fluctuations and random fluctuations seem to be roughly constant over time

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++PLOTTING KITCHEN+++++++++++++++++++++++++++++++++++++++++++++++++
plot.ts(kitchen_ts)
abline(reg = lm(kitchen_ts~time(kitchen_ts))) #variance and the mean are !=
plot(aggregate(kitchen_ts, FUN = mean))#values are decreasing and upward at the end
boxplot(kitchen_ts~cycle(Totalpower_ts),xlab="month",ylab="energy") #Jul and August have lower consumption with higher variance 
plot(log(kitchen_ts))

#++++++++++++++++++++++++++++++++++++++++++++++PLOTTING LAUNDRY+++++++++++++++++++++++++++++++++++++++++++++++++++++
plot.ts(laundry_ts) # seasonal variance in the data
abline(reg=lm(laundry_ts~time(laundry_ts))) #Difference in variance and mean
plot(aggregate(laundry_ts, FUN = mean)) #Straight downward
boxplot(laundry_ts~cycle(laundry_ts), xlab="Month",ylab="laundry") #less energy consumption during JUL & AUGUST

#++++++++++++++++++++++++++++++++++++++++++++++++PLOT AC & HEAT+++++++++++++++++++++++++++++++++++++++++++++++++

plot.ts(AC_ts) #less random fluctuation in the data over time
abline(reg=lm(AC_ts~time(AC_ts))) 
plot(aggregate(AC_ts, FUN = mean)) #our values go upward
boxplot(AC_ts~cycle(AC_ts), xlab="Month", ylab="AC") #Busiest months JAN, FEV and DEC
plot(log(AC_ts))

#+++++++++++++++++++++++++++++++++++++++++++++++++GENERAL PLOTTING (Totalpower_ts)+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Totalpower_ts
ts.plot(kitchen_ts, laundry_ts, AC_ts, gpars = list(xlab="YEARS", ylab="TOTAL_ENERGY",col=c('black','red','blue')))
legend("topleft", legend=c("kitchen_ts", "laundry_ts", "AC_ts"), 
       text.col=c('black','red','blue'),ncol=3) 

#Let's DECOMPOSE OUR SEASONAL TIME SERIES : It will each time series into seasonal(TREND AND RANDOM COMPONENTS) 

plot(decompose(Totalpower_ts))

plot(forecast(Totalpower_ts, h=60)) #60 forecast points
autoplot(forecast(HoltWinters(Totalpower_ts, alpha = 0.1, beta=0, gamma = 1)))
plot(stl(Totalpower_ts, s.window = 7))

ggseasonplot(Totalpower_ts, col = rainbow(5), year.labels = TRUE, continuous = TRUE)+
  geom_line(size=1)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++KITCHEN_ts+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#kitchen_ts
plot(decompose(kitchen_ts))
plot.ts(kitchen_ts)
plot(decompose(kitchen_ts))
plot(forecast(kitchen_ts, h=60))
autoplot(forecast(HoltWinters(kitchen_ts, alpha=0.1,beta=0,gamma=1)))
plot(stl(kitchen_ts, s.window = 7))

ggseasonplot(kitchen_ts, col = rainbow(5), year.labels = TRUE, continuous = TRUE)+
  geom_line(size=1)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++LAUNDRY_ts++++++++++++++++++++++++++++++++++++++++++++++++++++++
#LAUNDRY_ts
plot(decompose(laundry_ts))
plot.ts(laundry_ts)
plot(decompose(laundry_ts))
plot(forecast(laundry_ts, h=60))
autoplot(forecast(HoltWinters(laundry_ts, alpha = 0.1, beta = 0,gamma=1)))
plot(stl(laundry_ts, s.window = 7))

ggseasonplot(laundry_ts, col = rainbow(5), year.labels = TRUE, continuous = TRUE)+
  geom_line(size=1)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++AC_ts+++++++++++++++++++++++++++++++++++++++++++++++++++
#AC_ts
plot(decompose(AC_ts))
plot.ts(AC_ts)
plot(decompose(AC_ts))
plot(forecast(AC_ts, h=60))
autoplot(forecast(HoltWinters(AC_ts, alpha = 0.1, beta = 0, gamma = 1)))

ggseasonplot(AC_ts, col = rainbow(5), year.labels = TRUE, continuous = TRUE)+
geom_line(size=1)

