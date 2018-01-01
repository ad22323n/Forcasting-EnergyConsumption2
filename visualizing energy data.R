
household_power_consumption <- read.table("C:/Users/User/Desktop/Ubiqum/household_power_consumption/household_power_consumption.txt", sep=";", stringsAsFactors = FALSE, header = TRUE)
View(household_power_consumption)

household_power_consumption$Global_active_power<- as.numeric(household_power_consumption$Global_active_power) *(1000 /60)
household_power_consumption$Global_reactive_power<-as.numeric(household_power_consumption$Global_reactive_power)* (1000/60)
household_power_consumption$Voltage<- as.numeric(household_power_consumption$Voltage)
household_power_consumption$Global_intensity<- as.numeric(household_power_consumption$Global_intensity)
household_power_consumption$Sub_metering_1<- as.numeric(household_power_consumption$Sub_metering_1)
household_power_consumption$Sub_metering_2<- as.numeric(household_power_consumption$Sub_metering_2)
household_power_consumption$Sub_metering_3<- as.numeric(household_power_consumption$Sub_metering_3)

str(household_power_consumption)

household_power_consumption2<-household_power_consumption
View(household_power_consumption2)

str(household_power_consumption$Date)

#creating Date+Time
household_power_consumption2$DateTime <- paste(household_power_consumption2$Date,sep=":", household_power_consumption2$Time)
View(household_power_consumption2)
#household_power_consumption2$DateTime <- strptime(household_power_consumption2$DateTime, "%d/%m/%Y %H:%M:%S", tz ="") # Converts the string to a Date/Time Object
household_power_consumption2$DateTime =  as.POSIXct(household_power_consumption2$DateTime, "%d/%m/%Y: %H:%M:%S", tz = "GMT")
household_power_consumption2$Date<- as.Date(household_power_consumption2$Date, "%d/%m/%Y")
View(household_power_consumption2)
summary(household_power_consumption2)
#household_power_consumption$DateTime =  as.POSIXct(household_power_consumption$DateTime, "%d/%m/%Y %H:%M:%S", tz ="GMT")
No_NA<- na.omit(household_power_consumption2)
View(No_NA)
summary(No_NA)
#________________________ Working YET___________________
No_NA<- mutate(No_NA, Hours =format(No_NA$DateTime, "%H"))
#View(No_NA)
No_NA<- mutate(No_NA, years =format(No_NA$DateTime,  "%Y"))
#View(No_NA)
No_NA<- mutate(No_NA, months=format(No_NA$DateTime, "%m"))
#View(No_NA)
No_NA<- mutate(No_NA,  days=format(No_NA$DateTime, "%d"))
#View(No_NA)
No_NA<- mutate(No_NA, Other=round(Global_active_power) - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)
#View(No_NA)


View(No_NA)

str(No_NA)

#_______________________________________________________________Usable________________________________________________________

#per day

Per_days<- group_by(No_NA, days)%>%
  summarize(DaylyEnergy=sum(Global_active_power), count=n(),
    avg_d=mean(Global_active_power))

ggplot(Per_days, aes(as.numeric(days), avg_d))+geom_point()+geom_line(na.rm = TRUE) + 
  labs(x="Days", y="Avg_consumption_per_days")+ theme_minimal()

ggplot(Per_days, aes(x=as.numeric(days), y=avg_d, colour = avg_d)) +geom_point()+geom_line()+labs(x="Days", y="Avg_Energy")

################################################PerHours#####################################
#Per Hours

Per_Hours<-group_by(No_NA, Hours, months)%>%
  summarise(Sub_metering_1=mean(Sub_metering_1),
            Sub_metering_2=mean(Sub_metering_2),
            Sub_metering_3=mean(Sub_metering_3),
            Totalpower=mean(Global_active_power))
View(Per_Hours)

ggplot(Per_Hours, aes(Hours,Totalpower, fill=Totalpower)) +geom_bar(stat="identity", position="identity")+facet_wrap(~months)

#########################################################################################
#Per year

Per_years<- group_by(No_NA, years)%>%
  summarise(YearlyEnergy= sum(Global_active_power),
            count=n(), avgEnerg_years=mean(Global_active_power))

ggplot(Per_years, aes(as.numeric(years), avgEnerg_years, colour=avgEnerg_years)) + geom_line() + geom_smooth(method="lm") #so weirddddd

ggplot(Per_years, aes(x=as.numeric(years), y=YearlyEnergy)) + geom_point() + geom_line() + labs(x= "Years", y="Energy_consumption")

ggplot(Per_years, aes( x=years, y=YearlyEnergy)) + geom_bar() + labs(x= "Years", y="Energy_consumption")

ggplot(Per_years, aes(years,YearlyEnergy, color=years, shape=years)) +geom_point(size=10) + geom_smooth(method = "lm", aes(fill=years))
#################################################################
#Per_months
Per_months<- group_by(No_NA, months,days)%>%
            summarise(Sub_metering_1=mean(Sub_metering_1),
                      Sub_metering_2=mean(Sub_metering_2),
                      Sub_metering_3=mean(Sub_metering_3),
                      Totalpower=mean(Global_active_power))


ggplot(Per_months, aes(Totalpower)) + geom_freqpoly()
#12Months
ggplot(Per_months, aes(x=as.numeric(months),y=Totalpower ))+ geom_line(color="blue",, size=1.5)+
  facet_wrap(~days)+ theme_classic()+labs(x="Daily_consumption_per_Month", y="MonthlyEnergy")

#___________________selecting per years________________________

years2006<-filter(No_NA, years=="2006")
#ggplot(years2006, aes(x=as.numeric(days), y= Global_active_power)) + geom_line()+ labs(x="consumptions_per_Days")
#prop.table(table(years2006$TotalSubmet))
years2007<-filter(No_NA, years==2007)
years2008<- filter(No_NA, years==2008)
year2009<- filter(No_NA, years==2009)
years2010<- filter(No_NA, years==2010)
years consumbtion

#Seasonal consumption TOBECONTINUED

year2009winter<-year2009%>%
  filter(quarter(DateTime)==1)%>%
  group_by(hour(DateTime))%>%
  summarise(Sub_metering_1=sum(Sub_metering_1),
   Sub_metering_2=sum(Sub_metering_2),
   Sub_metering_3=sum(Sub_metering_3),
   Totalpower=sum(Global_active_power),
 )

colnames(year2009winter)<-c("Hour","Sub_Meter_1","Sub_Meter_2","Sub_Meter_3","TotalPower")
 ggplot(data = year2009winter)+geom_area(mapping = aes(x=Hour,y=TotalPower))+
  geom_line(aes(x=Hour,y=Sub_Meter_1),color="red")+
  geom_line(aes(x=Hour,y=Sub_Meter_2),color="green")+
  geom_line(aes(x=Hour,y=Sub_Meter_3),color="yellow")+
  ggtitle("Winter")+ylab("") 
 
#les annees par season and creating seasonal function
No_NA$Hours<- format(No_NA$DateTime, "%H")
No_NA$Quarter<-No_NA$DateTime%>% quarter(with_year=FALSE)
 
LesAnnees<- No_NA%>%
  filter(years == c(2007,2008,2009,2010))%>%
  group_by(year(DateTime), Quarter,months)%>%
  summarise(Sub_metering_1=mean(Sub_metering_1),
            Sub_metering_2=mean(Sub_metering_2),
            Sub_metering_3=mean(Sub_metering_3),
            Totalpower=mean(Global_active_power),
            
  )
colnames(LesAnnees)<-c("years","Quarter", "Months", "kitchen","Laundry","AC","Totalpower")
ggplot(LesAnnees) + geom_area(mapping = aes(x=as.numeric(Months), y=Totalpower))+
  geom_line(aes(x=as.numeric(Months), y=kitchen, colour="kitchen"),size=2)+
  geom_line(aes(x=as.numeric(Months), y=Laundry,colour="Laundry"),size=2)+ 
  geom_line(aes(x=as.numeric(Months), y= AC,colour="AC"),size=2)+ theme(legend.position = "Bottom")+
facet_wrap(~years) + labs(x="Years")+ guides(colour = guide_legend(title = "")) +
  theme_pander()


An2008<- No_NA%>%
  filter(years==2008)%>%
  group_by(year(DateTime), Quarter, months)%>%
  summarise(Sub_metering_1=mean(Sub_metering_1),
            Sub_metering_2=mean(Sub_metering_2),
            Sub_metering_3=mean(Sub_metering_3),
            Totalpower=mean(Global_active_power),)
colnames(An2008)<- c("years", "Quarter", "Months","kitchen","Laundry","AC","Totalpower")
ggplot(An2008) + geom_area(mapping = aes(x=as.numeric(Months), y=Totalpower))+
  geom_line(aes(x=as.numeric(Months), y=kitchen, colour="kitchen"),size=1)+
  geom_line(aes(x=as.numeric(Months), y=Laundry,colour="Laundry"),size=1)+ 
  geom_line(aes(x=as.numeric(Months), y= AC,colour="AC"),size=1 )+
  facet_wrap(Quarter~years) + labs(x="Years 2008")+
  theme_pander()+theme_economist()


An2009<- No_NA%>%
  filter(years==2009)%>%
  group_by(year(DateTime), Quarter, months)%>%
  summarise(Sub_metering_1=sum(Sub_metering_1),
            Sub_metering_2=sum(Sub_metering_2),
            Sub_metering_3=sum(Sub_metering_3),
            Totalpower=sum(Global_active_power),)
colnames(An2009)<- c("years", "Quarter", "Months","kitchen","Laundry","AC","Totalpower")
ggplot(An2009) + geom_area(mapping = aes(x=as.numeric(Months), y=Totalpower))+
  geom_line(aes(x=as.numeric(Months), y=kitchen, colour ="kitchen"), size=1.5)+
    geom_line(aes(x=as.numeric(Months), y=Laundry,colour="Laundry"),size=1.5)+ 
    geom_line(aes(x=as.numeric(Months), y= AC,colour="AC"),size=1.5)+
    facet_wrap(Quarter~years) + labs(x="Years 2009")  
  
An2010<- No_NA%>%
  filter(years==2010)%>%
  group_by(year(DateTime), Quarter, months)%>%
  summarise(Sub_metering_1=mean(Sub_metering_1),
            Sub_metering_2=mean(Sub_metering_2),
            Sub_metering_3=mean(Sub_metering_3),
            Totalpower=mean(Global_active_power),)


colnames(An2010)<- c("years", "Quarter", "Months","kitchen","Laundry","AC","Totalpower")
ggplot(An2010) + geom_area(mapping = aes(as.numeric(Months), y =Totalpower))+
  geom_line(aes(x=as.numeric(Months), y=kitchen, colour ="kitchen"), size=1.5)+
  geom_line(aes(x=as.numeric(Months), y=Laundry,colour="Laundry"),size=1.5)+ 
  geom_line(aes(x=as.numeric(Months), y= AC,colour="AC"),size=1.5)+
  facet_wrap(Quarter~years) + labs(x="Years 2010")  

##################################################################################
Annees<-filter(No_NA, years%in% c(2007,2008,2009,2010), as.numeric(months))%>%
  group_by(years,months)%>%
  summarise(Sub_metering_1=mean(Sub_metering_1),
            Sub_metering_2=mean(Sub_metering_2),
            Sub_metering_3=mean(Sub_metering_3),
            Totalpower=mean(Global_active_power),
  )
colnames(Annees)<- c("years", "months","kitchen","Laundry","AC","Totalpower")
ggplot(data = Annees) + geom_area(mapping = aes(x=as.numeric(months), y=Totalpower))+
  geom_line(aes(x=as.numeric(months), y= kitchen, color="kitchen"), size=1.5)+
  geom_line(aes(x=as.numeric(months), y= Laundry, color="Laundry"),size=1.5)+
  geom_line(aes(x=as.numeric(months), y= AC, color="AC_WatHeat"),size=1.5) + ylab("Years")+ facet_wrap(~years)+labs(x="Months")+ theme_economist()
  
ggplot(Annees, aes(months,Totalpower))+ geom_line(aes(group=Totalpower))

#_______________________________________________________________________________
myears<-No_NA %>%
group_by(years, months)%>%
  summarise(mYearSubmet=mean(Global_active_power),
            count=n())

ggplot(myears, aes( as.numeric(years) + (as.numeric(months) -1) / 12, mYearSubmet)) + geom_line(size=3)+labs(x="Years", y="Total Power")+theme_economist_white()

ggplot(myears, aes(as.numeric(months), mYearSubmet, group= years))+theme_dark()+ geom_line(aes(colour =years), size=3)+labs(x="Months", y="Monthly_Energy_per_Years")
#_______________________________________________

submeters<-No_NA%>%
  group_by(year(DateTime))%>%
  summarise(Sub_metering_1=mean(Sub_metering_1),
            Sub_metering_2=mean(Sub_metering_2),
            Sub_metering_3=mean(Sub_metering_3),
            mYearSubmet=mean(Global_active_power))
            
colnames(submeters)<- c("Years","kitchen","Laundry","AC","Total_Power")
ggplot(submeters) +geom_line(aes(x=Years, y=kitchen, color= "Total_Power"),  size=3) +  ylab("Total_power")


#2nd -----------------------------------------------------------------------------------------------------------------
No_NAtimeSeries<- No_NA%>% filter(years !=2006)%>%
  group_by(years, months)%>%
  summarise(Sub_metering_1=mean(Sub_metering_1),
          Sub_metering_2=mean(Sub_metering_2),
          Sub_metering_3=mean(Sub_metering_3),
          TotalPower=mean(Global_active_power))
summary(No_NAtimeSeries)

#No_NAtimeSeries$days<-as.Date(No_NAtimeSeries$days)

TotalPower<- ts(No_NAtimeSeries$TotalPower, frequency = 12, start = c(2007))
kitchen<- ts(No_NAtimeSeries$Sub_metering_1, frequency = 12, start = c(2007))
laundry<- ts(No_NAtimeSeries$Sub_metering_2, frequency = 12, start=c(2007))
ACts<-ts(No_NAtimeSeries$Sub_metering_3, frequency = 12, start = c(2007))

AllIn<- c(TotalPower, kitchen, laundry, ACts)

View(No_NAtimeSeries)
#=======================================ToTALPOWER=============================================================
#plotting
plot.ts(TotalPower)
#plot.ts(decompose(TotalPower))
#mean
abline(reg=lm(TotalPower~time(TotalPower)))# we see taht point are different from each other,included our mean
#trend:
plot(aggregate(TotalPower, FUN = mean)) # our values are downward, decreasing according to the time
#Boxplot that analyse a particular moment I was experiencing a max Energy consumption
# check the seasonality
boxplot(TotalPower~cycle(TotalPower))
#LOg functioin
plot(log(TotalPower)) # our fluctuation got constant 
#=================================KITCHEN==============================================================
plot.ts(kitchen)
abline(reg=lm(kitchen~time(kitchen)))# variance and the mean are !=
plot(aggregate(kitchen, FUN = mean)) #values are decreasing and upward at the end
boxplot(kitchen~cycle(TotalPower), xlab="Months", ylab="Kithen") #looking for seasonality
plot(log(kitchen))
#===============================LAUNDRY================================================================
plot.ts(laundry)
abline(reg=lm(laundry~time(laundry))) #difference in variance and mean
plot(aggregate(laundry, FUN = mean)) # straight downward
boxplot(laundry~cycle(laundry), xlab="Month",ylab="laundry")#looking for seasonality
plot(log(laundry))
#==============================AC======================================================================
plot.ts(ACts)
abline(reg=lm(ACts~time(ACts)))# our mean and variance at totally different
plot(aggregate(ACts, FUN = mean)) # our values go upward
boxplot(ACts~cycle(ACts),xlab="Month", ylab="AC")#looking for busiest season
plot(log(ACts))
#+++++++++++++++++++++++++++++++++GENERAL PLOTTING++++++++++++++++++++++++++++++++++++++++++++++++++
#AllVariable<-c(TotalPower,kitchen,laundry, ACts)

ts.plot(kitchen,laundry, ACts, gpars=list(xlab="years", ylab="Total_Energy", col = rainbow(5)))

#AllData<- data.frame(years(years), month(months), TotalPower,kitchen,laundry,ACts)


#3nd, Forcasting a time series
#Lest's group our data per_days to remove some granularity
Mydata<- No_NA%>%
  group_by(years,months)%>%
  summarise(Sub_metering_1=mean(Sub_metering_1),
            Sub_metering_2=mean(Sub_metering_2),
            Sub_metering_3=mean(Sub_metering_3),
            TotalPower=mean(Global_active_power))

colnames(Mydata)<- c("Years", "Months", "kitchen", "laundry", "AC", "Totalpower")
View(Mydata)

Mydata<-ts(Mydata$Totalpower, frequency = 12, start =( 2007))

plot(decompose(Mydata))

plot(forecast(Mydata, h = 60 ))
autoplot(forecast((HoltWinters(Mydata, alpha = 0.1, beta = 0.1, gamma = 0.5))))

#****************************FORECAST**************************

#++++++++++++++++++++++++++++++++Kitchen+++++++++++++++++++++++++++++++
Mydata2<- ts(kitchen, frequency = 12, start = (2007))
plot(decompose(Mydata2))
plot(forecast(Mydata2, h=60))
plot(forecast(HoltWinters(Mydata2, alpha = 0.1, beta = 0.1, gamma = 0.1)))
summary(Mydata2)

#==============================laundry==================================
Mydata3<-ts(Mydata$laundry, frequency = 12, start = (2007))
plot(decompose(Mydata3))
plot(forecast(Mydata3, h=60))
autoplot(forecast(HoltWinters(Mydata3, alpha = 0.2, beta = 0.1, gamma = 0.1)))
summary(Mydata3)

#==============================AC========================================
Mydata4<-ts(Mydata$AC, frequency = 12, start = (2007))
plot(decompose(Mydata4))
plot(forecast(Mydata4, h=60))
autoplot(forecast(HoltWinters(Mydata4, alpha = 0.1, beta = 0.1, gamma = 1)))

plot(stl(Mydata4, s.window = 7))

#******************************^^^^^^%**********************************
No_NAtimeSeries1<- mutate(No_NA, Myweek= format(No_NA$DateTime, "%W") )%>%
group_by(years, months, Myweek)%>%
  filter(years!=2006)%>%
  summarise(Sub_metering_1=mean(Sub_metering_1),
            Sub_metering_2=mean(Sub_metering_2),
            Sub_metering_3=mean(Sub_metering_3),
            TotalPower=mean(Global_active_power))

TotalPower<- ts(No_NAtimeSeries1$Totalpower, frequency = 52, start = (2007))
kitchen<- ts(No_NAtimeSeries1$Sub_metering_1, frequency = 52, start = (2007))
laundry<- ts(No_NAtimeSeries1$Sub_metering_2, frequency = 52, start=(2007))
ACts<-ts(No_NAtimeSeries1$Sub_metering_3, frequency = 52, start = (2007))


View(No_NAtimeSeries1)
colnames(No_NAtimeSeries1)<-c("Years","Months","weeks","kitchen","laundry","AC","Totalpower")

############################TOTALPOWER###########################################################
Myweek<- ts(No_NAtimeSeries1$Totalpower, frequency = 52, start = (2007))
plot.ts(Myweek)
plot(decompose(Myweek))
plot(forecast(Myweek, h=60))
autoplot(forecast(HoltWinters(Myweek, alpha = 0.1, beta = 0, gamma = 1)))
plot(stl(Myweek, s.window=7))

#+++++++++++++++++++++++++Kitchen++++++++++++++++++++++++++++++++++++
Myweek1<-ts(No_NAtimeSeries1$kitchen, frequency = 52, start = (2007))
plot.ts(Myweek1)
plot(decompose(Myweek1))
plot(forecast(Myweek1, h=60))
autoplot(forecast(HoltWinters(Myweek1, alpha = 0.1, beta = 0, gamma = 1)))
plot(stl(Myweek1, s.window = 7))

#+++++++++++++++++++++++++++++LAUNDRY+++++++++++++++++++++++++++++++
Myweek2<- ts(No_NAtimeSeries1$laundry, frequency = 52, start = (2007))
plot.ts(Myweek2)
plot(decompose(Myweek2))
plot(forecast(Myweek2, h=60))
autoplot(forecast(HoltWinters(Myweek2, alpha = 0.1, beta=0, gamma = 0)))

#++++++++++++++++++++++++++++++AC+++++++++++++++++++++++++++++++++++++++
Myweek<- ts(No_NAtimeSeries1$AC, frequency = 52, start = (2007))
plot.ts(Myweek3)
plot(decompose(Myweek3))
plot(forecast(Myweek3, h=60))
autoplot(forecast(HoltWinters(Myweek3, alpha = 0.1, beta=0, gamma = 1)))
summary(Myweek3)

ggseasonplot(Myweek, col=rainbow(5), year.labels = TRUE,continuous=TRUE)+ theme_economist()+geom_line(size=1)

plot.ts(cbind(Myweek,Myweek1,Myweek2,Myweek3))+guides(color=FALSE)+ legend("topright", legend =c("Energy"), lty=1,col=2:4)

#++++++++++++++++++++++++++++++++MONTHSAC++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

No_NAtimeSeries2<- No_NAtimeSeries1%>%
  group_by(Years, Months)%>%
  filter(Years!=2006)%>%
  summarise(kitchen=mean(kitchen),
            laundry=mean(laundry),
            AC=mean(AC),
            Totalpower=mean(Totalpower))
View(No_NAtimeSeries2)

MyAC<-ts(No_NAtimeSeries2$AC, frequency = 12, start = (2007))
plot.ts(MyAC)
plot(decompose(MyAC))
plot(forecast(Myweek3, h=60))
autoplot(forecast(HoltWinters(MyAC, alpha = 0.1, beta = 0.1, gamma=1)))
summary(MyAC)
