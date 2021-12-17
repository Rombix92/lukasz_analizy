
#task 1


budget=200
rate = 0.20
price=1
number_f=0



while (budget > price) {
  budget=budget-price
  price=rate*price+price
  number_f=number_f+1
}
print(paste0('For 200$ I managed to buy ', number_f, ' foobars and get ', format(budget, digits=2, nsmall=2),'$ rest' ))

#__________________________________________________________________________________________
#task 2
install.packages("dplyr")
install.packages("mice")
library(mice)
library(dplyr)


setwd('C:\\Users\\Ĺukasz\\Desktop\\refinitve')
data=read.csv('UK Temperatures.csv')

#correlations coefficients indicates that temperatures from different stations are very strongly related
#the R^2 values of two exemplary models for Brice Norton and Herstmonceux are arround 95%
#that's why i decided to impute missing values with linear regression models predictions
cor(data[,-1], use="pairwise.complete.obs")
model1 = lm(Brice.Norton~Herstmonceux+Heathrow+Nottingham+Shawbury+Waddington, data=data)
model2 = lm(Herstmonceux ~Brice.Norton+Heathrow+Nottingham+Shawbury+Waddington, data=data)
summary(model1)
summary(model2)



data_impu<-data[,-1]
imp <- mice(data_impu, method = "norm.predict", m = 1)

completeData <- complete(imp,1)
completeData['Date'] <- data$X

#in order to calculate mean temperature for each day I needed to create column
#which consist of unique information for specific day
completeData['Date1']<-format(as.Date(completeData$Date, format= "%d/%m/%Y  %H:%M"), "%Y-%m-%d")

Mean_temp <- completeData %>% group_by(Date1) %>% 
summarize(Brice.Norton=mean(Brice.Norton),Herstmonceux=mean(Herstmonceux),Heathrow=mean(Heathrow),Nottingham=mean(Nottingham),Shawbury=mean(Shawbury),Waddington=mean(Waddington) )

Mean_temp <- Mean_temp %>% mutate(Day_mean_temp=0.14*Brice.Norton+0.10*Herstmonceux+0.30*Heathrow+0.13*Nottingham+0.20*Shawbury+0.13*Waddington)

#__________________________________________________________________________________________
#task 
#In the residential sector, natural gas is used almost exclusively for space heating, cooking and water boilers. 
#Residential gas demand therefore shows a strong seasonality and a dependence on cold weather.

#Attached are two data files: Temperatures South Carolina.csv 
#with daily average temperatures in degree Fahrenheit from 01/01/2013 to 03/31/2017 
#and Residential Demand South Carolina.csv with monthly residential gas demand in Billion cubic feet (Bcf) per month from January 2013 to December 2016.

#Based on these data, create a residential natural gas demand forecast for January-March 2017. 
#You get extra credit, if you can also provide us with an assessment of daily natural gas demand for each day during this period. 
#Give an explanation of your methodology and a discussion of potential problems. 



install.packages("dplyr")
install.packages('forecast')
install.packages('anytime')
install.packages('zoo')

library(dplyr)
library(anytime)
library(zoo)
library(forecast)

Sys.setlocale("LC_ALL", "English")


setwd('C:\\Users\\Łukasz\\Desktop\\refinitve')

#preparing data sets for further works, it is changing temperatures to celsius 
#and changing date format for more friendly one 
Demand=read.csv('Residential Demand South Carolina.csv', header=FALSE)

#how data looks like
#         V1   V2
#1 01-Jan-13 5003
#2 01-Feb-13 5216
#3 01-Mar-13 4751
#4 01-Apr-13 1586
#5 01-May-13  824
#6 01-Jun-13  497

Demand['Date']=format(as.Date(Demand$V1, format="%d-%b-%y"),"%y-%m")
colnames(Demand)<-c('Date1','Demand','Date')
Demand<-select(Demand, -Date1)

Temperatures=read.csv('Temperatures South Carolina.csv', header=FALSE)
colnames(Temperatures)=c('Date1','Fahrenheit')
Temperatures['Celsius']= (Temperatures['Fahrenheit']-32)*5/9
Temperatures['Date']=format(as.Date(Temperatures$Date, format="%d-%b-%y"), "%y-%m")

#Calculating mean month temperature, the same time perios as in demand dataset
Mean_Temperatures_month <- Temperatures %>% group_by(Date) %>% summarize(Mean_temp=mean(Celsius)) 

#merging both dataset: demand and temperature
Total=left_join(Mean_Temperatures_month, Demand, by="Date")

#in order to check how does relations between temperature and demand looks like scatter plot are created
Total_no_NA = Total[complete.cases(Total), ]
plot(Total_no_NA$Mean_temp, Total_no_NA$Demand)

#as we can see relation between temperature and demand is linear till 20C, then it start radically to flatten
# that why I decided to create two separate models, one for case where temp <20C and one for situation when temp >20C

Total_no_NA_O20 = Total_no_NA[Total_no_NA$Mean_temp>20, ]
plot(Total_no_NA_O20$Mean_temp, Total_no_NA_O20$Demand)
model1=lm(Demand~Mean_temp, data=Total_no_NA_O20)
summary(model1)


Total_no_NA_B20 = Total_no_NA[Total_no_NA$Mean_temp<20, ]
plot(Total_no_NA_B20$Mean_temp, Total_no_NA_B20$Demand)
model2=lm(Demand~Mean_temp, data=Total_no_NA_B20)
summary(model2)

# Day demand forecast regressed on temperature

#For temperatures below 20C:
#Intercept  9891.87     
#Mean_temp    -459.88

#For temperatures above 20C:
#(Intercept) 1466.524   
#Mean_temp -35.335

#Creating column where predicted demand values will be stored
Temperatures['Demand_pred']=NA

#calculating predicted values, basing on information about real temperatures observed within January-March 2017
for (i in seq(from=1462, to=1551)) {
  if (Temperatures['Celsius'][i,]>20) {
    Temperatures['Demand_pred'][i,]=1466.524-35.335*Temperatures$Celsius[i]
  }
  else {
    Temperatures['Demand_pred'][i,]=9891.87-459.88*Temperatures$Celsius[i]
  }
}


#Because there are only monthly information about demand, the time series analysis predicting month values can be created
#Month Forecast basing time series analysis

dem = Demand[,1]
demand_timeseries = ts(dem, frequency = 12, start=c(2013,1))
#below plot shows that phenomena is characterizing with seasonality component
plot.ts(demand_timeseries)
demand_timeseries_components <- decompose(demand_timeseries)
demand_timeseries_components$seasonal
plot(demand_timeseries_components)
demand_forecast <- HoltWinters(demand_timeseries)

#exctacted information about time series components allow for very well fitted model
plot(demand_forecast)
demand_forecast2 <- forecast(demand_forecast, h=3)

#below the monthly demand prediction based on time series analysis are presented
plot(demand_forecast2)
demand_forecast2$mean

#Now the prediction from time series analysis can be compared to prediction from linear regression model
#which was based on temperatures observed in analysed period of time January-March 2017
demand_pred <- Temperatures %>% group_by(Date) %>% summarise(demand_lm_pred=mean(Demand_pred))

demand_pred<-demand_pred[complete.cases(demand_pred),]
demand_pred['demand_time_series_pred']<-demand_forecast2$mean

#as it can be seen there are big discrepancies between two predictions, the reason for those differences is the fact that
#temperatures in January-March 2017 where much higher than in earlier years


#______________________________________________

tsData <- EuStockMarkets[, 1] # ts data
decomposedRes <- decompose(tsData, type="mult")
plot (decomposedRes) # see plot below



Temperatures['M_Y']=0
for (i in seq(1:nrow(Temperatures['Date']))){
  Temperatures['M_Y'][i,]<-substr(Temperatures['Date'][i,],
                                  nchar(as.character(Temperatures['Date'][i,]))-5,
                                  nchar(as.character(Temperatures['Date'][i,])))
}  




temp = Temperatures[,3]
temperature_timeseries = ts(temp, frequency = 365, start=c(2013,1))
plot.ts(temperature_timeseries)
temperature_timeseries_components <- decompose(temperature_timeseries)
temperature_timeseries_components$seasonal
plot(temperature_timeseries_components)
temperature_forecast <- HoltWinters(temperature_timeseries)
plot(temperature_forecast)
temperature_forecast2 <- forecast(temperature_forecast, h=90)
plot(temperature_forecast2)

