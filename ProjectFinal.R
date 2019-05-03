#remove all the objects stored
rm(list=ls(all = TRUE))

#Set the current working directory
setwd("F:/DS/Project 1")

#Confirm the current working directory
getwd()



#Load Libraries
x = c("ggplot2", "forecast", "tseries")

#install.packages(x)
lapply(x, require, character.only = TRUE)

rm(x)


#Load the Data
Bike_Data=read.csv("day.csv",header = TRUE,na.strings = c(" ","",NA))



class(Bike_Data)
#dim(Bike_Data)   
#str(Bike_Data)
summary(Bike_Data$cnt)



#Appropriate Conversions needed
Bike_Data$dteday=as.Date(Bike_Data$dteday)


for (i in 1:length(Bike_Data)){
  print(length(unique(Bike_Data[,i])))
  
}

for(i in c(3:9)){
  Bike_Data[,i]=as.factor(Bike_Data[,i])
}

for(i in c(1,14:16)){
  Bike_Data[,i]=as.numeric(Bike_Data[,i])
}
str(Bike_Data)



assign("Duplicate_Data",Bike_Data)

#plot the data
ggplot(Duplicate_Data, aes(dteday, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")



##################################Missing Values Analysis###############################

#Check for the requirement of Imputation


Missing_val=data.frame(apply(Bike_Data,2,function(x){sum(is.na(x))}))
colnames(Missing_val)="Percentage of Missing Values"
Missing_val[1]=((Missing_val[1]/(nrow(Bike_Data)))*100)
Missing_val$Variables=row.names(Missing_val)
row.names(Missing_val)=NULL
Missing_val=Missing_val[,c(2,1)]




#No need for impution- No missing Values

#Also no need to plot the missing values since its 0% for all the variables

#Data Manupulation; convert string categories into factor numeric for
#less memory utilization(No need)
#################### Data Exploration################################################
 
numeric_index=sapply(Bike_Data,is.numeric)


#Instant represents the serial number hence ignoring it
numeric_data=Bike_Data[,numeric_index][,-c(1,8)]

cnames = colnames(numeric_data)

count1=0
#See the distribution of data
for(i in cnames){
  count1=count1+1
  assign(paste0("Hn",count1),ggplot(Bike_Data, aes_string(x = Bike_Data[,i])) + 
           geom_histogram(fill="lightblue", colour = "lightblue",bins =300) + geom_density(alpha=.6, fill="pink") +
           geom_vline(xintercept=mean(Bike_Data[,i]),color="blue", linetype="dashed", size=1)+
           geom_vline(xintercept=median(Bike_Data[,i]),color="red", linetype="dashed", size=1)+
           scale_y_continuous(breaks=scales::pretty_breaks(n=5)) + 
           scale_x_continuous(breaks=scales::pretty_breaks(n=5))+
           theme_bw() + xlab(i) + ylab("Frequency") + ggtitle("Bike_Data: ",i) +
           theme(text=element_text(size=20))
  )
}


gridExtra::grid.arrange(Hn1,Hn2,Hn3,Hn4,Hn5,Hn6,ncol=3)


#All the variables are normally distributed

############################################Outlier Analysis#################################

#Convert it to a time series object
cnt_ts = ts(Bike_Data[, c('cnt')])


#clean the outliers present in the data
Bike_Data$clean_cnt = tsclean(cnt_ts)

#plot the Cleaned count
ggplot() +
  geom_line(data = Bike_Data, aes(x = dteday, y = clean_cnt)) + ylab('Cleaned Bike Count')

###################################Model Development for Time forecasting#######################################

#Check the stationarity of the data

Bike_Data$cnt_ma = ma(Bike_Data$clean_cnt, order=7) # using the clean count with no outliers
Bike_Data$cnt_ma30 = ma(Bike_Data$clean_cnt, order=30)


ggplot() +
  geom_line(data = Bike_Data, aes(x = dteday, y = clean_cnt, colour = "Counts")) +
  geom_line(data = Bike_Data, aes(x = dteday, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = Bike_Data, aes(x = dteday, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

#The data is not stationary since both mean. variance are changing for different windows



#Decompose the data to check the various components seasonal,trend and random componenet present
count_ma = ts(na.omit(Bike_Data$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)




#Augmented Dickey-Fuller (ADF) test
adf.test(count_ma, alternative = "stationary")

#####check the ACF anf PACF for stationarity

Acf(count_ma, main='')

Pacf(count_ma, main='')

#Use an Integration factor 1

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")



#plotting the ACF and DCF
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')



############ARIMA model################

auto.arima(deseasonal_cnt, seasonal=FALSE)
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)

tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')



#Lets try with other parameters since the residuals are showing correlation

fit2 = arima(deseasonal_cnt, order=c(1,1,7))

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

fcast <- forecast(fit2, h=30)
plot(fcast)


#Evaluating using a hold window
hold <- window(ts(deseasonal_cnt), start=700)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))


#Including seasonality in data
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality



seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)

tsdisplay(residuals(fit_w_seasonality), lag.max=15, main='Seasonal Model Residuals')
