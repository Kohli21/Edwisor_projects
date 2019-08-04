#Clean the environment

rm(list = ls())

#Seting the  working directory
setwd("D:/gaggi")


#get Working directory
getwd()

#Loading the librarires which would be needed
libraries = c("dummies","caret","rpart.plot","plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","DataCombine")
lapply(X = libraries,require, character.only = TRUE)


rm(libraries)

#Read the csv file
cab_data = read.csv("train_cab.csv", header = T)
cab_data_test = read.csv("test.csv", header = T)

# lets start our general analysis
str(cab_data)
summary(cab_data)
head(cab_data)

#from above analysis we can draw few insights about data.
#there are missing values in the data
#max fare "54343" and max pessenger"5345" sounds absard for a cab
#continuing further analysis for test data as well.

str(cab_data_test)
summary(cab_data_test)

#data types for test daya seems apt
#test data looks okay at 1st glance no missing values no visable outliers as of now
#feature enginnering
#let us first covert all variables in the train data in right data type

cab_data$pickup_datetime=gsub("UTC","", cab_data$pickup_datetime)
cab_data_test$pickup_datetime=gsub("UTC","", cab_data_test$pickup_datetime)
new_date=cab_data$pickup_datetime
new_date_test=cab_data_test$pickup_datetime
new_date=strptime(new_date,"%Y-%m-%d%H:%M:%S")
new_date_test=strptime(new_date_test,"%Y-%m-%d%H:%M:%S")
cab_data$pickup_datetime=new_date
cab_data_test$pickup_datetime=new_date_test
cab_data$fare_amount=as.numeric(as.character(cab_data$fare_amount))
cab_data$passenger_count=as.integer(cab_data$passenger_count)
str(cab_data)
str(cab_data_test)
summary(cab_data)

#UNIVARIAT analysis
#lets start with passenger_count as observed it consists of outliers.
# from common understanding a cab cannot have max passanger count more than 6.
#we should consider max passanger count as 6
#also 0 passanger also sounds absard.

cab_data$passenger_count[cab_data$passenger_count < 0 | cab_data$passenger_count >6]=NaN
cab_data$fare_amount[cab_data$fare_amount < 2.5 | cab_data$fare_amount >300]=NaN

# doing some research we are considering the max fare as $300 and min fare to be $2.5
#we could treat the outliers in 2 posible ways 11 droping them completly along with whole row 
#second we can convert them into NA values and treat them as missing values. that is populating them baised on data left
#we are using second way as it results in less loss of data
#there are already missing values present int passenger_count column thus we will be deaaling with all missing values at once.

summary(cab_data)

#let us move our focus on latitude and longitude data
#data seems curropt as it has many 00 cordinates that actualy lie in sea not posible for cab to drop or pickup
#also it seems that lat and long data for few observations are interchanged
#lets find the the min max rnge for latitude and longitude from test data as it is almost free from any outliers
#max and min longitude from test data

lon_min=min(min(cab_data_test$pickup_longitude),min(cab_data_test$dropoff_longitude))
lon_max=max(max(cab_data_test$pickup_longitude),max(cab_data_test$dropoff_longitude))
print(lon_min)
print(lon_max)

#max and min longitude from test data

lat_min=min(min(cab_data_test$pickup_latitude),min(cab_data_test$dropoff_latitude))
lat_max=max(max(cab_data_test$pickup_latitude),max(cab_data_test$dropoff_latitude))
print(lat_min)
print(lat_max)

#let us find outliers on bases of this range

BB = c(-74.5, -72.8, 40.5, 41.8)
latlon_outliers = cab_data[which((cab_data$pickup_longitude < -74.5)|(cab_data$pickup_longitude > -72.8)|(cab_data$pickup_latitude < 40.5)|(cab_data$pickup_latitude > BB[4])|(cab_data$dropoff_longitude < BB[1])|(cab_data$dropoff_longitude > BB[2])|(cab_data$dropoff_latitude < BB[3])|(cab_data$dropoff_latitude > BB[4])),]
summary(latlon_outliers)
str(latlon_outliers)
head(latlon_outliers)

#lets first deal with zeros in lat and long data
#we are deleting all zero as the zero cordinate lies in ocean thats absard in tiself for a cab to travel

latlon_outliers=latlon_outliers[!(latlon_outliers$pickup_longitude==0 | latlon_outliers$dropoff_latitude==0 | latlon_outliers$dropoff_longitude==0 | latlon_outliers$dropoff_latitude==0),]
str(latlon_outliers)

#as we can see many rows have values inverted  for latitude and longitude thesr data rows can be usefull if we could fix this

latlon_outliers = latlon_outliers[which((latlon_outliers$pickup_longitude >=40.5)&(latlon_outliers$pickup_longitude <=41.8)&(latlon_outliers$pickup_latitude >=-74.5)&(latlon_outliers$pickup_latitude <=-72.8)&(latlon_outliers$dropoff_longitude >=40.5)&(latlon_outliers$dropoff_longitude <=41.8)&(latlon_outliers$dropoff_latitude >=-74.5)&(latlon_outliers$dropoff_latitude <=-72.8)),]
str(latlon_outliers)
head(latlon_outliers)
setnames(latlon_outliers, old=c("pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude"), new=c("pickup_latitude", "pickup_longitude","dropoff_latitude","dropoff_longitude"))
head(latlon_outliers)
colnames(latlon_outliers)
df=latlon_outliers[,c(1,2,4,3,6,5,7)]
head(df)

## Now we'll remove all rows with a datapoint that doesn't fall within the bounding box 

cab_data = cab_data[!((cab_data$pickup_longitude >=BB[1])&(cab_data$pickup_longitude <=BB[2])&(cab_data$pickup_latitude >= BB[3])&(cab_data$pickup_latitude <= BB[4])&(cab_data$dropoff_longitude >= BB[1])&(cab_data$dropoff_longitude <= BB[2])&(cab_data$dropoff_latitude >= BB[3])&(cab_data$dropoff_latitude <= BB[4])),]
str(cab_data)
cab_data=cab_data[!(cab_data$pickup_longitude==0 | cab_data$dropoff_latitude==0 | cab_data$dropoff_longitude==0 | cab_data$dropoff_latitude==0),]
str(cab_data)

#no we will concate the filterd data with this data

cab_data_copy=copy(cab_data)
cab_data=rbind(cab_data,df)
rm(cab_data_copy)
summary(cab_data)

#after dealing with latitude longitude data lts first trat missing values in the pessanger and fare variables

missing_val = data.frame(apply(cab_data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(cab_data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val
cab_data=subset(cab_data,!is.na(cab_data$pickup_datetime))

#as we can see there are about 0.4  0.2 percent missing values in fare and passanger count respectively
##Create missing value and impute using mean, median and knn

df1 =cab_data
df2= cab_data
df1[10,1]

# here the value we have choosen to remove is 8.9

df1[10,1]=NA
df2[10,1]=NA

# checking for different values
#mean

df1[10,1] = mean(df1$fare_amount, na.rm = T)
df1[10,1] #the value we got is 11.3

#median

df2[10,1] = median(df2$fare_amount, na.rm = T)
df2[10,1] #the value we got is 8.5

#thus imputing with median for whole data
#Median Method

cab_data$fare_amount[is.na(cab_data$fare_amount)] = median(cab_data$fare_amount, na.rm = T)
cab_data$passenger_count[is.na(cab_data$passenger_count)] = median(cab_data$passenger_count, na.rm = T)

#Check if any missing values

sum(is.na(cab_data))

#this concludes 1st phase of data cleaning
#lets start ourEDA for the data
#lets stars by converting pickup date timestamp to different other usefull columns
#also distance column using latitude longitude data

str(cab_data_test)
library(lubridate)
new_date=cab_data$pickup_datetime
new_date=strptime(new_date,"%Y-%m-%d%H:%M:%S")
cab_data$pickup_datetime=new_date
cab_data$hour = hour(cab_data$pickup_datetime)
cab_data$day_of_week = weekdays (cab_data$pickup_datetime)
cab_data$day_of_month = day(cab_data$pickup_datetime)
cab_data$week = week(cab_data$pickup_datetime)
cab_data$month= month(cab_data$pickup_datetime)
cab_data$year = year(cab_data$pickup_datetime)-2000

#doing the same for test data

cab_data_test$hour = hour(cab_data_test$pickup_datetime)
cab_data_test$day_of_week = weekdays (cab_data_test$pickup_datetime)
cab_data_test$day_of_month = day(cab_data_test$pickup_datetime)
cab_data_test$week = week(cab_data_test$pickup_datetime)
cab_data_test$month= month(cab_data_test$pickup_datetime)
cab_data_test$year = year(cab_data_test$pickup_datetime)-2000

#creating distance column with latitude and longitude data

library(NISTunits)
  R = 6371  #radius of earth in kilometers
#R = 3959 #radius of earth in miles
phi1 =NISTdegTOradian(cab_data$pickup_latitude)
phi2 = NISTdegTOradian(cab_data$dropoff_latitude)
phi3=NISTdegTOradian(cab_data$pickup_longitude)
phi4= NISTdegTOradian(cab_data$dropoff_longitude) 
delta_phi = phi2-phi1
delta_lambda = phi4-phi3
#a = sin²((fB - fA)/2) + cos fA . cos fB . sin²((??B - ??A)/2)
a = sin(delta_phi / 2.0) ** 2 + cos(phi1) * cos(phi2) * sin(delta_lambda / 2.0) ** 2

#c = 2 * atan2( va, v(1-a) )
c = 2 * atan2(sqrt(a), sqrt(1-a))

#d = R*c
d = (R * c) #in kilometers

cab_data$H_Distance=d

#doing the same for test data

Phi1 =NISTdegTOradian(cab_data_test$pickup_latitude)
Phi2 = NISTdegTOradian(cab_data_test$dropoff_latitude)
Phi3=NISTdegTOradian(cab_data_test$pickup_longitude)
Phi4= NISTdegTOradian(cab_data_test$dropoff_longitude) 
Delta_phi = Phi2-Phi1
Delta_lambda = Phi4-Phi3
#a = sin²((fB - fA)/2) + cos fA . cos fB . sin²((??B - ??A)/2)
A = sin(Delta_phi / 2.0) ** 2 + cos(Phi1) * cos(Phi2) * sin(Delta_lambda / 2.0) ** 2

#c = 2 * atan2( va, v(1-a) )
C = 2 * atan2(sqrt(A), sqrt(1-A))

#d = R*c
D = (R * C) #in kilometers
cab_data_test$H_Distance=D
summary(cab_data)
summary(cab_data_test)

#as we can see there are observations that have zero distance
#as we donot know direct relationship of fare with distance we need to clean aur data of these 0 distances
cab_data$H_Distance[cab_data$H_Distance <= 0 | cab_data$H_Distance >100]=NaN
sum(is.na(cab_data))
cab_data$H_Distance[is.na(cab_data$H_Distance)] = median(cab_data$H_Distance, na.rm = T)

#let us start aalysis between the variables
#Does the number of passengers affect the fare?
#Check the distribution of numerical data using histogram

hist1 = ggplot(data = cab_data, aes(x =cab_data$fare_amount)) + ggtitle("Distribution of : fare") + geom_histogram(bins = 100)
hist2 = ggplot(data = cab_data, aes(x =cab_data$passenger_count)) + ggtitle("Distribution of: passenger count") + geom_histogram(bins = 100)
hist3 = ggplot(data = cab_data, aes(x =cab_data$hour)) + ggtitle("Distribution of: hour") + geom_histogram(bins = 100)
#hist4 = ggplot(data = cab_data, aes(x =cab_data$day_of_week)) + ggtitle("Distribution of :day of week") + geom_histogram(bins = 25)
hist4 = ggplot(data = cab_data, aes(x =cab_data$day_of_month)) + ggtitle("Distribution of :day of month") + geom_histogram(bins = 100)
hist5 = ggplot(data = cab_data, aes(x =cab_data$week)) + ggtitle("Distribution of : week") + geom_histogram(bins = 100)
hist6 = ggplot(data = cab_data, aes(x =cab_data$month)) + ggtitle("Distribution of : month") + geom_histogram(bins = 100)
hist7 = ggplot(data = cab_data, aes(x =cab_data$year)) + ggtitle("Distribution of : year") + geom_histogram(bins = 100)
hist8 = ggplot(data = cab_data, aes(x =cab_data$H_Distance)) + ggtitle("Distribution of : distance") + geom_histogram(bins = 100)
bar1 = ggplot(data = cab_data, aes(x = cab_data$day_of_week)) + geom_bar() + ggtitle("day of week") + theme_dark()
#making a grid

gridExtra::grid.arrange(hist1,hist2,ncol=1)
gridExtra::grid.arrange(hist3,hist4,ncol=1)
gridExtra::grid.arrange(hist5,hist6,ncol=1)
gridExtra::grid.arrange(hist7,hist8,ncol=1)
gridExtra::grid.arrange(bar1,ncol=1)

#fare and pessanger
ggplot(cab_data, aes(x= passenger_count,y=fare_amount)) +
  geom_point()

#From the above 2 graphs we can see that single passengers are the most frequent travellers, and the highest fare also seems to come from cabs which carry just 1 passenger.
ggplot(cab_data, aes(x= day_of_month,y=fare_amount)) +
  geom_point()

#The fares throught the month mostly seem uniform, with the maximum fare received on the 16th
ggplot(cab_data, aes(x= hour,y=fare_amount)) +
  geom_point()

#Interesting! The time of day definitely plays an important role. The frequency of cab rides seem to be the lowest at 5AM and the highest at 7PM
#The fares, however, seem to be high betweeb 5AM and 10AM, and 2PM to 4PM. Maybe people who live far away prefer to leave earlier to avoid rush hour traffic?

ggplot(cab_data, aes(x= day_of_week,y=fare_amount)) +
  geom_point()

#The highest fares seem to be high weekdays  on  and the lowest on  and sunday almost an uniform distibution
#distace should have a direct relationship with fare
ggplot(cab_data, aes(x= H_Distance,y=fare_amount)) +
  geom_point()
summary(cab_data)

#feature selection
## Correlation Plot 

numeric_index = sapply(cab_data,is.numeric) #selecting only numeric
numeric_index_test = sapply(cab_data_test,is.numeric) #selecting only numeri

## Correlation Plot 

corrgram(cab_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Correlation Plot 

corrgram(cab_data_test[,numeric_index_test], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#as seen day of month anad day of week have very less co relation with targit variable thus we will drop both
#also date is being droped as all its features have been extracted already
#  dimensional  reduction

cab_data = subset(cab_data,select=-c(pickup_datetime,day_of_month,day_of_week))
cab_data_test = subset(cab_data_test,select=-c(pickup_datetime,day_of_month,day_of_week))
###################################Model Development#######################################
head(cab_data)
########DECISION TREE


#Splitting the  data (90-10 percent)
set.seed(1)
train_index = sample(1:nrow(cab_data), 0.9*nrow(cab_data))        
train = cab_data[train_index,]
test = cab_data[-train_index,]


#Build decsion tree using rpart
dt_model = rpart(fare_amount ~ ., data = train, method = "anova")
# here we can try any method other than anova ,
#one of "anova", "poisson", "class" or "exp".
#If method is missing then the routine tries to make an intelligent guess. 

#Ploting the tree
rpart.plot(dt_model)


#Perdict for test cases 

dt_predictions = predict(dt_model, test[,-1])

df3= data.frame((dt_predictions))

#Create data frame for actual and predicted values
df_pred = data.frame("actual"= test[,1], "dt_pred"=dt_predictions)
head(df_pred)
# analyse relationship between actual and predicted count
ggplot(df_pred, aes(x= actual ,y=dt_predictions)) +
  geom_point()+
  geom_smooth()

############# Evaluate  Decision tree ###################


#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,1], dt_predictions)

#Error Rate: 0.24789
#Accuracy: 76.77%

#Evaluate  Model using RMSE

RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
  
}


RMSE(test[,1], dt_predictions)

#RMSE = 4.36

########RANDOM FOREST


#Training the model using training data
rf_model = randomForest(fare_amount~., data = train, ntree = 200)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-1])

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)
# analyse relationship between actual and predicted count
ggplot(df_pred, aes(x= actual ,y=rf_predictions)) +
  geom_point()+
  geom_smooth()
############# Evaluate  ranom forest ###################


#MAPE
#calculate MAPE
MAPE(test[,1], rf_predictions)

#0.1925
#81.911% acuracy
RMSE(test[,1], rf_predictions)

#RMSE = 293.857

#################### Develop  Linear Regression Model ##########################

#check multicollearity
library(usdm)

vif(cab_data[,-1])

vifcor(cab_data[,-1], th = 0.9)

# develop Linear Regression  model
#dividind data into test and train
train_index = sample(1:nrow(cab_data), 0.9 * nrow(cab_data))
train_lr = cab_data[train_index,]
test_lr = cab_data[-train_index,]
train_lr = subset(train_lr,select=-c(pickup_longitude ,pickup_latitude ,dropoff_latitude ,week))

#run regression model
lm_model = lm(fare_amount ~., data = train_lr)

#Summary of the model
summary(lm_model)


# observe the  residuals and   coefficients  of the linear regression model
# Predict  the Test data 


#Predict
lm_predictions = predict(lm_model, test_lr[,-1])
#Creating a new dataframe for actual and predicted values
df_pred = cbind(df_pred,lm_predictions)
head(df_pred)

# analyse relationship between actual and predicted count
ggplot(df_pred, aes(x= actual ,y=lm_predictions)) +
  geom_point()+
  geom_smooth()

# Evaluate Linear Regression Model




MAPE(test_lr[,1], lm_predictions)

#Error Rate: 0.03757554
#Accuracy: 96.3%

RMSE(test_lr[,1], lm_predictions)

#RMSE = 2.327632e-12
#random forrest will be best fit
#for test data lets predict fare
X_test=cab_data_test
RF_fare_ammount=predict(rf_model,X_test)
cab_data_test$fare_predicted=RF_fare_ammount
#writinf the file in csv
write.csv(cab_data_test, file = 'output_cab_R .csv', row.names = FALSE, quote=FALSE)
ggplot(cab_data_test, aes(x= H_Distance,y=fare_predicted)) +
  geom_point()
summary(cab_data)