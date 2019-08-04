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
bike_data = read.csv("day.csv", header = T)

######Exploratory Data Analysis##################
# Summarizing  data 
colnames(bike_data)
#Verify first five rows of data
head(bike_data)
#target variable is 'cnt' and other variables are independent  variable(or predictors)
summary(bike_data$cnt)

#Verify  summary of data
summary(bike_data)

#It  shows  variables like 'mnth',holiday','weekday','weathersit' are 
#catogical variabless  and already encoded

#Nummeric  vaiables like 'temp','atem','hum','windspeed' are 
#standardized form

# data  contains  no  missing  values 
# Outliers might be present in variables 'actual','registered','cnt'
#instant variable is jus an aditional index for the data so droping the same
bike_data=subset(bike_data, select=-c(instant))
#structure of  data
str(bike_data)

##################################################converting in proper data type#################################################


bike_data$season=as.factor(bike_data$season)
bike_data$mnth=as.factor(bike_data$mnth)
bike_data$yr=as.factor(bike_data$yr)
bike_data$holiday=as.factor(bike_data$holiday)
bike_data$weekday=as.factor(bike_data$weekday)
bike_data$workingday=as.factor(bike_data$workingday)
bike_data$weathersit=as.factor(bike_data$weathersit)
bike_data$dteday=as.Date.factor(bike_data$dteday)
bike_data$temp=as.numeric(bike_data$temp)
bike_data$atemp=as.numeric(bike_data$atemp)
bike_data$hum=as.numeric(bike_data$hum)
bike_data$windspeed=as.numeric(bike_data$windspeed)
bike_data$casual=as.numeric(bike_data$casual)
bike_data$registered=as.numeric(bike_data$registered)
bike_data$cnt=as.numeric(bike_data$cnt)
str(bike_data)

###############################Missing Values Analysis###############################################


#checking for missing value
missing_val = data.frame(apply(bike_data,2,function(x){sum(is.na(x))}))
head(missing_val,16)
#as seen earliear also there are no missing values in the data thus we need not perform missing value analysis

# Analyze variables  by visualize


#Distribution of factor data using bar plot
bar1 = ggplot(data = bike_data, aes(x = bike_data$season)) + geom_bar() + ggtitle("season") + theme_dark()
bar2 = ggplot(data = bike_data, aes(x = bike_data$yr)) + geom_bar() +  ggtitle("year") + theme_dark()
bar3 = ggplot(data = bike_data, aes(x = bike_data$mnth)) + geom_bar() + ggtitle("Month") + theme_dark()
bar4 = ggplot(data = bike_data, aes(x = bike_data$holiday)) + geom_bar() + ggtitle("holiday") + theme_dark()
bar5 = ggplot(data = bike_data, aes(x = bike_data$workingday)) + geom_bar() + ggtitle("working day") + theme_dark()
bar6 = ggplot(data = bike_data, aes(x = bike_data$weathersit)) + geom_bar() + ggtitle("weather situation") + theme_dark()


#making a grid
gridExtra::grid.arrange(bar1,bar2,ncol=1)
gridExtra::grid.arrange(bar3,bar4,ncol=1)
gridExtra::grid.arrange(bar5,bar6,ncol=2)

#Check the distribution of numerical data using histogram

hist1 = ggplot(data = bike_data, aes(x =bike_data$cnt)) + ggtitle("Distribution of : count of bike users") + geom_histogram(bins = 25)
hist2 = ggplot(data = bike_data, aes(x =bike_data$registered)) + ggtitle("Distribution of: registered user") + geom_histogram(bins = 25)
hist3 = ggplot(data = bike_data, aes(x =bike_data$casual)) + ggtitle("Distribution of: casual user") + geom_histogram(bins = 25)
hist4 = ggplot(data = bike_data, aes(x =bike_data$windspeed)) + ggtitle("Distribution of : windspeed") + geom_histogram(bins = 25)
hist5 = ggplot(data = bike_data, aes(x =bike_data$hum)) + ggtitle("Distribution of : humidity") + geom_histogram(bins = 25)
hist6 = ggplot(data = bike_data, aes(x =bike_data$atemp)) + ggtitle("Distribution of : temprature felt") + geom_histogram(bins = 25)
hist7 = ggplot(data = bike_data, aes(x =bike_data$temp)) + ggtitle("Distribution of : temperature") + geom_histogram(bins = 25)
#making a grid

gridExtra::grid.arrange(hist1,hist2,ncol=1)
gridExtra::grid.arrange(hist3,hist4,ncol=1)
gridExtra::grid.arrange(hist5,hist6,hist7,ncol=2)

# *****************bivariate  relationship between numeric variables****************************

#check the relationship between 'temp' and 'atemp' variable

ggplot(bike_data, aes(x= temp,y=atemp)) +
  geom_point()+
  geom_smooth()

#This  graph is saying that very strong relationship  between 'temp' and 'atemp'

#check the relationship between 'temp' and 'hum' variable

ggplot(bike_data, aes(x= temp,y=hum)) +
  geom_point()+
  geom_smooth()

# here  it is showing  Humidity is increses  till temparature is 0.7 and it is decreasing  gradually

#check the relationship between 'temp' and 'windspeed' variable

ggplot(bike_data, aes(x= temp,y=windspeed)) +
  geom_point()+
  geom_smooth()

# it is showing that very less nagative   correlation between  temp and windspeed

#check the relationship between all numeric variable using pair plot

ggpairs(bike_data[,c('atemp','temp','hum','windspeed','cnt')])

# that above plot stating that less  nagative relationship between
# 'cnt'-'hum'  and cnt-windspeed

# and there is strong positive relationship between 
# temp- cnt and  atemp-cnt

# *************visualize the relationship between categorical variable***************

#check relationship between  season and holiday
var1= table(bike_data$season,bike_data$holiday)

var1

barplot(var1)
# here contgency table showing  holiday=0  is same for almost all the seasons

#check relationship between  season and weekday

var2= table(bike_data$season,bike_data$weekday)

barplot(var2)

#check relationship between  season and weathersit

var3= table(bike_data$weathersit,bike_data$season)
var3

prop.table(var3,2)

barplot(var3)

#It is stating that in all the season  whether 1 type is large numbers

##check relationship between  holiday and weathersit



var4= table(bike_data$weathersit,bike_data$holiday)
var4

barplot(var4)
#to check in proportion

prop.table(var4,2)

# it it staing that holiday type '0' and  weathersit type '1' almost covered 0.63%

###### Outlier Analysis########################################################

#####OUTLIER ANALYSIS
# here we will replace the outliers with Knn method.
#Get the data with only numeric columns
numeric_index = sapply(bike_data, is.numeric)
numeric_data = bike_data[,numeric_index]

#Get the data with only factor columns
factor_data = bike_data[,!numeric_index]

# analyse relationship between causal and cnt variables before  outlier treatment
ggplot(bike_data, aes(x= casual ,y=cnt)) +
  geom_point()+
  geom_smooth()


#Check for outliers using boxplots
for(i in 1:ncol(numeric_data)) {
  
  assign(paste0("box",i), ggplot(data = bike_data, aes_string(y = numeric_data[,i]))
         +stat_boxplot(geom = "errorbar", width = 1)
         +geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) 
         +labs(y = colnames(numeric_data[i]))
         +ggtitle(paste("Boxplot: ",colnames(numeric_data[i]))))
}

#Arrange the plots in grids
gridExtra::grid.arrange(box1,box2,ncol=2)
gridExtra::grid.arrange(box3,box4,ncol=2)
gridExtra::grid.arrange(box5,box6,ncol=2)
gridExtra::grid.arrange(box7,ncol=2)

#Get the names of numeric columns
numeric_columns = colnames(numeric_data)

#Replacing all outlier data with NA
for(i in numeric_columns){
  val = bike_data[,i][bike_data[,i] %in% boxplot.stats(bike_data[,i])$out]
  print(paste(i,length(val)))
  bike_data[,i][bike_data[,i] %in% val] = NA
}

#Check number of missing values
sapply(bike_data,function(x){sum(is.na(x))})

#Get number of missing values after replacing outliers as NA
missing_values_out = data.frame(sapply(bike_data,function(x){sum(is.na(x))}))
missing_values_out$Columns = row.names(missing_values_out)
row.names(missing_values_out) = NULL
names(missing_values_out)[1] = "Missing_percentage"
missing_values_out$Missing_percentage = ((missing_values_out$Missing_percentage/nrow(bike_data)) *100)
missing_values_out = missing_values_out[,c(2,1)]
missing_values_out = missing_values_out[order(-missing_values_out$Missing_percentage),]
missing_values_out

#Compute the NA values using KNN imputation
bike_data[,9:15] = knnImputation(bike_data[ ,9:15], k = 3)

#Check if any missing values
sum(is.na(bike_data))
#Check for outliers using boxplots
for(j in 1:ncol(numeric_data)) {
  
  assign(paste0("box",j), ggplot(data = bike_data, aes_string(y = numeric_data[,j]))
         +stat_boxplot(geom = "errorbar", width = 1)
         +geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) 
         +labs(y = colnames(numeric_data[i]))
         +ggtitle(paste("Boxplot: ",colnames(numeric_data[j]))))
}

#Arrange the plots in grids
gridExtra::grid.arrange(boxj,ncol=1)
gridExtra::grid.arrange(box3,box4,ncol=2)
gridExtra::grid.arrange(box5,box6,ncol=2)
gridExtra::grid.arrange(box7,ncol=2)
# analyse relationship between causal and cnt variables before  outlier treatment
ggplot(bike_data, aes(x= casual ,y=cnt)) +
  geom_point()+
  geom_smooth()
##################################Feature Selection################################################
## Correlation Plot 
corrgram(bike_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


# correlation matrix  stating  'temp' and 'atemp' having strong relationship
# and there is no  relationship between 'hum' and 'cnt'

#  dimensional  reduction

bike_data = subset(bike_data,select=-c(atemp,hum))
##################################Feature Scaling################################################
#Normalisation
cnames = c("casual","registered")

for(i in cnames){
  print(i)
  bike_data[,i] = (bike_data[,i] - min(bike_data[,i]))/
    (max(bike_data[,i] - min(bike_data[,i])))
}

###################################Model Development#######################################
head(bike_data)
########DECISION TREE


#Splitting the  data (80-20 percent)
set.seed(1)
train_index = sample(1:nrow(bike_data), 0.8*nrow(bike_data))        
train = bike_data[train_index,]
test = bike_data[-train_index,]


#Build decsion tree using rpart
dt_model = rpart(cnt ~ ., data = train, method = "anova")
# here we can try any method other than anova ,
#one of "anova", "poisson", "class" or "exp".
#If method is missing then the routine tries to make an intelligent guess. 

#Ploting the tree
rpart.plot(dt_model)


#Perdict for test cases 

dt_predictions = predict(dt_model, test[,-13])

df3= data.frame((dt_predictions))

#Create data frame for actual and predicted values
df_pred = data.frame("actual"= test[,13], "dt_pred"=dt_predictions)
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

MAPE(test[,13], dt_predictions)

#Error Rate: 0.1523074
#Accuracy: 84.77%

#Evaluate  Model using RMSE

RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
  
}


RMSE(test[,13], dt_predictions)

#RMSE = 733.1856

########RANDOM FOREST


#Training the model using training data
rf_model = randomForest(cnt~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-13])

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
MAPE(test[,13], rf_predictions)

#0.06089973
#93.911% acuracy
RMSE(test[,13], rf_predictions)

#RMSE = 293.857

#################### Develop  Linear Regression Model ##########################

#check multicollearity
library(usdm)
#converting multilevel categorical variable into ineger again
bike_data$season=as.integer(bike_data$season)
bike_data$mnth=as.integer(bike_data$mnth)
bike_data$yr=as.integer(bike_data$yr)
bike_data$holiday=as.integer(bike_data$holiday)
bike_data$weekday=as.integer(bike_data$weekday)
bike_data$workingday=as.integer(bike_data$workingday)
bike_data$weathersit=as.integer(bike_data$weathersit)

vif(bike_data[,2:12])

vifcor(bike_data[,2:12], th = 0.9)

# develop Linear Regression  model
#dividind data into test and train
train_index = sample(1:nrow(bike_data), 0.8 * nrow(bike_data))
train_lr = bike_data[train_index,]
test_lr = bike_data[-train_index,]
#run regression model
lm_model = lm(cnt ~., data = train_lr)

#Summary of the model
summary(lm_model)


# observe the  residuals and   coefficients  of the linear regression model
# Predict  the Test data 


#Predict
lm_predictions = predict(lm_model, test_lr[,-13])
#Creating a new dataframe for actual and predicted values
df_pred = cbind(df_pred,lm_predictions)
head(df_pred)

# analyse relationship between actual and predicted count
ggplot(df_pred, aes(x= actual ,y=lm_predictions)) +
  geom_point()+
  geom_smooth()

# Evaluate Linear Regression Model




MAPE(test_lr[,13], lm_predictions)

#Error Rate: 0.03757554
#Accuracy: 96.3%

RMSE(test_lr[,13], lm_predictions)

#RMSE = 2.327632e-12
write.csv(df_pred, file = ' output R .csv', row.names = FALSE, quote=FALSE)

