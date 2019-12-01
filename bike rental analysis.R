 #Cleaning environment
rm(list = ls())

 #Setting work directory
setwd("C:/Users/aditya joshi/Edwisor/Bike Rental Analysis")

#Loading librarires
libraries = c("plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Read the csv file
data = read.csv(file = "day.csv", header = T, sep = ",", na.strings = c(" ", "", "NA"))

########################################EXPLORING THE DATA########################################
#First few rows
head(data)

#data Dimensions 
dim(data)

#names of cloumns
names(data)

#Structures of variables
str(data)

########################################FEATURE ENGINEERING########################################
#Creating columns
data$actual_temp <- data$temp*39
data$actual_feel_temp <- data$atemp*50
data$actual_windspeed <- data$windspeed*67
data$actual_hum = data$hum * 100

data$actual_season = factor(x = data$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
data$actual_yr = factor(x = data$yr, levels = c(0,1), labels = c("2011","2012"))
data$actual_holiday = factor(x = data$holiday, levels = c(0,1), labels = c("Working day","Holiday"))
data$actual_weathersit = factor(x = data$weathersit, levels = c(1,2,3,4), 
                               labels = c("Clear","Cloudy/Mist","Rain/Snow/Fog","Heavy Rain/Snow/Fog"))

data$weathersit = as.factor(data$weathersit)
data$season = as.factor(data$season)
data$dteday = as.character(data$dteday)
data$mnth = as.factor(data$mnth)
data$weekday = as.factor(as.character(data$weekday))
data$workingday = as.factor(as.character(data$workingday))
data$yr = as.factor(data$yr)
data$holiday = as.factor(data$holiday)

########################################MISSING VALUES########################################
missing_values = sapply(data, function(x){sum(is.na(x))})


########################################EXPLORE USING GRAPHS########################################
#Checking distribution of categorical Data using bar graph
graph1 = ggplot(data = data, aes(x = actual_season)) + geom_bar() + ggtitle("Count of Season")
graph2 = ggplot(data = data, aes(x = actual_weathersit)) + geom_bar() + ggtitle("Count of Weather")
graph3 = ggplot(data = data, aes(x = actual_holiday)) + geom_bar() + ggtitle("Count of Holiday")
graph4 = ggplot(data = data, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")
# ## Plot plots together
gridExtra::grid.arrange(graph1,graph2,graph3,graph4,ncol=2)

#Checking distribution of numerical data using histogram
gram1 = ggplot(data = data, aes(x =actual_temp)) + ggtitle("Distribution of Temperature") + geom_histogram(bins = 25)
gram2 = ggplot(data = data, aes(x =actual_hum)) + ggtitle("Distribution of Humidity") + geom_histogram(bins = 25)
gram3 = ggplot(data = data, aes(x =actual_feel_temp)) + ggtitle("Distribution of Feel Temperature") + geom_histogram(bins = 25)
gram4 = ggplot(data = data, aes(x =actual_windspeed)) + ggtitle("Distribution of Windspeed") + geom_histogram(bins = 25)
# ## Plot plots together
gridExtra::grid.arrange(gram1,gram2,gram3,gram4,ncol=2)

#Checking distribution of numerical data using scatterplot
scan1 = ggplot(data = data, aes(x =actual_temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
scan2 = ggplot(data = data, aes(x =actual_hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike COunt")
scan3 = ggplot(data = data, aes(x =actual_feel_temp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike COunt")
scan4 = ggplot(data = data, aes(x =actual_windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike COunt")
# ## Plot plots together
gridExtra::grid.arrange(scan,scan2,scan3,scan4,ncol=2)

#Checking outliers in data using boxplot
columnnames = colnames(data[,c("actual_temp","actual_feel_temp","actual_windspeed","actual_hum")])
for (i in 1:length(columnnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = columnnames[i]), data = data)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=columnnames[i])+
           ggtitle(paste("Box plot for",columnnames[i])))
}
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)

#Removing outliers in Windspeed
val = data[,19][data[,19] %in% boxplot.stats(data[,19])$out]
data = data[which(!data[,19] %in% val),]

#Checking multicollinearity using VIF
df1 = data[,c("instant","temp","atemp","hum","windspeed")]
vifcor(df1)

#Checking for collinearity using corelation graph
corrgram(data, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Removing unwanted variables

data <- subset(data, select = -c(holiday,instant,dteday,atemp,casual,registered,actual_temp,actual_feel_temp,actual_windspeed,
                               actual_hum,actual_season,actual_yr,actual_holiday,actual_weathersit))

rmExcept(keepers = "data")
########################################DECISION TREE########################################
#MAPE: 17.47%
#MAE: 684
#RMSE: 864.8
#Accuracy: 82.53%

#Dividing data into train and test
set.seed(123)
train_index = sample(1:nrow(data), 0.8 * nrow(data))
train = data[train_index,]
test = data[-train_index,]

#rparting regression
dx1_model = rpart(cnt ~ ., data = train, method = "anova")

#Predicting test cases
dx1_predictions = predict(dx1_model, test[,-10])

#Creating dataframe for actual and predicted values
df1 = data.frame("actual"=test[,10], "pred"=dx1_predictions)
head(df1)

#calculating MAPE
regr.eval(trues = test[,10], preds = dx1_predictions, stats = c("mae","mse","rmse","mape"))

#calculating MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,10], dx1_predictions)

########################################RANDOM FOREST########################################
#MAPE: 10.68%
#MAE: 392
#RMSE: 535
#Accuracy: 89.32%

#Training data using random forest
rx1_model = randomForest(cnt~., data = train, ntree = 500)

#Predicting test cases
rx1_predictions = predict(rx1_model, test[,-10])

#Creating dataframe for actual and predicted values
df1 = cbind(df1,rx1_predictions)
head(df1)

#Calculating MAPE
regr.eval(trues = test[,10], preds = rx1_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], rx1_predictions)

########################################LINEAR REGRESSION########################################
#MAPE: 12.17%
#RMSE: 673
#Accuracy: 87.83%
#MAE: 494
#Adjusted R squared: 0.8373
#F-statistic: 110.2

#Trainning data using linear regression
lx1_model = lm(formula = cnt~., data = train)

#Checking summary of the model
summary(lx1_model)

#Predicting test cases
lx1_predictions = predict(lx1_model, test[,-10])

#Creating dataframe for actual and predicted values
df1 = cbind(df1,lrx1_predictions)
head(df1)

#Calculating MAPE
regr.eval(trues = test[,10], preds = lx1_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], lx1_predictions)

#Ploting graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="green")
lines(lx1_predictions,col="blue")

#Predicting  sample data
predict(lx1_model,test[2,])
