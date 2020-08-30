#Importing Important Libraries
library("ggplot2")
library("corrgram")
library( "DMwR")
library( "usdm")
library( "caret")
library( "randomForest")
library( "e1071")
library( "DataCombine")
library("doSNOW")
library("inTrees")
library( "rpart.plot")
library("rpart")
library(caTools)
library(rpart)

#Setting Up The Working Directory
setwd('C:/Users/Raaz/Documents/Data Science/Project')

#Importing The Dataset
df <- read.csv('day.csv')
head(df)
nrow(df)
ncol(df)
str(df)
ls(df)
summary(df)

#Removing instant,date
df$instant <- NULL
df$dteday <- NULL

#Changing Label Name
names(df) <- c('Season','Year','Month','Holiday',
               'Weekday','Workingday','Weather_Situation',
               'Temperature','Feeling_Temperature','Humidity','Windspeed',
               'Casual','Registered','Total_Count')
head(df)

#Missing Value Analysis
missing_val<-data.frame(apply(df,2,function(x){sum(is.na(x))}))
names(missing_val)[1]='missing_val'
missing_val

#Data Visualization
#Season
ggplot(df, aes(x=Season)) + geom_bar(fill="firebrick") + labs(title="Total Count by Season")
ggplot(df,aes(x=Season,y=Total_Count,fill=Season))+theme_bw()+geom_col()+labs(x='Season',
                         y='Total_Count',title='Season wise monthly distribution of counts')
qplot(data=df,x=Temperature,y=Total_Count,colour=Season,main="Temperature VS Total Count")
qplot(data=df,x=Feeling_Temperature,y=Total_Count,colour=Season,main="Feeling Temperature VS Total Count")
qplot(data=df,x=Humidity,y=Total_Count,colour=Season,main="Humidity VS Total Count")
qplot(data=df,x=Windspeed,y=Total_Count,colour=Season,main="Windspeed VS Total Count")

#Year
ggplot(df, aes(x=Year)) + geom_bar(fill="firebrick") + labs(title="Total Count by Year")
ggplot(df,aes(x=Year,y=Total_Count,fill=Year))+theme_bw()+geom_col()+labs(x='Year',
                   y='Total_Count',title='Year wise monthly distribution of counts')
qplot(data=df,x=Temperature,y=Total_Count,colour=Year,main="Temperature VS Total Count")
qplot(data=df,x=Feeling_Temperature,y=Total_Count,colour=Year,main="Feeling Temperature VS Total Count")
qplot(data=df,x=Humidity,y=Total_Count,colour=Year,main="Humidity VS Total Count")
qplot(data=df,x=Windspeed,y=Total_Count,colour=Year,main="Windspeed VS Total Count")

#Month
ggplot(df, aes(x=Month)) + geom_bar(fill="firebrick") + labs(title="Total Count by Month")
ggplot(df,aes(x=Month,y=Total_Count,fill=Month))+theme_bw()+geom_col()+labs(x='Month',
                           y='Total_Count',title='Month wise monthly distribution of counts')
qplot(data=df,x=Temperature,y=Total_Count,colour=Month,main="Temperature VS Total Count")
qplot(data=df,x=Feeling_Temperature,y=Total_Count,colour=Month,main="Feeling Temperature VS Total Count")
qplot(data=df,x=Humidity,y=Total_Count,colour=Month,main="Humidity VS Total Count")
qplot(data=df,x=Windspeed,y=Total_Count,colour=Month,main="Windspeed VS Total Count")

#Holiday
ggplot(df, aes(x=Holiday)) +  geom_bar(fill="firebrick") + labs(title="Total Count by Holiday")
ggplot(df,aes(x=Holiday,y=Total_Count,fill=Holiday))+theme_bw()+geom_col()+labs(x='Holiday',
               y='Total_Count',title='Holiday wise monthly distribution of counts')
qplot(data=df,x=Temperature,y=Total_Count,colour=Holiday,main="Temperature VS Total Count")
qplot(data=df,x=Feeling_Temperature,y=Total_Count,colour=Holiday,main="Feeling Temperature VS Total Count")
qplot(data=df,x=Humidity,y=Total_Count,colour=Holiday,main="Humidity VS Total Count")
qplot(data=df,x=Windspeed,y=Total_Count,colour=Holiday,main="Windspeed VS Total Count")

#Weekday
ggplot(df, aes(x=Weekday)) +  geom_bar(fill="firebrick") + labs(title="Total Count by Weekday")
ggplot(df,aes(x=Weekday,y=Total_Count,fill=Weekday))+theme_bw()+geom_col()+labs(x='Weekday',
                y='Total_Count',title='Weekday wise monthly distribution of counts')
qplot(data=df,x=Temperature,y=Total_Count,colour=Weekday,main="Temperature VS Total Count")
qplot(data=df,x=Feeling_Temperature,y=Total_Count,colour=Weekday,main="Feeling Temperature VS Total Count")
qplot(data=df,x=Humidity,y=Total_Count,colour=Weekday,main="Humidity VS Total Count")
qplot(data=df,x=Windspeed,y=Total_Count,colour=Weekday,main="Windspeed VS Total Count")

#Workingday
ggplot(df, aes(x=Workingday)) + geom_bar(fill="firebrick")+ labs(title="Total Count by Workingday")
ggplot(df,aes(x=Workingday,y=Total_Count,fill=Workingday))+theme_bw()+geom_col()+labs(x='Workingday',
              y='Total_Count',title='Workingday wise monthly distribution of counts')
qplot(data=df,x=Temperature,y=Total_Count,colour=Workingday,main="Temperature VS Total Count")
qplot(data=df,x=Feeling_Temperature,y=Total_Count,colour=Workingday,main="Feeling Temperature VS Total Count")
qplot(data=df,x=Humidity,y=Total_Count,colour=Workingday,main="Humidity VS Total Count")
qplot(data=df,x=Windspeed,y=Total_Count,colour=Workingday,main="Windspeed VS Total Count")

#Weather_Situation
ggplot(df, aes(x=Weather_Situation)) +  geom_bar(fill="firebrick") + labs(title="Total Count by Weather_Situation")
ggplot(df,aes(x=Weather_Situation,y=Total_Count,fill=Weather_Situation))+theme_bw()+geom_col()+labs(x='Weather_Situation',
               y='Total_Count',title='Weather_Situation wise monthly distribution of counts')
qplot(data=df,x=Temperature,y=Total_Count,colour=Weather_Situation,main="Temperature VS Total Count")
qplot(data=df,x=Feeling_Temperature,y=Total_Count,colour=Weather_Situation,main="Feeling Temperature VS Total Count")
qplot(data=df,x=Humidity,y=Total_Count,colour=Weather_Situation,main="Humidity VS Total Count")
qplot(data=df,x=Windspeed,y=Total_Count,colour=Weather_Situation,main="Windspeed VS Total Count")

#Outliers Analysis
#Box plot for Total Count
boxplot(df$Total_Count,main="Total_Count",sub=paste(boxplot.stats(df$Total_Count)$out))
#Box plot for Temperature outliers
boxplot(df$Temperature,main="Temperature",sub=paste(boxplot.stats(df$Temperature)$out))
#Box plot for Feeling Temperature outliers
boxplot(df$Feeling_Temperature,main="Feeling Temperature",sub=paste(boxplot.stats(df$Feeling_Temperature)$out))
#Box plot for Humidity outliers
boxplot(df$Humidity,main="Humidity",sub=paste(boxplot.stats(df$Humidity)$out))
#Box plot for Windspeed outliers
boxplot(df$Windspeed,main="Windspeed",sub=paste(boxplot.stats(df$Windspeed)$out))

#Removing Outliers
# Removing Humidity outliers
df <- df[df$Humidity > quantile(df$Humidity, .25) - 1.5*IQR(df$Humidity) & 
           df$Humidity < quantile(df$Humidity, .75) + 1.5*IQR(df$Humidity), ]
# Removing Windspeed outliers
df <- df[df$Windspeed > quantile(df$Windspeed, .25) - 1.5*IQR(df$Windspeed) & 
           df$Windspeed < quantile(df$Windspeed, .75) + 1.5*IQR(df$Windspeed), ]
nrow(df)

#Outliers Analysis After Removing Outliers
#Box plot for Humidity outliers
boxplot(df$Humidity,main="Humidity",sub=paste(boxplot.stats(df$Humidity)$out))
#Box plot for Windspeed outliers
boxplot(df$Windspeed,main="Windspeed",sub=paste(boxplot.stats(df$Windspeed)$out))


#Normality Test
#Quintle-Quintle normal plot
qqnorm(df$Total_Count)
#Quintle-Quintleline
qqline(df$Total_Count)

#Feature Selection
library(corrgram)
corrgram(df[,1:11], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main = 'CORRELATION PLOT')
#Dropping Casual and Registered due to their multicollinearity
#Working Day due low correlation with Total Count
#Dropping Feeling Temperature due to multicollinearity with Temperature
df_new <-subset(df,select=c('Season', 'Year', 'Month', 'Holiday', 'Weekday',
                            'Weather_Situation', 'Temperature', 'Humidity',
                            'Windspeed','Total_Count'))
head(df_new)

#Chi Square Test
H0<- c("Variables are Independent") 
H1 <- c("Variables are Dependent")
res<-chisq.test(df)
p_value<-res$p.value
alpha<-0.95
if (alpha>p_value) {
  print("Rejecting The Null Hypothesis")
  print(H1)
} else {
  print("Rejecting The Null Hypothesis")
  print(H0)
}

#Splitting The Dataset
split = sample.split(df_new$Total_Count, SplitRatio = 0.8)
training_set = subset(df_new, split == TRUE)
test_set = subset(df_new, split == FALSE)
test_set

#Linear Regression model
#Fitting The Linear Regression model on Training Set
lr_model = lm(Total_Count ~. , data = training_set)
summary(lr_model)

#Prediction by Linear Regression model on Training Set 
pred_lr = predict(lr_model, test_set[,-10])

# Visualizing the Predicted Test set results
ggplot() +
  geom_point(aes(x = test_set$Total_Count, y = pred_lr), colour = 'red') +
  ggtitle('Linear Regression Model model') +
  xlab('Actual values') +
  ylab('Predicted values')

#Residual plot
residuals<- test_set$Total_Count-pred_lr
plot(test_set$Total_Count,residuals,xlab='Observed',ylab='Residuals',
     main='Residual plot in Linear Regression Model',col='blue')
abline(0,0)

# Cross Validation of Linear Regression Model
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)
# Train the model
model <- train(Total_Count ~., data = training_set, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#Evaluation of Linear Regression Model
postResample(test_set$Total_Count, pred_lr)
rmse<-RMSE(test_set$Total_Count, pred_lr)
print(rmse)
mae<-MAE(test_set$Total_Count, pred_lr)
print(mae)
# R²
summary(lr_model)$r.squared
# adjusted R²
summary(lr_model)$adj.r.squared

#Decision Tree Regression
set.seed(121)
#Fitting the Decision Tree Regression Model on Training Dataset
dt_model = rpart(Total_Count~. , data = training_set, method = "anova")
summary(dt_model)

#Decision Tree Plot
plt = rpart.plot(dt_model, type = 5, digits = 2, fallen.leaves = TRUE)

#Predictions by Decision Tree Regression Model on Test Dataset
pred_dtr = predict(dt_model, test_set[,-10])

# # Visualizing the Predicted Test set results
ggplot() +
  geom_point(aes(x = test_set$Total_Count, y = pred_dtr), colour = 'red') +
  ggtitle('Decision Tree model') +
  xlab('Actual values') +
  ylab('Predicted values')

#Residual plot
residuals<- test_set$Total_Count-pred_dtr
plot(test_set$Total_Count,residuals,xlab='Observed',ylab='Residuals',
     main='Residual plot in Linear Regression Model',col='blue')
abline(0,0)

# Cross Validation of Decision Tree Regression Model
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)
# Train the model
model <- train(Total_Count ~., data = training_set, method = 'rpart',
               trControl = train.control)
# Summarize the results
print(model)

#Evaluation of Model
postResample(pred_dtr,test_set$Total_Count)
rmse<-RMSE(test_set$Total_Count, pred_dtr)
print(rmse)
mae<-MAE(test_set$Total_Count, pred_dtr)
print(mae)
rss <- sum((pred_dtr - test_set$Total_Count ) ^ 2)  ## residual sum of squares
tss <- sum((test_set$Total_Count - mean(test_set$Total_Count)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
print(rsq)#R²
# adjusted R²
n <- nrow(test_set)
p <- ncol(test_set)
Adj_r2 <- 1-(1-rsq)*(n-1)/(n-p-1)
print(Adj_r2)


#Random Forest
set.seed(101)
#Fitting the Random Forest Regression Model on Training Dataset
rf_model = randomForest(Total_Count~. , data = training_set, importance = TRUE, ntree = 1000)
rf_model
#Error plotting
plot(rf_model)
#Variable Importance plot
varImpPlot(rf_model)


#Predictions by Random Forest on the Test Dataset 
pred_rfr = predict(rf_model, test_set[,-10])
# Visualizing the  Predicted Test set results
ggplot() +
  geom_point(aes(x = test_set$Total_Count, y = pred_rfr), colour = 'red') +
  ggtitle('Random Forest model') +
  xlab('Actual values') +
  ylab('Predicted values')

#Residual plot
residuals<test_set$Total_Count-pred_rfr
plot(test_set$Total_Count,residuals,xlab='Observed',ylab='Residuals',main='Random Forest Residual plot',col='blue')
abline(0,0)

# Cross Validating the Random Forest Model
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Total_Count ~., data = training_set, method = 'ranger',
               trControl = train.control)
# Summarize the results
print(model)

#Importance of Independent Variables
importance(rf_model)

#Evaluation Of Model
postResample(pred_rfr,test_set$Total_Count)
rmse<-RMSE(test_set$Total_Count, pred_rfr)
print(rmse)
mae<-MAE(test_set$Total_Count, pred_rfr)
print(mae)
# R²
rss <- sum((pred_rfr - test_set$Total_Count ) ^ 2)  ## residual sum of squares
tss <- sum((test_set$Total_Count - mean(test_set$Total_Count)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
print(rsq)#R²
# adjusted R²
n <- nrow(test_set)
p <- ncol(test_set)
Adj_r2 <- 1-(1-rsq)*(n-1)/(n-p-1)
print(Adj_r2)

#Random Forest Regression has highest R2 score and accuracy so Final Model is Random Forest Regression

#Predicting a sample input
#Input('Season'=1, 'Year'=1, 'Month'=4, 'Holiday'=1, 'Weekday'=1,'Weather_Situation'=2, 'Temperature'=27,'Humidity'=0.8,'Windspeed'=0.16)


Prediction = predict(rf_model, data.frame('Season'=1, 'Year'=1, 'Month'=4, 'Holiday'=1, 'Weekday'=1,
                              'Weather_Situation'=2, 'Temperature'=27, 'Humidity'=0.8,'Windspeed'=0.16))
print('Prediction of Total Bike Renatl Count a sample input is')
print(Prediction)

