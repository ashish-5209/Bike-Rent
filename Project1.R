#remove all the objects stored
rm(list=ls())
#set current working directory
setwd("E:/")
#Current working directory
getwd()
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','gplots','scales','psych')
lapply(x, require, character.only = TRUE)
rm(x)
## Read the data
data=read.csv("day.csv",header = T)
###########################################Explore the data##########################################
str(data)
#count of unique values in a columns
length(unique(data$season))
#convert into factor as only four repeated values present in the seasons
data$season=as.factor(data$season)
#similarly calculating count of unique values in a colums and convert it into the appropriate data types
length(unique(data$yr))
data$yr=as.factor(data$yr)

length(unique(data$mnth))
data$mnth=as.factor(data$mnth)

length(unique(data$holiday))
data$holiday=as.factor(data$holiday)

length(unique(data$weekday))
data$weekday=as.factor(data$weekday)

length(unique(data$workingday))
data$workingday=as.factor(data$workingday)

length(unique(data$weathersit))
data$weathersit=as.factor(data$weathersit)



##################################Missing Values Analysis###############################################
table(is.na(data))
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))})) # counts the missinig value for each variable
#There are no missing value present in the data

############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(data,is.numeric) #selecting only numeric

numeric_data = data[,numeric_index]

cnames = colnames(numeric_data)

for(i in 1:length(cnames))
{
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(data))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="count")+
            ggtitle(paste("Box plot of responded for",cnames[i])))
}

# ## Plotting plots together
gridExtra::grid.arrange(gn2,gn3,gn4,ncol=3)
gridExtra::grid.arrange(gn1,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
#make copy of data
df=data
data=df


# #Replace all outliers with NA and impute
# #create NA on "windspeed"
valw = data$windspeed[data$windspeed %in% boxplot.stats(data$windspeed)$out]
print(length(valw))
data$windspeed[data$windspeed %in% valw] = NA


#For windspeed  
#Actual value[13]=0.30100
#mean=0.19038
#median=0.18
#KNN = 0.25

#Mean Method
#data$windspeed[is.na(data$windspeed)] = mean(data$windspeed, na.rm = T)


#Median Method
#data$windspeed[is.na(data$windspeed)] = median(data$windspeed, na.rm = T)


# kNN Imputation
#data = knnImputation(data, k = 3) 
data = knnImputation(data, k = 3)

# #create NA on "hum"
valh = data$hum[data$hum %in% boxplot.stats(data$hum)$out]
print(length(valh))
data$hum[data$hum %in% valh] = NA

#For hum  
#Actual value[16]=0.483750
#mean=0.6280915
#median=6270835
#KNN = 0.5253335

#Mean Method
#data$hum[is.na(data$hum)] = mean(data$hum, na.rm = T)


#Median Method
#data$hum[is.na(data$hum)] = median(data$hum, na.rm = T)


# kNN Imputation
#data = knnImputation(data, k = 3) 
data = knnImputation(data, k = 3)

# #create NA on "casual"
valc = data$casual[data$casual %in% boxplot.stats(data$casual)$out]
print(length(valc))
data$casual[data$casual %in% valc] = NA

#For casual  
#Actual value[3]=120
#mean=849.174
#median=717
#KNN = 93.59666

#Mean Method
#data$hum[is.na(data$hum)] = mean(data$hum, na.rm = T)


#Median Method
#data$hum[is.na(data$hum)] = median(data$hum, na.rm = T)


# kNN Imputation
#data = knnImputation(data, k = 3) 
data = knnImputation(data, k = 3)

##################################Feature Selection################################################
## Correlation Plot 
corrgram(data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Chi-squared Test of Independence
factor_index = sapply(data,is.factor)
factor_data = data[,factor_index]

for (i in 1:8)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data[,i])))
}

## Dimension Reduction
data = subset(data, 
                         select = -c(hum,atemp,season,yr,mnth,weekday))


#Bar plot(categorical data)
#Holiday
ggplot(data, aes_string(x = data$holiday)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +ggtitle("Holiday Analysis")

#workingday
ggplot(data, aes_string(x = data$workingday)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +ggtitle("workingday Analysis")

#weathersit
ggplot(data, aes_string(x = data$weathersit)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +ggtitle("Wethersit Analysis")

#histograms for the continous variable
hist(data$temp)
hist(data$windspeed)
hist(data$casual)
hist(data$registered)

#casual #Scatter Plot
ggplot(data, aes_string(x = data$casual, y = data$weathersit)) + 
  geom_point(aes_string(colour = data$holiday, shape = data$workingday),size = 4) +
  theme_bw()+ ylab("weathersit") + xlab("casual") + ggtitle("Scatter plot Analysis")

#registered #Scatter Plot
ggplot(data, aes_string(x = data$registered, y = data$weathersit)) + 
  geom_point(aes_string(colour = data$holiday, shape = data$workingday),size = 4) +
  theme_bw()+ ylab("weathersit") + xlab("Registered") + ggtitle("Scatter plot Analysis")
###################################Model Development#######################################

#Divide data into train and test using stratified sampling method
#Divide the data into train and test
set.seed(123)
train.index = createDataPartition(data$cnt, p = .80, list = FALSE)
train = data[ train.index,]
test  = data[-train.index,]
train = subset(train, 
              select = -c(instant, dteday))
test1 = test
test = subset(test, 
               select = -c(instant, dteday))

dim(train)
dim(test)

##Decision tree for Regression
#Develop Model on training data
# ##rpart for regression
fit = rpart(cnt ~ ., data = train, method = "anova")

library("rattle")
library("rpart.plot")
library("RColorBrewer")

fancyRpartPlot(fit)
#Predict for new test cases
predictions_DT = predict(fit, test[,-8])

###Random Forest
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 1000)

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-8])

#Linear Regression
#check multicollearity
library(usdm)
vifcor(data[,6:9], th = 0.9)


#run regression model
lm_model = lm(cnt ~., data = train)


#Summary of the model
summary(lm_model)


#Predict
predictions_LR = predict(lm_model, test[,1:7])

###########################################MODEL EVALUATION############################################
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}
#Decision Tree
# Accuracy=87.69%
# Error=12.61%
MAPE(test[,8], predictions_DT)

#Random Forest
# Accuracy=94%
# Error=6%
MAPE(test[,8], RF_Predictions)

#Multiple Regression
# Accuracy=96.8%
# Error=3.2%
MAPE(test[,8], predictions_LR)

sample=data.frame(instant=test1$instant,dteday=test1$dteday,cnt = predictions_LR)
write.csv(sample,file="Sample_Data.csv",row.names=F)
