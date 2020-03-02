library(readr)
FlightDelays <- read_csv("Documents/Assignment 3/FlightDelays.csv")
View(FlightDelays)
FDdata<-read.csv("Documents/Assignment 3/FlightDelays.csv")
head(FDdata)
summary(FDdata)


# Factorization of outcome variables Day of Week and Time
FDdata$DEP_TIME<- as.factor(FDdata$DEP_TIME)
FDdata$DAY_WEEK<-as.factor(FDdata$DAY_WEEK)

# Normalize data
Norm_FDdata<-preProcess(FDdata[, c(1,2,3,5,6,7,9,10,11,13)], method = c("center","scale"))
FDdata_normalized<- predict(Norm_FDdata, FDdata)
summary(FDdata_normalized)

#remove categorical variables
MyData <- FlightDelays[,-4,8]

# Partition the data into training (60%) and validation (40%) sets
Train_Index<- createDataPartition(FDdata$Flight.Status, p=0.6, list=FALSE)
Train_Data<FDdata[ ,Train_Index,]
Test_Data<-FDdata[-Train_Index,]

#Examine that the 2 sets have similar distribution in Flight Status
summary(Train_Data$Flight.Status)
summary(Test_Data$Flight.Status)


# Build a naïve Bayes classifier
library(caret)
library(ISLR)
install.packages("e1071")
summary(FlightDelays)
nb_model<-NaiveBayes (FlightDelays ~ DEP_TIME+DAY_WEEK, data = Train_Index)
nb_model

# Predict the default status of test dataset 
Predicted_Test_labels <-predict(nb_model,Test_Data)
library("gmodels")

# Show the confusion matrix of the classifier
CrossTable(x=Test$FDdata,y=Predicted_Test_FDdata, prop.chisq = FALSE) 


#Make predictions and return probability of each class
Predicted_Test_FDdata<-predict(nb_model,Test, type = "raw")
#show the first few values 
head(Predicted_Test_FDdata)

# ROC Curves
  
# install.packages("pROC") 
library(pROC)

#Passing the second column of the predicted probabilities 
#That column contains the probability associate to ‘yes’
roc(Test_Data$FDdata, Predicted_Test_labels[,3])
plot.roc(Test_Data$FDdata,Predicted_Test_labels[,3])

  
# Box-Cox Transformation

library(ISLR)
library(caret)

#Create a Box-Cox Transformation Model
Box_Cox_Transform<-preProcess(Default,method = "BoxCox")
Box_Cox_Transform


Default_Transformed=predict(Box_Cox_Transform,Default)
y <- Default_Transformed$FlightStatus
h<-hist(y, breaks=10, col="red", xlab="Day_Week",
        main="Histogram before Transformation")
xfit<-seq(min(y),max(y),length=40)
yfit<-dnorm(xfit,mean=mean(y),sd=sd(y))
yfit <- yfit*diff(h$mids[1:2])*length(y)
lines(xfit, yfit, col="blue", lwd=2) 

  
  ## Hypertuning
  

library(caret)
library(ISLR)
#remove Day-Week, which is the second variable

MyData<-Default[,-3]
set.seed(123)
#Divide data into test and train
Index_Train<-createDataPartition(MyData$`Flight Status`, p=0.6, list=FALSE)
Train <-MyData[Index_Train,]
Test  <-MyData[-Index_Train,]
nb_model <-train(FlightDelays~DAY_WEEK+Distance = Train, preProc = c("BoxCox", "center", "scale"))
# Predict the flight delays status of test dataset 
Predicted_Test_labels <-predict(nb_model,Test)
library("gmodels")
# Show the confusion matrix of the classifier
CrossTable(x=Test$default,y=Predicted_Test_labels, prop.chisq = FALSE) 
