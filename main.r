### Credit card fraud detection
# Importing the necessary libraries

library(ranger)  #table stats
library(caret)   #pre processing
library(data.table)  #pandas equivalent

data <- read.csv("creditcard.csv")

# Data Exploration
data.table(data)

# Statistical Analysis of the data
summary(data)
table(data$Class)
names(data)

# Summary of Amount
summary(data$Amount)
# Standard deviations
sd(data$Amount)
# Inter-Quartile Range values
IQR(data$Amount)
# Variance of the data frame
var(data$Amount)

# Data Manipulation
data$Amount <- scale(data$Amount)  #data normalization
# R, C
data2 <- data[,-c(1)]  #dropping the useless time column
head(data2)

set.seed(12)  # setting the random number seed
library(caTools)

sample_data <- sample.split(data2$Class,SplitRatio = 0.80)

train_data <- subset(data2,sample_data==TRUE)

test_data <- subset(data2,sample_data==FALSE)

dim(train_data)  # Dimesions of the dataset
dim(test_data)

# Logistic Regression
Logistic_Model <- glm(Class~., test_data, family= binomial())
summary(Logistic_Model)

plot(Logistic_Model)


Logistic_Model1 <- glm(Class~., train_data, family= binomial())
summary(Logistic_Model1)

# ROC Curve 
library(pROC)
# predicting model efficiency using prob distribution
lr.predict <- predict(Logistic_Model1,test_data,probability = TRUE)
auc.gb <- roc(test_data$Class,lr.predict,plot=TRUE,col="green")

# Decision Tree
library(rpart)
library(rpart.plot)

decision_model <- rpart(Class~.,data,method="class")
predicted_val <- predict(decision_model,data,method="class")
probability <- predict(decision_model,data,type="prob")
rpart.plot(decision_model)

