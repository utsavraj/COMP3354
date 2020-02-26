#-------For Installing packages that are missing (uncomment)-------#
#install.packages("data.table", type = "binary")
#install.packages("caret", dependencies = TRUE)
#install.packages('rattle', dependencies = TRUE) 
#install.packages("randomForest")
# install.packages("MLeval")

#-------#
library(caret)
library(mlbench)
library(rattle)
library(randomForest) #For RF Tune only
library(MLeval) #For ROC and AUC - only works with train

#-------#

#define Filename
filename <- "Telco-Customer-Churn.csv"

#load the CSV file from local directory (change working directory)
dataset <- read.csv(filename, header = TRUE , stringsAsFactors = T)

#Repmoves NA from dataset, and InternetService=No
dataset<- na.omit(dataset) 

#Remove the Unique ID from data as well as PhoneService and Gender based on Boruta
dataset[1] <- NULL # Unique ID
dataset[1] <- NULL # Gender
dataset[5] <- NULL # PhoneService
head(dataset)

#define an 80%/20% train/test split of the dataset
set.seed(3354)
trainIndex <- createDataPartition(y=dataset$Churn, p=0.8, list=FALSE)
data.train <- dataset[ trainIndex,]
data.test <- dataset[-trainIndex,]


head(dataset)

#Random Forest (method = 'rf')
#Finding the correct tuning values:
# - mtry: Number of variables randomly sampled as candidates at each split.

# Random Search: Try random values within a range.
set.seed(3354)
bestmtry <- tuneRF(dataset[,1:17], dataset[,18], stepFactor=1.5, improve=1e-5) #default ntree = 50
print(bestmtry) #Use mtry = 2 as OOB error = 0.2039

#Setting the mtry value
mtry <- 2
tunegrid <- expand.grid(.mtry=mtry)
set.seed(3354)
model.2 = train(Churn ~ ., 
              data=data.train, 
              method="rf", 
              tuneGrid=tunegrid,
              trControl = trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = TRUE ))

# make predictions
x_test <- data.test[,1:17]
y_test <- data.test[,18]
test_predict_rf <- predict(model.2, x_test )

#Plotting Random Forest
plot(model.2$finalModel,  main="Random Forest for our Dataset")
model.2.legend <- if (is.null(model.2$finalModel$test$err.rate)) {colnames(model.2$finalModel$err.rate)} else {colnames(model.2$finalModel$test$err.rate)}
legend("top", cex =0.5, legend=model.2.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)


# summarize results
#CV CM
cm_rf <- confusionMatrix(model.2)$table
cm_rf

acc_rf <- (cm_rf[1,1] + cm_rf[2,2])/100
acc_rf

#test CM
cm_test_rf <- confusionMatrix(test_predict_rf, y_test)$table
cm_test_rf

acc_test_rf <- (cm_test_rf[1,1] + cm_test_rf[2,2])/sum(cm_test_rf)
acc_test_rf

#Important features
varImpPlot(model.2$finalModel,type=2)
