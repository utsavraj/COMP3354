##############################
# CAUTION: RUNTIME > 2 hrs!! #
##############################
library(caret)
library(ggplot2)
library(magrittr)

MyData <- read.csv(file="Telco-Customer-Churn.csv", header=TRUE, sep=",")
MyData$customerID <- NULL
MyData <- na.omit(MyData)
MyData$gender<-NULL
MyData$PhoneService<-NULL

set.seed(3354)
training.samples <- MyData$Churn %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- MyData[training.samples, ]
test.data <- MyData[-training.samples, ]

###############
#Linear Kernel# Best accuracy of the kernels used
###############
# runtime: ~10 mins

set.seed(3354)
model1 <- train(
  Churn ~., data = train.data, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
  preProcess = c("center","scale")
)
# Print the best tuning parameter C that maximizes model accuracy
tuning_parameters1<-model1$bestTune
#C = 0.1052632
acc_cv_svmlinear <- max(model1$results$Accuracy)

plot(model1)

# test performance
test_predict_svmlinear <- model1 %>% predict(test.data)

acc_svmlinear<-mean(test_predict_svmlinear == test.data$Churn) # 0.7950

table_svmlinear<-table(test_predict_svmlinear,test.data$Churn)



#########################
#radial(gaussian kernel)#
#########################
# runtime: ~5 mins

set.seed(3354)
model2 <- train(
  Churn ~., data = train.data, method = "svmRadial",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 10
)

# Print the best tuning parameter sigma and C that maximizes model accuracy
tuning_parameters2<-model2$bestTune
acc_cv_svmgaus <- max(model2$results$Accuracy)
plot(model2)

# test performance
test_predict_svmgaus <- model2 %>% predict(test.data)

acc_svmgaus<-mean(test_predict_svmgaus == test.data$Churn) # 0.7943

table_svmgaus<-table(test_predict_svmgaus,test.data$Churn)

#########################
#   polynomial kernel   #
#########################
# runtime: ~2 hrs

set.seed(3354)
model3 <- train(
  Churn ~., data = train.data, method = "svmPoly",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 4
)
# Print the best tuning parameter sigma and C that
# maximizes model accuracy
tuning_parameters3<-model3$bestTune
acc_cv_svmpoly <- max(model3$results$Accuracy)

plot(model3)

# test performance
test_predict_svmpoly <- model3 %>% predict(test.data)

acc_test_svmpoly<-mean(test_predict_svmpoly == test.data$Churn) #0.7972

table_test_svmpoly<-table(test_predict_svmpoly,test.data$Churn)