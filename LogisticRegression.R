#install.packages('caret')
#install.packages('arm')
library(caret)
library(arm) # for BayesGLM

data1 <- read.csv("Telco-Customer-Churn.csv", header = T)
data1 <- na.omit(data1)#remove the observations without value
data1$customerID <- NULL#remove the id
#Monthly Charges and Total Charges transform them to numeric
data1$MonthlyCharges <- as.numeric(data1$MonthlyCharges)
data1$TotalCharges <- as.numeric(data1$TotalCharges)
#We will do a k-fold cross validation, with k=10
fitControl <- trainControl(method = "cv", number = 10)

#Excluding certain predictors according to Boruta
data1$gender <- NULL
data1$PhoneService <- NULL

#create 2 partitions, one for the training and one for the testing
set.seed(3354)
div <- createDataPartition(data1$Churn, p = .80, list = FALSE)#partiton of 80%
RTraining <- data1[div,]
RTest <- data1[-div,]

#standard logistic regression
set.seed(3354)
Log_regression <- train(Churn ~ ., data = RTraining, method = "glm", family = "binomial", trControl = fitControl)
cm_logreg <- confusionMatrix(Log_regression)$table
cm_logreg
acc_logreg <- (cm_logreg[1,1] + cm_logreg[2,2]) / 100
acc_logreg

#prediction on test set
test_predict_Log_regression <- predict(Log_regression, newdata = RTest)
cm_test_Log_regression <- table(test_predict_Log_regression, RTest$Churn)
cm_test_Log_regression
acc_test_Log_regression <- mean(test_predict_Log_regression == RTest$Churn)
acc_test_Log_regression

#Bayesian Generalized Linear Model
set.seed(3354)
bayesGLM <- train(Churn ~ ., data = RTraining, method = "bayesglm", family = "binomial",
             trControl = fitControl)
cm_bayesGLM <- confusionMatrix(bayesGLM)$table
cm_bayesGLM
acc_bayesGLM <- (cm_bayesGLM[1,1] + cm_bayesGLM[2,2]) / 100
acc_bayesGLM

#prediction on test set
test_predict_bayesglm <- predict(bayesGLM, newdata = RTest)
cm_test_bayesGLM <- table(test_predict_bayesglm, RTest$Churn)
cm_test_bayesGLM
acc_test_bayesGLM <- mean(test_predict_bayesglm == RTest$Churn)
acc_test_bayesGLM

#Boosted Logistic Regression
set.seed(3354)
boostedLR <- train(Churn ~ ., data = RTraining, method = "LogitBoost", trControl = fitControl)
cm_boostedLR <- confusionMatrix(boostedLR)$table
cm_boostedLR
acc_boostedLR <- (cm_boostedLR[1,1] + cm_boostedLR[2,2]) / 100
acc_boostedLR

#prediction on test set
test_predict_boostedLR <- predict(boostedLR, newdata = RTest)
cm_test_boostedLR <- table(test_predict_boostedLR, RTest$Churn)
cm_test_boostedLR
acc_test_boostedLR <- mean(test_predict_boostedLR == RTest$Churn)
acc_test_boostedLR


#comparing the CV
par(mfrow=c(1,1))
barplot(c( acc_logreg, acc_bayesGLM, acc_boostedLR),
        main = 'Accuracy - Logistic Regression',
        xlab = 'CV Accuarcy',
        names.arg = c('1', '2', '3'),
        xlim = c(0.77, 0.806),
        xpd = F,
        horiz = T)


