#----------------------------------------------------------------------
# CAUTION!!!
#----------------------------------------------------------------------
# Before running this comparison script,
# please run all FOUR models and don't clear any saved variables or objects

#----------------------------------------------------------------------
# Comparison of CV Accuracies among the Best Sub-models
#----------------------------------------------------------------------
# extract CV accuracies of the best sub-model for each machine learning model
acc_summary <- c(acc1_nb,
                 acc_logreg,
                 acc_rf,
                 acc_cv_svmpoly)

par(mfrow=c(1,1))
bp <- barplot(acc_summary,
              main = 'Comparison of CV Accuracy',
              xlab = 'CV Accuarcy',
              xlim = c(0.65, 0.82),
              names.arg = c('Naive Bayes', 'Logistic Reg', 'Random Forest', 'SVM'),
              xpd = F,
              horiz = T)
text(acc_summary, bp, labels = round(acc_summary, 4))

# test set accuracy of our "best" model
# Logistic Regression (Bayesian GLM)
cm_test_bayesGLM
acc_test_bayesGLM

#----------------------------------------------------------------------
# Comparison of Test Set Accuracies among the Best Sub-models
#----------------------------------------------------------------------
# extract CV accuracies of the best sub-model for each machine learning model
acc_test_summary <- c(acc_test_nb,
                      acc_test_logreg,
                      acc_test_rf,
                      acc_test_svmpoly)

par(mfrow=c(1,1))
bp <- barplot(acc_test_summary,
              main = 'Comparison of Test Set Accuracy',
              xlab = 'Test Set Accuarcy',
              xlim = c(0.65, 0.82),
              names.arg = c('Naive Bayes', 'Logistic Reg', 'Random Forest', 'SVM'),
              xpd = F,
              horiz = T)
text(acc_test_summary, bp, labels = round(acc_test_summary, 4))


#----------------------------------------------------------------------
# Comparison of AUCs among the Best Sub-models
#----------------------------------------------------------------------
# install.packages("pROC")
library(pROC)

# reload data set and perform train-test split
dataset <- read.csv(filename, header = TRUE , stringsAsFactors = T)
dataset <- na.omit(dataset)[ , -c(1,2,7)]
set.seed(3354)
trainIndex <- createDataPartition(y=dataset$Churn, p=0.8, list=FALSE)
data.train <- dataset[ trainIndex,]
data.test <- dataset[-trainIndex,]

# create roc objects for each model
roc_nb <- roc(data.test$Churn, as.numeric(test_pred_nb))
roc_logreg <- roc(data.test$Churn, as.numeric(test_predict_bayesglm))
roc_rf <- roc(data.test$Churn, as.numeric(test_predict_rf))
roc_svm <- roc(data.test$Churn, as.numeric(test_predict_svmpoly))

# plot ROCs
plot(roc_nb, print.auc.x = 0.9, print.auc.y = 0.2, print.auc=T)
plot(roc_logreg, col='blue', add=T, print.auc.x = 0.9, print.auc.y = 0.6, print.auc=T)
plot(roc_rf, col='red', add=T, print.auc.x = 0.5, print.auc.y = 0.6, print.auc=T)
plot(roc_svm, col='darkgreen', add=T, print.auc.x = 0.6, print.auc.y = 0.7, print.auc=T)
legend("bottom", c("Naive Bayes", "Logistic Regression", "Random Forest", "SVM"),
       lty = c(1,1),
       lwd = c(2,2),
       col = c("black", "blue", "red", "darkgreen"))