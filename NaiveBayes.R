#install.packages("caret")
#install.packages("klaR")
library(caret)
library(klaR)

#----------------------------------------------------------------------
# Load the dataset
#----------------------------------------------------------------------
dataset <- "Telco-Customer-Churn.csv"
df <- read.csv(dataset, header = T)

#----------------------------------------------------------------------
# Data preparation
#----------------------------------------------------------------------
df[!complete.cases(df),] # only 11 incomplete rows
df <- na.omit(df) # remove incomplete rows

df$SeniorCitizen <- as.factor(df$SeniorCitizen) # convert categorical variables into factor

# recode continuous variable for Naive Bayes Model
df$tenure.cat         <- cut(df$tenure, 
                             breaks=c(quantile(df$tenure, probs = seq(0, 1, by = 0.25))), 
                             labels=c("short","med-short","med-long","long"),
                             include.lowest=TRUE)
df$MonthlyCharges.cat <- cut(df$MonthlyCharges, 
                             breaks=c(quantile(df$MonthlyCharges, probs = seq(0, 1, by = 0.25))), 
                             labels=c("low","med-low","med-high","high"),
                             include.lowest=TRUE) 
df$TotalCharges.cat   <- cut(df$TotalCharges,
                             breaks=c(quantile(df$TotalCharges, probs = seq(0, 1, by = 0.25))), 
                             labels=c("low","med-low","med-high","high"),
                             include.lowest=TRUE)

df <- df[-1] # remove the index column

df <- df[-c(2, 7)] # remove PhoneService and Gender based on Boruta

#----------------------------------------------------------------------
# Data Visualization
#----------------------------------------------------------------------
# Inspect conditional distributions of continuous variables
par(mfrow = c(2,3))
hist(df[df$Churn=='Yes', 'tenure'],
     main = 'Tenure for Churned Customers',
     xlab = 'Tenure')
hist(df[df$Churn=='Yes', 'MonthlyCharges'],
     main = 'Monthly Charges for Churned Customers',
     xlab = 'Monthly Charges')
hist(df[df$Churn=='Yes', 'TotalCharges'],
     main = 'Total Charges for Churned Customers',
     xlab = 'Total Charges')

hist(df[df$Churn=='No', 'tenure'],
     main = 'Tenure for Unchurned Customers',
     xlab = 'Tenure')
hist(df[df$Churn=='No', 'MonthlyCharges'],
     main = 'Monthly Charges for Unchurned Customers',
     xlab = 'Monthly Charges')
hist(df[df$Churn=='No', 'TotalCharges'],
     main = 'Total Charges for Unchurned Customers',
     xlab = 'Total Charges')

#----------------------------------------------------------------------
# Model Training and Evaluation
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# Generate indices for the training set

set.seed(3354)
train <- createDataPartition(df$Churn, p=0.8, list = F)

#----------------------------------------------------------------------
# Variation1: Continuous variables are recoded as categorical ones

df1.train <- df[train, -c(4,16,17)] #remove continuous variables
df1.test <- df[-train, -c(4,16,17)] #remove continuous variables

# train the Naive Bayes model and perform 10-fold CV on training set
train.control <- trainControl(method="cv", number=10)

set.seed(3354)
model1_nb <- train(Churn~., data=df1.train,
                method = "nb",
                trControl=train.control)

cm1_nb <- confusionMatrix(model1_nb)$table
cm1_nb

acc1_nb <- (cm1_nb[1,1] + cm1_nb[2,2])/100
acc1_nb # CV accuracy = 0.6714

#----------------------------------------------------------------------
# Variation2: Continuous variables are preserved

df2.train <- df[train, -c(19,20,21)] #remove recoded variables
df2.test <- df[-train, -c(19,20,21)] #remove recoded variables

# train the Naive Bayes model and perform 10-fold CV on training set
train.control <- trainControl(method="cv", number=10)

set.seed(3354)
model2_nb <- train(Churn~., data=df2.train,
               method = "nb",
               trControl=train.control)

cm2_nb <- confusionMatrix(model2_nb)$table
cm2_nb          
    
acc2_nb <- (cm2_nb[1,1] + cm2_nb[2,2])/100
acc2_nb # CV accuracy = 0.6602

#----------------------------------------------------------------------
# Model Comparison
#----------------------------------------------------------------------
# Choose the model with highest cross-validation accuracy
accuracy <- c(acc1_nb, acc2_nb)

par(mfrow=c(2,1))
barplot(accuracy,
        main = 'CV Accuracy from Naive Bayes Algorithm',
        xlab = 'CV Accuarcy',
        xlim = c(0.65, 0.675),
        names.arg = c('Variation 1', 'Variation 2'),
        xpd = F,
        horiz = T)

#----------------------------------------------------------------------
# Obtain test accuracies
#----------------------------------------------------------------------
test_pred_nb <- predict(model1_nb, df1.test)
cm_test_nb <- confusionMatrix(test_pred_nb, df1.test$Churn)$table
cm_test_nb
acc_test_nb <- (cm_test_nb[1,1] + cm_test_nb[2,2])/sum(cm_test_nb)
acc_test_nb # 0.6641

test_pred_nb2 <- predict(model2_nb, df2.test)
cm_test_nb2 <- confusionMatrix(test_pred_nb2, df2.test$Churn)$table
cm_test_nb2
acc_test_nb2 <- (cm_test_nb2[1,1] + cm_test_nb2[2,2])/sum(cm_test_nb2)
acc_test_nb2 # 0.6577 
#----------------------------------------------------------------------

