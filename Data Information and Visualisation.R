#-------For Installing packages that are missing (uncomment)-------#
#install.packages("ggpubr")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("dplyr")

#-------#
library(ggplot2)
library(ggpubr)
library(corrplot)
library(dplyr)
#-------#

set.seed(3354)

#define Filename
filename <- "Telco-Customer-Churn.csv"

#load the CSV file from local directory
dataset <- read.csv(filename, header = TRUE , stringsAsFactors = T)
summary(dataset) #7043 obs. of  21 variables:

#Check for empty columns - Finding tells 11 rows of TotalCharges have NAs 
sum(is.na(dataset))

#Repmoves NA from dataset, and InternetService=No
dataset<- na.omit(dataset) #7032 obs. of 21 variables left

#Remove the Unique ID from data 
dataset[1] <- NULL # Unique ID

#Check for correlations
dataset %>%
  dplyr::select (TotalCharges, MonthlyCharges, tenure) %>%
  cor() %>%
  corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)


#CHURN- YES/NO
customer_m <- dataset %>% mutate(TotalCharges = ifelse(is.na(dataset$TotalCharges), dataset$MonthlyCharges*dataset$tenure, TotalCharges) )
ggplot(customer_m, aes(x = Churn))+
  geom_histogram(stat = "count", fill = c("red", "green")) + labs(title="Churn Values", x="Churn", y="Count")



#Check any relationship between numeric continuos features and Churn 
MonthlyCharges <- ggplot(data = dataset, aes(MonthlyCharges, color = Churn))+ #Monthly Charges
  geom_freqpoly(binwidth = 5, size = 1)

TotalCharges <- ggplot(data = dataset, aes(TotalCharges, color = Churn))+ #Total Charges
  geom_freqpoly(binwidth = 200, size = 1)

tenure <- ggplot(data = dataset, aes(tenure, colour = Churn))+ #tenure
  geom_freqpoly(binwidth = 5, size = 1)

ggarrange(MonthlyCharges, TotalCharges, tenure, 
          ncol = 3, nrow = 1)

#Check any relationship between category features and Churn
gender  <- ggplot(customer_m, aes_string(x = 'gender', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

SeniorCitizen  <- ggplot(customer_m, aes_string(x = 'SeniorCitizen', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

Partner <- ggplot(customer_m, aes_string(x = 'Partner', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

Dependents  <- ggplot(customer_m, aes_string(x = 'Dependents', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

PhoneService  <- ggplot(customer_m, aes_string(x = 'PhoneService', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

MultipleLines  <- ggplot(customer_m, aes_string(x = 'MultipleLines', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

InternetService  <- ggplot(customer_m, aes_string(x = 'InternetService', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

OnlineSecurity  <- ggplot(customer_m, aes_string(x = 'OnlineSecurity', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

ggarrange( gender, SeniorCitizen, Partner, Dependents, PhoneService, MultipleLines, InternetService, OnlineSecurity,
           ncol = 4, nrow = 2)

OnlineBackup  <- ggplot(customer_m, aes_string(x = 'OnlineBackup', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

DeviceProtection  <- ggplot(customer_m, aes_string(x = 'DeviceProtection', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

TechSupport  <- ggplot(customer_m, aes_string(x = 'TechSupport', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

StreamingTV  <- ggplot(customer_m, aes_string(x = 'StreamingTV', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

StreamingMovies  <- ggplot(customer_m, aes_string(x = 'StreamingMovies', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

Contract <- ggplot(customer_m, aes_string(x = 'Contract', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

PaperlessBilling  <- ggplot(customer_m, aes_string(x = 'PaperlessBilling', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

PaymentMethod  <- ggplot(customer_m, aes_string(x = 'PaymentMethod', fill = as.factor(customer_m$Churn)))+
  geom_bar( position = "stack")+ scale_fill_discrete(name = "churn")

ggarrange( OnlineBackup, DeviceProtection, TechSupport, StreamingTV, StreamingMovies, Contract, PaperlessBilling, PaymentMethod,
          ncol = 4, nrow = 2) #Takes 10 seconds, May also need to zoom in to see the visuals properly.'


