library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(MASS)
library(randomForest)
library(party)
library(caret)

churn <- read.csv('C:\\Users\\Uname\\Downloads\\Churn.csv') #<---- Change to user data file path
str(churn)

sapply(churn, function(x) sum(is.na(x)))

churn <- churn[complete.cases(churn), ]

cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

min(churn$tenure); max(churn$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)              

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

churn$Churn <- as.factor(churn$Churn)
churn$gender <- as.factor(churn$gender)
churn$Partner <- as.factor(churn$Partner)
churn$Dependents <- as.factor(churn$Dependents)
churn$PhoneService <- as.factor(churn$PhoneService)
churn$InternetService <- as.factor(churn$InternetService)

churn$Contract <- as.factor(churn$Contract)
churn$PaperlessBilling <- as.factor(churn$PaperlessBilling)
churn$PaymentMethod <- as.factor(churn$PaymentMethod)

intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]
str(training)

tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree)

pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); 
table(Predicted = pred_tree, Actual = testing$Churn)

p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

rfModel <- randomForest(Churn ~., data = training, ntree=200)
print(rfModel)

rfModel2 <- randomForest(Churn ~., data = training, ntree=1000)
print(rfModel2)

varImpPlot(rfModel, sort=T, n.var = 10, main = 'Top 10 Feature Importance')
