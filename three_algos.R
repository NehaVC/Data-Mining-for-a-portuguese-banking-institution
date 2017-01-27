setwd("C:/Users/Neha/Documents/737/Project")
training_data <- read.csv("bank-additional-full-new.csv")
test_data <- read.csv("bank_test_data.csv")

#Data Preprocessing

training_data$TDA_group[training_data$term_deposit_amount< 5000] <- "<5000"
training_data$TDA_group[training_data$term_deposit_amount>= 5000 & training_data$term_deposit_amount<=10000] <- "5000-10000"
training_data$TDA_group[training_data$term_deposit_amount>= 10001 & training_data$term_deposit_amount<=15000] <- "10001-15000"
training_data$TDA_group[training_data$term_deposit_amount>= 15001 & training_data$term_deposit_amount<=20000] <- "15001-20000"
training_data$TDA_group[training_data$term_deposit_amount>= 20001 & training_data$term_deposit_amount<=25000] <- "20001-25000"
training_data$TDA_group[training_data$term_deposit_amount> 25000] <- ">25000"

training_data$age_group[training_data$age < 18] <- "<18"
training_data$age_group[training_data$age >= 18 & training_data$age<=24] <- "18-24"
training_data$age_group[training_data$age >= 25 & training_data$age<=34] <- "25-34"
training_data$age_group[training_data$age >= 35 & training_data$age<=44] <- "35-44"
training_data$age_group[training_data$age >= 45 & training_data$age<=54] <- "45-54"
training_data$age_group[training_data$age >= 55 & training_data$age<=64] <- "55-64"
training_data$age_group[training_data$age > 64] <- ">64"

training_data$balance_group[training_data$balance < 5000] <- "<5000"
training_data$balance_group[training_data$balance >= 5000 & training_data$balance<=10000] <- "5000-10000"
training_data$balance_group[training_data$balance >= 10001 & training_data$balance<=15000] <- "10001-15000"
training_data$balance_group[training_data$balance >= 15001 & training_data$balance<=20000] <- "15001-20000"
training_data$balance_group[training_data$balance >= 20001 & training_data$balance<=25000] <- "20001-25000"
training_data$balance_group[training_data$balance > 25000] <- ">25000"

test_data$TDA_group[test_data$term_deposit_amount< 5000] <- "<5000"
test_data$TDA_group[test_data$term_deposit_amount>= 5000 & test_data$term_deposit_amount<=10000] <- "5000-10000"
test_data$TDA_group[test_data$term_deposit_amount>= 10001 & test_data$term_deposit_amount<=15000] <- "10001-15000"
test_data$TDA_group[test_data$term_deposit_amount>= 15001 & test_data$term_deposit_amount<=20000] <- "15001-20000"
test_data$TDA_group[test_data$term_deposit_amount>= 20001 & test_data$term_deposit_amount<=25000] <- "20001-25000"
test_data$TDA_group[test_data$term_deposit_amount> 25000] <- ">25000"

test_data$age_group[test_data$age < 18] <- "<18"
test_data$age_group[test_data$age >= 18 & test_data$age<=24] <- "18-24"
test_data$age_group[test_data$age >= 25 & test_data$age<=34] <- "25-34"
test_data$age_group[test_data$age >= 35 & test_data$age<=44] <- "35-44"
test_data$age_group[test_data$age >= 45 & test_data$age<=54] <- "45-54"
test_data$age_group[test_data$age >= 55 & test_data$age<=64] <- "55-64"
test_data$age_group[test_data$age > 64] <- ">64"

test_data$balance_group[test_data$balance < 5000] <- "<5000"
test_data$balance_group[test_data$balance >= 5000 & test_data$balance<=10000] <- "5000-10000"
test_data$balance_group[test_data$balance >= 10001 & test_data$balance<=15000] <- "10001-15000"
test_data$balance_group[test_data$balance >= 15001 & test_data$balance<=20000] <- "15001-20000"
test_data$balance_group[test_data$balance >= 20001 & test_data$balance<=25000] <- "20001-25000"
test_data$balance_group[test_data$balance > 25000] <- ">25000"

newtrainingdata <- training_data[ which(training_data$y=='yes'), ]
newtestdata <- test_data[ which(test_data$y=='yes'), ]

index = sample(2, nrow(training_data), replace=TRUE, prob=c(0.8, 0.2))
trainsubsetData = training_data[index==1, ]
testsubsetData = training_data[index==2, ]

#Retrieve the dimension of an object(matrix, array or data frame).
dim(trainsubsetData)
dim(testsubsetData)



index = sample(2, nrow(newtrainingdata), replace=TRUE, prob=c(0.8, 0.2))
newtrainsubsetData = newtrainingdata[index==1, ]
newtestsubsetData = newtrainingdata[index==2, ]


ggplot(newtrainingdata,aes(job, fill = y)) + geom_bar()

library(rpart)
library(caret)
library(e1071)
library(data.table)

#Decision tree

decisionTree_perf <- function(table_name, var_name, model_name, data_name) {
  pred_tree <- predict(model_name, data_name)
  
  predictions <- data.table(cbind(data_name$y,pred_tree))
  predictions[, predict := ifelse(no > yes, 1, 2)]
  misClassificError <- mean(predictions$V1 != predictions$predict)
  
   accr<- 1-misClassificError
  result_perc <- cbind(var_name,accr)
  table_name <- rbind(table_name, result_perc)
  return(table_name)
}


result1 <- data.frame("Parameters" = character(), "Accuracy" = numeric())
result1 <- decisionTree_perf(result1, "job+education+balance", rpart(y ~ job+education+balance, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "job+education+balance+age_group+poutcome", rpart(y ~ job+education+balance+age_group+poutcome, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "education", rpart(y ~ education, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "job+education+poutcome+age_group+balance_group", rpart(y ~ job+education+poutcome+age_group+balance_group, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "age+poutcome", rpart(y ~age+poutcome, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "age+nr.employed", rpart(y ~age+nr.employed, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "duration+emp.var.rate", rpart(y ~duration+emp.var.rate, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "duration+balance", rpart(y ~duration+balance, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "duration+balance+poutcome", rpart(y ~duration+balance+poutcome, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "job+education+balance", rpart(y ~job+education+balance, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "job+age+loan", rpart(y ~job+age+loan, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)
result1 <- decisionTree_perf(result1, "job+age+loan+previous", rpart(y ~job+age+loan+previous, data=trainsubsetData, control = rpart.control(minsplit = 1)), testsubsetData)


write.csv(result1,"Accuracy of all Decision Tree Models.csv")

#Final decision tree model
fmd<-rpart(y ~., data=trainsubsetData, control = rpart.control(minsplit = 1))

  pred_tree <- predict(fmd, testsubsetData)
  
  predictions <- data.table(cbind(testsubsetData$y,pred_tree))
  predictions[, predict := ifelse(no > yes, 1, 2)]
  misClassificError <- mean(predictions$V1 != predictions$predict)
  
  accr<- 1-misClassificError
  result_perc <- cbind('All Factors',accr)
View(result_perc)

#Lift curve for Decision Tree
bb=cbind(predictions$V1,predictions$predict)
bb1=bb[order(predictions$V1,decreasing=TRUE),]

# Order cases in test set according to their success prob
# actual outcome shown next to it
# overall success prob in the evaluation data set
xbar=mean(predictions$predict)
xbar
n2=length(predictions$predict)
# Calculating the lift
# cumulative 1's sorted by predicted values
# cumulative 1's using the average success prob from evaluation set
axis=dim(n2)
ax=dim(n2)
ay=dim(n2)
axis[1]=1
ax[1]=xbar
ay[1]=bb1[1,2]
for (i in 2:n2) {
  axis[i]=i
  ax[i]=xbar*i
  ay[i]=ay[i-1]+bb1[i,2]
}
aaa=cbind(bb1[,1],bb1[,2],ay,ax)
aaa[1:100,]

plot(axis,ay,
     type="l", col="blue", lwd=2,
     xlab="Number of cases",
     ylab="Number of successes",
     main="Lift: Cum successes sorted by pred value/success probability for Decision tree model")
points(axis,ax,type="l",lwd=2)





#Logistic Regression

training_data$y_numeric <- as.numeric(training_data$y)
training_data$y_numeric <- ifelse(training_data$y_numeric == 1,0,1)
test_data$y_numeric <- as.numeric(test_data$y)
test_data$y_numeric <- ifelse(test_data$y_numeric == 1,0,1)
training_data_glm <-training_data
test_data_glm <-test_data

LRmodel_glm_1 <- glm(y_numeric ~ education, family = binomial(link = 'logit'), data = training_data_glm)
fitted.results_glm_1 <- predict(LRmodel_glm_1, test_data_glm, type = 'response')
fitted.results_glm_1 <- ifelse(fitted.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(fitted.results_glm_1 != test_data_glm$y_numeric)
misClassificError_glm_1
print(paste('Accuracy (education)',1 - misClassificError_glm_1))
LRmodel_glm_1 <- glm(y_numeric ~ .+poutcome, family = binomial(link = 'logit'), data = training_data_glm)
fitted.results_glm_1 <- predict(LRmodel_glm_1, test_data_glm, type = 'response')
fitted.results_glm_1 <- ifelse(fitted.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(fitted.results_glm_1 != test_data_glm$y_numeric)
misClassificError_glm_1
print(paste('Accuracy (balance_group+age_group)',1 - misClassificError_glm_1))
LRmodel_glm_1 <- glm(y_numeric ~ job+education+balance, family = binomial(link = 'logit'), data = training_data_glm)
fitted.results_glm_1 <- predict(LRmodel_glm_1, test_data_glm, type = 'response')
fitted.results_glm_1 <- ifelse(fitted.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(fitted.results_glm_1 != test_data_glm$y_numeric)
misClassificError_glm_1
print(paste('Accuracy (job+education+balance)',1 - misClassificError_glm_1))

LRmodel_glm_1 <- glm(y_numeric ~ job+education+balance+age_group+poutcome, family = binomial(link = 'logit'), data = training_data_glm)
fitted.results_glm_1 <- predict(LRmodel_glm_1, test_data_glm, type = 'response')
fitted.results_glm_1 <- ifelse(fitted.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(fitted.results_glm_1 != test_data_glm$y_numeric)
misClassificError_glm_1
print(paste('Accuracy (job+education+balance+age_group+poutcome)',1 - misClassificError_glm_1))

LRmodel_glm_1 <- glm(y_numeric ~ age+nr.employed, family = binomial(link = 'logit'), data = training_data_glm)
fitted.results_glm_1 <- predict(LRmodel_glm_1, test_data_glm, type = 'response')
fitted.results_glm_1 <- ifelse(fitted.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(fitted.results_glm_1 != test_data_glm$y_numeric)
misClassificError_glm_1
print(paste('Accuracy (age+nr.employed)',1 - misClassificError_glm_1))

LRmodel_glm_1 <- glm(y_numeric ~ duration+emp.var.rate, family = binomial(link = 'logit'), data = training_data_glm)
fitted.results_glm_1 <- predict(LRmodel_glm_1, test_data_glm, type = 'response')
fitted.results_glm_1 <- ifelse(fitted.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(fitted.results_glm_1 != test_data_glm$y_numeric)
misClassificError_glm_1
print(paste('Accuracy (duration+emp.var.rate)',1 - misClassificError_glm_1))

LRmodel_glm_1 <- glm(y_numeric ~ duration+term_deposit_amount, family = binomial(link = 'logit'), data = training_data_glm)
fitted.results_glm_1 <- predict(LRmodel_glm_1, test_data_glm, type = 'response')
fitted.results_glm_1 <- ifelse(fitted.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(fitted.results_glm_1 != test_data_glm$y_numeric)
misClassificError_glm_1
print(paste('Accuracy (duration+balance_group)',1 - misClassificError_glm_1))

LRmodel_glm_1 <- glm(y_numeric ~ duration+term_deposit_amount+poutcome, family = binomial(link = 'logit'), data = training_data_glm)
fitted.results_glm_1 <- predict(LRmodel_glm_1, test_data_glm, type = 'response')
fitted.results_glm_1 <- ifelse(fitted.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(fitted.results_glm_1 != test_data_glm$y_numeric)
misClassificError_glm_1
print(paste('Accuracy (duration+balance_group+poutcome)',1 - misClassificError_glm_1))




#Best Logistic Regression Model
LRmodel_glm_1 <- glm(y_numeric ~ duration+term_deposit_amount+poutcome, family = binomial(link = 'logit'), data = training_data_glm)
fitted.results_glm_1 <- predict(LRmodel_glm_1, test_data_glm, type = 'response')
fitted.results_glm_1 <- ifelse(fitted.results_glm_1 > 0.5,1,0)
misClassificError_glm_1 <- mean(fitted.results_glm_1 != test_data_glm$y_numeric)
misClassificError_glm_1
print(paste('Accuracy (duration+balance+poutcome)',1 - misClassificError_glm_1))

#Lift curve

bb=cbind(fitted.results_glm_1,test_data_glm$y_numeric)
bb1=bb[order(fitted.results_glm_1,decreasing=TRUE),]

# Order cases in test set according to their success prob
# actual outcome shown next to it
# overall success  prob in the evaluation data set
xbar=mean(test_data_glm$y_numeric)
xbar
n2=length(test_data_glm$y_numeric)
# Calculating the lift
# cumulative 1's sorted by predicted values
# cumulative 1's using the average success prob from evaluation set
axis=dim(n2)
ax=dim(n2)
ay=dim(n2)
axis[1]=1
ax[1]=xbar
ay[1]=bb1[1,2]
for (i in 2:n2) {
  axis[i]=i
  ax[i]=xbar*i
  ay[i]=ay[i-1]+bb1[i,2]
}
aaa=cbind(bb1[,1],bb1[,2],ay,ax)
aaa[1:100,]

plot(axis,ay,
     type="l", col="blue", lwd=2,
     xlab="Number of cases",
     ylab="Number of successes",
     main="Lift: Cum successes sorted by pred value/success probability for Logistic Regression")
points(axis,ax,type="l",lwd=2)



library(e1071)
library(cwhmisc)


# a function to predict for SVM model
record_performance <- function(df, name, model, test) {
  svm.pred <- predict(model, test)
  svm.table <- table(pred = svm.pred, true=test$y)
  df <- rbind(df, data.frame(model=c(name), score=c(classAgreement(svm.table)$diag)))
  return(df)
}



results3 <- data.frame("Parameters" = character(), "Accuracy" = numeric())
results3 <- record_performance(results3, "balance", svm(y ~ balance, data=trainsubsetData), testsubsetData)
results3 <- record_performance(results3, "balance + education", svm(y ~ balance + education, data=trainsubsetData), testsubsetData)
results3 <- record_performance(results3, "balance + education +job", svm(y ~ balance + education +job, data=training_data), test_data)
results3 <- record_performance(results3, "age", svm(y ~ age, data=training_data), test_data)
results3 <- record_performance(results3, "job+education+balance+age_group+poutcome", svm(y ~job+education+balance+age_group+poutcome, data=training_data), test_data)
results3 <- record_performance(results3, "age+poutcome", svm(y ~age+poutcome, data=training_data), test_data)
results3 <- record_performance(results3, "age+nr.employed", svm(y ~age+nr.employed, data=training_data), test_data)
results3 <- record_performance(results3, "duration+emp.var.rate", svm(y ~duration+emp.var.rate, data=training_data), test_data)
results3 <- record_performance(results3, "duration+balance_group", svm(y ~duration+balance_group, data=training_data), test_data)
results3 <- record_performance(results3, "duration+balance_group+poutcome", svm(y ~duration+balance_group+poutcome, data=training_data), test_data)

write.csv(results3,"Accuracy of all SVM Models.csv")



#final svm model
fsm <- svm(y ~ duration+term_deposit_amount+poutcome, data=training_data)
svm.pred <- predict(fsm, test_data_svm)
svm.table <- table(pred = svm.pred, true=test_data$y)
View(svm.table)
df <- rbind(data.frame(model=c('duration+balancet+poutcome'), score=c(classAgreement(svm.table)$diag)))
View(df)



bb=cbind(svm.pred,test_data$y)
bb1=bb[order(svm.pred,decreasing=TRUE),]

# Order cases in test set according to their success prob
# actual outcome shown next to it
# overall success prob in the evaluation data set
xbar=mean(svm.pred)
xbar=1.24
n2=length(svm.pred)
# Calculating the lift
# cumulative 1's sorted by predicted values
# cumulative 1's using the average success prob from evaluation set
axis=dim(n2)
ax=dim(n2)
ay=dim(n2)
axis[1]=1
ax[1]=xbar
ay[1]=bb1[1,2]
for (i in 2:n2) {
  axis[i]=i
  ax[i]=xbar*i
  ay[i]=ay[i-1]+bb1[i,2]
}
aaa=cbind(bb1[,1],bb1[,2],ay,ax)
aaa[1:100,]

plot(axis,ay,
     type="l", col="blue", lwd=2,
     xlab="Number of cases",
     ylab="Number of successes",
     main="Lift: Cum successes sorted by pred value/success probability for SVM model")
points(axis,ax,type="l",lwd=2)












