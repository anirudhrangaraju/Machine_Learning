#install.packages("markdown")
#install.packages("DT")
#install.packages("ggplot")
#install.packages("c50")
#install.packages("rpart")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("Fselector)
#install.packages("grid")
#install.packages("libcoin")
#install.packages("mvtnorm")
#install.packages("rpart")


#1)Data Setup

#Read the Data Set by URL instead of your local machine
temp <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00356/student.zip",temp,mode = "wb")
unzip(temp,"student-mat.csv")

#View the Data
studentDataSet <- read.csv("student-mat.csv",sep = ";",header = T)
unlink(temp)

#2)Let us Take a view of the Data Set:
View(studentDataSet)
str(studentDataSet)
head(studentDataSet)
nrow(studentDataSet)

datatable(studentDataSet,class = "hover")

#3)Feature Engneering

#Introducing new feature to predict PASS or FAIL
studentDataSet$finalGrade <- NULL
studentDataSet$finalGrade <- factor(ifelse(studentDataSet$G3>=median(studentDataSet$G3), 1, 0),labels = c("pass","fail"))

#Feature Selection using information gain in R

library(FSelector)

#syntax:
#best_feature <- information.gain(formulae,data)

best_feature <- information.gain(finalGrade ~ .,studentDataSet)
print(best_feature)
cutoff.biggest.diff(best_feature)

#Hitogram of G3
hist(studentDataSet$G3)


#checking for Null Values
sum(is.na(studentDataSet))


#Normalization(to scale the attributes to (0-1) for the model accuracy)
#Min-Max Normalization
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

studentDataSet_subset$G1 <- normalise(studentDataSet_subset$G1)
studentDataSet_subset$G2 <- normalise(studentDataSet_subset$G2)
studentDataSet_subset$goout <- normalise(studentDataSet_subset$goout)
studentDataSet_subset$absences <- normalise(studentDataSet_subset$absences)
studentDataSet_subset$failures <- normalise(studentDataSet_subset$failures)
studentDataSet_subset$Fedu <- normalise(studentDataSet_subset$Fedu)
studentDataSet_subset$Medu <- normalise(studentDataSet_subset$Medu)
studentDataSet_subset$health <- normalise(studentDataSet_subset$health)
studentDataSet_subset$Walc <- normalise(studentDataSet_subset$Walc)
studentDataSet_subset$Dalc <- normalise(studentDataSet_subset$Dalc)
studentDataSet_subset$traveltime <- normalise(studentDataSet_subset$traveltime)
studentDataSet_subset$studytime <- normalise(studentDataSet_subset$studytime)


#Data Exploraion:

library(ggplot2)

?ggplot
print(ggplot(studentDataSet_subset, aes(x=finalGrade))+geom_bar()+facet_grid(.~sex)+ggtitle("Result of student by Gender of Applicant"))
print(ggplot(studentDataSet_subset,aes(x=finalGrade)) + geom_bar()+facet_grid(.~goout)+ggtitle("result of student regarding the impact of going out with friends(1-5 people)"))
print(ggplot(studentDataSet_subset,aes(x=finalGrade)) + geom_bar()+facet_grid(.~goout)+ggtitle("result of student regarding the impact of going out with friends(1-5 people)"))
print(ggplot(studentDataSet_subset,aes(x=finalGrade)) + geom_bar()+facet_grid(.~higher)+ggtitle("result of student prediction based on the higher education plans"))

#The number of students who has passes/failed in the Exam.
table(studentDataSet$finalGrade)

#Train and Test Sample

studentDataSet_train <- studentDataSet[1:295,]
studentDataSet_test <- studentDataSet[295:395,]

str(studentDataSet_test)
str(studentDataSet_train)

#Creating a Model using Decison Tree Algorithm

library("partykit")
# Create the tree.
output.tree <- ctree(finalGrade ~ ., data = studentDataSet)
plot(output.tree)

#syntax:
#C5.0(train_data_set_excluding(target_variable and G3),target_variable)
#We are excluding G3 as well,since it is aso indirect target variable

library(C50)
studentDataSet_model <- C5.0(studentDataSet_train[-c(33,34)],studentDataSet_train$finalGrade)
studentDataSet_model

#Decison Tree Model can be Viewed below by using Train Data Set.
summary(studentDataSet_model)

#Syntax:
#predict(model_outcome,train_data)
prediction_using_train_model <- predict(studentDataSet_model,studentDataSet_train)
table(prediction_using_train_model)

#To know our Model Accuracy in terms of actual pass/fail and predicted pass/fail for Train Data

#crossTable(prediction_outcome,target_variable,other attributes ..)
CrossTable(prediction_using_train_model,studentDataSet_train$finalGrade,prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c("actual pass",
                                                   "predicted pass"))

#Inference:
#Model incorrectly predicted total 11 outcomes out of 295 observation using Train Data

#Noe let us predict using the test Data

prediction_using_test_model <- predict(studentDataSet_model,studentDataSet_test)
table(prediction_using_test_model)

#To know the Model Accuracy in terms of actual pass/fail and predicted pass/fail for Test Data
CrossTable(prediction_using_test_model,studentDataSet_test$finalGrade,prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c("actual pass",
                                                   "predicted pass"))

#Inference:
#Model incorrectly predicted total 12 outcomes out of 101 observation using Test Data

#We can also use the trials parameter in C50 Algorithm for model accuracy
library(C50)
library(gmodels)
studentDataSet_model_boost <- C5.0(studentDataSet_train[-c(33,34)],studentDataSet_train$finalGrade)
prediction_using_test_model_boost <- predict(studentDataSet_model_boost,studentDataSet_test)
CrossTable(prediction_using_test_model,studentDataSet_test$finalGrade,prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c("actual pass",
                                                   "predicted pass"))






#Let us view the decison tree in graph
library(rpart)
library(rpart.plot)

tree.model <- rpart(G3~.,studentDataSet_test)
rpart.plot(tree.model)

#Let us view the decison tree in graph excluding the top 3 features(G1,G2,G3) to understand
tree.model_plot_extra <- rpart(data = studentDataSet_train[-c(31,32,33)], formula = finalGrade ~ .)
tree.model_plot_extra
rpart.plot(tree.model_plot_extra,type = 3,digits = 3,fallen.leaves = T)

#Model Development using CARET Package 

#Training the Decision tree with criterian as information gain


trctrl <- trctrl <- trainControl(method = "repeatedcv",number = 10,repeats = 3)

#Syntax: train(target_variable,data_set,...)

studentDataSet_model_2<- train(studentDataSet_train[-c(33,34)],studentDataSet_train$finalGrade,trControl = trctrl,method = "rpart",parms = list(split = "information"))
studentDataSet_model_2

#Predict
prediction_using_model_2 <- predict(studentDataSet_model_2,studentDataSet_test)
table(prediction_using_model_2)

#Calculating Accuracy using cofusion Marix method

confusionMatrix(prediction_using_model_2,studentDataSet_test$finalGrade)


#Training the Decision Tree classifier with criterion as gini index

student_model_gini <- train(studentDataSet_train[-c(33,34)],studentDataSet_train$finalGrade,method = "rpart",parms = list(split = "gini"),trControl=trctrl,tuneLength=10)
degree_fit_gini
?train

test_pred_gini <- predict(student_model_gini, newdata = studentDataSet_test)
summary(test_pred_gini)

confusionMatrix(test_pred_gini, studentDataSet_test$finalGrade) 

#Comparing the result obtained from criteria as gini index and information gain for accuracy of the data_set

result <- resamples(list(informaton_gain=studentDataSet_model_2,gini=student_model_gini))
summary(result)

bwplot(result)

?ctree


















?train()




























