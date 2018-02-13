temp1<-tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00356/student.zip",temp1)
unzip(temp1,"student-mat.csv")

studentData1<-read.csv("student-mat.csv",sep=";",header=T)
#studentData<-read.csv2("student-mat.csv")
View(studentData)
sum(is.na(studentData))

unzip(temp1,"student-por.csv")
studentData2<-read.csv2("student-por.csv")
View(studentData2)

studentDataset<-rbind(studentData1,studentData2)
dim(studentDataset)

??prodNA()
library(missForest)
install.packages("missForest")
studentDataset<-prodNA(studentDataset,noNA=0.1)
sum(is.na(studentDataset))

######################################
??avg
studentDataset$prevScore<-NULL
studentDataset$prevScore<-(studentDataset$G1+studentDataset$G2)/2
str(studentDataset)

#Feature Engineering 
?subset
studentDataset<-subset(studentDataset,select=-c(address,famsize,
                                                nursery,Walc,G1,G2))
colnames(studentDataset)[colnames(studentDataset)=="G3"] <- "finalScore"

View(studentDataset)
studentDataset$FinalGrade<-NULL
studentDataset$FinalGrade<-factor(ifelse(studentDataset$finalScore>=median(studentDataset$finalScore),"PASS","FAIL"))

###############
#Imputing Null Values

library(DT)
datatable(head(studentDataset))

sum(is.na(studentDataset$prevScore))
studentDataset$prevScore<-ifelse(is.na(studentDataset$prevScore),0,
                                 studentDataset$prevScore)

studentDataset_imputed<-missForest(studentDataset)
studentDataset<-studentDataset_imputed$ximp
View(studentDataset)


##############################################################

#Logistic Regression
str(studentDataset_train)

str(studentData1)
str(studentData2)

######Logistic Model using Raw Data
rawDataSet <- rbind(studentData1,studentData2)
View(rawDataSet)
rawDataSet$grade<-NULL
rawDataSet$grade<-factor(ifelse(rawDataSet$G3>=median(rawDataSet$G3),"PASS","FAIL"))

str(rawDataSet)
logisticModel_rawData <- glm(grade ~ . -grade ,rawDataSet[-c(33,32,31)],family = "binomial")
summary(logisticModel_rawData)

#By Doing backward Elimination we came to know that the attributes such as address,Walc,Helth can be removed

########
#Testing and Training Samples
studentDataset_train<-studentDataset[1:700,]
dim(studentDataset_train)
studentDataset_test<-studentDataset[701:1044,]
dim(studentDataset_test)

#Plotting graphs
library(ggplot2)


print(ggplot(studentDataset, aes(x=FinalGrade))+geom_bar()+facet_grid(.~sex)+ggtitle("Result of student by Gender of Applicant"))
print(ggplot(studentDataset,aes(x=FinalGrade)) + geom_bar()+facet_grid(.~goout)+ggtitle("result of student regarding the impact of going out with friends(1-5 people)"))
print(ggplot(studentDataset,aes(x=FinalGrade)) + geom_bar()+facet_grid(.~higher)+ggtitle("result of student prediction based on the higher education plans"))


#######################Logistic Regression###################
View(studentDataset_test)
LogisticModel_1 <- glm(FinalGrade ~ . -FinalGrade,studentDataset_train[-c(27,26)],family = "binomial")
LogisticModel_1

LogisticPredict_1 <- predict(LogisticModel_1,studentDataset_test[-c(27,26)],type = "response")
summary(LogisticPredict_1)

#Here we have considered threshold value of 0.5
table(Actualvalue = studentDataset_test$FinalGrade,PredictedValue=LogisticPredict_1 >0.5)#72%

#How to find the Threshold -> ROCR

#For this we will take train data to know the threshold for that and apply for test data

predict_train <- predict(LogisticModel_1,studentDataset_train,type= "response")
library(ROCR)
ROCRPrediction <- prediction(predict_train,studentDataset_train$FinalGrade)
ROCRPerformance <- performance(ROCRPrediction,"tpr","fpr")
plot(ROCRPerformance,print.cutoffs.at=seq(0.1,by=0.1))

#Here we have considered threshold value of 0.4 by looking into the ROCR graph
table(Actualvalue = studentDataset_test$FinalGrade,PredictedValue=LogisticPredict_1 >0.4)#75%

#######################################################################

################################C50 Algorithm#################################### 

#C50 Algorithm
str(studentDataset_test)
#Basic C50 Algorithm Excluding the Previous Results
library(C50)
C50model<-C5.0(studentDataset_train[-c(26,27,28)],studentDataset_train$FinalGrade)
c50predict<-predict(C50model,studentDataset_test[-c(26,27,28)])
table(c50predict)
summary(C50model)
C50model
library(gmodels)
CrossTable(c50predict,studentDataset_test$FinalGrade)
sum(c50predict == studentDataset_test$FinalGrade) / length(studentDataset_test$FinalGrade)

########Tune C50 Algorithm#####################################
#Basic C50 Algorithm with tuning Excluding the Previous Results
C50model_tuned<-C5.0(studentDataset_train[-c(26,27,28)],studentDataset_train$FinalGrade,trials=10)
c50predict_tuned<-predict(C50model_tuned,studentDataset_test[-c(26,27,28)])
table(c50predict)
summary(C50model)
C50model_tuned

CrossTable(c50predict_tuned,studentDataset_test$FinalGrade)
sum(c50predict_tuned == studentDataset_test$FinalGrade) / length(studentDataset_test$FinalGrade)

#################################################################


##########################Prune The Tree#########################
#To know the missclasfication error excluding the Prev Results
library(tree)

str(studentDataset_test)
tree_model_1 <- tree(FinalGrade ~ . -FinalGrade,studentDataset_train[-c(26,27)])
rm(tree_model_1)  
plot(tree_model_1)
text(tree_model_1,pretty = 0)

tree_predict_1 <- predict(tree_model_1,studentDataset_test[-c(26,27)],type = "class")

#Missclassification
mean(tree_predict_1!= studentDataset_test$FinalGrade)#41%

#Prune the Tree

#>>how much level we have to go up
#>>Cross Validation to check where to stop pruning

?cv.tree
cv_tree_1 <- cv.tree(tree_model_1, FUN = prune.misclass)
names(cv_tree_1)

#size of tree = 14
#missclassification rate -> dev -> deviation

plot(cv_tree_1$size,cv_tree_1$dev,type = "b")

#Prune the Tree:

prune_model_1 <- prune.misclass(tree_model_1,best = 10)
prune_model_1

plot(prune_model_1)
text(prune_model_1,pretty = 0)

#tree size after pruning
cv_tree_pr <- cv.tree(prune_model_1, FUN = prune.misclass)
names(cv_tree_pr)

plot(cv_tree_pr$size,cv_tree_pr$dev,type = "b")

#Continuation with the model using pruning
tree_pred_2 <- predict(prune_model_1,studentDataset_test[-c(26,27)],type = "class")
table(tree_pred_2)
mean(tree_pred_2 != studentDataset_test$FinalGrade)#34%

#Model using the pruned data

CrossTable(tree_pred_2,studentDataset_test$FinalGrade,prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c("actual pass",
                                                   "predicted pass"))

sum(tree_pred_2 == studentDataset_test$FinalGrade) / length(studentDataset_test$FinalGrade)#66%

###############################################################################


###############################CART Algorthim#############################################
#CART:

################Information gain as criteria###########

#Training the Decision tree with criteria as information gain
library(caret)
trctrl <- trainControl(method = "repeatedcv",number = 10,repeats = 3)

#Syntax: train(target_variable,data_set,...)
studentDataset_train
cartModelIgain<- train(studentDataset_train[-c(26,27,28)],studentDataset_train$FinalGrade,trControl = trctrl,method = "rpart",parms = list(split = "information"))
cartModelIgain

#Predict
cartIgainPrediction <- predict(cartModelIgain,studentDataset_test)
cartIgainPrediction
#Calculating Accuracy using cofusion Marix method
confusionMatrix(cartIgainPrediction,studentDataset_test$FinalGrade)#73%

########################################################

############Training the Decision Tree classifier with criterion as gini index###########
cartModelGini <- train(studentDataset_train[-c(26,27,28)],studentDataset_train$FinalGrade,method = "rpart",parms = list(split = "gini"),trControl=trctrl,tuneLength=10)
cartModelGini
cartGiniPrediction <- predict(cartModelGini, newdata = studentDataset_test)
confusionMatrix(cartGiniPrediction,studentDataset_test$FinalGrade)#74% 

result_CART <- resamples(list(information_Gain = cartModelIgain,Gini_Index = cartModelGini))

bwplot(result_CART)

#########################################################################################

#####################################sVM(Linear and Radial)#################################################

################LINEAR KERNEL####################
ctrl <- trainControl(method="repeatedcv",number = 10,repeats = 3)

grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

#Model Devlopment:
#train(target ~ . ,method=Linear/Radial/Poly,grid,ctrl)
svmLinear_grade <- train(FinalGrade ~. - FinalGrade,studentDataset_train[-c(26,27)],method = "svmLinear",tuneGrid = grid,trainControl = ctrl)
svmLinear_grade


#Prediction now !!!!
#predict(model_trained,testDataSet)
svmLinearPrediction_grade <- predict(svmLinear_grade,studentDataset_test[-c(26,27)])
summary(svmLinearPrediction_grade)

#Accuracy using confusion matrix
#confusionMatrix(prediction object,targetvariable of test Data)
confusionMatrix(svmLinearPrediction_grade,studentDataset_test$FinalGrade)
#75% aCCURACY

####################################################


###############################RADIAL KERNEL########
#The tuning parameter grid should have columns sigma, C

grid_radial <- expand.grid(sigma = c(0.01,0,015,0.2),C= c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

svmRadialKernel_grade <- train(FinalGrade ~. - FinalGrade,studentDataset_train[-c(26,27)],method ="svmRadial",tuneGrid = grid_radial,trainControl= ctrl)
svmRadialKernel_grade
svmRadialPrediction_grade <- predict(svmRadialKernel_grade,studentDataset_test[-c(26,27)])
summary(svmRadialPrediction_grade)

confusionMatrix(svmRadialPrediction_grade,studentDataset_test$FinalGrade)
#77% ACCURACY


#########Comparision between Linear and Radial Kernel#######
result <- resamples(list(linear = svmLinear_grade,Radial = svmRadialKernel_grade))
result


summary(result)
bwplot(result)
dotplot(result)

#####################################################################

##############################Random Forest###################################
#best mtry
str(studentDataset_train)

bestmtry <- tuneRF(studentDataset_train[-c(26,27,28)],studentDataset_train$FinalGrade,stepFactor = 1.2,improve = 0.01,trace = T,plot = T)
model_forest <- randomForest(FinalGrade ~ . -FinalGrade, data = studentDataset_train[-c(26,27)],mtry = 6,ntree = 500 )
predict <- predict(model_forest,studentDataset_test[-c(26,27,28)])
confusionMatrix(predict,studentDataset_test$FinalGrade)
importance(model_forest)
varImpPlot(model_forest)
varImpPlot(model_forest)

##############################################################################

