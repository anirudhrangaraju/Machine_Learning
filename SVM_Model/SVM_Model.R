

#a)Read and view data in R
#using readcsv2 instead of csv
bankDataSet <- read.csv2("C:\\Users\\A\\Downloads\\bank.csv",stringsAsFactors = F)
View(bankDataSet)

#b)Use Appropriate method to find out the Significant variables.



#c)Divide the dataset into Development and Validation Samples.
bankDataSet_new<- sample(1:nrow(bankDataSet),size = 0.3*nrow(bankDataSet))
developmentDataSet <- bankDataSet[bankDataSet_new,]
validationSampleDataSet <- bankDataSet[-bankDataSet_new,]
nrow(developmentDataSet)
nrow(validationSampleDataSet)

#d)Build SVM Model using linear Kernel and check Accuracy using Validation samples.

#Build SVM Model using linear Kernel
svmfit<-svm(factor(validationSampleDataSet$y) ~ ., data=validationSampleDataSet,kernel='linear', cost=0.1, scale=FALSE)


#tune the model for best Result
tune.out<-tune(svm,factor(validationSampleDataSet$y) ~ .,data=validationSampleDataSet,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)
bestmod$cost

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest [ ytest ==1 ,]= xtest [ ytest ==1 ,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

yhat <- predict(tune.out$best.model, testdat)
confusionMatrix(yhat, testdat$y)

#install.packages("e1071")


#e)Build SVM Model using Radial Basis Kernel and check Accuracy using Validation samples, tune the model for best Result.

#Build SVM Model using Radial Basis Kernel
Radialsvm=svm(factor(validationSampleDataSet$y) ~ .,data=validationSampleDataSet,kernel="radial",cost=5,scale=F)
Radialsvm

#Confusion matrix to ckeck the accuracy
table(predicted=Radialsvm$fitted,actual=validationSampleDataSet$y)

#misclassification Rate
mean(Radialsvm$fitted!=validationSampleDataSet$y)*100##24% wrong predictions

#tune the model for best Result[need to modify]

names(validationSampleDataSet)
tuneResult <- tune(svm, y ~ .,  data = validationSampleDataSet,cost = 2^(2:9))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)


#2 Question and Answer
mushroomDataSet <- read.csv("C:\\Users\\A\\Downloads\\mushrooms.csv",stringsAsFactors = F)
View(mushroomDataSet)

sum(is.na(mushroomDataSet))


#b)Use Appropriate method to find out the Significant variables.


#c)Divide the dataset into Development and Validation Samples.
mushroomDataSet_new<- sample(1:nrow(mushroomDataSet),size = 0.3*nrow(mushroomDataSet))
developmentDataSet <- bankDataSet[mushroomDataSet_new,]
validationSampleDataSet <- bankDataSet[-mushroomDataSet_new,]
nrow(developmentDataSet)
nrow(validationSampleDataSet)

#d)Build SVM Model using linear Kernel and check Accuracy using Validation samples.

#Build SVM Model using linear Kernel
svmfit<-svm(factor(validationSampleDataSet) ~ ., data=validationSampleDataSet,kernel='linear', cost=0.1, scale=FALSE)


#tune the model for best Result
tune.out<-tune(svm,factor(validationSampleDataSet$y) ~ .,data=validationSampleDataSet,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)
bestmod$cost

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest [ ytest ==1 ,]= xtest [ ytest ==1 ,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

yhat <- predict(tune.out$best.model, testdat)
confusionMatrix(yhat, testdat$y)

#install.packages("e1071")


#e)Build SVM Model using Radial Basis Kernel and check Accuracy using Validation samples, tune the model for best Result.

#Build SVM Model using Radial Basis Kernel
Radialsvm=svm(factor(validationSampleDataSet$y) ~ .,data=validationSampleDataSet,kernel="radial",cost=5,scale=F)
Radialsvm

#Confusion matrix to ckeck the accuracy
table(predicted=Radialsvm$fitted,actual=validationSampleDataSet$y)

#misclassification Rate
mean(Radialsvm$fitted!=validationSampleDataSet$loan)*100##24% wrong predictions

#tune the model for best Result[need to modify]

names(validationSampleDataSet)
tuneResult <- tune(svm, y ~ .,  data = validationSampleDataSet,cost = 2^(2:9))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

