test<-read.csv("C:\\Users\\user\\Downloads\\test.csv")
View(test)
train<-read.csv("C:\\Users\\user\\Downloads\\train.csv")


str(train)
str(test)

#Train Data Set
summary(train)


#LotFrontage,Alley,MasVnrType,MasVnrArea,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,
#Electrical,FireplaceQu,GarageType,GarageCond,GarageFinish,GarageQual,Fence,MiscFeature,poolQC

#72,73,74,75
#poolQC,Fence,misc

#TRAIN

#Replacing missing values in Fence with "MISSING" Label
trainFenceWithoutNA <- as.character(train$Fence)
train$fence <- ifelse(is.na(trainFenceWithoutNA),"MISSING", trainFenceWithoutNA)
train$fence

#Replacing missing values in poolQC with "MISSING" Label
poolQCWithoutNA <- as.character(train$PoolQC)
train$PoolQC <- ifelse(is.na(poolQCWithoutNA),"MISSING", poolQCWithoutNA)
train$PoolQC

#Replacing missing values in Fence with "MISSING" Label
trainMiscFeatureWithoutNA <- as.character(train$MiscFeature)
train$MiscFeature<- ifelse(is.na(trainMiscFeatureWithoutNA),"MISSING", trainMiscFeatureWithoutNA)
train$MiscFeature

#Replacing missing values in Fence with "MISSING" Label
trainAlleyWithoutNA <- as.character(train$Alley)
train$Alley<- ifelse(is.na(trainAlleyWithoutNA),"MISSING", trainMiscFeatureWithoutNA)
train$Alley

cleanedDataSet_train <- subset(train, select =c(LotFrontage,LotArea,Street,Alley,Utilities,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,TotalBsmtSF,CentralAir,BedroomAbvGr,
                                                GarageArea,OpenPorchSF,PoolArea,SaleType,SaleCondition,Fence,PoolQC,MiscFeature))
View(cleanedDataSet_train)

summary(cleanedDataSet_train)



########################

#TEST


#Replacing missing values in Fence with "MISSING" Label
testFenceWithoutNA <- as.character(test$Fence)
test$fence <- ifelse(is.na(testFenceWithoutNA),"MISSING", testFenceWithoutNA)
test$fence

#Replacing missing values in poolQC with "MISSING" Label
testpoolQCWithoutNA <- as.character(test$PoolQC)
test$PoolQC <- ifelse(is.na(testpoolQCWithoutNA),"MISSING", testpoolQCWithoutNA)
test$PoolQC

#Replacing missing values in Fence with "MISSING" Label
testMiscFeatureWithoutNA <- as.character(test$MiscFeature)
test$MiscFeature<- ifelse(is.na(testMiscFeatureWithoutNA),"MISSING", testMiscFeatureWithoutNA)
test$MiscFeature

#Replacing missing values in Fence with "MISSING" Label
testAlleyWithoutNA <- as.character(test$Alley)
test$Alley<- ifelse(is.na(testAlleyWithoutNA),"MISSING", testAlleyWithoutNA)
test$Alley

cleanedDataSet_test <- subset(test,  select =c(LotFrontage,LotArea,Street,Alley,Utilities,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,TotalBsmtSF,CentralAir,BedroomAbvGr,
                                               GarageArea,OpenPorchSF,PoolArea,SaleType,SaleCondition,Fence,PoolQC,MiscFeature))
View(cleanedDataSet_test)

summary(cleanedDataSet_test)

class(cleanedDataSet$LotArea)

########################################


imputed <- subset(cleanedDataSet_test, select = c(SaleType,LotFrontage))
View(imputed)

#imputation technique for Item Weight
(imputedData <- mice(imputed,m = 3,maxit = 5,method = 'pmm'))

#Ti view the Item weight data set
View(imputedData$imp$LotFrontage)

#To get the complete Data of the 2nd iteration used above

completeData <- complete(imputedData,2)
View(completeData)
str(completeData)
summary(completeData)
#To store the data in local which has imputed values
write.csv(x = completeData ,file ="mydata.csv")
getwd()

a <- subset(cleanedDataSet_test, select = -c(SaleType,LotFrontage))
write.csv(a,"new.csv")

imputed_withoutLotFrontage<- read.csv("C:\\Users\\user\\Documents\\new.csv")
imputed_withLotFrontage <- read.csv("C:\\Users\\user\\Documents\\mydata.csv")


datafile <- cbind(imputed_withoutLotFrontage,imputed_withLotFrontage)

final<- write.csv(datafile,"final.csv")

#########################################################

y <- as.matrix(train$Id,train$SalePrice)
x <- as.matrix(cleanedDataSet_test[c(1:80)])
Model1 <- lm(y ~ x[,80])
Model1

salePrice<- predict(Model1,cleanedDataSet_test)


write.csv(salePrice,"saleprice.csv")

##########################################################


y <- as.matrix(train$Id, train$SalePrice)
x <- as.matrix(cleanedDataSet_test[,c(1:80)])
Model1 <- lm(y ~ x[,c(1:80)])
Model1

salePrice<- predict(Model1,cleanedDataSet_test)


write.csv(salePrice,"saleprice_1.csv")