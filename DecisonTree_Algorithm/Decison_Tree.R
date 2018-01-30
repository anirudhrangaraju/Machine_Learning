#Task
#1. Split data into training and test datasets [70:30]
#2. Build a decision tree using C50,rpart and Random Forest Model and identify the top three predictive features
#3. Predict outcomes on the test data
#4. Create confusion matrix and print the matrix (use CrossTable function to generate confusion matrix)
#5. Try improving the performance of the decision tree (set the trials parameter in C50 and nTrees in Random Forest or you can do some data pre-processing)
#6. C50 and Random Forest by default handles missing data. Randomly remove few features for some of the training samples and build the decision tree.
#Create the confusion matrix and compare the performance of both the algorithms with and without missing data. (See na.action to understand missing features


#PRE-REQUISITE BEFORE TASKS:
creditDataSet <- read.csv("C:\\Users\\A\\Downloads\\credit.csv")


#Converting integer to categorical values in "default" variable of the Credit Data Set.
creditDataSet$default[creditDataSet$default==2] <- "Yes"
creditDataSet$default[creditDataSet$default==1] <- "No"
creditDataSet$default <- as.factor(creditDataSet$default)

# Visualizing the dataset
View(creditDataSet)
dim(creditDataSet) # 1000 rows and 21 variables
summary(creditDataSet)
sum(is.na(creditDataSet))

summary(creditDataSet$months_loan_duration)

table(creditDataSet$default)
table(credit$checking_balance)
table(credit$savings_balance)


#1. Split data into training and test datasets [70:30]
set.seed(125)
dt = sort(sample(nrow(creditDataSet), nrow(creditDataSet)*0.7))
creditTrain <- creditDataSet[dt,]
dim(creditTrain)
creditTest <- creditDataSet[-dt,]
dim(creditTest)

prop.table(table(creditTest$default)) # ratio of yes/no in Test Set
prop.table(table(creditTrain$default)) # ratio of yes/no in Train Set

#2. Build a decision tree using C50,rpart and Random Forest Model and identify the top three predictive features
install.packages("C50")
library(C50)

install.packages("gmodels")
library(gmodels)


### change levels of $default
creditDataSet$default<- factor(creditDataSet$default, levels = c("1", "2"),
                        labels = c("no", "yes"))

creditModel <- C5.0(creditTrain[-17], creditTrain$default)

### see some basic data about the tree
credit_model
summary(credit_model)


#3. Predict outcomes on the test data
creditPrediction <- predict(creditModel, creditTest)
creditPrediction

#4. Create confusion matrix and print the matrix (use CrossTable function to generate confusion matrix)
CrossTable(creditTest$default, creditPrediction,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#5. Try improving the performance of the decision tree (set the trials parameter in C50 and nTrees in Random Forest or you can do some data pre-processing)

creditBoost <- C5.0(creditTrain[-17], creditTrain$default,
                       trials = 10)

creditBoost
summary(creditBoost)

creditBoostPrediction <- predict(creditBoost, creditTest)
CrossTable(creditTest$default, creditBoostPrediction,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

  



