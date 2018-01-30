# Your task
# 1. Download and visualize data (draw plots)
# 2. Get rid of nominal attributes (such as channel and region)
# 3. Apply K-means and hClust with and without outlier removal
# 4. Plot Elbow curve for different values of K (for K-means with outliers removed)
# 5. Describe your inferences

# 1. Download and visualize data (draw plots)
dataSet <- read.csv("C:\\Users\\A\\Downloads\\Wholesale Customer dataset.csv")
table(is.na(dataSet))


summary(dataSet)
View(dataSet)
str(dataSet)

library(tabplot)
tableplot(dataSet)
plot(dataSet)


library(ggplot2)
ggplot(dataSet,aes(Milk,fill=factor(Channel)))+geom_histogram(color="blue")+facet_grid(.~Region)
ggplot(dataSet,aes(Delicassen,fill=factor(Channel)))+geom_histogram(color="blue")+facet_grid(.~Region)
ggplot(dataSet,aes(Fresh,fill=factor(Channel)))+geom_histogram(color="blue")+facet_grid(.~Region)
ggplot(dataSet,aes(Detergents_Paper,fill=factor(Channel)))+geom_histogram(color="blue")+facet_grid(.~Region)
ggplot(dataSet,aes(Grocery,fill=factor(Channel)))+geom_histogram(color="blue")+facet_grid(.~Region)

##################################################################################################

#CHANNEL: customersale Channel - Horeca (Hotel/Restaurant/Cafe) or Retail channel (Nominal)
#REGION: customersale Region - Lisnon, Oporto or Other (Nominal)

#The above is important information

dataSet1 <- dataSet
dataSet1$Channel <- as.character(dataSet1$Channel)
levels(dataSet1$Channel) <- c("Hotelrestcafe","Retail")
dataSet1$Channel <- ifelse(dataSet1$Channel=="1","Horerestcafe","Retail")
levels(dataSet1$Region) <- c("Lisnon","Oporto","Other")
dataSet1$Region <- ifelse(dataSet1$Region=="1","Lisnon",ifelse(dataSet1$Region=="2","Oporto","Other"))

#Get rid of nominal attributes (such as channel and region)

dataSet.data <- dataSet1[,c(-1,-2)]
summary(dataSet.data)
str(dataSet.data)
View(dataSet.data)

head(dataSet.data)
# 3a. Apply K-means and hClust without outlier removal

plot(hclust(dist(dataSet.data)))

dataSet.cluster <- kmeans(dataSet.data[,1:6],6,nstart=20)
dataSet.cluster

library(cluster)

clusplot(dataSet.data,dataSet.cluster1$cluster, color = T, shade = T, labels = 2, lines = 0)

dataSet.pam <- pam(dataSet.data,10)
dataSet.pam
clusplot(dataSet.pam, 10)

# 3b. Apply K-means and hClust with outlier removal

dataSet2 <- dataSet1
View(dataSet2)

boxplot(dataSet2$Grocery)
q1<-quantile(dataSet2$Grocery,.25)
q3<-quantile(dataSet2$Grocery,.75)
out<-q3 + 1.5*(q3-q1)
dataSet2$Grocery<-ifelse(dataSet2$Grocery>out,.95,dataSet2$Grocery)

boxplot(dataSet2$Frozen)
q1<-quantile(dataSet2$Frozen,.25)
q3<-quantile(dataSet2$Frozen,.75)
out<-q3 + 1.5*(q3-q1)
dataSet2$Frozen<-ifelse(dataSet2$Frozen>out,.95,dataSet2$Frozen)


boxplot(dataSet2$Delicassen)
q1<-quantile(dataSet2$Delicassen,.25)
q3<-quantile(dataSet2$Delicassen,.75)
out<-q3 + 1.5*(q3-q1)
dataSet2$Delicassen<-ifelse(dataSet2$Delicassen>out,.95,dataSet2$Delicassen)

boxplot(dataSet2$Detergents_Paper)
q1<-quantile(dataSet2$Detergents_Paper,.25)
q3<-quantile(dataSet2$Detergents_Paper,.75)
out<-q3 + 1.5*(q3-q1)
dataSet2$Detergents_Paper<-ifelse(dataSet2$Detergents_Paper>out,.95,dataSet2$Detergents_Paper)


boxplot(dataSet2$Fresh)
q1<-quantile(dataSet2$Fresh,.25)
q3<-quantile(dataSet2$Fresh,.75)
out<-q3 + 1.5*(q3-q1)
dataSet2$Fresh<-ifelse(dataSet2$Fresh>out,.95,dataSet2$Fresh)


boxplot(dataSet2$Milk)
q1<-quantile(dataSet2$Milk,.25)
q3<-quantile(dataSet2$Milk,.75)
out<-q3 + 1.5*(q3-q1)
dataSet2$Milk<-ifelse(dataSet2$Milk>out,.95,dataSet2$Milk)


dataSet.data2 <- dataSet2[,c(-1,-2)]

plot(hclust(dist(dataSet.data2)))

dataSet.cluster2 <- kmeans(dataSet.data2[,1:6],6,nstart=20)
dataSet.cluster2

clusplot(dataSet.data2,dataSet.cluster2$cluster, color = T, shade = T, labels = 2, lines = 0)


# 4. Plot Elbow curve for different values of K (for K-means with outliers removed)

install.packages("factoextra")
library(factoextra)
fviz_nbclust(dataSet.data2, kmeans, method = "wss")

# 5. Describe your inferences
# We can deduce from the above Elbow Cure that the optimum k is 4

dataSet.cluster <- kmeans(dataSet.data2[,1:6],4,nstart=20)
dataSet.cluster

clusplot(dataSet.data2,dataSet.cluster3$cluster, color = T, shade = T, labels = 2, lines = 0)

