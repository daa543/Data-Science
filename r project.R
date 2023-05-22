#a. Assess and clean your data if needed
install.packages("dplyr")
library(dplyr)
products <-read.csv("C:/Users/Seven/Desktop/grc.csv")
View(products)
#i. Compare cash and credit totals.
TotalByPaymentType <- group_by(products,paymentType)
TotalByPaymentType <- summarise(TotalByPaymentType, total_paymentType=sum(total))
TotalByPaymentType
pie(table(TotalByPaymentType$paymentType),main ="Cash vs. Credit totals")
#ii. Compare each age and sum of total spending.
TotalByAge <- group_by(products,age)
TotalByAge <- summarise(TotalByAge, total_age=sum(total))
TotalByAge
plot(
  x = TotalByAge$age,
  y = TotalByAge$total_age,
  main = "spending vs. Age",
  xlab = "age",
  ylab = "spending")
#iii. Show each city total spending and arrange it by total descending.
TotalBycity <- group_by(products,city)
TotalBycity <- summarise(TotalBycity, total_city=sum(total))
TotalBycity
TotalBycity<-arrange(TotalBycity,desc(total_city))
TotalBycity
barplot(
  height =TotalBycity$total_city,
  name =TotalBycity$city,
  col = "pink",
  main = "compare total spending",
  xlab = "city",
  ylab = "total_city"
)
#iv. Display the distribution of total spending.
boxplot(
  x = products$total,
  main = "Distribution of total spending",
  xlab = "customer"
)
#c. Put all previous plots in one dashboard.
par(mfrow=c(2,2))
pie(table(TotalByPaymentType$paymentType),main ="Cash vs. Credit totals")
plot(
  x = TotalByAge$age,
  y = TotalByAge$total_age,
  main = "spending vs. Age",
  xlab = "age",
  ylab = "spending")
barplot(
  height =TotalBycity$total_city,
  name =TotalBycity$city,
  col = "pink",
  main = "compare total spending",
  xlab = "city",
  ylab = "total_city"
)
boxplot(
  x = products$total,
  main = "Distribution of total spending",
  xlab = "customer"
)
#d-kmeans
library(stats)
library(dplyr)
group <- select(products,customer,age,total)
group <- group_by(group,age,customer)
group <- summarise(group,totalspend=sum(total))
total<- group[["totalspend"]]
total
years <- group[["age"]]
years 
group2 <- cbind(total,years)
rownames(group2) <- group[["customer"]]

numberOfClusters <- readline("Numbers of clusters:")
as.numeric(numberOfClusters)
if (numberOfClusters>2 & numberOfClusters<4)
{x<- kmeans(group2 , centers =numberOfClusters)
clusters <- x$cluster
clusters <- data.frame(clusters)
group_table <- cbind(group,clusters)
group_table
}else{
  print("wrong number")
}
#c-apriori algorithm
library(dplyr)
items <- select(products,items)
items
write.csv(items,"C:/Users/Seven/Desktop/items.csv")
install.packages("arules")
library(arules)
minSupport <- readline("Minimum Apriori support ")
minConfidence <- readline("Minimum Apriori confidence ")
datasetPath  <-  readline(" Dataset path")
#C:/Users/Seven/Desktop/items1.txt
x<-as.numeric(minSupport)
y<-as.numeric(minConfidence)
if ((minSupport>0.001 & minSupport<1)&(minConfidence>0.001 & minConfidence<1)){
  Path<-read.transactions(datasetPath , sep= ",")
  inspect(Path )
  apriori_rules<-apriori(Path,parameter=list(supp=x,conf=y))
  inspect(apriori_rules)
}else{ print("wrong number")}