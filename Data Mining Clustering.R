#We starting by Setting our working directory
setwd("C:/Users/Moh/Downloads")

#After setting our working directory , we going to import our dataset
House.clu <- read.csv("kc_house_data.csv",header = T)

names(House.clu)
head(House.clu)
tail(House.clu)
summary(House.clu)
str(House.clu)


#we will do clusterin on all houses with 3 bedrooms and got the maximum condition = 5
House.clu1 <- subset(House.clu,bedrooms == 3 & condition == 5)

House.clu2 <- House.clu1


#Now we will Select the required columns for clustering purpose

House.clu2 <-House.clu1[,c(3,5,6,8,12,15,17)]


#Now we will inspect our dataset
names(House.clu2)
head(House.clu2)
tail(House.clu2)
summary(House.clu2)
str(House.clu2)

house.n <- scale(House.clu2)
View(house.n)

install.packages("cluster")
library(cluster)

install.packages("factoextra")
library(factoextra)


tendency <- get_clust_tendency(house.n,30, graph = T)

tendency$plot

tendency$hopkins_stat


#Now we are going to try different k values and will compare the resultes by uso.
clu2 <- kmeans(house.n,centers = 2,iter.max = 100,nstart = 100)
clu3 <- kmeans(house.n,centers = 3,iter.max = 100,nstart = 100)
clu4 <- kmeans(house.n,centers = 4,iter.max = 100,nstart = 100)
clu5 <- kmeans(house.n,centers = 5,iter.max = 100,nstart = 100)

clu2
clu3
clu4
clu5

plot2 <- fviz_cluster(kmeans(house.n,centers = 2, iter.max = 100 , nstart = 100),data = house.n)
plot3 <- fviz_cluster(kmeans(house.n,centers = 3, iter.max = 100 , nstart = 100),data = house.n)
plot4 <- fviz_cluster(kmeans(house.n,centers = 4, iter.max = 100 , nstart = 100),data = house.n)
plot5 <- fviz_cluster(kmeans(house.n,centers = 5, iter.max = 100 , nstart = 100),data = house.n)

install.packages("gridExtra")
library(gridExtra)

grid.arrange(plot2, plot3, plot4, plot5, nrow = 2)


fviz_cluster(kmeans(house.n,centers = 3, iter.max = 100 , nstart = 100),data = house.n)



f1 <- fviz_nbclust(house.n,kmeans,method = "wss")
f2 <- fviz_nbclust(house.n,kmeans,method = "silhouette")
f3 <- fviz_nbclust(house.n,kmeans,method = "gap_stat")

grid.arrange(f1,f2,f3, nrow = 2)



clusplot(house.n,clu2$cluster,color=T , shade=T , lines=0)

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

#Now we will visualize our clusters by using the original variables and compare different variables
clu <- kmeans(house.n,centers = 3,iter.max = 100, nstart = 100)
clu
House.clu2 <- House.clu2 |> mutate(cluster = clu$cluster)
Zipcode <- House.clu2 |> ggplot(aes(x = zipcode, y = price , col = as.factor(cluster))) + geom_point()
Year <- House.clu2 |> ggplot(aes(x = yr_built, y = price , col = as.factor(cluster))) + geom_point()
Area <- House.clu2 |> ggplot(aes(x = sqft_living, y = price , col = as.factor(cluster))) + geom_point()
Bath <- House.clu2 |> ggplot(aes(x = bathrooms, y = price , col = as.factor(cluster))) + geom_point()
grid.arrange(Zipcode,Year,Area,Bath, nrow = 2)


clu <- kmeans(house.n,centers = 2,iter.max = 100, nstart = 100)
clu
House.clu2 <- House.clu2 |> mutate(cluster = clu$cluster)
Zipcode <- House.clu2 |> ggplot(aes(x = zipcode, y = price , col = as.factor(cluster))) + geom_point()
Year <- House.clu2 |> ggplot(aes(x = yr_built, y = price , col = as.factor(cluster))) + geom_point()
Area <- House.clu2 |> ggplot(aes(x = sqft_living, y = price , col = as.factor(cluster))) + geom_point()
Bath <- House.clu2 |> ggplot(aes(x = bathrooms, y = price , col = as.factor(cluster))) + geom_point()
grid.arrange(Zipcode,Year,Area,Bath, nrow = 2)


install.packages("fpc")
library(fpc)

pamk.result <- pamk(house.n)
pamk.result$nc
table(pamk.result$pamobject$clustering,House.clu3$price)


###########################################
House.clu3 <- House.clu1

House.clu3$price[House.clu3$price <= c(300000)] <- 1
House.clu3$price[(House.clu3$price > c(300000))&(House.clu3$price <= c(800000))] <- 2
House.clu3$price[House.clu3$price > c(800000)] <- 3

House.clu3$price[House.clu3$price == 1] <- "Low"
House.clu3$price[House.clu3$price == 2] <- "Medium"
House.clu3$price[House.clu3$price == 3] <- "Hight"

House.clu4 <-House.clu3[,c(3,5,6,8,12,15,17)]

House.clu4$price <- as.factor(House.clu4$price)

price <- House.clu4[,1]
House.clu4 <- House.clu4[,2:7]

house4.n <- scale(House.clu4)

House.clu4$price <- price
House.clu4<-House.clu4[,c(7,1,2,3,4,5,6)]

names(House.clu4)
head(House.clu4)
tail(House.clu4)
summary(House.clu4)
str(House.clu4)


Hclu<-kmeans(house4.n,3)
Hclu

Hclu$cluster
Hclu$totss
Hclu$size

table(House.clu4$price)

table(House.clu4$price, Hclu$cluster)

clusplot(House.clu4,Hclu$cluster,color=T , shade=T , lines=0)


#END


