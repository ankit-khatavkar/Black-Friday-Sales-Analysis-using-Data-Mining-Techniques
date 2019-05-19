
install.packages("plotly")
install.packages("factoextra")
#libraries
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(openxlsx)
library(cluster)
library(factoextra)
library(plotly)
library(ggplot2)

dir.create("D:/NEU/Data Mining/Assignments/Final Project/Solution/Code/R", showWarnings = TRUE, recursive = FALSE, mode = "0777")
setwd("D:/NEU/Data Mining/Assignments/Final Project/Solution/Code/R")
getwd()
BlackFriday_Data<- read.csv("D:/NEU/Data Mining/Assignments/Final Project/Solution/Code/BlackFriday.csv")


head(BlackFriday_Data)
str(BlackFriday_Data)
summary(BlackFriday_Data)


#Black Friday Distribution by total purchase by buyers
colnames(BlackFriday_Data)
t<-group_by(BlackFriday_Data,User_ID)
t
summarise(BlackFriday_Data,total_purchase = sum(Purchase))
r<-summarise(t,total_purchase = sum(Purchase))
r
w<- ggplot(data=BlackFriday_Data,aes(x=Purchase))
w
h<-w + geom_histogram(binwidth = 500,fill="Blue",colour="black",na.rm=TRUE)
h + xlab("Dollars")+ ylab("Number of Buyers") +
  ggtitle("Distribution Of Total Purchase by Buyers") +
  theme(axis.title.x=element_text(colour = "Orange",size = 15),
        axis.title.y=element_text(colour="Red",size=15),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title = element_text(colour ="Dark Blue",size=20))

## total purchase by city

a <- ggplot(data=BlackFriday_Data,aes(x=Purchase))
Purchase_BY_City<- a + geom_histogram(binwidth = 500,aes(fill=City_Category),colour="black",na.rm=TRUE)
Purchase_BY_City
Purchase_BY_City + xlab("Dollars") + ylab("Number of buyers") +
  
  ggtitle("Distribution of Cities by Buyers") +
  theme(axis.title.x=element_text(colour = "Orange",size = 15),
        axis.title.y=element_text(colour="Red",size=15),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title = element_text(colour ="Dark Blue",size=20))


#gender proportion by age group, city 

q<- group_by(BlackFriday_Data,Age)
q
summarise(BlackFriday_Data,total_purchase = sum(Purchase))

Purchase_BY_Age <- ggplot(data=BlackFriday_Data,aes(x=Age,fill=Purchase))
Purchase_BY_Age + geom_bar(width = 0.5,aes(fill=City_Category),colour="black",na.rm=TRUE) +  facet_grid(City_Category~.)+ ggtitle("Purchase Percentage by age") + 
  theme(axis.title.x=element_text(colour = "Orange",size = 15),axis.title.y=element_text(colour="Red",size=15),
       axis.text.x=element_text(size=10),
       axis.text.y=element_text(size=10),
       axis.title = element_text(colour = "Dark Blue",size=20))
## classification of purchase by gender,city_category, martial status
install.packages("GGally")
library(GGally)
colnames(BlackFriday_Data)
Purchase_BY_Gender <- ggplot(data=BlackFriday_Data,aes(x=Gender,y=Purchase,fill=Gender)) +geom_jitter(color="Light Blue") + geom_boxplot(size=1.5,alpha=0.5)
Purchase_BY_Gender 

Purchase_BY_CityCategory<- ggplot(data=BlackFriday_Data,aes(x=City_Category,y=Purchase,fill=City_Category)) + geom_jitter(color="Light Green") + geom_boxplot(size=1,alpha=0.5)
Purchase_BY_CityCategory

Purchase_By_MartialStatus<- ggplot(data=BlackFriday_Data,aes(x=as.factor(Marital_Status),y=Purchase,fill=Gender)) + geom_jitter(colour="Grey") + geom_boxplot(size=1,alpha=0.5)
Purchase_By_MartialStatus


#Data formatting
BlackFriday_Data<-BlackFriday_Data %>% mutate_at(vars(3),funs(as.factor(.)))
BlackFriday_Data<-BlackFriday_Data %>% mutate_at(vars(5),funs(as.factor(.)))
BlackFriday_Data<-BlackFriday_Data %>% mutate_at(vars(6,8:11),funs(as.factor(.)))

#differnt levels of purchase, and ranking each purchases high to low
BlackFriday_Data <- BlackFriday_Data  %>% arrange(desc(Purchase)) %>% mutate(rank=c(1:537577)) %>% mutate(PriceLevel=case_when(rank<=537577*0.25~"Top25",rank>537577*0.25 & rank<=537577*0.75~"Average",rank>537577*0.75~"Bottom25"))
table(BlackFriday_Data$PriceLevel)

#Histogram 
ggplot(data=BlackFriday_Data,aes(x=Purchase,fill=PriceLevel))+ geom_histogram(show.legend= T, position= "stack", na.rm = FALSE,bins=100, inherit.aes = TRUE)+ xlab("Purchase")+ ylab("Count") +
  ggtitle("Levels of purchases done by customers") +
  theme(axis.title.x=element_text(colour = "Orange",size =15),
        axis.title.y=element_text(colour="Red",size=15),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title = element_text(colour = "Dark Blue",size=20)
  )

#Variable defined to Replacing NA with 0
fillzero<-function(x) {ifelse(is.na(x)==TRUE,0,x)}

#Grouped UserID with number of purchases made at different PriceLevel by customers
User<- BlackFriday_Data %>%  group_by(User_ID,PriceLevel) %>% summarise(n=n()) %>% spread(PriceLevel,n) %>%  mutate_at(.vars=vars(c(2:4)),funs(fillzero(.)))
head(User)

#Standardilizing the data for Kmeans, Z Distribution 
standard_data<- scale(User[,c(2:4)])
head(standard_data)

#Clustering with Kmeans
kmeancluster<-kmeans(standard_data,centers=3)
head(kmeancluster)

#Binding the kmeanscluster columns from kmeans to User dataset
Usersales <- cbind(User, clusterNumber = kmeancluster$cluster)
head(Usersales)

#grouped by clusternumbers from kmeans sumarising it with low high and median value orders
Usersales %>% group_by(clusterNumber) %>% summarise(Lowvalueorders=mean(Bottom25),Mediumvalueorders=mean(Average),Highvalueorders=mean(Top25))

#Trimmed actual dataset into a small dataset of only customer data
Userdataset <- BlackFriday_Data %>% distinct(User_ID,Age,Gender,Occupation,Marital_Status,City_Category)
head(Userdataset)

#mergeing two dataset 
Userdataset <-merge(Userdataset,Usersales, by="User_ID", all.x=TRUE)
head(Userdataset)
UserCluster<-Userdataset %>% mutate_at(vars("clusterNumber"), funs(case_when(.==3~"Lowvalue",.==1~"Mediumvalue",.==2~"HighValue")))
Userdataset$clusterNumber<-factor(Userdataset$clusterNumber,levels=c("Low Value Cluster","Medium Value Cluster","High Value Cluster"))
head(Userdataset)

#Converted the Userdataset columns into factors
Userdataset<-Userdataset %>% mutate_at(vars(5,10),funs(as.factor(.)))

#City category graph
UserCluster %>% group_by(clusterNumber,City_Category) %>% summarise(n=n()) %>% mutate(Percentage =n/sum(n)) %>% group_by(clusterNumber) %>% ggplot(aes(x=clusterNumber,fill=City_Category,y=Percentage)) + geom_col() + 
  geom_text(aes(label=paste0(round(Percentage,2)*100,"%")),position=position_stack(vjust=0.5))+ xlab("Cluster")+ ylab("Percentage") +
  ggtitle("Purchases made by each city, different clusters") +
  theme(axis.title.x=element_text(colour = "Orange",size =15),
        axis.title.y=element_text(colour="Red",size=15),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title = element_text(colour = "Dark Blue",size=20)
  )
