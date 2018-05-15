#EDA in Kaggle kernel

rm(list = ls())

#Loading Libraries

library(data.table)
library(ggplot2)
library(magrittr)
library(corrplot)
library(Rmisc)
library(ggalluvial)
library(caret)
library(ModelMetrics)
require(scales)
library(irlba)
library(forcats)
library(TSA)
library(zoo)
library(skimr)
library(fasttime)
library(gridExtra)

# Lets use train data and we will later split it into training and testing
# Since the data is quite large, this approach can be implemented on larger data with server and cloud
train<-
  read.csv("E:/Semester 2/KDD/Project/TalkingData Adtracking/Datasets/train_sample.csv")

#Check the head of Train
head(train,5)

#Check data for Null Values
sapply(train, function(y) sum(is.na(y)))

#Check Factor Variable
table(train$is_attributed)
barplot(prop.table(table(train$is_attributed))) 

#View Data
str(train)

summary(train)

#Scatterplot of Features
plot(train[,2:5], main="Scatterplot", pch=19) 

# Let's have a look at features counts:
fea <- c("os", "channel", "device", "app", "attributed_time", "click_time", "ip")
train[, lapply(.SD, uniqueN), .SDcols = fea] %>%
  melt(variable.name = "features", value.name = "unique_values") %>%
  ggplot(aes(reorder(features, -unique_values), unique_values)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  scale_y_log10(breaks = c(50,100,250, 500, 10000, 50000)) +
  geom_text(aes(label = unique_values), vjust = 1.6, color = "white", size=3.5) +
  theme_minimal() +
  labs(x = "features", y = "Number of unique values")


# Checking Important Features

#Application ID vs is_attributed

p1=ggplot(train,aes(x=is_attributed,y=app,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Application ID v/s Is_attributed")+
  xlab("App ID") +
  labs(fill = "is_attributed")  

p2=ggplot(train,aes(x=app,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  scale_x_continuous(breaks = c(0,50,100,200,300,400))+
  ggtitle("Application ID v/s Is_attributed")+
  xlab("App ID") +
  labs(fill = "is_attributed")  


p3=ggplot(train,aes(x=is_attributed,y=app,fill=is_attributed))+
  geom_violin()+
  ggtitle("Application ID v/s Is_attributed")+
  xlab("App ID") +
  labs(fill = "is_attributed")  


grid.arrange(p1,p2,p3, nrow=2,ncol=2)

#App downloaded vs OS version id of user mobile phone

p4=ggplot(train,aes(x=is_attributed,y=os,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Os version v/s Is_attributed")+
  xlab("OS version") +
  labs(fill = "is_attributed")  


p5=ggplot(train,aes(x=os,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  scale_x_continuous(breaks = c(0,50,100,200,300,400))+
  ggtitle("Os version v/s Is_attributed ")+
  xlab("Os version") +
  labs(fill = "is_attributed")


p6=ggplot(train,aes(x=is_attributed,y=os,fill=is_attributed))+
  geom_violin()+
  ggtitle("Os version v/s Is_attributed")+
  xlab("Os version") +
  labs(fill = "is_attributed")  


grid.arrange(p4,p5, p6, nrow=2,ncol=2)

###App was downloaded v/s ip address of click.
p7=ggplot(train,aes(x=is_attributed,y=ip,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("IP Address v/s Is_attributed")+
  xlab("Ip Adresss of click") +
  labs(fill = "is_attributed")  


p8=ggplot(train,aes(x=ip,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  scale_x_continuous(breaks = c(0,50,100,200,300,400))+
  ggtitle("IP Address v/s Is_attributed")+
  xlab("Ip Adresss of click") +
  labs(fill = "is_attributed")  



p9=ggplot(train,aes(x=is_attributed,y=ip,fill=is_attributed))+
  geom_violin()+
  ggtitle("IP Address v/s Is_attributed")+
  xlab("Ip Adresss of click") +
  labs(fill = "is_attributed")  

grid.arrange(p7,p8, p9, nrow=2,ncol=2)

###App was downloaded v/s device type id of user mobile phone

p10=ggplot(train,aes(x=device,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  ggtitle("Device type v/s Is_attributed")+
  xlab("Device Type ID") +
  labs(fill = "is_attributed")  


p11=ggplot(train,aes(x=is_attributed,y=device,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Device type v/s Is_attributed")+
  xlab("Device Type ID") +
  labs(fill = "is_attributed")  


p12=ggplot(train,aes(x=is_attributed,y=device,fill=is_attributed))+
  geom_violin()+
  ggtitle("Device type v/s Is_attributed")+
  xlab("Device Type ID") +
  labs(fill = "is_attributed")  

grid.arrange(p10,p11, p12, nrow=2,ncol=2)

###App was downloaded v/s channel id of mobile ad publisher

p13=ggplot(train,aes(x=channel,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  ggtitle("Channel v/s Is_attributed")+
  xlab("Channel of mobile") +
  labs(fill = "is_attributed")  


p14=ggplot(train,aes(x=is_attributed,y=channel,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Channel v/s Is_attributed")+
  xlab("Channel of mobile") +
  labs(fill = "is_attributed")  

p15=ggplot(train,aes(x=is_attributed,y=channel,fill=is_attributed))+
  geom_violin()+
  ggtitle("Channel v/s Is_attributed")+
  xlab("Channel of mobile") +
  labs(fill = "is_attributed")  

grid.arrange(p13,p14, p15, nrow=2,ncol=2)

# Correlation

train[, -c("click_time", "attributed_time"), with=F] %>%
  cor(method = "spearman") %>%
  corrplot(type="lower", method = "number", tl.col = "black", diag=FALSE)

#END


