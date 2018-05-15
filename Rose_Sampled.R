#install.packages("ROSE")

rm(list = ls())
library(ROSE)

df <- read.csv("E://MIS/513B/R/Project/Unsampled_Data.csv")
#str(df)
#View(df)

is.factor(df$is_attributed)
df1<-df

df1$is_attributed<-as.factor(df$is_attributed)
table(df1$is_attributed)

#View(df1)
df.rose<-ROSE(is_attributed~.,data=df1,seed=123)$data
table(df.rose$is_attributed)
#write.csv(df.rose,"E://MIS/513B/R/Project/Rose_Sampled_Data.csv")
#View(df.rose)
