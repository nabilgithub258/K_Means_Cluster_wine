library(corrgram)
library(corrplot)
library(caTools)
library(Amelia)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ISLR)
library(e1071)
library(cluster)


#### importing the csv file

df1 <- read.csv('winequality-red.csv',sep = ';')
df2 <- read.csv('winequality-white.csv', sep=';')

head(df1)
head(df2)

df1$label <- 'red'
df2$label <- 'white'

head(df1)
head(df2)

#### combining them together

wine <- rbind(df1,df2)

View(wine)

print(table(wine$label))

#### EDA time

ggplot(wine,aes(residual.sugar)) + geom_histogram(aes(fill=label),color='black',alpha=0.7)  + scale_fill_manual(values = c('#993333','#ffe5b4')) +theme_bw()

ggplot(wine,aes(quality)) + geom_histogram(aes(fill=label),color='black',position='dodge',alpha=0.7)  + scale_fill_manual(values = c('red','white')) +theme_bw()

ggplot(wine,aes(fixed.acidity)) + geom_histogram(aes(fill=label),color='black',position='dodge',alpha=0.7)  + scale_fill_manual(values = c('red','white')) +theme_bw()

ggplot(wine,aes(citric.acid,residual.sugar)) + geom_point(aes(colour = label),alpha=0.2) + scale_color_manual(values = c('#993333','#ffe5b4')) +theme_dark()

ggplot(wine,aes(volatile.acidity,residual.sugar)) + geom_point(aes(colour = label),alpha=0.2) + scale_color_manual(values = c('#993333','#ffe5b4')) +theme_dark()


#### its going to be challenge to properly label them and separate them due to the fact our closely packed each of them are
#### lets put the k means cluster in the act

model <- kmeans(wine[,1:12],2)

summary(model)

table(wine$label,model$cluster)

clusplot(wine,model$cluster,color=T,labels = F,shade = T)

#### in this we had the privilege to know if the clustering is working by using the label column but usually these are called
#### unsupervised clustering method meaning we cluster them without knowing to which column to compare it to