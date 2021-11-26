library(tidyverse)      
library("plot3D")
library("psych")  
library(aod)
library(ggplot2)
library(vcd)
library(ROCR)
library(lift)

Titanic <- read.table(file = "Titanic.csv", 
                      sep = ",",                      
                      header = TRUE,                  
                      na.strings = "NA",               
                      stringsAsFactors = FALSE)  
summary(Titanic)
head(Titanic)
Titanic_noNA <- na.omit(Titanic)
summary(Titanic_noNA)
str(Titanic_noNA)



Titanic_noNA$PClass <- factor(Titanic_noNA$PClass)
str(Titanic_noNA)
contrasts(Titanic_noNA$PClass)

# Training and test
set.seed(70)
sample <- sample(c(TRUE, FALSE), nrow(Titanic_noNA), replace = T, prob=c(0.7, 0.3))

train_titanic <- Titanic_noNA[sample,]
test_titanic <- Titanic_noNA[!sample,]


# Model
log_model<-glm(Survived~PClass+Age+SexCode, family="binomial", data=train_titanic)
summary(log_model)

confint(log_model)
confint.default(log_model)

predictions<-ifelse(test=log_model$fitted.values<0.4,yes=0, no=1)

confusion_matrix<-table(log_model$model$Survived,predictions,
                        dnn=c("observations", "predictions"))

confusion_matrix

mosaic(confusion_matrix,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green3", "red2", "red2", "green3"),2,2)))
