#Ukloniti NA
data <- read.csv("datasets/world-happiness-report-2017.csv")
str(data)
summary(data)
data$Country <- NULL

set.seed(10)
source('Utility.r')

#Uklanjanje NA vrednosti
which(is.na(data))

#Uklanjanje NA vrednosti Health.Life.Expectancy
shapiro.test(data$Health.Life.Expectancy)
#Nema normalnu distribuciju
data$Health.Life.Expectancy[is.na(data$Health.Life.Expectancy)] <- median(data$Health.Life.Expectancy,na.rm=T)

#Uklanjanje NA vrednosti Generosity
shapiro.test(data$Generosity)
#Nema normalnu distribuciju
data$Generosity[is.na(data$Generosity)] <- median(data$Generosity,na.rm = T)

#Ponovna provera NA vrednosti
which(is.na(data))

#Napraviti faktorsku
summary(data)
data$Generous <- ifelse(test=data$Generosity<mean(data$Generosity), yes='Shit',no='Generous')
data$Generous <- as.factor(data$Generous)
data$Generosity <- NULL

#Podeliti dataset
library(caret)
indexi <- createDataPartition(data$Generous,p=0.8,list=FALSE)
train <- data[indexi,]
test <- data[-indexi,]

#Odrediti optimalno cp
library(e1071)
library(caret)
library(rpart)
folds <- trainControl(method="cv",number=10)
cpGrid <- expand.grid(.cp = seq(from=0.001,to=0.05,by=0.001))
result <- train(Generous ~ .,data=train,method="rpart",control=rpart.control(minsplit=20),trControl=folds,tuneGrid=cpGrid)

#Napraviti model
model <- rpart(Generous ~ .,data=train,method="class",control=rpart.control(minsplit = 10,cp=result$bestTune))
model

#Nacrtati model
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(model)

#Odraditi predikcije
pred <- predict(model,test,type="class")
head(pred)

#Matrica konfuzije i komentari
cm <- table(actual=test$Generous, predicted=pred)
cm
tp <- cm[1,1]
tn <- cm[2,2]
fp <- cm[2,1]
fn <- cm[1,2]
acc <- (tp+tn)/(tp+tn+fp+fn)
prec <- tp/(tp+fp)
rec <- tp/(tp+fn)
f1 <- 2*prec*rec/(prec+rec)
