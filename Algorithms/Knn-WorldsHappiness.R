#Ucitavanje data seta
data <- read.csv("world-happiness-report-2017.csv")
str(data)
summary(data)
set.seed(10)

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

#Pravljenje faktorske
data$Familijo <- as.factor(ifelse(data$Family<mean(data$Family),'Yes','No'))
data$Family <- NULL

#Standardizacija
apply(data[,c(2:11)],2,shapiro.test)
#3,4,5,11 Normalne
standardized <- as.data.frame(apply(data[,c(3,4,5,11)],2,function(x) scale(x,center=TRUE,scale=TRUE)))
standardized <- cbind(standardized,as.data.frame(apply(data[,c(2,6,7,8,9,10)],2,function(x) scale(x,center=median(x),scale=IQR(x)))))
standardized$Familijo <- as.factor(data$Familijo)
str(standardized)

#Podela dataseta
library(caret)
indexi <- createDataPartition(standardized$Familijo,p=0.8,list=FALSE)
train <- data[indexi,]
test <- data[-indexi,]

#Optimalno k
library(e1071)
library(caret)
folds <- trainControl(method="cv",number=10)
kGrid <- expand.grid(.k = seq(from=3,to=25,by=2))
result <- train(Familijo ~ .,data=train,method="knn",trControl=folds,tuneGrid=kGrid)
result$bestTune

#Pravljenje modela
library(class)
str(test)
str(train)
model <- knn(train=train[,-c(1,12)],test=test[,-c(1,12)],cl=train$Familijo,k=result$bestTune)
model

#Matrica konfuzije i komentari
cm <- table(actual=test$Familijo,predicted=model)
cm
tp <- cm[1,1]
tn <- cm[2,2]
fp <- cm[2,1]
fn <- cm[1,2]
acc <- (tp+tn)/(tp+tn+fp+fn)
precision <- tp/(tp+fp)
recall <- tp/(tp+fn)
f1 <- 2*precision*recall/(precision+recall)
