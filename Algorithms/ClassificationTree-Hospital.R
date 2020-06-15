data <- read.csv("datasets/hospital_ratings.csv")
str(data)

set.seed(10)

#Izbacujemo sve koje su jedinstvene i/ili nemaju prediktivnu moc
data$ID <- NULL
data$Name <- NULL
data$Address <- NULL
data$City <- NULL
data$County <- NULL

data <- data[data$State=="TX" | data$State=="CA" | data$State=="FL" | data$State=="PA",]
summary(data)
data$State <- factor(data$State,levels=c("TX","CA","FL","PA"))

#NA
summary(data)

shapiro.test(data$Overall.rating)
data$Overall.rating[is.na(data$Overall.rating)] <- median(data$Overall.rating,na.rm=T)

summary(data$Mortality.national.comparison)
data$Mortality.national.comparison[is.na(data$Mortality.national.comparison)] <- "Same as the national average"

summary(data$Timeliness.of.care)
data$Timeliness.of.care[is.na(data$Timeliness.of.care)] <- "Same as the national average"

summary(data$Effectiveness.of.care)
data$Effectiveness.of.care[is.na(data$Effectiveness.of.care)] <- "Same as the national average"

summary(data$Patient.experience)
data$Patient.experience[is.na(data$Patient.experience)] <- "Below the national average"

summary(data$Readmission)
data$Readmission[is.na(data$Readmission)] <- "Same as the national average"

data$Can.use.EHRs <- factor(data$Can.use.EHRs,levels=c("N","Y"))
data$Can.use.EHRs[is.na(data$Can.use.EHRs)] <- "N"

data$rating <- as.factor(ifelse(data$Overall.rating<=3,"Low","High"))
data$Overall.rating <- NULL

library(caret)
indexi <- createDataPartition(data$rating,p=0.8,list=FALSE)
train <- data[indexi,]
test <- data[-indexi,]

library(rpart)
model <- rpart(rating ~ .,data=train,method="class")

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(model)

pred1 <- predict(object=model,newdata=test,type="class")
head(pred1)
cm1 <- table(actual=test$rating,predicted=pred1)
cm1

tp1 <- cm1[2,2]
tn1 <- cm1[1,1]
fp1 <- cm1[1,2]
fn1 <- cm1[2,1]
acc1 <- (tp1+tn1)/(tp1+tn1+fp1+fn1)
prec1 <- tp1/(tp1+fp1)
rec1 <- tp1/(tp1+fn1)
f11 <- 2*acc1*rec1/(acc1+rec1)

library(e1071)
library(caret)
folds <- trainControl(metho="cv",number=10)
cpGrid <- expand.grid(.cp=seq(from=0.001,to=0.05,by=0.001))
result <- train(rating ~ .,data=train,method="rpart",control=rpart.control(minsplit=20),trControl=folds,tuneGrid=cpGrid)
optcp <- result$bestTune

model2 <- rpart(rating ~ .,data=train,method="class",control = rpart.control(minsplit = 20, cp = optcp))

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(model2)

pred2 <- predict(object=model2,newdata=test,type="class")
head(pred2)
cm2 <- table(actual=test$rating,predicted=pred2)
cm2

tp2 <- cm2[2,2]
tn2 <- cm2[1,1]
fp2 <- cm2[1,2]
fn2 <- cm2[2,1]
acc2 <- (tp2+tn2)/(tp2+tn2+fp2+fn2)
prec2 <- tp2/(tp2+fp2)
rec2 <- tp2/(tp2+fn2)
f12 <- 2*acc2*rec2/(acc2+rec2)

