data <- read.csv("world-happiness-report-2017.csv")
str(data)
summary(data)

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

#Pravljenje faktorske
data$Corrupt <- ifelse(test=data$Trust.Government.Corruption<mean(data$Trust.Government.Corruption),
                      yes='Good',no='Bad')
data$Trust.Government.Corruption<-NULL
data$Corrupt<- as.factor(data$Corrupt)

#Diskretizacija
data$Country <- NULL
data$Happiness.Rank <- NULL
summary(data)
apply(data[,-10],2,shapiro.test)
#Dystopia i prve tri imaju normalnu raspodelu
library(bnlearn)
?discretize()
discretized <- discretize(data[,c(4,5,6,7,8)],method="quantile",breaks=c(5,5,5,5,5))
discretized <- cbind(discretized,data[,c(1,2,3,9,10)])

#Podela seta
library(caret)
indexi <- createDataPartition(discretized$Corrupt,p=0.8,list=FALSE)
train <- discretized[indexi,]
test <- discretized[-indexi,]

#Pravljenje modela
library(e1071)
model <- naiveBayes(Corrupt~.,train)
model

#Raw predikcije
pred.raw <- predict(model,test,type = "raw")
head(pred.raw)

#Roc kriva
library(pROC)
roc.curve.params <- roc(response = as.numeric(test$Corrupt), predictor=pred.raw[,2], levels=c(1,2))
roc.curve.params$auc
plot.roc(roc.curve.params,print.thres=TRUE,print.thres.best.method = "youden")
roc.coords <- coords(roc.curve.params,ret=c('acc','spec','sens','thr'),x="local maximas")
roc.coords
threshold <- roc.coords[3,4]

#Nove predikcije
pred.new <- ifelse(test=pred.raw[,2]>threshold,yes='Good',no='Bad')
head(pred.new)

#Matrica konfuzije + komentari
cm <- table(actual=test$Corrupt,predicted=pred.new)
cm
tp <- cm[2,2]
tn <- cm[1,1]
fn <- cm[2,1]
fp <- cm[1,2]
acc <- (tp+tn)/(tp+tn+fn+fp)
prec <- tp/(tp+fp)
recall <- tp/(tp+fn)
f <- 2*prec*recall/(prec+recall)
