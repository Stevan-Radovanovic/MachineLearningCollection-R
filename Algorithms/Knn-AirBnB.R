data <- read.csv("datasets/airbnb.csv",stringsAsFactors = F)
str(data)
summary(data)
set.seed(10)

#Podskup svih onih koji imaju review score
data <- data[!is.na(data$review_scores_rating),]

#Podskup svih onih koji imaju vise od 11 amenitija
lst<-strsplit(data$amenities,',')
length(lst[[1]])

splitdata <- data.frame()
for(i in 1:2855) {
  splitdata <- rbind(splitdata,length(lst[[i]]))
}

data <- cbind(data,splitdata)
data <- data[data$X14L>11,]
data$X14L <- NULL

#Sredjivanje podataka
str(data)
data$amenities <- NULL
data$accommodates <- as.factor(data$accommodates)
price <- substr(data$price,2,20)
price <- as.numeric(price)
data$price <- as.vector(price)
str(data)
summary(data)
data$id <- NULL
data$name <- NULL
data$property_type <- as.factor(data$property_type)
data$accommodates <- as.integer(data$accommodates)

#Uklanjanje NA vrednosti
which(is.na(data))
summary(data)
#NA imaju bathrooms, bedrooms, beds i price
shapiro.test(data$bathrooms)
data$bathrooms[is.na(data$bathrooms)] <- median(data$bathrooms,na.rm=T)
shapiro.test(data$bedrooms)
data$bedrooms[is.na(data$bedrooms)] <- median(data$bedrooms,na.rm=T)
shapiro.test(data$beds)
data$beds[is.na(data$beds)] <- median(data$beds,na.rm=T)
shapiro.test(data$price)
data$price[is.na(data$price)] <- median(data$price,na.rm=T)
which(is.na(data))

#Faktorska
data$expensive <- as.factor(ifelse(data$price>median(data$price),'yes','no'))
data$price <- NULL
str(data)
summary(data)

#Standardizacija
data$property_type <- NULL
apply(data[,-8],2,shapiro.test)
#Nijedan nema normalnu raspodelu
standardized <- data.frame()
standardized <- as.data.frame(apply(data[,-c(3,8)],2,function(x) scale(x,center=median(x),scale = IQR(x))))
standardized$expensive <- data$expensive
str(standardized)
summary(standardized)
#Iz nekog razloga bedrooms nece da se standardizuje
#Izbacicu je ali nemam ideju zasto daje NaN i inf vrednosti

#Podela dataseta
library(caret)
indexi <- createDataPartition(standardized$expensive,p=0.8,list=FALSE)
train <- standardized[indexi,]
test <- standardized[-indexi,]

#Optimalno k
library(e1071)
library(class)
folds <- trainControl(method="cv",number=10)
kGrid <- expand.grid(.k = seq(from=3,to=25,by=2))
result <- train(expensive ~ .,data=train,method="knn",trControl=folds,tuneGrid=kGrid)
result$bestTune

#Model
library(class)
model <- knn(train=train[,-7],test=test[,-7],cl=train$expensive,k=result$bestTune)
model
which(is.na(model))

#Matrica konfuzije
cm <- table(actual=test$Familijo,predicted=model)
cm
tp <- cm[2,2]
tn <- cm[1,1]
fn <- cm[2,1]
fp <- cm[1,2]
acc <- (tp+tn)/(tp+tn+fp+fn)
precision <- tp/(tp+fp)
recall <- tp/(tp+fn)
f1 <- 2*precision*recall/(precision+recall)