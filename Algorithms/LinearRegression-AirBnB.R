data <- read.csv("datasets/airbnb.csv",stringsAsFactors = F)
set.seed(10)

data <- data[!is.na(data$review_scores_rating),]

data$count <- strsplit(data$amenities,',')

dataf <- data.frame()
for(i in 1:2855) {
  dataf <- rbind(dataf,length(data$count[[i]]))
}

data <- cbind(data,dataf) 
data <- data[data$X14L>11,]
data$count <- NULL
data$X14L <- NULL

summary(data)
data$id <- NULL
data$price <- substr(data$price,2,10)
data$price <- as.numeric(data$price)
#Vrednost 1200 zbog zareza prelazi u NA, pa cemo nju vratiti rucno
data$price[is.na(data$price)] <- 1200

summary(data)

shapiro.test(data$bathrooms)
shapiro.test(data$bedrooms)
shapiro.test(data$beds)

data$bathrooms[is.na(data$bathrooms)] <- median(data$bathrooms,na.rm=T)
data$bedrooms[is.na(data$bedrooms)] <- median(data$bedrooms,na.rm=T)
data$beds[is.na(data$beds)] <- median(data$beds,na.rm=T)

library(corrplot)
cm <- cor(data[,c(3:6,8:11)])
corrplot.mixed(cm,number.cex=0.75,tl.cex=0.55)
#U odnosu na price -> beds, bedrooms. bathrooms. accommodates, pozitivno

library(caret)
indexi <- createDataPartition(data$price,p=0.8,list=FALSE)
train <- data[indexi,]
test <- data[-indexi,]

model <- lm(price ~ accommodates+bathrooms+bedrooms+beds,data=train)
coef(model)
summary(model)
#R je 48%, sve su znacajni prediktori

plot(model)
#Nijedan grafik ne zadovoljava kriterijume

pred <- predict(model,test)
head(pred)
head(test$price)

#Racunamo na kraju rss,rss,r squared, i rmse
