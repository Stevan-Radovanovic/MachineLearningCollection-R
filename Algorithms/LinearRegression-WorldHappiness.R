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

#Matrica korelacije
cormat <- cor(data[,-1])
library(corrplot)
corrplot.mixed(cormat,tl.cex=0.55,number.cex = 0.75)

#U odnosu na Freedom, korelisane su HappinessRank,HappinessScore,Whisker.High,Whisker.Low

#Podela dataseta
library(caret)
indexi <- createDataPartition(data$Freedom,p=0.8,list=FALSE)
train <- data[indexi,]
test <- data[-indexi,]

#Zbog perfektne kolinearnosti whisker.high i low, izbacujemo low iz modela
model <- lm(Freedom ~ Happiness.Rank+Happiness.Score+Whisker.high,train)
coef(model)
summary(model)
#Model je ocajan, nista nije znacajno, varijabilitet koji je objasnjen bolje da nije, pozdrav

plot(model)
#Nista nije zadovoljeno, samo cetvrti grafik nema leverage pointse
#1. Linerana veza
#2. Normalno distribuirani reziduali
#3. Homoskedasticni reziduali
#4. Leverage pointsi ima li ih

#Pravimo predikcije
predictions <- predict(model,test)
head(predictions)
head(test$Freedom)

#rss, tss, rmse
rss <- sum((predictions-test$Freedom)^2)
tss <- sum((mean(train$Freedom)-test$Freedom)^2)
rsq <- 1-rss/tss
rsq
rmse <- sqrt(rss/nrow(test))
rmse/mean(test$Freedom)

#Note: Model je apsolutno ocajan ne postoji gori






