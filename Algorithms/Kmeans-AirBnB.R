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

str(data)
data$name <- NULL
data$amenities <- NULL
data$property_type <- NULL
#Provera autlajera
apply(data,2,function(x) boxplot.stats(x)$out)

#Accomodates Outliers
sort(boxplot.stats(data$accommodates)$out)
vec1 <- as.vector(quantile(data$accommodates,probs=seq(from=0.9,to=1,by=0.025)))
limit1 <- vec1[3]
data$accommodates[data$accommodates>limit1] <- limit1

#Bathrooms Outliers
sort(boxplot.stats(data$bathrooms)$out)
vec2 <- as.vector(quantile(data$bathrooms,probs=seq(from=0.9,to=1,by=0.025)))
vec2
limit2 <- vec2[2]
data$bathrooms[data$bathrooms>limit2] <- limit2
vec3 <- as.vector(quantile(data$bathrooms,probs=seq(from=0,to=0.1,by=0.025)))
vec3
limit3 <- vec3[2]
data$bathrooms[data$bathrooms<limit3] <- limit3

#Bedrooms Outliers
#Ovde je neki haos, sredicu rucno
sort(boxplot.stats(data$bathrooms)$out)
data$bedrooms[!is.na(data$bedrooms)] <- 1

#Beds Outliers
sort(boxplot.stats(data$beds)$out)
vec4 <- as.vector(quantile(data$beds,probs=seq(from=0.9,to=1,by=0.025)))
vec4
limit4 <- vec4[2]
data$beds[data$beds>limit4] <- limit4

#Price Outliers
sort(boxplot.stats(data$price)$out)
vec5 <- as.vector(quantile(data$price,probs=seq(from=0.9,to=1,by=0.025)))
vec5
limit5 <- vec5[1]
data$price[data$price>limit5] <- limit5

#Minimum Nights Outliers
sort(boxplot.stats(data$minimum_nights)$out)
vec6 <- as.vector(quantile(data$minimum_nights,probs=seq(from=0.9,to=1,by=0.025)))
vec6
limit6 <- vec6[4]
data$minimum_nights[data$minimum_nights>limit6] <- limit6

#Number of reviews Outliers
sort(boxplot.stats(data$number_of_reviews)$out)
vec7 <- as.vector(quantile(data$number_of_reviews,probs=seq(from=0.9,to=1,by=0.025)))
vec7
limit7 <- vec7[1]
data$number_of_reviews[data$number_of_reviews>limit7] <- limit7

#Review Scores Rating Outliers
sort(boxplot.stats(data$review_scores_rating)$out)
vec8 <- as.vector(quantile(data$review_scores_rating,probs=seq(from=0,to=0.1,by=0.025)))
vec8
limit8 <- vec8[2]
data$review_scores_rating[data$review_scores_rating<limit8] <- limit8

#Normalizacija
normalized <- as.data.frame(apply(data,2,function(x) (x-min(x))/(max(x)-min(x))))
str(normalized)
#Bedrooms opet pravi problem, bice izbacen
normalized$bedrooms <- NULL
summary(normalized)

#Optimalno K
helping <- data.frame()
for(k in 2:8) {
  model <- kmeans(normalized,centers = k,iter.max = 20,nstart = 1000)
  helping <- rbind(helping,c(k,model$tot.withinss,model$betweenss/model$totss))
}
names(helping) <- c('k','totwithin','ratio')
helping
source('Utility.R')
compute.difference(helping$totwithin)
library(ggplot2)
ggplot(data=helping,mapping=aes(x=k,y=totwithin))+geom_line()
optimk <- 4

model <- kmeans(normalized,centers = optimk,iter.max = 20,nstart = 1000)
model
summary.stats(feature.set = normalized,clusters = model$cluster,cl.num = optimk)
