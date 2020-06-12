data <- read.csv("datasets/world-happiness-report-2017.csv")
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

#Uklanjanje autlajera
apply(data[,-1],2,function(x) boxplot.stats(x)$out)
#Family
boxplot.stats(data$Family)$out
quantilesFamily <- as.vector(quantile(data$Family,probs=seq(from=0,to=0.1,by=0.025)))
limitFamily <- quantilesFamily[2]
data$Family[data$Family<limitFamily] <- limitFamily
#Trust Goverment
boxplot.stats(data$Trust.Government.Corruption)$out
quantilesGov <- as.vector(quantile(data$Trust.Government.Corruption,probs=seq(from=0.9,to=1,by=0.025)))
quantilesGov
limitGov <- quantilesGov[4]
data$Trust.Government.Corruption[data$Trust.Government.Corruption>limitGov] <- limitGov

#Normalizacija
normalized <- as.data.frame(apply(data[,-1],2,function(x) (x-min(x))/(max(x)-min(x))))
summary(normalized)

#Odredjivanje optimalnog K
side <- data.frame()
for(k in 2:8) {
  model <- kmeans(normalized,k,iter.max = 20,nstart = 1000)
  side <- rbind(side,c(k,model$tot.withinss,model$betweenss/model$totss))
}
names(side)<-c('K','TotalWithinSS','Ratio')
side
compute.difference(side$TotalWithinSS)
library(ggplot2)
ggplot(side,mapping = aes(x=K,y=TotalWithinSS))+geom_line()
optimalK <- 3

#Pravljenje modela
model <- kmeans(normalized,optimalK,iter.max = 20,nstart = 1000)
model

#Komentarisanje modela
#Tri klastera - velicina 59, 31 i 65
#Tot SS - zbir razdaljina izmedju srednje vrednosti celog seta, i svake pojedinacne obzervacije
#Between SS - zbir razdaljina izmedju srednje vrednosti celog seta, i svakog centra klastera
#Within SS - zbir razdaljina izmedju centra klastera, i svake pojedinacne obzervacije u tom klasteru
summary.stats(normalized,model$cluster,3)
