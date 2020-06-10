data <- read.csv("world-happiness-report-2017.csv")
str(data)
summary(data)

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

#Standardizacija

#Odredjivanje optimalnog K

#Pravljenje modela

#Komentarisanje modela