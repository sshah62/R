
library (mlbench)
library(Amelia)
library(VIM)
library(mice)
library(MASS)
library(ggplot2)
library(ggbiplot)
library(reshape2)
library(car)
library(EnvStats)
library(scales)
library(gridExtra)
library(GGally)
library(robustbase)
library(randomForest)
library(lattice)
library(dplyr)


(.packages())

data(Glass)
summary(Glass)
structure(Glass)

#Glass Visualizations
#Full Data Plot
plot(Glass)
plot(Glass[,1:9], col = Glass$Type)
pairs(Glass[1:9], main = "Glass Data by Type of Glass", pch=21, bg = Glass$Type )

#WARNING: THIS IS A VERY SLOW METHOD(1-2 MIN TO COMPLETE).  All in One
ggpairs(Glass[,1:9], lower=list(continuous="smooth", bins = 5), axisLabels='show')


Elements = c("RI", "Na", "Mg", "Al", "Si", "K" , "Ca", "Ba","Fe")

## Histograms
par(mfrow=c(3,3))               # Divides the screen into three sections
#apply(Glass[,1:9], 2, hist)    # Not very pretty
hist(Glass$RI, main= "Reflective Index", xlab= "RI")
hist(Glass$Na, main="Sodium", xlab="Na %")
hist(Glass$Mg, main="Magnesium", xlab="Mg %")
hist(Glass$Al, main="Aluminum", xlab="Al %")
hist(Glass$Si, main="Silicon", xlab="Si %")
hist(Glass$K,  main="Potassium", xlab="$")
hist(Glass$Ca, main="Calcium", xlab="Calcium Percentage")
hist(Glass$Ba, main="Barium", xlab="Barium Percentage")
hist(Glass$Fe, main="Iron", xlab="Iron Percentage")

par(mfrow=c(1,1))               # Reset display
          
#Boxplots and adjusted box plots
ggplot(Glass2,aes(x=Type, y=value, fill=variable)) + geom_boxplot()
par(mfrow=c(2,2))
boxplot(data=Glass, RI ~ Type,         # boxplot of RI
        main = "RI % vs. Type of Glass ",      # main plot title
        xlab = "Type of Glass",             # x-axis label   
        ylab = "RI")
adjbox(data=Glass, RI ~ Type, xlab="Type", ylab="RI", main="Adjusted")
boxplot(data=Glass, Na ~ Type,         # boxplot of Sepal.Length by Species 
        main = "Sodium % vs. Type of Glass ",      # main plot title
        xlab = "Type of Glass",             # x-axis label   
        ylab = "Na")
adjbox(data=Glass, Na ~ Type, xlab="Type", ylab="Na", main="Adjusted")

boxplot(data=Glass, Mg ~ Type,         # boxplot of Sepal.Length by Species 
        main = "Mg vs. Type of Glass ",      # main plot title
        xlab = "Type of Glass",             # x-axis label   
        ylab = "Mg")
adjbox(data=Glass, Mg ~ Type, xlab="Type", ylab="Mg", main="Adjusted")

boxplot(data=Glass, Al ~ Type,         # boxplot of Sepal.Length by Species 
        main = "Al vs. Type of Glass ",      # main plot title
        xlab = "Type of Glass",             # x-axis label   
        ylab = "Al")
adjbox(data=Glass, Al ~ Type, xlab="Type", ylab="Al", main="Adjusted")

boxplot(data=Glass, Si ~ Type,         # boxplot of Sepal.Length by Species 
        main = "Si vs. Type of Glass ",      # main plot title
        xlab = "Type of Glass",             # x-axis label   
        ylab = "Si")
adjbox(data=Glass, Si ~ Type, xlab="Type", ylab="Si", main="Adjusted")

boxplot(data=Glass, K ~ Type,         # boxplot of Sepal.Length by Species 
        main = "K vs. Type of Glass ",      # main plot title
        xlab = "Type of Glass",             # x-axis label   
        ylab = "K")
adjbox(data=Glass, K ~ Type, xlab="Type", ylab="K", main="Adjusted")

boxplot(data=Glass, Ca ~ Type,         # boxplot of Sepal.Length by Species 
        main = "Ca vs. Type of Glass ",      # main plot title
        xlab = "Type of Glass",             # x-axis label   
        ylab = "Ca")
adjbox(data=Glass, Ca ~ Type, xlab="Type", ylab="Ca", main="Adjusted")

boxplot(data=Glass, Ba ~ Type,         # boxplot of Sepal.Length by Species 
        main = "Ba vs. Type of Glass ",      # main plot title
        xlab = "Type of Glass",             # x-axis label   
        ylab = "Ba")
adjbox(data=Glass, Ba ~ Type, xlab="Type", ylab="Ba", main="Adjusted")

boxplot(data=Glass, Fe ~ Type,         # boxplot of Sepal.Length by Species 
        main = "Fe vs. Type of Glass ",      # main plot title
        xlab = "Type of Glass",             # x-axis label   
        ylab = "Fe")
adjbox(data=Glass, Fe ~ Type, xlab="Type", ylab="Fe", main="Adjusted")

par(mfrow=c(1,1))

##Scatter plot and Heat map
scattmatrixMiss(Glass[,1:9])    # Breif view for binary scatter
heatmap(cor(Glass[,1:9]))       # Check the correlations between variables

#Pairs
Glass2 <- melt(Glass)

# Densities
for (i in Elements) 
{
  print(ggplot(Glass2[Glass2$variable == i,], aes(x=value, fill=Type)) +
          geom_density(alpha=0.45) +        #set geometry and transparency    
          labs(x = "Ion Concentration",          #set x-label and title
               title = paste ("Densities for", i, "of Glass Types")))
}

#Fancy Box plots using melted dataframe
ggplot(Glass2,aes(x=variable, y=value, fill=Type)) + geom_boxplot()

#Parallel plots
parallelplot(~Glass[1:9] | Type, data = Glass,             
             groups = Type,   
             horizontal.axis = FALSE, 
             scales = list(x = list(rot = 90))) 

#Skewness
apply(Glass[,1:9], 2, skewness)

# Check the correlations
cor(Glass[,1:9],method = "kendall")
cor(Glass[,1:9],method = "pearson")
cor(Glass[,1:9],method = "spearman")

################################################
####Selecting Mg, K and Ba for Skew Transformation

symbox(Glass$Mg+0.00001, data= Glass, powers=c(3,2,1,0,-0.5,-1,-2)) 
symbox(Glass$K+0.00001, data= Glass, powers=c(3,2,1,0,-0.5,-1,-2)) 
symbox(Glass$Ba+0.00001, data= Glass, powers=c(3,2,1,0,-0.5,-1,-2)) 

#Mg
boxcox(Glass$Mg+0.00001)
###Select Lambda = 2
symbox(Glass$Mg+0.00001, data= Glass, powers=c(2))

#K
boxcox(Glass$K+0.00001)
###Select Lambda = 0.5
symbox(Glass$K+0.00001, data= Glass, powers=c(0.5))

#Ba
boxcox(Glass$Ba+0.00001)
###Select Lambda = 0.0
symbox(Glass$Ba+0.00001, data= Glass, powers=c(0.0))

#PCA

Glassprcomp <- prcomp(Glass[,1:9], scale = TRUE)
Glassprcomp
plot(Glassprcomp)
summary(Glassprcomp)

ggbiplot(Glassprcomp, circle = TRUE, obs.scale = 1, var.scale = 1, labels = rownames(Glass), choices=c(1,2))
ggbiplot(Glassprcomp, circle = TRUE, obs.scale = 1, var.scale = 1, labels = rownames(Glass), choices=c(1,3))
ggbiplot(Glassprcomp, circle = TRUE, obs.scale = 1, var.scale = 1, labels = rownames(Glass), choices=c(1,4))
ggbiplot(Glassprcomp, circle = TRUE, obs.scale = 1, var.scale = 1, labels = rownames(Glass), choices=c(1,5))
ggbiplot(Glassprcomp, circle = TRUE, obs.scale = 1, var.scale = 1, labels = rownames(Glass), choices=c(2,3))
ggbiplot(Glassprcomp, circle = TRUE, obs.scale = 1, var.scale = 1, labels = rownames(Glass), choices=c(2,4))
ggbiplot(Glassprcomp, circle = TRUE, obs.scale = 1, var.scale = 1, labels = rownames(Glass), choices=c(2,5))

#LDA
fit.lda <- lda(formula = Type ~ ., data = Glass)
#fit.lda$prior
#fit.lda$counts
#fit.lda$means
#fit.lda$scaling
#fit.lda$svd
#prop = fit.lda$svd^2/sum(fit.lda$svd^2)
#prop
fit.lda
fit.predict <- predict(fit.lda, newdata=Glass[,1:9])$class
table(fit.predict, Glass[,10])



#--------------------Problem 2-------------------
data(freetrade)
summary(freetrade)
head(freetrade)
str(freetrade)



#Cleanup and proper formattting
freetrade$year <- as.numeric(fretrade$year)
freetrade$polity <- as.numeric(freetrade$polity)
freetrade$country <- as.factor(freetrade$country)
freetrade$signed <- as.numeric(freetrade$signed)

#2a: regreesion using listwise deletion
freetradeLD <- na.omit(freetrade) #deletion of records with 1 or more missing fields
freetradeLDfit <- lm(data = freetrade, tariff~year+country+polity+pop+gdp.pc+intresmi+signed+fiveop+usheg)

summary(freetradeLDfit)

sf.a <- summary(freetradeLDfit)
coef.a <- sf.a[[4]]
se.a <- sf.a[[6]]
se.a

#Rgression using mean imputation
freetrademimp <- freetrade
freetrademimp[is.na(freetrademimp$tariff), "tariff"] <- mean(freetrademimp$tariff,na.rm=T)
freetrademimp[is.na(freetrademimp$polity), "polity"] <- mean(freetrademimp$polity,na.rm=T)
freetrademimp[is.na(freetrademimp$intresmi), "intresmi"] <- mean(freetrademimp$intresmi,na.rm=T)
freetrademimp[is.na(freetrademimp$signed), "signed"] <- mean(freetrademimp$signed,na.rm=T)
freetrademimp[is.na(freetrademimp$fiveop), "fiveop"] <- mean(freetrademimp$fiveop,na.rm=T)

freetrademimpfit <- lm(data=freetrademimp, tariff ~ year+country+polity+pop+gdp.pc+intresmi+signed+fiveop+usheg)
summary(freetrademimpfit)
sf.b <- summary(freetrademimpfit)
sf.b
coef.b <- sf.b[[4]]
se.b <- sf.b[[6]]
se.b

#2(c) regression using multiple imputations
freetradeMI <- mice(freetrade, m=5, maxit=10, method="mean")
summary(freetrade.MI)
freetradeMI <- mice(freetrade, m=5, maxit=10, method="cart")
summary(freetrade.MI)
freetradeMI <- mice(freetrade, m=5, maxit=10, method="sample")
summary(freetrade.MI)
freetradeMI <- mice(freetrade, m=5, maxit=10, method="rf")
summary(freetrade.MI)
freetradeMIcomplete <-complete(freetradeMI, "long")  #Complete Imputed data
summary(freetradeMIcomplete)
head(freetradeMIcomplete)
str(freetradeMI)


freetradeMI$chainMean
freetradeMI$chainVar

plot(freetradeMI)
freetradeMIfit <- with(freetradeMI, 
                         lm(tariff ~ year + country + polity + 
                              pop + gdp.pc + intresmi + signed + fiveop + usheg))

summary(freetradeMIfit)
sf.c <- summary(freetradeMIfit)
sf.c
coef.c <- sf.c[[4]]
coef.c
se.c <- sf.c[[6]]
se.c

##Compare coeffficents.



#problem 3
SensorData <- read.csv(file="bridgeSensor.csv", header = TRUE, sep=",")

#helper function
plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k)/2)) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}
sens1fft<-fft(SensorData$Sensor1)
sens2fft<-fft(SensorData$Sensor2)

#plot signal 1 and signal 2
par(mfrow = c(2,2))
plot(SensorData$Time, SensorData$Sensor1, type = "l",main="Sensor 1 Wave", col=2); abline(h=0)
plot(SensorData$Time, SensorData$Sensor2, type = "l",main="Sensor 2 Wave", col=8); abline(h=0)

#plot fourier transform for frquency spectrum
plot.frequency.spectrum(sens1fft)
plot.frequency.spectrum(sens2fft)

par(mfrow = c(1,1))


#Split 2 distinct events
Sensor1Truck1 <- SensorData[1:801,c(1,2)]
Sensor1Truck2 <- SensorData[802:length(SensorData$Time),c(1,2)]
Sensor2Truck1 <- SensorData[1:801,c(1,3)]
Sensor2Truck2 <- SensorData[802:length(SensorData$Time),c(1,3)]

S1T1fft <- fft(Sensor1Truck1$Sensor1)
S1T2fft <- fft(Sensor1Truck2$Sensor1)
S2T1fft <- fft(Sensor2Truck1$Sensor2)
S2T2fft <- fft(Sensor2Truck2$Sensor2)

#create numeric values for fft
S1T1_Mod  <- cbind(0:(length(S1T1fft)-1), Mod(S1T1fft))
S1T2_Mod  <- cbind(0:(length(S1T2fft)-1), Mod(S1T2fft))
S2T1_Mod  <- cbind(0:(length(S2T1fft)-1), Mod(S2T1fft))
S2T2_Mod  <- cbind(0:(length(S2T2fft)-1), Mod(S2T2fft))

par(mfrow = c(2,2))

plot(S1T1_Mod, t="h", lwd=2, main="Sensor=1 / Truck=1", xlab="Frequency (Hz)", ylab="Strength",
     xlim=c(0,length(S1T1fft)/2), ylim=c(0,max(Mod(S1T1_Mod[,2]))))
plot(S1T2_Mod, t="h", lwd=2, main="Sensor=1 / Truck=2", xlab="Frequency (Hz)", ylab="Strength",
     xlim=c(0,length(S1T2fft)/2), ylim=c(0,max(Mod(S1T2_Mod[,2]))))
plot(S2T1_Mod, t="h", lwd=2, main="Sensor=2 / Truck=1", xlab="Frequency (Hz)", ylab="Strength",
     xlim=c(0,length(S2T1fft)/2), ylim=c(0,max(Mod(S2T1_Mod[,2]))))
plot(S2T2_Mod, t="h", lwd=2, main="Sensor=2 / Truck=2", xlab="Frequency (Hz)", ylab="Strength",
     xlim=c(0,length(S2T2fft)/2), ylim=c(0,max(Mod(S1T2_Mod[,2]))))

#Problem 4:
zikaData <- read.csv(file="cdc_zika.csv", header = TRUE, sep=",")
str(zikaData)

#Data Cleanup
zikaData$value <- as.numeric(zikaData$value)
zikaData$location <- as.character(zikaData$location)
zikaData$report_date <- as.Date(zikaData$report_date)
zikaData$country <- sapply(zikaData$location, function(x) strsplit(x, '-')[[1]][1]) # get country from location

summary(zikaData)


