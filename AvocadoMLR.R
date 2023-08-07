# Ryan Essem 12 December 2021

setwd("~/Desktop/INST314 R Work")

# load libraries
install.packages("car")
library(car)
install.packages("pwr")
library(pwr)  # for power: pwr.f2.test


avocadoData <- read.csv(file.choose(), header = TRUE)
attach(avocadoData)
names(avocadoData)
head(avocadoData)

# check data type
class(Small.Bags)
class(Large.Bags)
class(XLarge.Bags)
class(Total.Volume)
class(AveragePrice)

# standardize dataset variables
avocadoData$Small.Bags.st <- scale(avocadoData$Small.Bags)
Small <- avocadoData$Small.Bags.st
summary(Small)
length(Small)
sd(Small)

avocadoData$Large.Bags.st  <-scale(avocadoData$Large.Bags)
Large <- avocadoData$Large.Bags.st
summary(Large)
length(Large)
sd(Large)

avocadoData$XLarge.Bags.st <-scale(avocadoData$XLarge.Bags)
XLarge <- avocadoData$XLarge.Bags.st 
summary(XLarge)
length(XLarge)
sd(XLarge)

avocadoData$Total.Volume.st <-scale(avocadoData$Total.Volume)
TotalVolume <- avocadoData$Total.Volume.st
summary(TotalVolume)
length(TotalVolume)
sd(TotalVolume)

avocadoData$AveragePrice.st  <-scale(avocadoData$AveragePrice)
AvgPrice <- avocadoData$AveragePrice.st
summary(AvgPrice)
length(AvgPrice)
sd(AvgPrice)


# X variables (IV) : Small.Bags, Large.Bags, XLarge.Bags, Total.Volume
# Y variables (DV) : AveragePrice

# MULTIPLE LINEAR REGRESSION
summary(lm(AvgPrice ~ Small + Large + XLarge + TotalVolume))


# Our Multiple R-squared of 0.04043 indicates that 
# approximately 4% of variation in Average Price can be explained by our model
# (Small.Bags, Large.Bags, X.Large.Bags, and Total.Volume)

# regression equation 
# AveragePrice =  0.0000000000000003359 +  0.1925943035348550714 * Small 
#               - 0.0651009022852839098 * Large + 0.0309166781188925550 * XLarge 
#               - 0.3448061508510401052 * TotalVolume


# check correlation
cor(avocadoData[, c('Small.Bags.st', 'Large.Bags.st', 'XLarge.Bags.st', 
                    'Total.Volume.st')])

# scatterplots
plot(Small, AvgPrice, main = "Relationship between Small Bags and Average Price")
abline(lm(AvgPrice ~ Small, data = avocadoData), col = "red", lwd =2)

plot(Large, AvgPrice, main = "Relationship between Large Bags and Average Price")
abline(lm(AvgPrice ~ Large, data = avocadoData), col = "red", lwd =2)

plot(XLarge, AvgPrice, main = "Relationship between Extra Large Bags and Average Price")
abline(lm(AvgPrice ~ XLarge, data = avocadoData), col = "red", lwd =2)

plot(TotalVolume, AvgPrice, main = "Relationship between Total Volume and Average Price")
abline(lm(AvgPrice ~ TotalVolume, data = avocadoData), col = "red", lwd =2)

mod <- lm(AvgPrice ~ Small + Large + XLarge + TotalVolume, data=avocadoData)
confint(mod, conf.level = 0.95)


# scatterplot pairs
pairs(~AvgPrice + Small + Large
      + XLarge + TotalVolume, main= "Bag Size and Total Volume",
      col=c('red'))

# plotting regression diagnostics
avgprice_bags_vol <- lm(AvgPrice ~ Small + Large + XLarge + TotalVolume, data=avocadoData)
par(mfrow=c(2,2)) # view on 2x2 grid
plot(avgprice_bags_vol)
par(mfrow=c(1,1)) # review to default view


# VIF
lm.avocado <- lm(AvgPrice ~ Small + Large + XLarge + TotalVolume, data=avocadoData)
vif(lm.avocado)
round(vif(lm.avocado),3)

# mean VIF of model as a whole
mean(vif(lm.avocado))

# massive multicollinearity is present

#FULL MODEL
lm.avocado <- lm(AvgPrice ~ Small + Large + XLarge + TotalVolume, data=avocadoData)
vif(lm.avocado)

# DROP SMALL BAGS
lm.avocado <- lm(AvgPrice ~ Large + XLarge  + TotalVolume, data=avocadoData)
vif(lm.avocado)

# after omitting small bags, the VIF scores for the model have VIFS under 5
# large bags = 4.58, XLarge bags = 2.23, Total Vol = 5.13 (which is ok or way better than before)

library(stargazer) # for regression table outputs
stargazer(mod,
          title = "Avocado Bag Size, Bag Volume and Average Price Analysis",
          dep.var.caption = "DV: Average Price of Avocados", #DV caption
          covariate.labels =c("Small"),
          type = "html",
          notes.label = "Signifance levels",
          out = "Project3_RE.htm")

# report the power
pwr.f2.test(u = 4,
            v = 18244,
            f2 = 0.042,
            sig.level = 0.001,
            power = NULL)

