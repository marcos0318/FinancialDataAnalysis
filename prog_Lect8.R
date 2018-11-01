rm(list = ls())
setwd("...//Lect5")  # set directory

BHData <- read.table("...//SP_BH.csv",sep=',',header=TRUE)
attach(BHData)

### original price
par(mfrow=c(1,2))
plot(SP,BH, main="BH vs SP500")

BHSPl2 <- lsfit(SP,BH)
abline(BHSPl2)

plot(BHSPl2$residuals,type="l",main="Residual Plot of LS Regression of BH Against SP500 ")

### to see positive association btw adjacent residuals
par(mfrow=c(1,1))
n <- length(BH)
plot(BHSPl2$residuals[-n],BHSPl2$residuals[-1],main="Residual against previous residual ")
cor(BHSPl2$residuals[-n],BHSPl2$residuals[-1])

acf(BHSPl2$residuals, type = "correlation", plot = TRUE)

# returns
rBH<-diff(BH)/BH[-n]
rSP<-diff(SP)/SP[-n]

par(mfrow=c(1,2))
plot(rSP, rBH, main="Returns on BH vs returns on SP500")

# ls fit
rBHSPl2 <- lsfit(rSP,rBH)
abline(rBHSPl2)

plot(rBHSPl2$residuals,type="l",main="Residual Plot of LS Regression of rBH Against rSP")

###  association btw adjacent residuals
par(mfrow=c(1,1))
n <- length(BH) - 1
plot(rBHSPl2$residuals[-n],rBHSPl2$residuals[-1],main="Residual against previous residual ")
cor(rBHSPl2$residuals[-n],rBHSPl2$residuals[-1])

acf(rBHSPl2$residuals, type = "correlation", plot = TRUE)

##########  Factor  Models #########
BHData <- read.table("...//BH2009-2018.csv",sep=',',header=TRUE)
 
summary(BHData) 
  
rBH <- BHData$rBH
rf <- BHData$rf
rBH_ex <- rBH - rf
rM_ex <- BHData$rM_ex
rSmB <- BHData$rSmB
rHmL <- BHData$rHmL
 
#One factor model
Onefactor <- lm(rBH_ex ~ rM_ex)
summary(Onefactor)

c(summary(Onefactor)$r.squared,cor(rBH_ex,rM_ex)^2)

#Model diagnostics
library(MASS)
par(mfrow=c(2,1))
plot(stdres(Onefactor),type="l",main="Standardized Residuals")
plot(studres(Onefactor),type="l",main="Studentized Residuals")

n <- length(rBH_ex) - 1
plot(stdres(Onefactor)[-n],stdres(Onefactor)[-1],main="Standardized residual against previous one ")
cor(stdres(Onefactor)[-n],stdres(Onefactor)[-1])

plot(studres(Onefactor)[-n],studres(Onefactor)[-1],main="Studentized residual against previous one ")
cor(studres(Onefactor)[-n],studres(Onefactor)[-1])

acf(stdres(Onefactor), type = "correlation", plot = TRUE)
acf(studres(Onefactor), type = "correlation", plot = TRUE)


par(mfrow=c(1,2))
qqnorm(stdres(Onefactor),main="Q-Q Plot of Standardized Residuals")
abline(0,1,col="red")
qqnorm(studres(Onefactor),main="Q-Q Plot of Studentized Residuals")
abline(0,1,col="red")


### 3-factor model
pairs(cbind(rBH_ex,rM_ex,rSmB,rHmL))

FF3factor <- lm(rBH_ex ~ rM_ex + rSmB + rHmL)
summary(FF3factor)

#Model diagnostics
par(mfrow=c(2,1))
plot(stdres(FF3factor),type="l",main="Standardized Residuals")
plot(studres(FF3factor),type="l",main="Studentized Residuals")

par(mfrow=c(1,2))
qqnorm(stdres(FF3factor),main="Q-Q Plot of Standardized Residuals")
abline(0,1,col="red")
qqnorm(studres(FF3factor),main="Q-Q Plot of Studentized Residuals")
abline(0,1,col="red")

anova(Onefactor,FF3factor)

round(c(summary(Onefactor)$r.squared, summary(FF3factor)$r.squared),3)

