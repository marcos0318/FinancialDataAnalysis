rm(list = ls())
setwd("...//Lect7")  # set directory

BHData <- read.table("...//SP_BH.csv",sep=',',header=TRUE)
attach(BHData)
head(BHData)

plot(SP,BH, main="BH price VS SP500 index")

# LS regression
BHSPl2 <- lsfit(SP,BH)
BHSPl2$coef
plot(SP,BH)
abline(BHSPl2)

# LAD regression
library(Rsafd)
BHSPl1 <- l1fit(SP,BH)
BHSPl1$coef
plot(SP,BH)
abline(BHSPl1)

#predictions
NEWSPi<-1800 #interpolation
predi2<-BHSPl2$coef[1]+BHSPl2$coef[2]*NEWSPi
predi1<-BHSPl1$coef[1]+BHSPl1$coef[2]*NEWSPi
round(c(predi2,predi1,predi2 - predi1),0)
  
NEWSPe<-3000 #extrapolation (more risky!)
prede2<-BHSPl2$coef[1]+BHSPl2$coef[2]*NEWSPe
prede1<-BHSPl1$coef[1]+BHSPl1$coef[2]*NEWSPe
round(c(prede2,prede1,prede2 - prede1),0)

#Robustness
NEWBH<-c(BH[1:60], BH[61:70] - 100000, BH[71:115])
  
par(mfrow=c(1,2))

NBHSPl2<-lsfit(SP,NEWBH)
plot(SP,NEWBH,main="LS Regression: Original vs Perturbed")
abline(NBHSPl2,col="red")
abline(BHSPl2,lty=4)
  
NBHSPl1<-l1fit(SP,NEWBH)
plot(SP,NEWBH,main="LAD Regression: Original vs Perturbed")
abline(NBHSPl1,col="red")
abline(BHSPl1,lty=4)


#perturbed predictions
newpredi2<-NBHSPl2$coef[1]+NBHSPl2$coef[2]*NEWSPi
newpredi1<-NBHSPl1$coef[1]+NBHSPl1$coef[2]*NEWSPi
newprede2<-NBHSPl2$coef[1]+NBHSPl2$coef[2]*NEWSPe
newprede1<-NBHSPl1$coef[1]+NBHSPl1$coef[2]*NEWSPe
# LS: Compare with previous results
round(c(predi2, newpredi2,predi2 - newpredi2),0) 
round(c(prede2, newprede2,prede2 - newprede2),0)
# LAD: Compare with previous results
round(c(predi1, newpredi1,predi1 - newpredi1),0) 
round(c(prede1, newprede1,prede1 - newprede1),0)
  

# diagonostics
# for LS regression
par(mfrow=c(2,1))
plot(BHSPl2$residuals,type="l",main="Residual Plot of LS Regression of BH Against SP500 ")

### to see positive association btw adjacent residuals
n <- length(BH)
plot(BHSPl2$residuals[-n],BHSPl2$residuals[-1],main="Residual against previous residual ")
cor(BHSPl2$residuals[-n],BHSPl2$residuals[-1])

# for LAD regression
plot(BHSPl1$residuals,type="l",main="Residual Plot of LAD Regression of BH Against SP500 ")
plot(BHSPl1$residuals[-n],BHSPl1$residuals[-1],main="Residual against previous residual ")
cor(BHSPl1$residuals[-n],BHSPl1$residuals[-1])


