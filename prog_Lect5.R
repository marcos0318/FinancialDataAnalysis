rm(list = ls())
setwd("...//Lect5")  # set directory

###### Fit GPD to DSP
library(Rsafd) 
SHAPE.XI <- TRUE

SP500<-read.table("...//DSP500.csv",header = T, sep=",")

attach(SP500)

DSP<-rev(SP500$Close)
n<-length(DSP)
DSPRET <- diff(DSP)/DSP[1:(n-1)] #compute raw returns

par(mfrow=c(1,1))
DSP_time<- seq(from=1960,to=2018,length.out=length(DSP)) 
plot(DSP_time[2:length(DSP)], DSPRET,type="l",xlab="Date",main="Daily Raw return of S&P500 from Jan 1960 to July 2018")

eda.shape(DSPRET)  # Empirical Data Analysis

# choose threshold
shape.plot(DSPRET,tail="upper")
shape.plot(DSPRET,tail="lower")


# fit 
DSPRET.est <- gpd.tail(DSPRET, upper=0.01,lower=-0.011)      
 # check its help file by typing in help(gpd.tail)

# xi
DSPRET.est$upper.par.ests[2]       
DSPRET.est$lower.par.ests[2]       


# goodness of fit
par(mfrow=c(2,1))

tailplot(DSPRET.est,tail="upper")
tailplot(DSPRET.est,tail="lower")

# Another check
par(mfrow=c(1,1))
SDSPRET<-gpd.2q(runif(10000),DSPRET.est)    
 # check its help file

qqplot(DSPRET,SDSPRET,main = "Q-Q Plot of DSPRET vs Simulated")
abline(0,1)
qqnorm(DSPRET)

# compute VaR
VaR_emp <- - quantile(DSPRET,0.005)
VaR_N <- - qnorm(0.005,mean(DSPRET),sd(DSPRET))
VaR_GPD <- - gpd.2q(0.005,DSPRET.est)       
VaR_Ratio <-  (VaR_N - VaR_GPD)/VaR_GPD
round(c(VaR_emp,VaR_N,VaR_GPD,VaR_Ratio),3)


#################

