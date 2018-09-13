rm(list = ls())
setwd("...//Lect4")  # set directory

# LLN
n <- 1000
Z <- rnorm(n)
sample_mean <- cumsum(Z)/seq(1:n)

par(mfrow=c(2,1))
plot(sample_mean,type="l",xlab="n",ylab="sample mean", main="Sample Mean of N(0,1) -> 0 as n -> infinity")
abline(h=0,lty=2,col="red")

cond_mean <- cumsum(Z*(Z>0))/cumsum(Z>0)
plot(cond_mean,type="l",xlab="n",ylab="sample mean", main=expression(paste("Sample Mean of N(0,1)'s > 0 -> ", 2/sqrt(2*pi), " as n -> infinity")))
abline(h=2/sqrt(2*pi),lty=2,col="red")

# Cauchy simulation
N<-100000
U <- runif(N,0,1)
X <- tan((U-0.5)*pi)

q <- seq(0.01,0.99,by=0.001)
q_Cauchy <- qcauchy(q)
q_emp <- quantile(X,q)

par(mfrow=c(1,1))
plot(q_Cauchy, q_emp,xlab="theoretical quantiles", ylab="Empirical Quantiles", 
 main=paste("Empirical Q-Q Plot of ", bquote(.(N)), " Simulated Cauchy(0,1) vs Cauchy(0,1)"))
abline(0,1,col="red")

SP500 <- read.table("...//Data//DSP500.csv",header = T, sep=",")

attach(SP500)
DSP<-rev(SP500$Close)

DSPLR <- diff(log(DSP))  # compute log returns; 

# Cauchy(0,1) vs N(0,1)
N<- length(DSPLR)# = length(DSPLR)        
N
GWN <- rnorm(N)
CWN <- rcauchy(N)

par(mfrow=c(2,1))
ts.plot(GWN,main=paste("Plot of ", bquote(.(N)), " N(0,1)"),xlab="index",ylab="")
ts.plot(CWN,main=paste("Plot of ", bquote(.(N)), " C(0,1)"),xlab="index",ylab="")

# extreme values: DSPLR
par(mfrow=c(1,1))
DSP_time<- seq(from=1960,to=2018,length.out=length(DSP)) 
plot(DSP_time[2:length(DSP)], DSPLR,type="l",xlab="Date",main="Daily log return of S&P500 from Jan 1960 to July 2018")

 mean(DSPLR)
 sd(DSPLR)
 min(DSPLR)
 (min(DSPLR)-mean(DSPLR))/sd(DSPLR)


# GPD as a location-scale family

library(Rsafd)   # load the package
SHAPE.XI <- T
n<-10000
X1 <- rpareto(n, m=0, lambda = 1, xi = 1)
X2 <- rpareto(n, m=2, lambda = 3, xi = 1)
par(mfrow=c(1,1))
qqplot(X1,X2,xlim=c(0,60),ylim=c(0,180),xlab="GPD(0,1,1)",ylab="GPD(2,3,1)",main="Empirical Q-Q Plot of GPD(2,3,1) vs GPD(0,1,1)")
abline(2,3,col="red")
legend("bottomright","y=3x+2", text.col="red")

# Tails of GPD 

xi <- c(2,1,0.5)

x <- seq(6,12,by=0.01)
N<-length(x)

Y <- matrix(0,4,N)

for (i in 1:3)
{
  Y[i,] <- (1+xi[i]*x)^(-1-1/xi[i])
}

Y[4,] <- dnorm(x)

par(mfrow=c(1,1))
plot(x,Y[1,],type="l",lty=1,lwd=2,ylim=c(0,0.023),ylab="",main = expression(paste("GPD(0,1,",xi,") with Different ", xi, "'s vs N(0,1)")))
lines(x,Y[2,],lty=2,col="red",lwd=2)
lines(x,Y[3,],lty=3,col="blue",lwd=2)
lines(x,Y[4,],lty=4,col="purple",lwd=2)

legend("topright", c(expression(paste(xi,"=2")), expression(paste(xi,"=1")),expression(paste(xi,"=0.5")),"N(0,1)"), text.col=c("black","red","blue","purple"), lty = c(1,2,3,4),lwd=c(2,2,2,2), col=c("black","red","blue","purple"))

# Use QQ to compare tails
n<-10000
GPD_2 <- rpareto(n, m=0, lambda = 1, xi = 2)
GPD_1 <- rpareto(n, m=0, lambda = 1, xi = 1)
GPD_05 <- rpareto(n, m=0, lambda = 1, xi = 0.5)
Cauchy <- abs(rcauchy(n))
Z <- abs(rnorm(n))

par(mfrow=c(2,2))
qqplot(GPD_1,GPD_2,xlab="GPD(0,1,1)",ylab="GPD(0,1,2)",xlim=c(0,400),ylim=c(0,8*10^4),main="Q-Q Plot of GPD(0,1,2) vs GPD(0,1,1)")
qqplot(GPD_1,GPD_05,xlab="GPD(0,1,1)",ylab="GPD(0,1,0.5)",xlim=c(0,400),ylim=c(0,40),main="Q-Q Plot of GPD(0,1,0.5) vs GPD(0,1,1)")
qqplot(Cauchy,GPD_1,xlab="Cauchy(0,1)",ylab="GPD(0,1,1)",,xlim=c(0,250),ylim=c(0,400),main="Q-Q Plot of GPD(0,1,1) vs Cauchy(0,1)")
abline(0,pi/2,col="red")
#legend("bottomright","y=pi/2*x", text.col="red")
qqplot(Z,GPD_1,xlab="N(0,1)",ylab="GPD(0,1,1)",,xlim=c(0,3),ylim=c(0,400),main="Q-Q Plot of GPD(0,1,1) vs N(0,1)")

par(mfrow=c(1,1))
KD9 <- density(DSPLR, bw = .001) 
hist(DSPLR,breaks=50,  freq = F,main="Histogram & KDE of DSPLR, #bin = 50, bw=0.001",ylim=c(0,65))   
points(KD9,type="l",col="red")


# Compare right tail of DSPLR with GPD


DSPLR_p <- DSPLR[DSPLR > 0.013]
N <- length(DSPLR_p)

par(mfrow=c(1,2))
GPD_1 <- rpareto(N, m=0, lambda = 1, xi = 0.5)
qqplot(GPD_1,DSPLR_p,xlab="GPD(0,1,0.5)",ylab="DSPLR_p",main="Q-Q Plot of Positive DSPLR vs GPD(0,1,0.5)")
GPD_2 <- rpareto(N, m=0, lambda = 1, xi = 0.005)
qqplot(GPD_2,DSPLR_p,xlab="GPD(0,1,0.005)",ylab="DSPLR_p",main="Q-Q Plot of Positive DSPLR vs GPD(0,1,0.005)")

