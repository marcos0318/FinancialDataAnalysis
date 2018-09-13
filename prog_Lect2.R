rm(list = ls())
setwd("...")  # set directory


SP500 <- read.table("DSP500.csv",header = T, sep=",")

attach(SP500)
summary(SP500)
names(SP500)

DSP<-rev(SP500$Close)

DSP_time<- seq(from=1960,to=2018,length.out=length(DSP)) 

plot(DSP_time,DSP,type="l",xlab="Date",main="Daily S&P500 index from Jan 1960 to July 2018")

DSPLR <- diff(log(DSP))  # compute log returns; 
                         # try help(diff) for the usage of diff

plot(DSP_time[2:length(DSP)], DSPLR,type="l",xlab="Date",main="Daily log return of S&P500 from Jan 1960 to July 2018")

par(mfrow=c(1,2))  
# divide the plotting area into 1 by 2 to facilitate comparison 
plot(DSP_time,DSP,type="l",xlab="Date",main="Daily S&P500 index from Jan 1960 to July 2018")
plot(DSP_time[1:length(DSPLR)], DSPLR,type="l",xlab="Date",main="Daily log return of S&P500 from Jan 1960 to July 2018")


##### common distributions

# normal
x<-seq(-4,4,by=0.01)
plot(x,pnorm(x),type="l",lwd=2 )

plot(x,dnorm(x),type="l",lwd=2 )

x<-seq(-5,5,by=0.01)
plot(x,dnorm(x),type="l",lwd=3)
lines(x,dnorm(x,mean=2),lty=2,lwd=3,col="red")
lines(x,dnorm(x,mean=0,sd=2),lty=5,lwd=3,col="blue")
legend("topleft", c("A, N(0,1); B, N(2,1);  C, N(0,4)", "", "",""),lty=c(0,1,2,5),lwd=c(3,3,3,3),col=c("black","black","red","blue"),
	text.col=c("black","black","red","blue"))


# uniform
x<-seq(-0.5,1.5,by=0.01)
par(mfrow=c(1,2))
plot(c(0,1),c(1,1),xlim=c(-0.5,1.5),ylim=c(0,1.1),type="l",lwd=2,ylab="dunif(x)",xlab="x")
lines(c(-0.5,0),c(0,0),lwd=2)
lines(c(1,1.5),c(0,0),lwd=2)

plot(x,punif(x),type="l",lwd=2)

# exponential
x<-seq(0,5,by=0.01)
par(mfrow=c(1,1))
plot(x,dexp(x),type="l",lwd=3)
lines(x,dexp(x,rate=2),lty=2,lwd=3,col="red")
lines(x,dexp(x,rate=0.5),lty=5,lwd=3,col="blue")
legend("topright", c("A, Exp(1); B, Exp(2);  C, Exp(0.5)", "", "",""),lty=c(0,1,2,5),lwd=c(3,3,3,3),col=c("black","black","red","blue"),
	text.col=c("black","black","red","blue"))

# Cauchy
x<-seq(-5,5,by=0.01)
plot(x,dcauchy(x),type="l",lwd=3)
lines(x,dcauchy(x,location=2),lty=2,lwd=3,col="red")
lines(x,dcauchy(x,scale=2),lty=5,lwd=3,col="blue")
legend("topleft", c("A, C(0,1); B, C(2,1);  C, C(0,2)", "", "",""),lty=c(0,1,2,5),lwd=c(3,3,3,3),col=c("black","black","red","blue"),
	text.col=c("black","black","red","blue"))

# Normal vs Cauchy
x<-seq(-5,5,by=0.01)
plot(x,dnorm(x),type="l",lwd=3,ylab="")
lines(x,dcauchy(x),lty=2,lwd=3,col="red")
legend("topright", c("N(0,1)","C(0,1)"),lty=c(1,2),lwd=c(3,3),col=c("black","red"),text.col=c("black","red"))


################# 

# histogram
par(mfrow=c(2,2))  # to divide the plotting area into 2 by 2  
hist(DSPLR, breaks = 20, freq = F, main="Histogram of DSPLR, #bins = 20")   
   # try help(hist) to learn its options
hist(DSPLR, breaks=50, freq = F,main="Histogram of DSPLR, #bins = 50") 
hist(DSPLR,breaks=500,  freq = F,main="Histogram of DSPLR, #bins = 500")   
hist(DSPLR,breaks=5000, freq = F,main="Histogram of DSPLR, #bins = 5000")

# histogram
par(mfrow=c(1,1)) 
hist(DSPLR,breaks=100,  freq = F,main="Histogram of DSPLR vs Fitted Normal Density")   

mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)

x<-seq(-0.2,0.1,by=0.001)
y<-dnorm(x,mean=mu_DSPLR,sd = sd_DSPLR)
points(x,y,type="l",col="red")


# kernel density
## KDE illustration
n<-10
X<-rnorm(n)
y0<-rep(0,n)

bw <- 0.3

par(mfrow = c(2,2))
plot(X,y0, xlim=c(-3,3), ylim=c(0,1.3), main = paste(bquote(.(n)), " N(0,1) Observations"))

plot(X,y0, xlim=c(-3,3), ylim=c(0,1.3), main = paste(bquote(.(n)), " N(0,1) Observations & the First 5 Kernels"))
x<-seq( - 3,  + 3, by = 0.01)
for (i in 1:5)
{
 KD <- dnorm(x, X[i], bw) 
 points(x, KD, type="l", col = i)
}

plot(X,y0, xlim=c(-3,3), ylim=c(0,0.5), main = paste(bquote(.(n)), " Observations & the Average of First 5 Kernels"))
KD<- rep(0,601)
for (i in 1:5)
{
 KD <- KD + dnorm(x, X[i], bw) 
}
KD <- KD/5
points(x, KD, type="l", col = "red")


plot(X,y0, xlim=c(-3,3), ylim=c(0,0.6), main = paste(bquote(.(n)), " Observations & the Average of All Kernels"))
KD<- rep(0,601)
for (i in 1:n)
{
 KD <- KD + dnorm(x, X[i], bw) 
}
KD <- KD/n
points(x, KD, type="l", col = "red")

# superimpose the N(0,1) density
y1<-dnorm(x)
points(x, y1, type="l", col = "black")


# different kernel
par(mfrow=c(2,2))
KD5 <- density(DSPLR, kernel = "gaussian", bw = .005) 
plot(KD5, type="l",ylim=c(0,55), main="KDE of DSPLR with Gaussian Kernel")
KD6 <- density(DSPLR, kernel = "rectangular", bw = .005) 
plot(KD6, type="l",ylim=c(0,55), main="KDE of DSPLR with rectangular Kernel")
KD7 <- density(DSPLR, kernel = "triangular", bw = .005) 
plot(KD7, type="l",ylim=c(0,55), main="KDE of DSPLR with triangular Kernel")
KD8 <- density(DSPLR, kernel = "cosine", bw = .005) 
plot(KD8, type="l", ylim=c(0,55),main="KDE of DSPLR with cosine Kernel")


# different bandwidth
par(mfrow=c(2,2))
KD1 <- density(DSPLR, kernel = "gaussian", bw = .01) 
plot(KD1, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.01")
KD2 <- density(DSPLR, kernel = "gaussian", bw = .005) 
plot(KD2, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.005")
KD3 <- density(DSPLR, kernel = "gaussian", bw = .001) 
plot(KD3, type="l",ylim=c(0,65), main="KDE of DSPLR with BW=.001")
KD4 <- density(DSPLR, kernel = "gaussian", bw = .00001) 
plot(KD4, type="l", main="KDE of DSPLR with BW=.00001")



# histogram and Kernel
par(mfrow=c(2,1))
hist(DSPLR,breaks=50,  freq = F,main="Histogram & KDE of DSPLR, #bin = 50, bw=0.01",ylim=c(0,65))   
KD4 <- density(DSPLR, kernel = "gaussian", bw = .01) 
points(KD4,type="l",col="red")

KD9 <- density(DSPLR, kernel = "gaussian", bw = .001) 
hist(DSPLR,breaks=50,  freq = F,main="Histogram & KDE of DSPLR, #bin = 50, bw=0.001",ylim=c(0,65))   
points(KD9,type="l",col="red")


# default choice of bw
KD0 <- density(DSPLR, kernel = "gaussian") 
hist(DSPLR,breaks=50,  freq = F,main="Histogram & KDE of DSPLR, #bin = 50, bw=default",ylim=c(0,65))   
points(KD0,type="l", col="red")


KD0
names(KD0)
KD0$bw
