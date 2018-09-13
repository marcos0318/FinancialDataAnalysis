rm(list = ls())
setwd("...//Lect3")  # set directory

# QQ plots
q<-c(.01,.025,.05,.1,.15,.25,.5,0.75,0.85,0.9,0.95,0.975,0.99)
q_norm <- round(qnorm(q),3)
q_cauchy <- round(qcauchy(q),3)
q_exp <- round(qexp(q),3)
rbind(q,q_norm,q_cauchy,q_exp)

par(mfrow=c(1,2))
plot(q_norm,q_exp,main="Exp(1) Quantiles vs N(0,1) Quantiles")
plot(q_norm,q_cauchy,main="Cauchy(0,1) Quantiles vs N(0,1) Quantiles")

q <- seq(0.001,0.999,by=0.001)
q_norm1 <- qnorm(q)
q_norm2 <- qnorm(q,3,6)
q_cauchy <- round(qcauchy(q),3)
q_exp <- round(qexp(q),3)

par(mfrow=c(1,2))
plot(q_norm1,q_exp,main="QQ: Exp(1) vs N(0,1)",xlab="",ylab="")
plot(q_norm1,q_cauchy,main="QQ: Cauchy(0,1) vs N(0,1)",xlab="",ylab="")



# Heavy tail
par(mfrow=c(1,1))

x<-seq(3,6,by=0.01)
y_n <- dnorm(x)
y_c <- dcauchy(x)

plot(x,y_c,ylim=c(0,0.031), type="l", main="Right Tails of N(0,1) and Cauchy(0,1)", ylab="Densities",lty=1,lwd=2)
lines(x,y_n,lty=4,col="red",lwd=2)

legend("topright", c("N(0,1)","Cauchy(0,1)"), text.col=c("red","black"), lty = c(4,1),lwd=c(2,2), col=c("red","black"))

q<-seq(0.998,0.9999,by=0.00001)
Q_normal <- qnorm(q)
Q_Cauchy <- tan((q-0.5)*pi)

par(mfrow=c(1,1))
plot(q,Q_normal,type="l",lty=4,lwd=2,col="red", main="N(0,1) Quantiles")

plot(q,Q_Cauchy,type="l",lty=1,lwd=2,col="black",main="Cauchy(0,1) Quantiles")

plot(Q_normal, Q_Cauchy,main="Q-Q Plot of Right Tails of N(0,1) & C(0,1)")


par(mfrow=c(1,1))
plot(q_exp,q_cauchy,main="QQ: Cauchy(0,1) vs Exp(1)",xlab="",ylab="")


plot(q_norm1,q_norm2,main="QQ: N(3,36) vs N(0,1)",xlab="",ylab="")
abline(3,6,col="red") # draw a straight line


# edf 
N <- c(10, 50, 100, 1000)

par(mfrow=c(1,1))
X <- rnorm(N[1])
sort(X)
edf_normal <- ecdf(X)
plot(edf_normal, xlim=c(-4,4), main = paste("EDF of N(0,1) with ", bquote(.(N[1])), " observations"))

par(mfrow=c(2,2))
for (i in 1:4)
{
  X <- rnorm(N[i])
  edf_normal <- ecdf(X)
  plot(edf_normal, xlim=c(-4,4), main = paste("EDF of N(0,1) with ", bquote(.(N[i])), " observations"))
}

x<-seq(-3,3,by=0.001)
y<-pnorm(x)
lines(x,y,col="red")


# empirical QQ
N <- c(10, 50, 100, 1000)

par(mfrow=c(2,2))
for (i in 1:4)
{
  X <- rnorm(N[i])
  qqnorm(X,  main = paste("Empirical Q-Q Plot of N(0,1) with ", bquote(.(N[i])), " observations"))
}
abline(0,1,col="red")


# empirical QQ for DSPLR
SP500 <- read.table("...//Data//DSP500.csv",header = T, sep=",")

attach(SP500)
DSP<-rev(SP500$Close) 
DSPLR <- diff(log(DSP))  # compute log returns; 
                         # try help(diff) for the usage of diff

n <- length(DSPLR)
q <- seq(1/(n+1),n/(n+1),by=1/n)
q_cauchy <- qcauchy(q)

q_norm <- qnorm(q)

par(mfrow = c(1,2))

#qqnorm(DSPLR)
qqplot(q_norm, DSPLR,main="Empirical Quantiles of DSPLR vs N(0,1)",xlab="N(0,1) quantitles", ylab="DSPLR quantiles")

qqplot(q_cauchy, DSPLR, main="Empirical Quantiles of DSPLR vs Cauchy(0,1)",xlab="Cauchy(0,1) quantitles", ylab="DSPLR quantiles")

# Compute VaR under different distribution assumptions
# based on empirical distribution
q <- 0.01 
VaR_emp <- -quantile(DSPLR,q)

# if based on normal
mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)

VaR_normal <- - qnorm(q,mu_DSPLR, sd_DSPLR)
VaR_normal 

# if based on Cauchy
m_DSPLR <- median(DSPLR)
lam_DSPLR <- 1/2 * (quantile(DSPLR,3/4) - quantile(DSPLR,1/4))
c(m_DSPLR, lam_DSPLR)

# Or: MLE fitting
library("MASS") 
fitdistr(DSPLR,"cauchy") 

VaR_cauchy <- - qcauchy(q,m_DSPLR, lam_DSPLR)
VaR_cauchy

c(VaR_normal, VaR_cauchy)


# empirical VaR vs VaR_normal
q<-0.01
VaR_normal <- - qnorm(q,mu_DSPLR, sd_DSPLR)
VaR_emp <- -quantile(DSPLR,q)
c(VaR_normal, VaR_emp)

q <- seq(0.001,0.01,by=0.0002)
q_DSPLR <- quantile(DSPLR,q)
q_norm <- qnorm(q,mu_DSPLR,sd_DSPLR)

par(mfrow=c(1,2))

plot(q,q_DSPLR,ylim=c(-0.07,-0.02),main="Left Tails: DSPLR & Normal",xlab="q",ylab="Quantiles",pch=19)
points(q,q_norm,col="red",pch=23)
legend("bottomright", pch=c(19,23),c("DSPLR","Fitted Normal"), text.col=c("black","red"))

par(mfrow=c(1,2))
plot(q,-q_DSPLR,ylim=c(0.02,0.07),main="VaR based on normal and empirical VaR",xlab="q",ylab="",pch=19)
points(q,-q_norm,col="red",pch=23)
legend("topright", pch=c(19,23),c("DSPLR","Fitted Normal"), text.col=c("black","red"))
plot(q,q_norm/q_DSPLR, main="Ratios between VaR based on normal and empirical VaR ",ylab="Ratios")




# expected shortall
q<-0.01
VaR_emp <- - quantile(DSPLR,q)
ES_emp <- mean(- DSPLR[- DSPLR > VaR_emp])

# to understand the usage of [ ], try
A<-c(1,2,3)
B<-A[A<=2]
B


mu_DSPLR <- mean(DSPLR)
sd_DSPLR <- sd(DSPLR)
VaR_normal <-  - qnorm(q,mu_DSPLR, sd_DSPLR)

N<-100000
X<-rnorm(N,mu_DSPLR,sd_DSPLR)
ES_normal <- mean( - X[- X > VaR_normal])

c(ES_emp, ES_normal)


q<-0.001

VaR_normal <- - qnorm(q,mu_DSPLR, sd_DSPLR)
VaR_emp <- - quantile(DSPLR,q)
ES_emp <- mean(- DSPLR[- DSPLR > VaR_emp])

N<-1000000
X<-rnorm(N,mu_DSPLR,sd_DSPLR)
ES_normal <- mean(- X[- X > VaR_normal])

c(ES_emp, ES_normal)

