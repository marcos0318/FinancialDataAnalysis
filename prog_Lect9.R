rm(list = ls())
setwd("...//Lect9")  # set directory

# Gas future data
Gas_Future_2018<-read.csv("...//GAS_Future_2018_Aug03.csv",skip=1,header=T)
attach(Gas_Future_2018)
head(Gas_Future_2018)

Gas_Future <- Settle[1:36]
Delivery_month <- seq(1,36)

plot(Delivery_month, Gas_Future, xlab="Delivery Month", main = "Gas Future Contract Values on Aug 03, 2018")

# polynomial regression
n<-100
x <- rnorm(n,0,1)
y<- x^2 + rnorm(n,0,0.5)
plot(x,y,main="Polynomially Related Data")

fit_2 <- lm(y ~ poly(x,degree=2,raw=TRUE))  # try raw=FALSE to see the difference, see \S 4.6.3
summary(fit_2)


# Model diagnostics
library(Rsafd)
fit_2.diag <- lm.diag(fit_2)
ts.plot(fit_2.diag$studres,main="Studentized Residuals vs fitted")
qqnorm(fit_2.diag$studres)

# check fit
par(mfrow=c(1,1))
plot(x,y,main="Polynomially Related Data")

ranks <- order(x)
x_ordered <- x[ranks]
lines(x_ordered,fit_2$fitted[ranks],lty = 2, col = 2,lwd=2)

# wrong way of adding the fitted line:
# lines(x,fit_2$fitted,lty = 2, col = 2,lwd=2)

# poly for double-exp
n <- 100
x <- rnorm(n,0,1)
y <- x^2 +  (2*rbinom(n,1,0.5) - 1) * rexp(n,1)  # (2*Bernoulli(0.5) - 1 )* rexp = double-exp
plot(x,y,main="Polynomially Related Data")

fit_2_LAD <- l1fit(poly(x, degree = 2,raw=TRUE),y)
summary(fit_2_LAD)

ranks <- order(x)
x_ordered <- x[ranks]
lines(x_ordered,fit_2_LAD$fitted[ranks],lty = 2, col = 2,lwd=2)


# fit Gas future
plot(Delivery_month, Gas_Future, xlab="Delivery Month", main = "Gas Future Contract Values on Aug 03, 2018")

lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, 3))),lty = 1, col = 1,lwd=1.5)
lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, 6))),lty = 2, col = 2,lwd=1.5)
lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, 9))),lty = 4, col = 4,lwd=1.5)
lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, 12))),lty = 6, col = 6,lwd=1.5)

legend("topright", c("Degree = 3", "Degree = 6", "Degree = 9","Degree = 12"), lty = c(1,2,4,6), col = c(1,2,4,6), lwd=c(1.5,1.5,1.5,1.5))

# choose p
fit_12 <- lm(Gas_Future ~ poly(Delivery_month, 12, raw = TRUE))
fit_9 <- lm(Gas_Future ~ poly(Delivery_month, 9, raw = TRUE))
anova(fit_9, fit_12)

summary(fit_12)
summary(fit_9)

fit_7 <- lm(Gas_Future ~ poly(Delivery_month, 7, raw = TRUE))
anova(fit_7, fit_9)

### R2 as a function of degree
R2 <- rep(0,24)
for (i in 1:24)
{
  fit <- lm(Gas_Future ~ poly(Delivery_month,i))
  R2[i] <- summary.lm(fit)$r.squared
}

plot(R2,xlab="p",main="R^2 vs degree p")


#prediction
p <- 9  # if use p = 9
I <- seq(36,42)
T<-length(I)
coef <- lm(Gas_Future ~ poly(Delivery_month,  degree = p, raw = TRUE))$coef
pred <- rep(0,T)
for (i in 1:T)
{
  pred[i] <- sum(coef * I[i]^(0:p))
}

plot(Gas_Future,xlim=c(0,42),ylim=c(-10,3.3),xlab="Delivery Month",  main = "Polynomial Gas Future Price Prediction")
lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, p))), lty = 2, col = 2,lwd=1.5)
lines(I,pred, lty = 4, col = 4,lwd=1.5)
legend("bottomleft", c("Fitted", "Predicted"), lty = c(2,2), col = c(2,4), lwd=c(1.5,1.5))

