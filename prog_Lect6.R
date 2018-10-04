rm(list = ls())
setwd("...//Lect6")  # set directory


################# Coffee data
library(Rsafd)

data(BCofLRet)
data(CCofLRet)

CofLR <- cbind(BCofLRet,CCofLRet)

summary(BCofLRet)
summary(CCofLRet)

### density plot of bivariate normal
dmvnorm_2d <- function (x, mean, sd, rho, log = FALSE) 
 # there's some problem with the dmvnorm function in the Rsafd package, 
 # let's create a one specifically for 2D case
 {
    d <- 2
    if (is.vector(x)) 
        x <- matrix(x, ncol = d)
    if (missing(mean)) 
        mean <- rep(0, d)
    if (!is.vector(mean)) 
        stop("mean must be a vector")
    d.mean <- length(mean)
    if (missing(sd) || missing(rho)) 
            stop("Both parameters sd and rho must be present")
    sigma <- (1 - rho) * diag(sd * sd) + rho * outer(sd, 
        sd, "*")
    distval <- mahalanobis(x, center = mean, cov = sigma)
    logdet <- sum(log(eigen(sigma, symmetric = TRUE, only.values = TRUE)$values))
    logretval <- -(d * log(2 * pi) + logdet + distval)/2
    if (log) 
        return(logretval)
    exp(logretval)
}

mu <- c(1,-1)
sigma <- c(1,2)
rho <-   0.5 # try rho = -0.5, 0, 0.5, 0.9
#rho <- 0

x_grid <- seq((1-4),(1+4), length.out = 256)
y_grid <- seq((-1 - 4*2), (-1 + 4*2), length.out = 256)
xmat <- rep(x_grid,256)
ymat <- rep(y_grid,each=256)
grid <- cbind(xmat,ymat)

mvnorm_density <- dmvnorm_2d(grid,mean=mu,sd=sigma,rho=rho) 

library(rgl) # package needed to draw a persp 3d plot
persp3d(x_grid, y_grid, mvnorm_density, aspect = c(1, 1, 0.5), col = "lightblue", zlab = "Density")

#hist 2d

library(gplots) # need to install gregmisc_bundle
#help(hist2d)

h2d <- hist2d(BCofLRet,CCofLRet,show=FALSE, same.scale=TRUE)
par(mfrow=c(1,1)) 
persp(h2d$x, h2d$y, h2d$counts, ticktype="detailed",main = "Histgram of B C Coffee LR", theta=30, phi=30, expand=0.5, shade=0.5, col="lightblue", ltheta=-30)

# hist: removing 0
NZ <-(BCofLRet != 0 & CCofLRet !=0)
BLR <- BCofLRet[NZ]
CLR <- CCofLRet[NZ]

h2d_NZ <- hist2d(BLR,CLR,show=FALSE, same.scale=TRUE)
par(mfrow=c(1,1))
persp( h2d_NZ$x, h2d_NZ$y, h2d_NZ$counts, ticktype="detailed", main = "Histgram of B C Coffee LR After Removing 0's",  theta=30, phi=30, expand=0.5, shade=0.5, col="cyan", ltheta=-30)


# hist: restricted [-0.1,0.1]^2
bulk <-(BLR > -0.1 & BLR < 0.1 & CLR > -0.1 & CLR < 0.1)
BLR_bulk <- BLR[bulk]
CLR_bulk <- CLR[bulk]

h2d_bulk <- hist2d(BLR_bulk,CLR_bulk,show=FALSE, same.scale=TRUE)
par(mfrow=c(1,1))
persp( h2d_bulk$x, h2d_bulk$y, h2d_bulk$counts, ticktype="detailed", main = "Histgram of B C Coffee LR Around Center", theta=30, phi=30, expand=0.5, shade=0.5, col="cyan", ltheta=-30)

### KDE
kdest(BLR_bulk,CLR_bulk)

# Scatter plot
plot(BCofLRet,CCofLRet,main="Scatter plot of B C Coffee LR")

NZ <-(BCofLRet != 0 & CCofLRet !=0)
BLR <- BCofLRet[NZ]   # remove the zeros
CLR <- CCofLRet[NZ]
plot(BLR,CLR,main="Scatter Plot of B C Coffee LR After Removing 0's")

##########
cov_BCLR <- cov(BLR, CLR)
var_BLR <- var(BLR)
var_CLR <- var(CLR)
rho_BCLR <- cor(BLR, CLR)
c(cov_BCLR/sqrt(var_BLR*var_CLR), rho_BCLR)

rho_BCLR <- cor(BLR, CLR)
rho_BCLR

fit_BCLR <- lm(CLR ~ BLR)
summary(fit_BCLR)

c(summary(fit_BCLR)$r.squared, rho_BCLR^2)

### cor and linear regression
n <- 10000
X <- rnorm(n)
Y <- X^2
par(mfrow=c(1,1))
plot(X,Y) 
cor(X,Y)

## check whether BC LR follows bivariate normal
Mu<- c(mean(BLR),mean(CLR))
Sigma<-var(cbind(BLR,CLR))
N<-length(BLR)
BCsim<-rmvnorm(N,mean=Mu,cov=Sigma)
par(mfrow=c(1,2))
plot(BLR,CLR,xlim=c(-0.4,0.3),ylim=c(-0.22,0.4),main="Scatter Plot of BCCoffeeLR")
plot(BCsim,,xlim=c(-0.4,0.3),ylim=c(-0.22,0.4),main="Scatter Plot of Simulated Fitted Normal")

## separate normal but not jointly normal
n<-1000
X <- rnorm(n)
Y<- 2*(rbinom(n,1,0.5) - 0.5) * X
qqnorm(X)
qqnorm(Y)

plot(X,Y)

























