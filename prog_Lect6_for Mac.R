rm(list = ls())
setwd("...//Lect6")  # set directory

library(Rsafd)

data(BCofLRet)
data(CCofLRet)

CofLR <- cbind(BCofLRet,CCofLRet)


# Scatterplot

plot(BCofLRet,CCofLRet,main="Scatter plot of B C Coffee LR")


# Corr, cov etc
BC_corr <- cor(BCofLRet,CCofLRet)
BC_corr

BC_cov <- cov(BCofLRet,CCofLRet)
B_var <- var(BCofLRet)
C_var <- var(CCofLRet)
c(BC_cov, B_var,C_var)

c(BC_corr,BC_cov/sqrt(B_var * C_var))

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


## check whether BC LR follows bivariate normal
NZ <-(BCofLRet != 0 & CCofLRet !=0)
BLRet <- BCofLRet[NZ]
CLRet <- CCofLRet[NZ]

Mu<- c(mean(BLRet),mean(CLRet))
Sigma<-var(cbind(BLRet,CLRet))
N<-length(BLRet)
BCsim<-rmvnorm(N,mean=Mu,cov=Sigma)
par(mfrow=c(1,2))
range.x <- c(min(BLRet),max(BLRet))
range.y <- c(min(CLRet),max(CLRet))
plot(BLRet,CLRet,main="Scatterplot of B C coffee LR")
plot(BCsim,xlim=range.x,ylim=range.y,,main="Scatterplot of simulated fitted normal")


















