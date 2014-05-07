####
#### Code for fitting an autoregression model to the cement data
####

if(!exists("master.table"))
    master.table <- dget(file="cement-data.dat")

columns.armod <- c("pccement.lag5", "urban.growth", "urban.pop", "pcGDP", "GDP.rate")
## vector to reorder the coefficients of one of the linear models we fit
lm.reorder <- c(columns.armod, "(Intercept)")

data.armod <- master.table[master.table$year %% 5 == 4,c(columns.armod,"pccement","ISO")]
data.armod <- split(data.armod, data.armod$ISO, drop=TRUE)

## This is a straight-up linear log likelihood.  It actually returns
## the negative of the likelihood, so optim can minimize it without
## any additional fiddling. 
## TODO: put together one using the earth model
ar.lag = 5
loglik.ar <- function(coefs) {
    ## coefs are c(ar.coef, columns.armod[2:...], intercept)
    sum(sapply(data.armod,
               function(tbl) {
                   n <- nrow(tbl)
                   if(n<ar.lag+1)          # not enough data for this country.  Skip
                       return(0)
                   
                   yhat           <- rep(0,n)
                   yhat[1:ar.lag] <- tcrossprod(coefs, as.matrix(cbind(tbl[1:ar.lag,columns.armod], 1)))
                   for(i in (ar.lag+1):nrow(tbl)) {
                       x       <- cbind(tbl[i,columns.armod], 1)
                       x[1]    <- yhat[i-ar.lag]     # the previous value of yhat
                       yhat[i] <- sum(coefs*x)
                   }
                   -sum(loglik.ar.single(yhat, tbl$pccement)) # return value.  Here's where we put the - sign in.
               }))
}

loglik.ar.single <- function(yhat, y) {
    yhat <- max(yhat, 0)
    ifelse(yhat==0,
           ifelse(y==0, 0,               # match: zero predicted, zero actual
                  dlnorm(y, meanlog=1.0e-6, sdlog=0.5, log=TRUE) - (y-yhat)),
           ifelse(y==0,
                  dlnorm(yhat, meanlog=1.0e-6, sdlog=0.5, log=TRUE) - (yhat-y),
                  dlnorm(y, meanlog=yhat, sdlog=0.5, log=TRUE)))
}

