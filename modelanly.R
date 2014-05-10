

####
#### This function is out of date
####
### For models that use lagged predictors, we can't evaluate the fit
### using the lagged actual predictors, so we need to go through the
### data sequentially for each country to construct the lagged model
predict.simul <- function(object, newdata, lag=5, lagvar="pccement.lag5", type="response") {
    ## deal with the glm absurdity
    pfun <- if("glm" %in% class(object)) {
        function(data) {predict(object=object, newdata=data, type=type)}
    }
    else {
        function(data) {predict(object=object, newdata=data)}
    } 

    ## This function simulates a single country, stepping through the
    ## years, computing the predicted output, and placing that into
    ## the lagged output variable for the appropriate future year.  It
    ## goes without saying that for this to work the lag length must
    ## be evenly divisible by the step size in the data.
    simul <- function(dat) {
        n <- nrow(dat)
        predictions <- 0
        length(predictions) <- n
        for(i in 1:n) {
            predictions[i] <- pfun(dat[i,])
            j   <- min(i+lag,n)               # maximum number of entries further on the lag could be
            yr  <- dat$year[i]+lag       # year that will have this prediction as its lagged data
            if(i<j) {
                for(k in (i+1):j) {
                    if(dat$year[k] == yr) {
                        dat[[lagvar]][k] <- predictions[i]
                        break
                    }
                }
            }
        }
        predictions                 # return the vector of predictions
    }
    
    ## Split the input data by country, apply simul to each country, then reassemble
    unsplit(              # reassemble the results of the lapply into a single vector
            lapply(                         # apply the function below to the split data
                   split(newdata,newdata$ISO), # split the data by country
                   simul),
            newdata$ISO)
    ## ^ return the reassembled vector.
}

### Evaluate the models.  Here's the evaluator (example:  rms.eval(pcc.lm, datasets, "pccement") ) 
rms.eval <- function(model, datasets, prn=TRUE, outvar="pcc.rate") {
    ## Unlike the plotting function, this function wants a list with two datasets: training and testing (FIXME)
    if("glm" %in% class(model)) {
        train.pred   <- predict(object=model, newdata=datasets$training, type="response")
        test.pred    <- predict(object=model, newdata=datasets$testing, type="response")
    }
    else {
        train.pred   <- predict(object=model, newdata=datasets$training)
        test.pred    <- predict(object=model, newdata=datasets$testing)
    }        
    train.actual <- datasets$training[[outvar]]
    test.actual  <- datasets$testing[[outvar]]

    train.err    <- sqrt(mean((train.actual - train.pred)^2))
    test.err     <- sqrt(mean((test.actual - test.pred)^2))

    train.maxloc <- which.max(abs(train.actual - train.pred))
    train.max    <- train.pred[train.maxloc] - train.actual[train.maxloc]
    test.maxloc  <- which.max(abs(test.actual - test.pred))
    test.max     <- test.pred[test.maxloc] - test.actual[test.maxloc]

    if(prn) {
        cat("\n\tmax error entry for training set\n")
        print(datasets$training[train.maxloc,])
        cat("\n\tmax error entry for testing set\n")
        print(datasets$testing[test.maxloc,])
        cat("\n")
    }
    c(training.rmserr=train.err, testing.rmserr=test.err, training.maxerr=train.max, testing.maxerr=test.max)
}


scatterplot.model <- function(model, data, outvar="pcc.rate", pal="Set1", sz=3) {
    ## model is the object returned by a model fitting function like lm, glm, etc.
    ## data is a list of data frames that are themselves subsets of the master table
    ## pal is a palette designator.  It can be the name of a palette or a number 1-8.
    pltdata <- lapply(data,
                      function(dat) {
                          pred <- if("glm" %in% class(model)) {
                              c(predict(object=model, newdata=dat, type="response"))
                          }
                          else {
                              c(predict(object=model, newdata=dat))
                          }
                          act <- dat[[outvar]]
                          data.frame(actual=act, model=pred)
                      })
    for(dset in names(pltdata))
        pltdata[[dset]]$dataset <- dset
    pltdata <- do.call(rbind,pltdata)
    ggplot(data=pltdata, aes(x=actual, y=model, color=dataset)) +
        geom_point(size=sz) +
            ggtitle("Per Capita Cement: model vs. actual") +
                scale_color_brewer(type="qual", palette=pal) +
                    stat_function(fun="identity", color="black", linetype=2, size=1) 
}

## Plot a box plot of the model residuals, grouped by another variable 
boxplot.model.resid <- function(model, data, by, n=25, main="", outvar="pcc.rate") {
    if("glm" %in% class(model))
        resid <- predict(object=model, newdata=data, type="response") - data[[outvar]]
    else
        resid <- predict(object=model, newdata=data) - data[[outvar]]

    if(is.factor(data[[by]]))
        condition <- data[[by]]
    else if(is.character(data[[by]]))
        condition <- as.factor(data[[by]])
    else {
        x      <- data[[by]]
        x0     <- min(x)
        x1     <- max(x)

        ## shift the lower end slightly so that x==x0 doesn't map to the nonexistent bin zero
        eps    <- 1.0e-3
        x0     <- (1+eps)*x0 - eps*x1
        
        width  <- (x1 - x0)/n
        binlbl <- as.factor(signif(x0 + width*((1:n)+0.5), digits=2))
        condition <- sapply(x, function(x){ binlbl[ceiling((x-x0)/width)] })
    }

    f <- data.frame(resid=resid, condition=condition)
    colnames(f)[2] <- by
    boxplot(formula=as.formula(paste("resid",by, sep='~')), data=f, xlab=by, ylab="resid", main=main)
    abline(h=0, lty=2, lwd=2, col="magenta")
}
        
    


mk.country.data <- function(country.list, alldata) {
    sapply(country.list,
           function(country) {
               alldata[alldata$ISO==country,]
           }, simplify=FALSE, USE.NAMES=TRUE)
}

## These are some countries that were challenging in early versions of
## the model (not all of them still are), so they might be worth
## taking a closer look at
problem.countries <- c("ARE", "CYP", "LUX", "OMN", "SAU", "SVN")
pltdata <- c(datasets, mk.country.data(problem.countries, master.nonzero))


#### functions for plotting and analyzing cluster analyses

### make a data frame of the cluster centroids
cluster.centroids <- function(cluster.obj, data) {
    clist <- split(data,cluster.obj$cluster)
    sapply(clist, function(tbl){colMeans(tbl[,cluster.basic])})
}

### plot a box-plot of the cluster variables
cluster.boxplot <- function(cluster.obj, data,
                            vars=c("pccement","urban.growth", "urban.pop", "pcGDP", "GDP.rate"),
                            logs=NULL,
                            layout=NULL) {
    if(is.null(layout)) {
        ## guess the layout of the plots from the number of variables
        n     <- length(vars)
        sqrn  <- sqrt(n)
        sqrnl <- floor(sqrn)
        sqrnh <- ceiling(sqrn)
        if(sqrnl == sqrnh)
            layout <- as.integer(c(sqrnl,sqrnl))
        else if(sqrnl*sqrnh >= n)
            ## more columns than rows
            layout <- as.integer(c(sqrnl,sqrnh))
        else
            layout <- as.integer(c(sqrnh,sqrnh))
    }
    ## If the layout was specified, it's on the user if it's not big enough for all the plots

    if(is.null(logs)) {
        zero   <- sapply(vars, function(v) {any(data[[v]] <= 0)})
        excl   <- sapply(vars, function(v) {
            v %in% c("urban.pop") # predictors we don't want to log, even if they are all positive
        })
        logs   <- ifelse(zero | excl, "", "y")
    }

    par(mfrow=layout)
    cluster <- cluster.obj$cluster

    for(i in 1:n) {
        var  <- vars[i]
        logp <- logs[i]
        form <- as.formula(paste(var,"cluster", sep='~'))
        boxplot(formula=form, data=data, main=var, log=logp)
    }
}
