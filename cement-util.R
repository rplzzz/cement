require(reshape)

####
#### Various functions for working with the cement data and models
#### based on it.
####

### General functions

datefix <- function(dd) {as.numeric(substr(as.character(dd),2,5))}
select.complete <- function(dd) {dd[complete.cases(dd),]}
to.ISO <- function(dd) {
    dd$country <- tolower(dd$country)
    subset(merge(dd, ccodes, by="country", all.x=TRUE), select= -c(two.letter, numerical, country))
}
reorg.by.yr <- function(dd, name) {
    dd.m <- melt(dd, variable="xyear")
    names(dd.m)[names(dd.m)=="value"] <- name
    dd.m$year <- datefix(dd.m$xyear)
    dd.m$xyear <- NULL
    dd.m
}


### Model analysis

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


#### functions for plotting and analyzing cluster analyses

clust.normalize <- function(data) {
    for(name in names(data)) {
        data[[name]] <- data[[name]] / max(abs(data[[name]]))
    }
    data
}

### Find the countries in each of the clusters
clust.countries <- function(data, clustid) {
    clusters <- split(data,clustid)
    lapply(clusters, function(d) {unique(sort.int(d$ISO, method="quick"))})
}

### Find the data entries in a cluster
clust.members <- function(data, clustid) {
    data$ID <- rownames(data)
    clusters <- split(data, clustid)
    lapply(clusters, function(d) {d$ID})
} 

### Find the clusters that contain a country.  There may be
### more than one because each country has several years of data
clust.srch <- function(iso, country.list) {
    sapply(country.list, function(l) {iso %in% l})
}

### Similarity measure for cluster member lists (will work for country lists too)
mlist.sim <- function(cl1, cl2) {
    ## Edit distance is the number in 1 not in 2, plus number in 2 not in 1
    sum(!(cl1 %in% cl2)) + sum(!(cl2 %in% cl1))
}

### Matrix of edit distances for two lists of cluster memberships (as
### produced by clust.members or clust.countries)
mlist.sim.matrix <- function(cm1, cm2) {
    ## The elements of cm1 are represented on the rows of the matrix,
    ## and the elements of cm2 on the columns
    matrix(
        sapply(cm2, function(c2) {sapply(cm1, function(c1) {mlist.sim(c2,c1)})}),
        nrow= length(cm1))
}

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


### fit a separate model to the data in each cluster
cluster.regression <- function(cluster.obj, data,
                             formula=pcc.rate~GDP.rate,
                             fitfun=lm, testset=NULL,
                             ...)
{
    cdata <- split(data, as.factor(cluster.obj$cluster))
    if(!is.null(testset)) {
        cdata <- lapply(cdata, function(d) {d[!d$ISO %in% testset,]})
    }
    lapply(cdata, function(d){fitfun(formula=formula, data=d, ...)})
}


### perform clustering and fit cluster regression
cluster.model <- function(data, clustvars, extravars, formula, centers, fitfun=lm,
                          testset=NULL, ...)
{
    allvars          <- c(clustvars, extravars, "ISO", "year")
    raw.table        <- select.complete(data[,allvars])
    normalized.table <- clust.normalize(raw.table[,clustvars])
    km               <- kmeans(normalized.table, centers=centers)
    model            <- cluster.regression(km, raw.table, formula, fitfun, testset, ...)

    list(table=raw.table, km=km, model=model)
}
    


### predict values for a dataset when a separate model has been fit
### for each cluster (e.g. as returned by cluster.regression)
cluster.predict <- function(clustid, data, modellist)
{
    cdata <- split(data, as.factor(clustid))
    unsplit(
        mapply(function(dat, model) {
            if("glm" %in% class(model)) {predict(object=model, newdata=dat, type="response")}
            else {predict(object=model, newdata=dat)}},
               cdata, modellist, SIMPLIFY=FALSE),
        clustid) 
}


### evaluate rms error for a model comprising a collection of cluster
### submodels.  Unfortunately, this involves some duplication from
### rms.eval
cluster.rms.eval <- function(cluster.obj, data, modellist, testing.set=NULL, outvar="pcc.rate", prn=TRUE)
{
    data.split  <- split(data, data$ISO %in% testing.set)
    clust.split <- split(cluster.obj$cluster, data$ISO %in%testing.set)
    
    train.pred   <- cluster.predict(clust.split$`FALSE`, data.split$`FALSE`, modellist)
    train.actual <- data.split$`FALSE`[[outvar]]
    train.err    <- sqrt(mean((train.pred-train.actual)^2))
    train.maxloc <- which.max(abs(train.pred-train.actual))
    train.max    <- train.pred[train.maxloc] - train.actual[train.maxloc]
    if(prn) {
        cat("\nmax error entry for training set:\n")
        print(data.split$`FALSE`[train.maxloc,])
    }
    
    if(!is.null(testing.set)) {
        test.pred   <- cluster.predict(clust.split$`TRUE`, data.split$`TRUE`, modellist)
        test.actual <- data.split$`TRUE`[[outvar]]
        test.err    <- sqrt(mean((test.pred-test.actual)^2))
        test.maxloc <- which.max(abs(test.pred-test.actual))
        test.max    <- test.pred[test.maxloc] - test.actual[test.maxloc]
        if(prn) {
            cat("\nmax error entry for testing set:\n")
            print(data.split$`TRUE`[test.maxloc,])
        }
    }
    else {
        test.err = 0
        test.max = 0
    }
    
    
    c(training.rmserr=train.err, testing.rmserr=test.err,
      training.maxerr=train.max, testing.max=test.max)
      
}

### plot predicted vs. actual for cluster blah, blah, blah
cluster.scatterplot.model <- function(cluster.obj, data, modellist, outvar="pcc.rate",
                                      pal="Set1", sz=3)
{
    clustid <- cluster.obj$cluster
    pred   <- cluster.predict(clustid, data, modellist)
    actual <- data[[outvar]]
    pdata <- data.frame(observed=actual, predicted=pred, cluster.id=as.factor(clustid))
    ggplot(data=pdata, aes(x=observed, y=predicted, color=cluster.id)) +
        geom_point(size=sz) + ggtitle("PCC growth rate: model vs. actual") +
            scale_color_brewer(type="qual", palette=pal) +
                stat_function(fun="identity", color="black", linetype=2, size=1)
}


