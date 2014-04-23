### Evaluate the models.  Here's the evaluator (example:  rms.eval(pcc.lm, datasets, "pccement") )
rms.eval <- function(model, datasets, prn=TRUE) {
    ## Unlike the plotting function, this function wants a list with two datasets: training and testing (FIXME)
    if("glm" %in% class(model)) {
        train.pred   <- predict(object=model, newdata=datasets$training, type="response")
        test.pred    <- predict(object=model, newdata=datasets$testing, type="response")
    }
    else {
        train.pred   <- predict(object=model, newdata=datasets$training)
        test.pred    <- predict(object=model, newdata=datasets$testing)
    }        
    train.actual <- datasets$training$pccement
    test.actual  <- datasets$testing$pccement

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


scatterplot.model <- function(model, data, pal="Set1", sz=3) {
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
                          act <- dat$pccement
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

mk.country.data <- function(country.list, alldata) {
    dtmp <- lapply(country.list,
                   function(country) {
                       alldata[alldata$ISO==country,]
                   })
    names(dtmp) <- country.list
    dtmp
}
