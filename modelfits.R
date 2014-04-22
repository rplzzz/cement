require(ggplot2)
require(rpart)
require(randomForest)
require(earth)

if(!exists("master.table")) master.table <- dget(file="cement-table.dat")

## filter out all of the data with zero production.  Also, since the
## data are highly serially correlated, take only a single datum per
## decade from each country.
master.nonzero <- master.table[master.table$cement > 0 & master.table$year %% 10 == 9,]

## select a sample of countries to hold back for a validation set (the "testing set")
if(!exists("testing.countries")) {
    if(file.exists("testing-countries.dat"))
        testing.countries <- dget(file="testing-countries.dat")
    else {
        testing.countries <- with(list(countries=levels(as.factor(master.nonzero$ISO))),
                                  sample(countries, length(countries)/5))
        comment(testing.countries) <- date()
        dput(testing.countries, file="testing-countries.dat") 
    }
    print("Testing countries are:")
    print(testing.countries)
} 

datasets <- split(master.nonzero, master.nonzero$ISO %in% testing.countries)
names(datasets) <- c("training", "testing")

### fit various per-capita cement models
predictors <- c("urban.growth", "urban.pop", "pcGDP", "GDP.rate", "pccement.stock")
## Linear regression
pcc.lm <- lm(pccement~(urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock)^2, data=datasets$training)

## stepwise lr
pcc.lm.step <- step(pcc.lm)

## generalized lm (lots more to do here)
pcc.glm <- glm(formula=pccement~(urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock)^2,
               family=Gamma(link=log), data=datasets$training)

pcc.glm.gauss <- glm(formula=pccement~(urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock)^2,
                     family=gaussian(link=log), data=datasets$training)


## ## stepwise generalized lm
## pcc.glm.step <- step(pcc.glm)

## Add some more:  regression trees, etc.
## regression tree - note regression trees can't do interaction terms
pcc.rpart <- rpart(formula= pccement~urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock, data=datasets$training)

## random forest
pcc.rf     <- randomForest(formula=pccement~urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock, data=datasets$training)
pcc.rf.int <- randomForest(formula=pccement~(urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock)^2, data=datasets$training)

## Multi-adaptive regression splines
pcc.earth <- earth(formula=pccement~urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock, data=datasets$training)
##pcc.earth.int <- earth(formula=pccement~pccement~(urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock)^2,
##                       data=datasets$training)


### Evaluate the models.  Here's the evaluator (example:  rms.eval(pcc.lm, datasets, "pccement") )
rms.eval <- function(model, datasets, prn=TRUE) {
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

## model.scatterplot <- function(model, dat) {
##     if("glm" %in% class(model)) {
##         train.pred   <- c(predict(object=model, newdata=dat$training, type="response")) # c() strips out unwanted attributes
##         test.pred    <- c(predict(object=model, newdata=dat$testing, type="response"))
##     }
##     else {
##         train.pred   <- c(predict(object=model, newdata=dat$training)) # c() strips out unwanted attributes
##         test.pred    <- c(predict(object=model, newdata=dat$testing))
##     }        
##     train.actual <- dat$training$pccement
##     test.actual  <- dat$testing$pccement

##     pd.training <- data.frame(actual=train.actual, model=train.pred, dataset="training")
##     pd.testing  <- data.frame(actual=test.actual, model=test.pred, dataset="testing")
##     pd <- rbind(pd.training, pd.testing)
##     qplot(x=actual, y=model, data=pd, geom="point", color=dataset) + ggtitle("Per Capita Cement: model vs. actual") +
##         stat_function(fun="identity")
## }

scatterplot.model <- function(model, data, pal="Set1") {
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
    qplot(x=actual, y=model, data=pltdata, geom="point", color=dataset) +
        ggtitle("Per Capita Cement: model vs. actual") +
            scale_color_brewer(type="qual", palette=pal) +
                stat_function(fun="identity", color="black") 
}


cat("\nlinear:\n")
print(rms.eval(pcc.lm, datasets, FALSE))
cat("\nstepwise linear:\n")
print(rms.eval(pcc.lm.step, datasets, FALSE))
cat("\nglm (Gamma):\n")
print(rms.eval(pcc.glm, datasets, FALSE))
## cat("\nstepwise glm:\n") ### didn't solve properly
## print(rms.eval(pcc.glm.step, datasets, FALSE))
cat("\nglm (Gaussian):\n")
print(rms.eval(pcc.glm.gauss, datasets, FALSE))
cat("\nregression tree:\n")
print(rms.eval(pcc.rpart, datasets, FALSE))
cat("\nrandom forest (no interactions):\n")
print(rms.eval(pcc.rf, datasets, FALSE))
cat("\nrandom forest (including interaction terms):\n")
print(rms.eval(pcc.rf.int, datasets, FALSE))
cat("\nMARS (no interactions):\n")
print(rms.eval(pcc.earth, datasets, FALSE))
## cat("\nMARS (including interaction terms):\n")
## print(rms.eval(pcc.earth.int, datasets, FALSE))


