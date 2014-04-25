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
pcc.earth.d2 <- earth(formula=pccement~urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock,
                      data=datasets$training, degree=2, nk=64)
pcc.earth.d2a <- earth(formula=pccement~urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock,
                      data=datasets$training, degree=2, nk=128)
pcc.earth.d2b <- earth(formula=pccement~urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock,
                       data=datasets$training, degree=2, nk=128, nprune=15)
pcc.earth.d3 <- earth(formula=pccement~urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock,
                      data=datasets$training, degree=3, nk=128)
pcc.earth.d3a <- earth(formula=pccement~urban.growth+urban.pop+pcGDP+GDP.rate+pccement.stock,
                      data=datasets$training, degree=3, nk=128, nprune=20)
pcc.earth.nostock <- earth(formula=pccement~urban.growth+urban.pop+pcGDP+GDP.rate,
                           data=datasets$training, degree=3, nk=128, nprune=15)


source("modelanly.R")

cat("\nlinear:\t\tpcc.lm\n")
print(rms.eval(pcc.lm, datasets, FALSE))
cat("\nstepwise linear:\t\tpcc.lm.step\n")
print(rms.eval(pcc.lm.step, datasets, FALSE))
cat("\nglm (Gamma(link=log)):\t\tpcc.glm\n")
print(rms.eval(pcc.glm, datasets, FALSE))
## cat("\nstepwise glm:\n") ### didn't solve properly
## print(rms.eval(pcc.glm.step, datasets, FALSE))
cat("\nglm (gaussian(link=log)):\t\tpcc.glm.gauss\n")
print(rms.eval(pcc.glm.gauss, datasets, FALSE))
cat("\nregression tree:\t\tpcc.rpart\n")
print(rms.eval(pcc.rpart, datasets, FALSE))
cat("\nrandom forest (no interactions):\t\tpcc.rf\n")
print(rms.eval(pcc.rf, datasets, FALSE))
cat("\nrandom forest (including interaction terms):\t\tprr.rf.int\n")
print(rms.eval(pcc.rf.int, datasets, FALSE))
cat("\nMARS degree=1 (i.e., no interactions):\t\tpcc.earth\n")
print(rms.eval(pcc.earth, datasets, FALSE))
cat("\nMARS degree=2 (first-order interactions):\t\tpcc.earth.d2\n")
print(rms.eval(pcc.earth.d2, datasets, FALSE))
cat("\nMARS degree=2, nk=128:\t\tpcc.earth.d2a\n")
print(rms.eval(pcc.earth.d2a, datasets, FALSE))
cat("\nMARS degree=2, pruned to 15 terms:\t\tpcc.earth.d2b\n")
print(rms.eval(pcc.earth.d2b, datasets, FALSE))
cat("\nMARS degree=3 (second-order interactions):\t\tpcc.earth.d3\n")
print(rms.eval(pcc.earth.d3, datasets, FALSE))
cat("\nMARS degree=3, pruned to 20 terms:\t\tpcc.earth.d3a\n")
print(rms.eval(pcc.earth.d3a, datasets, FALSE))
