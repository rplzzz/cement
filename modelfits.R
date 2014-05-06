require(ggplot2)
require(rpart)
require(randomForest)
require(earth)

if(!exists("master.table")) master.table <- dget(file="cement-table.dat")

## filter out all of the data with zero production.  Also, since the
## data are highly serially correlated, take only a single datum per
## decade from each country.  Finally, the data from the Bahamas looks
## weird (among other things, it's only present for a few years), so
## exclude it.
master.nonzero <- master.table[master.table$cement > 0 & master.table$year %% 10 == 9
                               & master.table$ISO != "BHS",]

## having data at 5-year resolution will be useful for evaluating the
## fits.  Leave the cement=0 lines in.
master.5yr <- master.table[master.table$year %% 5 == 4 & master.table$ISO != "BHS",]

## Load the validation set.
if(!exists("testing.countries")) {
    testing.countries <- dget(file="testing-countries.dat")
} 

datasets <- split(master.nonzero, master.nonzero$ISO %in% testing.countries)
names(datasets) <- c("training", "testing")

### fit various per-capita cement models
## formulae to use in the fits:
##  basic   :  just the state variables
##  pcstock :  state variables + per-capita stock
##  lag5    :  state variables + 5-year lagged per-capita production
##  d1, d2, etc refer to the degree of crossing
## 
##  The fitted models are named after the type of the formula used.
##  We omit the degree unless we fit a couple of models of the same
##  type and different degree.
f.basic.d1 <- as.formula(pccement~urban.growth + urban.pop + pcGDP + GDP.rate)
f.basic.d2 <- as.formula(pccement~(urban.growth + urban.pop + pcGDP + GDP.rate)^2)
f.pcstock.d1 <- as.formula(pccement~urban.growth + urban.pop + pcGDP + GDP.rate + pccement.stock)
f.pcstock.d2 <- as.formula(pccement~(urban.growth + urban.pop + pcGDP + GDP.rate + pccement.stock)^2)
f.lag5.d1 <- as.formula(pccement~urban.growth + urban.pop + pcGDP + GDP.rate + pccement.lag5)
f.lag5.d2 <- as.formula(pccement~(urban.growth + urban.pop + pcGDP + GDP.rate + pccement.lag5)^2)

## Linear regression
pcstock.lm <- lm(formula=f.pcstock.d2, data=datasets$training)
lag5.lm    <- lm(formula=f.lag5.d2, data=datasets$training)
basic.lm   <- lm(formula=f.basic.d1, data=datasets$training)

## stepwise lr
pcstock.lm.step <- step(pcstock.lm)
lag5.lm.step    <- step(lag5.lm)

## generalized lm (lots more to do here)
pcstock.glm <- glm(formula=f.pcstock.d2, family=Gamma(link=log), data=datasets$training) 
pcstock.glm.gauss <- glm(formula=f.pcstock.d2, family=gaussian(link=log), data=datasets$training) 

## Add some more:  regression trees, etc.
## regression tree - note regression trees can't do interaction terms, so we have to use the d1 version
pcstock.rpart <- rpart(formula=f.pcstock.d1, data=datasets$training)
basic.rpart   <- rpart(formula=f.basic.d1, data=datasets$training)
lag5.rpart    <- rpart(formula=f.lag5.d1, data=datasets$training)

## random forest
pcstock.rf.d1 <- randomForest(formula=f.pcstock.d1, data=datasets$training)
pcstock.rf.d2 <- randomForest(formula=f.pcstock.d2, data=datasets$training)
basic.rf.d1   <- randomForest(formula=f.basic.d1,   data=datasets$training)
basic.rf.d2   <- randomForest(formula=f.basic.d2,   data=datasets$training)

## Multi-adaptive regression splines 
## Note: the earth function always wants a degree=1 formula.  You
##       specify the crossing degree explicitly with an argument to
##       the fitting function. 
## TODO There is a lot more we can do here.  Notably, we could use the glm capability to try to fit log(pccement)
pcstock.earth.d1  <- earth(formula=f.pcstock.d1, data=datasets$training)
pcstock.earth.d2  <- earth(formula=f.pcstock.d1, data=datasets$training, degree=2, nk=64)
pcstock.earth.d2a <- earth(formula=f.pcstock.d1, data=datasets$training, degree=2, nk=128)
pcstock.earth.d2b <- earth(formula=f.pcstock.d1, data=datasets$training, degree=2, nk=128, nprune=15)
pcstock.earth.d3  <- earth(formula=f.pcstock.d1, data=datasets$training, degree=3, nk=128)
pcstock.earth.d3a <- earth(formula=f.pcstock.d1, data=datasets$training, degree=3, nk=128, nprune=20)

basic.earth.d1    <- earth(formula=f.basic.d1, data=datasets$training, degree=1)
basic.earth.d3    <- earth(formula=f.basic.d1, data=datasets$training, degree=3, nk=128, nprune=15)

lag5.earth.d1     <- earth(formula=f.lag5.d1, data=datasets$training, degree=1)
lag5.earth.d2     <- earth(formula=f.lag5.d1, data=datasets$training, degree=2, nk=128)
lag5.earth.d2a    <- earth(formula=f.lag5.d1, data=datasets$training, degree=2, nk=128, nprune=15)
lag5.earth.d3     <- earth(formula=f.lag5.d1, data=datasets$training, degree=3, nk=128)
lag5.earth.d3a    <- earth(formula=f.lag5.d1, data=datasets$training, degree=3, nk=128, nprune=20)

source("modelanly.R")

cat("\nModels using 'carbon stock'\n")
cat("\nlinear:\t\tpcstock.lm\n")
print(rms.eval(pcstock.lm, datasets, FALSE))
cat("\nstepwise linear:\t\tpcstock.lm.step\n")
print(rms.eval(pcstock.lm.step, datasets, FALSE))
cat("\nglm (Gamma(link=log)):\t\tpcstock.glm\n")
print(rms.eval(pcstock.glm, datasets, FALSE))
cat("\nglm (gaussian(link=log)):\t\tpcstock.glm.gauss\n")
print(rms.eval(pcstock.glm.gauss, datasets, FALSE))
cat("\nregression tree:\t\tpcstock.rpart\n")
print(rms.eval(pcstock.rpart, datasets, FALSE))
cat("\nrandom forest (no interactions):\t\tpcstock.rf.d1\n")
print(rms.eval(pcstock.rf.d1, datasets, FALSE))
cat("\nrandom forest (including interaction terms):\t\tpcstock.rf.d2\n")
print(rms.eval(pcstock.rf.d2, datasets, FALSE))
cat("\nMARS degree=1 (i.e., no interactions):\t\tpcstock.earth.d1\n")
print(rms.eval(pcstock.earth.d1, datasets, FALSE))
cat("\nMARS degree=2, pruned to 15 terms:\t\tpcstock.earth.d2b\n")
print(rms.eval(pcstock.earth.d2b, datasets, FALSE))
cat("\nMARS degree=3, pruned to 20 terms:\t\tpcstock.earth.d3a\n")
print(rms.eval(pcstock.earth.d3a, datasets, FALSE))

cat("\nModels using lag5\n")
cat("\nstepwise linear lag5:\t\tlag5.lm.step\n")
print(rms.eval(lag5.lm.step, datasets, FALSE))
cat("\nregression tree lag5:\t\tlag5.rpart\n")
print(rms.eval(lag5.rpart, datasets, FALSE))
cat("\nMARS degree=1, lag5:\t\tlag5.earth.d1\n")
print(rms.eval(lag5.earth.d1, datasets, FALSE)) 
cat("\nMARS degree=2, lag5:\t\tlag5.earth.d2\n")
print(rms.eval(lag5.earth.d2, datasets, FALSE))
cat("\nMARS degree=2, lag5 (pruned to 15):\t\tlag5.earth.d2a\n")
print(rms.eval(lag5.earth.d2a, datasets, FALSE))
cat("\nMARS degree=3, lag5:\t\tlag5.earth.d3\n")
print(rms.eval(lag5.earth.d3, datasets, FALSE))
cat("\nMARS degree=3, lag5 (pruned to 20):\t\tlag5.earth.d3a\n")
print(rms.eval(lag5.earth.d3a, datasets, FALSE))

cat("\nModels uging only the basic predictors\n")
cat("\nMARS degree=3:\t\tbasic.earth.d3\n")
print(rms.eval(basic.earth.d3, datasets, FALSE))
cat("\nMARS degree=1:\t\tbasic.earth.d1\n")
cat("\nrpart:\tbasic.rpart\n")
print(rms.eval(basic.rpart, datasets, FALSE))
cat("\nrandom forest degree=1:\t\tbasic.rf.d1\n")
print(rms.eval(basic.rf.d1, datasets, FALSE))
cat("\nrandom forest degree=2:\t\tbasic.rf.d2\n")
print(rms.eval(basic.rf.d2, datasets, FALSE))
