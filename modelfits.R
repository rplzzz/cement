require(ggplot2)
require(rpart)
require(randomForest)
require(earth)

if(!exists("master.table")) master.table <- dget(file="cement-table.dat")

## Since the
## data are highly serially correlated, take only a single datum per
## five years from each country.  
master.5yr <- master.table[master.table$year %% 5 == 1,]


## Load the validation set.
if(!exists("testing.countries")) {
    testing.countries <- dget(file="testing-countries.dat")
} 

datasets <- split(master.5yr, master.5yr$ISO %in% testing.countries)
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
f.basic.d1   <- as.formula(pcc.rate~urban.growth + urban.pop + pcGDP + GDP.rate)
f.basic.d2   <- as.formula(pcc.rate~(urban.growth + urban.pop + pcGDP + GDP.rate)^2)
f.pcstock.d1 <- as.formula(pcc.rate~urban.growth + urban.pop + pcGDP + GDP.rate + pccement.stock)
f.pcstock.d2 <- as.formula(pcc.rate~(urban.growth + urban.pop + pcGDP + GDP.rate + pccement.stock)^2)
f.gdpr.d1    <- as.formula(pcc.rate~GDP.rate)

## Linear regression
basic.lm   <- lm(formula=f.basic.d1, data=datasets$training)
basic.lm.d2<- lm(formula=f.basic.d2, data=datasets$training)
pcstock.lm <- lm(formula=f.pcstock.d2, data=datasets$training)
gdpr.lm.d1 <- lm(formula=f.gdpr.d1, data=datasets$training)

## stepwise lr
basic.lm.step   <- step(basic.lm.d2)
pcstock.lm.step <- step(pcstock.lm)
gdpr.lm.step       <- step(gdpr.lm.d2)

## generalized lm (lots more to do here)  # the specifications below won't work with pcc.rate
## pcstock.glm <- glm(formula=f.pcstock.d2, family=Gamma(link=log), data=datasets$training) 
## pcstock.glm.gauss <- glm(formula=f.pcstock.d2, family=gaussian(link=log), data=datasets$training) 

## Add some more:  regression trees, etc.
## regression tree - note regression trees can't do interaction terms, so we have to use the d1 version
basic.rpart   <- rpart(formula=f.basic.d1, data=datasets$training)
pcstock.rpart <- rpart(formula=f.pcstock.d1, data=datasets$training)
gdpr.rpart    <- rpart(formula=f.gdpr.d1, data=datasets$training) # does this even make sense?

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
basic.earth.d2    <- earth(formula=f.basic.d1, data=datasets$training, degree=2, nk=64)
basic.earth.d3    <- earth(formula=f.basic.d1, data=datasets$training, degree=3, nk=128)

gdpr.earth.d1     <- earth(formula=f.gdpr.d1, data=datasets$training, degree=1)
gdpr.earth.d2     <- earth(formula=f.gdpr.d1, data=datasets$training, degree=2)
gdpr.earth.d3     <- earth(formula=f.gdpr.d1, data=datasets$training, degree=3)

source("modelanly.R")

cat("\nLinear models\n")
cat("\nGDP rate (degree=1):\t\tgdpr.lm.d1\n")
print(rms.eval(gdpr.lm.d1, datasets, FALSE))
cat("\nGDP rate (degree=2):\t\tgdpr.lm.d2\n")
print(rms.eval(gdpr.lm.d2, datasets, FALSE))
cat("\nbasic (degree=1):\t\tbasic.lm\n")
print(rms.eval(basic.lm, datasets, FALSE))
cat("\nbasic (degree=2):\t\tbasic.lm.d2\n")
print(rms.eval(basic.lm.d2, datasets, FALSE))


cat("\nStepwise linear models\n")
cat("\nGDP rate (degree=2):\t\tgdpr.step\n")
print(rms.eval(gdpr.lm.step, datasets, FALSE))
cat("\nbasic:\t\tbasic.step\n")
print(rms.eval(basic.lm.step, datasets, FALSE))

cat("\nPartition trees\n")
cat("\nGDP rate (not sure if this even makes sense):\t\tgdpr.rpart\n")
print(rms.eval(gdpr.rpart, datasets, FALSE))
cat("\nbasic:\t\tbasic.rpart\n")
print(rms.eval(basic.rpart, datasets, FALSE))

cat("\nMARS models\n")
cat("\nGDP rate (degree 1):\t\tgdpr.earth.d1\n")
print(rms.eval(gdpr.earth.d1, datasets, FALSE))
cat("\nGDP rate (degree 2):\t\tgdpr.earth.d2\n")
print(rms.eval(gdpr.earth.d2, datasets, FALSE))
cat("\nGDP rate (degree 3):\t\tgdpr.earth.d3\n")
print(rms.eval(gdpr.earth.d3, datasets, FALSE))
cat("\nbasic (degree 1):\t\tbasic.earth.d1\n")
print(rms.eval(basic.earth.d1, datasets, FALSE))
cat("\nbasic (degree 2):\t\tbasic.earth.d2\n")
print(rms.eval(basic.earth.d2, datasets, FALSE))
cat("\nbasic (degree 3):\t\tbasic.earth.d3\n")
print(rms.eval(basic.earth.d3, datasets, FALSE))
