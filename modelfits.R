require(ggplot2)
require(rpart)
require(randomForest)
require(earth)

if(!exists("master.table")) master.table <- dget(file="cement-table.dat")

## Since the
## data are highly serially correlated, take only a single datum per
## five years from each country.  
master.trailing.5yr <- master.table[master.table$year %% 5 == 1 &
                                    !is.na(master.table$GDP.rate),]
master.leading.5yr  <- master.table[master.table$year %% 5 == 1 &
                                    !is.na(master.table$GDP.ld.rate),]


## Load the validation set.
if(!exists("testing.countries")) {
    testing.countries <- dget(file="testing-countries.dat")
} 

datasets <- split(master.trailing.5yr, master.trailing.5yr$ISO %in% testing.countries)
names(datasets) <- c("training", "testing")

### fit various per-capita cement models
## formulae to use in the fits:
##  simple  :  log(pcc.rate) = a + b*(GDP.rate)
##  basic   :  just the state variables
##  pcstock :  state variables + per-capita stock
##  d1, d2, etc refer to the degree of crossing
## 
##  The fitted models are named after the type of the formula used.
##  We omit the degree unless we fit a couple of models of the same
##  type and different degree.
f.gcam.d1    <- as.formula(pcc.rate~GDP.rate+0)
f.simple.d1  <- as.formula(pcc.rate~GDP.rate)
f.basic.d1   <- as.formula(pcc.rate~urban.growth + urban.pop + pcGDP + GDP.rate)
f.basic.d2   <- as.formula(pcc.rate~(urban.growth + urban.pop + pcGDP + GDP.rate)^2)
f.pcstock.d1 <- as.formula(pcc.rate~urban.growth + urban.pop + pcGDP + GDP.rate + pccement.stock)
f.pcstock.d2 <- as.formula(pcc.rate~(urban.growth + urban.pop + pcGDP + GDP.rate + pccement.stock)^2)
f.prate.d1   <- as.formula(pcc.rate~urban.growth + urban.pop + pcGDP + GDP.rate + pop.rate) 

### Linear regression
## simple.lm is the baseline model.  Beat that and you're "useful"
gcam.lm     <- lm(formula=f.gcam.d1,    data=datasets$training) # the model actually (sort of) used in gcam
simple.lm   <- lm(formula=f.simple.d1,  data=datasets$training)
basic.lm    <- lm(formula=f.basic.d1,   data=datasets$training)
basic.lm.d2 <- lm(formula=f.basic.d2,   data=datasets$training)
pcstock.lm  <- lm(formula=f.pcstock.d2, data=datasets$training)
prate.lm    <- lm(formula=f.prate.d1,   data=datasets$training)


## stepwise lr (no point in using for simple, which has only a single predictor)
## basic.lm.step   <- step(basic.lm.d2)
## pcstock.lm.step <- step(pcstock.lm)

## generalized lm (lots more to do here)  # the specifications below won't work with pcc.rate
## pcstock.glm <- glm(formula=f.pcstock.d2, family=Gamma(link=log), data=datasets$training) 
## pcstock.glm.gauss <- glm(formula=f.pcstock.d2, family=gaussian(link=log), data=datasets$training) 

## Add some more:  regression trees, etc.
## regression tree - note regression trees can't do interaction terms, so we have to use the d1 version
simple.rpart    <- rpart(formula=f.simple.d1,  data=datasets$training) 
basic.rpart     <- rpart(formula=f.basic.d1,   data=datasets$training)
pcstock.rpart   <- rpart(formula=f.pcstock.d1, data=datasets$training)
prate.rpart     <- rpart(formula=f.prate.d1,   data=datasets$training)

## random forest
simple.rf     <- randomForest(formula=f.simple.d1,  data=datasets$training)
pcstock.rf.d1 <- randomForest(formula=f.pcstock.d1, data=datasets$training)
pcstock.rf.d2 <- randomForest(formula=f.pcstock.d2, data=datasets$training)
basic.rf.d1   <- randomForest(formula=f.basic.d1,   data=datasets$training)
basic.rf.d2   <- randomForest(formula=f.basic.d2,   data=datasets$training)

## Multivariate adaptive regression splines 
## Note: the earth function always wants a degree=1 formula.  You
##       specify the crossing degree explicitly with an argument to
##       the fitting function. 
## TODO There is a lot more we can do here, especially using the glm option
simple.earth.d1     <- earth(formula=f.simple.d1, data=datasets$training, degree=1)
simple.earth.d2     <- earth(formula=f.simple.d1, data=datasets$training, degree=2)
simple.earth.d3     <- earth(formula=f.simple.d1, data=datasets$training, degree=3)

pcstock.earth.d1  <- earth(formula=f.pcstock.d1, data=datasets$training)
pcstock.earth.d2  <- earth(formula=f.pcstock.d1, data=datasets$training, degree=2, nk=64)
pcstock.earth.d2a <- earth(formula=f.pcstock.d1, data=datasets$training, degree=2, nk=128)
pcstock.earth.d2b <- earth(formula=f.pcstock.d1, data=datasets$training, degree=2, nk=128, nprune=15)
pcstock.earth.d3  <- earth(formula=f.pcstock.d1, data=datasets$training, degree=3, nk=128)
pcstock.earth.d3a <- earth(formula=f.pcstock.d1, data=datasets$training, degree=3, nk=128, nprune=20)

basic.earth.d1    <- earth(formula=f.basic.d1, data=datasets$training, degree=1)
basic.earth.d2    <- earth(formula=f.basic.d1, data=datasets$training, degree=2, nk=64)
basic.earth.d3    <- earth(formula=f.basic.d1, data=datasets$training, degree=3, nk=128)

prate.earth.d1    <- earth(formula=f.prate.d1, data=datasets$training, degree=1)

source("modelanly.R")

cat("\n****************Linear models****************\n")
cat("\ngcam:\t\tgcam.lm\n")
print(rms.eval(gcam.lm, datasets, FALSE))
cat("\nsimple:\t\tsimple.lm\n")
print(rms.eval(simple.lm, datasets, FALSE))
cat("\nbasic (degree=1):\t\tbasic.lm\n")
print(rms.eval(basic.lm, datasets, FALSE))
cat("\nbasic (degree=2):\t\tbasic.lm.d2\n")
print(rms.eval(basic.lm.d2, datasets, FALSE))
cat("\nprate (degree=1):\t\tprate.lm\n")
print(rms.eval(prate.lm, datasets, FALSE))


## cat("\n****************Stepwise linear models****************\n")
## cat("\nbasic:\t\tbasic.step\n")
## print(rms.eval(basic.lm.step, datasets, FALSE))

cat("\n****************Partition trees****************\n")
cat("\nGDP rate (not sure if this even makes sense):\t\tsimple.rpart\n")
print(rms.eval(simple.rpart, datasets, FALSE))
cat("\nbasic:\t\tbasic.rpart\n")
print(rms.eval(basic.rpart, datasets, FALSE))

cat("\n****************MARS models****************\n")
cat("\nGDP rate (degree 1):\t\tsimple.earth.d1\n")
print(rms.eval(simple.earth.d1, datasets, FALSE))
cat("\nGDP rate (degree 2):\t\tsimple.earth.d2\n")
print(rms.eval(simple.earth.d2, datasets, FALSE))
cat("\nGDP rate (degree 3):\t\tsimple.earth.d3\n")
print(rms.eval(simple.earth.d3, datasets, FALSE))
cat("\nbasic (degree 1):\t\tbasic.earth.d1\n")
print(rms.eval(basic.earth.d1, datasets, FALSE))
cat("\nbasic (degree 2):\t\tbasic.earth.d2\n")
print(rms.eval(basic.earth.d2, datasets, FALSE))
cat("\nprate (degree 1):\t\tprate.earth.d1\n")
print(rms.eval(prate.earth.d1, datasets, FALSE))
