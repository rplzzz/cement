require(ggplot2)
require(rpart)
require(randomForest)
require(earth)

if(!exists("master.table")) master.table <- dget(file="cement-table.dat")

## list of columns to keep for various types of tables
## for all tables you need
basevars <- c("ISO","year","pop.tot", "urban.growth", "urban.pop", "pcGDP",
              "pccement", "pccement.stock")
trailvars <- c("GDP.rate", "pcc.rate", "pop.rate")
leadvars  <- c("ld.GDP.rate", "ld.pcc.rate", "ld.pop.rate")

## Since the
## data are highly serially correlated, take only a single datum per
## five years from each country.  
mt.5yr <- master.table[master.table$year %% 5 == 1, c(basevars, trailvars)]
mt.5yr <- mt.5yr[complete.cases(mt.5yr),] # scrub out incomplete data

ml.5yr  <- master.table[master.table$year %% 5 == 1, c(basevars, leadvars)]
ml.5yr  <- ml.5yr[complete.cases(ml.5yr),]

## Load the validation set.
if(!exists("testing.countries")) {
    testing.countries <- dget(file="testing-countries.dat")
} 

datasets.trailing <- split(mt.5yr, mt.5yr$ISO %in% testing.countries)
names(datasets.trailing) <- c("training", "testing")

datasets.leading <- split(ml.5yr, ml.5yr$ISO %in% testing.countries)
names(datasets.leading) <- c("training", "testing")

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
f.gcam.d1    <- pcc.rate~GDP.rate+0
f.simple.d1  <- pcc.rate~GDP.rate
f.basic.d1   <- pcc.rate~urban.growth + urban.pop + pcGDP + GDP.rate
f.basic.d2   <- pcc.rate~(urban.growth + urban.pop + pcGDP + GDP.rate^2)
f.pcstock.d1 <- pcc.rate~urban.growth + urban.pop + pcGDP + GDP.rate + pccement.stock
f.pcstock.d2 <- pcc.rate~(urban.growth + urban.pop + pcGDP + GDP.rate + pccement.stock^2)
f.prate.d1   <- pcc.rate~urban.growth + urban.pop + pcGDP + GDP.rate + pop.rate

fl.basic.d1   <- ld.pcc.rate~urban.growth + urban.pop + pcGDP + ld.GDP.rate
fl.prate.d1   <- ld.pcc.rate~urban.growth + urban.pop + pcGDP + ld.GDP.rate + ld.pop.rate

### Linear regression
## simple.lm is the baseline model.  Beat that and you're "useful"
gcam.lm     <- lm(formula=f.gcam.d1,    data=datasets.trailing$training) # the model actually (sort of) used in gcam
simple.lm   <- lm(formula=f.simple.d1,  data=datasets.trailing$training)
basic.lm    <- lm(formula=f.basic.d1,   data=datasets.trailing$training)
basic.lm.d2 <- lm(formula=f.basic.d2,   data=datasets.trailing$training)
pcstock.lm  <- lm(formula=f.pcstock.d2, data=datasets.trailing$training)
prate.lm    <- lm(formula=f.prate.d1,   data=datasets.trailing$training)

lbasic.lm    <- lm(formula=fl.basic.d1,   data=datasets.leading$training)
lprate.lm    <- lm(formula=fl.prate.d1,   data=datasets.leading$training)

## stepwise lr (no point in using for simple, which has only a single predictor)
## basic.lm.step   <- step(basic.lm.d2)
## pcstock.lm.step <- step(pcstock.lm)

## generalized lm (lots more to do here)  # the specifications below won't work with pcc.rate
## pcstock.glm <- glm(formula=f.pcstock.d2, family=Gamma(link=log), data=datasets$training) 
## pcstock.glm.gauss <- glm(formula=f.pcstock.d2, family=gaussian(link=log), data=datasets$training) 

## Add some more:  regression trees, etc.
## regression tree - note regression trees can't do interaction terms, so we have to use the d1 version
simple.rpart    <- rpart(formula=f.simple.d1,  data=datasets.trailing$training) 
basic.rpart     <- rpart(formula=f.basic.d1,   data=datasets.trailing$training)
pcstock.rpart   <- rpart(formula=f.pcstock.d1, data=datasets.trailing$training)
prate.rpart     <- rpart(formula=f.prate.d1,   data=datasets.trailing$training)

lbasic.rpart     <- rpart(formula=fl.basic.d1,   data=datasets.leading$training)
lprate.rpart     <- rpart(formula=fl.prate.d1,   data=datasets.leading$training)

## random forest
simple.rf     <- randomForest(formula=f.simple.d1,  data=datasets.trailing$training)
pcstock.rf.d1 <- randomForest(formula=f.pcstock.d1, data=datasets.trailing$training)
pcstock.rf.d2 <- randomForest(formula=f.pcstock.d2, data=datasets.trailing$training)
basic.rf.d1   <- randomForest(formula=f.basic.d1,   data=datasets.trailing$training)
basic.rf.d2   <- randomForest(formula=f.basic.d2,   data=datasets.trailing$training)

## Multivariate adaptive regression splines 
## Note: the earth function always wants a degree=1 formula.  You
##       specify the crossing degree explicitly with an argument to
##       the fitting function. 
## TODO There is a lot more we can do here, especially using the glm option
simple.earth.d1     <- earth(formula=f.simple.d1, data=datasets.trailing$training, degree=1)

pcstock.earth.d1  <- earth(formula=f.pcstock.d1, data=datasets.trailing$training)
pcstock.earth.d2  <- earth(formula=f.pcstock.d1, data=datasets.trailing$training, degree=2, nk=64)
pcstock.earth.d2a <- earth(formula=f.pcstock.d1, data=datasets.trailing$training, degree=2, nk=128)
pcstock.earth.d2b <- earth(formula=f.pcstock.d1, data=datasets.trailing$training, degree=2, nk=128, nprune=15)
pcstock.earth.d3  <- earth(formula=f.pcstock.d1, data=datasets.trailing$training, degree=3, nk=128)
pcstock.earth.d3a <- earth(formula=f.pcstock.d1, data=datasets.trailing$training, degree=3, nk=128, nprune=20)

basic.earth.d1    <- earth(formula=f.basic.d1, data=datasets.trailing$training, degree=1)
basic.earth.d2    <- earth(formula=f.basic.d1, data=datasets.trailing$training, degree=2, nk=64)
basic.earth.d3    <- earth(formula=f.basic.d1, data=datasets.trailing$training, degree=3, nk=128)

prate.earth.d1    <- earth(formula=f.prate.d1, data=datasets.trailing$training, degree=1)

lbasic.earth.d1    <- earth(formula=fl.basic.d1, data=datasets.leading$training, degree=1)
lprate.earth.d1    <- earth(formula=fl.prate.d1, data=datasets.leading$training, degree=1)

source("modelanly.R")

cat("\n****************Linear models****************\n")
cat("\ngcam:\t\tgcam.lm\n")
print(rms.eval(gcam.lm, datasets.trailing, FALSE))
cat("\nsimple:\t\tsimple.lm\n")
print(rms.eval(simple.lm, datasets.trailing, FALSE))
cat("\nbasic (degree=1):\t\tbasic.lm\n")
print(rms.eval(basic.lm, datasets.trailing, FALSE))
cat("\nbasic (degree=2):\t\tbasic.lm.d2\n")
print(rms.eval(basic.lm.d2, datasets.trailing, FALSE))
cat("\nprate (degree=1):\t\tprate.lm\n")
print(rms.eval(prate.lm, datasets.trailing, FALSE))


## cat("\n****************Stepwise linear models****************\n")
## cat("\nbasic:\t\tbasic.step\n")
## print(rms.eval(basic.lm.step, datasets, FALSE))

cat("\n****************Partition trees****************\n")
cat("\nGDP rate:\t\tsimple.rpart\n")
print(rms.eval(simple.rpart, datasets.trailing, FALSE))
cat("\nbasic:\t\tbasic.rpart\n")
print(rms.eval(basic.rpart, datasets.trailing, FALSE))

cat("\n****************MARS models****************\n")
cat("\nGDP rate (degree 1):\t\tsimple.earth.d1\n")
print(rms.eval(simple.earth.d1, datasets.trailing, FALSE))
cat("\nbasic (degree 1):\t\tbasic.earth.d1\n")
print(rms.eval(basic.earth.d1, datasets.trailing, FALSE))
cat("\nbasic (degree 2):\t\tbasic.earth.d2\n")
print(rms.eval(basic.earth.d2, datasets.trailing, FALSE))
cat("\nprate (degree 1):\t\tprate.earth.d1\n")
print(rms.eval(prate.earth.d1, datasets.trailing, FALSE))

cat("\n****************models with leading rate data****************\n")
cat("\nbasic (degree=1):\t\tlbasic.lm\n")
print(rms.eval(lbasic.lm, datasets.leading, FALSE))
cat("\nprate (degree=1):\t\tlprate.lm\n")
print(rms.eval(lprate.lm, datasets.leading, FALSE))
cat("\nprate (degree 1):\t\tlprate.earth.d1\n")
print(rms.eval(lprate.earth.d1, datasets.leading, FALSE))
