require(cluster)

## Do some clustering analysis on the data

if(!exists("master.table")) master.table <- dget(file="cement-table.dat")
if(!exists("testing.countries")) testing.countries <- dget(file="testing-countries.dat")

### create a version that has the zero entries filtered out.  This is
### slightly different than the one created in modelfits.R, so we
### choose a different name. 
### TODO gather all this junk up into functions so we don't have such
### a polluted namespace.
master.nz <- master.table[master.table$cement>0,]

### make some split datasets, in case we want to do cross-validation
datasets.full <- split(master.table, master.table$ISO %in% testing.countries)
names(datasets.full) <- c("training", "testing") 
datasets.nz <- split(master.nz, master.nz$ISO %in% testing.countries)
names(datasets.nz) <- c("training", "testing")

### list of "basic" (non-autoregressive) predictors
predictors.basic <-  c("urban.growth", "urban.pop", "pcGDP", "GDP.rate")
### add the thing we're trying to predict
cluster.basic <- c("pccement",predictors.basic)


### Start with k-means cluster.  Hypothesis: the cluster.basic set has
### 5 kinds of countries: rich, middle income, poor, non-producing,
### and basket-case (i.e., failed states and other countries with
### highly erratic indicators)

mt.complete <- master.table[complete.cases(master.table[,cluster.basic]),cluster.basic]
if(!exists("km.full.cb.5")) {             # don't compute it, if it's already been done
    km.full.cb.5 <- kmeans(mt.complete, centers=5)
}
## got one cluster that seemed like kind of a hodgepodge, so try 6 centers
if(!exists("km.full.cb.6")) {
    km.full.cb.6 <- kmeans(mt.complete, centers=6)
}

## cluster on just the countries with nonzero production.  In keeping
## with our hypotheses above, this should require one fewer cluster
## than the ones above.
mnz.complete <- master.nz[complete.cases(master.nz[,cluster.basic]), cluster.basic]
if(!exists("km.nonzero.cb.4")) {
    km.nz.cb.4 <- kmeans(mnz.complete, centers=4)
}
if(!exists("km.nonzero.cb.5")) {
    km.nz.cb.5 <- kmeans(mnz.complete, centers=5)
}



## use the daisy function to compute a distance matrix, mainly because
## it can automatically standardize the variables, which presently
## have very different magnitudes.  Using the manhattan metric on a
## hunch. 
dist.full.cb <- daisy(master.table[,cluster.basic], metric="manhattan", stand=TRUE)

dist.nz.cb <- daisy(master.nz[,cluster.basic],
                         metric="manhattan", stand=TRUE)
