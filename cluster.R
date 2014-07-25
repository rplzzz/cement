require(cluster)


clust.normalize <- function(data) {
    for(name in names(data)) {
        data[[name]] <- data[[name]] / max(abs(data[[name]]))
    }
    data
}

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

master.complete <- clust.normalize(master.table[complete.cases(master.table[,cluster.basic]),cluster.basic])
if(!exists("km.full.cb.5")) {             # don't compute it, if it's already been done
    km.full.cb.5 <- kmeans(master.complete, centers=5)
}
## got one cluster that seemed like kind of a hodgepodge, so try 6 centers
if(!exists("km.full.cb.6")) {
    km.full.cb.6 <- kmeans(master.complete, centers=6)
}

## cluster on just the countries with nonzero production.  In keeping
## with our hypotheses above, this should require one fewer cluster
## than the ones above.
master.nz.complete <- clust.normalize(master.nz[complete.cases(master.nz[,cluster.basic]), cluster.basic])
if(!exists("km.nonzero.cb.4")) {
    km.nz.cb.4 <- kmeans(master.nz.complete, centers=4)
}
if(!exists("km.nonzero.cb.5")) {
    km.nz.cb.5 <- kmeans(master.nz.complete, centers=5)
}


## Re-fit the clustering using the trailing indicators
trailing.basic <- c("GDP.rate", "pcc.rate", "pop.rate", "urban.growth", "urban.pop", "pcGDP")
trailing.complete <- clust.normalize(master.table[complete.cases(master.table[,trailing.basic]),trailing.basic])
km.trailing.4 <- kmeans(trailing.complete, centers=4)
km.trailing.5 <- kmeans(trailing.complete, centers=5)
km.trailing.6 <- kmeans(trailing.complete, centers=6)

## use the daisy function to compute a distance matrix, mainly because
## it can automatically standardize the variables, which presently
## have very different magnitudes.  Using the manhattan metric on a
## hunch. 
dist.full.cb <- daisy(master.table[,cluster.basic], metric="manhattan", stand=TRUE)

dist.nz.cb <- daisy(master.nz[,cluster.basic],
                         metric="manhattan", stand=TRUE)
