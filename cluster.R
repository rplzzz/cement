require(cluster)

## k-means uses a RNG.  
set.seed(8675309)

####
#### A bunch of utilities functions for working with clusters
####

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

complete.cb <- master.table[complete.cases(master.table[,cluster.basic]),]
master.complete <- clust.normalize(complete.cb[,cluster.basic])
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
complete.nz <- master.nz[complete.cases(master.nz[,cluster.basic]),]
master.nz.complete <- clust.normalize(complete.nz[,cluster.basic])
if(!exists("km.nonzero.cb.4")) {
    km.nz.cb.4 <- kmeans(master.nz.complete, centers=4)
}
if(!exists("km.nonzero.cb.5")) {
    km.nz.cb.5 <- kmeans(master.nz.complete, centers=5)
}


## Re-fit the clustering using the trailing indicators
trailing.basic <- c("GDP.rate", "pcc.rate", "pop.rate", "urban.growth", "urban.pop", "pcGDP")
complete.trailing <- master.table[complete.cases(master.table[,trailing.basic]),]
tc <- clust.normalize(complete.trailing[,trailing.basic])
km.trailing.4 <- kmeans(tc, centers=4)
km.trailing.5 <- kmeans(tc, centers=5)
km.trailing.6 <- kmeans(tc, centers=6)

## Compute similarity matrices for the trailing indicator 4, 5, and 6 cluster versions
trl4.members <- clust.members(complete.trailing, km.trailing.4$cluster)
trl5.members <- clust.members(complete.trailing, km.trailing.5$cluster)
trl6.members <- clust.members(complete.trailing, km.trailing.6$cluster)

trl.sim.44 <- mlist.sim.matrix(trl4.members, trl4.members)
trl.sim.45 <- mlist.sim.matrix(trl4.members, trl5.members)
trl.sim.46 <- mlist.sim.matrix(trl4.members, trl6.members)
trl.sim.55 <- mlist.sim.matrix(trl5.members, trl5.members)
trl.sim.56 <- mlist.sim.matrix(trl5.members, trl6.members)
trl.sim.66 <- mlist.sim.matrix(trl6.members, trl6.members)

## country lists for the trailing indicator
countries.trl.4 <- clust.countries(complete.trailing, km.trailing.4$cluster)
countries.trl.5 <- clust.countries(complete.trailing, km.trailing.5$cluster)
countries.trl.6 <- clust.countries(complete.trailing, km.trailing.6$cluster)

#### This is here in case we decide to try using some other cluster algorithms

## ## use the daisy function to compute a distance matrix, mainly because
## ## it can automatically standardize the variables, which presently
## ## have very different magnitudes.  Using the manhattan metric on a
## ## hunch. 
## dist.full.cb <- daisy(master.table[,cluster.basic], metric="manhattan", stand=TRUE)

## dist.nz.cb <- daisy(master.nz[,cluster.basic],
##                          metric="manhattan", stand=TRUE)
