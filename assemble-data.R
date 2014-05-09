require(reshape)


datefix <- function(dd) {as.numeric(substr(as.character(dd),2,5))}
select.complete <- function(dd) {dd[complete.cases(dd),]}
to.ISO <- function(dd) {
    dd$country <- tolower(dd$country)
    subset(merge(dd, ccodes, by="country", all.x=TRUE), select= -c(two.letter, numerical, country))
}
reorg.by.yr <- function(dd, name) {
    dd.m <- melt(dd, variable="xyear")
    names(dd.m)[names(dd.m)=="value"] <- name
    dd.m$year <- datefix(dd.m$xyear)
    dd.m$xyear <- NULL
    dd.m
}

## Country code lookup table.  Some tables need this, some don't
ccodes <- read.csv("raw/country-codes-with-synonyms.csv", comment.char='#')
ccodes$country <- tolower(ccodes$country)

## Urban population fraction
urban.pop <- read.csv("raw/urban-pop-pct.csv")
urban.pop <- to.ISO(urban.pop)
urban.pop.m <- reorg.by.yr(urban.pop, "urban.pop")

## GDP
gdp.mer <- read.csv("raw/USDA_GDP_MER.csv", comment.char='#')
gdp.mer$ISO <- toupper(gdp.mer$iso)
gdp.mer$iso <- NULL
gdp.mer.m <- melt(gdp.mer, variable="xyear")
names(gdp.mer.m)[names(gdp.mer.m)=="value"] <- "GDP"
gdp.mer.m$year <- datefix(gdp.mer.m$xyear)
gdp.mer.m <- subset(gdp.mer.m, select=c(Country, ISO, GDP, year))

master.table <- merge(select.complete(urban.pop.m), select.complete(gdp.mer.m))

## CO2 emissions from various sources.  We are using the cement
## emissions as a proxy for cement production
co2 <- read.csv("raw/L100.CDIAC_CO2_ctry_hist.csv", comment.char='#')
co2$ISO <- toupper(co2$iso)
co2 <- subset(co2, select=c(ISO, year, cement))
## Some countries have multiple entries in a year, typically
## representing two precusror countries.  Sum duplicate entries into a
## single entry.
co2 <- aggregate(co2[,"cement"], by=list(ISO=co2$ISO, year=co2$year), FUN=sum)
names(co2)[names(co2)=="x"] = "cement"
master.table <- merge(co2,master.table)

## Growth rate for urban population fraction
urban.growth <- to.ISO(read.csv("raw/urban-pop-growth.csv",comment.char='#'))
urban.growth.m <- reorg.by.yr(select.complete(urban.growth), "urban.growth")
master.table <- merge(urban.growth.m, master.table)

## Total population
pop.tot <- read.csv("raw/UN_popTot.csv", comment.char='#')
pop.tot <- pop.tot[pop.tot$Scenario=="EST",]
pop.tot$Region <- NULL
pop.tot$Sex <- NULL
pop.tot$Scenario <- NULL
master.table <- merge(pop.tot, master.table)

## Treat 0 cement values as missing
master.table$cement[master.table$cement <= 0] <- NA

## per capita quantities
master.table$pcGDP <- master.table$GDP / master.table$pop.tot
master.table$pccement <- master.table$cement / master.table$pop.tot
## Let's also try "cement intensity"
master.table$cement.intensity <- master.table$cement / master.table$GDP

### For the remaining calculations we need to break up the tables by
### country

master.table$ISO <- factor(master.table$ISO) #get rid of unused levels
master.table.m <- melt(master.table, id=c("ISO", "year", "Country"))
working.table <- cast(master.table.m, year~variable|ISO)

working.table <- lapply(working.table,
                        function(tbl) {
                            ## cement stock -- NB: this calculation
                            ## isn't exactly right.  We should have a
                            ## window instead of a cumulative sum from
                            ## the very beginning.
                            tbl$cement.stock <- cumsum(tbl$cement)
                            tbl$pccement.stock <- tbl$cement.stock / tbl$pop.tot
                            
                            ## ratio of GDP to 5-years prior gdp.
                            ## We're going to stick with trailing
                            ## ratios for now, though arguably leading
                            ## ratios might be better.
                            lag     <- 5
                            n       <- nrow(tbl)-lag
                            gdpshft <- c(rep(NA, lag), tbl$GDP[1:n])
                            yd      <- c(rep(NA, lag), diff(tbl$year, lag=lag)[1:n])
                            tbl$GDP.rate <-
                                log((tbl$GDP / gdpshft)^(lag/yd))

                            ## same for growth rate in per-capita cement
                            pccshft <- c(rep(NA, lag), tbl$pccement[1:n])
                            tbl$pcc.rate <-
                                log((tbl$pccement / pccshft)^(lag/yd))

                            tbl 
                        })


## Trim away all the data with NA values
working.table <- lapply(working.table,
                        function(tbl) {
                            data.frame(tbl[complete.cases(tbl),])
                        })

## delete the countries that had insufficient data
working.table[lapply(working.table, nrow)<10] <- NULL
master.table.m <- melt(working.table, id="year") 
names(master.table.m)[names(master.table.m)=="L1"] <- "ISO"
master.table <- cast(master.table.m,ISO+year~variable)


### Save output for future use
comment(master.table) <- date()
dput(master.table, file="cement-table.dat")

### select a sample of countries to hold back for a validation set (the "testing set")
### This will only happen if the list of testing countries isn't
### present on the disk.  Otherwise we load the previously generated
### set.
gen.test.countries <- function(frac=5, file=NULL) {
    testing.countries <- with(list(countries=levels(as.factor(master.table$ISO))),
                              sample(countries, length(countries)/frac))
    ## Make sure China is a testing country
    if(!"CHN" %in% testing.countries)
        testing.countries <- c(testing.countries, "CHN")
    comment(testing.countries) <- date()
    if(!is.null(file))
        dput(testing.countries, file=file)
    testing.countries
}

if(file.exists("testing-countries.dat")) {
    testing.countries <- dget(file="testing-countries.dat")
} else {
    testing.countries <- gen.test.countries(file="testing-countries.dat")
}

print("Testing countries are:")
print(testing.countries)
