require(ggplot2)
require(gridExtra)

if(!exists("master.table")) master.table <- dget(file="cement-table.dat")
## The cement stocks are are really only meaningful when we've got about 20 years worth of data
stock.table <- master.table[master.table$year >= 1990, ]

scatter.vars <- list("urban.growth", "urban.pop", "pcGDP", "GDP.rate", "pccement.stock")

## for an input string, return the elements of the input list that follow the string
trimlist <- function(str,ll) {
    ltmp <- ll
    while(length(ltmp) > 1 && ltmp[[1]] != str) {
        ltmp <- ltmp[-1]
    }
    if(ltmp[[1]]==str)
        ltmp[-1]
    else
        # if the element isn't in the list at all, return the original list
        ll
}

## scatter pccement against each of the scatter vars
pccplots <- lapply(scatter.vars,
                   function(var) {
                       table <- if(var == "pccement.stock") stock.table else master.table
                       qplot(data=table, x=eval(as.symbol(var)), y=pccement, geom="point", color=ISO) +
                           theme(legend.position="none") + xlab(var)
                   })


scatterplots <- as.list(
    unlist(
        lapply(scatter.vars,
               function(var1) {
                   sv <- trimlist(var1, scatter.vars)
                   lapply(sv, function(var2) {
                       table <- if(var1=="pccement.stock" || var2=="pccement.stock") stock.table else master.table
                       qplot(data=table, x=eval(as.symbol(var1)), y=eval(as.symbol(var2)), size=pccement, geom="point", color=ISO) +
                           theme(legend.position="none") + xlab(var1) + ylab(var2)
                   })
               }) 
        , recursive=FALSE)
    )

key <- qplot(data=master.table[master.table$pccement>0.2,], x=urban.pop, y=pccement, geom="point", color=ISO)


cat("Plots are in 'pccplots' and 'scatterplots'.  Try something like the following to view them:",
    " do.call(grid.arrange, pccplots)",
    " do.call(grid.arrange,scatterplots)", sep='\n')


### Example of the nrow argument:
## #grid.arrange(qlt46,qlt48,qlt49,qlt68,qlt69,qlt89, nrow=2)
