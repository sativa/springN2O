library(aqp)
library(soilDB)
library(compositions)
library(reshape2)
library(latticeExtra)
library(plyr)

# get sand, silt, clay data from the named series
# return weighted mean of top 10cm
get.data <- function(series) {
  x <- fetchKSSL(series)
  s <- slab(x, pedon_key ~ clay + sand + silt + oc + ph_h2o, slab.structure=c(0, 10), strict = FALSE, slab.fun = mean, na.rm=TRUE)
  s <- dcast(s, pedon_key ~ variable, value.var = 'value')
  ssc <- na.omit(s[, c('sand', 'silt', 'clay', 'oc' 'ph')])
  return(ssc)
}

# combine original texture + simulated textures from normal composition
sim.data <- function(ssc, n.sim=1000) {
  # convert to compositional class, note range is now [0,1]
  ssc.acomp <- acomp(ssc)
  # simulate normally-distributed composition based on data
  # note that var() is dispatched to var.acomp()
  ssc.sim <- rnorm.acomp(n=n.sim, mean=meanCol(ssc.acomp), var=var(ssc.acomp))
  # convert to data.frame and original range of [0,100]
  ssc.sim <- as.data.frame(unclass(ssc.sim) * 100)
  # return stacked original + simulated data
  return(make.groups(original=ssc, simulated=ssc.sim))
}


# get data for named series
s.list <- c('clarksville', 'vista', 'auburn', 'cecil', 'drummer', 'capay')
d <- lapply(s.list, get.data)
names(d) <- s.list
d <- ldply(d)