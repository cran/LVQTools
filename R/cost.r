cost <- function (LVQscheme, data, labels, prototypes, protolabels, distscheme, customdist, relevancemode, relevancescheme, relevances, alfa) {
  
  customdist <- switch(EXPR = distscheme, manhattan = 1, euclidean = 2, custom = customdist)

  result <- 0
  nrdatapoints <- length(data[,1])
  for(i in 1:nrdatapoints) {
    datapoint <- data[i,]
    dataclass <- labels[i]
    difference <- t(datapoint - t(prototypes))
    dists <- switch(EXPR = LVQscheme, LVQ1 = metricDistWrap(difference, customdist, relevancemode, relevancescheme, relevances, protolabels),
				      cauchyschwarz = csDist(prototypes, datapoint),
				      renyi = renyiDist(prototypes, datapoint, alfa))
    winclass <- order(dists)[which.max(protolabels[order(dists)] == dataclass)]
    result <- result + sum(dists[winclass], na.rm = TRUE)
  }
  result
}

generalCost <- function (LVQscheme, data, labels, prototypes, protolabels, distscheme, customdist, relevancemode, relevancescheme, relevances, alfa) {

  customdist <- switch(EXPR = distscheme, manhattan = 1, euclidean = 2, custom = customdist)

  result <- 0
  nrdatapoints <- length(data[,1])
  for (i in 1:nrdatapoints) {
    datapoint <- data[i,]
    dataclass <- labels[i]
    difference <- t(datapoint - t(prototypes))
    dists <- switch(EXPR = LVQscheme, LVQ1 = metricDistWrap(difference, customdist, relevancemode, relevancescheme, relevances, protolabels),
				      cauchyschwarz = csDist(prototypes, datapoint),
				      renyi = renyiDist(prototypes, datapoint, alfa))
    winclass <- order(dists)[which.max(protolabels[order(dists)] == dataclass)]
    winnotclass <- order(dists)[which.max(protolabels[order(dists)] != dataclass)]
    result <- result + (dists[winclass] - dists[winnotclass])/(dists[winclass]+dists[winnotclass])
  }
  result
}