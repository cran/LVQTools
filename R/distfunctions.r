metricDistWrap <- function (difference, customdist, relevancemode, relevancescheme, relevances, protolabels) {
  result <- switch(EXPR = relevancescheme,
			  global = calcAllDist(difference, customdist, relevancemode, relevances),
			  local = allLocalDist(difference, customdist, relevancemode, relevances),
			  classwise = allClasswiseDist(difference, customdist, relevancemode, relevances, protolabels))
  result
}

testMetricDist <- function (difference, customdist, prototypenumber, prototypeclass, relevancemode, relevancescheme, relevances) {
  result <- switch(EXPR = relevancescheme,
			    global = calcAllDist(difference, customdist, relevancemode, relevances),
			    local = calcAllDist(difference, customdist, relevancemode, relevances[[prototypenumber]]),
			    classwise = calcAllDist(difference, customdist, relevancemode, relevances[[prototypeclass]]))

  result
}

allClasswiseDist <- function (difference, customdist, relevancemode, relevances, protolabels) {
  result <- vector()
  if (relevancemode == 'normal') {
    result <- calcAllDist(difference, customdist, relevancemode, relevances)
  } else if (any(relevancemode == c('relevance', 'matrix'))){
    result <- switch(EXPR = relevancemode,
			    relevance = classwiseRelevanceDist(relevances, abs(difference), customdist, protolabels),
			    matrix = classwiseMatrixDist(relevances, abs(difference), protolabels))

  }
  result
}

allLocalDist <- function (difference, customdist, relevancemode, relevances) {
  result <- vector()
  if (relevancemode == 'normal') {
    result <- calcAllDist(difference, customdist, relevancemode, relevances)
  } else if (any(relevancemode == c('relevance', 'matrix'))){
    result <- switch(EXPR = relevancemode,
			    relevance = localRelevanceDist(relevances, abs(difference), customdist),
			    matrix = localMatrixDist(relevances, abs(difference)))

  }
  result
}

calcAllDist <- function (difference, customdist, relevancemode, relevances) {

  result <- switch(EXPR = relevancemode, 

    normal = rowSums((abs(difference)^customdist), na.rm = TRUE),


    relevance = rowSums(t(relevances * (t(abs(difference)))^customdist), na.rm = TRUE),

    matrix = matrixDist(relevances, abs(difference))

  )
  result
}

classwiseRelevanceDist <- function(relevances, difference, customdist, protolabels) {
  nrofprototypes <- length(difference[,1])
  dimensions <- length(relevances[[1]])
  rels <- array(dim=c(nrofprototypes, dimensions))
  for (i in 1:nrofprototypes) {
    rels[i,] <- relevances[[protolabels[i]]]
  }
  result <- rowSums(((rels * abs(difference))^customdist), na.rm = TRUE)
  result
}

classwiseMatrixDist <- function(relevances, difference, protolabels) {
  dimensions <- length(difference[1,])
  for (i in 1:length(relevances)) {
    relevances[[protolabels[i]]] <- t(relevances[[protolabels[i]]]) %*% relevances[[protolabels[i]]]
  }
  result <- vector()
  difference <- ifelse(is.na(difference), 0, difference)
  for (i in 1:length(difference[,1])) {
    result[i] <- t(difference[i,]) %*% relevances[[protolabels[i]]] %*% difference[i,]
  }
  result
}

localRelevanceDist <- function(relevances, difference, customdist) {
  nrofprototypes <- length(relevances)
  dimensions <- length(relevances[[1]])
  rels <- array(dim=c(nrofprototypes, dimensions))
  for (i in 1:nrofprototypes) {
    rels[i,] <- relevances[[i]]
  }
  result <- rowSums((rels * (abs(difference))^customdist), na.rm = TRUE)
  result
}

localMatrixDist <- function(relevances, difference) {
  dimensions <- length(difference[1,])
  for (i in 1:length(relevances)) {
    relevances[[i]] <- t(relevances[[i]]) %*% relevances[[i]]
  }
  result <- vector()
  difference <- ifelse(is.na(difference), 0, difference)
  for (i in 1:length(difference[,1])) {
    result[i] <- t(difference[i,]) %*% relevances[[i]] %*% difference[i,]
  }
  result
}

matrixDist <- function (relmatrix, difference) {
  
  dimensions <- length(difference[1,])
  relmatrix <- t(relmatrix) %*% relmatrix
  result <- vector()
  difference <- ifelse(is.na(difference), 0, difference)
  for (i in 1:length(difference[,1])) {
    result[i] <- t(difference[i,]) %*% relmatrix %*% difference[i,]
  }
  result
}

csDist <- function(protomatrix, datapoint) {
  dist <- log(sum(datapoint^2, na.rm = T)*rowSums(protomatrix^2))/2 - log(rowSums(t(datapoint*t(protomatrix)), na.rm = T))
  dist
}

renyiDist <- function(protomatrix, datapoint, alfa) {
  dist <- 1/(alfa-1) * log(rowSums(t((datapoint^alfa)/t(protomatrix^(alfa-1))), na.rm = T))
  dist
}

renyiTestDist <- function (prototype, data, alfa) {
  dist <- 1/(alfa-1) * log(rowSums(t(t(data^alfa) / prototype^(alfa-1)), na.rm = T))
  dist
}