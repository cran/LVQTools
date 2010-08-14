# wrapper function to switch between different initialisation schemes
initializePrototypes <- function(initscheme, prototypes, data, labels, LVQscheme) {
  switch(initscheme,

      randomwindow = randomWindowInit(prototypes, data, LVQscheme),
      zero = zeroInit(prototypes, data),
      randomsample = randomSampleInit(prototypes, data),
      mean = meanInit(prototypes, data),
      classmean = meanClasswiseInit(prototypes, data, labels))
}

# randomly initializes prototypes within window of the data
randomWindowInit <- function(prototypes, data, LVQscheme) {
  dimensions <- length(data[1,])
  minmaxmatrix <- minmax(data[,1:dimensions])
  nrprototypes <- sum(prototypes)
  length <- length(minmaxmatrix[1,])
  random <- array(runif(nrprototypes*length), dim=c(nrprototypes, length))
  protomatrix <- t((minmaxmatrix[2,]-minmaxmatrix[1,])*t(random) + minmaxmatrix[1,])
  if (any(LVQscheme == c('cauchyschwarz', 'renyi'))) {
    protomatrix[1:nrprototypes,] <- protomatrix[1:nrprototypes,]/rowSums(protomatrix[1:nrprototypes,])
  }
  labels <- constructlabels(prototypes)
  protomatrix <- array(c(protomatrix,labels),dim=c(nrprototypes,dimensions+1))
  protomatrix
}

# initializes prototypes by setting all values to zero
zeroInit <- function(prototypes, data) {
  dimensions <- length(data[1,])
  protomatrix <- array(seq(0:0, by = 0, length = (dimensions+1) * sum(prototypes)), dim=c(sum(prototypes), dimensions+1))
  prev <- 1
  cumprot <- cumsum(prototypes)
  for (i in 1:length(prototypes)) {
    protomatrix[prev:cumprot[i],dimensions+1] <- i
    prev <- cumprot[i]+1
  }
  protomatrix
}

# initializes prototypes by selecting a random sample from the dataset
randomSampleInit <- function (prototypes, data) {
  dimensions <- length(data[1,])
  nrprototypes <- sum(prototypes)
  samp <- sample(1:length(data[,1]))
  while (any(is.na(data[samp[1:nrprototypes],]))) {
    samp <- sample(1:length(data[,1]))
  }
  protomatrix <- data[samp[1:nrprototypes],]
  labels <- constructlabels(prototypes)
  protomatrix <- array(c(protomatrix,labels),dim=c(nrprototypes,dimensions+1))
  protomatrix
}

# initializes prototypes by taking the mean of all samples of the dataset
meanInit <- function (prototypes, data) {
  dimensions <- length(data[1,])
  nrprototypes <- sum(prototypes)
  means <- colMeans(data, na.rm = TRUE)
  protomatrix <- array(dim = c(dimensions, nrprototypes))
  protomatrix[,1:nrprototypes] <- means
  protomatrix <- t(protomatrix)
  labels <- constructlabels(prototypes)
  protomatrix <- array(c(protomatrix,labels),dim=c(nrprototypes,dimensions+1))
  protomatrix
}

# initializes prototypes by taking the mean of each class of the dataset and using that for the appropriate prototypes
meanClasswiseInit <- function (prototypes, data, labels) {
  lvls <- levels(factor(labels))
  nrdatapoints <- length(data[,1])
  nrprototypes <- sum(prototypes)
  datalength <- length(data[1,])
  protomatrix <- array(dim=c(nrprototypes,datalength+1))
  start <- 1
  for (i in 1:length(lvls)) {
    means <- colMeans(data[labels == lvls[i],], na.rm = TRUE)
    protomatrix[start:(start+ prototypes[lvls[i]] -1 ), 1:datalength] <- means
    protomatrix[start:(start+ prototypes[lvls[i]] -1 ),datalength+1] <- i
    start <- start + prototypes[lvls[i]]
  }
  protomatrix
}

# auxiliary function, takes the minimum and maximum of all dimensions
minmax <- function (data) {
  datadim <- dim(data)
  minmaxmat <- array(dim=c(2, datadim[2]))
  for (i in 1:(datadim[2])) {
    minmaxmat[1,i] <- min(data[,i], na.rm = TRUE)
    minmaxmat[2,i] <- max(data[,i], na.rm = TRUE)
  }
  minmaxmat
}

constructlabels <- function(prototypes) {
  labels <- vector()
  prev <- 1
  cumprot <- cumsum(prototypes)
  for (i in 1:length(prototypes)) {
    labels[prev:cumprot[i]] <- i
    prev <- cumprot[i]+1
  }
  labels
}

initializeRelevances <- function (relevances, dimensions, relevancemode, relevancescheme, classes, nrofprototypes) {
  relevances <- switch (EXPR = relevancescheme, 
		  global = globalInit(relevances, dimensions, relevancemode),
		  local = localInit(relevances, dimensions, relevancemode, nrofprototypes),
		  classwise = classwiseInit(relevances, dimensions, relevancemode, classes))

  relevances
}

globalInit <- function (relevances, dimensions, relevancemode) {
  relevances <- switch(EXPR = relevancemode,
		  normal = relevances,
		  relevance = initializeRelVector(relevances, dimensions),
		  matrix = initializeRelMatrix(relevances, dimensions))

  relevances
}

localInit <- function (relevances, dimensions, relevancemode, nrofprototypes) {
  
  rels <- list()
  if (all(is.na(relevances))) {
    relevances <- list()
    length(relevances) <- nrofprototypes
    relevances[1:nrofprototypes] <- NA
  }
  for (i in 1:nrofprototypes) {
    rels[[i]] <- switch(EXPR = relevancemode,
			  normal = relevances,
			  relevance = initializeRelVector(relevances[[i]], dimensions),
			  matrix = initializeRelMatrix(relevances[[i]], dimensions))
  }
  relevances <- rels
  relevances
}

classwiseInit <- function(relevances, dimensions, relevancemode, classes) {

  rels <- list()
  if (all(is.na(relevances))) {
    relevances <- list()
    length(relevances) <- length(classes)
    relevances[classes] <- NA
  }
  for (i in 1:length(classes)) {
    rels[[i]] <- switch(EXPR = relevancemode,
		  normal = relevances,
		  relevance = initializeRelVector(relevances[[classes[i]]], dimensions),
		  matrix = initializeRelMatrix(relevances[[classes[i]]], dimensions))
  }
  relevances <- rels
  relevances
}

initializeRelVector <- function(relevances, dimensions) {
  if (all(is.na(relevances))) {
    relvec <- vector()
    relvec[1:dimensions] <- runif(dimensions, min = 0, max = 1)
    relvec <- relvec/sum(relvec)
  } else {
    relvec <- relevances
  }
  relvec
}

initializeRelMatrix <- function(relevances, dimensions) {
  relmat <- matrix()
  if (all(is.na(relevances))) {
    relmat <- relmat <- array(runif(dimensions^2, min = -1, max = 1),dim=c(dimensions, dimensions))
    relmat <- normalizeMatrix(relmat)
  } else {
    relmat <- relevances
  }
  relmat
}

initLearningrate <- function (learningrate, epochs) {
  if (length(learningrate) == 1) {
    learningrate[1:epochs] <- learningrate
  }
  learningrate
}

initRelrate <- function(relrate, epochs) {
  if (length(relrate) == 1) {
    relrate[1:epochs] <- relrate
  }
  relrate
}