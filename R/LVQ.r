LVQ <- function (data, originallabels, testdata = NA, prototypes = vector(), learningrate = 0.01, epochs = 10, initscheme = 'zero',
		  distscheme = 'euclidean', relevancemode = 'normal', relevancescheme = 'global', LVQscheme = 'LVQ1', optimisationscheme = 'normal',
		  relevances = NA, relrate = 0.001, customdist = 3, alfa = 2, show = FALSE, graphics = FALSE, costfunction = FALSE, plotcurve = FALSE,
		  progress = FALSE, relevanceprogress = FALSE, trainerrorprogress = FALSE, testerrorprogress = FALSE) {

  # split data in values and labels
  traindata <- data
  dimensions <- length(data[1,]) - 1
  labels <- data[,dimensions+1]
  data <- data[,1:dimensions]

  learningrate <- initLearningrate(learningrate, epochs)
  relrate <- initRelrate(relrate, epochs)

  #initialize customdist for distancemeasure and generalUpdate
  customdist <- switch(EXPR = distscheme, euclidean = 2, manhattan = 1, custom = customdist)
  if (customdist != 2 & relevancemode == 'matrix') {
    error(29)
  }

  # initialize prototypes according to initscheme
  prototypes <- prototypes[sort(levels(factor(originallabels)))]
  protomatrix <- initializePrototypes(initscheme, prototypes, data, originallabels, LVQscheme)
  protolabels <- protomatrix[,dimensions+1]
  protomatrix <- protomatrix[,1:dimensions]

  sorteddata <- vector()
  if (graphics) {
    # construct data for displaying results, sorteddata contains the trainingdata per class
    sorteddata <- sortData(data, labels)
  }

  costcurve <- vector()
  if (costfunction) {
    length(costcurve) <- epochs
  }

  nrofprototypes <- sum(prototypes)
  classes <- sort(levels(factor(originallabels)))
  relevances <- initializeRelevances(relevances, dimensions, relevancemode, relevancescheme, classes, nrofprototypes) 

  # show start configuration
  if (graphics) {
    dev.new()
  }
  showResults(protomatrix, protolabels, 0, sorteddata, data, labels, originallabels, costcurve, relevances, dimensions, graphics, show, relevancemode, relevancescheme, costfunction)

  protoprogress <- list()
  relprogress <- list()
  trainerrors <- vector()
  testerrors <- vector()
  if (progress) {
    length(protoprogress) <- epochs+1
    protoprogress[[1]] <- protomatrix
  }
  if (relevanceprogress) {
    length(relprogress) <- epochs+1
    if (class(relevances) == 'numeric') {
	relprogress[[1]] <- relevances
      }
      if (class(relevances) == 'matrix') {
	relprogress[[1]] <- relevances %*% relevances
      }

      if (class(relevances) == 'list') {
	if (class(relevances[[1]]) == 'numeric') {
	  relprogress[[1]] <- relevances
	}
	if (class(relevances[[1]]) == 'matrix') {
	  relproglist <- list()
	  for (k in 1:length(relevances)) {
	    relproglist[[k]] <- relevances[[k]] %*% relevances[[k]]
	  }
	  relprogress[[1]] <- relproglist
	}
      }
  }
  if (trainerrorprogress) {
    length(trainerrors) <- epochs
  }
  if (testerrorprogress) {
    length(testerrors) <- epochs
  }

  # start LVQ1
  for (i in 1:epochs) {
    # shuffle data
    datapermutation <- sample(1:length(data[,1]), replace=FALSE)
    # for every sample calculate closest prototype and update prototypes
    for (j in 1:length(datapermutation)) {
      datapoint <- data[datapermutation[j],]
      dataclass <- labels[datapermutation[j]]
      difference <- t(datapoint - t(protomatrix))
      dist <- switch(EXPR = LVQscheme, LVQ1 = metricDistWrap(difference, customdist, relevancemode, relevancescheme, relevances, protolabels),
				      cauchyschwarz = csDist(protomatrix, datapoint),
				      renyi = renyiDist(protomatrix, datapoint, alfa))
      ordered <- order(dist)
      winner <- ordered[1]
      winclass <- ordered[which.max(protolabels[ordered] == dataclass)]
      winnotclass <- ordered[which.max(protolabels[ordered] != dataclass)]
      protomatrix <-  switch (EXPR = LVQscheme, LVQ1 = LVQ1Update(optimisationscheme, protomatrix, protolabels, winner, winclass, winnotclass,
								  dataclass, difference, learningrate[j], dist, customdist, relevances, relevancemode, relevancescheme),
						cauchyschwarz = csUpdate(optimisationscheme, protomatrix, protolabels, winner, winclass, winnotclass,
									  datapoint, dataclass, learningrate[j], dist),
						renyi = renyiUpdate(optimisationscheme, protomatrix, protolabels, winner, winclass, winnotclass,
								    datapoint, dataclass, learningrate[j], dist, alfa))
	if (any(relevancemode == c('relevance', 'matrix'))) {
	  relevances <- updateRelevancesWrap(protolabels, winner, winclass, winnotclass, relevances, dataclass, relrate[j], difference,
					      dist, optimisationscheme, relevancescheme, relevancemode, customdist)
	}
    }

    if (costfunction) {
    	costcurve[i] <- switch(EXPR = optimisationscheme, normal = cost(LVQscheme, data, labels, protomatrix, protolabels, distscheme, customdist, relevancemode, relevancescheme,
									relevances, alfa),
							  general = generalCost(LVQscheme, data, labels, protomatrix, protolabels, distscheme, customdist, relevancemode, relevancescheme,
										relevances, alfa))
    }
    # show configuration at this point
    showResults(protomatrix, protolabels, i, sorteddata, data, labels, originallabels, costcurve, relevances, dimensions, graphics, show, relevancemode, relevancescheme, costfunction)

    if (progress) {
      protoprogress[[i+1]] <- protomatrix
    }
    if (relevanceprogress) {
      if (class(relevances) == 'numeric') {
	relprogress[[i+1]] <- relevances
      }
      if (class(relevances) == 'matrix') {
	relprogress[[i+1]] <- relevances %*% relevances
      }

      if (class(relevances) == 'list') {
	if (class(relevances[[1]]) == 'numeric') {
	  relprogress[[i+1]] <- relevances
	}
	if (class(relevances[[1]]) == 'matrix') {
	  relproglist <- list()
	  for (k in 1:length(relevances)) {
	    relproglist[[k]] <- relevances[[k]] %*% relevances[[k]]
	  }
	  relprogress[[i+1]] <- relproglist
	}
      }
    }
    if (trainerrorprogress) {
      trainerrors[i] <- test(data = traindata, prototypes = protomatrix, protolabels = protolabels, distscheme = distscheme,
			    relevancemode = relevancemode, relevancescheme = relevancescheme, relevances = relevances, customdist = customdist, alfa = alfa)
    }
    if (testerrorprogress) {
      testerrors[i] <- test(data = testdata, prototypes = protomatrix, protolabels = protolabels, distscheme = distscheme,
			    relevancemode = relevancemode, relevancescheme = relevancescheme, relevances = relevances, customdist = customdist, alfa = alfa)
    }
  }

  showEndResults(protomatrix, protolabels, originallabels, relevances, costcurve, 1:epochs, plotcurve, show, relevancemode, relevancescheme)

  nrofrelevances <- numeric()
  if (any(relevancescheme == c('local', 'classwise'))) {
    nrofrelevances <- length(relevances)
  } else {
    nrofrelevances <- 1
  }

  troutput <- new("LVQoutput")
  attr(troutput, "prototypes") <- protomatrix
  attr(troutput, "protolabels") <- protolabels
  attr(troutput, "relevances") <- relevances
  attr(troutput, "costcurve") <- costcurve
  attr(troutput, "protoprogress") <- protoprogress
  attr(troutput, 'relevanceprogress') <- relprogress
  attr(troutput, "originallabels") <- originallabels
  attr(troutput, "trainerrorprogress") <- trainerrors
  attr(troutput, "testerrorprogress") <- testerrors
  attr(troutput, "nrofrelevances") <- nrofrelevances
  troutput
}