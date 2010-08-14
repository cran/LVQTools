validate <- function( validatescheme = 'train', testdatapath = NA, nfold = 8, LVQscheme = 'LVQ1', optimisationscheme = 'normal', inp = NA, testinp = NA,

		      prototypeoutput = TRUE, relevanceoutput = FALSE, costcurve = FALSE, progress = FALSE, relevanceprogress = FALSE, trainerror = FALSE, testerror = FALSE, trainerrorprogress = FALSE, testerrorprogress = FALSE,

		      datapath = NA, normalizescheme = 'none', normalclasswise = 'none', replaceNA = FALSE, replaceclasswise = FALSE,

		      prototypes = vector() , learningrate = 0.01, epochs = 10, initscheme = 'zero', distscheme = 'euclidean', relevancemode = 'normal', relevancescheme = 'global', relevances = NA, relrate = 0.001, customdist = 3, alfa = 2, show = FALSE, graphics = FALSE, plotcurve = FALSE) {

  checkInput(inp, testinp)

  inp <- input(datapath, normalizescheme = normalizescheme, normalclasswise = normalclasswise, replaceNA = replaceNA,
		replaceclasswise = replaceclasswise, input = inp)

  testinp <- input(testdatapath, normalizescheme = normalizescheme, normalclasswise = normalclasswise, replaceNA = replaceNA,
      		replaceclasswise = replaceclasswise, input = testinp)

  data 			<- attr(inp, 'data')
  originaldatalabels 	<- attr(inp, 'labels')

  testdata <- attr(testinp, 'data')
  originaltestlabels <- attr(testinp, 'labels')

  lvls <- levels(factor(originaldatalabels))
  dimensions <- length(data[1,]) - 1

  checkLVQvars(prototypes, learningrate, epochs, initscheme, distscheme, relevancemode, relevancescheme, LVQscheme, optimisationscheme, relrate, customdist, alfa, show, graphics, plotcurve, lvls, dimensions)

  checkValidateVars(validatescheme, nfold, length(data[,1]))

  checkOutputVars(prototypeoutput, relevanceoutput, costcurve, progress, relevanceprogress, trainerror, testerror, trainerrorprogress, testerrorprogress, validatescheme, relevancemode)

  checkData(data, LVQscheme, relevances, relevancescheme)

  checkData(testdata, LVQscheme, relevances, relevancescheme)

  result <- switch(EXPR = validatescheme,

	  nfold = nfoldcross(data = data, labels = originaldatalabels, nfold = nfold, LVQscheme = LVQscheme, optimisationscheme = optimisationscheme,
				prototypes = prototypes, learningrate = learningrate, epochs = epochs, initscheme = initscheme, distscheme = distscheme,
				relevancemode = relevancemode, relevancescheme = relevancescheme, relevances = relevances, relrate = relrate, customdist = customdist, alfa = alfa,
				show = show, graphics = graphics, costcurve = costcurve, plotcurve = plotcurve, progress = progress,
				relevanceprogress = relevanceprogress, trainerror = trainerror, testerror = testerror, trainerrorprogress = trainerrorprogress,
				testerrorprogress = testerrorprogress),
	  
	  traintest = traintest(data = data, labels = originaldatalabels, testdata = testdata, LVQscheme = LVQscheme,
				  optimisationscheme = optimisationscheme, prototypes = prototypes, learningrate = learningrate, epochs = epochs,
				  initscheme = initscheme, distscheme = distscheme, relevancemode = relevancemode, relevancescheme = relevancescheme,
				  relevances = relevances, relrate = relrate, customdist = customdist, alfa = alfa, show = show, graphics = graphics,
				  costcurve = costcurve, plotcurve = plotcurve, progress = progress, relevanceprogress = relevanceprogress,
				  trainerror = trainerror, testerror = testerror, trainerrorprogress = trainerrorprogress, testerrorprogress = testerrorprogress), 

	  train = train(data = data, labels = originaldatalabels, LVQscheme = LVQscheme, optimisationscheme = optimisationscheme,
			prototypes = prototypes, learningrate  = learningrate, epochs = epochs, initscheme = initscheme,
			distscheme = distscheme, relevancemode = relevancemode, relevancescheme = relevancescheme, relevances = relevances, relrate = relrate,
			customdist = customdist, alfa = alfa, show = show, graphics = graphics, costcurve = costcurve, plotcurve = plotcurve,
			progress = progress, relevanceprogress = relevanceprogress, trainerror = trainerror,
			trainerrorprogress = trainerrorprogress))

  if (any(validatescheme == c('traintest', 'train'))) {
    attr(result, 'relevances') <- attr(result, 'relevances') %*% attr(result, 'relevances')
  }

  result <- output(result, validatescheme, prototypeoutput, relevanceoutput, costcurve, progress, relevanceprogress, trainerror, testerror,
		    trainerrorprogress, testerrorprogress)
  result
}

traintest <- function (data, labels, testdata, LVQscheme = 'LVQ1', optimisationscheme = 'normal', prototypes = vector(), learningrate = 0.01, epochs = 10,
			initscheme = 'zero', distscheme = 'euclidean', relevancemode = 'normal', relevancescheme = 'global', relevances = NA, relrate = 0.001,
			customdist = 3, alfa = 2, show = FALSE, graphics = FALSE, costcurve = FALSE, plotcurve = FALSE, progress = FALSE, relevanceprogress = FALSE, trainerror = FALSE, testerror = FALSE , trainerrorprogress = FALSE, testerrorprogress = FALSE) {

  troutput <- train(data = data, labels = labels, testdata = testdata, LVQscheme = LVQscheme, optimisationscheme = optimisationscheme, prototypes = prototypes,
			learningrate = learningrate, epochs = epochs, initscheme = initscheme, distscheme = distscheme, relevancemode = relevancemode,
			relevancescheme = relevancescheme, relevances = relevances, relrate = relrate, customdist = customdist, alfa = alfa, show = show,
			graphics = graphics, costcurve = costcurve, plotcurve = plotcurve, progress = progress,
			relevanceprogress = relevanceprogress, trainerror = trainerror, trainerrorprogress = trainerrorprogress, testerrorprogress = testerrorprogress)

  protomatrix <- attr(troutput, 'prototypes')
  protolabels <- attr(troutput, 'protolabels')
  relevances <- attr(troutput, 'relevances')

  if (testerror) {
    missclass <- test(data = testdata, prototypes = protomatrix, protolabels = protolabels, distscheme = distscheme,
		    relevancemode = relevancemode, relevancescheme = relevancescheme, LVQscheme = LVQscheme, relevances = relevances, customdist = customdist, alfa = alfa)

    attr(troutput, 'testerror') <- missclass
  }

  troutput
}

nfoldcross <- function(data, labels, nfold  = 8, LVQscheme = 'LVQ1', optimisationscheme = 'normal', prototypes = vector(), learningrate = 0.01, epochs = 10, initscheme = 'zero',
			distscheme = 'euclidean', relevancemode = 'normal', relevancescheme = 'global', relevances = NA, relrate = 0.001, customdist = 3, alfa = 2,
			show = FALSE, graphics = FALSE, costcurve = FALSE, plotcurve = FALSE, progress = FALSE, relevanceprogress = FALSE,
			trainerror = FALSE, testerror = FALSE, trainerrorprogress = FALSE, testerrorprogress = FALSE) {

  dimensions <- length(data[1,]) - 1

  foldindices <- constructFoldIndices(data, nfold)

  relevancesoutput <- vector()

  folds <- list()
  foldlabels <- list()
  for (i in 1:nfold) {
    folds[[i]] <- data[foldindices[i,],]
    foldlabels[[i]] <- labels[foldindices[i,]]
    lengte <- length(folds[[i]][,1])
    if (all(is.na(folds[[i]][lengte,]))) {
      folds[[i]] <- folds[[i]][-lengte,]
      foldlabels[[i]] <- foldlabels[[i]][-lengte]
    }
  }

  outputlist <- list()
  length(outputlist) <- nfold

  for (i in 1:nfold) {
    testdata <- folds[[i]]
    traindata <- constructTrainData(folds, i)
    trainlabels <- constructTrainLabels(foldlabels, i)
    trainoutput <- train(data = traindata, labels = trainlabels, testdata = testdata, LVQscheme = LVQscheme, optimisationscheme = optimisationscheme,
			  prototypes = prototypes, learningrate = learningrate, epochs = epochs, initscheme = initscheme, distscheme = distscheme,
			  relevancemode = relevancemode, relevancescheme = relevancescheme, relevances = relevances, relrate = relrate,
			  customdist = customdist, alfa = alfa, show = show, graphics = graphics, costcurve = costcurve, plotcurve = plotcurve,
			  progress = progress, relevanceprogress = relevanceprogress, trainerror = trainerror, trainerrorprogress = trainerrorprogress, testerrorprogress=testerrorprogress)

    protomatrix <- attr(trainoutput, 'prototypes')
    protolabels <- attr(trainoutput, 'protolabels')
    relevancesoutput <- attr(trainoutput, 'relevances')

    if (testerror) {
      missclass <- test(data = testdata, prototypes = protomatrix, protolabels = protolabels, distscheme = distscheme,
		    relevancemode = relevancemode, relevancescheme = relevancescheme, LVQscheme = LVQscheme, relevances = relevancesoutput, customdist = customdist, alfa = alfa)
      attr(trainoutput, 'testerror') <- missclass
    }

    if (class(relevancesoutput) == 'matrix') {
      attr(trainoutput, 'relevances') <- relevancesoutput %*% relevancesoutput
    }
    if (class(relevancesoutput) == 'list') {
      if (class(relevancesoutput[[1]]) == 'matrix') {
	for (j in 1: length(relevancesoutput)) {
	  relevancesoutput[[j]] <- relevancesoutput[[j]] %*% relevancesoutput[[j]]
	}
	attr(trainoutput, 'relevances') <- relevancesoutput
      }
    }

    outputlist[[i]] <- trainoutput

  }
  outputlist
}

constructTrainLabels <- function(folds, iteratie) {
  trainlabels <- vector()
  for (i in 1:length(folds)) {
    if (i != iteratie) {
      trainlabels <- c(trainlabels, folds[[i]])
    }
  }
  trainlabels
}

constructTrainData <- function(folds, iteratie) {
  traindata <- vector()
  nrdatapoints <- 0
  dimensions <- length(folds[[1]][1,])
  for (i in 1:length(folds)) {
    if (i != iteratie) {
      nrdatapoints <- nrdatapoints + length(folds[[i]][,1])
      traindata <- c(traindata, t(folds[[i]]))
    }
  }
  traindata <- t(array(traindata, dim = c(dimensions, nrdatapoints)))
  traindata
}

constructFoldIndices <- function(data, nfold) {
  nrdatapoints <- length(data[,1])

  samp <- sample(1:nrdatapoints)

  foldlength <- ceiling(nrdatapoints/nfold)
  foldindices <- array(dim=c(nfold,foldlength))
  for (i in 1:nfold) {
    start <- ((i-1)*floor(nrdatapoints/nfold))+1
    end <- i*floor(nrdatapoints/nfold)
    foldindices[i,1:(end-start+1)] <- samp[start:end]
  }
  remainingdatapoints <- nrdatapoints%%nfold
  if ((remainingdatapoints)  != 0) {
    for(i in 1:remainingdatapoints) {
      foldindices[i,foldlength] <- samp[nrdatapoints-i+1]
    }
  }
  foldindices
}

train <- function(data, labels, testdata = NA, LVQscheme = 'LVQ1', optimisationscheme = 'normal', prototypes = vector(), learningrate = 0.01, epochs = 10,
		    initscheme = 'zero', distscheme = 'euclidean', relevancemode = 'normal', relevancescheme = 'global', relevances = NA, relrate = 0.001, customdist = 3, alfa = 2,
		    show = FALSE, graphics = FALSE, costcurve = FALSE, plotcurve = FALSE, progress = FALSE, relevanceprogress = FALSE,
		    trainerror = FALSE, trainerrorprogress = FALSE, testerrorprogress = FALSE) {

	  trainoutput <- LVQ(data = data, originallabels = labels, testdata = testdata, prototypes = prototypes, learningrate = learningrate,
		  epochs = epochs, initscheme = initscheme, distscheme = distscheme, relevancemode = relevancemode, relevancescheme = relevancescheme,
		  LVQscheme = LVQscheme, optimisationscheme = optimisationscheme, relevances=relevances, relrate = relrate, customdist = customdist,
		  alfa = alfa, show = show, graphics = graphics, costfunction = costcurve, plotcurve = plotcurve, progress = progress,
		  relevanceprogress = relevanceprogress, trainerrorprogress = trainerrorprogress, testerrorprogress = testerrorprogress)

	  protomatrix <- attr(trainoutput, 'prototypes')
	  protolabels <- attr(trainoutput, 'protolabels')
	  relevances <- attr(trainoutput, 'relevances')

	  if (trainerror) {
	    missclass <- test(data = data, prototypes = protomatrix, protolabels = protolabels, distscheme = distscheme,
		    relevancemode = relevancemode, relevancescheme = relevancescheme, LVQscheme = LVQscheme,
		    relevances = relevances, customdist = customdist, alfa = alfa)

	    attr(trainoutput, 'trainerror') <- missclass
	  }

	  trainoutput
}

test <- function(data, prototypes = vector(), protolabels, distscheme = 'euclidean', relevancemode = 'normal', relevancescheme = 'global',
		  LVQscheme = 'LVQ1', relevances = NA, customdist = 3, alfa = 2) {

  customdist <- switch(EXPR = distscheme, manhattan = 1, euclidean = 2, custom = customdist)

  dimensions <- length(data[1,]) - 1
  labels <- data[,dimensions + 1]
  data <- data[,1:dimensions]
  missclass <- 0
  nrdatapoints <- length(data[,1])
  nrprototypes <- length(prototypes[,1])
  dist <- vector()
  length(dist) <- nrprototypes*nrdatapoints
  for (i in 1:nrprototypes) {
    start <- (i-1)*nrdatapoints+1
    end <- i*nrdatapoints
    prot <- prototypes[i,]
    difference <- t(t(data) - prot)
    dist[start:end] <- switch(EXPR = LVQscheme, LVQ1 = testMetricDist(difference, customdist, i, protolabels[i], relevancemode, relevancescheme, relevances),
					      cauchyschwarz = csDist(data, prot),
					      renyi = renyiTestDist(prot, data, alfa))
  }
  for (i in 1:nrdatapoints) {
    indices <- nrdatapoints*(0:(nrprototypes-1))+i
    winner <- ((order(dist[indices])[1]-1) %% nrdatapoints)+1
    if (labels[i] != protolabels[winner]) {
      missclass <- missclass+1
    }
  }
  ratio <- missclass/nrdatapoints
  ratio
}