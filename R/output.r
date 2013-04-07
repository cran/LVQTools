setClass('LVQoutput', representation(prototypes = 'matrix', protolabels = 'vector', relevances = 'matrix', costcurve = 'vector',
					protoprogress = 'list', relevanceprogress = 'list', trainerror = 'integer', testerror = 'integer',
					trainerrorprogress = 'vector', testerrorprogress = 'vector', originallabels = 'vector', nrofrelevances = 'numeric'),
		      prototype(prototypes = matrix(), protolabels = vector(), relevances = matrix(), costcurve = vector(), protoprogress = list(),
					relevanceprogress = list(), trainerror = integer(), testerror = integer(), trainerrorprogress = vector(),
					testerrorprogress = vector(), originallabels = vector(), nrofrelevances = numeric()))

setClass('input', representation(data = 'matrix', labels = 'vector'), prototype(data = matrix(), labels = vector()))

setClass('trainoutput', representation(prototypes = 'matrix', relevances = 'vector', costcurve = 'vector', protoprogress = 'list',
					relevanceprogress = 'list', trainerror = 'integer', trainerrorprogress = 'vector', nrofrelevances = 'numeric'),
			prototype(prototypes = matrix(), relevances = vector(), costcurve = vector(), protoprogress = list(),
					relevanceprogress = list(), trainerror = integer(), trainerrorprogress = vector(), nrofrelevances = numeric()))

setClass('traintestoutput', representation(prototypes = 'matrix', relevances = 'vector', costcurve = 'vector', protoprogress = 'list',
					      relevanceprogress = 'list', trainerror = 'integer', testerror = 'integer',
					      trainerrorprogress = 'vector', testerrorprogress = 'vector', nrofrelevances = 'numeric'),
			    prototype(prototypes = matrix(), relevances = vector(), costcurve = vector(), protoprogress = list(),
					      relevanceprogress = list(), trainerror = integer(), testerror= integer(),
					      trainerrorprogress = vector(), testerrorprogress = vector(), nrofrelevances = numeric()))

setClass('nfoldoutput', representation(prototypes = 'list', relevances = 'list', costcurve = 'list', protoprogress = 'list', relevanceprogress = 'list',
					      trainerror = 'vector', testerror = 'vector', trainerrorprogress = 'list', testerrorprogress = 'list',
					      nfold = 'numeric', nrofrelevances = 'numeric'),
			prototype(prototypes = list(), relevances = list(), costcurve = list(), protoprogress = list(), relevanceprogress = list(),
					      trainerror = vector(), testerror = vector(), trainerrorprogress = list(), testerrorprogress = list(),
					      nfold = numeric(), nrofrelevances = numeric()))

output <- function (LVQresult, validatescheme, prototypeoutput, relevanceoutput, costfunction, progress, relevanceprogress, trainerror, testerror, trainerrorprogress, testerrorprogress) {

  result <- switch(EXPR = validatescheme, train = trainoutput(LVQresult, prototypeoutput, relevanceoutput, costfunction, progress,
					  relevanceprogress, trainerror, trainerrorprogress),
					  traintest = traintestoutput(LVQresult, prototypeoutput, relevanceoutput, costfunction, progress, relevanceprogress, trainerror, testerror, trainerrorprogress, testerrorprogress),
					  nfold = nfoldcrossoutput(LVQresult, prototypeoutput, relevanceoutput, costfunction, progress, relevanceprogress, trainerror, testerror, trainerrorprogress, testerrorprogress))
}

trainoutput <- function(LVQresult, prototypeoutput, relevanceoutput, costfunction, progress, relevanceprogress, trainerror, trainerrorprogress) {

  result <- new('trainoutput')

  attr(result, 'nrofrelevances') <- attr(LVQresult, 'nrofrelevances')

  if (prototypeoutput) {
    prototypes <- attr(LVQresult, 'prototypes')
    protolabels <- attr(LVQresult, 'protolabels')
    originallabels <- attr(LVQresult, 'originallabels')
    attr(result, 'prototypes') <- attachLabels(prototypes, protolabels, originallabels)
  }
  if (relevanceoutput) {
    rels <- attr(LVQresult, 'relevances')
    
    if (class(rels) == 'numeric') {
      attr(result, 'relevances') <- rels
    }

    if (class(rels) == 'matrix') {
      attr(result, 'relevances') <- rels %*% rels
    }

    
  }

  if (costfunction) {
    attr(result, 'costcurve') <- attr(LVQresult, 'costcurve')
  }

  if (progress) {
    attr(result, 'protoprogress') <- attr(LVQresult, 'protoprogress')
  }

  if (relevanceprogress) {
    rels <- vector()
    if (attr(result, 'nrofrelevances') > 1) {
      rels <- orderRelevances(attr(LVQresult, 'relevanceprogress'))
    } else {
      rels <- attr(LVQresult, 'relevanceprogress')
    }
    attr(result, 'relevanceprogress') <- rels
  }

  if (trainerror) {
    attr(result, 'trainerror') <- attr(LVQresult, 'trainerror')
  }

  if (trainerrorprogress) {
    attr(result, 'trainerrorprogress') <- attr(LVQresult, 'trainerrorprogress')
  }

  result
}

traintestoutput <- function (LVQresult, prototypeoutput, relevanceoutput, costfunction, progress, relevanceprogress, trainerror, testerror, trainerrorprogress, testerrorprogress) {

  result <- new('traintestoutput')

  attr(result, 'nrofrelevances') <- attr(LVQresult, 'nrofrelevances')

  if (prototypeoutput) {
    prototypes <- attr(LVQresult, 'prototypes')

    protolabels <- attr(LVQresult, 'protolabels')
    originallabels <- attr(LVQresult, 'originallabels')
    attr(result, 'prototypes') <- attachLabels(prototypes, protolabels, originallabels)
  }
  if (relevanceoutput) {
    attr(result, 'relevances') <- attr(LVQresult, 'relevances')
  }

  if (costfunction) {
    attr(result, 'costcurve') <- attr(LVQresult, 'costcurve')
  }

  if (progress) {
    attr(result, 'protoprogress') <- attr(LVQresult, 'protoprogress')
  }

  if (relevanceprogress) {
    rels <- vector()
    if (attr(result, 'nrofrelevances') > 1) {
      rels <- orderRelevances(attr(LVQresult, 'relevanceprogress'))
    } else {
      rels <- attr(LVQresult, 'relevanceprogress')
    }
    attr(result, 'relevanceprogress') <- rels
  }

  if (testerror) {
    attr(result, 'testerror') <- attr(LVQresult, 'testerror')
  }

  if (trainerror) {
    attr(result, 'trainerror') <- attr(LVQresult, 'trainerror')
  }

  if (testerrorprogress) {
    attr(result, 'testerrorprogress') <- attr(LVQresult, 'testerrorprogress')
  }

  if (trainerrorprogress) {
    attr(result, 'trainerrorprogress') <- attr(LVQresult, 'trainerrorprogress')
  }

  result
}

nfoldcrossoutput <- function (LVQlist, prototypeoutput, relevanceoutput, costfunction, progress, relevanceprogress, trainerror, testerror, trainerrorprogress, testerrorprogress) {

  result <- new('nfoldoutput')

  n <- length(LVQlist)

  attr(result, 'nfold') <- n

  attr(result, 'nrofrelevances') <- attr(LVQlist[[1]], 'nrofrelevances')

  if (prototypeoutput) {
    prototypes <- list()
    for (i in 1:n ) {
      prots <- attr(LVQlist[[i]], 'prototypes')
      protolabels <- attr(LVQlist[[i]], 'protolabels')
      originallabels <- attr(LVQlist[[i]], 'originallabels')
      prototypes[[i]] <- attachLabels(prots, protolabels, originallabels)
    }
    attr(result, 'prototypes') <- prototypes
  }

  if (relevanceoutput) {
    relevances <- list()
    for (i in 1:n) {
      relevances[[i]] <- attr(LVQlist[[i]], 'relevances')
    }

    attr(result, 'relevances') <- relevances
  }

  if (costfunction) {
    costcurve <- list()
    for (i in 1:n) {
      costcurve[[i]] <- attr(LVQlist[[i]], 'costcurve')
    }
    attr(result, 'costcurve') <- costcurve
  }

  if (progress) {
    protprog <- list()
    for (i in 1:n) {
      protprog[[i]] <- attr(LVQlist[[i]], 'protoprogress')
    }
    attr(result, 'prototprogress') <- protprog
  }

  if (relevanceprogress) {
    relprog <- list()
    for (i in 1:n) {
      rels <- vector()
      if (attr(result, 'nrofrelevances') > 1) {
	rels <- orderRelevances(attr(LVQlist[[i]], 'relevanceprogress'))
      } else {
	rels <- attr(LVQlist[[i]], 'relevanceprogress')
      }
      relprog[[i]] <- rels
    }
    attr(result, 'relevanceprogress') <- relprog
  }

  if (testerror) {
    missclassifications <- vector()
    for (i in 1:n) {
      missclassifications[i] <- attr(LVQlist[[i]], 'testerror')
    }
    attr(result, 'testerror') <- missclassifications
  }

  if (trainerror) {
    missclassifications <- vector()
    for (i in 1:n) {
      missclassifications[i] <- attr(LVQlist[[i]], 'trainerror')
    }
    attr(result, 'trainerror') <- missclassifications
  }

   if (trainerrorprogress) {
    trainerrors <- list()
    for (i in 1:n) {
      trainerrors[[i]] <- attr(LVQlist[[i]], 'trainerrorprogress')
    }
    attr(result, 'trainerrorprogress') <- trainerrors
  }

   if (testerrorprogress) {
    testerrors <- list()
    for (i in 1:n) {
      testerrors[[i]] <- attr(LVQlist[[i]], 'testerrorprogress')
    }
    attr(result, 'testerrorprogress') <- testerrors
  }

  result
}

orderRelevances <- function (relevancelist) {
  epochs <- length(relevancelist)
  nrofrelevances <- length(relevancelist[[1]])
  rellist <- list()
  length(rellist) <- nrofrelevances
  for (i in 1:nrofrelevances) {
    rellist[[i]] <- list()
    for (j in 1:epochs) {
      rellist[[i]][[j]] <- relevancelist[[j]][[i]]
    }
  }
  rellist
}

showAll <- function (LVQoutput) {
  prototypes <- FALSE
  relevances <- FALSE
  costcurve <- FALSE
  prototypeprogress <- FALSE
  relevanceprogress <- FALSE
  testerror <- FALSE
  trainerror <- FALSE
  testerrorprogress <- FALSE
  trainerrorprogress <- FALSE
  
  if (length(attr(LVQoutput, 'prototypes')) != 0) {
    prototypes <- TRUE
  }
  
  if (length(attr(LVQoutput, 'relevances')) != 0) {
    relevances <- TRUE
  }

  if (length(attr(LVQoutput, 'costcurve')) != 0) {
    costcurve <- TRUE
  }

  if (length(attr(LVQoutput, 'protoprogress')) != 0) {
    prototypeprogress <- TRUE
  }

  if (length(attr(LVQoutput, 'relevanceprogress')) != 0) {
    relevanceprogress <- TRUE
  }

  if (length(attr(LVQoutput, 'testerror')) != 0) {
    testerror <- TRUE
  }

  if (length(attr(LVQoutput, 'trainerror')) != 0) {
    trainerror <- TRUE
  }

  if (length(attr(LVQoutput, 'testerrorprogress')) != 0) {
    testerrorprogress <- TRUE
  }

  if (length(attr(LVQoutput, 'trainerrorprogress')) != 0) {
    trainerrorprogress <- TRUE
  }

  showStuff(LVQoutput = LVQoutput, prototypes = prototypes, relevances = relevances, costcurve = costcurve, prototypeprogress = prototypeprogress,
	relevanceprogress = relevanceprogress, trainerror = trainerror, testerror = testerror, trainerrorprogress = trainerrorprogress,
	testerrorprogress = testerrorprogress)
}

showStuff <- function (LVQoutput, prototypes = FALSE, relevances = FALSE, costcurve = FALSE, prototypeprogress = FALSE, relevanceprogress = FALSE,
		  trainerror = FALSE, testerror= FALSE, trainerrorprogress = FALSE, testerrorprogress = FALSE,
		  protofold = -1, relfold = -1, costfold = -1, protoprogfold = -1, relprogfold = -1, trainerrorprogfold = -1,
		  testerrorprogfold = -1, relevancenumber = -1, relevanceprognumber = -1) {

  checkShowVars(LVQoutput, prototypes, relevances, costcurve, prototypeprogress, relevanceprogress, trainerror, testerror, trainerrorprogress, testerrorprogress, relevancenumber, relevanceprognumber)

  if (class(LVQoutput) == 'trainoutput' | class(LVQoutput) == 'traintestoutput') {
    
    if (prototypes) {
      showEndPrototypes(attr(LVQoutput, 'prototypes'))
    }

    if (relevances) {
      showRelevancesWrap(attr(LVQoutput, 'relevances'), relevancenumber = relevancenumber)
    }

    if (costcurve) {
      showCostcurve(attr(LVQoutput, 'costcurve'))
    }

    if (prototypeprogress) {
      showPrototypeProgress(attr(LVQoutput, 'protoprogress'))
    }

    if (relevanceprogress) {
      showRelevanceProgressWrap(attr(LVQoutput, 'relevanceprogress'), relevanceprognumber = relevanceprognumber)
    }

    if(trainerror) {
      showTrainError(attr(LVQoutput, 'trainerror'))
    }

    if (trainerrorprogress) {
      showTrainerrorProgress(attr(LVQoutput, 'trainerrorprogress'))
    }

  }

  if (class(LVQoutput) == 'traintestoutput') {

    if (testerror) {
      showTestError(attr(LVQoutput, 'testerror'))
    }

    if (testerrorprogress) {
      showTesterrorProgress(attr(LVQoutput, 'testerrorprogress'))
    }

  }

  if (class(LVQoutput) == 'nfoldoutput') {

    checkShowNfoldVars(LVQoutput, protofold, relfold, costfold, protoprogfold, relprogfold, trainerrorprogfold, testerrorprogfold)

    if (prototypes) {
      showEndPrototypesNfold(attr(LVQoutput, 'prototypes'), protofold)
    }

    if (relevances) {
      showRelevancesNfold(attr(LVQoutput, 'relevances'), relfold, relevancenumber)
    }

    if (costcurve) {
      showCostcurveNfold(attr(LVQoutput, 'costcurve'), costfold)
    }

    if (prototypeprogress) {
      showPrototypeProgressNfold(attr(LVQoutput, 'protoprogress'), protoprogfold)
    }

    if (testerror) {
      showTrainErrorNfold(attr(LVQoutput, 'trainerror'))
    }

    if (testerror) {
      showTestErrorNfold(attr(LVQoutput, 'testerror'))
    }

    if (trainerrorprogress) {
      showTrainerrorProgressNfold(attr(LVQoutput, 'trainerrorprogress'), trainerrorprogfold)
    }

    if (testerrorprogress) {
      showTesterrorProgressNfold(attr(LVQoutput, 'testerrorprogress'), testerrorprogfold)
    }

    if (relevanceprogress) {
      showRelevanceProgressNfold(attr(LVQoutput, 'relevanceprogress'), relprogfold, relevancenumber)
    }
  }
}

showRelevancesWrap <- function (relevances, fold = NA, relevancenumber = NA) {
  if (class(relevances) == 'list') {
    if (relevancenumber == -1) {
      for (i in 1:length(relevances)) {
	showRelevances(relevances[[i]], fold, i)
      }
    } else {
      showRelevances(relevances[[relevancenumber]], fold, relevancenumber)
    }
  } else {
    showRelevances(relevances, fold)
  }
}

showRelevanceProgressWrap <- function (relevances, fold = NA, relevanceprognumber = NA) {
  if (class(relevances[[1]]) == 'list') {
    if (relevanceprognumber == -1) {
      for (i in 1:length(relevances)) {
	showRelevanceProgress(relevances[[i]], fold, i)
      }
    } else {
      showRelevanceProgress(relevances[[relevanceprognumber]], fold, relevanceprognumber)
    }
  } else {
    showRelevanceProgress(relevances, fold)
  }
}

showEndPrototypesNfold <- function (prototypelist, fold) {
  if (fold == -1) {
    for (i in 1:length(prototypelist)) {
      showEndPrototypes(prototypelist[[i]], i)
    }
  } else {
    showEndPrototypes(prototypelist[[fold]], fold)
  }
}

showPrototypeProgressNfold <- function (protoproglist, fold) {
  if (fold == -1) {
    for (i in 1:length(protoproglist)) {
      showPrototypeProgress(protoproglist[[i]], i)
    }
  } else {
    showPrototypeProgress(protoproglist[[fold]], fold)
  }
}

showTrainErrorNfold <- function (trainerror) {
  dev.new()
  lngth <- length(trainerror)
  names <- vector()
  names[1:lngth] <- 'fold'
  names <- paste(names, 1:lngth)
  mainlab <- 'trainerror by fold'
  barplot(trainerror, names.arg = names, xlab = 'folds', ylab = 'trainerror', main = mainlab)
}

showTestErrorNfold <- function (testerror) {
  dev.new()
  lngth <- length(testerror)
  names <- vector()
  names[1:lngth] <- 'fold'
  names <- paste(names, 1:lngth)
  mainlab <- 'testerror by fold'
  barplot(testerror, names.arg = names, xlab = 'folds', ylab = 'testerror', main = mainlab)
}

showRelevanceProgressNfold <- function (relproglist, relprogfold, relevancenumber) {
  if (relprogfold == -1) {
    for (i in 1:length(relproglist)) {
      showRelevanceProgressWrap(relproglist[[i]], i, relevancenumber)
    }
  } else {
    showRelevanceProgressWrap(relproglist[[relprogfold]], relevancenumber)
  }
}

showRelevancesNfold <- function (rellist, relfold, relevancenumber) {
  if (relfold == -1) {
    for (i in 1:length(rellist)) {
      showRelevancesWrap(rellist[[i]], i, relevancenumber)
    }
  } else {
    showRelevancesWrap(rellist[[relfold]], relfold, relevancenumber)
  }
}

showCostcurveNfold <- function (costlist, costfold) {
  if (costfold == -1) {
    for (i in 1:length(costlist)) {
      showCostcurve(costlist[[i]], i)
    }
  } else {
    showCostcurve(costlist[[costfold]], costfold)
  }
}

showTrainerrorProgressNfold <- function (errorlist, trainfold) {
  if (trainfold == -1) {
    for (i in 1:length(errorlist)) {
      showTrainerrorProgress(errorlist[[i]], i)
    }
  } else {
    showTrainerrorProgress(errorlist[[trainfold]], trainfold)
  }
}

showTesterrorProgressNfold <- function (errorlist, testfold) {
  if (testfold == -1) {
    for (i in 1:length(errorlist)) {
      showTesterrorProgress(errorlist[[i]], i)
    }
  } else {
    showTesterrorProgress(errorlist[[testfold]], testfold)
  }
}

showRelevances <- function (relevances, fold = NA, relevancenumber = NA) {
  if (!is.na(fold)) {
    fold <- paste('fold ', fold, ' ')
  } else {
    fold <- ''
  }
  if (!is.na(relevancenumber)) {
    relnum <- paste('relevances number ', relevancenumber)
  } else {
    relnum <- ''
  }
  dev.new()
  if (class(relevances) == 'numeric') {
    mainlab <- paste('endresult relevancevector ', fold, relnum)
    barplot(relevances, ylim = c(0,1), names.arg = 1:length(relevances), xlab = 'dimensions', main = mainlab)
  }
  if (class(relevances) == 'matrix') {
    dim <- dim(relevances)
    mainlab <- paste('endresult relevancematrix ', fold, relnum)
    grays <- gray(1000:1/1000)
    relevances[1:dim[1],] <- relevances[dim[1]:1,]
    image(z = t(relevances), col = grays, main = mainlab)
  }
}

showCostcurve <- function (costcurve, fold = NA) {
  if (!is.na(fold)) {
    fold <- paste('fold ', fold)
  } else {
    fold <- ''
  }
  dev.new()
  mainlab <- paste('progress costfunction ', fold)
  plot(y = costcurve, x = 1:length(costcurve), type = "l", xlab = 'epochs', ylab = 'cost', main = mainlab)
}

showRelevanceProgress <- function (rellist, fold = NA, relevancenumber = NA) {
  if (!is.na(fold)) {
    fold <- paste('fold ', fold, ' ')
  } else {
    fold <- ''
  }
  if (!is.na(relevancenumber)) {
    relnum <- paste('relevances number ', relevancenumber)
  } else {
    relnum <- ''
  }
  dev.new()
  add = TRUE
  clss <- class(rellist[[1]])
  names <- vector()
  grays <- gray(1000:1/1000)
  for (i in 1:length(rellist)) {

    mainlab <- paste('relevanceprogress epoch: ', i-1, ' ', fold, relnum)
    
    if (clss == 'numeric') {
      barplot(rellist[[i]], ylim = c(0,1), names.arg = 1:length(rellist[[i]]), xlab = 'dimensions', main = mainlab)
    }
    if (clss == 'matrix') {
      dim <- dim(rellist[[i]])
      rellist[[i]][1:dim[1],] <- rellist[[i]][dim[1]:1,]
      image(z = t(rellist[[i]]), col = grays, main = mainlab)
    }

  }
  add = FALSE
}

showTrainerrorProgress <- function (trainerrors, fold = NA) {
  if (!is.na(fold)) {
    fold <- paste('fold ', fold)
  } else {
    fold <- ''
  }
  dev.new()
  mainlab <- paste('progress trainerror ', fold)
  plot(y = trainerrors, x = 1:length(trainerrors), type = "l", xlab = 'epochs', ylab = 'error', main = mainlab)
}

showTesterrorProgress <- function (testerrors, fold = NA) {
  if (!is.na(fold)) {
    fold <- paste('fold ', fold)
  } else {
    fold <- ''
  }
  dev.new()
  mainlab <- paste('progress testerror ', fold)
  plot(y = testerrors, x = 1:length(testerrors), type = "l", xlab = 'epochs', ylab = 'error', main = mainlab)
}

showEndPrototypes <- function (prototypes, fold = NA) {
  if (!is.na(fold)) {
    cat('fold ', fold, '\n')
  }
  print('prototypes endconfiguration:')
  print(prototypes)
}

showPrototypeProgress <- function (protolist, fold = NA) {
  if (!is.na(fold)) {
    cat('fold ', fold, '\n')
  }
  print('prototypes initialisation:')
  print(protolist[[1]])
  for (i in 2:length(protolist)) {
    cat('prototype configuration after epoch', (i-1), '\n')
    print(protolist[[i]])
  }
}

showTrainError <- function (trainerror, fold = NA) {
  if (!is.na(fold)) {
    cat('fold ', fold, '\n')
  }
  print('Ratio of errors when testing on the trainingset:')
  print(trainerror)
}

showTestError <- function (testerror, fold = NA) {
  if (!is.na(fold)) {
    cat('fold ', fold, '\n')
  }
  print('Ratio of errors when testing on the testset:')
  print(testerror)
}

getPrototypes <- function (LVQout, fold = NA) {
  result <- vector()
  if (!is.na(fold)) {
    checkGetVars(LVQout, fold)
    if (fold == -1) {
      result <- attr(LVQout, 'prototypes')
    } else {
      result <- attr(LVQout, 'prototypes')[[fold]]
    }
  } else {
    result <- attr(LVQout, 'prototypes')
  }
  result
}

getRelevances <- function (LVQout, fold = NA) {
  result <- vector()
  if (!is.na(fold)) {
    checkGetVars(LVQout, fold)
    if (fold == -1) {
      result <- attr(LVQout, 'relevances')
    } else {
      result <- attr(LVQout, 'relevances')[[fold]]
    }
  } else {
    result <- attr(LVQout, 'relevances')
  }
  result
}

getPrototypeProgress <- function (LVQout, fold = NA) {
  result <- vector()
  if (!is.na(fold)) {
    checkGetVars(LVQout, fold)
    if (fold == -1) {
      result <- attr(LVQout, 'protoprogress')
    } else {
      result <- attr(LVQout, 'protoprogress')[[fold]]
    }
  } else {
    result <- attr(LVQout, 'protoprogress')
  }
  result
}

getRelevanceProgress <- function (LVQout, fold = NA) {
  result <- vector()
  if (!is.na(fold)) {
    checkGetVars(LVQout, fold)
    if (fold == -1) {
      result <- attr(LVQout, 'relevanceprogress')
    } else {
      result <- attr(LVQout, 'relevanceprogress')[[fold]]
    }
  } else {
    result <- attr(LVQout, 'relevanceprogress')
  }
  result
}

getCostcurve <- function (LVQout, fold = NA) {
  result <- vector()
  if (!is.na(fold)) {
    checkGetVars(LVQout, fold)
    if (fold == -1) {
      result <- attr(LVQout, 'costcurve')
    } else {
      result <- attr(LVQout, 'costcurve')[[fold]]
    }
  } else {
    result <- attr(LVQout, 'costcurve')
  }
  result
}

getTrainError <- function (LVQout, fold = NA) {
  result <- vector()
  if (!is.na(fold)) {
    checkGetVars(LVQout, fold)
    if (fold == -1) {
      result <- attr(LVQout, 'trainerror')
    } else {
      result <- attr(LVQout, 'trainerror')[[fold]]
    }
  } else {
    result <- attr(LVQout, 'trainerror')
  }
  result
}

getTestError <- function (LVQout, fold = NA) {
  result <- vector()
  if (!is.na(fold)) {
    checkGetVars(LVQout, fold)
    if (fold == -1) {
      result <- attr(LVQout, 'testerror')
    } else {
      result <- attr(LVQout, 'testerror')[[fold]]
    }
  } else {
    result <- attr(LVQout, 'testerror')
  }
  result
}

getTrainErrorProgress <- function (LVQout, fold = NA) {
  result <- vector()
  if (!is.na(fold)) {
    checkGetVars(LVQout, fold)
    if (fold == -1) {
      result <- attr(LVQout, 'trainerrorprogress')
    } else {
      result <- attr(LVQout, 'trainerrorprogress')[[fold]]
    }
  } else {
    result <- attr(LVQout, 'trainerrorprogress')
  }
  result
}

getTestErrorProgress <- function (LVQout, fold = NA) {
  result <- vector()
  if (!is.na(fold)) {
    checkGetVars(LVQout, fold)
    if (fold == -1) {
      result <- attr(LVQout, 'testerrorprogress')
    } else {
      result <- attr(LVQout, 'testerrorprogress')[[fold]]
    }
  } else {
    result <- attr(LVQout, 'testerrorprogress')
  }
  result
}