# sort data according to class for fast displaying, each entry of the output-list contains a matrix with samples of exactly one class
sortData <- function (data, labels) {
	lst <- list()
	numlabels <- as.numeric(levels(factor(labels)))
	for (i in 1:length(numlabels)) {
	  lst[[i]] <- vector()
	}
	datadim <- dim(data)
	dimensions <- datadim[2]
	nrdatapoints <- datadim[1]
	for (i in 1:nrdatapoints) {
		class <- labels[i]
		lst[[class]] <- append(lst[[class]], data[i,])
	}
	for (i in 1:length(lst)) {
		amount <- length(lst[[i]])/dimensions
		lst[[i]] <- t(array(lst[[i]], dim=c(dimensions, amount)))
	}
	lst
}

# plots training-data and prototypes
plotData <- function (sorteddata, prototypes, protolabels, data, labels) {

	protodim <- dim(prototypes)
	datadim <- dim(data)
	dimensions <- datadim[2]

	#colorvectors for plotting of datasets and prototypes of up to 7 different clusters and prototypeclasses
	colorvector <- c("purple", "yellow", "blue", "brown", "cyan", "darkgreen", "darkmagenta")
	protocolorvector <- c("darkorange1", "black", "chocolate", "darkblue", "cornflowerblue", "darkgray", "darkolivegreen1")	

	#prepare the axis
	plot(data, type="n")
	add = TRUE
	#plot the data
	for (i in 1:length(sorteddata)) {
		points(sorteddata[[i]], col=colorvector[i])
	}
	#plot prototypes
	for (i in 1:protodim[1]) {
		points(prototypes[i,1], prototypes[i,2], col=protocolorvector[protolabels[i]], pch=19)
	}
	add = FALSE
}

# shows results by plotting and printing data
showResults <- function (protomatrix, protolabels, epoch, sorteddata, data, labels, originallabels, costcurve, relevances, dimensions, graphics, show, relevancemode, relevancescheme, costfunction){
  if(relevancemode == 'matrix') {
    if (relevancescheme == 'global') {
      relevances <- t(relevances) %*% relevances
    } else if (any(relevancescheme == c('local', 'classwise'))) {
      for (i in 1:length(relevances)) {
	relevances[[i]] <- t(relevances[[i]]) %*% relevances[[i]]
      }
    }
  }
  if(dimensions == 2 & graphics) {
    plotData(sorteddata, protomatrix, protolabels, data, labels)
  }
  if (show) {
    protomatrix <- attachLabels(protomatrix, protolabels, originallabels)
    print("epoch")
    print(epoch)
    print("prototype configuration")
    print(protomatrix)
    
  if(relevancemode == 'relevance' | relevancemode == 'matrix') {
      print("relevances")
      print(relevances)
    }
    if (costfunction) {
      print("costcurve")
      print(costcurve)
    }
  }
}

showEndResults <- function (protomatrix, protolabels, originallabels, relevances, costcurve, epochs, plotcurve, show, relevancemode, relevancescheme) {
  if(relevancemode == 'matrix') {
    if (relevancescheme == 'global') {
      relevances <- t(relevances) %*% relevances
    } else if (any(relevancescheme == c('local', 'classwise'))) {
      for (i in 1:length(relevances)) {
	relevances[[i]] <- t(relevances[[i]]) %*% relevances[[i]]
      }
    }
  }

  if (show) {
    print("prototype endconfiguration")
    protomatrix <- attachLabels(protomatrix, protolabels, originallabels)
    print(protomatrix)
    if (relevancemode == 'relevance' | relevancemode == 'matrix') {
      print("end relevances")
      print(relevances)
    }
  }
  if (plotcurve) {
	dev.new()
  	plot(y = costcurve, x = epochs, type = "l")
  }
}

attachLabels <- function(protomatrix, protolabels, originallabels) {
  protodim <- dim(protomatrix)
  sortedlvls <- sort(levels(factor(originallabels)))
  labels <- sortedlvls[protolabels]
  protomatrix <- array(append(protomatrix, labels), dim=c(protodim[1], protodim[2]+1))
  protomatrix
}