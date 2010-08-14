input <- function(datapath, normalizescheme = 'none', normalclasswise = 'none', replaceNA = FALSE, replaceclasswise = FALSE, input = NA) {

  data <- matrix()
  originallabels <- vector()
  lvls <- vector()
  dimensions <- vector()

  if (all(is.na(input)) & !is.na(datapath)) {

    frame <- read.table(datapath)
    data <- data.matrix(frame)
    dimensions <- length(data[1,]) - 1
    data[, dimensions + 1] <- transformlabels(data[, dimensions + 1])
    dimensions <- length(data[1,]) - 1
    originallabels <- as.character(frame[,dimensions+1])
    lvls <- levels(factor(originallabels))
    ordorlabs <- lvls[order(lvls)]
    classlabels <- data[,dimensions+1]

  } else if (!all(is.na(input))) {

    dimensions <- length(input[1,]) - 1
    nrdatapoints <- length(input[,1])
    dimensions <- length(input[1,]) - 1
    originallabels <- input[,dimensions+1]
    lvls <- levels(factor(originallabels))
    labels <- transformlabels(data.matrix(data.frame(input))[,dimensions+1])
    dat <- as.numeric(input[,1:dimensions])
    data <- array(c(dat,labels), dim = c(nrdatapoints,dimensions+1))

  }

  checkInputVars(normalizescheme, normalclasswise, replaceNA, replaceclasswise, lvls)

  if (replaceNA) {
    data[,1:dimensions] <- replaceNAwrap(data[,1:dimensions], replaceclasswise, classlabels)
  }
  
  if (!all(is.na(data))) {
    data[,1:dimensions] <- normalize(data[,1:dimensions], data[,dimensions+1], normalizescheme, normalclasswise, ordorlabs)
  }

  inp <- new('input')
  attr(inp,'data') <- data
  attr(inp,'labels') <- originallabels
  inp
}

normalize <- function(data, labels, normalizescheme = 'ztransform', classwise = 'none', ordorlabs) {
  
  normalclass <- 'none'
  if (classwise != 'none'){
    normalclass <- which.max(ordorlabs == classwise)
  }

  data <- switch(EXPR = normalizescheme,

	  none = data,
	  ztransform = ztransform(data, labels, normalclass),
	  iqr = iqrnorm(data, labels, normalclass),
	  sumone = sumonenorm(data))
  data
}

sumonenorm <- function(data) {
  data <- data/rowSums(data, na.rm = TRUE)
  data
}

ztransform <- function(data, labels, normalclass) {
  variance <- vector()
  if (normalclass == 'none') {
    mean <- colMeans(data, na.rm = TRUE)
    for(i in 1:length(data[1,])) {
      variance[i] <- var(data[,i], na.rm = TRUE)
    }
  } else {
    mean <- colMeans(data[labels == normalclass,], na.rm = TRUE)
    for(i in 1:length(data[1,])) {
      variance[i] <- var(data[labels == normalclass,i], na.rm = TRUE)
    }
  }
  data <- t((t(data)-mean)/variance)
  data
}

iqrnorm <- function(data, labels, normalclass) {
  median <- vector()
  iqr <- vector()
  if (normalclass == 'none') {
    for(i in 1:length(data[1,])) {
      median[i] <- median(data[,i], na.rm = TRUE)
      iqr[i] <- IQR(data[,i], na.rm = TRUE)
    }
  } else {
    for(i in 1:length(data[1,])) {
      median[i] <- median(data[labels == normalclass,i], na.rm = TRUE)
      iqr[i] <- IQR(data[labels == normalclass,i], na.rm = TRUE)
    }
  }
  data <- t((t(data)-median)/iqr)
  data
}

replaceNAwrap <- function(data, classwise, classlabels) {
  if(classwise) {
    nrclasses <- length(levels(factor(classlabels)))
    for (i in 1:nrclasses) {
      data[classlabels == i,] <- replaceNA(data[classlabels == i,])
    }
  } else {
    data <- replaceNA(data)
  }
  data
}

replaceNA <- function(data) {
  mean <- colMeans(data, na.rm = TRUE)
  for (i in 1:length(data[1,])) {
    data[is.na(data[,i]),i] <- mean[i]
  }
  data
}

transformlabels <- function (classlabels) {
  lvls <- levels(factor(classlabels))
  newclasslabels <- vector()
  for (i in 1:length(lvls)) {
    indices <- classlabels == lvls[i]
    newclasslabels[indices] <- i
  }
  newclasslabels
}