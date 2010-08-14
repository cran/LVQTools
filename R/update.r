updateRelevancesWrap <- function(protolabels, winner, winclass, winnotclass, relevances, dataclass, relrate, difference,
				  distance, optimisationscheme, relevancescheme, relevancemode, customdist) {

  relevances <- switch(EXPR = optimisationscheme,
			      normal = updateRelevancesNormalWrap(protolabels, winner,relevances, dataclass, relrate, difference,
								    distance, relevancescheme, relevancemode, customdist),
			      general = updateRelevancesGeneralWrap(protolabels, winclass, winnotclass, relevances, dataclass, relrate, difference,
								    distance, relevancescheme, relevancemode, customdist))

  relevances
}



updateRelevancesNormalWrap <- function (protolabels, winner,relevances, dataclass, relrate, difference,
					distance, relevancescheme, relevancemode, customdist) {
  relevances <- switch(EXPR = relevancescheme,
			      global = updateNormalGlobalWrap(protolabels, winner,relevances, dataclass, relrate, difference,
							      distance, relevancemode, customdist),
			      local = updateNormalLocalWrap(protolabels, winner,relevances, dataclass, relrate, difference,
							      distance, relevancemode, customdist),
			      classwise = updateNormalClassWrap(protolabels, winner,relevances, dataclass, relrate, difference,
								distance, relevancemode, customdist))
  relevances
}

updateRelevancesGeneralWrap <- function (protolabels, winclass, winnotclass, relevances, dataclass, relrate, difference,
					  distance, relevancescheme, relevancemode, customdist) {

  relevances <- switch(EXPR = relevancescheme,
			global = updateGeneralGlobalWrap(protolabels, winclass, winnotclass, relevances, dataclass, relrate, difference,
							  distance, relevancemode, customdist),
			local = updateGeneralLocalWrap(protolabels, winclass, winnotclass, relevances, dataclass, relrate, difference,
							  distance, relevancemode, customdist),
			classwise = updateGeneralClassWrap(protolabels, winclass, winnotclass, relevances, dataclass, relrate, difference,
							    distance, relevancemode, customdist))
  relevances
}

updateNormalGlobalWrap <- function(protolabels, winner, relevances, dataclass, relrate, difference, distance, relevancemode, customdist) {
  relevances <- switch(EXPR = relevancemode,
			normal = relevances,
			relevance = updateRelevances(protolabels[winner], relevances, dataclass, relrate, difference[winner,], customdist),
			matrix = updateMatrix(protolabels[winner], relevances, dataclass, relrate, difference[winner,]))

  relevances
}

updateGeneralGlobalWrap <- function(protolabels, winclass, winnotclass, relevances, dataclass, relrate, difference,
				    distance, relevancemode, customdist) {

  relevances <- switch (EXPR = relevancemode,
			normal = relevances,
			relevance = updateRelevancesGeneral(relevances, difference, distance, winclass, winnotclass, relrate, customdist),
			matrix = updateMatrixGeneral(relevances, difference, distance, winclass, winnotclass, relrate, customdist))
  relevances
}

updateNormalLocalWrap <- function (protolabels, winner, relevances, dataclass, relrate, difference, distance, relevancemode, customdist) {

  relevances[[winner]] <- switch(EXPR = relevancemode,
			normal = relevances,
			relevance = updateRelevances(protolabels[winner], relevances[[winner]], dataclass, relrate, difference[winner,], customdist),
			matrix = updateMatrix(protolabels[winner], relevances[[winner]], dataclass, relrate, difference[winner,]))
  relevances
}

updateGeneralLocalWrap <- function (protolabels, winclass, winnotclass, relevances, dataclass, relrate, difference,
				      distance, relevancemode, customdist) {

  relevances <- switch(EXPR = relevancemode,
			  normal = relevances,
			  relevance = updateGeneralLocalRelevances(relevances, difference, distance, winclass, winnotclass, relrate, customdist),
			  matrix = updateGeneralLocalMatrix(relevances, difference, distance, winclass, winnotclass, relrate, customdist))
  relevances
}

updateNormalClassWrap <- function (protolabels, winner, relevances, dataclass, relrate, difference, distance, relevancemode, customdist) {
  relevances[[protolabels[winner]]] <- switch(EXPR = relevancemode,
						normal = relevances,
						relevance = updateRelevances(protolabels[winner], relevances[[protolabels[winner]]], dataclass, relrate, difference[winner,], customdist),
						matrix = updateMatrix(protolabels[winner], relevances[[protolabels[winner]]], dataclass, relrate, difference[winner,]))
  relevances
}

updateGeneralClassWrap <- function (protolabels, winclass, winnotclass, relevances, dataclass, relrate, difference,
					distance, relevancemode, customdist) {
  relevances <- switch(EXPR = relevancemode,
				normal = relevances,
				relevance = updateGeneralClassRelevances(relevances, protolabels, difference, distance, winclass, winnotclass, relrate, customdist),
				matrix = updateGeneralClassMatrix(relevances, protolabels, difference, distance, winclass, winnotclass, relrate, customdist))

}

updateGeneralClassRelevances <- function (relevances, protolabels, difference, distance, winclass, winnotclass, relrate, customdist) {

  classupd <- - relrate * 2*distance[winnotclass]/((distance[winclass]+distance[winnotclass])^2) * difference[winclass,]^customdist

  notclassupd <- - relrate * -2*distance[winclass]/((distance[winclass]+distance[winnotclass])^2) * difference[winnotclass,]^customdist
  
  relevances[[protolabels[winclass]]] <- relevances[[protolabels[winclass]]] + ifelse(is.na(classupd), 0, classupd)
  relevances[[protolabels[winnotclass]]] <- relevances[[protolabels[winnotclass]]] + ifelse(is.na(notclassupd), 0, notclassupd)

  relevances[[protolabels[winclass]]] <- ifelse(relevances[[protolabels[winclass]]] < 0, 0, relevances[[protolabels[winclass]]])
  relevances[[protolabels[winnotclass]]] <- ifelse(relevances[[protolabels[winnotclass]]] < 0, 0, relevances[[protolabels[winnotclass]]])

  relevances[[protolabels[winclass]]] <- relevances[[protolabels[winclass]]]/sum(relevances[[protolabels[winclass]]])
  relevances[[protolabels[winnotclass]]] <- relevances[[protolabels[winnotclass]]]/sum(relevances[[protolabels[winnotclass]]])

  relevances
}

updateGeneralClassMatrix <- function (relevances, protolabels, difference, distance, winclass, winnotclass, relrate, customdist) {
  dimensions <- length(relevances[[1]][,1])
  for (m in 1:dimensions) {
    classupd <- 2*distance[winnotclass]/((distance[winclass]+distance[winnotclass])^customdist)
    classupd <- classupd * 2 * relevances[[protolabels[winclass]]][m,] * difference[winclass,] * difference[winclass,]

    notclassupd <- -2 * distance[winclass]/((distance[winclass]+distance[winnotclass])^customdist)
    notclassupd <- notclassupd * 2 * relevances[[protolabels[winnotclass]]][m,] * difference[winnotclass,] * difference[winnotclass,]

    classupd <- -relrate * classupd
    notclassupd <- -relrate * notclassupd

    classupd <- ifelse(is.na(classupd), 0, classupd)
    notclassupd <- ifelse(is.na(notclassupd), 0, notclassupd)

    relevances[[protolabels[winclass]]][m,] <- relevances[[protolabels[winclass]]][m,] + classupd
    relevances[[protolabels[winnotclass]]][m,] <- relevances[[protolabels[winnotclass]]][m,] + notclassupd
  }
  
  relevances[[protolabels[winclass]]] <- normalizeMatrix(relevances[[protolabels[winclass]]])
  relevances[[protolabels[winnotclass]]] <- normalizeMatrix(relevances[[protolabels[winnotclass]]])

  relevances
}

updateGeneralLocalRelevances <- function (relevances, difference, distance, winclass, winnotclass, relrate, customdist) {
  classupd <- - relrate * 2*distance[winnotclass]/((distance[winclass]+distance[winnotclass])^2) * difference[winclass,]^customdist

  notclassupd <- - relrate * -2*distance[winclass]/((distance[winclass]+distance[winnotclass])^2) * difference[winnotclass,]^customdist
  
  relevances[[winclass]] <- relevances[[winclass]] + ifelse(is.na(classupd), 0, classupd)
  relevances[[winnotclass]] <- relevances[[winnotclass]] + ifelse(is.na(notclassupd), 0, notclassupd)

  relevances[[winclass]] <- ifelse(relevances[[winclass]] < 0, 0, relevances[[winclass]])
  relevances[[winnotclass]] <- ifelse(relevances[[winnotclass]] < 0, 0, relevances[[winnotclass]])

  relevances[[winclass]] <- relevances[[winclass]]/sum(relevances[[winclass]])
  relevances[[winnotclass]] <- relevances[[winnotclass]]/sum(relevances[[winnotclass]])

  relevances
}

updateGeneralLocalMatrix <- function (relevances, difference, distance, winclass, winnotclass, relrate, customdist) {
  dimensions <- length(relevances[[1]][,1])
  for (m in 1:dimensions) {
    classupd <- 2*distance[winnotclass]/((distance[winclass]+distance[winnotclass])^customdist)
    classupd <- classupd * 2 * relevances[[winclass]][m,] * difference[winclass,] * difference[winclass,]

    notclassupd <- -2 * distance[winclass]/((distance[winclass]+distance[winnotclass])^customdist)
    notclassupd <- notclassupd * 2 * relevances[[winnotclass]][m,] * difference[winnotclass,] * difference[winnotclass,]

    classupd <- -relrate * classupd
    notclassupd <- -relrate * notclassupd

    classupd <- ifelse(is.na(classupd), 0, classupd)
    notclassupd <- ifelse(is.na(notclassupd), 0, notclassupd)

    relevances[[winclass]][m,] <- relevances[[winclass]][m,] + classupd
    relevances[[winnotclass]][m,] <- relevances[[winnotclass]][m,] + notclassupd
  }
  
  relevances[[winclass]] <- normalizeMatrix(relevances[[winclass]])
  relevances[[winnotclass]] <- normalizeMatrix(relevances[[winnotclass]])

  relevances
}

updateRelevancesGeneral <- function (relevances, difference, distance, winclass, winnotclass, relrate, customdist) {
  classupd <- - relrate * 2*distance[winnotclass]/((distance[winclass]+distance[winnotclass])^2) * difference[winclass,]^customdist

  notclassupd <- - relrate * -2*distance[winclass]/((distance[winclass]+distance[winnotclass])^2) * difference[winnotclass,]^customdist
  
  relevances <- relevances + ifelse(is.na(classupd), 0, classupd) + ifelse(is.na(notclassupd), 0, notclassupd)


  relevances <- ifelse(relevances < 0, 0, relevances)
  relevances <- relevances/sum(relevances)
  relevances
}

updateMatrixGeneral <- function(relmat, difference, distance, winclass, winnotclass, relrate, customdist) {
  dimensions <- length(relmat[,1])
  for (m in 1:dimensions) {
    classupd <- 2*distance[winnotclass]/((distance[winclass]+distance[winnotclass])^customdist)
    classupd <- classupd * 2 * relmat[m,] * difference[winclass,] * difference[winclass,]

    notclassupd <- -2 * distance[winclass]/((distance[winclass]+distance[winnotclass])^customdist)
    notclassupd <- notclassupd * 2 * relmat[m,] * difference[winnotclass,] * difference[winnotclass,]

    upd <- -relrate * (classupd + notclassupd)

    relmat[m,] <- relmat[m,] + ifelse(is.na(upd), 0, upd)
  }
  
  relmat <- normalizeMatrix(relmat)
  relmat
}

updateMatrix <- function(prototypeclass, relmat, dataclass, relrate, difference) {
  correct <- prototypeclass == dataclass
  for(i in 1:length(difference)) {
    updatedifference <- abs(difference[i]) * abs(difference)
    updvec <- vector()
    if(correct) {
      updvec <- -relrate * difference
    } else {
      updvec <- relrate * difference
    }
  }
  relmat[i,] <- relmat[i,] + ifelse(is.na(updvec), 0, updvec)
  relmat <- normalizeMatrix(relmat)
  relmat
}

updateRelevances <- function(prototypeclass, relvec, dataclass, relrate, difference, customdist) {
  correct <- prototypeclass == dataclass
  updvec <- vector()
  if (correct) {
    updvec <- -relrate * abs(difference)^customdist
  } else {
    updvec <- relrate * abs(difference)^customdist
  }
  updvec <- ifelse(is.na(updvec), 0, updvec)
  relvec <- relvec + updvec
  relvec <- ifelse(relvec < 0, 0, relvec)
  relvec <- relvec/sum(relvec)
  relvec
}

LVQ1Update <- function (optimisationscheme, protomatrix, protolabels, winner, winclass, winnotclass, dataclass, difference, learningrate, dist, customdist, relevances, relevancemode, relevancescheme) {

  relevances1 <- switch (EXPR = relevancescheme, global = relevances, local = relevances[[winclass]], classwise = relevances[[protolabels[winclass]]])
  relevances2 <- switch (EXPR = relevancescheme, global = relevances, local = relevances[[winnotclass]], classwise = relevances[[protolabels[winnotclass]]])

  protomatrix <- switch (EXPR = optimisationscheme, normal = update(protomatrix, protolabels, winner, dataclass,
								      difference[winner,], learningrate),
						general = generalUpdate(protomatrix, protolabels, winclass, winnotclass,
									difference[winclass,], difference[winnotclass,], dist[winclass],
									dist[winnotclass], learningrate, customdist, relevances1, relevances2,
									relevancemode))

  protomatrix
}


# update function according to LVQ1
update <- function (protomatrix, protolabels, winnerindex, dataclass, difference, learningrate) {
  upd <- learningrate * difference
  newprototype <- protomatrix[winnerindex,]
  if (protolabels[winnerindex] == dataclass) {
    newprototype <- ifelse(is.na(upd), newprototype, newprototype + upd)
  } else {
    newprototype <- ifelse(is.na(upd), newprototype, newprototype - upd)
  }
  protomatrix[winnerindex,] <- newprototype
  protomatrix
}

generalUpdate <- function (protomatrix, protolabels, winclass, winnotclass, diffclass, diffnotclass, distclass, distnotclass, learningrate,
			      customdist, classrelevances, notclassrelevances, relevancemode) {
  classrelmat <- matrix()
  notclassrelmat <- matrix()
  if (relevancemode == 'matrix') {
    classrelmat <- classrelevances
    notclassrelmat <- notclassrelevances
    classrelmat <- t(classrelmat) %*% classrelmat
    notclassrelmat <- t(notclassrelmat) %*% notclassrelmat
  }

  newprotclass <- protomatrix[winclass,]
  newprotnotclass <- protomatrix[winnotclass,]

  classupd <- switch(EXPR = relevancemode,	normal = learningrate * 2*distnotclass/((distclass+distnotclass)^2) * customdist * abs(diffclass)^(customdist-1) * sign(diffclass),
						relevance = learningrate * 2*distnotclass/((distclass+distnotclass)^2) * customdist * classrelevances * abs(diffclass)^(customdist-1) * sign(diffclass),
						matrix = learningrate * 2*distnotclass/((distclass+distnotclass)^2) * customdist * classrelmat %*% diffclass^(customdist-1))
  
  newprotclass <- ifelse(is.na(classupd), newprotclass, newprotclass + classupd)

  notclassupd <- switch(EXPR = relevancemode,	normal = learningrate * -2*distclass/((distclass+distnotclass)^2) * customdist * abs(diffnotclass)^(customdist-1) * sign(diffnotclass),
						relevance = learningrate * -2*distclass/((distclass+distnotclass)^2) * customdist * notclassrelevances * abs(diffnotclass)^(customdist-1) * sign(diffnotclass),
						matrix = learningrate * -2*distclass/((distclass+distnotclass)^2) * customdist * notclassrelmat %*% diffnotclass^(customdist-1))

  newprotnotclass <- ifelse(is.na(notclassupd), newprotnotclass, newprotnotclass + notclassupd)

  protomatrix[winclass, ] <- newprotclass
  protomatrix[winnotclass, ] <- newprotnotclass

  protomatrix
}

csUpdate <- function (optimisationscheme, protomatrix, protolabels, winner, winclass, winnotclass, datapoint, dataclass, learningrate, dist) {
  protomatrix <- switch(EXPR = optimisationscheme, normal = csNormalUpdate(protomatrix, protolabels, winner, datapoint, dataclass, learningrate),
						   general = csGeneralUpdate(protomatrix, winclass, winnotclass, datapoint, dist, learningrate)) 

  protomatrix
}

renyiUpdate <- function (optimisationscheme, protomatrix, protolabels, winner, winclass, winnotclass, datapoint, dataclass, learningrate, dist, alfa) {
  protomatrix <- switch(EXPR = optimisationscheme, normal = renyiNormalUpdate(protomatrix, protolabels, winner, datapoint, dataclass, learningrate, alfa),
						   general = renyiGeneralUpdate(protomatrix, winclass, winnotclass, datapoint, dist, learningrate, alfa)) 

  protomatrix
}

csNormalUpdate <- function (protomatrix, protolabels, winner, datapoint, dataclass, learningrate) {
  prototype <- protomatrix[winner,]
  protlabel <- protolabels[winner]

  upd <- learningrate * (prototype/sum(prototype^2) - datapoint/sum(datapoint*prototype, na.rm = T))
  upd <- ifelse(is.na(upd), 0, upd)
  if (protlabel == dataclass) {
    prototype <- prototype - upd
  } else {
    prototype <- prototype + upd
  }
  protomatrix[winner,] <- prototype
  protomatrix[winner,] <- entropyNormalize(protomatrix[winner,], 0)
  protomatrix
}

csGeneralUpdate <- function (protomatrix, winclass, winnotclass, datapoint, dist, learningrate) {
  protclass <- protomatrix[winclass,]
  protnotclass <- protomatrix[winnotclass,]

  classupd <- -learningrate * (2 * dist[winnotclass] / (dist[winclass]+dist[winnotclass])^2) * (protclass/sum(protnotclass^2) - datapoint/sum(datapoint*protclass, na.rm = TRUE))
  notclassupd <- -learningrate * (-2 * dist[winclass] / (dist[winclass]+dist[winnotclass])^2) * (protnotclass/sum(protclass^2) - datapoint/sum(datapoint*protnotclass, na.rm = TRUE))

  classupd <- ifelse(is.na(classupd), 0, classupd)
  notclassupd <- ifelse(is.na(notclassupd), 0, notclassupd)

  protclass <- protclass + classupd
  protnotclass <- protnotclass + notclassupd

  protomatrix[winclass, ] <- protclass
  protomatrix[winnotclass, ] <- protnotclass

  protomatrix[winclass, ] <- entropyNormalize(protomatrix[winclass, ], 0)
  protomatrix[winnotclass, ] <- entropyNormalize(protomatrix[winnotclass, ], 0)

  protomatrix
}

renyiNormalUpdate <- function (protomatrix, protolabels, winner, datapoint, dataclass, learningrate, alfa) {
  prototype <- protomatrix[winner,]
  protlabel <- protolabels[winner]
  upd <- learningrate * 1/(alfa-1) * -(alfa-1) * ((datapoint/prototype)^alfa)/ sum(datapoint*(datapoint/prototype)^(alfa-1), na.rm=TRUE)
  upd <- ifelse(is.na(upd), 0, upd)
  if (protlabel == dataclass) {
    prototype <- prototype - upd
  } else {
    prototype <- prototype + upd
  }
  protomatrix[winner,] <- prototype

  protomatrix[winner,] <- entropyNormalize(protomatrix[winner,], 1e-3)

  protomatrix
}

renyiGeneralUpdate <- function (protomatrix, winclass, winnotclass, datapoint, dist, learningrate, alfa) {
  protclass <- protomatrix[winclass,]
  protnotclass <- protomatrix[winnotclass,]

  classupd <- -learningrate * (2 * dist[winnotclass] / (dist[winclass]+dist[winnotclass])^2) * 1/(alfa-1) * -(alfa-1) * ((datapoint/protclass)^alfa)/ sum(datapoint*(datapoint/protclass)^(alfa-1), na.rm=TRUE)
  notclassupd <- -learningrate * (-2 * dist[winclass] / (dist[winclass]+dist[winnotclass])^2) * 1/(alfa-1) * -(alfa-1) * ((datapoint/protnotclass)^alfa)/ sum(datapoint*(datapoint/protnotclass)^(alfa-1), na.rm=TRUE)

  classupd <- ifelse(is.na(classupd), 0, classupd)
  notclassupd <- ifelse(is.na(notclassupd), 0, notclassupd)

  protclass <- protclass + classupd
  protnotclass <- protnotclass + notclassupd

  protomatrix[winclass, ] <- protclass
  protomatrix[winnotclass, ] <- protnotclass

  protomatrix[winclass, ] <- entropyNormalize(protomatrix[winclass, ], 1e-3)
  protomatrix[winnotclass, ] <- entropyNormalize(protomatrix[winnotclass, ], 1e-3)

  protomatrix
}

normalizeMatrix <- function (matrix) {
  matrix <- matrix/sqrt(sum(matrix^2))
  matrix
}

entropyNormalize <- function (prototype, lowerlimit) {

  dimensions <- length(prototype)
  nrbelowlimit <- sum(prototype <= lowerlimit)
  belowlimit <- prototype <= lowerlimit
  abovelimit <- prototype > lowerlimit
  normalizesum <- sum(prototype[abovelimit]) / (1-(nrbelowlimit * lowerlimit))

  prototype[belowlimit] <- lowerlimit
  prototype[abovelimit] <- prototype[abovelimit] / normalizesum

  prototype
}