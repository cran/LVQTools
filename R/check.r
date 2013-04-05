checkInputVars <- function (normalizescheme, normalclasswise, replaceNA, replaceclasswise, orglabellvls) {
  errors <- vector()

  if (!any(normalizescheme == c('none','ztransform', 'iqr', 'sumone'))| length(normalizescheme) != 1) {
    errors[length(errors) + 1] <- 3
  }

  if ((!any(normalclasswise == orglabellvls) & normalclasswise != 'none') | length(normalclasswise) != 1) {
    errors[length(errors) + 1] <- 4
  }

  if (class(replaceNA) != 'logical' | length(replaceNA) != 1) {
    errors[length(errors) + 1] <- 5
  }

  if (class(replaceclasswise) != 'logical' | length(replaceclasswise) != 1) {
    errors[length(errors) + 1] <- 6
  }
  
  if (length(errors) > 0) {
    error(errors)
  }
}


checkLVQvars <- function (prototypes, learningrate, epochs, initscheme, distscheme, relevancemode, relevancescheme, LVQscheme, optimisationscheme, relrate, customdist, alfa, show, graphics, plotcurve, labellvls, dimensions) {

  errors <- vector()

  if (length(prototypes) != length(labellvls) | any(is.na(prototypes[labellvls]))) {
    errors[length(errors) + 1] <- 7
  }

  if (!any(class(prototypes) == c('numeric', 'integer'))) {
    errors[length(errors) + 1] <- 8
  }

  if (any(class(prototypes) == c('numeric', 'integer'))) {
    if (any(prototypes <= 0) | any(prototypes %% 1 != 0)) {
      errors[length(errors) + 1] <- 9
    }
  }

  if (epochs <= 0 | epochs %% 1 != 0) {
    errors[length(errors) + 1] <- 10
  }

  if (class(learningrate) != 'numeric' | (length(learningrate) != 1 & length(learningrate) != epochs)) {
    errors[length(errors) + 1] <- 11
  }

  if (class(learningrate) == 'numeric') {
    if (any(learningrate <= 0) | any(learningrate >= 1)) {
      errors[length(errors) + 1] <- 12
    }
  }

  if (class(relrate) != 'numeric' | (length(relrate) != 1 & length(relrate) != epochs)) {
    errors[length(errors) + 1] <- 13
  }

  if (class(relrate) == 'numeric') {
    if (any(relrate <= 0) | any(relrate >= 1)) {
      errors[length(errors) + 1] <- 14
    }
  }

  if (!any(initscheme == c('randomwindow', 'zero', 'randomsample', 'mean', 'classmean')) | length(initscheme) != 1) {
    errors[length(errors) + 1] <- 15
  }

  if (!any(distscheme == c('euclidean', 'manhattan', 'custom')) | length(distscheme) != 1) {
    errors[length(errors) + 1] <- 16
  }

  if (!any(relevancemode == c('normal', 'relevance', 'matrix')) | length(relevancemode) != 1) {
    errors[length(errors) + 1] <- 17
  }

  if (!any(LVQscheme == c('LVQ1', 'cauchyschwarz', 'renyi')) | length(LVQscheme) != 1) {
    errors[length(errors) + 1] <- 18
  }

  if (!any(class(customdist) == c('numeric', 'integer')) | length(customdist) != 1) {
    errors[length(errors) + 1] <- 19
  }

  if (any(class(customdist) == c('numeric', 'integer'))) {
    if (customdist < 1) {
      errors[length(errors) + 1] <- 20
    }
  }

  if (class(show) != 'logical' | length(show) != 1) {
    errors[length(errors) + 1] <- 21
  }

  if (class(graphics) != 'logical' | length(graphics) != 1) {
    errors[length(errors) + 1] <- 22
  }

  if (class(graphics) == 'logical' & dimensions != 2) {
    if (graphics) {
      errors[length(errors) + 1] <- 23
    }
  }

  if (class(plotcurve) != 'logical' | length(plotcurve) != 1) {
    errors[length(errors) + 1] <- 25
  }

  
  if (!any(class(alfa) == c('numeric', 'integer')) | length(alfa) != 1) {
    errors[length(errors) + 1] <- 73
  }

  if (any(class(alfa) == c('numeric', 'integer'))) {
    if (alfa <= 1) {
      errors[length(errors) + 1] <- 74
    }
  }

  if (!any(optimisationscheme == c('normal', 'general')) | length(optimisationscheme) != 1) {
    errors[length(errors) + 1] <- 75
  }

  if (any(LVQscheme == c('cauchyschwarz', 'renyi')) & any(initscheme == c('zero'))){
    errors[length(errors) + 1] <- 76
  }

  if (!any(relevancescheme == c('global', 'local', 'classwise')) | length(relevancescheme) != 1) {
    errors[length(errors) + 1] <- 87
  }

  if (any(relevancemode == c('relevance', 'matrix')) & any(LVQscheme == c('cauchyschwarz', 'renyi'))) {
    errors[length(errors) + 1] <- 94
  }

  if (length(errors) > 0) {
    error(errors)
  }
}

checkValidateVars <- function (validatescheme, nfold, nrdatapoints) {
  
  errors <- vector()

  if (!any(validatescheme == c('traintest', 'nfold', 'train')) | length(validatescheme) != 1) {
    errors[length(errors) + 1] <- 26
  }

  if (!any(class(nfold) == c('numeric', 'integer')) | length(nfold) != 1) {
    errors[length(errors) + 1] <- 27
  }

  if (any(class(nfold) == c('numeric', 'integer'))) {
    if (validatescheme == 'nfold' & (nfold <= 1 | nfold %% 1 != 0 | nfold > nrdatapoints)) {
      errors[length(errors) + 1] <- 28
    }
  }

  if (length(errors) > 0) {
    error(errors)
  }
}

checkOutputVars <- function (prototypeoutput, relevanceoutput, costcurve, progress, relevanceprogress, trainerror, testerror, trainerrorprogress, testerrorprogress, validatescheme, relevancemode) {

  errors <- vector()

  if (class(prototypeoutput) != 'logical' | length(prototypeoutput) != 1) {
    errors[length(errors) + 1] <- 31
  }

  if (class(relevanceoutput) != 'logical' | length(relevanceoutput) != 1) {
    errors[length(errors) + 1] <- 32
  }

  if (class(relevanceoutput) == 'logical' & relevancemode != 'relevance' & relevancemode != 'matrix') {
    if (relevanceoutput) {
      errors[length(errors) + 1] <- 33
    }
  }

  if (class(costcurve) != 'logical' | length(costcurve) != 1) {
    errors[length(errors) + 1] <- 34
  }

  if (class(progress) != 'logical' | length(progress) != 1) {
    errors[length(errors) + 1] <- 35
  }

  if (class(relevanceprogress) != 'logical' | length(relevanceprogress) != 1) {
    errors[length(errors) + 1] <- 36
  }

  if (class(relevanceprogress) == 'logical' & relevancemode != 'relevance' & relevancemode != 'matrix') {
    if (relevanceprogress) {
      errors[length(errors) + 1] <- 37
    }
  }

  if (class(testerror) != 'logical' | length(testerror) != 1) {
    errors[length(errors) + 1] <- 38
  }

  if (class(testerror) == 'logical' & validatescheme != 'traintest' & validatescheme != 'nfold') {
    if (testerror) {
      errors[length(errors) + 1] <- 39
    }
  }

  if (class(trainerror) != 'logical' | length(trainerror) != 1) {
    errors[length(errors) + 1] <- 59
  }

  if (class(testerrorprogress) != 'logical' | length(testerrorprogress) != 1) {
    errors[length(errors) + 1] <- 62
  }

  if (class(testerrorprogress) == 'logical' & validatescheme != 'traintest' & validatescheme != 'nfold') {
    if (testerrorprogress) {
      errors[length(errors) + 1] <- 63
    }
  }

  if (class(trainerrorprogress) != 'logical' | length(trainerrorprogress) != 1) {
    errors[length(errors) + 1] <- 64
  }

  if (length(errors) > 0) {
    error(errors)
  }

}

checkShowVars <- function(LVQoutput, prototypes, relevances, costcurve, prototypeprogress, relevanceprogress, trainerror, testerror, trainerrorprogress, testerrorprogress, relevancenumber, relevanceprognumber) {
  errors <- vector()
print('checkShowVars')
  if (!any(class(LVQoutput) == c('trainoutput', 'traintestoutput', 'nfoldoutput'))) {
    errors[length(errors) + 1] <- 40
  }

  if (class(prototypes) != 'logical' | length(prototypes) != 1) {
    errors[length(errors) + 1] <- 77
  }

  if (class(relevances) != 'logical' | length(relevances) != 1) {
    errors[length(errors) + 1] <- 41
  }

  if (class(costcurve) != 'logical' | length(costcurve) != 1) {
    errors[length(errors) + 1] <- 42
  }

  if (class(prototypeprogress) != 'logical' | length(prototypeprogress) != 1) {
    errors[length(errors) + 1] <- 78
  }

  if (class(relevanceprogress) != 'logical' | length(relevanceprogress) != 1) {
    errors[length(errors) + 1] <- 43
  }

  if (class(testerror) != 'logical' | length(testerror) != 1) {
    errors[length(errors) + 1] <- 44
  }

  if (class(relevances) == 'logical' & length(relevances) == 1) {
    if (relevances & length(attr(LVQoutput, 'relevances')) == 0) {
      errors[length(errors) + 1] <- 45
    }
  }

  if (class(costcurve) == 'logical' & length(costcurve) == 1) {
    if (costcurve & length(attr(LVQoutput, 'costcurve')) == 0) {
      errors[length(errors) + 1] <- 46
    }
  }

  if (class(relevanceprogress) == 'logical' & length(relevanceprogress) == 1) {
    if (relevanceprogress & length(attr(LVQoutput, 'relevanceprogress')) == 0) {
      errors[length(errors) + 1] <- 47
    }
  }

  if (class(testerror) == 'logical' & length(testerror) == 1) {
    if (testerror & length(attr(LVQoutput, 'testerror')) == 0) {
      errors[length(errors) + 1] <- 48
    }
  }

  if (class(trainerror) != 'logical' | length(trainerror) != 1) {
    errors[length(errors) + 1] <- 60
  }

  if (class(trainerror) == 'logical' & length(trainerror) == 1) {
    if (trainerror & length(attr(LVQoutput, 'trainerror')) == 0) {
      errors[length(errors) + 1] <- 61
    }
  }

  if (class(trainerrorprogress) != 'logical' | length(trainerrorprogress) != 1) {
    errors[length(errors) + 1] <- 65
  }

  if (class(trainerrorprogress) == 'logical' & length(trainerrorprogress) == 1) {
    if (trainerrorprogress & length(attr(LVQoutput, 'trainerrorprogress')) == 0) {
      errors[length(errors) + 1] <- 66
    }
  }

  if (class(testerrorprogress) != 'logical' | length(testerrorprogress) != 1) {
    errors[length(errors) + 1] <- 67
  }

  if (class(testerrorprogress) == 'logical' & length(testerrorprogress) == 1) {
    if (testerrorprogress & length(attr(LVQoutput, 'testerrorprogress')) == 0) {
      errors[length(errors) + 1] <- 68
    }
  }

  if (!any(class(relevancenumber) == c('numeric', 'integer')) | length(relevancenumber) != 1) {
    errors[length(errors) + 1] <- 90
  }

  if (!any(class(relevanceprognumber) == c('numeric', 'integer')) | length(relevanceprognumber) != 1) {
    errors[length(errors) + 1] <- 91
  }

  if (any(class(relevancenumber) == c('numeric', 'integer'))) {
    if (relevancenumber %% 1 != 0 | (relevancenumber != -1 & relevancenumber <= 0 & relevancenumber > attr(LVQoutput, 'nrofrelevances'))) {
      errors[length(errors) + 1] <- 92
    }
  }

  if (any(class(relevanceprognumber) == c('numeric', 'integer'))) {
    if (relevanceprognumber %% 1 != 0 | (relevanceprognumber != -1 & relevanceprognumber <= 0 & relevanceprognumber > attr(LVQoutput, 'nrofrelevances'))) {
      errors[length(errors) + 1] <- 93
    }
  }
  
  if (length(errors) > 0) {
    error(errors)
  }

}

checkShowNfoldVars <- function (LVQoutput, protofold, relfold, costfold, protoprogfold, relprogfold, trainerrorfold, testerrorfold) {
  
  errors <- vector()

  if (!any(class(protofold) == c('numeric', 'integer')) | length(protofold) != 1) {
    errors[length(errors) + 1] <- 79
  }

  if (!any(class(relfold) == c('numeric', 'integer')) | length(relfold) != 1) {
    errors[length(errors) + 1] <- 49
  }

  if (!any(class(costfold) == c('numeric', 'integer')) | length(costfold) != 1) {
    errors[length(errors) + 1] <- 50
  }

  if (!any(class(relprogfold) == c('numeric', 'integer')) | length(relprogfold) != 1) {
    errors[length(errors) + 1] <- 51
  }

  if (!any(class(protoprogfold) == c('numeric', 'integer')) | length(protoprogfold) != 1) {
    errors[length(errors) + 1] <- 80
  }

  if (any(class(protofold) == c('numeric', 'integer'))) {
    if (protofold %% 1 != 0 | (protofold != -1 & protofold <= 0 & protofold > attr(LVQoutput, 'nfold'))) {
      errors[length(errors) + 1] <- 81
    }
  }

  if (any(class(relfold) == c('numeric', 'integer'))) {
    if (relfold %% 1 != 0 | (relfold != -1 & relfold <= 0 & relfold > attr(LVQoutput, 'nfold'))) {
      errors[length(errors) + 1] <- 52
    }
  }

  if (any(class(costfold) == c('numeric', 'integer'))) {
    if (costfold %% 1 != 0 | (costfold != -1 & costfold <= 0 & costfold > attr(LVQoutput, 'nfold'))) {
      errors[length(errors) + 1] <- 53
    }
  }

  if (any(class(relprogfold) == c('numeric', 'integer'))) {
    if (relprogfold %% 1 != 0 | (relprogfold != -1 & relprogfold <= 0 & relprogfold > attr(LVQoutput, 'nfold'))) {
      errors[length(errors) + 1] <- 82
    }
  }

  if (any(class(relprogfold) == c('numeric', 'integer'))) {
    if (relprogfold %% 1 != 0 | (relprogfold != -1 & relprogfold <= 0 & relprogfold > attr(LVQoutput, 'nfold'))) {
      errors[length(errors) + 1] <- 54
    }
  }

   if (!any(class(trainerrorfold) == c('numeric', 'integer')) | length(trainerrorfold) != 1) {
    errors[length(errors) + 1] <- 69
  }

  if (any(class(trainerrorfold) == c('numeric', 'integer'))) {
    if (trainerrorfold %% 1 != 0 | (trainerrorfold != -1 & trainerrorfold <= 0 & trainerrorfold > attr(LVQoutput, 'nfold'))) {
      errors[length(errors) + 1] <- 70
    }
  }

   if (!any(class(testerrorfold) == c('numeric', 'integer')) | length(testerrorfold) != 1) {
    errors[length(errors) + 1] <- 71
  }

  if (any(class(testerrorfold) == c('numeric', 'integer'))) {
    if (testerrorfold %% 1 != 0 | (testerrorfold != -1 & testerrorfold <= 0 & testerrorfold > attr(LVQoutput, 'nfold'))) {
      errors[length(errors) + 1] <- 72
    }
  }

  if (length(errors) > 0) {
    error(errors)
  }

}

checkInput <- function (traininp, testinp) {

  errors <- vector()

  if (!all(is.na(traininp))) {
    if (!class(traininp) == 'matrix') {
      errors[length(errors) + 1] <- 55
    } else if (dim(traininp)[2] < 2) {
      errors[length(errors) + 1] <- 56
    }
  }


  if (!all(is.na(testinp))) {
    if (!class(testinp) == 'matrix') {
      errors[length(errors) + 1] <- 57
    } else if (dim(testinp)[2] < 2) {
      errors[length(errors) + 1] <- 58
    }
  }

  if (length(errors) > 0) {
    error(errors)
  }

}

checkGetVars <- function (LVQout, fold) {
  errors <- vector()

  if (!is.na(fold)) {
    if (class(LVQout) != 'nfoldoutput') {
      errors[length(errors) + 1] <- 83
    }
  

    if (class(LVQout) == 'nfoldoutput' & any(class(fold) == c('numeric', 'integer'))) {
      if ((fold != -1 & fold < 1 & attr(LVQout, 'nfold') > fold) | fold %% 1 != 0) {
	errors[length(errors) + 1] <- 84
      }
    }

    if (!any(class(fold) == c('numeric', 'integer'))) {
      errors[length(errors) + 1] <- 85
    }
  }


  if (length(errors) > 0) {
    error(errors)
  }

}

checkData <- function (data, LVQscheme, relevances, relevancescheme) {
  errors <- vector()

  dimensions <- length(data[1,])-1

  if (relevancescheme == 'relevance' & class(relevances) != 'vector' & !is.na(relevances)) {
    errors[length(errors) + 1] <-88
  }

  if (relevancescheme == 'relevance' & class(relevances) != 'matrix' & !is.na(relevances)) {
    errors[length(errors) + 1] <- 89
  }

  if (class(relevances) == 'vector') {
    if (length(relevances) != dimensions) {
      errors[length(errors) + 1] <- 1
    }
  }

  if (class(relevances) == 'matrix') {
    if (dim(relevances)[1] != dimensions | dim(relevances)[2] != dimensions) {
      errors[length(errors) + 1] <- 2
    }
  }

  if (class(relevances) == 'list') {
    for (i in 1:length(relevances)) {
      if (class(relevances[[i]]) == 'vector') {
	if (length(relevances) != dimensions) {
	  errors[length(errors) + 1] <- 1
	} else if (class(relevances[[i]]) == 'matrix') {
	  if (dim(relevances[[i]])[1] != dimensions | dim(relevances[[i]])[2] != dimensions) {
	    errors[length(errors) + 1] <- 2
	  }
	}
      }
    }
  }
  if (!all(is.na(data))) {
    if (any(LVQscheme == c('cauchyschwarz', 'renyi')) & (any(data[,1:dimensions] < 0, na.rm = TRUE) | any(rowSums(data[,1:dimensions], na.rm = TRUE) < (1 - 1e-8)) | any(rowSums(data[,1:dimensions], na.rm = TRUE) > (1 + 1e-8)))) {
      errors[length(errors) + 1] <- 86
    }
  }
  if (length(errors) > 0) {
    error(errors)
  }
}