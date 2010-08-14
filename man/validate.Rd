\name{validate}
\alias{validate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Learning vector Quantization validation}
\description{
  This is the entrypoint of the LVQTools. This function performs LVQ and validation according to the given parameters.
}
\usage{
validate(validatescheme = "train", testdatapath = NA, nfold = 8, LVQscheme = "LVQ1", optimisationscheme = "normal", inp = NA, testinp = NA, prototypeoutput = TRUE, relevanceoutput = FALSE, costcurve = FALSE, progress = FALSE, relevanceprogress = FALSE, trainerror = FALSE, testerror = FALSE, trainerrorprogress = FALSE, testerrorprogress = FALSE, datapath = NA, normalizescheme = "none", normalclasswise = "none", replaceNA = FALSE, replaceclasswise = FALSE, prototypes = vector(), learningrate = 0.01, epochs = 10, initscheme = "zero", distscheme = "euclidean", relevancemode = "normal", relevancescheme = "global", relevances = NA, relrate = 0.001, customdist = 3, alfa = 2, show = FALSE, graphics = FALSE, plotcurve = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{validatescheme}{ Determines how training and testing is to be executed. \strong{train} only trains the prototypes,
					  while \strong{traintest} also tests the prototypes after training with a different set of testdata.
					  The \strong{nfold}-scheme will apply nfoldcross-validation with the \strong{nfold}-parameter determining in how 						  many sets the data is to be divided.
					  The sets are divided randomly without consideration to class.
					  Available values: \strong{train}, \strong{traintest}, \strong{nfold}.
					  Default value: \strong{train} }
  \item{testdatapath}{ he location of the file containing data used for testing.
				If this value is \strong{NA} \strong{testinp} will be used, otherwise the specified file will be used.
				Default value: \strong{NA}. }
  \item{nfold}{ Determines the number of sets the data is divided in when using nfoldcross-validation.
			    Available values: any whole positive number in the range of \eqn{[2...numberofdatapoints]}.
			    Default value: \strong{8}. }
  \item{LVQscheme}{ Determines which version of LVQ is used. Together with \strong{distscheme}, \strong{relevancemode}, \strong{relevancescheme} and \strong{optimisationscheme} this determines the complete distancemeasure.
				     Available values: \strong{LVQ1}, \strong{cauchyschwarz}, \strong{renyi}. Default value: \strong{LVQ1}.}
  \item{optimisationscheme}{ Determines which type of costfunction is used and thus how the prototypes are updated.
						The \strong{normal} optimisationscheme uses the winner takes all principle and only updates the closest 						prototype.
						The \strong{general} optimisationscheme is used for generalized LVQ. It uses stochastic gradient descent to 							determine the prototype updates. The following function is used for this purpose:
						\eqn{\Sigma_i  \Phi(\mu)} with \eqn{\mu = \frac{d^\Lambda_J - d^\Lambda_K}{d^\Lambda_J + d^\Lambda_K}}.
						And with \eqn{d^\Lambda_J} as the distance to the nearest prototype of the appropriate class and \eqn{d^\Lambda_K} 							as the distance to the nearest prototype of another class.\\
						Available uses: \strong{normal}, \strong{general}.\\
						Default value: \strong{normal}. }
  \item{inp}{ The data used for training. If \strong{datapath} is \strong{NA} \strong{inp} will be used, otherwise \strong{datapath} will be used. Default value: \strong{NA}.}
  \item{testinp}{ The data used for training. If \strong{datapath} is \strong{NA} \strong{inp} will be used, otherwise \strong{datapath} will be used. Default value: \strong{NA}.}
  \item{prototypeoutput}{ When \strong{TRUE} records the endconfiguration of the prototypes of a training
					  and returns it (possibly among other things) after terminating.
					  Possible values: \strong{TRUE}, \strong{FALSE}.
					  Default value: \strong{TRUE}. }
  \item{relevanceoutput}{ When \strong{TRUE} records the endconfiguration of the relevance-vector or matrix of a training
					  and returns it (possibly among other things) after terminating.
					  Possible values: \strong{TRUE}, \strong{FALSE}.
					  Default value: \strong{FALSE}. }
  \item{costcurve}{ When \strong{TRUE} the cost is calculated after each epoch and the value stored.
				    When the program ends this cost (possibly among other things) is returned.
				    Available values: \strong{TRUE}, \strong{FALSE}.
				    Default value: \strong{FALSE}. }
  \item{progress}{ When \strong{TRUE} records the value of all prototypes before the first and after each epoch
				    and returns it (possibly among other things) after terminating.
				    Possible values: \strong{TRUE}, \strong{FALSE}.
				    Default value: \strong{FALSE}. }
  \item{relevanceprogress}{ When \strong{TRUE} records the value of the relevance-vector or matrix before the first and after each epoch
					  and returns it (possibly among other things) after terminating.
					  Possible values: \strong{TRUE}, \strong{FALSE}.
					  Default value: \strong{FALSE}. }
  \item{trainerror}{ After training tests with the trainingset and stores the number of missclassifications
						and returns (possibly among other things) it after terminating.
						Possible values: \strong{TRUE}, \strong{FALSE}.
						Default value: \strong{FALSE}. }
  \item{testerror}{ When testing with a different set than the trainingset stores the number of missclassifications after training
						and returns (possibly among other things) it after terminating.
						Possible values: \strong{TRUE}, \strong{FALSE}.
						Default value: \strong{FALSE}. }
  \item{trainerrorprogress}{ When \strong{TRUE} the trainerror is calculated after every epoch and stored to be returned (possibly) among other output.}
  \item{testerrorprogress}{ When using testdata to test the outcome of a training and \strong{testerrorprogress} is \strong{TRUE} calculates the testerror after very epoch and stores it to return (possibly) among other output.}
  \item{datapath}{ The location of the file containing data used for training or nfoldcrossvalidation.
				If this value is \strong{NA} \strong{inp} will be used, otherwise the specified file will be used.
				Default value: \strong{NA}. }
  \item{normalizescheme}{ Determines how the data is normalized. \strong{ztransform} substracts the mean and divides by variance. 
					  \strong{iqr} substracts the median and divides by the InterQuantile Range.
					  \strong{sumone} makes each datapoint sum up to one by dividing all values by the datapiont's sum.
					  Available schemes: \strong{ztransform}, \strong{iqr}, \strong{sumone}, \strong{none}.
					  Default value: \strong{none} }
  \item{normalclasswise}{ Determines the class on which the normalisation is based for classwise normalisation. Default value: \strong{none}. }
  \item{replaceNA}{ Determines whether or not the \strong{NA} values in the input will be replaced. If \strong{TRUE} they will be replaced by the overall median, unless classwise replacement is used. Available values: \strong{TRUE}, \strong{FALSE}. Default value: \strong{FALSE}.}
  \item{replaceclasswise}{ Determines whether or not the replacement of \strong{NA} values will be classwise. If \strong{TRUE} \strong{NA} values will be replaced by the median of the class to which the datapoint belongs to, otherwise the overall median is used.}
  \item{prototypes}{ Determines the number of prototypes for each class.
					This vector must have entries accesible by strings representing the classlabels.
					Each entry lists the number of prototypes for the class whose label was used for accessing it.
					A usable default value is not present. This parameter has to be specified manually.}
  \item{learningrate}{ Determines the rate at which the prototypes are adjusted.
				      This can be a single value to be used throughout the whole training process or a vector of length \strong{epochs}, 					      which will use each value once in order.
				      Default value: \strong{0.01}. }
  \item{epochs}{ The number of epochs used in training. Default value: \strong{10}.}
  \item{initscheme}{ Determines the way the prototypes are initialized.
				      \strong{mean} initializes all prototypes at the mean of all the datapoints.
				      \strong{randomsample} initializes all prototypes by selecting a different random sample for each prototype and using 					      its values for initialisation.
				      \strong{randomwindow} initializes all prototypes by constructing a window which includes all datapoints and 					      initialising each prototype randomly within this window.
				      \strong{zero} initializes all prototypes by setting all values to \strong{0}.
				      Available schemes: \strong{mean}, \strong{randomsample}, \strong{randomwindow}, \strong{zero}.\\
				      Default scheme: \strong{zero}. }
 				      \item{distscheme}{ The distance measure used for determining the difference between prototype and datapoint.
					   Together with \strong{LVQscheme}, \strong{relevancemode}, \strong{relevancescheme} and
					   \strong{optimisationscheme} this determines the complete distancemeasure.
					   When using scheme \strong{custom} a custom differencemeasure can be used by setting the \strong{customdist} 						   parameter. 
					   The \strong{distscheme} variable is only used in conjunction with the \strong{LVQ1} LVQscheme and not in 						   conjunction with \strong{cauchyschwarz} or \strong{renyi}.\\
					   Available schemes: \strong{manhattan}, \strong{euclidean}, \strong{custom}.\\
					   Default scheme: \strong{euclidean}. }
  \item{relevancemode}{ Determines if relevances should be used or not.
				  \strong{normal} mode does not use relevances at all.
				  \strong{relevance} mode uses a relevancevector to assign relevances to each dimension.
				  \strong{matrix} mode uses a square relevancematrix to assign relevances to dimensions and correlations between them.
				  When using mode \strong{matrix} only \strong{euclidean distancescheme}  is available.\\
				  Relevances are not available when using \strong{cauchyschwarz}- or \strong{renyi}-LVQscheme.\\
				  Available values: \strong{normal}, \strong{relevance}, \strong{matrix}.\\
				  Default value: \strong{normal}. }
  \item{relevancescheme}{ Determines how many different sets of relevances should be used. When using \strong{global}-relevances only 1 set of relevances 						      is used for all prototypes.
					      When using \strong{local}-relevances, each prototype has its own set of relevances.
					      When using \strong{classwise}-relevances all prototypes of the same class share a set of relevances.\\
					      Available values: \strong{global}, \strong{local}, \strong{classwise}\\
					      Default value: \strong{global}. }
  \item{relevances}{ When \strong{mode relevance} or \strong{matrix} is used this parameter contains the relevance-vector or matrix respectively.
				  The relevances can be specified manually using this parameter or when no relevances are provided they will be randomly 					  initialized.
				  Default value: \strong{NA}. }
  \item{relrate}{ When using relevances determines the rate at which the relevance-vector or matrix adapts.
				  This can be a single value to be used throughout the whole training process or a vector of length \strong{epochs}, which 					  will use each value once in order.
				  Default value: \strong{0.001} }
  \item{customdist}{ When using \strong{distancemeasure custom}, this parameter determines the distance-measure used.
				\strong{customdist} is \eqn{p} in \eqn{\sqrt[p]{|datapoint - prototype|^p}}}
  \item{alfa}{ A variable used only in conjunction with the \strong{renyi} LVQscheme. Determines the variant of renyi-divergence to be used.}
  \item{show}{ When \strong{TRUE} prints the prototype configuration and if applicable the relevance-vector or matrix and the costcurve to the console.
				Available values: \strong{TRUE}, \strong{FALSE}.
				Default value: \strong{FALSE}. }
  \item{graphics}{ When \strong{TRUE} and the data is 2-dimensional the progress of the prototypes will be plotted after every epoch.
				  Available values: \strong{TRUE}, \strong{FALSE}.
				  Default value: \strong{FALSE}. }
  \item{plotcurve}{ When \strong{TRUE} and \strong{costcurve} is also set to \strong{TRUE} the costcurve will be plotted after every training.
				    Available values: \strong{TRUE}, \strong{FALSE}.
				    Default value: \strong{FALSE}. }
}

\value{
  One of three object: either nfoldoutput, trainoutput, or traintestouput which contain the selected output.
}
\references{                 LVQTools Bachelor project: implementing LVQ in R
 }
\author{ Sander Kelders }


\seealso{
	  \code{\link{validate}}
	  \code{\link{show}},
	  \code{\link{showAll}},
	  \code{\link{getCostcurve}},
	  \code{\link{getPrototypeProgress}},
	  \code{\link{getPrototypes}},
	  \code{\link{getRelevanceProgress}},
	  \code{\link{getRelevances}},
	  \code{\link{getTestErrorProgress}},
	  \code{\link{getTestError}},
	  \code{\link{getTrainErrorProgress}},
	  \code{\link{getTrainError}},
	  \code{\link{nfoldoutput-class}},
	  \code{\link{trainoutput-class}},
	  \code{\link{traintestoutput-class}}
}


