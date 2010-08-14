\name{getTestError}
\alias{getTestError}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ getTestError }
\description{
  This function returns the number of missclassifications that were encountered when classifying the testset with the prototype end-configuration.
}
\usage{
getTestError(LVQout, fold = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LVQout}{ The output-class containing the output to be returned.}
  \item{fold}{ Determines from which subset the output is to be returned. }
}

\value{
 A numeric which is the testerror after the last epoch.
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



