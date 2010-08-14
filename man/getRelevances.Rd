\name{getRelevances}
\alias{getRelevances}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ getRelevances }
\description{
  This function returns the end-configuration of the relevances.
}
\usage{
getRelevances(LVQout, fold = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LVQout}{ The output-class containing the output to be returned.}
  \item{fold}{ Determines from which subset the output is to be returned. }
}

\value{
  A vector or matrix (depending on the relevance version used) containing the end relevance values.
  Or a list of such vectors or matrices in case of nfoldcross validation or the use of local or classwise relevances.
  Or a list of lists of such vectors or matrices in case of both nfoldcross validation and the use of local or classwise relevances.
  
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



