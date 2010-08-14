\name{getPrototypes}
\alias{getPrototypes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ getPrototypes }
\description{
  This function returns the end-configuration of the prototypes.
}
\usage{
getPrototypes(LVQout, fold = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LVQout}{ The output-class containing the output to be returned.}
  \item{fold}{ Determines from which subset the output is to be returned. }
}

\value{
  A matrix of prototype endvalues with the classlabels attached or a list of such matrices in case of nfoldcross validation.
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


