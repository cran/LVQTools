\name{getRelevanceProgress}
\alias{getRelevanceProgress}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ getRelevanceProgress }
\description{
  This function returns the progress of the relevances.
}
\usage{
getRelevanceProgress(LVQout, fold = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LVQout}{ The output-class containing the output to be returned.}
  \item{fold}{ Determines from which subset the output is to be returned. }
}

\value{
  A list of relevances (which is a vector or a matrix depending on the variant of relevances used) containing the relevance values after eaxh epoch.
  Or a list of such lists in case of nfoldcross validation or the use of local or classwise relevances.
  Or a list of lists of such lists in case of both nfoldcross validation and the use of local or classwise relevances.
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



