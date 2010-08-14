\name{trainoutput-class}
\docType{class}
\alias{trainoutput-class}

\title{Class "trainoutput"}
\description{	This class contains all the output of a validate-run according to the train-scheme.}

\section{Slots}{
	 \describe{
    \item{\code{prototypes}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{relevances}:}{Object of class \code{"vector"} ~~ }
    \item{\code{costcurve}:}{Object of class \code{"vector"} ~~ }
    \item{\code{protoprogress}:}{Object of class \code{"list"} ~~ }
    \item{\code{relevanceprogress}:}{Object of class \code{"list"} ~~ }
    \item{\code{trainerror}:}{Object of class \code{"integer"} ~~ }
    \item{\code{trainerrorprogress}:}{Object of class \code{"vector"} ~~ }
    \item{\code{nrofrelevances}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Methods}{
No methods defined with class "trainoutput" in the signature.
}

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

\references{ LVQTools Bachelor project: implementing LVQ in R }
\author{ Sander Kelders }


