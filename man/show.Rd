\name{show}
\alias{show}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ show }
\description{
  This function shows all the selected output. The selection is made by providing the appropriate parameters with TRUE.
}
\usage{
show(LVQoutput, prototypes = FALSE, relevances = FALSE, costcurve = FALSE, prototypeprogress = FALSE, relevanceprogress = FALSE, trainerror = FALSE, testerror = FALSE, trainerrorprogress = FALSE, testerrorprogress = FALSE, protofold = -1, relfold = -1, costfold = -1, protoprogfold = -1, relprogfold = -1, trainerrorprogfold = -1, testerrorprogfold = -1, relevancenumber = -1, relevanceprognumber = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LVQoutput}{ The output-class containing all the output.}
  \item{prototypes}{ This determines if the end-configuration of the prototypes should be among the output. }
  \item{relevances}{ This determines if the end-configuration of the relevances should be among the output. }
  \item{costcurve}{ This determines if the progress of the costfunction should be among the output. }
  \item{prototypeprogress}{ This determines if all the configurations of the prototypes should be among the output. }
  \item{relevanceprogress}{ This determines if all the configurations of the relevances should be among the output. }
  \item{trainerror}{ This determines if the number of missclassifications when testing the end-configuration of the prototypes with the testset should be among the output. }
  \item{testerror}{ This determines if the number of missclassifications when testing the end-configuration of the prototypes with the testset should be among the output. }
  \item{trainerrorprogress}{ This determines if the progress of the number of missclassifications when testing the end-configuration of the prototypes with the trainingset should be among the output. }
  \item{testerrorprogress}{ This determines if the progress of the number of missclassifications when testing the end-configuration of the prototypes with the testset should be among the output. }
  \item{protofold}{ Selects which prototype-end-configuration should be shown. -1 selects all. }
  \item{relfold}{ Selects which relevance-end-configuration should be shown. -1 selects all. }
  \item{costfold}{ Selects which costfunction-progress should be shown. -1 selects all. }
  \item{protoprogfold}{ Selects which prototype-progress should be shown. -1 selects all. }
  \item{relprogfold}{ Selects which prototype-progress should be shown. -1 selects all. }
  \item{trainerrorprogfold}{ Selects which trainerror-progress should be shown. -1 selects all. }
  \item{testerrorprogfold}{ Selects which testerror-progress should be shown. -1 selects all. }
  \item{relevancenumber}{ Selects which relevances should be shown in the case of classwise or local relevances. -1 selects all. }
  \item{relevanceprognumber}{ Selects of which relevances the progress should be shown in the case of classwise or local relevances. -1 selects all. }
}

\value{
  Graphics and/or values printed to screen depending on the output class and the chosen options.
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


