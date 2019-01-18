\name{step}
\alias{step}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recursively simulate a '3+3/PC' dose-titration study
}
\description{
This function is invoked recursively by \code{de.sim}, to simulate
a complete 3+3/PC study. For details on the 3+3/PC rules, please see
the references below.
}
\usage{
step(de, MTDi, verbose = is.null(sys.call(-1)), n.doses = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{de}{A data frame describing the events of a dose-titration study underway}
  \item{MTDi}{Vector of MTDi's, indexed by participant id}
  \item{verbose}{Logical: the default provides verbose output (for debugging and instruction) when invoked interactively from the R prompt, but not when invoked 'programmatically' by client code.}
  \item{n.doses}{When specified, prevents escalation to (n+1)th dose}
}
\value{
A data frames summarizing the events of a dose-titration study
up to a given DLT assessment period, having columns as follows:
\itemize{
  \item{id}{Integer: participants are numbered in order of enrollment, starting from 1}
  \item{period}{Integer: DLT assessment periods of the study, numbered from 1}
  \item{dose}{Integer: dose level received by \code{id} during \code{period}}
  \item{dlt}{Logical: did participant \code{id} experience a DLT during \code{period}?}
}
}
\references{
Norris DC. Precautionary Coherence Unravels Dose Escalation Designs. bioRxiv. December 2017:240846. doi:10.1101/240846. \url{https://www.biorxiv.org/content/early/2017/12/29/240846}

}
\author{David C. Norris}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{de.sim}} invokes \code{step} to simulate a full study.
}
\keyword{ datagen }
