\name{de.sim}
\alias{de.sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate a '3+3/PC' dose-titration study
}
\description{
Please see the reference cited below.
}
\usage{
de.sim(CV = 0.7, start.dose = 0.25, dose.jump = 0.4, N = 24, periods = N/3 + 2,
       testing = is.null(sys.call(-1)), once = testing)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CV}{Coefficient of variation for MTDi in the population}
  \item{start.dose}{Lowest dose in geometric sequence of doses to trial}
  \item{dose.jump}{The fractional increase between dose levels}
  \item{N}{Number of simulated subjects to enroll}
  \item{periods}{Number of DLT assessment periods to simulate}
  \item{testing}{This parameter helps differentiate function behavior (type of value returned) appropriately to interactive vs programmed invocation}
  \item{once}{When TRUE, does a single simulation returning \code{de} object}
}
\value{
When once=TRUE, a list of data frames summarizing the events of a dose-titration
study up to a given DLT assessment period. Thus, \code{de[[3]]} is a data frame
with columns as follows:
\itemize{
  \item{id}{Integer: participants are numbered in order of enrollment, starting from 1}
  \item{period}{Integer: DLT assessment periods of the study, numbered from 1}
  \item{dose}{Integer: dose level received by \code{id} during \code{period}}
  \item{dlt}{Logical: did participant \code{id} experience a DLT during \code{period}?}
}
For periods after dose escalation stops, a 'stop.esc' attribute will be
attached to subsequent period elements. Thus, if escalation stops at the
end of period 7, \code{attr(de[[8]],'stop.esc')} will equal 7, and this
will be true for all subsequent periods as well.

When testing=FALSE, a simple summary result is returned, suitable for
aggregating an ensemble of simulations.
}
\references{
Norris DC. Precautionary Coherence Unravels Dose Escalation Designs. bioRxiv. December 2017:240846. doi:10.1101/240846. \url{https://www.biorxiv.org/content/early/2017/12/29/240846}

}
\author{David C. Norris}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{step}}, which \code{de.sim} calls recursively
}
\examples{
# Perform a regression test for package:DTAT
de.compare <- de.sim(testing=TRUE)
# Strip down to basics from v0.2-1
attributes(de.compare) <- NULL
class(de.compare) <- "list"
data(de.bioRxiv.240846)
stopifnot(identical(de.compare, de.bioRxiv.240846))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }
