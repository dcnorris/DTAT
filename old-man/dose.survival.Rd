\name{dose.survival}
\alias{dose.survival}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract interval-censored dose tolerance data from a dose titration study
}
\description{
Extract interval-censored dose tolerance data from a dose titration study
}
\usage{
dose.survival(de)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{de}{A data frame describing a dose-titration study}
}
\value{
A \code{Surv} object of type='interval2'
}
\author{David C. Norris}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dose.survfit}}
}
\keyword{survival}

