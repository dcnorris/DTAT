\name{ds.curve}
\alias{ds.curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract the dose-survival curve, with its upper and lower confidence band limits
}
\description{
This utility function simply makes the results of \code{dose.survfit} available
in the convenient form of a list.
}
\usage{
ds.curve(de, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{de}{A data frame describing a dose-titration study.}
  \item{\dots}{Passed through to function \code{dose.survfit}}
}
\value{
A list with components \code{surv}, \code{upper} and \code{lower}, each containing a vector that can be indexed by dose level.
}
\author{David C. Norris}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dose.survfit}}
}
