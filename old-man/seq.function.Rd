\name{seq.function}
\alias{seq.function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A seq method supporting custom-scaled plot axes.
}
\description{
This provides a \code{seq} method for class \code{function}, supporting
a natural axis scaling idiom.
}
\usage{
\S3method{seq}{function}(scalefun, from, to, length.out, digits = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scalefun}{A numeric function that will be invoked componentwise, and so need not be vectorized)}
  \item{from, to}{The starting and ending values of the sequence returned}
  \item{length.out}{Desired length of the sequence}
  \item{digits}{If non-NULL, returned value is rounded accordingly}
  \item{\dots}{Unused; included for S3 generic/method consistency.}
}
\value{
A numeric vector that (not considering the effect of any rounding applied),
becomes an arithmetic sequence after application of \code{scalefun} to it.
The initial and final elements of that vector are \code{from} and \code{to}.
}
\author{David C. Norris}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# Provide evenly-spaced length-6 sequence from 100 to 1000,
# evenly spaced on a fourth-root scale:
seq(function(dose, a=4.0) dose^(1/a), from=100, to=1000, length.out=6, digits=0)
}
