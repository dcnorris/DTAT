\name{newton.raphson}
\alias{newton.raphson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A dose titration algorithm (DTA) 'factory' based on the Newton-Raphson heuristic
}
\description{
This higher-order ('factory') function produces a simple dose titration algorithm
for neutrophil-guided chemotherapy dosing.
}
\usage{
newton.raphson(dose1, omega, slope1, slopeU)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dose1}{
%%     ~~Describe \code{dose1} here~~
}
  \item{omega}{
%%     ~~Describe \code{omega} here~~
}
  \item{slope1}{
%%     ~~Describe \code{slope1} here~~
}
  \item{slopeU}{
%%     ~~Describe \code{slopeU} here~~
}
}
\details{
This function manifests the core concept of Dose Titration Algorithm Tuning
by delivering an objectively realized 'DTA'. It therefore enables a variety
of DTAs to be implemented and compared.
}
\value{
A dose titration function that advises dose for next cycle of chemotherapy.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{titrate}}
}
