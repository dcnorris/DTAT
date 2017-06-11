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
\code{\link{doChemo}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dose1, omega, slope1, slopeU) 
{
    dta <- function(id, cycle, course) {
        dose <- dose1
        if (cycle > 1) {
            lag_1 <- which(course$cycle == (cycle - 1) & course$id == 
                id)
            CircMin_1 <- course[lag_1, "CircMin"]
            dose_1 <- course[lag_1, "dose"]
            if (cycle == 2) {
                slope <- slope1
            }
            else {
                lag_2 <- which(course$cycle == (cycle - 2) & 
                  course$id == id)
                CircMin_2 <- course[lag_2, "CircMin"]
                dY <- log(CircMin_1/CircMin_2)
                dose_2 <- course[lag_2, "dose"]
                dX <- scaled(dose_1) - scaled(dose_2)
                slope <- dY/dX
                slope <- min(slope, slopeU)
            }
            delta.scaleddose <- log(500/CircMin_1)/slope
            delta.safer <- ifelse(delta.scaleddose > 0, omega * 
                delta.scaleddose, delta.scaleddose)
            new.scaleddose <- scaled(dose_1) + delta.safer
            dose <- uniroot(function(y) scaled(y) - new.scaleddose, 
                c(0, 1e+05))$root
        }
        return(dose)
    }
    dta
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
