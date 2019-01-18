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
  \item{dose1}{The starting dose for titration}
  \item{omega}{A relaxation parameter used to moderate dose increments}
  \item{slope1}{Dose-response slope assumed prior to 2nd measured neutrophil nadir}
  \item{slopeU}{Upper bound imposed on slope estimates}
}
\details{
This function manifests the core concept of Dose Titration Algorithm Tuning
by delivering an objectively realized 'DTA'. It therefore enables a variety
of DTAs to be implemented and compared.
}
\value{
A dose titration function that advises dose for next cycle of chemotherapy.
}
\author{David C. Norris}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{titrate}}
}
