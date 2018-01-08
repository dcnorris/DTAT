\name{titrate}
\alias{titrate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform Dose Titration Algorithm Tuning (DTAT)
}
\description{
Perform neutrophil-guided dose titration of a chemotherapy drug.
}
\usage{
titrate(draw.days = NULL, Tcyc = 3 * 7 * 24, Ncycles = 10,
        doserange = c(50, 500), dta = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{draw.days}{
%%     ~~Describe \code{draw.days} here~~
}
  \item{Tcyc}{
%%     ~~Describe \code{Tcyc} here~~
}
  \item{Ncycles}{
%%     ~~Describe \code{Ncycles} here~~
}
  \item{doserange}{
%%     ~~Describe \code{doserange} here~~
}
  \item{dta}{
  A Dose Titration Algorithm (DTA) to drive the titration
}
}
\value{
A list with 2 components:
\item{course}{A data frame containing cycle-wise measures of each id's titration course}
\item{anc.ts}{A data frame detailing hourly ANC measures for each id}
}
\author{David C. Norris}
\note{
TODO: Once I've implemented several nontrivial dosing algorithms, I should
      consider whether the 'doserange' case ought to be handled by a special
      non-adaptive DTA.

}
