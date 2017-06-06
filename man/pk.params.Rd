\name{pk.params}
\alias{pk.params}
\docType{data}
\title{
Parameter estimates for a 2-compartment population pharmacokinetic model for docetaxel, from Onoue et al (2016).
}
\description{
Parameter estimates for a 2-compartment population pharmacokinetic model for docetaxel, from Onoue et al (2016).
}
\usage{data("pk.params")}
\format{
  A data frame with 4 observations on the following 4 variables.
  \describe{
    \item{\code{Param}}{a character vector}
    \item{\code{Units}}{a character vector}
    \item{\code{Mean}}{a numeric vector}
    \item{\code{CV}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Onoue H, Yano I, Tanaka A, et al. Significant effect of age on docetaxel pharmacokinetics in Japanese female breast cancer patients by using the population modeling approach. Eur J Clin Pharmacol. 2016;72(6):703-710. doi:10.1007/s00228-016-2031-3.
}
\examples{
data(pk.params)
## maybe str(pk.params) ; plot(pk.params) ...
}
\keyword{datasets}
