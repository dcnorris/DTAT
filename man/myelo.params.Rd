\name{myelo.params}
\alias{myelo.params}
\docType{data}
\title{
Parameters of the chemotherapy-induced myelosuppression model of Friberg et al. (2002).
}
\description{
Parameters of the chemotherapy-induced myelosuppression model of Friberg et al. (2002). These parameters are from the Docetaxel row of Table 4.
}
\usage{data("myelo.params")}
\format{
  A data frame with 5 observations on the following 4 variables.
  \describe{
    \item{\code{Param}}{a character vector}
    \item{\code{Units}}{a character vector}
    \item{\code{Mean}}{a numeric vector}
    \item{\code{CV}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Friberg LE, Henningsson A, Maas H, Nguyen L, Karlsson MO. Model of chemotherapy-induced myelosuppression with parameter consistency across drugs. J Clin Oncol. 2002;20(24):4713-4721. doi:10.1200/JCO.2002.02.140.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(myelo.params)
## maybe str(myelo.params) ; plot(myelo.params) ...
}
\keyword{datasets}
