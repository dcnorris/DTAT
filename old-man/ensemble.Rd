\name{ensemble}
\alias{ensemble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate and summarize an ensemble of simulated dose titration studies.
}
\description{
Generate and summarize an ensemble of simulated dose titration studies.
}
\usage{
ensemble(K = 10, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{K}{Number of simulated studies to run}
  \item{\dots}{Parameters to be passed through to \code{de.sim}}
}
\value{
A data frame summarizing key outcomes of the simulated trials:
\item{enrolling.dose}{Enrolling dose at conclusion of study}
\item{max.dose}{Maximum allowed dose at conclusion of study}
\item{effic.33pc}{Efficiency of titration over retained doses, relative to perfect 'MTDi' dosing}
\item{effic.1sfa}{Efficiency of 1-size-fits-all dosing, relative to perfect 'MTDi' dosing}
}
\author{David C. Norris}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{de.sim}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (K = 10, ...) 
{
    pb <- txtProgressBar(max = K, style = 3)
    df <- de.sim(...)
    MTDi.ensemble <<- list(MTDi.last)
    de.ensemble <<- list(de.last)
    setTxtProgressBar(pb, 1)
    for (k in 2:K) {
        df <- rbind(df, de.sim(...))
        MTDi.ensemble[[k]] <<- list(MTDi.last)
        de.ensemble[[k]] <<- de.last
        setTxtProgressBar(pb, k)
    }
    df
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
