\name{dose.survival}
\alias{dose.survival}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract interval-censored dose tolerance data from a dose titration study
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dose.survival(de)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{de}{
%%     ~~Describe \code{de} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \code{Surv} object
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (de) 
{
    de <- select(de, id, dose, dlt)
    suppressMessages({
        L <- summarize(group_by(subset(de, !dlt), id), doseL = max(dose))
        suppressWarnings(R <- summarize(group_by(subset(de, dlt), 
            id), doseR = min(dose)))
        ds <- full_join(L, R)
    })
    ds$doseL[is.na(ds$doseL)] <- 0
    ds$doseR[is.na(ds$doseR)] <- Inf
    ds <- arrange(ds, id)
    ds.debug <<- ds
    S <- with(ds, Surv(time = doseL, time2 = doseR, type = "interval2"))
    S
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{survival}

