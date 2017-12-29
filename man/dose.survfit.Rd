\name{dose.survfit}
\alias{dose.survfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate a dose-survival curve from a dose titration study, adding a confidence band
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dose.survfit(de, method = "rothman", avoid.degeneracy = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{de}{A dose titration experiment of the form produced by \code{de.sim}}
  \item{method}{The method to be used by \code{package:km.ci} when calculating CI}
  \item{avoid.degeneracy}{When TRUE, this parameter directs the function to introduce artificial events into the dose titration experiment, to avoid degeneracies at the lower and upper ends of the dose-survival curve.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of class \code{survfit}.
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
function (de, method = "rothman", avoid.degeneracy = TRUE) 
{
    artif.x <- 0.5
    artif.o <- 0.25
    weights <- rep(1, length(unique(de$id)))
    if (avoid.degeneracy) {
        if (with(de, !sum(dlt[dose == 1]))) {
            de <- rbind(data.frame(id = 0, period = 0, dose = 1, 
                dlt = TRUE), de)
            weights <- c(artif.x, weights)
        }
        if (with(subset(de, dose == max(dose)), all(dlt))) {
            de <- rbind(de, data.frame(id = Inf, period = max(de$period), 
                dose = max(de$dose), dlt = FALSE))
            weights <- c(weights, artif.o)
        }
    }
    de.inspect <<- de
    S <- dose.survival(de)
    S.inspect <<- S
    fit <- survfit(S ~ 1, weights = weights)
    fit.inspect <<- fit
    stopifnot(max(fit$time) == max(de$dose) || !avoid.degeneracy)
    fit <- km.ci(fit, method = method, conf.level = getOption("ds.conf.level"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
