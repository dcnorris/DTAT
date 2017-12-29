\name{ds.curve}
\alias{ds.curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract the dose-survival curve, with its upper and lower confidence band limits
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ds.curve(de, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{de}{
%%     ~~Describe \code{de} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with components \code{surv}, \code{upper} and \code{lower}, each containing a vector that can be indexed by dose level.
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
function (de, ...) 
{
    fit <- dose.survfit(de, ...)
    surv <- approx(fit$time, fit$surv, method = "constant", xout = 1:max(fit$time))$y
    lower <- approx(fit$time, fit$lower, method = "constant", 
        xout = 1:max(fit$time))$y
    upper <- approx(fit$time, fit$upper, method = "constant", 
        xout = 1:max(fit$time))$y
    list(surv = surv, lower = lower, upper = upper)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
