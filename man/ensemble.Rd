\name{ensemble}
\alias{ensemble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate an ensemble of simulated dose titration studies.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ensemble(K = 10, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{K}{
%%     ~~Describe \code{K} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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
