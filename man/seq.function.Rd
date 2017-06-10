\name{seq.function}
\alias{seq.function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A seq method supporting custom-scaled plot axes.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\S3method{seq}{function}(scalefun, from, to, length.out, digits = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scalefun}{
%%     ~~Describe \code{scalefun} here~~
}
  \item{from}{
%%     ~~Describe \code{from} here~~
}
  \item{to}{
%%     ~~Describe \code{to} here~~
}
  \item{length.out}{
%%     ~~Describe \code{length.out} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{\dots}{Unused; included for S3 generic/method consistency.}
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
function (scalefun, from, to, length.out, digits = NULL) 
{
    x <- seq(from = scalefun(from), to = scalefun(to), length.out = length.out)
    y <- numeric(length.out)
    if (from == to) {
        y1 <- uniroot(function(y) scaled(y) - x[1], c(from - 
            1, from + 1))$root
        return(rep(y1, length.out))
    }
    for (i in seq(length(y))) y[i] <- uniroot(function(y) scaled(y) - 
        x[i], sort(c(from, to)))$root
    if (!is.null(digits)) 
        y <- round(y, digits = digits)
    y
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
