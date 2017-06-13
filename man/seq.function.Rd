\name{seq.function}
\alias{seq.function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A seq method supporting custom-scaled plot axes.
}
\description{
This provides a \code{seq} method for class \code{function}, supporting
a natural axis scaling idiom.
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
# Provide evenly-spaced length-6 sequence from 100 to 1000,
# evenly spaced on a fourth-root scale:
seq(function(dose, a=4.0) dose^(1/a), from=100, to=1000, length.out=6, digits=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
