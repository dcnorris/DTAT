\name{OXDSanimate}
\alias{OXDSanimate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transparent animation of a 3+3/PC dose-titration study
}
\description{
Animate the periods of a 3+3/PC dose-titration study, showing the
dose-survival curve as it accumulates with the conclusion of each
DLT assessment period.
}
\usage{
OXDSanimate(de)
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
\code{\link{OXDSplot}}
}
\examples{
\dontrun{
library(animation)
ani.options(ani.width=1012, ani.height=506)
de <- de.sim(testing=TRUE)
saveVideo(OXDSanimate(de))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
