\name{OXDSplot}
\alias{OXDSplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transparent visualization of a 3+3/PC dose-titration study
}
\description{
Visualize a 3+3/PC dose-titration study
}
\usage{
OXDSplot(de, tox.pending=FALSE, periods=max(de$period), top.dose=max(de$dose),
         ox.width=0.7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{de}{A data frame describing a dose-titration study}
  \item{tox.pending}{Set TRUE to show last-period DLT assessment pending}
  \item{periods}{Number of periods of \code{de} to show}
  \item{top.dose}{Highest dose level to show on the plot}
  \item{ox.width}{Fraction of plot width occupied by the 'OX plot'}
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
\code{\link{OXplot}}, \code{\link{xyplot.survfit}}
}
\examples{
}
\keyword{hplot}
\keyword{survival}
