\name{xyplot.survfit}
\alias{xyplot.survfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a dose-survival curve with its confidence bounds
}
\description{
Accept a \code{survfit} object, and plot 'dose-survival curve' in the form of a complementary cumulative distribution function of dose tolerability.
}
\usage{
\method{xyplot}{survfit}(x, data = NULL, ylim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{survfit}
}
  \item{data}{Unused; included for S3 generic/method consistency.}
  \item{ylim}{Passed through to xyplot}
  \item{\dots}{Passed through to xyplot}
}
\value{A \code{lattice} plot}
\author{
David C. Norris
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ds.curve}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
\keyword{ methods }
