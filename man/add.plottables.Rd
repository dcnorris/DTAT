\name{add.plottables}
\alias{add.plottables}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Attach plottable variables to a dose-titration study
}
\description{
Prepare a dose-titration study data frame for plotting.
}
\usage{
add.plottables(de, tox.pending=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{de}{A data frame describing a dose-titration study.}
  \item{tox.pending}{Set TRUE to show last-period DLT assessments pending}
}
\value{
A data frame derived from \code{de}, with additional columns as follows:
\item{cohort}{Factor identifying each participant's cohort}
\item{x}{Horizontal position at which to plot participant on OXplot}
\item{pch}{Integer ASCII code for participant's plotting character (per period)}
}
\author{David C. Norris}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{OXplot}}
}
\keyword{hplot}
