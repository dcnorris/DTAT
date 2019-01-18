\name{plot.de}
\alias{plot.de}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
D3 visualization for dose-escalation/titration studies.
}
\description{
An interactive visualization, including many mouseover interactions
and clickable dividers between the DLT assessment periods.
}
\usage{
\method{plot}{de}(x, viewer=c("internal","external","browser"), ...)
}
\arguments{
  \item{x}{The 'de' object to be transformed}
  \item{viewer}{"internal" to use the RStudio internal viewer pane
  for output; "external" to display in an external RStudio window;
  "browser" to display in an external browser.}
  \item{\dots}{Additional parameters passed to \code{r2d3}}
}
\details{
TODO: Detail all interactions supported, and their significance.
}
\value{
No value is returned. A plot is displayed.
}
\author{
David C. Norris
}
\examples{
\donttest{
library(DTAT)
de <- de.sim(testing=TRUE)
plot(de)
}
}
\keyword{hplot}
\keyword{dynamic}
