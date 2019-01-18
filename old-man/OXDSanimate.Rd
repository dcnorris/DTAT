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
  \item{de}{A list of period-wise snapshots from a dose-titration study}
}
\value{
No value is returned. This function is intended to be passed
as the \code{expr} argument to package \code{animation} functions
such as \code{saveGIF} and \code{saveVideo}. See the example
below.
}
\author{David C. Norris}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{OXDSplot}}
\code{\link[animation]{animation}}
}
\examples{
\dontrun{
# Running this is not quite 'catastrophic', but the saveVideo()
# function may take a good fraction of a minute to run.
library(animation)
ani.options(ani.width=1012, ani.height=506)
de <- de.sim(testing=TRUE)
saveVideo(OXDSanimate(de)) # produces file "animation.mp4"
}
}
\keyword{dynamic}
