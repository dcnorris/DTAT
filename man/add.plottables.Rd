\name{add.plottables}
\alias{add.plottables}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Attach plottable variables to a dose titration study
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add.plottables(de, tox.pending=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{de}{A data frame describing a dose-titration study.}
  \item{tox.pending}{Set TRUE to show last period DLT assessments pending}
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
function (de) 
{
    de <- upData(de, cohort = factor((id - 1)\%/\%3 + 1), x = 1 + 
        (id - 1)\%\%3 + 3 * (period - 1), pch = c(111, 120)[1 + 
        dlt], print = FALSE)
    de <- arrange(de, id, period)
    ix <- with(de, which(id == lag(id) & dose == lag(dose)))
    de$pch[ix] <- 20
    for (x in unique(de$x)) {
        for (dose in unique(de$dose)) {
            ix <- which(de$x == x & de$dose == dose)
            n <- length(ix)
            ord <- order(de$id[ix])
            if (n > 1) 
                de$x[ix] <- de$x[ix] + seq(from = -0.12, to = 0.12, 
                  length.out = n)[ord]
        }
    }
    de
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
