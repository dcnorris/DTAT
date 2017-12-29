\name{xyplot.survfit}
\alias{xyplot.survfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a dose-survival curve with its confidence interval
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
  \item{data}{ignored}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
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
function (fit, ylim = NULL, ...) 
{
    if (is.null(ylim)) {
        ylim <- range(fit$time) + c(0, 0.5)
    }
    df <- with(fit, data.frame(dose = time, surv = surv, upper = upper, 
        lower = lower))
    df <- rbind(data.frame(dose = 0, surv = 1, upper = 1, lower = 1), 
        df)
    discon <- which(diff(df$surv) != 0)
    left <- df[discon, ]
    left$dose <- df$dose[discon + 1]
    df <- rbind(left, df)
    df <- df[order(df$dose), ]
    xyplot(NA ~ NA, ylab = NULL, ylab.right = "Dose level", xlab = "Fraction tolerant", 
        par.settings = list(layout.heights = list(left.padding = -1)), 
        scales = list(y = list(limits = ylim, alternating = 2, 
            rot = 90, at = seq(from = max(1, ceil(ylim[1])), 
                to = floor(ylim[2] - 0.001))), x = list(limits = c(1, 
            0), at = seq(0, 1, 0.2), cex = 0.8, rot = 90)), panel = function(...) {
            panel.xyplot(...)
            panel.lines(y = df$dose, x = df$surv, col = "black", 
                lwd = 3)
            panel.lines(y = df$dose, x = df$upper, col = "gray", 
                lty = 3, lwd = 3)
            panel.lines(y = df$dose, x = df$lower, col = "gray", 
                lty = 3, lwd = 3)
            panel.refline(v = getOption("dose.drop.threshold"))
            panel.refline(v = getOption("stop.esc.under"))
        })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
