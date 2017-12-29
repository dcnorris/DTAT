\name{de.sim}
\alias{de.sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate a dose titration study
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
de.sim(CV = 0.7, start.dose = 0.25, dose.jump = 0.4, N = 24, periods = N/3 + 2,
       testing = is.null(sys.call(-1)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CV}{Coefficient of variation for MTDi in the population}
  \item{start.dose}{Lowest dose in geometric sequence of doses to trial}
  \item{dose.jump}{The fractional increase between dose levels}
  \item{N}{Number of simulated subjects to enroll}
  \item{periods}{Number of DLT assessment periods to simulate}
  \item{testing}{This parameters helps differentiate function behavior appropriately to interactive vs programmed invocation}
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
function (CV = 0.7, start.dose = 0.25, dose.jump = 0.4, N = 24, 
    periods = N/3 + 2, testing = is.null(sys.call(-1))) 
{
    if (testing) 
        set.seed(2017)
    shape <- CV^-2
    scale <- 1/shape
    mtd <- rgamma(N, shape = shape, scale = scale)
    MTDi <- 1 + log(mtd/start.dose)/log(1 + dose.jump)
    MTDi.last <<- MTDi
    de <- list(step(NULL, MTDi))
    for (p in 2:periods) {
        de[[p]] <- step(de[[p - 1]], MTDi)
    }
    if (testing) 
        OXDSplot(de[[periods]])
    dsc <- ds.curve(de[[periods]])
    enrolling.dose <- min(which(dsc$lower < getOption("dose.drop.threshold")))
    top.dose <- max(which(dsc$upper > getOption("undo.esc.under")))
    Ds <- start.dose * (1 + dose.jump)^(1:top.dose - 1)
    if (testing) {
        cat(paste("retained doses:\n"))
        print(data.frame(dose.level = (1:top.dose), dose = Ds))
    }
    Ds_ <- Ds/scale
    Qs <- sapply(Ds_, function(D) Rgamma(shape - 0.5, D, lower = FALSE))
    Pr <- 0.5 * gamma(shape - 0.5)/gamma(shape) * sum(Qs * diff(sqrt(c(0, 
        Ds_))))
    if (testing) 
        cat("Population rate of remission (exact):", Pr, "\n")
    if (testing) {
        Pr_50k <- numeric(10)
        for (i in 1:length(Pr_50k)) {
            mtd50k <- rgamma(50000, shape = shape, scale = scale)
            tol50k <- floor(log(mtd50k/start.dose)/log(1 + dose.jump))
            MTD50k <- start.dose * (1 + dose.jump)^tol50k
            MTD50k[tol50k < 0] <- 0
            MTD50k[tol50k > top.dose - 1] <- max(Ds)
            Pr_50k[i] <- 0.5 * mean(sqrt(MTD50k/mtd50k))
        }
        if (Pr < min(Pr_50k) || max(Pr_50k) < Pr) 
            warning("Population rate of remission (sim):", paste(range(Pr_50k), 
                collapse = "--"), "\n")
        else cat("Population rate of remission (sim):", mean(Pr_50k), 
            "±", round(sd(Pr_50k), 7), "\n")
    }
    Prs <- 0.5 * gamma(shape - 0.5)/gamma(shape) * Qs * sqrt(Ds_)
    if (testing) {
        cat(paste("Pr's for 1-size-fits-all dosing at each retained dose:\n"))
        print(data.frame(dose.level = (1:top.dose), Pr = Prs, 
            Q = Qs))
    }
    if (testing) 
        invisible(de)
    else {
        de.last <<- de
        data.frame(enrolling.dose = enrolling.dose, max.dose = top.dose, 
            effic.33pc = Pr/0.5, effic.1sfa = max(Prs)/0.5)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
