\name{step}
\alias{step}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recursively simulate a dose titration study
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
step(de, MTDi, verbose = is.null(sys.call(-1)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{de}{
%%     ~~Describe \code{de} here~~
}
  \item{MTDi}{
%%     ~~Describe \code{MTDi} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (de, MTDi, verbose = is.null(sys.call(-1))) 
{
    if (is.null(de)) {
        de1 <- data.frame(id = integer(0), period = integer(0), 
            dose = integer(0), dlt = logical(0))
        de1[1:3, "id"] <- 1:3
        de1[1:3, "period"] <- 1
        de1[1:3, "dose"] <- 1
        de1[1:3, "dlt"] <- MTDi[1:3] < 1
        return(de1)
    }
    dsc <- ds.curve(de)
    domax <- max(de$dose)
    if (verbose) {
        cat("dose-survival curve:\n")
        print(as.data.frame(dsc))
        cat("domax =", domax, "\n")
    }
    stop.esc <- attr(de, "stop.esc")
    undo.esc <- FALSE
    if (is.null(stop.esc)) {
        stop.esc <- (getOption("stop.esc.under") > dsc$upper[domax])
        if (stop.esc) {
            undo.esc <- (getOption("undo.esc.under") > dsc$upper[domax])
            if (verbose && undo.esc) 
                cat("Whoa! Backing away from a too-high dose", 
                  domax, "\n")
        }
    }
    if (verbose) 
        cat("stop.esc =", stop.esc, "\n")
    permax <- max(de$period)
    last <- subset(de, period == permax)
    top <- subset(last, dose == max(dose))
    if (!stop.esc && sum(!top$dlt) >= 3) 
        domax <- domax + 1
    if (undo.esc) 
        domax <- domax - 1
    if (verbose) 
        cat("domax =", domax, "\n")
    if (permax == 1) {
        follow <- subset(last, !dlt)
        reduce.ids <- NULL
    }
    else {
        last2 <- subset(de, period >= permax - 1 & id <= 3 * 
            (permax - 1))
        cross.ids <- subset(summarize(group_by(last2, id), crossed = sum(dlt) == 
            1), crossed)$id
        follow <- subset(last, !(id \%in\% cross.ids) & !(dose == 
            1 & dlt))
        reduce.ids <- subset(summarize(group_by(de, id), all.dlt = all(dlt), 
            min.dose = min(dose)), all.dlt & min.dose > 1)$id
        stopifnot(all(reduce.ids \%in\% follow$id))
        if (stop.esc) {
            maxout.ids <- subset(de, period == permax & dose >= 
                domax & !dlt)$id
            follow <- subset(follow, !(id \%in\% maxout.ids))
        }
    }
    follow$period <- follow$period + 1
    follow$dose <- with(follow, ifelse(id \%in\% reduce.ids, dose - 
        1, pmin(dose + 1, domax)))
    follow$dlt <- follow$dose > MTDi[follow$id]
    enrolling.dose <- last$dose[which.max(last$id)]
    if (getOption("dose.drop.threshold") < dsc$lower[enrolling.dose]) {
        if (verbose) {
            cat(paste(dsc$lower[enrolling.dose], "<", getOption("dose.drop.threshold"), 
                "in period", permax, "-- dropping dose", enrolling.dose, 
                "\n"))
        }
        enrolling.dose <- enrolling.dose + 1
    }
    n <- length(unique(de$id))
    if (length(MTDi) > n) {
        enroll.ids <- (n + 1):min(n + 3, length(MTDi))
        enroll <- data.frame(id = enroll.ids, period = permax + 
            1, dose = enrolling.dose, dlt = MTDi[enroll.ids] < 
            enrolling.dose)
        de <- rbind(de, enroll)
    }
    if (stop.esc && is.null(attr(de, "stop.esc"))) {
        if (verbose) 
            cat("Setting 'stop.esc' attribute <-", permax, "\n")
        attr(de, "stop.esc") <- permax
    }
    rbind(de, follow)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
