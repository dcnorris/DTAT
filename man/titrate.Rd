\name{titrate}
\alias{titrate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform Dose Titration Algorithm Tuning (DTAT)
}
\description{
Perform neutrophil-guided dose titration of a chemotherapy drug.
}
\usage{
titrate(draw.days = NULL, Tcyc = 3 * 7 * 24, Ncycles = 10,
        doserange = c(50, 500), dta = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{draw.days}{
%%     ~~Describe \code{draw.days} here~~
}
  \item{Tcyc}{
%%     ~~Describe \code{Tcyc} here~~
}
  \item{Ncycles}{
%%     ~~Describe \code{Ncycles} here~~
}
  \item{doserange}{
%%     ~~Describe \code{doserange} here~~
}
  \item{dta}{
  A Dose Titration Algorithm (DTA) to drive the titration
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
TODO: Once I've implemented several nontrivial dosing algorithms, I should
      consider whether the 'doserange' case ought to be handled by a special
      non-adaptive DTA.

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
function (draw.days = NULL, Tcyc = 3 * 7 * 24, Ncycles = 10, 
    doserange = c(50, 500), adapt.dosing = c("Newton"), omega = NA, 
    slope1 = NA, slopeU = NA) 
{
    hourly <- which(abs(time(pkpd) - round(time(pkpd))) < .Machine$double.eps^0.5)
    anc.ts <- data.frame()
    course <- expand.grid(cycle = 1:Ncycles, id = 1:nrow(pop), 
        Cc = 0, Cp = 0, Prol = NA, Tx.1 = NA, Tx.2 = NA, Tx.3 = NA, 
        Circ = NA, dose = NA, CircMin = NA, tNadir = NA, scaled.dose = NA)
    trajic <<- lapply(1:nrow(pop), function(.) list())
    for (day in draw.days) {
        newcolumn <- paste("ANC", day, sep = "_d")
        course[, newcolumn] <- NA
        units(course[, newcolumn]) <- "cells/mm^3"
        label(course[, newcolumn]) <- paste("Day-", day, " ANC", 
            sep = "")
    }
    course$dose <- seq(scaled, from = min(doserange), to = max(doserange), 
        length.out = max(course$cycle), digits = 0)[course$cycle]
    statevector <- c("Cc", "Cp", "Prol", "Tx.1", "Tx.2", "Tx.3", 
        "Circ", "CircMin", "tNadir")
    course[, statevector[-(1:2)]] <- pop$Circ0[course$id]
    for (id in 1:nrow(pop)) {
        params <- paramset(id)
        for (cycle in 1:max(course$cycle)) {
            idx <- which(course$cycle == cycle & course$id == 
                id)
            if (cycle > 1) {
                lag_1 <- which(course$cycle == (cycle - 1) & 
                  course$id == id)
                CircMin_1 <- course[lag_1, "CircMin"]
                dose_1 <- course[lag_1, "dose"]
                if (adapt.dosing == "Newton") {
                  params[paste(statevector, "0", sep = ".")] <- traj[nrow(traj), 
                    statevector]
                  params["CircMin.0"] <- params["Circ.0"]
                  params["tNadir.0"] <- 0
                  if (cycle == 2) {
                    slope <- slope1
                  }
                  else {
                    lag_2 <- which(course$cycle == (cycle - 2) & 
                      course$id == id)
                    CircMin_2 <- course[lag_2, "CircMin"]
                    dY <- log(CircMin_1/CircMin_2)
                    dose_2 <- course[lag_2, "dose"]
                    dX <- scaled(dose_1) - scaled(dose_2)
                    slope <- dY/dX
                    slope <- min(slope, slopeU)
                  }
                  delta.scaleddose <- log(500/CircMin_1)/slope
                  delta.safer <- ifelse(delta.scaleddose > 0, 
                    omega * delta.scaleddose, delta.scaleddose)
                  new.scaleddose <- scaled(dose_1) + delta.safer
                  course$dose[idx] <- uniroot(function(y) scaled(y) - 
                    new.scaleddose, c(0, 1e+05))$root
                }
            }
            params["dose"] <- course$dose[idx]
            traj <- trajectory(pkpd, params = params, as.data.frame = TRUE)
            trajic[[id]][[cycle]] <<- traj
            to.add <- data.frame(id = rep(id, length(hourly)), 
                time = traj$time[hourly] + (cycle - 1) * Tmax, 
                ANC = traj$Circ[hourly])
            anc.ts <- rbind(anc.ts, to.add)
            course[idx, statevector] <- traj[which.max(traj$time), 
                statevector]
            stopifnot(course[idx, "CircMin"] < min(traj$Circ) + 
                0.05)
            stopifnot(course[idx, "CircMin"] > min(traj$Circ) - 
                0.5)
            for (day in draw.days) {
                day.idx <- which(traj$time == day * 24)
                course[idx, paste("ANC", day, sep = "_d")] <- traj[day.idx, 
                  "Circ"]
            }
        }
    }
    course <- upData(course, id = ordered(paste("id", id, sep = ""), 
        levels = paste("id", 1:N, sep = "")), tNadir = tNadir/24, 
        scaled.dose = scaled(dose), labels = c(CircMin = "ANC nadir", 
            tNadir = "Time of ANC nadir", dose = "Drug Dose", 
            scaled.dose = "Drug Dose (rescaled)"), units = c(CircMin = "cells/mm^3", 
            tNadir = "days", dose = "mg", scaled.dose = "mg"), 
        print = FALSE)
    anc.ts <- upData(anc.ts, id = ordered(paste("id", id, sep = ""), 
        levels = paste("id", 1:N, sep = "")), time = time/(24 * 
        7), labels = c(ANC = "ANC"), units = c(ANC = "cells/mm^3", 
        time = "weeks"), print = FALSE)
    list(course = course, anc.ts = anc.ts)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
