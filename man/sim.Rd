\name{sim}
\alias{sim}
\docType{data}
\title{
Environment for simulation global variables.
}
\description{
To simplify the code of package:DTAT, as well as client tasks,
this exported environment contains a handful of global variables
useful for the simulations.
}
%%\usage{data("sim")}
%%\format{
%%  The format is:
%%<environment: 0x738c7b8> 
%%}
\details{
Global variables maintained within environment \code{sim} are:
\enumerate{
    \item \code{pkpd}: The population PK/PD model to be simulated.
    \item \code{pop}: A sample drawn from the population model.
    \item \code{N}: Restricts simulation to first \code{N} subjects in \code{pop}.
    \item \code{inits_fac}: An initializer factory taking an initial state as argument.
    \item \code{params.default}: Default parameters.
    \item Regular lines start with a non-whitespace character.
    \item Records are separated by one or more empty lines.
}
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
# Even when nrow(pop) is large, one may easily restrict
# time-consuming simulations to pop[1:N,], as follows:
sim$N <- 25
# Now perform simulation work
\dontrun{doChemo(...)}
}
\keyword{datasets}
