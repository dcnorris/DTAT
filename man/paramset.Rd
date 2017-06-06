\name{paramset}
\alias{paramset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set up a parameter vector from pop[id,]
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
paramset(id, states = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{states}{
%%     ~~Describe \code{states} here~~
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
function (id, states = NULL) 
{
    id <- as.integer(id)
    params <- unlist(pop[id, c("Circ0", "gamma", "Emax", "EC50", 
        "CL", "Q", "Vc", "Vp", "kTR")])
    params["sigma"] <- 0.05
    params["duration"] <- Tinfusion
    if (is.null(states)) {
        params[c("Cc.0", "Cp.0")] <- 0
        params[c("Prol.0", "Tx.1.0", "Tx.2.0", "Tx.3.0", "Circ.0", 
            "CircMin.0")] <- params["Circ0"]
        params["tNadir.0"] <- 0
    }
    else {
        statenames <- c("Cc", "Cp", "Prol", "Tx.1", "Tx.2", "Tx.3", 
            "Circ", "dose", "CircMin", "tNadir")
        stopifnot(setequal(names(states), statenames))
        params[paste(statenames, "0", sep = ".")] <- states[statenames]
        params["CircMin.0"] <- params["Circ.0"]
        params["tNadir.0"] <- 0
    }
    params["dose"] <- dose1
    unlist(params)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
