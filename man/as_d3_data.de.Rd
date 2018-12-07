\name{as_d3_data.de}
\alias{as_d3_data.de}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform the 'de' object to JSON data for D3 visualization.
}
\description{
The returned object is a JSON representation of a JavaScript object
having components 'mtd', 'trial' and 'ds' -- the last of which stands
for 'dose-survival'.
}
\usage{
\method{as_d3_data}{de}(x, ...)
}
\arguments{
  \item{x}{The 'de' object to be transformed}
  \item{\dots}{Unused; included to match signature of generic method}
}
\details{
The (unhelpful) default behavior of function \code{r2d3} is to effect this
transformation so as to reduce the size of transmitted data, then restore
the original form upon receipt using JavaScript method
\code{HTMLWidgets.dataframeToD3()}.
See \url{https://rstudio.github.io/r2d3/articles/data_conversion.html}.
}
\value{
The returned value is a JSON string corresponding to a list of data frames:
\item{mtd}{Data frame with columns c('id','mtd')}
\item{trial}{Data frame with 'id', 'period', 'dose', 'dlt'.)}
\item{ds}{Data frame with columns 'period','surv','lower','upper','dose'.}
}
\author{
David C. Norris
}
\examples{
library(DTAT)
de <- de.sim(testing=TRUE)
data <- as_d3_data.de(de)
## Read 'data' back into a list and check a few things
check <- jsonlite::fromJSON(data)
stopifnot(
  all.equal(target=list(mtd=c("id","mtd"),
                        doses=character(0),
                        dunit=NULL,
                        trial=c("id","period","dose","dlt"),
                        mtd_quantiles=NULL,
                        ds=c("period","surv","lower","upper","dose")),
            current=lapply(check, names)
            )
)
}
\keyword{manip}
\keyword{dynamic}
