#' Attach plottable variables to a dose-titration study
#' 
#' Prepare a dose-titration study data frame for plotting.
#' 
#' 
#' @param de A data frame describing a dose-titration study.
#' @param tox.pending Set TRUE to show last-period DLT assessments pending
#' @return A data frame derived from \code{de}, with additional columns as
#' follows: \item{cohort}{Factor identifying each participant's cohort}
#' \item{x}{Horizontal position at which to plot participant on OXplot}
#' \item{pch}{Integer ASCII code for participant's plotting character (per
#' period)}
#' @author David C. Norris
#' @seealso \code{\link{OXplot}}
#' @keywords hplot
#' @importFrom Hmisc upData
#' @export
add.plottables <- function(de, tox.pending=FALSE){
  de <- upData(de
               ,cohort = factor((de$id-1) %/% 3 + 1)
               ,x = 1 + (de$id-1) %% 3 + 3*(de$period-1)
               ,pch = c(111,120)[1+de$dlt] # ASCII 111='o' 120='x'
               ,print = FALSE
  )
  if(tox.pending){ # support display of last period *pending* DLT assessment
    de$pch[de$period == max(de$period)] <- 63 # ASCII '?'
  }
  # Find carried-forward doses and substitute '.' for 'o'
  de <- de[with(de, order(id, period)),]
  ix <- with(de, which(id==lag(id) & dose==lag(dose)))
  de$pch[ix] <- 20 # a smaller filled circle
  # Find and splay overlapping symbols
  for(x in unique(de$x)){
    for(dose in unique(de$dose)){
      ix <- which(de$x==x & de$dose==dose)
      n <- length(ix)
      ord <- order(de$id[ix]) # Ensure left-right splay preserves id-order
      if(n > 1)
        de$x[ix] <- de$x[ix] + seq(from=-0.12, to=0.12, length.out=n)[ord]
    }
  }
  de
}
