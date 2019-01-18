#' Generate and summarize an ensemble of simulated dose titration studies.
#' 
#' Generate and summarize an ensemble of simulated dose titration studies.
#' 
#' 
#' @param K Number of simulated studies to run
#' @param \dots Parameters to be passed through to \code{de.sim}
#' @return A data frame summarizing key outcomes of the simulated trials:
#' \item{enrolling.dose}{Enrolling dose at conclusion of study}
#' \item{max.dose}{Maximum allowed dose at conclusion of study}
#' \item{effic.33pc}{Efficiency of titration over retained doses, relative to
#' perfect 'MTDi' dosing} \item{effic.1sfa}{Efficiency of 1-size-fits-all
#' dosing, relative to perfect 'MTDi' dosing}
#' @author David C. Norris
#' @seealso \code{\link{de.sim}}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (K = 10, ...) 
#' {
#'     pb <- txtProgressBar(max = K, style = 3)
#'     df <- de.sim(...)
#'     MTDi.ensemble <<- list(MTDi.last)
#'     de.ensemble <<- list(de.last)
#'     setTxtProgressBar(pb, 1)
#'     for (k in 2:K) {
#'         df <- rbind(df, de.sim(...))
#'         MTDi.ensemble[[k]] <<- list(MTDi.last)
#'         de.ensemble[[k]] <<- de.last
#'         setTxtProgressBar(pb, k)
#'     }
#'     df
#'   }
#' 
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
ensemble <- function(K=10, ...){
  pb <- txtProgressBar(max=K, style=3)
  df <- de.sim(...)
  #MTDi.ensemble <<- list(MTDi.last)
  #de.ensemble <<- list(de.last)
  setTxtProgressBar(pb, 1)
  for(k in 2:K){
    df <- rbind(df, de.sim(...))
    #MTDi.ensemble[[k]] <<- list(MTDi.last)
    #de.ensemble[[k]] <<- de.last
    setTxtProgressBar(pb, k)
  }
  cat('\n') # finish with progress bar before interactive printing
  df
}
