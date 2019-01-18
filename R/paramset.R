#' Set up a parameter vector from sim$pop\[id,\]
#' 
#' Obtain a parameter vector for given id from global \code{sim$pop} data
#' frame.
#' 
#' TODO: This is really a straightforward utility function, apparently used only
#' once, in function \code{titrate}. Perhaps I should have made it an internal
#' function!
#' 
#' @param id Subject identifier, an index into \code{pop} global variable.
#' @param states This looks like a 'legacy' parameter, no longer used. TODO:
#' Factor it out?
#' @param Tinfusion Duration of infusion, in hours.
#' @param dose1 Infusion dose.
#' @return A named parameter vector, suitable a the \code{params} argument for
#' \code{pomp::trajectory}
#' @author David C. Norris
#' @seealso \code{\link[pomp]{trajectory}}
#' 
#' @export
paramset <-
function(id, states=NULL, Tinfusion=1.0, dose1=50){
  id <- as.integer(id)
  params <- unlist(sim$pop[id,c('Circ0','gamma','Emax','EC50','CL','Q','Vc','Vp','kTR')])
  params['sigma'] <- 0.05
  params['duration'] <- Tinfusion
  if (is.null(states)) {
    params[c('Cc.0','Cp.0')] <- 0.0
    params['tNadir.0'] <- 0.0
  } else {
    statenames <- c('Cc','Cp','Prol','Tx.1','Tx.2','Tx.3','Circ','dose','CircMin','tNadir')
    stopifnot(setequal(names(states), statenames))
    params[paste(statenames,'0',sep='.')] <- states[statenames]
    # The 'pseudo-states' CircMin and tNadir, however, require special treatment:
    params['CircMin.0'] <- params['Circ.0']
    params['tNadir.0'] <- 0.0
  }
  params['dose'] <- dose1
  unlist(params)
}
