paramset <-
function(id, states=NULL){
  id <- as.integer(id)
  params <- unlist(pop[id,c('Circ0','gamma','Emax','EC50','CL','Q','Vc','Vp','kTR')])
  params['sigma'] <- 0.05
  params['duration'] <- Tinfusion
  if (is.null(states)) {
    params[c('Cc.0','Cp.0')] <- 0.0
    params[c('Prol.0','Tx.1.0','Tx.2.0','Tx.3.0','Circ.0','CircMin.0')] <- params['Circ0']
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
