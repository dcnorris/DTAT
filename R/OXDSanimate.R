# Generate an OXDSplot animation suitable for animation::saveGIF(expr=.)
OXDSanimate <- function(de){
  periods <- max(last(de)$period)
  top.dose <- max(last(de)$dose)
  for(dwell in 1:3){ # dwell on end-of-study results before animation starts
    OXDSplot(last(de))
  }
  for(period in seq_along(de)){
    OXDSplot(de[[period]], tox.pending=TRUE, periods=periods, top.dose=top.dose)
    OXDSplot(de[[period]], tox.pending=FALSE, periods=periods, top.dose=top.dose)
  }
}
