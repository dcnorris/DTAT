dose.survfit <- function(de, method="rothman", avoid.degeneracy=TRUE){
  # To avoid degeneracy, plant an artificial DLT at lowest dose (unless already present!)
  # and also do the converse (artificial 'O') at highest dose when it shows only DLTs.
  artif.x <- 0.5
  artif.o <- 0.25 # weight applied to create 'fractional' artificial individual
  weights <- rep(1, length(unique(de$id)))
  if(avoid.degeneracy){
    if(with(de, !sum(dlt[dose==1]))){
      de <- rbind(data.frame(id=0, period=0, dose=1, dlt=TRUE), de)
      weights <- c(artif.x, weights)
    }
    if(with(subset(de, dose==max(dose)), all(dlt))){
      de <- rbind(de, data.frame(id=Inf, period=max(de$period), dose=max(de$dose), dlt=FALSE))
      weights <- c(weights, artif.o)
    }
  }
  de.inspect <<- de
  S <- dose.survival(de)
  S.inspect <<- S
  fit <- survfit(S ~ 1, weights=weights)
  fit.inspect <<- fit
  stopifnot(max(fit$time) == max(de$dose) || !avoid.degeneracy) # assert degeneracy avoided
  fit <- km.ci(fit, method=method, conf.level=getOption('ds.conf.level'))
}
