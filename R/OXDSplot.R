OXDSplot <- function(de, tox.pending=FALSE, ..., ox.width=0.7){
  ylim <- range(de$dose) + c(-0.5, 0.5) # xyplot.survfit default is insufficient
  ox <- OXplot(de, tox.pending, ...)
  if(tox.pending){
    de <- subset(de, period < max(de$period))
  }
  fit <- dose.survfit(de)
  ds <- xyplot(fit, ylim)
  print(ox, position=c(0, 0, ox.width+0.02, 1), more=TRUE)
  print(ds, position=c(ox.width-0.02, 0, 1, 1), more=FALSE)
}
