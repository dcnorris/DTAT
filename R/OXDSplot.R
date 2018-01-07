OXDSplot <- function(de, tox.pending=FALSE, periods=max(de$period),
                     top.dose=max(de$dose), ox.width=0.7){
  ylim <- range(de$dose) + c(-0.5, 0.5) # xyplot.survfit default is insufficient
  ox <- OXplot(de, tox.pending=tox.pending, periods=periods, top.dose=top.dose)
  if(tox.pending){
    # Next line causes a package-check NOTE re "global variable 'period'"
    #de <- subset(de, period < max(de$period))
    de <- de[de$period < max(de$period),]
  }
  if(top.dose < max(de$dose)){
    top.dose <- max(de$dose)
    warning(paste("Increasing top.dose to", top.dose, "to avoid lopping top off OXDSplot."))
  }
  fit <- dose.survfit(de)
  ds <- xyplot(fit, ylim=0.5+c(0, top.dose))
  print(ox, position=c(0, 0, ox.width+0.02, 1), more=TRUE)
  print(ds, position=c(ox.width-0.02, 0, 1, 1), more=FALSE)
}
