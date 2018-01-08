# Construct a Surv object from a dose-escalation experiment,
# using interval-censoring constructs a la type='interval2'
dose.survival <- function(de){
  de <- de[,c('id','dose','dlt')]
  # TODO: Use plainer code (like summary.default?) to avoid 'no visible binding'
  #       complaints around max(dose) during package check.
  suppressMessages({
    L <- summarize(group_by(de[!de$dlt,], id), doseL=max(dose))
    suppressWarnings( # expect min() to yield Inf often below
      R <- summarize(group_by(de[de$dlt,], id), doseR=min(dose))
    )
    ds <- full_join(L, R)
  })
  ds$doseL[is.na(ds$doseL)] <- 0
  ds$doseR[is.na(ds$doseR)] <- Inf
  ds <- arrange(ds, id)
  #ds.debug <<- ds
  # Compute the Surv object
  S <- with(ds, Surv(time=doseL, time2=doseR, type='interval2'))
  S
}
