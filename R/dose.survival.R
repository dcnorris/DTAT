# Construct a Surv object from a dose-escalation experiment,
# using interval-censoring constructs a la type='interval2'
dose.survival <- function(de){
  suppressMessages({
    L <- aggregate(dose ~ id, data=de, FUN=max, subset=!de$dlt)
    names(L)[2] <- 'doseL'
    suppressWarnings({ # expect min() to yield Inf often below
      R <- aggregate(dose ~ id, data=de, FUN=min, subset=de$dlt)
      names(R)[2] <- 'doseR'
    })
    ds <- full_join(L, R)
  })
  ds$doseL[is.na(ds$doseL)] <- 0
  ds$doseR[is.na(ds$doseR)] <- Inf
  ds <- arrange(ds, id)
  # Compute the Surv object from ds data frame with cols (id, doseL, doseR)
  S <- with(ds, Surv(time=doseL, time2=doseR, type='interval2'))
  S
}
