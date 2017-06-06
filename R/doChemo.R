doChemo <-
function(draw.days=NULL, Tcyc=3*7*24, Ncycles=10, doserange=c(50,500),
                    adapt.dosing=c('Newton'), omega=NA, slope1=NA, slopeU=NA) {
  # Find the ANC nadirs of all 25 IDs, checking ANCs on (integer-vector) draw.days
  # We will accumulate data about each course of treatment into this data frame.
  hourly <- which(abs(time(pkpd) - round(time(pkpd))) < .Machine$double.eps^0.5)
  anc.ts <- data.frame() # This will be used to collect an hourly 'Circ' time series
  course <- expand.grid(cycle=1:Ncycles, id=1:nrow(pop), Cc=0.0, Cp=0.0
                        , Prol=NA, Tx.1=NA, Tx.2=NA, Tx.3=NA, Circ=NA
                        , dose=NA, CircMin=NA, tNadir=NA, scaled.dose=NA
                        )
  trajic <<- lapply(1:nrow(pop), function(.) list()) # collect all trajectories for inspection/debug
  for (day in draw.days) {
    newcolumn <- paste("ANC", day, sep="_d")
    course[,newcolumn] <- NA
    units(course[,newcolumn]) <- "cells/mm^3"
    label(course[,newcolumn]) <- paste("Day-",day," ANC", sep="")
  }
  course$dose <- seq(scaled, from=min(doserange), to=max(doserange), length.out=max(course$cycle), digits=0)[course$cycle]
  statevector <- c('Cc','Cp','Prol','Tx.1','Tx.2','Tx.3','Circ','CircMin','tNadir')
  course[,statevector[-(1:2)]] <- pop$Circ0[course$id] # Prol(0)=Tx.1(0)=Tx.2(0)=Tx.3(0)=Circ(0):=Circ0
  for (id in 1:nrow(pop)) { # outer loop over IDs permits state cycling
    params <- paramset(id)
    for (cycle in 1:max(course$cycle)) {
      idx <- which(course$cycle==cycle & course$id==id)
      if (cycle>1) {
        lag_1 <- which(course$cycle==(cycle-1) & course$id==id)
        CircMin_1 <- course[lag_1,'CircMin']
        dose_1 <- course[lag_1,'dose']
        if (adapt.dosing=='Newton') { # Override preconfigured dose
          params[paste(statevector,'0',sep='.')] <- traj[nrow(traj),statevector] # recycle end-state
          params['CircMin.0'] <- params['Circ.0'] # ..according the 'pseudo-states' CircMin and
          params['tNadir.0'] <- 0.0               #   tNadir the necessary special treatment.
          if (cycle==2) {
            slope <- slope1
          } else { # cycle >= 3 so we also have lag -2 to look back at
            lag_2 <- which(course$cycle==(cycle-2) & course$id==id)
            CircMin_2 <- course[lag_2,'CircMin']
            dY <- log(CircMin_1 / CircMin_2)
            dose_2 <- course[lag_2,'dose']
            dX <- scaled(dose_1) - scaled(dose_2)
            slope <- dY/dX
            slope <- min(slope, slopeU) # bound dY/dX away from zero in face of possible hysteresis
          }
          delta.scaleddose <- log(500 / CircMin_1) / slope
          # For safety's sake, we (asymmetrically) apply relaxation factor 'omega' to any proposed dose increase:
          delta.safer <- ifelse(delta.scaleddose > 0
                                , omega*delta.scaleddose
                                , delta.scaleddose)
          new.scaleddose <- scaled(dose_1) + delta.safer
          course$dose[idx] <- uniroot(function(y) scaled(y)-new.scaleddose, c(0,100000))$root
        } 
      }
      params['dose'] <- course$dose[idx]
      traj <- trajectory(pkpd, params=params, as.data.frame=TRUE)
      trajic[[id]][[cycle]] <<- traj
      to.add <- data.frame(id=rep(id,length(hourly))
                           , time=traj$time[hourly]+(cycle-1)*Tmax
                           , ANC=traj$Circ[hourly])
      anc.ts <- rbind(anc.ts, to.add)
      course[idx,statevector] <- traj[which.max(traj$time),statevector]
      # Halt if integrated CircMin > min(traj$Circ) + eps, or is substantially less
      stopifnot(course[idx,'CircMin'] < min(traj$Circ) + 0.05)
      stopifnot(course[idx,'CircMin'] > min(traj$Circ) - 0.5)
      for (day in draw.days) {
        day.idx <- which(traj$time==day*24)
        course[idx,paste("ANC", day, sep="_d")] <- traj[day.idx,'Circ']
      }
    }
  }
  
  course <- upData(course #[order(course$cycle),]
                   , id = ordered(paste("id",id,sep="")
                          ,levels=paste("id",1:N,sep=""))
                   , tNadir = tNadir/24
                   , scaled.dose = scaled(dose)
                   , labels=c(CircMin="ANC nadir"
                              ,tNadir="Time of ANC nadir"
                              ,dose="Drug Dose"
                              ,scaled.dose="Drug Dose (rescaled)")
                   , units=c(CircMin="cells/mm^3"
                             ,tNadir="days"
                             ,dose="mg"
                             ,scaled.dose="mg")
                   , print=FALSE
  )

  anc.ts <- upData(anc.ts
                   , id = ordered(paste("id",id,sep="")
                          ,levels=paste("id",1:N,sep=""))
                   , time = time/(24*7)
                   , labels=c(ANC="ANC")
                   , units=c(ANC="cells/mm^3"
                             ,time="weeks")
                   , print=FALSE
  )
  
  list(course=course, anc.ts=anc.ts)
}
