#' Perform Dose Titration Algorithm Tuning (DTAT)
#' 
#' Perform neutrophil-guided dose titration of a chemotherapy drug.
#' 
#' 
#' @param draw.days %% ~~Describe \code{draw.days} here~~
#' @param Tcyc %% ~~Describe \code{Tcyc} here~~
#' @param Ncycles %% ~~Describe \code{Ncycles} here~~
#' @param doserange %% ~~Describe \code{doserange} here~~
#' @param dta A Dose Titration Algorithm (DTA) to drive the titration
#' @return A list with 2 components: \item{course}{A data frame containing
#' cycle-wise measures of each id's titration course} \item{anc.ts}{A data
#' frame detailing hourly ANC measures for each id}
#' @note TODO: Once I've implemented several nontrivial dosing algorithms, I
#' should consider whether the 'doserange' case ought to be handled by a
#' special non-adaptive DTA.
#' @author David C. Norris
titrate <-
function(draw.days=NULL, Tcyc=3*7*24, Ncycles=10,
         doserange=c(50,500), dta=NULL) {
  stopifnot(sim$N <= nrow(sim$pop))
  # Find the ANC nadirs of all 25 IDs, checking ANCs on (integer-vector) draw.days
  # We will accumulate data about each course of treatment into this data frame.
  pkpd <- sim$pkpd # Obtain a local copy
  hourly <- which(abs(time(pkpd) - round(time(pkpd))) < .Machine$double.eps^0.5)
  anc.ts <- data.frame() # This will be used to collect an hourly 'Circ' time series
  course <- expand.grid(cycle=1:Ncycles, id=1:sim$N, Cc=0.0, Cp=0.0
                        , Prol=NA, Tx.1=NA, Tx.2=NA, Tx.3=NA, Circ=NA
                        , dose=NA, CircMin=NA, tNadir=NA, scaled.dose=NA
  )
  #trajic <<- lapply(1:sim$N, function(.) list()) # collect all trajectories for inspection/debug
  for (day in draw.days) {
    newcolumn <- paste("ANC", day, sep="_d")
    course[,newcolumn] <- NA
    units(course[,newcolumn]) <- "cells/mm^3"
    label(course[,newcolumn]) <- paste("Day-",day," ANC", sep="")
  }
  course$dose <- seq(scaled, from=min(doserange), to=max(doserange), length.out=max(course$cycle), digits=0)[course$cycle]
  statevector <- c('Cc','Cp','Prol','Tx.1','Tx.2','Tx.3','Circ','CircMin','tNadir')
  course[,statevector[-(1:2)]] <- sim$pop$Circ0[course$id] # Prol(0)=Tx.1(0)=Tx.2(0)=Tx.3(0)=Circ(0):=Circ0
  for (id in 1:sim$N) { # outer loop over IDs permits state cycling
    params <- paramset(id)
    recycle.state <- NULL
    for (cycle in 1:max(course$cycle)) {
      idx <- which(course$cycle==cycle & course$id==id)
      if (!is.null(dta)) { # Override preconfigured dose
        course$dose[idx] <- dta(id, cycle, course)
        if (cycle>1)
          recycle.state <- unlist(traj[nrow(traj),statevector[1:7]]) # set components of 'real state'
      }
      params['dose'] <- course$dose[idx]
      pkpd <- pomp(pkpd, initializer = sim$inits_fac(recycle.state))
      traj <- trajectory(pkpd, params=params, as.data.frame=TRUE)
      #trajic[[id]][[cycle]] <<- traj
      to.add <- data.frame(id=rep(id,length(hourly))
                           , time=traj$time[hourly]+(cycle-1)*max(pkpd@times)
                           , ANC=traj$Circ[hourly])
      anc.ts <- rbind(anc.ts, to.add)
      course[idx,statevector] <- traj[which.max(traj$time),statevector]
      # Halt if integrated CircMin > min(traj$Circ) + eps, or is substantially less
      # if(!(course[idx,'CircMin'] < min(traj$Circ) + 0.05)){
      #   inspect.recycle.state <<- recycle.state
      #   inspect.course <<- course
      #   inspect.idx <<- idx
      #   inspect.traj <<- traj
      #   inspect.id <<- id
      # }
      if (course[idx,'CircMin'] > min(traj$Circ) + 0.5) {
        warning("In cycle ", cycle, " for id", id,
                ", CircMin=", course[idx,'CircMin'], " >> ", min(traj$Circ), "=min(traj$Circ)")
      }
      if (course[idx,'CircMin'] < min(traj$Circ) - 2.5) {
        warning("In cycle ", cycle, " for id", id,
                ", CircMin=", course[idx,'CircMin'], " << ", min(traj$Circ), "=min(traj$Circ)")
      }
      #stopifnot(course[idx,'CircMin'] < min(traj$Circ) + 2.5)
      #stopifnot(course[idx,'CircMin'] > min(traj$Circ) - 2.5)
      for (day in draw.days) {
        day.idx <- which(traj$time==day*24)
        course[idx,paste("ANC", day, sep="_d")] <- traj[day.idx,'Circ']
      }
    }
  }
  
  course$id <- ordered(paste("id",course$id,sep=""), levels=paste("id",1:sim$N,sep=""))
  course$tNadir <- course$tNadir/24
  course$scaled.dose <- scaled(course$dose)
  course <- upData(course #[order(course$cycle),]
                   #, id = ordered(paste("id",id,sep="")
                   #              ,levels=paste("id",1:sim$N,sep=""))
                   #, tNadir = tNadir/24
                   #, scaled.dose = scaled(dose)
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
                                  ,levels=paste("id",1:sim$N,sep=""))
                   , time = time/(24*7)
                   , labels=c(ANC="ANC")
                   , units=c(ANC="cells/mm^3"
                             ,time="weeks")
                   , print=FALSE
  )
  
  list(course=course, anc.ts=anc.ts)
}
