library(methods)
# AllClass.R
#
# To achieve greater clarity through this refactoring, I must first
# abstract the object(s) that I now pass around under the unfortunate
# name 'de'.
# Let us allow -doses- and -MTDi- to be specified in absolute terms,
# with the conversion to dose-number scale being done internally.
# Note that the -de- list I build in 'step' is hardly necessary,
# except for the odd design decision to use attr(.,'stop.esc')
# for remembering when certain decisions got made. A proper class
# will use specially designated slots to hold such info!
setClass("DE"
        , slots = c(doses = "numeric"
                   ,MTDi = "numeric"
                   ,data = "data.frame"  # like the former 'de[[n]]'
                   ,stop_esc = "integer" # period(s?) when escalation stops
                   ,ds_conf_level = "numeric"
                   ,dose_drop_threshold = "numeric"
                   ,stop_esc_under = "numeric"
                   ,undo_esc_under = "numeric"
                   )
        )

setMethod("initialize", "DE",
    function(.Object, doses, MTDi, ...){
      .Object <- callNextMethod(.Object, ...) # invoke the default method
      .Object@doses <- doses
      .Object@MTDi <- MTDi
      .Object@data <- data.frame(id=integer(0),
                                 period=integer(0),
                                 dose=integer(0),
                                 dlt=logical(0))
      .Object@data[1:3, 'id'] <- 1:3
      .Object@data[1:3, 'period'] <- 1
      .Object@data[1:3, 'dose'] <- 1
      .Object@data[1:3, 'dlt'] <- .Object@MTDi[1:3] < .Object@doses[1]
      .Object@stop_esc <- as.integer(NA)
      .Object@ds_conf_level = 0.8
      .Object@dose_drop_threshold = 0.8
      .Object@stop_esc_under = 1/3
      .Object@undo_esc_under = 1/4
      .Object
    })

setGeneric("step_time", def=function(x, ...) NULL)

setMethod("step_time", "DE",
    function(x, verbose=is.null(sys.call(-1))){
      # 0. Obtain the dose-survival curve & decide (stop|undo).esc
      dsc <- ds.curve(x@data)
      domax <- max(x@data$dose) # TODO: Move this inside the IF-block below?
      if(verbose){ cat("dose-survival curve:\n"); print(as.data.frame(dsc)); cat("domax =", domax, "\n") }
      # NB: I'll use dotted local variables 'stop.esc' and 'undo.esc',
      #     to preserve distinction against underscored @stop_esc slot.
      undo.esc <- FALSE
      if(is.na(x@stop_esc)){ # Catch the transition to the (terminal) stop.esc state
        stop.esc <- (x@stop_esc_under > dsc$upper[domax])
        if(stop.esc){ # Perhaps we must undo.esc as well; let's decide...
          undo.esc <- (x@undo_esc_under > dsc$upper[domax])
          if(verbose && undo.esc) cat("Whoa! Backing away from a too-high dose", domax, "\n")
        }
      }
      if(verbose) cat("stop.esc =", stop.esc, "\n")
      # 1. Obtain the *last* period in de, 'sufficient' for intra-individual escalation decisions
      permax <- max(x@data$period)
      last <- x@data[x@data$period == permax,]
      # 2. Determine whether there will be a dose escalation
      top <- last[last$dose == max(last$dose),]
      if(!stop.esc && sum(!top$dlt) >= 3) # simple condition: must have 3+ IDs to titrate up
      {
        domax <- domax + 1
        if(domax > length(x@doses)){
          domax <- domax - 1 # "Nope."
          # Treat a pre-set max dose WLOG as though 'found' during trial:
          stop.esc <- TRUE
        }
      }
      if(undo.esc)
        domax <- domax - 1
      if(verbose) cat("domax =", domax, "\n")
      # 3. Drop individuals who have crossed their MTDi going in either direction,
      #    or who cannot tolerate the lowest dose on trial. (Note that these IDs
      #    will still be counted in the dose-survival curve; the point is simply
      #    that we do not need to carry them forward any further for purposes of
      #    visualization or escalation/dose-dropping decisions.)
      if(permax == 1){ # trivial case - drop 1st-period DLTs
        follow <- last[!last$dlt,]
        reduce.ids <- NULL
      } else { # we have 2 periods to look back on
        last2 <- x@data[x@data$period >= permax - 1 & x@data$id <= 3*(permax-1),]
        # For IDs who have crossed their MTDi's, we will have sum(dlt) = T+F (or F+T) = 1.
        # Otherwise, we'll have sum(dlt) = F+F = 0 or T+T = 2.
        crossings <- aggregate(dlt ~ id, data=last2, FUN=function(dlt) sum(dlt)==1)
        cross.ids <- crossings[crossings$dlt,]$id
        follow <- last[!(last$id %in% cross.ids) & !(last$dose==1 & last$dlt),]
        all.dlt <- aggregate(dlt ~ id, data=de, FUN=all)
        min.dose <- aggregate(dose ~ id, data=de, FUN=min)
        reduce.ids <- intersect(all.dlt[all.dlt$dlt,]$id, min.dose[min.dose$dose>1,]$id)
        # Verify that all reduce.ids are retained in 'follow':
        stopifnot(all(reduce.ids %in% follow$id))
        # Omit top-dose finalizers if we have stopped escalation
        if(stop.esc){
          maxout.ids <- x@data[x@data$period == permax & x@data$dose >= domax
                               & !x@data$dlt,]$id
          follow <- follow[!(follow$id %in% maxout.ids),]
        }
      }
      # 4. Carry forward subjects at (same | escalated | reduced) dose
      follow$period <- follow$period + 1
      follow$dose <- with(follow,
                          ifelse(id %in% reduce.ids, dose - 1, pmin(dose + 1, domax)))
      follow$dlt <- x@doses[follow$dose] > x@MTDi[follow$id]
      # 5. Abandon the lowest dose if the lower limit of 80% confidence band
      #    (i.e., 10% quantile) of computed dose-survival curve lies above 80%.
      # TODO: Move this logic up into step 1 above, right after getting 'last'
      enrolling.dose <- last$dose[which.max(last$id)] # i.e., initial dose of last-enrolled cohort
      if(x@dose_drop_threshold < dsc$lower[enrolling.dose]){
        if(verbose){
          cat(paste(dsc$lower[enrolling.dose], "<", x@dose_drop_threshold,
                    "in period", permax, "-- dropping dose", enrolling.dose, "\n"))
        }
        enrolling.dose <- enrolling.dose + 1
      }
      # 6. Add any new subjects at lowest (remaining) dose
      n <- length(unique(x@data$id))
      if(length(x@MTDi) > x@doses[n]){
        enroll.ids <- (n+1):min(n+3, length(x@MTDi))
        # Extract info from df on whether DLTs occur at enrolling.dose
        enroll <- data.frame(id=enroll.ids,
                             period=permax+1, #follow$period[1],
                             dose=enrolling.dose,
                             dlt=x@MTDi[enroll.ids] < x@doses[enrolling.dose])
        x@data <- rbind(x@data, enroll)
      }
      # 7. Return new value of de
      if(stop.esc && is.na(x@stop_esc)){
        if(verbose) cat("Setting 'stop.esc' attribute <-", permax, "\n")
        x@stop_esc <- permax
      }
      x@data <- rbind(x@data, follow)
      x
    })

setClass("DiscreteDoseTitrationDesign"
        # Let this be a generic class for all dose-titration
        # (or -escalation as a special case!) designs established
        # over a fixed set of discrete doses.
        , slots = c(doses = "numeric"
                   ,dose_unit = "character"
                   ,cohort_size = "integer")
        )

# I hope to see the 'step()' function broken down into distinct,
# simple, clearly-named and easily-understood methods. That may
# serve as the best motivation for my immediate design!
setClass("3+3/PC"
         , contains = "DiscreteDoseTitrationDesign"
         #, slots = c(placeholder="NA")
         )

setClass("DoseSurvivalCurve"
        #, slots = c(placeholder="NA")
        )

# TODO: Consider this class as a way to incorporate an MTDi distribution
#       and dose-response curve into a complete simulation scenario
#       amenable to multifaceted analysis.
setClass("DoseTitrationScenario"
        #, slots = c(placeholder="NA")
        )

# Implement a test of the step_time("DE") method.
# This should verify that we get the same result
# as from the 'step' function.
test.DE <- function(seed=2017, CV=0.7, mean_mtd=1.0,
                    start.dose=0.25, dose.jump=0.4){
  set.seed(seed)
  # Generate N individual MTDi's from Gamma(alpha=CV^-2, beta=alpha/mean_mtd)
  shape <- CV^-2
  scale <- mean_mtd/shape # for Gamma dist, mean = shape*scale = alpha/beta
  N <- 24
  mtd <- rgamma(N, shape=shape, scale=scale)
  trial <- new("DE", doses=0.25*1.4^(0:6), MTDi=mtd)
  trial <- step_time(trial)
  trial
}
