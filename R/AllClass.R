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

#' An S4 class for simulating dose-titration study designs
#'
#' @slot doses A numeric vector of prospectively-determined discrete doses to trial. 
#' @slot units A string indicating dose units, e.g. `"mg/kg"`. 
#' @slot MTDi A numeric vector of optimal doses for simulated study participants.
#'  Optionally a call to an `r<distribution>(...)` function which may be parsed
#'  to calculated the `mtd_quantiles` slot. 
#' @slot mtd_quantiles A numeric vector of quantiles of the distribution
#'  from which the MTDi slot was simulated. Intended mainly to support
#'  visualization of this distribution, e.g. as an transparent overlay
#'  on the dose-survival plot. NULL in case `MTDi` is provided verbatim.
#' @slot data A data.frame with columns **TODO**. 
#' @slot stop_esc integer **TODO**. 
#' @slot ds_conf_level numeric **TODO**. 
#' @slot dose_drop_threshold numeric **TODO**. 
#' @slot stop_esc_under numeric **TODO**. 
#' @slot undo_esc_under numeric **TODO**. 
#'
#' @export
setClass("DE"
        , slots = c(doses = "numeric"
                   ,units = "character"
                   ,MTDi = "numeric"
                   ,mtd_quantiles = "numeric"
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
      # Generate an mtd_quantiles slot if possible
      if(is.call(Q <- substitute(MTDi)) && substr(Q[[1]],1,1)=='r'){
        Q1 <- as.character(Q[[1]])
        substr(Q1,1,1) <- 'q'
        Q[[1]] <- as.name(Q1)
        Q[[2]] <- as.name(".p_")
        env <- parent.frame(n = 3) # caller is 3 frames back!
        assign(".p_", 1:49/50, envir = env)
        .Object@mtd_quantiles <- eval(Q, envir = env)
      } else {
        .Object@mtd_quantiles <- numeric(0)
      }
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
      # Attach selected ... arguments as slots
      dots <- list(...)
      if(!is.null(dots$units)){
        .Object@units = dots$units
      }
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
      else {
        stop.esc <- x@stop_esc
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
        all.dlt <- aggregate(dlt ~ id, data=x@data, FUN=all)
        min.dose <- aggregate(dose ~ id, data=x@data, FUN=min)
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
      if(length(x@MTDi) > n){
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
        x@stop_esc <- as.integer(permax)
      }
      x@data <- rbind(x@data, follow)
      x
    })

#' Convert a DE object to JSON
#'
#' @docType methods
#' @param x An object of S4 class 'DE'
#' @param \dots Unused; included to match signature of generic method
#'
#' @importFrom stats lm
#' @export
setMethod("as_d3_data", "DE",
    function(x, ...){
      # Assemble a data list suitable for passing in r2d3(data=).
      #
      # Utility function for converting 'actual' doses (expressed in mg/kg, say)
      # to corresponding 'ordinal' doses on a logarithmically spaced scale.
      # Log-linear interpolation is used within the range of the @doses slot,
      # and a log-linear regression is employed to extrapolate beyond.
      # TODO: Consider converting this to a DE method,
      #       or even attaching it as a slot.
      rel_dose <- function(act_dose){
        rel <- approx(x=log(x@doses)
                     ,y=seq(length(x@doses))
                     ,xout=log(act_dose)
                     ,method="linear")$y
        dose.mult <- exp(stats::lm(log(x@doses) ~ seq(along=x@doses))$coef[2])
        extrapolated <- 1 + log(act_dose/x@doses[1]) / log(dose.mult)
        where_na <- is.na(rel)
        rel[is.na(rel)] = extrapolated[is.na(rel)]
        rel
      }
      data <- list(mtd = data.frame(id = seq_along(x@MTDi)
                                    ,mtd = x@MTDi
                                    ,doscale = rel_dose(x@MTDi)
                                    ,fractol = rep(0.5, length(x@MTDi))
                                    )
                   ,doses = x@doses
                   ,dunit = x@units
                   ,trial = x@data
                   ,stop_esc = x@stop_esc
                   ,mtd_quantiles = rel_dose(x@mtd_quantiles)
                   ,ds = vector("list", max(x@data$period))
                   )
      # Fill out the $ds component
      for(period in 1:length(data$ds)){
        dsc <- as.data.frame(ds.curve(x@data[x@data$period <= period,]))
        dsc$dose <- seq(nrow(dsc))
        # TODO: Delegate the following data 'tweak'
        #       to the visualization code, since the
        #       need for it arises purely from
        #       visualization considerations:
        dsc <- dsc[c(1,1:nrow(dsc)),] # duplicate dose=1 row
        dsc$dose[1] <- 0.5
        data$ds[[period]] <- dsc
      }

      data$ds <- rbindlist(data$ds, idcol="period")
      # NB: We use a *generic* transformation to JSON,
      #     and not r2d3's special default approach
      #     designed to reduce size of data transmitted:
      jsonlite::toJSON(data)
    })

setMethod("plot", c("DE","missing"),
    function(x, y, ...){
      # TODO: Consider how to make 'viewer' explicit in the signature,
      #       or at least communicate it in a standard way in the function
      #       documentation.
      r2d3::r2d3(data=as_d3_data(x),
                 script=system.file("htmlwidgets/lib/main.js", package="DTAT"),
                 d3_version = 4, container = "div",
                 dependencies = system.file(paste("htmlwidgets/lib",
                                                  c("margins.js", "exx.js",
                                                    "ox-plot.js", "ds-plot.js"),
                                                  sep="/"), package="DTAT"),
                 #viewer="internal",
                 ...)
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

#' Temporary test of 'DE' class
#' 
#' TODO: Implement a 'sim.DE'-like function and a more comprehensive
#'       suite of tests.
#' 
#' @param seed RNG seed
#'
#' @param CV Coefficient of variation of Gamma-distributed MTDi
#' @param mean_mtd Mean of Gamma-distributed MTDi
#' @param start.dose Lowest (starting) dose to trial
#' @param dose.jump Proportional step between geometrically-spaced doses
#'
#' @export
test.DE <- function(seed=2017, CV=0.7, mean_mtd=1.0,
                    start.dose=0.25, dose.jump=0.4){
  set.seed(seed)
  # Generate N individual MTDi's from Gamma(alpha=CV^-2, beta=alpha/mean_mtd)
  shape <- CV^-2
  scale <- mean_mtd/shape # for Gamma dist, mean = shape*scale = alpha/beta
  N <- 24
  #mtd <- rgamma(N, shape=shape, scale=scale)
  trial <- new("DE", doses=0.25*1.4^(0:6),
               MTDi=rgamma(N, shape=shape, scale=scale),
               units="mg/kg")
  for(period in 2:10){
    trial <- step_time(trial)
  }

  old.way <- de.sim(testing=TRUE)
  stopifnot(all(trial@data == old.way[[10]]))
  cat("New DE class has passed a regression test!\n")
  
  invisible(trial)
}
