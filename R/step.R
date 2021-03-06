# Define a recursive function that implements the next period


#' Recursively simulate a '3+3/PC' dose-titration study
#' 
#' This function is invoked recursively by \code{de.sim}, to simulate a
#' complete 3+3/PC study. For details on the 3+3/PC rules, please see the
#' references below.
#' 
#' 
#' @param de A data frame describing the events of a dose-titration study
#' underway
#' @param MTDi Vector of MTDi's, indexed by participant id
#' @param verbose Logical: the default provides verbose output (for debugging
#' and instruction) when invoked interactively from the R prompt, but not when
#' invoked 'programmatically' by client code.
#' @param n.doses When specified, prevents escalation to (n+1)th dose
#' @return A data frames summarizing the events of a dose-titration study up to
#' a given DLT assessment period, having columns as follows: \itemize{
#' \item id Integer: participants are numbered in order of enrollment, starting
#' from 1
#'  \item period Integer: DLT assessment periods of the study, numbered
#' from 1
#'  \item dose Integer: dose level received by \code{id} during
#' \code{period}
#'  \item dlt Logical: did participant \code{id} experience a DLT
#' during \code{period}? }
#' @author David C. Norris
#' @seealso \code{\link{de.sim}} invokes \code{step} to simulate a full study.
#' @references Norris DC. Precautionary Coherence Unravels Dose Escalation
#' Designs. bioRxiv. December 2017:240846. doi:10.1101/240846.
#' \url{https://www.biorxiv.org/content/early/2017/12/29/240846}
#' @keywords datagen
#' @importFrom stats aggregate
#' @export
step <- function(de, MTDi, verbose=is.null(sys.call(-1)), n.doses = Inf){
  # 0. Handle the base case
  if(is.null(de)){
    de1 <- data.frame(id=integer(0),
                      period=integer(0),
                      dose=integer(0),
                      dlt=logical(0))
    de1[1:3, 'id'] <- 1:3
    de1[1:3, 'period'] <- 1
    de1[1:3, 'dose'] <- 1
    de1[1:3, 'dlt'] <- MTDi[1:3] < 1
    return(de1)
  }
  # 0. Obtain the dose-survival curve & decide (stop|undo).esc
  dsc <- ds.curve(de)
  domax <- max(de$dose) # TODO: Move this inside the IF-block below?
  if(verbose){ cat("dose-survival curve:\n"); print(as.data.frame(dsc)); cat("domax =", domax, "\n") }
  stop.esc <- attr(de,'stop.esc'); undo.esc <- FALSE
  if(is.null(stop.esc)){ # Catch the transition to the (terminal) stop.esc state
    stop.esc <- (getOption('stop.esc.under') > dsc$upper[domax])
    if(stop.esc){ # Perhaps we must undo.esc as well; let's decide...
      undo.esc <- (getOption('undo.esc.under') > dsc$upper[domax])
      if(verbose && undo.esc) cat("Whoa! Backing away from a too-high dose", domax, "\n")
    }
  }
  if(verbose) cat("stop.esc =", stop.esc, "\n")
  # 1. Obtain the *last* period in de, 'sufficient' for intra-individual escalation decisions
  permax <- max(de$period)
  last <- de[de$period == permax,]
  # 2. Determine whether there will be a dose escalation
  top <- last[last$dose == max(last$dose),]
  if(!stop.esc && sum(!top$dlt) >= 3) # simple condition: must have 3+ IDs to titrate up
  {
    domax <- domax + 1
    if(domax > n.doses){
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
    last2 <- de[de$period >= permax - 1 & de$id <= 3*(permax-1),]
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
      maxout.ids <- de[de$period == permax & de$dose >= domax & !de$dlt,]$id
      follow <- follow[!(follow$id %in% maxout.ids),]
    }
  }
  # 4. Carry forward subjects at (same | escalated | reduced) dose
  follow$period <- follow$period + 1
  follow$dose <- with(follow,
                      ifelse(id %in% reduce.ids, dose - 1, pmin(dose + 1, domax)))
  follow$dlt <- follow$dose > MTDi[follow$id]
  # 5. Abandon the lowest dose if the lower limit of 80% confidence band
  #    (i.e., 10% quantile) of computed dose-survival curve lies above 80%.
  # TODO: Move this logic up into step 1 above, right after getting 'last'
  enrolling.dose <- last$dose[which.max(last$id)] # i.e., initial dose of last-enrolled cohort
  if(getOption('dose.drop.threshold') < dsc$lower[enrolling.dose]){
    if(verbose){
      cat(paste(dsc$lower[enrolling.dose], "<", getOption('dose.drop.threshold'),
                "in period", permax, "-- dropping dose", enrolling.dose, "\n"))
    }
    enrolling.dose <- enrolling.dose + 1
  }
  # 6. Add any new subjects at lowest (remaining) dose
  n <- length(unique(de$id))
  if(length(MTDi) > n){
    enroll.ids <- (n+1):min(n+3, length(MTDi))
    # Extract info from df on whether DLTs occur at enrolling.dose
    enroll <- data.frame(id=enroll.ids,
                         period=permax+1, #follow$period[1],
                         dose=enrolling.dose,
                         dlt=MTDi[enroll.ids] < enrolling.dose)
    de <- rbind(de, enroll)
  }
  # 7. Return new value of de
  if(stop.esc && is.null(attr(de,'stop.esc'))){
    if(verbose) cat("Setting 'stop.esc' attribute <-", permax, "\n")
    attr(de,'stop.esc') <- permax
  }
  rbind(de, follow) # NB: rbind propagates attributes of 1st arg
}
