#' Simulate a '3+3/PC' dose-titration study
#' 
#' Please see the reference cited below.
#' 
#' 
#' @param CV Coefficient of variation for MTDi in the population
#' @param mean_mtd Mean MTDi in the population
#' @param start.dose Lowest dose in geometric sequence of doses to trial
#' @param dunit A string specifying the dose unit
#' @param dose.jump The fractional increase between dose levels
#' @param N Number of simulated subjects to enroll
#' @param periods Number of DLT assessment periods to simulate
#' @param testing This parameter helps differentiate function behavior (type of
#' value returned) appropriately to interactive vs programmed invocation
#' @param once When TRUE, does a single simulation returning \code{de} object
#' @param \dots Additional arguments passed to function \code{step}
#' @return When once=TRUE, a list of data frames summarizing the events of a
#' dose-titration study up to a given DLT assessment period. Thus,
#' \code{de[[3]]} is a data frame with columns as follows:
#'  \itemize{
#' \item id Integer: participants are numbered in order of enrollment, starting
#' from 1
#' \item period Integer: DLT assessment periods of the study, numbered
#' from 1
#' \item dose Integer: dose level received by \code{id} during
#' \code{period}
#' \item dlt Logical: did participant \code{id} experience a DLT
#' during \code{period}? } For periods after dose escalation stops, a
#' 'stop.esc' attribute will be attached to subsequent period elements. Thus,
#' if escalation stops at the end of period 7, \code{attr(de[[8]],'stop.esc')}
#' will equal 7, and this will be true for all subsequent periods as well.
#' 
#' When testing=FALSE, a simple summary result is returned, suitable for
#' aggregating an ensemble of simulations.
#' @author David C. Norris
#' @seealso \code{\link{step}}, which \code{de.sim} calls recursively
#' @references Norris DC. Precautionary Coherence Unravels Dose Escalation
#' Designs. bioRxiv. December 2017:240846. doi:10.1101/240846.
#' \url{https://www.biorxiv.org/content/early/2017/12/29/240846}
#' @keywords datagen
#' @examples
#' 
#' # Perform a regression test for package:DTAT
#' de.compare <- de.sim(testing=TRUE)
#' # Strip down to basics from v0.2-1
#' attributes(de.compare) <- NULL
#' class(de.compare) <- "list"
#' data(de.bioRxiv.240846)
#' stopifnot(identical(de.compare, de.bioRxiv.240846))
#' 
#' @importFrom stats pgamma qgamma rgamma sd
#' @importFrom zipfR Rgamma
#' @export
de.sim <- function(CV=0.7, mean_mtd=1.0, start.dose=0.25, dunit="mg", dose.jump=0.4, N=24, periods=N/3+2,
                   testing=is.null(sys.call(-1)), once=testing, ...){
  # Invoked interactively, the function aims to support reproducible, interactive testing.
  # Otherwise (the intended default) it simply returns simulation summaries that the caller
  # is supposed to be interested in aggregating.
  if(testing)
    set.seed(2017)
  # 1. Generate N individual MTDi's from Gamma(alpha=CV^-2, beta=alpha/mean_mtd)
  shape <- CV^-2
  scale <- mean_mtd/shape # for Gamma dist, mean = shape*scale = alpha/beta
  mtd <- rgamma(N, shape=shape, scale=scale)
  # 2. Transform these MTDi's onto the geometric dose range
  MTDi <- 1 + log(mtd/start.dose) / log(1+dose.jump)
  #MTDi.last <<- MTDi
  # 3. Iteratively build a de *list* (de[[1]], de[[2]], ..., de[[periods]])
  de <- list(step(NULL, MTDi, ...))
  for(p in 2:periods){
    de[[p]] <- step(de[[p-1]], MTDi, ...)
  }
  # 4. Do an OXDSplot (in the interactive K=1 case)
  if(once) OXDSplot(de[[periods]])
  # 5. Determine the dose levels retained, based on study-end DS curve
  dsc <- ds.curve(de[[periods]])
  enrolling.dose <- min(which(dsc$lower < getOption('dose.drop.threshold')))
  top.dose <- max(which(dsc$upper > getOption('undo.esc.under')))
  Ds <- start.dose * (1+dose.jump)^(1:top.dose - 1)
  if(once){ # ...and print them in interactive K=1 case
    cat(paste("retained doses:\n"))
    print(data.frame(dose.level=(1:top.dose), dose=Ds))
  }
  # 6. Do an (Epilogue Eq. 12)-type integral over retained dose levels
  Ds_ <- Ds / scale # cf. normalized beta~ := beta*D in 'Cost' preprint
  Qs <- sapply(Ds_, function(D) Rgamma(shape-0.5, D, lower=FALSE))
  Pr <- 0.5 * gamma(shape-0.5) / gamma(shape) * sum( Qs * diff(sqrt(c(0,Ds_))) )
  if(testing) cat("Population rate of remission (exact):", Pr, "\n")
  # When -testing-, double-check this calculation by brute-force simulation
  if(once){
    Pr_50k <- numeric(10)
    for(i in 1:length(Pr_50k)){
      mtd50k <- rgamma(50000, shape=shape, scale=scale)
      tol50k <- floor(log(mtd50k/start.dose) / log(1+dose.jump)) # highest level tolerated
      MTD50k <- start.dose * (1+dose.jump)^tol50k
      MTD50k[tol50k < 0] <- 0 # can't even tolerate start.dose
      MTD50k[tol50k > top.dose-1] <- max(Ds)
      Pr_50k[i] <- 0.5*mean(sqrt(MTD50k/mtd50k))
    }
    if(Pr < min(Pr_50k) || max(Pr_50k) < Pr)
      warning("Population rate of remission (sim):", paste(range(Pr_50k), collapse="--"), "\n")
    else # BTW: u00B1 is the plusminus sign
      cat("Population rate of remission (sim):", mean(Pr_50k), "\u00B1", round(sd(Pr_50k),7), "\n")
  }
  # 7. Calculate Pr for 1-size-fits-all dosing at each dose level
  Prs <- 0.5 * gamma(shape-0.5) / gamma(shape) * Qs * sqrt(Ds_)
  if(once){
    cat(paste("Pr's for 1-size-fits-all dosing at each retained dose:\n"))
    # TODO: Consider 1:top.dose below, or even 1:Inf!
    print(data.frame(dose.level=(1:top.dose), Pr=Prs, Q=Qs))
  }
  # 8. Depending on mode (-testing- or not), return whole 'de' list or 1-row data frame
  if(once) {
    # To facilitate visualization, let me deliver 'mtd' not as
    # a mere vector, but as a data frame. For each id, it should
    # give -absolute-, -scaled- and -fraction tolerant- of the MTDi.
    mtd_df <- data.frame(
      id = seq_along(mtd)
      ,mtd = mtd
      ,doscale = 1 + log(mtd/start.dose) / log(1+dose.jump)
      ,fractol = 1 - pgamma(mtd, shape=shape, scale=scale)
    )
    attr(de, 'mtd') <- mtd_df
    attr(de, 'dunit') <- dunit
    # When '...' includes an 'n.doses' parameter, ensure that
    # a vector of absolute doses gets attached:
    if(!is.null(n.doses <- list(...)$n.doses)){
      attr(de, 'doses') <- start.dose * (1+dose.jump)^(0:(n.doses-1))
    }
    else { # let n.doses = N/3 to avoid absent $doses attr fouling plot.de
      attr(de, 'doses') <- start.dose * (1+dose.jump)^(0:(N/3-1))
    }
    # To enable plotting of (unknown) 'true' MTDi distribution
    # underneath the D-S plot, we attach also a vector of quantiles:
    Q <- 50 # TODO: Try 100?
    mtd_quantiles <- qgamma((1:(Q-1))/Q, shape=shape, scale=scale)
    # 2. Transform these MTDi's onto the geometric dose range
    mtd_quantiles <- 1 + log(mtd_quantiles/start.dose) / log(1+dose.jump)
    attr(de,'mtd_quantiles') <- mtd_quantiles
    
    class(de) <- c("de", class(de))
    invisible(de)
  }
  else {
    #de.last <<- de
    # TODO: What else should I include below?
    data.frame(enrolling.dose=enrolling.dose
               , max.dose=top.dose
               , effic.33pc=Pr/0.5
               , effic.1sfa=max(Prs)/0.5)
  }
}
