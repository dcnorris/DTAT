de.sim <- function(CV=0.7, start.dose=0.25, dose.jump=0.4, N=24, periods=N/3+2,
                   testing=is.null(sys.call(-1)), once=testing, ...){
  # Invoked interactively, the function aims to support reproducible, interactive testing.
  # Otherwise (the intended default) it simply returns simulation summaries that the caller
  # is supposed to be interested in aggregating.
  if(testing)
    set.seed(2017)
  # 1. Generate N individual MTDi's from Gamma distribution with mean = 1
  shape <- CV^-2
  scale <- 1/shape # 1 =: mean = shape*scale
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
    attr(de, 'mtd') <- mtd
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
