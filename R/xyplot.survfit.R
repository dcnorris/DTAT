# Create a *lattice* version of the base graphics 'plot.survfit'


#' Plot a dose-survival curve with its confidence bounds
#' 
#' Accept a \code{survfit} object, and plot 'dose-survival curve' in the form
#' of a complementary cumulative distribution function of dose tolerability.
#' 
#' 
#' @param x An object of class \code{survfit}
#' @param data Unused; included for S3 generic/method consistency.
#' @param ylim Passed through to xyplot
#' @param \dots Passed through to xyplot
#' @return A \code{lattice} plot
#' @author David C. Norris
#' @seealso \code{\link{ds.curve}}
#' @keywords hplot methods
#' @importFrom survival survfit
#' @export
xyplot.survfit <- function(x, data=NULL, ylim=NULL, ...){
  fit <- x # rename the S3 generic argument
  # 1. Lay out axes with appropriate limits and labels
  if(is.null(ylim)){ # NB: May not find full time range
    ylim <- range(fit$time) + c(0, 0.5)
  }
  # 2. Generate data frame for step function
  df <- with(fit,
             data.frame(dose=time,
                        surv=surv,
                        upper=upper,
                        lower=lower)
  )
  df <- rbind(data.frame(dose=0, surv=1.0, upper=1, lower=1), df)
  # Insert rows in the data frame as needed to achieve a proper zig-zag
  # Since the survfit object seems to be right-continuous, our task becomes
  # to duplicate each left-limit and advance it toward the discontinuity. 
  discon <- which(diff(df$surv)!=0)
  left <- df[discon,]
  left$dose <- df$dose[discon+1]
  df <- rbind(left, df)
  df <- df[order(df$dose),]
  # 3. Determine which sections of the BYPASS and STOP thresholds got tripped
  bypass.lim <- df$dose[which(df$lower < getOption('dose.drop.threshold'))[1]]
  stop.lim   <- df$dose[which(df$upper < getOption('stop.esc.under'))[1]]
  # 4. Invoke 'panel.lines' to draw main K-M curve and its upper/lower band
  xyplot(NA ~ NA,
         ylab = NULL,
         ylab.right = "Dose level",
         xlab = "Fraction tolerant",
         par.settings=list(layout.heights=list(left.padding=-1)),
         scales = list(
           y = list(limits=ylim,
                    alternating=2, rot=90,
                    at=seq(from=max(1,ceiling(ylim[1])),
                           to=floor(ylim[2] - 0.001))
           ),
           x = list(limits=c(1,0), at=seq(0,1,0.2), cex=0.8, rot=90)
         ),
         panel = function(...){
           panel.xyplot(...)
           panel.lines(y=df$dose, x=df$surv, col='black', lwd=3)
           panel.lines(y=df$dose, x=df$upper, col='gray', lty=3, lwd=3)
           panel.lines(y=df$dose, x=df$lower, col='gray', lty=3, lwd=3)
           # Show the dose-dropping and stop.esc decision criteria also
           panel.refline(v=getOption('dose.drop.threshold'))
           panel.refline(v=getOption('stop.esc.under'))
           # Overplot the tripped sections of these thresholds
           panel.lines(x=rep(getOption('dose.drop.threshold'),2),
                       y=c(ylim[1], bypass.lim), col='green')
           panel.lines(x=rep(getOption('stop.esc.under'),2),
                       y=c(stop.lim, ylim[2]), col='red')
         }
  )
}
