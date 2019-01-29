# Generate an OXDSplot animation suitable for animation::saveGIF(expr=.)


#' Transparent animation of a 3+3/PC dose-titration study
#' 
#' Animate the periods of a 3+3/PC dose-titration study, showing the
#' dose-survival curve as it accumulates with the conclusion of each DLT
#' assessment period.
#' 
#' 
#' @param de A list of period-wise snapshots from a dose-titration study
#' @return No value is returned. This function is intended to be passed as the
#' \code{expr} argument to package \code{animation} functions such as
#' \code{saveGIF} and \code{saveVideo}. See the example below.
#' @author David C. Norris
#' @seealso \code{\link{OXDSplot}} \code{\link[animation]{animation}}
#' @keywords dynamic
#' @examples
#' 
#' \dontrun{
#' # Running this is not quite 'catastrophic', but the saveVideo()
#' # function may take a good fraction of a minute to run.
#' library(animation)
#' ani.options(ani.width=1012, ani.height=506)
#' de <- de.sim(testing=TRUE)
#' saveVideo(OXDSanimate(de)) # produces file "animation.mp4"
#' }
#' 
#' @importFrom dplyr last
#' @export
OXDSanimate <- function(de){
  periods <- max(last(de)$period)
  top.dose <- max(last(de)$dose)
  for(dwell in 1:3){ # dwell on end-of-study results before animation starts
    OXDSplot(last(de))
  }
  for(period in seq_along(de)){
    OXDSplot(de[[period]], tox.pending=TRUE, periods=periods, top.dose=top.dose)
    OXDSplot(de[[period]], tox.pending=FALSE, periods=periods, top.dose=top.dose)
  }
}
