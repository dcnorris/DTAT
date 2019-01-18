#' D3 visualization for dose-escalation/titration studies.
#' 
#' An interactive visualization, including many mouseover interactions and
#' clickable dividers between the DLT assessment periods.
#' 
#' TODO: Detail all interactions supported, and their significance.
#' 
#' @param x The 'de' object to be transformed
#' @param viewer "internal" to use the RStudio internal viewer pane for output;
#' "external" to display in an external RStudio window; "browser" to display in
#' an external browser.
#' @param \dots Additional parameters passed to \code{r2d3}
#' @return No value is returned. A plot is displayed.
#' @author David C. Norris
#' @keywords hplot dynamic
#' @examples
#' 
#' \donttest{
#' library(DTAT)
#' de <- de.sim(testing=TRUE)
#' plot(de)
#' }
#' 
#' @importFrom graphics plot
#' @export
plot.de <- function(x, viewer=c("internal","external","browser"), ...){
  r2d3::r2d3(data=x,
             script=system.file("htmlwidgets/lib/main.js", package="DTAT"),
             d3_version = 4, container = "div",
             dependencies = system.file(paste("htmlwidgets/lib",
                                              c("margins.js", "exx.js",
                                                "ox-plot.js", "ds-plot.js"),
                                              sep="/"), package="DTAT"),
             viewer=viewer,
             ...)
}
